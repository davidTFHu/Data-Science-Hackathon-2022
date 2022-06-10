#####
# Plotting tidied Audience Origin (AO) data. The AO data focuses on survey 
# responses for those who purchased a car in this last year. Populations are 
# projections using online survey responses
#####

# Setup
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
library(glue)
library(ggh4x)

set.seed(42)

wd = "~/git/ford-hackathon/personal/annie"
setwd(wd)

source("utils_plotting.R")  

# Get tidy AO data
data = read_csv("AO21/ao_data_tidy.csv") %>% select(-1)
data[["car_type"]] = factor(
  data[["car_type"]], 
  levels=c("second_hand", "new"), 
  labels=c("Used", "New")
)
data

# Percent buying used and new cars
pct_buy = data %>% group_by(Market, car_type) %>% summarise(pop=sum(pop))
pct_buy = (
  pct_buy %>%
  group_by(Market) %>%
  mutate(
    pct=pop / sum(pop) * 100,
    label=glue("{round(pct, digits=0)}%")
  ) %>%
  ungroup
)
pct_buy

cols_fill = c("#C6C6C6", "#133A7C")
cols = c("#081534", "#FFFFFF")

plot = (
  pct_buy %>%
    
  # Instantiate plot and geoms
  ggplot(aes(x=Market, y=pct)) +
  geom_col(aes(fill=car_type)) +
  geom_text(
    aes(label=label, group=car_type, colour=car_type), 
    position=position_stack(), 
    vjust=1.75,
    family="Raleway", 
    fontface="bold",
    size=3
  ) +
    
  # Coordinates and scales
  coord_cartesian(expand=FALSE) +
  scale_fill_manual(values=cols_fill) +
  scale_colour_manual(values=cols) +
  guides(colour="none") +
    
  # Theme specs
  theme_hackathon(hjust=FALSE) +
  theme(
    panel.grid=element_blank(), 
    axis.text.y=element_blank(), 
    axis.title=element_blank(),
    axis.text.x=element_text(face="bold")
  ) +
    
  # Add labels
  labs(fill="Car condition", title="**Car purchases**")
); plot

ggsave("car_purchases.png", plot, height=6, width=12, units="cm", dpi=300)

# Focus on new cars hereafter
data = data %>% filter(car_type == "New") %>% select(-car_type)
data

# What's the demographic split in the new car buying market?
pct_new_demo = data %>% group_by(Market, age_sex) %>% summarise(pop=sum(pop))
pct_new_demo = (
  pct_new_demo %>%
  group_by(Market) %>%
  mutate(
    pct=pop / sum(pop) * 100,
    label=glue("{round(pct, digits=0)}%")
  ) %>%
  ungroup
)
pct_new_demo[["age_sex"]] = factor(
  pct_new_demo[["age_sex"]], 
  levels=c("Female <35", "Female >35", "Male <35", "Male >35"), 
  labels=c("Female, < 35", "Female, 35+", "Male, < 35", "Male, 35+")
)

cols = c("white", "white", "white", "#081534")

plot = (
  pct_new_demo %>%
    
  # Instantiate plot and geoms
  ggplot(aes(x=Market, y=pct, fill=age_sex)) +
  geom_col() +
  geom_text(
    aes(label=label, colour=age_sex), 
    position=position_stack(), 
    vjust=1.75,
    family="Raleway", 
    fontface="bold",
    size=3
  ) +
    
  # Coordinates and scales
  coord_cartesian(expand=FALSE) +
  scale_fill_viridis_d(begin=0.2, end=0.8) +
  scale_colour_manual(values=cols) +
  guides(colour="none") +
    
  # Theme specs
  theme_hackathon(hjust=FALSE) +
  theme(
    panel.grid=element_blank(), 
    axis.text.y=element_blank(), 
    axis.title=element_blank(),
    axis.text.x=element_text(face="bold")
  ) +
  
  # Add labels
  labs(fill="Demographic", title="**New car purchases**") 
); plot

ggsave("new_car_purchases.png", plot, height=6, width=12, units="cm", dpi=300)

# Touchpoints, aggregated
tp_pct = (
  data %>%
  group_by(Market, var1, var3, value) %>%
  summarise(pop=sum(pop)) %>%
  ungroup
)
tp_pct = tp_pct %>% pivot_wider(names_from=value, values_from=pop)
tp_pct = (
  tp_pct %>%
  mutate(
    `Not selected`=replace_na(`Not selected`, 0),
    `Selected`=replace_na(`Selected`, 0),
    pct=`Selected` / (`Selected` + `Not selected`) * 100)
)

tp_pct_wide = tp_pct %>% pivot_wider(id_cols=c(Market, var3), names_from=var1, values_from=pct)
tp_pct_wide = (
  tp_pct_wide %>%
  group_by(Market) %>%
  mutate(
    rank_helped=rank(-`Touchpoints helped`), 
    rank_noticed=rank(-`Touchpoints noticed`),
    text=rank_helped <= 3 | rank_noticed <= 3,
    col=ifelse(text, var3, "z")
  ) %>% 
  ungroup
)

# Instantiate plot
plot = tp_pct_wide %>% ggplot(aes(x=`Touchpoints noticed`, y=`Touchpoints helped`))

# Build plot
plot = plot_ao_scatter(
  ggplot_obj=plot,
  col_vector=c('#081534', '#173c75', '#2a6cad', '#639dd4', '#aeceee', '#c6c6c6'),
  lim_max=60,
  steps_minor=10,
  title="**Touchpoints**",
  xlab="Touchpoints noticed",
  ylab="Touchpoints helped"
  
); plot

ggsave("noticed_vs_helped.png", plot, width=12, units="cm", dpi=300)

# Conversion rate
pct_noticed = tp_pct %>% filter(var1 == "Touchpoints noticed") %>% select(Market, var3, pct_noticed=pct)
pct_conversion = data %>% filter(value == "Selected")
pct_conversion = (
  pct_conversion %>% 
  group_by(Market, var1, var3) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup
)
pct_conversion = pct_conversion %>% pivot_wider(names_from=var1, values_from=pop)
pct_conversion = (
  pct_conversion %>% 
  mutate(conversion_rate=`Touchpoints helped` / `Touchpoints noticed` * 100)
)
pct_conversion = pct_conversion %>% inner_join(pct_noticed)
pct_conversion = (
  pct_conversion %>% 
  group_by(Market) %>% 
  mutate(
    rank=rank(-conversion_rate), 
    text=rank <= 3 | rank(-pct_noticed) <= 2, 
    col=ifelse(text, var3, "z")
  ) %>% 
  ungroup
)

# Instantiate plot
plot = pct_conversion %>% ggplot(aes(x=pct_noticed, y=conversion_rate))

# Build plot
plot = plot_ao_scatter(
  ggplot_obj=plot,
  col_vector=c('#081534', '#123063', '#1f508f', '#3073b3', '#5996cf', '#8db9e4', '#c5dcf4', '#C6C6C6'),
  lim_max=100,
  steps_minor=20,
  title="**Noticed touchpoints vs conversion**",
  xlab="Touchpoints noticed", 
  ylab="Conversion rate"
); plot

ggsave("conversion.png", plot, width=12, units="cm", dpi=300)
