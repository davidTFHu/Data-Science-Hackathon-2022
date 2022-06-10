library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
library(glue)
library(ggh4x)

set.seed(42)

font_add_google("Raleway", family="Raleway", regular.wt=400, bold.wt=700)
showtext_auto(TRUE)

wd = "~/git/ford-hackathon/personal/annie"
setwd(wd)

source("utils_plotting.R")

brandscape = read_csv("brandscape.csv", col_type=cols()) %>% select(-1)
brandscape

# Instantiate vars
cols = c("country", "brand", "metric_name", "value")
metrics = c("Total Users", "Lapsed User", "Recommend To A Friend")
map_metrics = c("owners", "lapsed", "recommend")
names(map_metrics) = metrics

# Mask brandscape
mask = (brandscape[["metric_name"]] %in% metrics) & (brandscape[["base"]] == "1_All Adults")
data = brandscape[mask, cols]

# Map metrics
data = data %>% mutate(metric_name=recode(metric_name, !!!map_metrics))

# Reshape to wide format
data = data %>% pivot_wider(names_from=metric_name, values_from=value)

# Add new cols
data = (
  data %>% 
  group_by(country) %>% 
  mutate(
    rank_owned=rank(-owners),
    top_owned=rank_owned <= 5,
    bin_recommend=cut(
      recommend, 
      c(0,10,15,20,25,30,35),
      c("0-10%", "10-15%", "15-20%", "20-25%", "25-30%", "30-35%")
    )
  ) %>% 
  ungroup
)

# Create label data
data_labels = rbind(
  data %>% 
  filter(top_owned==TRUE) %>%
  mutate(label=ifelse(brand=="Ford", "bold('Ford')", brand)),
  data %>% 
  filter(top_owned==FALSE) %>% 
  group_by(country) %>% 
  slice_sample(n=1) %>% 
  mutate(label="bold('Not')~top~5~brands")
)
data_labels

# Plot caption
plot_caption = glue("
  Past - Previously owned brand
  Present - Currently own brand
  Future - Would buy brand again or recommend
")
  
# Instantiate plot object
plot = (
  data %>% 
  ggplot(aes(x=owners, y=lapsed)) +
    
  # Add geometries
  geom_point(
    data=. %>% filter(top_owned == FALSE),
    aes(size=top_owned, shape=top_owned), stroke=0.8, colour="lightgrey"
  ) +
  geom_point(
    data=. %>% filter(top_owned == TRUE),
    aes(size=top_owned, shape=top_owned, color=bin_recommend), stroke=0.8
  ) +
  geom_text_repel(
    seed=42,
    min.segment.length=unit(0,"cm"),
    data=data_labels, 
    aes(label=label), 
    color="#081534", 
    family="Raleway", 
    size=3,
    hjust=0, 
    nudge_x=3.5, 
    nudge_y=-0.25,
    parse=TRUE
  ) +
  facet_grid(.~str_to_upper(country)) +
  
  # Specify scales
  scale_size_manual(values=c(2, 3)) +
  scale_shape_manual(values=c(21, 16)) +
  scale_colour_viridis_d(drop=FALSE, direction=-1) +
  scale_x_continuous(
    limits=c(0,40), 
    breaks=seq(0,40,40), 
    minor_breaks=seq(0,40,10),
    labels=function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    limits=c(0,42.5), 
    breaks=seq(0,42.5,40), 
    minor_breaks=seq(0,42.5,10),
    labels=function(y) paste0(y, "%")
  ) +
  coord_equal(expand=FALSE, clip="off") +
  
  # Specify legend guides
  guides(
    size="none", 
    shape="none", 
    color=guide_legend(
      title.position="top", 
      direction="vertical", 
      ncol=1, 
      title.hjust=0, 
      override.aes=list(size=3.5)
    )
  ) +
  
  # Add plot labels
  labs(
    x="Present\n(Currently own brand)", 
    y="Past\n(Previously bought brand)", 
    colour="Future\n(Would buy\n brand again)", 
    title="**Top 5 owned brands**"
  ) +
    
  # Add theme
  theme_hackathon()

); plot

ggsave("brand_ownership.png", plot, width=12, units="cm", dpi=300)


cols = c("country", "brand", "metric_name", "value")
mask = (brandscape[["metric_group"]] == "Powergrid") & (brandscape[["base"]] == "1_All Adults")
powergrid = brandscape[mask, cols] 
powergrid

powergrid = powergrid %>% inner_join(data %>% select(country, brand, top_owned))

powergrid_labels = tibble(
  label=c(
    "New or\n unfocused",
    "Eroded",
    "Niche or\n unrealized", 
    "Leader", 
    "Mass\nmarket"
  ),
  x=c(25, 75, 25, 65, 80), 
  y=c(25, 25, 75, 90, 60)
) 
powergrid_labels

powergrid = (
  powergrid %>% 
  pivot_wider(names_from="metric_name", values_from="value")
)
powergrid

powergrid_data_labels = data_labels %>% select(country, brand, label) %>% inner_join(powergrid)
powergrid_data_labels

brand_colours = c(
  "#F2D173", # Chevvy
  "#133A7C", # Ford
  "#9F9F9F", # Honda, could also be silver
  "#CDCDCD", # Hyundai
  "#097DC6", # Mazda
  "black", # Nissan
  "#FF0101" # Toyota
)

plot = (
  ggplot() +
    
  # Set up powergrid matrix
  geom_hline(yintercept=50) +
  geom_vline(xintercept=50) +
  annotate(geom="segment", x=50, xend=100, y=50, yend=100, linetype=2) +
  geom_text(
    data=powergrid_labels, 
    aes(x=x, y=y, label=label), 
    lineheight=0.825, 
    family="Raleway", 
    hjust=0.5,
    size=2.5
  ) +
  
  # Add non-top brands
  geom_point(
    data=powergrid %>% filter(top_owned == FALSE),
    aes(x=`Brand Stature`, y=`Brand Strength`), 
    shape=21, 
    size=2, 
    stroke=0.8, 
    colour="lightgrey",
    alpha=0.5
  ) +
  
  # Add top brands
  geom_point(
    data=powergrid %>% filter(top_owned == TRUE),
    aes(x=`Brand Stature`, y=`Brand Strength`, colour=brand), 
    stroke=0.8, 
    shape=16, 
    size=3
  ) +
  
  # Label ford
  geom_text_repel(
    data=powergrid %>% filter(brand == "Ford"), 
    aes(x=`Brand Stature`, y=`Brand Strength`, label=brand),
    seed=42, 
    min.segment.length=unit(0,"cm"),
    family="Raleway", 
    size=3, 
    color="#081534", 
    hjust=0, 
    fontface="bold",
    nudge_y=1.5
  ) +
  facet_grid(.~str_to_upper(country)) +
  coord_equal(expand=FALSE, clip="off") +
  
  # Set scales
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,50), minor_breaks=NULL) + 
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,50), minor_breaks=NULL) +
  scale_colour_manual(values=brand_colours) +
  
  # Labels
  labs(
    x="Brand stature\n(Esteem & knowledge)", 
    y="Brand strength\n(Differentation & relevance)", 
    color="Top 5 owned\nbrands", 
    title="**BAV PowerGrid**"
  ) +
    
  # Add theme
  theme_hackathon()
); plot

ggsave("bav_powergrid.png", plot, width=12, units="cm", dpi=300)

# Brand imagery results
# - Get data
imagery = read_csv("imagery_PCA.csv") %>% select(grp, variable, value)

# - Remove duplicates and rank
imagery = (
  imagery %>% 
  arrange(grp, variable, desc(value)) %>% 
  group_by(grp, variable) %>% 
  slice(n=1) %>% 
  ungroup %>% 
  arrange(grp, desc(value))
)
imagery = imagery %>% group_by(grp) %>% mutate(rank=rank(-value)) %>% ungroup

# Filter to top 3 imageries
imagery = imagery %>% filter(rank <= 3)

# Key of groups
imagery_key = (
  imagery %>% 
  select(grp) %>% 
  unique() %>% 
  mutate(grp1=factor(
    grp, 
    levels=c("allbrands_both", "allbrands_Australia", "allbrands_USA", "allbrands_female", "allbrands_male", "allbrands_ford", "allbrands_toyota")
  )) %>%
  arrange(grp1) %>%
  mutate(
    grp1=fct_inorder(grp1), 
    grp2=c("", "by country", "by country", "by sex", "by sex", "by brand", "by brand"), 
    grp2=fct_inorder(grp2),
    grp3=c("All", "Australia", "USA", "Female", "Male", "Ford", "Toyota"), 
    grp3=fct_inorder(grp3)
  )  
)

# Merge key to imagery data
imagery_plot = imagery_key %>% inner_join(imagery)

cols = c('#133a7c', '#4f7ab2', '#a6bbd5')

# Filter down to 2 levels for plotting
imagery_plot = (
  imagery_plot %>% 
  filter(grp2 %in% c("by country", "by brand")) %>%
  mutate(grp2=factor(grp2, levels=c("by brand", "by country")))
)

# Plot of imagery ranks
plot =
  imagery_plot %>%
  ggplot(aes(
    y=interaction(grp3, grp2), 
    x=str_wrap(variable, 10), 
    color=factor(rank), 
    label=rank
  )) + 
  geom_point(size=6.5) + 
  geom_text(colour="white", family="Raleway", size=2.75, fontface="bold") +
  guides(y="axis_nested", color="none") +
  scale_color_manual(values=cols) +
  theme_hackathon(hjust=FALSE) +
  labs(x=NULL, y=NULL, title="**Top 3 imagery attr by group**"); plot

ggsave("bav_imagery.png", plot, width=12, height=6, units="cm", dpi=300)
