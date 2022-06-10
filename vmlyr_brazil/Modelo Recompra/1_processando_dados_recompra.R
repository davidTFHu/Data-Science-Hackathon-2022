# Saving data in a new format ----

# Reading the repurchase data ----

library(tidyverse)


# Reading repurchase data - 05/03/2022 ----
dados_recompra5 <- read.csv2("dados/dados_brutos_txt/recompra_20220503.txt", sep = ";") %>% 
  as.data.frame()
dados_recompra5 %>% dim()  # 177457 lines, 9 variables


# Loading the previously used "final" database ----
dados_recompra_old <- readRDS("dados/dados_recompra_final.rds")
dados_recompra_old %>% dim() # 2914835 linhas, 11 variáveis


# Modifying the new database
dados_recompra5 <- dados_recompra5 %>%
  as.data.frame() %>%
  mutate(CPF = as.character(CPF))



dados_recompra_bind <- bind_rows(dados_recompra5, dados_recompra_old)
dados_recompra_bind %>% dim() # 3092292 lines, 11 variables

dados_recompra_bind <- dados_recompra_bind %>%  # from 3092292 to 2918406 (173886 equals line )
  distinct()

dados_recompra_bind <- dados_recompra_bind %>% 
  group_by(CPF) %>% 
  mutate(
    N_de_Recompras = n(),
    Recomprou = ifelse(RECOMPRA == "SIM", 1, 0)
  )

saveRDS(dados_recompra_bind, "dados/dados_recompra_final.rds", version = 2)


##############################################################
##############################################################

library(tidyverse)

dados_recompra <- readRDS("dados/dados_recompra_final.rds")

dados_recompra_final<- dados_recompra %>% 
  mutate(NAMEPLATE_ANTERIOR = as.character(trimws(NAMEPLATE_ANTERIOR)),
         NAMEPLATE = as.character(trimws(NAMEPLATE)),
         NAMEPLATE_ANTERIOR = as.character(ifelse(NAMEPLATE_ANTERIOR=="KA SEDAN", "K_SEDAN", NAMEPLATE_ANTERIOR)),
         NAMEPLATE = as.character(ifelse(NAMEPLATE=="KA SEDAN", "K_SEDAN", NAMEPLATE)))

saveRDS(dados_recompra_final, "dados/dados_recompra_final.rds", version = 2)

dados_recompra <- readRDS("dados/dados_recompra_final.rds")

# VChecking the car models in the final database ====
dados_recompra$NAMEPLATE %>% table()
dados_recompra$NAMEPLATE %>% unique()

### Recategorizing the necessary models ====
dados_recompra$NAMEPLATE <- case_when(
  dados_recompra$NAMEPLATE == "KA+" | 
    dados_recompra$NAMEPLATE == "KA HATCH" |
    dados_recompra$NAMEPLATE == "KA" |
    dados_recompra$NAMEPLATE == "KA 1.6" |
    dados_recompra$NAMEPLATE == "NOVO KA" |
    dados_recompra$NAMEPLATE == "KA SPORT"  ~ "KA",
  dados_recompra$NAMEPLATE == "EDGE" | 
    dados_recompra$NAMEPLATE == "NOVO EDGE" ~ "EDGE",
  dados_recompra$NAMEPLATE == "FIESTA" |
    dados_recompra$NAMEPLATE == "FIESTA 1.6" |
    dados_recompra$NAMEPLATE == "FIESTA HATCH" |
    dados_recompra$NAMEPLATE == "FIESTA TRAIL" |
    dados_recompra$NAMEPLATE == "NEW FIESTA" |
    dados_recompra$NAMEPLATE == "NOVO FIESTA FLEX" |
    dados_recompra$NAMEPLATE == "NOVO FIESTA HATCH" |
    dados_recompra$NAMEPLATE == "FIESTA SEDAN" |
    dados_recompra$NAMEPLATE == "NEW FIESTA SEDAN" |
    dados_recompra$NAMEPLATE == "FIESTA TRAIL 1.6" ~ "FIESTA",
  # dados_recompra$NAMEPLATE == "FIESTA SEDAN" |
  #   dados_recompra$NAMEPLATE == "NEW FIESTA SEDAN" ~ "FIESTA SEDAN",
  dados_recompra$NAMEPLATE == "ECOSPORT" |
    dados_recompra$NAMEPLATE == "ECOSPORT  XLT" |
    dados_recompra$NAMEPLATE == "ECOSPORT 4WD" |
    dados_recompra$NAMEPLATE == "ECOSPORT 4WD 2" |
    dados_recompra$NAMEPLATE == "ECOSPORT 4WD 2.0 FLEX" |
    dados_recompra$NAMEPLATE == "ECOSPORT XL" |
    dados_recompra$NAMEPLATE == "ECOSPORT XL FLEX" |
    dados_recompra$NAMEPLATE == "ECOSPORT XLS" |
    dados_recompra$NAMEPLATE == "ECOSPORT XLS 2.0 FLEX" |
    dados_recompra$NAMEPLATE == "ECOSPORT XLT" |
    dados_recompra$NAMEPLATE == "ECOSPORT XLT 2.0 FLEX" ~ "ECOSPORT",
  dados_recompra$NAMEPLATE == "FOCUS" |
    dados_recompra$NAMEPLATE == "FOCUS GHIA" |
    dados_recompra$NAMEPLATE == "FOCUS HATCH" |
    dados_recompra$NAMEPLATE == "FOCUS 2.0" ~ "FOCUS",
  dados_recompra$NAMEPLATE == "FOCUS 2.0 SEDAN" |
    dados_recompra$NAMEPLATE == "FOCUS SEDAN" ~ "FOCUS SEDAN",
  dados_recompra$NAMEPLATE == "TERRITORRY" |
    dados_recompra$NAMEPLATE == "TERRITORY" ~ "TERRITORY",
  dados_recompra$NAMEPLATE == "CARGO" |
    dados_recompra$NAMEPLATE == "F-350" |
    dados_recompra$NAMEPLATE == "F-4000" |
    # dados_recompra$NAMEPLATE == "MAVERICK" |
    dados_recompra$NAMEPLATE == "TRANSIT"  ~ "Outros",
  TRUE ~ as.character(dados_recompra$NAMEPLATE)
) 

dados_recompra <- dados_recompra %>%  
  mutate(Modelo_Atual_recategorizado = 
           case_when(
             NAMEPLATE ==  "FOCUS SEDAN" |
               NAMEPLATE == "MUSTANG"  ~ "Carro de Luxo",
             NAMEPLATE == "TERRITORY" |
               NAMEPLATE == "BRONCO" |
               NAMEPLATE == "ECOSPORT" ~ "SUV",
             TRUE ~ as.character(NAMEPLATE)
           )
  )



# Saving data for old vehicles ----

library(lubridate)

lista_veiculos <- c("RANGER", "KA",  "K_SEDAN", "FIESTA", "FUSION", "FOCUS", "EDGE" ,
                    "MAVERICK" # Novo Veiculo = poucos dados (8) - 21/03/2022
                    #"FIESTA SEDAN"#, Virou FIESTA 
                    # "FOCUS SEDAN", "MUSTANG", > Recategorizados
                    # "Carro de Luxo",
                    # # "ECOSPORT", "TERRITORY", "BRONCO", -> Recategorizados
                    # "SUV"
                    )


for (i in 1:length(lista_veiculos)) {
  
  veiculo <- lista_veiculos[[i]]

  print(paste("veiculo", veiculo))
  
  dados_veiculo <- dados_recompra %>% 
    mutate(Filtro1 = ifelse(str_detect(toupper(NAMEPLATE_ANTERIOR), veiculo),1,0),
           Filtro2 = ifelse(Recomprou==0 & str_detect(toupper(Modelo_Atual_recategorizado), veiculo),1,0),
           Filtro = ifelse(Filtro1==1|Filtro2==1, 1,0)) %>% 
    filter(Filtro==1)
  
  dados_veiculo$DT_COMP <-  as.Date(dados_veiculo$DT_COMP)
  dados_veiculo$DT_COMP_ANTERIOR <-  as.Date(dados_veiculo$DT_COMP_ANTERIOR,format="%Y/%m/%d") # erro territory Error in charToDate(x) : 
                                                                            #string de caracteres não é um formato padrão não ambíguo, por isso adicionei format
  
  dados_veiculo <- dados_veiculo %>% 
    as.data.frame() %>% 
    transmute(
      ID = CPF,
      N_de_Recompras = N_de_Recompras,
      Data_Compra = DT_COMP,
      Data_Compra_Anterior = DT_COMP_ANTERIOR,
      Modelo_Atual = Modelo_Atual_recategorizado,
      Modelo_Anterior = NAMEPLATE_ANTERIOR,
      UF = DS_UF,
      Recomprou = Recomprou
    ) %>% 
    mutate(
      Dias_Recompra = Data_Compra - Data_Compra_Anterior,
      Dias_Atual = as.Date(Sys.Date()) - Data_Compra,
      Dias_Recompra = ifelse(is.na(Dias_Recompra), Dias_Atual, Dias_Recompra)/365,
      Modelo_Atual = trimws(as.character(Modelo_Atual)),
      Modelo_Anterior = trimws(as.character(Modelo_Anterior)),
      UF = trimws(as.character(toupper(UF)))
    )
  
  saveRDS(dados_veiculo, paste0("dados/veiculos/dados_", tolower(veiculo),".rds"), version = 2)
  
}



# Saving data by grouped vehicles ----
lista_veiculos_agrupados <- c( 
  # "FOCUS SEDAN", "MUSTANG", > Recategorizados
  "Carro de Luxo",
  # "ECOSPORT", "TERRITORY", "BRONCO", -> Recategorizados
  "SUV"
)


for (i in 1:length(lista_veiculos_agrupados)) {
  
  veiculo <- lista_veiculos_agrupados[[i]]
  
}
  
  ##### SUV
  veiculo <- "SUV"
  
  print(paste("veiculo", veiculo))
  
  dados_veiculo <- dados_recompra %>% 
    mutate(Filtro1 = case_when(
      str_detect(toupper(NAMEPLATE_ANTERIOR), "ECOSPORT") |
        str_detect(toupper(NAMEPLATE_ANTERIOR), "TERRITORY") |
        str_detect(toupper(NAMEPLATE_ANTERIOR), "BRONCO") ~ 1,
      TRUE ~ 0),
           Filtro2 = ifelse(Recomprou==0 & str_detect(toupper(Modelo_Atual_recategorizado), toupper(veiculo)),1,0),
           Filtro = ifelse(Filtro1==1|Filtro2==1, 1,0)) %>% 
    filter(Filtro==1)
  
  dados_veiculo$DT_COMP <-  as.Date(dados_veiculo$DT_COMP)
  dados_veiculo$DT_COMP_ANTERIOR <-  as.Date(dados_veiculo$DT_COMP_ANTERIOR,format="%Y/%m/%d") # erro territory Error in charToDate(x) : 
  #character string is not an unambiguous standard format, so I added format
  
  
  dados_veiculo <- dados_veiculo %>% 
    as.data.frame() %>% 
    transmute(
      ID = CPF,
      N_de_Recompras = N_de_Recompras,
      Data_Compra = DT_COMP,
      Data_Compra_Anterior = DT_COMP_ANTERIOR,
      Modelo_Atual_original = NAMEPLATE,
      Modelo_Atual = Modelo_Atual_recategorizado,
      Modelo_Anterior = NAMEPLATE_ANTERIOR,
      UF = DS_UF,
      Recomprou = Recomprou
    ) %>% 
    mutate(
      Dias_Recompra = Data_Compra - Data_Compra_Anterior,
      Dias_Atual = as.Date(Sys.Date()) - Data_Compra,
      Dias_Recompra = ifelse(is.na(Dias_Recompra), Dias_Atual, Dias_Recompra)/365,
      Modelo_Atual_original = trimws(as.character(Modelo_Atual_original)),
      Modelo_Atual = trimws(as.character(Modelo_Atual)),
      Modelo_Anterior = trimws(as.character(Modelo_Anterior)),
      UF = trimws(as.character(toupper(UF)))
    )
  
  saveRDS(dados_veiculo, paste0("dados/veiculos/dados_", tolower(veiculo),".rds"), version = 2)
  
  
  ##### Luxury car
  veiculo <- "Carro de Luxo"
  
  print(paste("veiculo", veiculo))
  
  dados_veiculo <- dados_recompra %>% 
    mutate(Filtro1 = case_when(
      str_detect(toupper(NAMEPLATE_ANTERIOR), "MUSTANG") |
        str_detect(toupper(NAMEPLATE_ANTERIOR), "FOCUS SEDAN") ~ 1,
      TRUE ~ 0),
      Filtro2 = ifelse(Recomprou==0 & Modelo_Atual_recategorizado == veiculo,1,0),
      Filtro = ifelse(Filtro1==1|Filtro2==1, 1,0)) %>% 
    filter(Filtro==1)
  
  dados_veiculo$DT_COMP <-  as.Date(dados_veiculo$DT_COMP)
  dados_veiculo$DT_COMP_ANTERIOR <-  as.Date(dados_veiculo$DT_COMP_ANTERIOR,format="%Y/%m/%d") # erro territory Error in charToDate(x) : 
  #character string is not an unambiguous standard format, so I added format
  
  
  dados_veiculo <- dados_veiculo %>% 
    as.data.frame() %>% 
    transmute(
      ID = CPF,
      N_de_Recompras = N_de_Recompras,
      Data_Compra = DT_COMP,
      Data_Compra_Anterior = DT_COMP_ANTERIOR,
      Modelo_Atual_original = NAMEPLATE,
      Modelo_Atual = Modelo_Atual_recategorizado,
      Modelo_Anterior = NAMEPLATE_ANTERIOR,
      UF = DS_UF,
      Recomprou = Recomprou
    ) %>% 
    mutate(
      Dias_Recompra = Data_Compra - Data_Compra_Anterior,
      Dias_Atual = as.Date(Sys.Date()) - Data_Compra,
      Dias_Recompra = ifelse(is.na(Dias_Recompra), Dias_Atual, Dias_Recompra)/365,
      Modelo_Atual_original = trimws(as.character(Modelo_Atual_original)),
      Modelo_Atual = trimws(as.character(Modelo_Atual)),
      Modelo_Anterior = trimws(as.character(Modelo_Anterior)),
      UF = trimws(as.character(toupper(UF)))
    )
  
  saveRDS(dados_veiculo, paste0("dados/veiculos/dados_", tolower(veiculo),".rds"), version = 2)
  
# }



###########################################################################################


##### building triggers for old vehicles ----
library(tidyverse)
library(survival)

lista_veiculos <- c("ranger"
                    #, "ecosport"
                    , "ka", "k_sedan", "fiesta", "fusion", "focus", "edge" ,
                    "maverick"
                    #"fiesta sedan" #, Virou FIESTA 
                    #, "focus sedan"
                    )


for (i in 1:length(lista_veiculos)) {
  
  veiculo <- lista_veiculos[[i]]
  
  print(paste("veiculo", veiculo))
  
  dados_veiculo <- readRDS(paste0("dados/veiculos/dados_", tolower(veiculo),".rds"))
  
  dados_veiculo0<- dados_veiculo %>% 
    dplyr::select(ID, Dias_Recompra, Data_Compra, Recomprou, Modelo_Atual, Modelo_Anterior, UF) %>% 
    filter(Recomprou==1)
  
  fit <- survfit(Surv(Dias_Recompra, Recomprou) ~ 1, data = dados_veiculo0)
  
  df_fit <- data.frame(
    recompra = 1 - fit$surv,
    time = fit$time
  ) %>% 
    mutate(
      gatilho1 = ifelse(recompra > 0.15 & recompra < 0.35, 1, 0),
      gatilho2 = ifelse(recompra > 0.35 & recompra < 0.50, 1, 0),
      gatilho22 = ifelse(recompra > 0.50 & recompra < 0.7, 1, 0), # I raised the trigger until 70% ----
      gatilho3 = ifelse(recompra > 0.7 & recompra < 0.85, 1, 0)
    )
  
  gatilho1 <- df_fit %>% filter(gatilho1 == 1) # %>% summary()
  gatilho1_min <- gatilho1$time %>% min()
  gatilho1_max <- gatilho1$time %>% max()
  
  
  gatilho2 <- df_fit %>% filter(gatilho2 == 1) # %>% summary()
  gatilho2_min <- gatilho2$time %>% min()
  gatilho2_max <- gatilho2$time %>% max()
  
  gatilho22 <- df_fit %>% filter(gatilho22 == 1) # %>% summary()
  gatilho22_min <- gatilho22$time %>% min()
  gatilho22_max <- gatilho22$time %>% max()
  
  gatilho3 <- df_fit %>% filter(gatilho3 == 1) # %>% summary()
  gatilho3_min <- gatilho3$time %>% min()
  gatilho3_max <- gatilho3$time %>% max()
  
  
  dados_veiculo <- dados_veiculo %>% 
    group_by(ID) %>% 
    summarise(
      dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
      data_ultima_compra = max(as.Date(Data_Compra)),
      UF = first(UF)
    ) %>% 
    mutate(
      gatilho = case_when(
        dias_da_ultima_compra <= gatilho1_min*365 ~ "Buy Recently",
        dias_da_ultima_compra >= gatilho1_min*365 & dias_da_ultima_compra <= gatilho1_max*365 ~ "Grotwh",
        dias_da_ultima_compra >= gatilho2_min*365 & dias_da_ultima_compra <= gatilho2_max*365 ~ "Maturing",
        dias_da_ultima_compra >= gatilho22_min*365 & dias_da_ultima_compra <= gatilho22_max*365 ~ "Maturity",
        dias_da_ultima_compra >= gatilho3_min*365 & dias_da_ultima_compra <= gatilho3_max*365 ~ "Decline",
        TRUE ~ "Lost"
      )
    ) 
  
  saveRDS(dados_veiculo, paste0("dados/veiculos/dados_gatilho_", tolower(veiculo),".rds"), version = 2)
  
}


# trigger new vehicles/SUV ----

  
dados_veiculo <- readRDS(paste0("dados/veiculos/dados_suv.rds"))

veiculo <- "SUV"
print(paste("veiculo", veiculo))

 ### Calculating trigger just for ecosport
  dados_veiculo0<- dados_veiculo %>% 
    filter(Modelo_Atual_original=="ECOSPORT") %>% 
    dplyr::select(ID, Dias_Recompra, Data_Compra, Recomprou, Modelo_Atual, Modelo_Anterior, UF) %>% 
    filter(Recomprou==1)
  
  fit <- survfit(Surv(Dias_Recompra, Recomprou) ~ 1, data = dados_veiculo0)
  
  df_fit <- data.frame(
    recompra = 1 - fit$surv,
    time = fit$time
  ) %>% 
    mutate(
      gatilho1 = ifelse(recompra > 0.15 & recompra < 0.35, 1, 0),
      gatilho2 = ifelse(recompra > 0.35 & recompra < 0.50, 1, 0),
      gatilho22 = ifelse(recompra > 0.50 & recompra < 0.7, 1, 0), # I raised the trigger until 70% ----
      gatilho3 = ifelse(recompra > 0.7 & recompra < 0.85, 1, 0)
    )
  
  gatilho1 <- df_fit %>% filter(gatilho1 == 1) # %>% summary()
  gatilho1_min <- gatilho1$time %>% min()
  gatilho1_max <- gatilho1$time %>% max()
  
  
  gatilho2 <- df_fit %>% filter(gatilho2 == 1) # %>% summary()
  gatilho2_min <- gatilho2$time %>% min()
  gatilho2_max <- gatilho2$time %>% max()
  
  gatilho22 <- df_fit %>% filter(gatilho22 == 1) # %>% summary()
  gatilho22_min <- gatilho22$time %>% min()
  gatilho22_max <- gatilho22$time %>% max()
  
  gatilho3 <- df_fit %>% filter(gatilho3 == 1) # %>% summary()
  gatilho3_min <- gatilho3$time %>% min()
  gatilho3_max <- gatilho3$time %>% max()
  
  dados_veiculo <- dados_veiculo %>% 
    group_by(ID) %>% 
    filter(Modelo_Atual_original %in% c("ECOSPORT","BRONCO","TERRITORY")) %>% # Selecting only the cases of interest
    summarise(
      dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
      data_ultima_compra = max(as.Date(Data_Compra)),
      UF = first(UF),
      Modelo_Atual_original = Modelo_Atual_original
    ) %>% 
    distinct() %>% 
    mutate(
      gatilho = case_when(
        dias_da_ultima_compra <= 0.5*365 &  Modelo_Atual_original == "TERRITORY" |
          dias_da_ultima_compra <= 0.5*365 &  Modelo_Atual_original == "BRONCO" ~ "Buy Recently",
        dias_da_ultima_compra > 0.5*365 &  Modelo_Atual_original == "TERRITORY" |
          dias_da_ultima_compra > 0.5*365 &  Modelo_Atual_original == "BRONCO" ~ "Grotwh",
        dias_da_ultima_compra <= gatilho1_min*365 &  Modelo_Atual_original == "ECOSPORT" ~ "Buy Recently",
        dias_da_ultima_compra >= gatilho1_min*365 & dias_da_ultima_compra <= gatilho1_max*365 &  Modelo_Atual_original == "ECOSPORT" ~ "Grotwh",
        dias_da_ultima_compra >= gatilho2_min*365 & dias_da_ultima_compra <= gatilho2_max*365  &  Modelo_Atual_original == "ECOSPORT"~ "Maturing",
        dias_da_ultima_compra >= gatilho22_min*365 & dias_da_ultima_compra <= gatilho22_max*365 &  Modelo_Atual_original == "ECOSPORT"~ "Maturity",
        dias_da_ultima_compra >= gatilho3_min*365 & dias_da_ultima_compra <= gatilho3_max*365 &  Modelo_Atual_original == "ECOSPORT"~ "Decline",
        TRUE ~ "Lost"
      )
    )

saveRDS(dados_veiculo, paste0("dados/veiculos/dados_gatilho_", tolower(veiculo),".rds"), version = 2)

# }



# Mustang trigger/Luxury car ----
dados_veiculo <- readRDS(paste0("dados/veiculos/dados_carro de luxo.rds"))

veiculo <- "Carro de luxo"
print(paste("veiculo", veiculo))

dados_veiculo0<- dados_veiculo %>% 
  filter(Modelo_Atual_original=="MUSTANG") %>% 
  dplyr::select(ID, Dias_Recompra, Data_Compra, Recomprou, Modelo_Atual, Modelo_Anterior, UF) %>% 
  filter(Recomprou==1) 

fit <- survfit(Surv(Dias_Recompra, Recomprou) ~ 1, data = dados_veiculo0)

df_fit <- data.frame(
  recompra = 1 - fit$surv,
  time = fit$time
) %>% 
  mutate(
    gatilho1 = ifelse(recompra > 0.15 & recompra <= 0.45, 1, 0),
    gatilho2 = ifelse(recompra > 0.35 & recompra <= 0.50, 1, 0),
    gatilho22 = ifelse(recompra > 0.50 & recompra <= 0.7, 1, 0), # I raised the trigger until 70% ----
    gatilho3 = ifelse(recompra > 0.7 & recompra <= 0.85, 1, 0)
  )

gatilho1 <- df_fit %>% filter(gatilho1 == 1) # %>% summary()
gatilho1_min <- gatilho1$time %>% min()
gatilho1_max <- gatilho1$time %>% max()


gatilho2 <- df_fit %>% filter(gatilho2 == 1) # %>% summary()
gatilho2_min <- gatilho2$time %>% min()
gatilho2_max <- gatilho2$time %>% max()

gatilho22 <- df_fit %>% filter(gatilho22 == 1) # %>% summary()
gatilho22_min <- gatilho22$time %>% min()
gatilho22_max <- gatilho22$time %>% max()

gatilho3 <- df_fit %>% filter(gatilho3 == 1) # %>% summary()
gatilho3_min <- gatilho3$time %>% min()
gatilho3_max <- gatilho3$time %>% max()


dados_veiculo <- dados_veiculo %>% 
  group_by(ID) %>% 
  filter(Modelo_Atual_original %in% c("MUSTANG","FOCUS SEDAN")) %>%
  summarise(
    dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
    data_ultima_compra = max(as.Date(Data_Compra)),
    UF = first(UF),
    Modelo_Atual_original = Modelo_Atual_original
  ) %>% 
  distinct() %>% 
  mutate(
    gatilho = case_when(
      ### Mustang triggers being used for Focus Sedan, but with equal time of the other cars
      dias_da_ultima_compra <= gatilho1_min*365 &  Modelo_Atual_original == "FOCUS SEDAN" ~ "Buy Recently",
      dias_da_ultima_compra >= gatilho1_min*365 &  Modelo_Atual_original == "FOCUS SEDAN" & dias_da_ultima_compra <= gatilho1_max*365 &  Modelo_Atual_original == "FOCUS SEDAN" ~ "Grotwh",
      dias_da_ultima_compra >= gatilho2_min*365 &  Modelo_Atual_original == "FOCUS SEDAN" & dias_da_ultima_compra <= gatilho2_max*365  &  Modelo_Atual_original == "FOCUS SEDAN"~ "Maturing",
      dias_da_ultima_compra >= gatilho22_min*365 &  Modelo_Atual_original == "FOCUS SEDAN" & dias_da_ultima_compra <= gatilho22_max*365 &  Modelo_Atual_original == "FOCUS SEDAN"~ "Maturity",
      dias_da_ultima_compra >= gatilho3_min*365 &  Modelo_Atual_original == "FOCUS SEDAN" & dias_da_ultima_compra <= gatilho3_max*365 &  Modelo_Atual_original == "FOCUS SEDAN"~ "Decline",
      ### Mustang triggers (over age)
      dias_da_ultima_compra <= gatilho1_min*9983 ~ "Buy Recently", # Until 1.5 years
      dias_da_ultima_compra >= gatilho1_min*9983 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho1_max*1273 &  Modelo_Atual_original == "MUSTANG"~ "Grotwh", # 1.5 a 3 years
      dias_da_ultima_compra >= gatilho2_min*2247 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho2_max*1771 &  Modelo_Atual_original == "MUSTANG"~ "Maturing", # 3 a 4
      dias_da_ultima_compra >= gatilho22_min*1360 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho22_max*911 &  Modelo_Atual_original == "MUSTANG"~ "Maturity", # 4 a 5
      dias_da_ultima_compra >= gatilho3_min*898 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho3_max*1031 &  Modelo_Atual_original == "MUSTANG"~ "Decline", # 5 a 7
      TRUE ~ "Lost"
    )
  ) 

saveRDS(dados_veiculo, paste0("dados/veiculos/dados_gatilho_", tolower(veiculo),".rds"), version = 2)




# Saving optimal repurchase time data for vehicles (updating always) ----

lista_veiculos_tempo_otimo <- c(# "ranger", Just ran (uncheck next update, commented to save time)
                                #"ecosport",
                                "ka", "k_sedan", "fiesta", "fusion", "focus",
                                #"mustang", 
                                "edge", 
                                "maverick",
                                #"territory",
                                # "fiesta sedan", # Virou FIESTA
                                #"focus sedan", "bronco",
                                "suv",
                                "carro de luxo"
                                )

# Ranger already ran, run from ka
system.time(
  
  for (i in 1:length(lista_veiculos_tempo_otimo)) {
    
    veiculo <- lista_veiculos_tempo_otimo[[i]]
    
    print(paste("veiculo", veiculo))
  
  dados_veiculo_recompra <- readRDS(paste0("dados/veiculos/dados_", veiculo, ".rds"))
  
  dados_plot <- dados_veiculo_recompra %>%
    group_by(ID) %>% 
    summarise(
      ultima_data_compra = max(as.Date(Data_Compra)),
      Dias_Atual = as.Date(Sys.Date()) - ultima_data_compra
    ) %>%
    group_by(Dias_Atual) %>% 
    summarise(N=n())
  
  saveRDS(dados_plot, paste0("dados/tempo_otimo_recompra/dados_tempo_otimo_", tolower(veiculo),".rds"), version = 2)
  
  print(paste("Base de tempo otimo de recompra do veiculo", veiculo, "salva!"))
  }
)

# user ...system elapsed
# 14233.09   1206.83  16541.84  