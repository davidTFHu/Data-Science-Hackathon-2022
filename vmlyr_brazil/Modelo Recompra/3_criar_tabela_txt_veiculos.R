
library(tidyverse)
library(survival)


processar_dados_txt_veiculos <- function() {
  
  lista_veiculos <-  c("RANGER", 
                       #"ECOSPORT", 
                       "KA",  "K_SEDAN", "FIESTA", "FUSION", "FOCUS", 
                       #"MUSTANG",
                       "EDGE",
                       "MAVERICK", # Novo Veiculo
                       #"TERRITORY",
                       "FIESTA SEDAN",
                       #"FOCUS SEDAN", "BRONCO"
                       "SUV",
                       "Carro de Luxo"
  )
  
  for (i in 1:length(lista_veiculos)) {
    
    veiculo <- lista_veiculos[[i]]
    
    print(paste("veiculo", veiculo))
    
    dados_veiculo <- readRDS(paste0("dados/veiculos/dados_", tolower(veiculo),".rds"))
    
    # if(veiculo=="TERRITORY" | veiculo== "BRONCO"){
    
    if(veiculo=="SUV"){
      
      dados_veiculo0<- dados_veiculo %>% 
        filter(Modelo_Atual_original=="ECOSPORT") %>% 
        dplyr::select(ID, Dias_Recompra, Data_Compra, Recomprou, Modelo_Atual_original, Modelo_Anterior, UF) %>%  
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
      
      DT_final <- dados_veiculo %>% 
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
          ),
          id = as.character(ID)
        )
      
    }else{
      
      if(veiculo=="Carro de Luxo"){ # It used to be Mustang
        
        dados_veiculo0<- dados_veiculo %>% 
          filter(Modelo_Atual_original=="MUSTANG") %>% 
          dplyr::select(ID, Dias_Recompra, Data_Compra, Recomprou, Modelo_Atual_original, Modelo_Anterior, UF) %>% 
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
        
        
        DT_final <- dados_veiculo %>% 
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
              ### Triggers Mustang (over age)
              dias_da_ultima_compra <= gatilho1_min*9983 ~ "Buy Recently", # until 1.5 years
              dias_da_ultima_compra >= gatilho1_min*9983 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho1_max*1273 &  Modelo_Atual_original == "MUSTANG"~ "Grotwh", # 1.5 a 3 years
              dias_da_ultima_compra >= gatilho2_min*2247 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho2_max*1771 &  Modelo_Atual_original == "MUSTANG"~ "Maturing", # 3 a 4
              dias_da_ultima_compra >= gatilho22_min*1360 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho22_max*911 &  Modelo_Atual_original == "MUSTANG"~ "Maturity", # 4 a 5
              dias_da_ultima_compra >= gatilho3_min*898 &  Modelo_Atual_original == "MUSTANG" & dias_da_ultima_compra <= gatilho3_max*1031 &  Modelo_Atual_original == "MUSTANG"~ "Decline", # 5 a 7
              TRUE ~ "Lost"
            ),
            id = as.character(ID) # RRenaming id column to become a character
          ) 
      }else{
        
        if(veiculo=="MAVERICK"){
          
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
          
          
          DT_final <- dados_veiculo%>% 
            mutate(id = as.character(ID)) %>% 
            group_by(id) %>% 
            summarise(
              dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
              data_ultima_compra = max(as.Date(Data_Compra)),
              UF = first(UF)
            ) %>% 
            mutate(
              gatilho = case_when(
                dias_da_ultima_compra <= 0.5*365 ~ "Buy Recently",
                dias_da_ultima_compra > 0.5*365 ~ "Grotwh",
                TRUE ~ "Lost"
              )
            ) 
          
        }else{
          
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
              gatilho22 = ifelse(recompra > 0.50 & recompra < 0.7, 1, 0),
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
          
          
          DT_final <- dados_veiculo %>% 
            mutate(id = as.character(ID)) %>% 
            dplyr::select(id, Dias_Atual, Data_Compra) %>% 
            group_by(id) %>% 
            summarise(
              dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
              data_ultima_compra = max(as.Date(Data_Compra))
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
          
        }
      }
    }
    
    dados_modelo <- readRDS(paste0("dados/modelos/DT_final_", tolower(veiculo), ".rds")) %>% 
      mutate(id = as.character(ID)) %>% 
      dplyr::select(id, 
                    probabilidade_recompra = Probabilidade_Recompra, 
                    class_prob_recompra = `Probabilidade_Recompra_C`,
                    utilizou_modelo)
    
    if(veiculo=="SUV" | veiculo=="Carro de Luxo"){
      dados_final <- left_join(DT_final, dados_modelo, by = "id") %>%
        mutate(
          id = as.character(id),
          veiculo = as.character(Modelo_Atual_original),
          `data_ultima_compra` = as.character(`data_ultima_compra`),
          dias_da_ultima_compra = as.character(dias_da_ultima_compra),
          probabilidade_recompra = as.character(probabilidade_recompra),
          gatilho = as.character(gatilho),
          class_prob_recompra = as.character(class_prob_recompra)
        ) %>% 
        dplyr::select(`veiculo`, id, `data_ultima_compra`, 
                      dias_da_ultima_compra, probabilidade_recompra, 
                      gatilho, class_prob_recompra,utilizou_modelo)
    } else {
      dados_final <- left_join(DT_final, dados_modelo, by = "id") %>%
        mutate(
          id = as.character(id),
          veiculo = as.character(veiculo),
          `data_ultima_compra` = as.character(`data_ultima_compra`),
          dias_da_ultima_compra = as.character(dias_da_ultima_compra),
          probabilidade_recompra = as.character(probabilidade_recompra),
          gatilho = as.character(gatilho),
          class_prob_recompra = as.character(class_prob_recompra)
        ) %>% 
        dplyr::select(`veiculo`, id, `data_ultima_compra`, 
                      dias_da_ultima_compra, probabilidade_recompra, 
                      gatilho, class_prob_recompra,utilizou_modelo)
    }
    
    
    write.table(dados_final, file = paste0("dados/dados_txt/dados_veiculo_", tolower(veiculo),".txt"), sep = ";")
    
    message(paste("-- Base txt ",  veiculo," - Salva!"))
    
  }
  
}



juntar_os_dados_txt_veiculos <- function() {
  
  df_ranger <- read.csv2("dados/dados_txt/dados_veiculo_ranger.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  message("Carregado dados ranger")
  
  # df_ecosport <- read.csv2("dados/dados_txt/dados_veiculo_ecosport.txt", sep = ";",stringsAsFactors=FALSE) %>% 
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados ecosport")
  
  df_ka <- read.csv2("dados/dados_txt/dados_veiculo_ka.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  message("Carregado dados ka")
  
  df_ka_sedan <- read.csv2("dados/dados_txt/dados_veiculo_k_sedan.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  message("Carregado dados ka sedan")
  
  df_fiesta <- read.csv2("dados/dados_txt/dados_veiculo_fiesta.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  
  message("Carregado dados fiesta")
  
  df_fusion <- read.csv2("dados/dados_txt/dados_veiculo_fusion.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  message("Carregado dados fusion")
  
  df_focus <- read.csv2("dados/dados_txt/dados_veiculo_focus.txt", sep = ";",stringsAsFactors=FALSE) %>% 
    as.data.frame(stringsAsFactors=FALSE)
  
  message("Carregado dados focus")
  
  # df_mustang <- read.csv2("dados/dados_txt/dados_veiculo_mustang.txt", sep = ";",stringsAsFactors=FALSE) %>%
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados mustang")

  df_edge <- read.csv2("dados/dados_txt/dados_veiculo_edge.txt", sep = ";",stringsAsFactors=FALSE) %>%
    as.data.frame(stringsAsFactors=FALSE)

  message("Carregado dados edge")
  
  df_maverick <- read.csv2("dados/dados_txt/dados_veiculo_maverick.txt", sep = ";",stringsAsFactors=FALSE) %>%
    as.data.frame(stringsAsFactors=FALSE) # new vehicle
  
  message("Carregado dados maverick")

  # df_territory <- read.csv2("dados/dados_txt/dados_veiculo_territory.txt", sep = ";",stringsAsFactors=FALSE) %>%
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados territory")
  
  # df_fiestasedan <- read.csv2("dados/dados_txt/dados_veiculo_fiesta sedan.txt", sep = ";",stringsAsFactors=FALSE) %>%
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados fiesta sedan")
  # 
  # df_focussedan <- read.csv2("dados/dados_txt/dados_veiculo_focus sedan.txt", sep = ";",stringsAsFactors=FALSE) %>%
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados focus sedan")
  # 
  # df_bronco <- read.csv2("dados/dados_txt/dados_veiculo_bronco.txt", sep = ";",stringsAsFactors=FALSE) %>%
  #   as.data.frame(stringsAsFactors=FALSE)
  # 
  # message("Carregado dados bronco")
  
  df_SUV <- read.csv2("dados/dados_txt/dados_veiculo_suv.txt", sep = ";",stringsAsFactors=FALSE) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% 
    select(-ID)  # Removing duplicate column

  message("Carregado dados SUV")
  
  df_carrodeluxo <- read.csv2("dados/dados_txt/dados_veiculo_carro de luxo.txt", sep = ";",stringsAsFactors=FALSE) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% 
    select(-ID)  # Removing duplicate columns

  message("Carregado dados Carro de Luxo")
  
  # dados_final_veiculos <- rbind(df_ranger, df_ecosport)
  dados_final_veiculos <- rbind(df_ranger, df_ka)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_ka_sedan)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_fiesta)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_fusion)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_focus)
  # dados_final_veiculos <- rbind(dados_final_veiculos, df_mustang)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_edge)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_maverick) # Novo veiculo
  # dados_final_veiculos <- rbind(dados_final_veiculos, df_territory)
  # dados_final_veiculos <- rbind(dados_final_veiculos, df_fiestasedan)
  # dados_final_veiculos <- rbind(dados_final_veiculos, df_focussedan)
  # dados_final_veiculos <- rbind(dados_final_veiculos, df_bronco)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_SUV)
  dados_final_veiculos <- rbind(dados_final_veiculos, df_carrodeluxo)
  
  message("Criada base final veículos!")
  
  write.table(dados_final_veiculos, file = paste0("dados/dados_txt/dados_final_veiculos.txt"), sep = ";")
  
  message("Base final veículos - Salva!")
  
}


processar_dados_txt_veiculos()
juntar_os_dados_txt_veiculos()


dt<- read.delim("dados/dados_txt/dados_final_veiculos.txt", sep=";") %>% 
  group_by(id,veiculo) %>% 
  mutate(
    tempo_compra_min = min(dias_da_ultima_compra),
    filtro = ifelse(dias_da_ultima_compra==tempo_compra_min,1,0)
  ) %>% 
  filter(filtro==1) %>% 
  select(-c(9,10)) %>% 
  filter(!is.na(id)) # unbase lines without cpf



# ** Final base - climb the GTB 'server' ====
write.table(dt, file = paste0("dados/dados_txt/dados_final_veiculos1.txt"), sep = ";", row.names = F) # Removing the names of the lines to not affect in part 2
