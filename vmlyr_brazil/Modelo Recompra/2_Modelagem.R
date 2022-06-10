### CLoading the required packages ====
require(plotly)
library(DT)
library(caret)
require(tidyverse)
require(highcharter)
library(knitr)
require(lubridate)
require(shiny)
require(magrittr)
require(zoo)
require(RMySQL)
require(readxl)
require(fastDummies)
require(pROC)
library(FactoMineR)
library(factoextra)
library(kableExtra)
library(rminer)
library(viridisLite)
library(ROCR)
library(lme4)
library(ROCR)
library(PRROC)
library(caretEnsemble)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(xtable)
library(kableExtra)
library(survival)


lista_veiculos <- c("ranger", 
                    # "ecosport",
                    "ka", "k_sedan", "fiesta", "fusion", "focus") # recently added vehicles


##### Before running this line of code, select the database "dados_final_veiculos1" latest and call "dados_final_veiculos1_antigo" ====
dados_prob <- read_delim("dados/dados_txt/dados_final_veiculos1_antigo.txt",delim=";") %>%  # Here we are using the new bank created in step 3
  mutate(id= as.character(id))   


system.time(
  for(i in 1:length(lista_veiculos)){
    
    veiculo<- lista_veiculos[i]
    
    print(paste("veiculo", veiculo))
    
    dados_gatilho <- readRDS(paste0("dados/veiculos/dados_gatilho_",veiculo,".rds")) %>% 
      mutate(NAMEPLATE = paste(toupper(veiculo)))
    
    dados_gatilho_prob <- dados_gatilho %>% 
      left_join(dados_prob,by=c("ID"="id","NAMEPLATE"="veiculo"))
    
    dados_gatilho_prob <- dados_gatilho_prob %>% 
      transmute(ID=ID,
                NAMEPLATE=NAMEPLATE,
                data_ultima_compra = data_ultima_compra.x,
                dias_da_ultima_compra=dias_da_ultima_compra.x,
                UF=UF,
                gatilho = gatilho.x, 
                probabilidade_recompra = probabilidade_recompra,
                utilizou_modelo=utilizou_modelo)
    
    prob_gatilho <- dados_gatilho_prob %>% 
      filter(utilizou_modelo=="nao") %>% 
      group_by(gatilho) %>% 
      summarise(probabilidade_recompra_media=mean(probabilidade_recompra, na.rm = T)) # Fixing problem with NA's in the results
    
    dados_gatilho_prob <- dados_gatilho_prob %>% 
      left_join(prob_gatilho,by=c("gatilho"="gatilho")) %>% 
      mutate(
        probabilidade_recompra = ifelse(is.na(utilizou_modelo),probabilidade_recompra_media,probabilidade_recompra),
        utilizou_modelo = ifelse(is.na(utilizou_modelo),"nao",utilizou_modelo)
      ) %>% 
      select(-9)
    
    caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")
    
    dados_veiculo <- readRDS(caminho) 
    
    dados_recompra_models <- dados_veiculo %>%
      left_join(dados_gatilho_prob,by=c("ID"="ID"))
    
    dados_recompra_models <- dados_recompra_models%>% 
      transmute(
        ID = ID,
        Dias_Atual = Dias_Atual, 
        Dias_Recompra = Dias_Recompra - 2,
        Dias_Recompra2 = Dias_Recompra^2,
        UF = UF.x,
        Data_Compra = Data_Compra,
        Data_Compra_Anterior = Data_Compra_Anterior,
        Modelo_Atual = Modelo_Atual,
        Modelo_Anterior = Modelo_Anterior,
        Recomprou = Recomprou,
        probabilidade_recompra = probabilidade_recompra,
        utilizou_modelo=utilizou_modelo
      ) %>% 
      mutate_if(is.character, str_to_lower) %>%
      select(
        -Data_Compra_Anterior,
        -Modelo_Atual,
        -Modelo_Anterior
      ) %>% 
      na.omit() %>% 
      select_if(function(x) length(unique(x)) > 1) %>%  
      na.omit() %>% 
      # select_if(function(x) length(unique(x)) > 1) %>% 
      # fatorizar() %>%
      # map_dfc(droplevels) %>% 
      mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
             Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
             ID = as.numeric(as.character(ID)))
    
    lista_variaveis_excluir<- list(c("Dias_Atual", "ID", "Data_Compra","UF"),
                                   
                                   c("Dias_Atual", "ID", "Data_Compra","UF"),
                                   
                                   c("Dias_Atual","ID", "Data_Compra","UF"),
                                   
                                   c("Dias_Atual","ID", "Data_Compra","UF"),
                                   
                                   c("Dias_Atual","ID", "Data_Compra","UF"),
                                   
                                   c("Dias_Atual","ID", "Data_Compra","UF"),
                                   
                                    c("Dias_Atual","ID", "Data_Compra","UF")#,
                                   # 
                                   # c("Dias_Atual","ID", "Data_Compra","UF"),
                                   # 
                                   # c("Dias_Atual","ID", "Data_Compra","UF"),
                                   # 
                                   # c("Dias_Atual","ID", "Data_Compra","UF"),
                                   # 
                                   # c("Dias_Atual","ID", "Data_Compra","UF"),
                                   # 
                                   # c("Dias_Atual","ID", "Data_Compra","UF")
                                   ) # Adicionou-se mais 3 linhas para os veiculos novos
    
    
    nomes <- names(dados_recompra_models)
    respostas <- c("Recomprou")
    covariaveis <- nomes[!nomes %in% respostas]
    n_variaveis <- length(covariaveis)
    
    
    covariaveis_modelo <- names(dados_recompra_models)[!names(dados_recompra_models) %in% respostas]
    covariaveis_modelo_F <- covariaveis_modelo[!covariaveis_modelo %in% c(lista_variaveis_excluir[[i]])]
    
    form <- paste("Recomprou ~",paste(paste(covariaveis_modelo_F[1:2], collapse = "+"), "+","utilizou_modelo*probabilidade_recompra","+", "(1|ID)","+","(1|UF)"))
    
    Model_Logistic<- glmer(formula = form, family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
    
    saveRDS(Model_Logistic, paste0("dados/modelos/modelo_", tolower(veiculo),".rds"), version = 2)
    
    
    ## Accuracy
    
    
    Train <- createDataPartition(dados_recompra_models$Recomprou, p=0.7, list=FALSE)
    training <- dados_recompra_models[ Train, ]
    testing  <- dados_recompra_models[-Train, ]
    model_train<- glmer(formula = form, family=binomial, data=training,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
    
    
    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
    
    Resp<- ifelse(prob <prop.table(table(dados_recompra_models$Recomprou))[2], "nao", "sim")
    tab_confusion<- table(Resp, ifelse(testing$Recomprou==1, "sim", "nao"))
    
    # Compute AUC for predicting Class with the model
    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
    pred <- prediction(prob, testing$Recomprou)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]
    
    goodness_of_fit<- round(c(confusionMatrix(tab_confusion, positive="sim")$byClass[c(1,2,3,4)],auc)*100, 2)
    nomes_goodness_of_fit<- rbind("Sensibilidade", "Especificidade", "Valor Preditivo Positivo", "Valor Preditivo Negativo", "AUC")
    tab_goodness_of_fit<- data.frame(Medidas = nomes_goodness_of_fit, Resultado = goodness_of_fit) 
    
    saveRDS(tab_goodness_of_fit, paste0("dados/modelos/tab_goodness_of_fit_", tolower(veiculo),".rds"), version = 2)
    
    ## Resultss 
    
    dados_veiculo <- readRDS(paste0("dados/veiculos/dados_", tolower(veiculo),".rds")) %>% 
      dplyr::select(ID, Dias_Recompra, Recomprou, Modelo_Atual, Modelo_Anterior,  UF) %>% 
      filter(Recomprou==1)
    
    fit <- survfit(Surv(Dias_Recompra, Recomprou) ~ 1, data = dados_veiculo)
    
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
    
    Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
    Percentual<- Percentual[2]*100
    
    dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)
    
    DT_final<- dados_recompra_models %>% 
      select(ID, Dias_Atual, Data_Compra, prob, utilizou_modelo) %>% group_by(ID) %>% 
      summarise(
        Dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))), 
        Data_Compra_Última = max(as.Date(as.character(Data_Compra))), 
        Probabilidade_Recompra = round(mean(prob, na.rm = T)*100,2),
        utilizou_modelo = first(utilizou_modelo)
      )%>%
      distinct() %>% 
      mutate(Dias_Atual_C = case_when(
        Dias_da_ultima_compra <= gatilho1_min*365 ~ "Buy Recently",
        Dias_da_ultima_compra >= gatilho1_min*365 & Dias_da_ultima_compra <= gatilho1_max*365 ~ "Grotwh",
        Dias_da_ultima_compra >= gatilho2_min*365 & Dias_da_ultima_compra <= gatilho2_max*365 ~ "Maturing",
        Dias_da_ultima_compra >= gatilho22_min*365 & Dias_da_ultima_compra <= gatilho22_max*365 ~ "Maturity",
        Dias_da_ultima_compra >= gatilho3_min*365 & Dias_da_ultima_compra <= gatilho3_max*365 ~ "Decline",
        TRUE ~ "Lost"),
        Probabilidade_Recompra_C = case_when(
          Probabilidade_Recompra < Percentual*0.5 ~ "Muito Ruim",
          Probabilidade_Recompra < Percentual*0.8 ~ "Ruim",
          Probabilidade_Recompra < Percentual ~ "Médio",
          Probabilidade_Recompra < Percentual*1.3 ~ "Bom",
          TRUE ~ "Muito Bom"),
      ) 
    
    
    saveRDS(DT_final, paste0("dados/modelos/DT_final_", tolower(veiculo),".rds"), version = 2)
    
    tab_modelo<- summary(Model_Logistic)
    
    saveRDS(tab_modelo, paste0("dados/modelos/tab_modelo_", tolower(veiculo),".rds"), version = 2)
    
  }
)

# time it ran

# user ...system elapsed
# 1584.72    610.28   2733.32



# Vehicles with some probabilities but that did not use the model (Running only with new vehicles or without the variable used the model) ----

lista_veiculos_sem_prob <- c(#"mustang",
                             "edge")

system.time(
  for(i in 1:length(lista_veiculos_sem_prob)){

    veiculo<- "edge"

    print(paste("veiculo", veiculo))

    dados_gatilho <- readRDS(paste0("dados/veiculos/dados_gatilho_",veiculo,".rds")) %>%
      mutate(NAMEPLATE = paste(toupper(veiculo)))

    if(veiculo=="mustang"){
      prob_gatilho <- dados_prob %>%
        filter(veiculo=="FUSION")%>%
        group_by(gatilho) %>%
        summarise(probabilidade_recompra=mean(probabilidade_recompra, na.rm = T))
    }else{
      prob_gatilho <- dados_prob %>%
        filter(veiculo=="ECOSPORT")%>%
        group_by(gatilho) %>%
        summarise(probabilidade_recompra=mean(probabilidade_recompra, na.rm = T))
    }

    dados_gatilho_prob <- dados_gatilho %>%
      left_join(prob_gatilho,by=c("gatilho"="gatilho")) %>%
      mutate(
        utilizou_modelo = "nao"
      )

    caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")

    dados_veiculo <- readRDS(caminho)

    dados_recompra_models <- dados_veiculo %>%
      left_join(dados_gatilho_prob,by=c("ID"="ID"))

    dados_recompra_models <- dados_recompra_models%>%
      transmute(
        ID = ID,
        Dias_Atual = Dias_Atual,
        Dias_Recompra = Dias_Recompra - 2,
        Dias_Recompra2 = Dias_Recompra^2,
        UF = UF.x,
        Data_Compra = Data_Compra,
        Data_Compra_Anterior = Data_Compra_Anterior,
        Modelo_Atual = Modelo_Atual,
        Modelo_Anterior = Modelo_Anterior,
        Recomprou = Recomprou,
        probabilidade_recompra = probabilidade_recompra,
        utilizou_modelo=utilizou_modelo
      ) %>%
      mutate_if(is.character, str_to_lower) %>%
      select(
        -Data_Compra_Anterior,
        -Modelo_Atual,
        -Modelo_Anterior
      ) %>%
      na.omit() %>%
      # select_if(function(x) length(unique(x)) > 1) %>%
      # fatorizar() %>%
      # map_dfc(droplevels) %>%
      mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
             Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
             ID = as.numeric(as.character(ID)))

    lista_variaveis_excluir<- c("Dias_Atual", "ID", "Data_Compra","UF","utilizou_modelo")

    nomes <- names(dados_recompra_models)
    respostas <- c("Recomprou")
    covariaveis <- nomes[!nomes %in% respostas]
    n_variaveis <- length(covariaveis)


    covariaveis_modelo <- names(dados_recompra_models)[!names(dados_recompra_models) %in% respostas]
    covariaveis_modelo_F <- covariaveis_modelo[!covariaveis_modelo %in% c(lista_variaveis_excluir)]

    form <- paste("Recomprou ~",paste(paste(covariaveis_modelo_F, collapse = "+"),"+", "(1|ID)","+","(1|UF)"))

    Model_Logistic<- glmer(formula = form, family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))

    saveRDS(Model_Logistic, paste0("dados/modelos/modelo_", tolower(veiculo),".rds"), version = 2)

    ## Accuracy


    Train <- createDataPartition(dados_recompra_models$Recomprou, p=0.7, list=FALSE)
    training <- dados_recompra_models[ Train, ]
    testing  <- dados_recompra_models[-Train, ]
    model_train<- glmer(formula = form, family=binomial, data=training,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))


    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)

    Resp<- ifelse(prob <prop.table(table(dados_recompra_models$Recomprou))[2], "nao", "sim")
    tab_confusion<- table(Resp, ifelse(testing$Recomprou==1, "sim", "nao"))

    # Compute AUC for predicting Class with the model
    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
    pred <- prediction(prob, testing$Recomprou)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]

    goodness_of_fit<- round(c(confusionMatrix(tab_confusion, positive="sim")$byClass[c(1,2,3,4)],auc)*100, 2)
    nomes_goodness_of_fit<- rbind("Sensibilidade", "Especificidade", "Valor Preditivo Positivo", "Valor Preditivo Negativo", "AUC")
    tab_goodness_of_fit<- data.frame(Medidas = nomes_goodness_of_fit, Resultado = goodness_of_fit)

    saveRDS(tab_goodness_of_fit, paste0("dados/modelos/tab_goodness_of_fit_", tolower(veiculo),".rds"), version = 2)

    ## Results

    dados_veiculo <- readRDS(paste0("dados/veiculos/dados_", tolower(veiculo),".rds")) %>%
      dplyr::select(ID, Dias_Recompra, Recomprou, Modelo_Atual, Modelo_Anterior,  UF) %>%
      filter(Recomprou==1)

    fit <- survfit(Surv(Dias_Recompra, Recomprou) ~ 1, data = dados_veiculo)

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

    Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
    Percentual<- Percentual[2]*100

    dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)

    DT_final<- dados_recompra_models %>%
      select(ID, Dias_Atual, Data_Compra, prob, utilizou_modelo) %>% group_by(ID) %>%
      summarise(
        Dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
        Data_Compra_Última = max(as.Date(as.character(Data_Compra))),
        Probabilidade_Recompra = round(mean(prob, na.rm = T)*100,2),
        utilizou_modelo = first(utilizou_modelo)
      )%>%
      distinct() %>%
      mutate(Dias_Atual_C = case_when(
        Dias_da_ultima_compra <= gatilho1_min*365 ~ "Buy Recently",
        Dias_da_ultima_compra >= gatilho1_min*365 & Dias_da_ultima_compra <= gatilho1_max*365 ~ "Grotwh",
        Dias_da_ultima_compra >= gatilho2_min*365 & Dias_da_ultima_compra <= gatilho2_max*365 ~ "Maturing",
        Dias_da_ultima_compra >= gatilho22_min*365 & Dias_da_ultima_compra <= gatilho22_max*365 ~ "Maturity",
        Dias_da_ultima_compra >= gatilho3_min*365 & Dias_da_ultima_compra <= gatilho3_max*365 ~ "Decline",
        TRUE ~ "Lost"),
        Probabilidade_Recompra_C = case_when(
          Probabilidade_Recompra < Percentual*0.5 ~ "Muito Ruim",
          Probabilidade_Recompra < Percentual*0.8 ~ "Ruim",
          Probabilidade_Recompra < Percentual ~ "Médio",
          Probabilidade_Recompra < Percentual*1.3 ~ "Bom",
          TRUE ~ "Muito Bom"),
      )


    saveRDS(DT_final, paste0("dados/modelos/DT_final_", tolower(veiculo),".rds"), version = 2)

    tab_modelo<- summary(Model_Logistic)

    saveRDS(tab_modelo, paste0("dados/modelos/tab_modelo_", tolower(veiculo),".rds"), version = 2)

  }
)

#user ...system elapsed 
# 9.75      2.73     89.77 



##### Maverick ----
lista_veiculos_sem_prob <- c(
  "maverick"
)

system.time(
  
  veiculo<- "maverick"
  
  print(paste("veiculo", veiculo))
  
  dados_gatilho <- readRDS(paste0("dados/veiculos/dados_gatilho_maverick.rds")) %>%
    mutate(NAMEPLATE = paste(toupper(veiculo))) 
  
  
  prob_gatilho <- dados_prob %>%
    filter(veiculo=="ECOSPORT")%>% # ECOSPORT?
    group_by(gatilho) %>%
    summarise(probabilidade_recompra=mean(probabilidade_recompra, na.rm = T)) # Added "na.rm = T" as it was resulting in NA at the base
  
  
  dados_gatilho_prob <- dados_gatilho %>%
    left_join(prob_gatilho,by=c("gatilho"="gatilho")) %>%
    mutate(
      utilizou_modelo = "nao"
    )
  
  caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")
  
  dados_veiculo <- readRDS(caminho)
  
  dados_recompra_models <- dados_veiculo %>%
    left_join(dados_gatilho_prob,by=c("ID"="ID"))
  
  dados_recompra_models <- dados_recompra_models%>%
    transmute(
      ID = ID,
      Dias_Atual = Dias_Atual,
      Dias_Recompra = ifelse(Dias_Recompra < 2,Dias_Recompra,Dias_Recompra - 2),
      Dias_Recompra2 = Dias_Recompra^2,
      UF = UF.x,
      Data_Compra = Data_Compra,
      Data_Compra_Anterior = Data_Compra_Anterior,
      Modelo_Atual = Modelo_Atual,
      Modelo_Anterior = Modelo_Anterior,
      Recomprou = Recomprou,
      probabilidade_recompra = probabilidade_recompra,
      utilizou_modelo=utilizou_modelo
    ) %>%
    mutate_if(is.character, str_to_lower) %>%
    select(
      -Data_Compra_Anterior,
      -Modelo_Atual,
      -Modelo_Anterior
    ) %>%
    na.omit() %>%
    # select_if(function(x) length(unique(x)) > 1) %>%
    # fatorizar() %>%
    # map_dfc(droplevels) %>%
    mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
           Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
           ID = as.numeric(as.character(ID)))
  
  lista_variaveis_excluir<- c("Dias_Atual", "ID", "Data_Compra","UF","utilizou_modelo")
  
  nomes <- names(dados_recompra_models)
  respostas <- c("Recomprou")
  covariaveis <- nomes[!nomes %in% respostas]
  n_variaveis <- length(covariaveis)
  
  
  covariaveis_modelo <- names(dados_recompra_models)[!names(dados_recompra_models) %in% respostas]
  covariaveis_modelo_F <- covariaveis_modelo[!covariaveis_modelo %in% c(lista_variaveis_excluir)]
  
  form <- paste("Recomprou ~",paste(paste(covariaveis_modelo_F, collapse = "+"),"+", "(1|ID)","+","(1|UF)"))
  
  Model_Logistic<- glmer(formula = form, family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
  
  saveRDS(Model_Logistic, paste0("dados/modelos/modelo_", tolower(veiculo),".rds"), version = 2)
  
  ## Accuracy
  
  
  Train <- createDataPartition(dados_recompra_models$Recomprou, p=0.7, list=FALSE)
  training <- dados_recompra_models[ Train, ]
  testing  <- dados_recompra_models[-Train, ]
  model_train<- glmer(formula = form, family=binomial, data=training,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
  
  
  prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
  
  Resp<- ifelse(prob <prop.table(table(dados_recompra_models$Recomprou))[2], "nao", "sim")
  tab_confusion<- table(Resp, ifelse(testing$Recomprou==1, "sim", "nao"))
  
  # Compute AUC for predicting Class with the model
  prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
  pred <- prediction(prob, testing$Recomprou)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  
  goodness_of_fit<- round(c(confusionMatrix(tab_confusion, positive="sim")$byClass[c(1,2,3,4)],auc)*100, 2)
  nomes_goodness_of_fit<- rbind("Sensibilidade", "Especificidade", "Valor Preditivo Positivo", "Valor Preditivo Negativo", "AUC")
  tab_goodness_of_fit<- data.frame(Medidas = nomes_goodness_of_fit, Resultado = goodness_of_fit)
  
  saveRDS(tab_goodness_of_fit, paste0("dados/modelos/tab_goodness_of_fit_", tolower(veiculo),".rds"), version = 2)
  
  ## Results
  
  Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
  Percentual<- Percentual[2]*100
  
  dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)
  
  DT_final<- dados_recompra_models %>%
    select(ID, Dias_Atual, Data_Compra, prob, utilizou_modelo) %>%
    group_by(ID) %>%
    summarise(
      Dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
      Data_Compra_Última = max(as.Date(as.character(Data_Compra))),
      Probabilidade_Recompra = round(mean(prob, na.rm = T)*100,2),
      utilizou_modelo = first(utilizou_modelo)
    )%>%
    distinct() %>%
    mutate(Dias_Atual_C = case_when(
      Dias_da_ultima_compra <= 0.5*365 ~ "Buy Recently",
      TRUE ~ "Grotwh"
    ),
    Probabilidade_Recompra_C = case_when(
      Probabilidade_Recompra < Percentual*0.5 ~ "Muito Ruim",
      Probabilidade_Recompra < Percentual*0.8 ~ "Ruim",
      Probabilidade_Recompra < Percentual ~ "Médio",
      Probabilidade_Recompra < Percentual*1.3 ~ "Bom",
      TRUE ~ "Muito Bom")
    )
  
  
  saveRDS(DT_final, paste0("dados/modelos/DT_final_", tolower(veiculo),".rds"), version = 2)
  
  tab_modelo<- summary(Model_Logistic)
  
  saveRDS(tab_modelo, paste0("dados/modelos/tab_modelo_", tolower(veiculo),".rds"), version = 2)
  
)




##### SUV ----
lista_veiculos_sem_prob <- c(
 "SUV"
)

system.time(

    veiculo<- "suv"
    
    print(paste("veiculo", veiculo))
    
    dados_gatilho <- readRDS(paste0("dados/veiculos/dados_gatilho_suv.rds")) %>%
      mutate(NAMEPLATE = paste(toupper(veiculo))) 
    
    
    prob_gatilho <- dados_prob %>%
      filter(veiculo=="ECOSPORT")%>% # ECOSPORT?
      group_by(gatilho) %>%
      summarise(probabilidade_recompra=mean(probabilidade_recompra, na.rm = T)) # Added "na.rm = T" as it was resulting in NA at the base
    
    
    dados_gatilho_prob <- dados_gatilho %>%
      left_join(prob_gatilho,by=c("gatilho"="gatilho")) %>%
      mutate(
        utilizou_modelo = "nao"
      )
    
    caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")
    
    dados_veiculo <- readRDS(caminho)
    
    dados_recompra_models <- dados_veiculo %>%
      left_join(dados_gatilho_prob,by=c("ID"="ID"))
    
    dados_recompra_models <- dados_recompra_models%>%
      transmute(
        ID = ID,
        Dias_Atual = Dias_Atual,
        Dias_Recompra = ifelse(Dias_Recompra < 2,Dias_Recompra,Dias_Recompra - 2),
        Dias_Recompra2 = Dias_Recompra^2,
        UF = UF.x,
        Data_Compra = Data_Compra,
        Data_Compra_Anterior = Data_Compra_Anterior,
        Modelo_Atual = Modelo_Atual,
        Modelo_Anterior = Modelo_Anterior,
        Recomprou = Recomprou,
        probabilidade_recompra = probabilidade_recompra,
        utilizou_modelo=utilizou_modelo
      ) %>%
      mutate_if(is.character, str_to_lower) %>%
      select(
        -Data_Compra_Anterior,
        -Modelo_Atual,
        -Modelo_Anterior
      ) %>%
      na.omit() %>%
      # select_if(function(x) length(unique(x)) > 1) %>%
      # fatorizar() %>%
      # map_dfc(droplevels) %>%
      mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
             Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
             ID = as.numeric(as.character(ID)))
    
    lista_variaveis_excluir<- c("Dias_Atual", "ID", "Data_Compra","UF","utilizou_modelo")
    
    nomes <- names(dados_recompra_models)
    respostas <- c("Recomprou")
    covariaveis <- nomes[!nomes %in% respostas]
    n_variaveis <- length(covariaveis)
    
    
    covariaveis_modelo <- names(dados_recompra_models)[!names(dados_recompra_models) %in% respostas]
    covariaveis_modelo_F <- covariaveis_modelo[!covariaveis_modelo %in% c(lista_variaveis_excluir)]
    
    form <- paste("Recomprou ~",paste(paste(covariaveis_modelo_F, collapse = "+"),"+", "(1|ID)","+","(1|UF)"))
    
    Model_Logistic<- glmer(formula = form, family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
    
    saveRDS(Model_Logistic, paste0("dados/modelos/modelo_", tolower(veiculo),".rds"), version = 2)
    
    ## Accuracy
    
    
    Train <- createDataPartition(dados_recompra_models$Recomprou, p=0.7, list=FALSE)
    training <- dados_recompra_models[ Train, ]
    testing  <- dados_recompra_models[-Train, ]
    model_train<- glmer(formula = form, family=binomial, data=training,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
    
    
    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
    
    Resp<- ifelse(prob <prop.table(table(dados_recompra_models$Recomprou))[2], "nao", "sim")
    tab_confusion<- table(Resp, ifelse(testing$Recomprou==1, "sim", "nao"))
    
    # Compute AUC for predicting Class with the model
    prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
    pred <- prediction(prob, testing$Recomprou)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    auc <- performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]
    
    goodness_of_fit<- round(c(confusionMatrix(tab_confusion, positive="sim")$byClass[c(1,2,3,4)],auc)*100, 2)
    nomes_goodness_of_fit<- rbind("Sensibilidade", "Especificidade", "Valor Preditivo Positivo", "Valor Preditivo Negativo", "AUC")
    tab_goodness_of_fit<- data.frame(Medidas = nomes_goodness_of_fit, Resultado = goodness_of_fit)
    
    saveRDS(tab_goodness_of_fit, paste0("dados/modelos/tab_goodness_of_fit_", tolower(veiculo),".rds"), version = 2)
    
    ## Results
    
    Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
    Percentual<- Percentual[2]*100
    
    dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)
    
    DT_final<- dados_recompra_models %>%
      select(ID, Dias_Atual, Data_Compra, prob, utilizou_modelo) %>%
      group_by(ID) %>%
      summarise(
        Dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
        Data_Compra_Última = max(as.Date(as.character(Data_Compra))),
        Probabilidade_Recompra = round(mean(prob, na.rm = T)*100,2),
        utilizou_modelo = first(utilizou_modelo)
      )%>%
      distinct() %>%
      mutate(Dias_Atual_C = case_when(
        Dias_da_ultima_compra <= 0.5*365 ~ "Buy Recently",
        TRUE ~ "Grotwh"
      ),
      Probabilidade_Recompra_C = case_when(
        Probabilidade_Recompra < Percentual*0.5 ~ "Muito Ruim",
        Probabilidade_Recompra < Percentual*0.8 ~ "Ruim",
        Probabilidade_Recompra < Percentual ~ "Médio",
        Probabilidade_Recompra < Percentual*1.3 ~ "Bom",
        TRUE ~ "Muito Bom")
      )
    
    
    saveRDS(DT_final, paste0("dados/modelos/DT_final_", tolower(veiculo),".rds"), version = 2)
    
    tab_modelo<- summary(Model_Logistic)
    
    saveRDS(tab_modelo, paste0("dados/modelos/tab_modelo_", tolower(veiculo),".rds"), version = 2)
    
)


##### Luxury car ----

lista_veiculos_sem_prob <- c(
  "Carro de Luxo"
)

system.time(
  
  veiculo<- "carro de luxo"
  
  print(paste("veiculo", veiculo))
  
  dados_gatilho <- readRDS(paste0("dados/veiculos/dados_gatilho_carro de luxo.rds")) %>%
    mutate(NAMEPLATE = paste(toupper(veiculo))) 
  
  
  prob_gatilho <- dados_prob %>%
    filter(veiculo=="ECOSPORT")%>% # ECOSPORT?
    group_by(gatilho) %>%
    summarise(probabilidade_recompra=mean(probabilidade_recompra, na.rm = T)) # Added "na.rm = T" as it was resulting in NA at the base
  
  
  dados_gatilho_prob <- dados_gatilho %>%
    left_join(prob_gatilho,by=c("gatilho"="gatilho")) %>%
    mutate(
      utilizou_modelo = "nao"
    )
  
  caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")
  
  dados_veiculo <- readRDS(caminho)
  
  dados_recompra_models <- dados_veiculo %>%
    left_join(dados_gatilho_prob,by=c("ID"="ID"))
  
  dados_recompra_models <- dados_recompra_models%>%
    transmute(
      ID = ID,
      Dias_Atual = Dias_Atual,
      Dias_Recompra = ifelse(Dias_Recompra < 2,Dias_Recompra,Dias_Recompra - 2),
      Dias_Recompra2 = Dias_Recompra^2,
      UF = UF.x,
      Data_Compra = Data_Compra,
      Data_Compra_Anterior = Data_Compra_Anterior,
      Modelo_Atual = Modelo_Atual,
      Modelo_Anterior = Modelo_Anterior,
      Recomprou = Recomprou,
      probabilidade_recompra = probabilidade_recompra,
      utilizou_modelo=utilizou_modelo
    ) %>%
    mutate_if(is.character, str_to_lower) %>%
    select(
      -Data_Compra_Anterior,
      -Modelo_Atual,
      -Modelo_Anterior
    ) %>%
    na.omit() %>%
    # select_if(function(x) length(unique(x)) > 1) %>%
    # fatorizar() %>%
    # map_dfc(droplevels) %>%
    mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
           Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
           ID = as.numeric(as.character(ID)))
  
  lista_variaveis_excluir<- c("Dias_Atual", "ID", "Data_Compra","UF","utilizou_modelo")
  
  nomes <- names(dados_recompra_models)
  respostas <- c("Recomprou")
  covariaveis <- nomes[!nomes %in% respostas]
  n_variaveis <- length(covariaveis)
  
  
  covariaveis_modelo <- names(dados_recompra_models)[!names(dados_recompra_models) %in% respostas]
  covariaveis_modelo_F <- covariaveis_modelo[!covariaveis_modelo %in% c(lista_variaveis_excluir)]
  
  form <- paste("Recomprou ~",paste(paste(covariaveis_modelo_F, collapse = "+"),"+", "(1|ID)","+","(1|UF)"))
  
  Model_Logistic<- glmer(formula = form, family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
  
  saveRDS(Model_Logistic, paste0("dados/modelos/modelo_", tolower(veiculo),".rds"), version = 2)
  
  ## Accuracy
  
  
  Train <- createDataPartition(dados_recompra_models$Recomprou, p=0.7, list=FALSE)
  training <- dados_recompra_models[ Train, ]
  testing  <- dados_recompra_models[-Train, ]
  model_train<- glmer(formula = form, family=binomial, data=training,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
  
  
  prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
  
  Resp<- ifelse(prob <prop.table(table(dados_recompra_models$Recomprou))[2], "nao", "sim")
  tab_confusion<- table(Resp, ifelse(testing$Recomprou==1, "sim", "nao"))
  
  # Compute AUC for predicting Class with the model
  prob <- predict(model_train, newdata=testing, type="response", allow.new.levels = TRUE)
  pred <- prediction(prob, testing$Recomprou)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  
  goodness_of_fit<- round(c(confusionMatrix(tab_confusion, positive="sim")$byClass[c(1,2,3,4)],auc)*100, 2)
  nomes_goodness_of_fit<- rbind("Sensibilidade", "Especificidade", "Valor Preditivo Positivo", "Valor Preditivo Negativo", "AUC")
  tab_goodness_of_fit<- data.frame(Medidas = nomes_goodness_of_fit, Resultado = goodness_of_fit)
  
  saveRDS(tab_goodness_of_fit, paste0("dados/modelos/tab_goodness_of_fit_", tolower(veiculo),".rds"), version = 2)
  
  ## Results
  
  Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
  Percentual<- Percentual[2]*100
  
  dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)
  
  DT_final<- dados_recompra_models %>%
    select(ID, Dias_Atual, Data_Compra, prob, utilizou_modelo) %>% group_by(ID) %>%
    summarise(
      Dias_da_ultima_compra = min(as.numeric(as.character(Dias_Atual))),
      Data_Compra_Última = max(as.Date(as.character(Data_Compra))),
      Probabilidade_Recompra = round(mean(prob, na.rm = T)*100,2),
      utilizou_modelo = first(utilizou_modelo)
    )%>%
    distinct() %>%
    mutate(Dias_Atual_C = case_when(
      Dias_da_ultima_compra <= 0.5*365 ~ "Buy Recently",
      TRUE ~ "Grotwh"
    ),
    Probabilidade_Recompra_C = case_when(
      Probabilidade_Recompra < Percentual*0.5 ~ "Muito Ruim",
      Probabilidade_Recompra < Percentual*0.8 ~ "Ruim",
      Probabilidade_Recompra < Percentual ~ "Médio",
      Probabilidade_Recompra < Percentual*1.3 ~ "Bom",
      TRUE ~ "Muito Bom")
    )
  
  
  saveRDS(DT_final, paste0("dados/modelos/DT_final_", tolower(veiculo),".rds"), version = 2)
  
  tab_modelo<- summary(Model_Logistic)
  
  saveRDS(tab_modelo, paste0("dados/modelos/tab_modelo_", tolower(veiculo),".rds"), version = 2)
  
)


############### Models per UF ----

library(tidyverse)
library(lme4)


lista_veiculos <- c("ranger",
                    # "ecosport", 
                    "ka", "k_sedan", "fiesta", "fusion", "focus", 
                    #"mustang", 
                    "edge", 
                    "maverick", # Novo veiculo
                    #"territory",
                    # "fiesta sedan", # Virou FIESTA
                    #"focus sedan", "bronco"
                    "suv",
                    "carro de luxo"
                    )

# i <- 9

for(i in 1:length(lista_veiculos)){
  
  veiculo<- lista_veiculos[i]
  
  print(paste("veiculo", veiculo))
  
  caminho<- paste0("dados/veiculos/dados_", veiculo, ".rds")
  
  source("./atualizar_dados_recompra/Funcoes.R", encoding = "UTF-8")
  
  if(veiculo=="carro de luxo"){
  dados_recompra_models<- readRDS(caminho) %>%
    transmute(
      UF =  trimws(as.character(toupper(UF))),
      Dias_Recompra = ifelse(Dias_Recompra < 2,Dias_Recompra,Dias_Recompra - 2),
      Dias_Recompra2 = Dias_Recompra^2,
      Recomprou = Recomprou
    ) %>%
    mutate_if(is.character, str_to_lower) %>%
    na.omit() %>%
    filter(UF != "", UF != "ZZ") %>%
    mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
           Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)))
  } else {
    dados_recompra_models<- readRDS(caminho) %>%
      transmute(
        UF =  trimws(as.character(toupper(UF))),
        Dias_Recompra = Dias_Recompra - 2,
        Dias_Recompra2 = Dias_Recompra^2,
        Recomprou = Recomprou
      ) %>%
      mutate_if(is.character, str_to_lower) %>%
      na.omit() %>%
      filter(UF != "", UF != "ZZ") %>%
      mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
             Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)))
  }
  
  Model_Logistic<- glmer(Recomprou ~ Dias_Recompra + Dias_Recompra2 + (Dias_Recompra + Dias_Recompra2|UF), family=binomial, data=dados_recompra_models,  nAGQ=0, control=glmerControl(optimizer = "nloptwrap"))
  
  dados_recompra_models$prob <- predict(Model_Logistic, type="response", allow.new.levels = TRUE)
  
  Tab_UF<- dados_recompra_models %>%
    group_by(UF) %>%
    summarise(Prob = mean(prob, na.rm = T))
  
  saveRDS(
    Tab_UF,
    paste0("dados/modelos/Tab_UF_", tolower(veiculo),".rds"),
    version = 2
  )
  
}