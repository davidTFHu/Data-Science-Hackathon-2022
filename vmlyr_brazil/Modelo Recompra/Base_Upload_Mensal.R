#################### Monthly Upload GTB Server - Repurchase =====================

##### Loading required packages
library("tidyverse")


##### Loading the necessary database to check for possible errors ====
dados_resultado_modelo_recompra <- reactive({
  
  lista_veiculos <- c("geral",
                      "ranger",
                      #"ecosport",
                      "ka", 
                      "k_sedan",
                      "fiesta", 
                      "fusion", 
                      "focus",
                      #"mustang", 
                      "edge", 
                      "maverick", # Novo veiculo
                      #"territory", 
                      # "fiesta sedan", 
                      # "focus sedan", "bronco",
                      "suv",
                      "carro de luxo"
  )
  
  i <- 1
  input_veiculo <- lista_veiculos[i]
  input_veiculo <- "geral"
  dados_resultado <- readRDS(paste0("dados/modelos/dt_modelagem_veiculo_", input_veiculo, ".rds"))
  
  # Analyzing the number of rows to check for missing data or duplicate data
  dados_resultado %>% dim() # 2.244.202 lines  and    8 variables
  dados_resultado %>% distinct() %>% dim() # 2.244.202 lines and 8 variables (no repeted data)
  dados_resultado$Probabilidade_Recompra %>% summary() # No missing data
  dados_resultado %>% filter(is.na(Probabilidade_Recompra)) # No missing data

  # March 2022: 2242793 lines (1409 more lines in May 2022)
  
  #### Reaching the data again ====
  dt<- read.delim("dados/dados_txt/dados_final_veiculos.txt", sep=";") %>% 
    group_by(id,veiculo) %>% 
    mutate(
      tempo_compra_min = min(dias_da_ultima_compra),
      filtro = ifelse(dias_da_ultima_compra==tempo_compra_min,1,0)
    ) %>% 
    filter(filtro==1) %>% 
    select(-c(9,10)) %>% 
    filter(!is.na(id)) # unbase lines without cpf
  
  dt %>% dim()# 2.244.202 lines  and   8 variables
  dt %>% distinct() %>% dim() # 2.244.202 lines and 8 variables (no repeted data)
  dt$probabilidade_recompra %>% summary() # No mising data
  dt %>% filter(is.na(probabilidade_recompra)) # No missing data
  
  
  # ** Final base - climb the GTB 'server's
  
  write.table(dados_resultado, file = "dados/dados_txt/dados_final_recompra_Maio.txt", sep = ";", row.names = F) # Removing the names of the lines to not affect in part 2

  # Conclusion May/2022: Raise the base on the GTB server (no problems)
})