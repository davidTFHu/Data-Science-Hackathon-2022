library(tidyverse)


lista_veiculos <- c("ranger",
                    #"ecosport", 
                    "ka", "k_sedan", "fiesta", "fusion", "focus", 
                    #"mustang",
                    "edge", 
                    "maverick", # Novo veiculo
                    #"territory",
                    # "fiesta sedan",
                    #"focus sedan", "bronco",
                    "suv",
                    "carro de luxo"
                    )

for (i in 1:length(lista_veiculos)) {
  
  veiculo <- lista_veiculos[[i]]
  
  print(paste("veiculo", veiculo))
  
  dados_veiculo <- readRDS(paste0("dados/veiculos/dados_gatilho_", tolower(veiculo),".rds"))
  
  dados_n <- dados_veiculo %>% 
    mutate(veiculo = toupper(veiculo)) %>% 
    group_by(UF, veiculo, gatilho) %>% 
    summarise(n = n())
  
  
  saveRDS(dados_n, paste0("dados/dados_pre_processados/dados_", tolower(veiculo),".rds"), version = 2)
  
}

