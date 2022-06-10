library(tidyverse)


Corrigir_extremos <- function(dados){
  Novos_dados <- dados %>%
    mutate(Probabilidade_Recompra = case_when(Probabilidade_Recompra == 0 ~ 0.01,
                                     Probabilidade_Recompra == 100 ~ 99.99,
                                     TRUE ~ as.numeric(Probabilidade_Recompra))
    )
  dados <- Novos_dados
}



# general model table ----
dados_final_veiculo1 <-  read.delim("dados/dados_txt/dados_final_veiculos1.txt",sep = ";")

dados_veiculos_geral <- dados_final_veiculo1 %>% 
  dplyr::select(ID = id, 
                Veiculo = veiculo, # AAdding the vehicle to the general table
                `Data_Compra_Última` = data_ultima_compra, 
                Dias_da_ultima_compra = dias_da_ultima_compra,
                Probabilidade_Recompra = probabilidade_recompra, 
                Gatilho = gatilho, 
                `Class. Prob. Recompra` = class_prob_recompra,
                utilizou_modelo)
dados_veiculos_geral <- Corrigir_extremos(dados_veiculos_geral)

saveRDS(dados_veiculos_geral, paste0("dados/modelos/dt_modelagem_veiculo_geral.rds"), version = 2)


# groupings table ----
lista_veiculos_agrupados <- c("SUV","Carro de Luxo")

### SUV
car <- "SUV"
print(paste("car", car))

dados_veiculo_SUV <- dados_final_veiculo1 %>% 
  filter(veiculo == "ECOSPORT"| veiculo == "TERRITORY"| veiculo == "BRONCO") %>% 
  dplyr::select(ID = id, 
                Veiculo = veiculo,
                `Data_Compra_Última` = data_ultima_compra, 
                Dias_da_ultima_compra = dias_da_ultima_compra,
                Probabilidade_Recompra = probabilidade_recompra, 
                Gatilho = gatilho, 
                `Class. Prob. Recompra` = class_prob_recompra,
                utilizou_modelo)

print(paste("dim", dim(dados_veiculo_SUV)))
dados_veiculo_SUV <- Corrigir_extremos(dados_veiculo_SUV)

saveRDS(dados_veiculo_SUV, paste0("dados/modelos/dt_modelagem_veiculo_", tolower(car),".rds"), version = 2)


### Luxury car
car <- "Carro de Luxo"
print(paste("car", car))

dados_veiculo_CarroLuxo <- dados_final_veiculo1 %>% 
  filter(veiculo == "MUSTANG"| veiculo == "FOCUS SEDAN") %>% 
  dplyr::select(ID = id, 
                Veiculo = veiculo,
                `Data_Compra_Última` = data_ultima_compra, 
                Dias_da_ultima_compra = dias_da_ultima_compra,
                Probabilidade_Recompra = probabilidade_recompra, 
                Gatilho = gatilho, 
                `Class. Prob. Recompra` = class_prob_recompra,
                utilizou_modelo)

print(paste("dim", dim(dados_veiculo_CarroLuxo)))
dados_veiculo_CarroLuxo <- Corrigir_extremos(dados_veiculo_CarroLuxo)

saveRDS(dados_veiculo_CarroLuxo, paste0("dados/modelos/dt_modelagem_veiculo_", tolower(car),".rds"), version = 2)





##### all other vehicles ----
lista_veiculos <- c("RANGER", 
                    #"ECOSPORT",
                    "KA",  "K_SEDAN", "FIESTA", "FUSION", "FOCUS",
                    #"MUSTANG", 
                    "EDGE",
                    "MAVERICK" #, Novo veiculo
                    #"TERRITORY", 
                    # "FIESTA SEDAN"#,
                    #"FOCUS SEDAN", "BRONCO"
                    )


for (i in 1:length(lista_veiculos)) {
  
  car <- lista_veiculos[[i]]
  print(paste("car", car))
  
  dados_veiculo <- dados_final_veiculo1 %>% 
    filter(veiculo == car) %>% 
    dplyr::select(ID = id, 
                  `Data_Compra_Última` = data_ultima_compra, 
                  Dias_da_ultima_compra = dias_da_ultima_compra,
                  Probabilidade_Recompra = probabilidade_recompra, 
                  Gatilho = gatilho, 
                  `Class. Prob. Recompra` = class_prob_recompra,
                  utilizou_modelo)
  
  print(paste("dim", dim(dados_veiculo)))
  dados_veiculo <- Corrigir_extremos(dados_veiculo)
  
  
  saveRDS(dados_veiculo, paste0("dados/modelos/dt_modelagem_veiculo_", tolower(car),".rds"), version = 2)
  
}
