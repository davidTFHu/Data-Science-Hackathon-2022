##################### Saving Maps so they don't always download ========


##### Required Packages ----
library("tidyverse")
library("highcharter")



#### Tab: Purchasse analysis =====

# * Server: Client per UF ----

###  Trigger Buy Recently ---
input_gatilho <- "Buy Recently"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge", 
                    "maverick", # New Vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}


###  Trigger Grotwh ---
input_gatilho <- "Grotwh"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge", 
                    "maverick", # New vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}


###  Trigger Maturing ---
input_gatilho <- "Maturing"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge",
                    "maverick", # New vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}


###  Trigger Maturing ---
input_gatilho <- "Maturity"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge", 
                    "maverick", # New vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}



###  Trigger Decline ---
input_gatilho <- "Decline"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge", 
                    "maverick", # New vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}


###  Trigger Lost ---
input_gatilho <- "Lost"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge",
                    "maverick", # New vehicles
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/dados_pre_processados/dados_", input_veiculo, ".rds")) %>%
    filter(gatilho == as.character(input_gatilho)) %>%
    filter(UF != "", UF != "ZZ")
  
  format_label <- '{point.value:.,0f}'
  decimal_value <- 0
  value_suffix <- ""
  nome <- "Total de clientes"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}


###  Trigger Pct ---
input_gatilho <- "pct"

lista_veiculos <- c("ranger","ka","k_sedan","fiesta","fusion","focus",
                    "edge", 
                    "maverick", # New vehicle
                    "suv","carro de luxo")

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_uf <- readRDS(paste0("dados/modelos/Tab_UF_", input_veiculo, ".rds")) %>%
    mutate(
      UF = toupper(UF),
      n = as.numeric(Prob)*100
    ) %>%
    filter(UF != "12", UF != "ZZ", UF != "0")
  
  format_label <- '{point.value:.2f}%'
  decimal_value <- 2
  value_suffix <- "%"
  nome <- "Probabilidade Recompra Modelo UF"
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_uf, value = "n",
                        joinBy =  c("hc-a2", "UF"), name = nome,
                        dataLabels = list(enabled = TRUE, format = format_label,
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(valueDecimals = decimal_value, valueSuffix = value_suffix))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_recompra_",input_gatilho,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "e gatilho",input_gatilho, "salva!"))
}



#### Tab: Purchase Website Analysis =====

#### Map PERCENTAGE OF CONVERSION BY STATE ----

dados_lead_filtrado <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds")

## All vehicles
input_veiculo <- "Todos"

print(paste("veiculo", input_veiculo))

dados_lead <- dados_lead_filtrado

dados_plot <- dados_lead %>%  
  group_by(ESTADO) %>% 
  count(CONVERSAO) %>% 
  mutate(
    total = sum(n),
    pct = n/total*100,
    CONVERSAO = case_when(
      CONVERSAO == 0 ~ "Não converteu",
      TRUE ~"Converteu")
  ) %>% 
  filter(CONVERSAO == "Converteu")

Mapa_todos <- hcmap("countries/br/br-all", data = dados_plot, value = "pct",
                    joinBy =  c("hc-a2", "ESTADO"), name = "",
                    dataLabels = list(enabled = TRUE, format = '<b>{point.value:.1f}%<b> <br>',
                                      filter = list(property= 'value',
                                                    operator= '>',
                                                    value= 0)),
                    tooltip = list(pointFormat = paste0('<b>{point.ESTADO}</b> <br>',
                                                        'Porcentagem conversão: <b>{point.value:.1f}%<b> <br>')))

saveRDS(Mapa_todos, paste0("dados/Mapas/mapa_site_porcentagem_",tolower(input_veiculo),".rds"), version = 2)

print(paste("Mapa do veiculo", input_veiculo, "salva!"))

Mapa_teste <- readRDS(paste0("dados/Mapas/mapa_site_porcentagem_",tolower(input_veiculo),".rds"))


## Each vehicle
lista_veiculos <- c("RANGER","MUSTANG","TERRITORY","EDGE","BRONCO","TRANSIT"
                    , "MAVERICK") # New vehicle

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_lead <- dados_lead_filtrado
  
  dados_plot <- dados_lead %>%  
    filter(VEICULO_INTERESSE==input_veiculo) %>% 
    group_by(ESTADO) %>% 
    count(CONVERSAO) %>% 
    mutate(
      total = sum(n),
      pct = n/total*100,
      CONVERSAO = case_when(
        CONVERSAO == 0 ~ "Não converteu",
        TRUE ~"Converteu")
    ) %>% 
    filter(CONVERSAO == "Converteu")
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_plot, value = "pct",
                        joinBy =  c("hc-a2", "ESTADO"), name = "",
                        dataLabels = list(enabled = TRUE, format = '<b>{point.value:.1f}%<b> <br>',
                                          filter = list(property= 'value',
                                                        operator= '>',
                                                        value= 0)),
                        tooltip = list(pointFormat = paste0('<b>{point.ESTADO}</b> <br>',
                                                            'Porcentagem conversão: <b>{point.value:.1f}%<b> <br>')))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_site_porcentagem_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "salva!"))
  
}



#### AVERAGE CONVERSION MAP BY STATE ----

## All vehicles
input_veiculo <- "Todos"

print(paste("veiculo", input_veiculo))

dados_lead <- dados_lead_filtrado

dados_plot <- dados_lead %>% 
  filter(!is.na(TEMPO_ATE_CONVERSAO)) %>% 
  group_by(ESTADO) %>% 
  summarise(
    media_tempo = round(mean(TEMPO_ATE_CONVERSAO),0)
  )

Mapa_todos <- hcmap("countries/br/br-all", data = dados_plot, value = "media_tempo",
                    joinBy =  c("hc-a2", "ESTADO"),name="",
                    dataLabels = list(enabled = TRUE, format = '<b>{point.value:,0f}<b> <br>'),
                    tooltip = list(pointFormat = paste0(
                      '<b>{point.ESTADO}</b> <br>',
                      'Média de dias até conversão: <b>{point.value:,0f}<b> <br>')))

saveRDS(Mapa_todos, paste0("dados/Mapas/mapa_site_media_",tolower(input_veiculo),".rds"), version = 2)

print(paste("Mapa do veiculo", input_veiculo, "salva!"))


## Each vehicles
lista_veiculos <- c("RANGER","MUSTANG","TERRITORY","EDGE","BRONCO","TRANSIT" 
                    , "MAVERICK") # New vehicle

for(i in 1:length(lista_veiculos)){
  
  input_veiculo <- lista_veiculos[i]
  
  print(paste("veiculo", input_veiculo))
  
  dados_lead <- dados_lead_filtrado
  
  dados_plot <- dados_lead %>% 
    filter(!is.na(TEMPO_ATE_CONVERSAO),VEICULO_INTERESSE==input_veiculo) %>% 
    group_by(ESTADO) %>% 
    summarise(
      media_tempo = round(mean(TEMPO_ATE_CONVERSAO),0)
    )
  
  Mapa_veiculo <- hcmap("countries/br/br-all", data = dados_plot, value = "media_tempo",
                        joinBy =  c("hc-a2", "ESTADO"),name="",
                        dataLabels = list(enabled = TRUE, format = '<b>{point.value:,0f}<b> <br>'),
                        tooltip = list(pointFormat = paste0(
                          '<b>{point.ESTADO}</b> <br>',
                          'Média de dias até conversão: <b>{point.value:,0f}<b> <br>')))
  
  saveRDS(Mapa_veiculo, paste0("dados/Mapas/mapa_site_media_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Mapa do veiculo", input_veiculo, "salva!"))
  
}

