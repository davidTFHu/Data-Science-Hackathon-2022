library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
library(readr)

# Reading lead data ----

# New (05/03/2022)
novos_lead <- read.csv("dados/Dados_brutos_txt/leads_cssc_20220503.txt", sep = ";", header = T) %>% 
  as.data.frame()
novos_lead %>% dim() # 877730     22


# Old (Last update)
base_lead_antiga <- read.csv("dados/Dados_brutos_txt/leads_cssc_20220309.txt",sep = ";", header = T) %>% 
  as.data.frame()
base_lead_antiga %>% dim() # 871811     22

base_lead_antiga2 <- read.csv("dados/Dados_brutos_txt/leads_cssc_20220214.txt",sep = ";", header = T) %>% 
  as.data.frame()
base_lead_antiga2 %>% dim() # 869164     22

base_lead_antiga3 <- read.csv("dados/dados_lead_compra_site/leads_cssc_20210811.txt",sep = ";", header = T) %>% 
  as.data.frame()
base_lead_antiga3 %>% dim() # 841385     22
base_lead_antiga3 <- base_lead_antiga3 %>% 
  mutate(COD_CONCESSIONARIA = as.character(COD_CONCESSIONARIA)) # Modifications needed to join the bases

base_lead_antiga_final <- bind_rows(base_lead_antiga3, base_lead_antiga2, base_lead_antiga)


### Making a join between the two bases ----
dados_lead_bind <- bind_rows(base_lead_antiga_final, novos_lead)
dados_lead_bind %>% dim() # 3460090  lines,  22 variables

dados_lead_bind_final <- dados_lead_bind %>%  # from 3460090  to  1719068 (1741022 duplicated lines)
  distinct()

# Updating database
base_lead <- dados_lead_bind_final


# Final lead data treatment ----

source("atualizar_analise_site/funcoes_tratamento_variaveis.R", encoding = "UTF-8")

dados_lead <- base_lead %>% 
  as.data.frame() %>% 
  transmute(
    ID = as.character(NR_CPF),
    ORIGEM_LEAD = ORIGEM_LEAD,
    SUB_ORIGEM = SUB_ORIGEM,
    MARCA_VEICULO_ATUAL_FORD = classificar_marca_veiculo_ford(MARCA_VEICULO),
    MARCA_VEICULO_ATUAL = classificar_marca_veiculo(MARCA_VEICULO),
    MODELO_VEICULO_ATUAL = classificar_modelo_veiculo(MODELO_VEICULO),
    ANO_MODELO_ATUAL = ANO_MODELO,
    FORMA_PAGAMENTO = classificar_forma_pagamento(COMO_PRETENDE_FAZER_PAGAMENTO),
    NAO_SEI_VALOR_ENTRADA = classificar_saber_valor_entrada(NAO_SEI_VALOR_ENTRADA),
    VALOR_PARCELA_PODE_PAGAR = classificar_parcela(as.numeric(gsub(",",".",QUAL_VALOR_PARCELA_PODE_PAGAR))),
    ID_PRINCIPAL = ID_PRINCIPAL,
    DATA_CRIACAO = dmy(DATA_CRIACAO),
    VALOR_ENTRADA = classificar_entrada(as.numeric(gsub(",",".",VALOR_ENTRADA))),
    ESTADO =  classificar_estado(ESTADO) %>% toupper(),
    REGIAO = classificar_regiao(ESTADO),
    COD_CONCESSIONARIA = COD_CONCESSIONARIA,
    TRANSMISSAO_VEICULO_INTERESSE = classificar_transmissao_veiculo(VEICULO_INTERESSE),
    POSSUI_FORD_CREDIT = classificar_possui_ford_credit(VEICULO_INTERESSE),
    VEICULO_INTERESSE = veiculo_interesse_dist(VEICULO_INTERESSE),
    TMA = TMA,
    CATALOGO = CATALOGO,
    DATA_COMPRA_CONVERSAO = as.Date.character(DATA_COMPRA_CONVERSAO),
    #TRANSMISSAO_MODELO_CONVERSAO = classificar_transmissao_veiculo(MODELO_CONVERSAO),
    MODELO_CONVERSAO = modelo_conversao_dist(MODELO_CONVERSAO),
    CONVERSAO = CONVERSAO,
    DATA_ULTIMA_COMPRA = as.Date(DT_COMP_ATUA,origin = "1899-12-30"),
    DS_MODELO_ATUAL = str_trim(DS_MODL_ATUA)
  ) %>% 
  mutate(
    ANO_MODELO_ATUAL = 
      factor(ANO_MODELO_ATUAL, c(NA,"2018","2019","2020","2021","2022","2023")), # Adding years of 2022 and 2023
    VALOR_PARCELA_PODE_PAGAR = 
      factor(VALOR_PARCELA_PODE_PAGAR,c(NA,"Entre 0 - 300,00 reais","Entre 300,01 - 700,00 reais","Entre 700,01 - 1000,00 reais","Mais de 1000,00 reais")),
    VALOR_ENTRADA = 
      factor(VALOR_ENTRADA,c(NA,"R$0,00","Entre 0,01 - 8000,00 reais","Entre 8000,01 - 15000,00 reais","Entre 15000,01 - 28000,00 reais","Mais de 28000,00 reais")),
    REGIAO=factor(REGIAO,c("Não especificado","Centro-Oeste","Nordeste","Norte","Sudeste","Sul")),
    TEMPO_ATE_CONVERSAO = as.numeric(DATA_COMPRA_CONVERSAO - DATA_CRIACAO),
    INTERVALO_TEMPO_ATE_CONVERSAO = classificar_intervalo_tempo_conversao(TEMPO_ATE_CONVERSAO)
  ) 

saveRDS(dados_lead, "dados/dados_lead_compra_site/dados_lead_final.rds", version = 2)


# Filtered Data -- only vehicles of interest

dados_lead_filtrado <- base_lead %>% 
  as.data.frame() %>% 
  mutate(veiculo_interesse=veiculo_interesse_dist(VEICULO_INTERESSE)) %>% 
  filter(veiculo_interesse %in% c("RANGER","MUSTANG","TERRITORY",
                                  "EDGE", 
                                  "MAVERICK", # New vehicle added
                                  "BRONCO", "TRANSIT" # New vehicles added
                                  
                                  )) %>% 
  transmute(
    ID = as.character(NR_CPF),
    ORIGEM_LEAD = ORIGEM_LEAD,
    SUB_ORIGEM = SUB_ORIGEM,
    MARCA_VEICULO_ATUAL_FORD = classificar_marca_veiculo_ford(MARCA_VEICULO),
    MARCA_VEICULO_ATUAL = classificar_marca_veiculo(MARCA_VEICULO),
    MODELO_VEICULO_ATUAL = classificar_modelo_veiculo(MODELO_VEICULO),
    ANO_MODELO_ATUAL = ANO_MODELO,
    FORMA_PAGAMENTO = classificar_forma_pagamento(COMO_PRETENDE_FAZER_PAGAMENTO),
    NAO_SEI_VALOR_ENTRADA = classificar_saber_valor_entrada(NAO_SEI_VALOR_ENTRADA),
    VALOR_PARCELA_PODE_PAGAR = classificar_parcela_filtro(as.numeric(gsub(",",".",QUAL_VALOR_PARCELA_PODE_PAGAR))),
    ID_PRINCIPAL = ID_PRINCIPAL,
    DATA_CRIACAO = dmy(DATA_CRIACAO),
    VALOR_ENTRADA = classificar_entrada_filtro(as.numeric(gsub(",",".",VALOR_ENTRADA))),
    ESTADO =  classificar_estado(ESTADO) %>% toupper(),
    REGIAO = classificar_regiao(ESTADO),
    COD_CONCESSIONARIA = COD_CONCESSIONARIA,
    TRANSMISSAO_VEICULO_INTERESSE = classificar_transmissao_veiculo(VEICULO_INTERESSE),
    POSSUI_FORD_CREDIT = classificar_possui_ford_credit(VEICULO_INTERESSE),
    VEICULO_INTERESSE = veiculo_interesse_dist(VEICULO_INTERESSE),
    TMA = TMA,
    CATALOGO = CATALOGO,
    DATA_COMPRA_CONVERSAO = as.Date(DATA_COMPRA_CONVERSAO),
    #TRANSMISSAO_MODELO_CONVERSAO = classificar_transmissao_veiculo(MODELO_CONVERSAO),
    MODELO_CONVERSAO = modelo_conversao_dist(MODELO_CONVERSAO),
    CONVERSAO = CONVERSAO,
    DATA_ULTIMA_COMPRA = as.Date(DT_COMP_ATUA,origin = "1899-12-30"),
    DS_MODELO_ATUAL = str_trim(DS_MODL_ATUA)
  ) %>% 
  mutate(
    ANO_MODELO_ATUAL = 
      factor(ANO_MODELO_ATUAL, c(NA,"2018","2019","2020","2021","2022","2023")),
    VALOR_PARCELA_PODE_PAGAR = 
      factor(VALOR_PARCELA_PODE_PAGAR,c(NA,"R$0,00","Entre 0,01 - 1100,00 reais","Entre 1100,01 - 2000,00 reais","Mais de 2000,00 reais")),
    VALOR_ENTRADA = 
      factor(VALOR_ENTRADA,c(NA,"R$0,00","Entre 0,01 - 18000,00 reais","Entre 18000,01 - 50000,00 reais","Mais de 50000,00 reais")),
    REGIAO=factor(REGIAO,c("Não especificado","Centro-Oeste","Nordeste","Norte","Sudeste","Sul")),
    TEMPO_ATE_CONVERSAO = as.numeric(DATA_COMPRA_CONVERSAO - DATA_CRIACAO),
    INTERVALO_TEMPO_ATE_CONVERSAO = classificar_intervalo_tempo_conversao(TEMPO_ATE_CONVERSAO)
  ) 

saveRDS(dados_lead_filtrado, "dados/dados_lead_compra_site/dados_lead_final_filtrado.rds", version = 2)


# Left Join base probabilidade recompra ----

dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado.rds")


dados_veiculos <- read_delim("dados/dados_txt/dados_final_veiculos1.txt",delim = ";",skip = 1,
                             col_names = c(
                               # "nrow",
                               "veiculo","id","data_ultima_compra","dias_da_ultima_compra",
                                           "probabilidade_recompra","gatilho","class_prob_recompra","utilizou_modelo"))# [2:9]

dados_veiculos <- dados_veiculos %>% 
  mutate(id = as.character(id)) %>% 
  group_by(id) %>%
  mutate(data_ultima_compra_max=max(data_ultima_compra),
         filtro = ifelse(data_ultima_compra==data_ultima_compra_max,1,0)) %>% 
  filter(filtro==1) %>% 
  summarise(probabilidade_recompra = mean(probabilidade_recompra, na.rm = T)) # wondering if it wouldn't be better to keep vehicle differentiation

dados_final <- dados_lead %>% left_join(dados_veiculos,by = c("ID"="id"))

media_prob <- mean(dados_final$probabilidade_recompra,na.rm = T) %>% round(2)

dados_final2 <- dados_final %>% 
  mutate(class_prob_recompra = case_when(
    probabilidade_recompra > media_prob~ paste0("Probabilida Alta (Acima média ",media_prob,")"),
    is.na(probabilidade_recompra) ~ "Não está na base recompra",
    TRUE ~ paste0("Probabilida Baixa (Abaixo ou igual média ",media_prob,")")
  ) %>% factor(c(paste0("Probabilida Baixa (Abaixo ou igual média ",media_prob,")"), paste0("Probabilida Alta (Acima média ",media_prob,")"),"Não está na base recompra"))) %>% 
  select(-c(probabilidade_recompra))

saveRDS(dados_final2, "dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds", version = 2)
