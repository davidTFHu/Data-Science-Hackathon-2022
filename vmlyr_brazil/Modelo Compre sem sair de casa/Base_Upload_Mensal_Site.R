#################### Monthly upload - Purchase Website ==================

##### Loading required packages
library("tidyverse")


##### Loading the necessary database to check for possible errors ====

dados_tabela <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2_presenca_serasa.rds")

Corrigir_extremos2 <- function(dados){
  Novos_dados <- dados %>%
    mutate(PROB_QUALIFICAÇÃO = case_when(PROB_QUALIFICAÇÃO == 0 ~ 0.0001,
                                         PROB_QUALIFICAÇÃO == 100 ~ 99.99,
                                         TRUE ~ as.numeric(PROB_QUALIFICAÇÃO))
    )
  dados <- Novos_dados
}

### parsing the number of rows to check for missing data or duplicate data
dados_tabela %>% dim() # 425829 lines  and   32 variabless
dados_tabela %>% slice(-distinct()) %>% dim() # 425563 and 8 variables (266 repeted datas)

dados_tabela %>% distinct() %>% rownames()
library("janitor")
dados_tabela %>% get_dupes()

dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado.rds")
dados_lead %>% dim()
dados_lead %>% distinct() %>% dim()


Dados_upload_site <- dados_tabela %>%
  dplyr::select("ID", "ORIGEM", "SUB_ORIGEM", "ANO_MODELO_ATUAL_cat", "DATA_CRIACAO",
                "REGIAO", "VEICULO_INTERESSE", "CONVERSAO", "DATA_ULTIMA_COMPRA",
                "DS_MODELO_ATUAL", "class_prob_recompra", "presenca_serasa", 
                "PROB_LOGISTICO", "tipo_cliente") %>% 
  mutate(PROB_LOGISTICO = round(PROB_LOGISTICO, 4)) %>%
  rename("Qualificação" = tipo_cliente, PROB_QUALIFICAÇÃO = PROB_LOGISTICO,
         "ANO_MODELO_ATUAL" = ANO_MODELO_ATUAL_cat) %>% 
  Corrigir_extremos2 

# Analyzing the number of rows to check for missing datas
Dados_upload_site %>% dim() # 425829 lines  and   14 variables
Dados_upload_site$PROB_QUALIFICAÇÃO %>% summary() # No missing data
Dados_upload_site %>% filter(is.na(PROB_QUALIFICAÇÃO)) # No missing data

Dados_upload_site %>% head()

# March total lines: 417236 
# Difference between May and March: 8593 lines


# ** Final Base Site  ====
write.table(Dados_upload_site, file = "dados/dados_txt/dados_final_site_Maio.txt", sep = ";", row.names = F) # Removing the names of the lines to not affect in part 2
