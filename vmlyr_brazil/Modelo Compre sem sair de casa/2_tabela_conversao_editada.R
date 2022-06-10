library(tidyverse)
library(kableExtra)

fatorizar <- function(x) {
  x %>% 
    mutate(
      CONVERSAO = case_when(
        CONVERSAO == 1 ~ "Converteu",
        TRUE ~ "Não converteu"
      ) 
    ) %>% 
    mutate_all(as.factor)
}

fatorizar2 <- function(x) {
  x %>% 
    mutate(
      CONVERSAO = case_when(
        CONVERSAO.x == 1 ~ "Converteu", # Use ".x" because it is encoded in 0 or 1
        TRUE ~ "Não converteu"
      ) 
    ) %>% 
    mutate_all(as.factor)
}

prop_fisher <- function(x) {
  tryCatch(
    prop.test(x),
    warning = function(w) fisher.test(x, simulate.p.value = TRUE),
    error = function(e) fisher.test(x, simulate.p.value = TRUE)
  )
}

testar_contingencia <- function(x, dados_contingencia) {
  fator <- x
  
  print(fator)
  
  contingencia <- 
    dados_contingencia[, c(fator, "CONVERSAO")] %>% 
    na.omit() %>% 
    table()
  
  relativa <- paste0(round(contingencia / rowSums(contingencia) * 100, 2), "%")
  
  relativa <- str_replace_all(relativa,"NaN%","-")
  
  p_valor <- prop_fisher(contingencia)$p.value
  
  print(p_valor)
  
  tamanho <- dim(contingencia)[1]
  
  print(tamanho)
  
  saida <- 
    as.data.frame(
      matrix(
        c(
          rep(fator, tamanho),
          rownames(contingencia),
          paste0(contingencia, " (", relativa, ")"),
          rep(p_valor, tamanho)
        ),
        nrow = tamanho
      ) 
    )
  names(saida) <- c("Fator", "Nível", "Converteu", "Não converteu", "P-valor")
  saida
}

dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds")

serasa <- readRDS("dados/dados_lead_compra_site/dados_serasa.rds")
serasa %>% dim() # 44919   114 (Expected 615634, ie 570715 lines lesss)
# valores_unicos <- serasa %>% distinct(CPF, .keep_all = T)
valores_unicos <- serasa %>% distinct(Cpf_Padr, .keep_all = T) # we had to update the variable = 36652, ie , 8267 repeated lines

lista_veiculos <- c("Todos",dados_lead$VEICULO_INTERESSE %>% na.omit() %>% unique() %>% as.character())
# no longer removed the edge from the vehicle list because it has conversation

na_char <- function(vetor){fct_explicit_na(as.factor(vetor), na_level = "NA")}

criar_tabela <- function(variavel){
  
  for(i in 1:length(lista_veiculos)){
    
    input_veiculo <- lista_veiculos[i]
    
    if (variavel == "var_leads"){ 
      
      dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds") %>% 
        mutate(ESTADO = case_when(
          ESTADO == "PARA" ~ "PA",
          ESTADO == "PARÁ" ~ "PA",
          ESTADO == "PARANÁ" ~ "PR",
          ESTADO == "PARANA" ~ "PA",
          ESTADO == "RIO GRANDE DO SUL" ~ "RS",
          ESTADO == "GOIÁS" ~ "GO",
          TRUE ~ ESTADO
        )) %>% 
        filter(ESTADO %in% c("AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO", "MA",
                             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ",
                             "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO", "DF"))
      
      if (input_veiculo =="Todos"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      } 
      
      if (input_veiculo == "RANGER"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "MUSTANG"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "TERRITORY"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "EDGE"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "VALOR_PARCELA_PODE_PAGAR","VALOR_ENTRADA","TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "MAVERICK"){ # New vehicle added
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "VALOR_PARCELA_PODE_PAGAR","VALOR_ENTRADA","TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "TRANSIT"){ # New vehicle added
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "VALOR_PARCELA_PODE_PAGAR","VALOR_ENTRADA","TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "BRONCO"){ # New vehicle added
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "VALOR_PARCELA_PODE_PAGAR","VALOR_ENTRADA","TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
    } else {
      
      dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds") %>% 
        mutate(ID = as.character(ID)) %>% 
        inner_join(valores_unicos %>% mutate(CPF = as.character(Cpf_Padr)), by = c("ID" = "CPF")) %>% # Modifield 02/23/2022
        mutate_if(is.character, na_char) %>% 
        mutate(SITUACAO_CADASTRAL_ENR = ifelse(SITUACAO_CADASTRAL_ENR == "REGULAR",
                                               "REGULAR", "NÃO REGULAR"),
               SEXO_ENR = ifelse(SEXO_ENR == "M", "MASCULINO", "NÃO MASCULINO"),
               ESTADO_CIVIL_ENR = ifelse(ESTADO_CIVIL_ENR %in% c("C", "V", "D"), "Pelo menos um casamento", "Outro"),
               FLAG_SERVIDOR_PUBLICO_ENR = ifelse(FLAG_SERVIDOR_PUBLICO_ENR == "Sim", "Sim", "Não"),
               ESCOLARIDADE_ENR = case_when(
                 ESCOLARIDADE_ENR != "NA" ~ "Informado",
                 TRUE ~ "Não informado"
               ),
               FLAG_PRODUTOR_RURAL_ENR = ifelse(FLAG_PRODUTOR_RURAL_ENR == "Sim", "Sim", "Não"),
               FLAG_SOCIO_EMPRESA_ENR = ifelse(FLAG_SOCIO_EMPRESA_ENR == "Sim", "Sim", "Não"),
               FAIXA_RENDA_ENR = case_when(
                 FAIXA_RENDA_ENR %in% c("ATÉ R$ 500,00", "MAIS DE R$ 500 A R$ 1.000,00", "MAIS DE R$ 1.000,00 A R$ 1.500,00", "MAIS DE R$ 1.500,00 A R$ 2.000,00", "MAIS DE R$ 2.000,00 A R$ 2.500,00", "MAIS DE R$ 2.500,00 A R$ 3.000,00") ~ "Até R$3000,00 ou Não Informado",
                 TRUE ~ "MAIS DE R$ 3000,00"
               ),
               TRIAGEM_RISCO_ENR = case_when(
                 TRIAGEM_RISCO_ENR %in% c("ALTISSIMO", "ALTO", "NA") ~ "Alto - Altíssimo - NA",
                 TRIAGEM_RISCO_ENR %in% c("BAIXO", "BAIXISSIMO RISCO") ~ "Baixo - Baixíssimo Risco",
                 TRUE ~ "Médio"
               ),
               ATIVIDADE_CONSUMO_ENR = case_when(
                 ATIVIDADE_CONSUMO_ENR %in% c("A1", "A2", "A3") ~ "A",
                 TRUE ~ "Outro"
               ),
               CLASSE_SOCIAL_ENR = case_when(
                 CLASSE_SOCIAL_ENR %in% c("A", "B") ~ "A ou B",
                 TRUE ~ "Outro"
               ),
               FLAG_REPRESENTANTE_LEGAL_ENR = ifelse(FLAG_REPRESENTANTE_LEGAL_ENR == "Sim", "Sim", "Não"),
               FLAG_ARQUIVO_NOVO = ifelse(FLAG_ARQUIVO_NOVO == "Sim", "Sim", "Não"),
               PROPENSAO_POSSECARTAOCREDITO_ENR = ifelse(PROPENSAO_POSSECARTAOCREDITO_ENR == "Não", "Não", "Não informado"),
               PROPENSAO_LUXO_ENR = ifelse(PROPENSAO_LUXO_ENR == "NA", "Não informado", "Informado"),
               PROPENSAO_TVASSINATURA_ENR = ifelse(PROPENSAO_TVASSINATURA_ENR == "NA", "Não informado", "Informado"),
               PROPENSAO_BANDALARGA_ENR = ifelse(PROPENSAO_BANDALARGA_ENR == "NA", "Não informado", "Informado"),
               PROPENSAO_CREDIMOB_ENR = ifelse(PROPENSAO_CREDIMOB_ENR == "NA", "Não informado", "Informado"),
               PROPENSAO_ECOMMERCE_ENR = ifelse(PROPENSAO_ECOMMERCE_ENR == "Não", "Não", "Não informado"),
               PROPENSAO_CREDCONSIG_ENR = ifelse(PROPENSAO_CREDCONSIG_ENR == "Sim", "Sim", "Não informado"),
               PROPENSAO_MOBILE_ENR = ifelse(PROPENSAO_MOBILE_ENR == "NA", "Não informado", "Informado"),
               PROPENSAO_VIAGEMTURISMO_ENR = ifelse(PROPENSAO_VIAGEMTURISMO_ENR  == "Não", "Não", "Não informado")
        )
      
      nomes_variaveis <- c("SITUACAO_CADASTRAL_ENR", "SEXO_ENR", "ESTADO_CIVIL_ENR", "FLAG_SERVIDOR_PUBLICO_ENR",
                           "ESCOLARIDADE_ENR", "FLAG_PRODUTOR_RURAL_ENR", "FAIXA_RENDA_ENR", "TRIAGEM_RISCO_ENR",
                           "ATIVIDADE_CONSUMO_ENR", "CLASSE_SOCIAL_ENR", "FLAG_REPRESENTANTE_LEGAL_ENR", "PROPENSAO_POSSECARTAOCREDITO_ENR",
                           "PROPENSAO_LUXO_ENR", "PROPENSAO_TVASSINATURA_ENR", "PROPENSAO_BANDALARGA_ENR", "PROPENSAO_CREDIMOB_ENR",
                           "PROPENSAO_ECOMMERCE_ENR", "PROPENSAO_CREDCONSIG_ENR", "PROPENSAO_MOBILE_ENR",
                           "PROPENSAO_VIAGEMTURISMO_ENR", "FLAG_ARQUIVO_NOVO")
      
    }
    
    print(input_veiculo)
    
    if(input_veiculo=="Todos"){
      dados_lead <- dados_lead
    }else{
      if (variavel == "var_leads"){
      dados_lead <- dados_lead %>%
        filter(VEICULO_INTERESSE==input_veiculo)
      } else {
      # One more code edited due to join ----
       dados_lead <- dados_lead %>% 
        filter(VEICULO_INTERESSE.x ==input_veiculo) # Maybe the y, because it's Serasa
      }
    }
    
    
    # dados_contingencia <- fatorizar(dados_lead)
    
    # The function below had to be updated for the database ----
    if (variavel == "var_leads"){
      dados_contingencia <- fatorizar(dados_lead)
      nomes_contingencia <- names(dados_contingencia)
    } else {
    dados_contingencia <- fatorizar2(dados_lead)
    nomes_contingencia <- names(dados_contingencia)
    }
    
    if (variavel == "var_leads"){
      
      variaveis_contingencia <- subset(
        nomes_contingencia,
        !nomes_contingencia %in% nomes_variaveis
      )
      
    } else {
      
      variaveis_contingencia <- nomes_variaveis
    
    }
    
    tabelas_contingencia <- 
      map_dfr(variaveis_contingencia, testar_contingencia, dados_contingencia) %>% 
      mutate(`P-valor` = round(as.numeric(as.character(`P-valor`)),3)) %>% 
      mutate(`P-valor` = case_when(
        `P-valor` == 0.0000 ~ "< 0.001",
        TRUE ~ as.character(`P-valor`)
      ))%>% 
      mutate(`P-valor` = cell_spec(`P-valor`,
                                   color = case_when(
                                     as.numeric(`P-valor`)<=0.050 ~"red",
                                     is.na(as.numeric(`P-valor`))  ~"red",
                                     TRUE ~ "black"),
                                   bold = case_when(
                                     as.numeric(`P-valor`)<=0.050 ~T,
                                     is.na(as.numeric(`P-valor`))  ~T,
                                     TRUE ~ F))) 
    
    saveRDS(tabelas_contingencia,paste0("dados/tabelas_site/dados_tabela_conversao_", tolower(input_veiculo),"_",variavel ,".rds"), version = 2)
    
    
  }
  
}

criar_tabela("var_leads")
criar_tabela("var_serasa")



# Testing
input_veiculo <- "MAVERICK"
variavel <- "var_serasa"
Teste <- readRDS(paste0("dados/tabelas_site/dados_tabela_conversao_", tolower(input_veiculo),"_",variavel ,".rds"))


dados_lead %>% 
  filter(VEICULO_INTERESSE == input_veiculo) %>% 
  select(ORIGEM_LEAD) %>% 
  table()