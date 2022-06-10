testar_pop <- function(x, dados){
  
  fator <- x
  fator_sym <- sym(x)
  
  print(fator)
  
  dados_filtro <- dados %>% 
    filter(!is.na(!!fator_sym ),!is.na(TEMPO_ATE_CONVERSAO)) %>% 
    mutate(
      Fator = as.factor(!!fator_sym),
      TEMPO_ATE_CONVERSAO = as.numeric(TEMPO_ATE_CONVERSAO)
    ) %>% 
    select(c(TEMPO_ATE_CONVERSAO, Fator)) 
  
  dados_tabela <- dados_filtro %>% 
    group_by(Fator) %>% 
    summarise(
      quartil_1 = round(quantile(TEMPO_ATE_CONVERSAO)[2],2),
      mediana = round(median(TEMPO_ATE_CONVERSAO),2),
      quartil_3 = round(quantile(TEMPO_ATE_CONVERSAO)[4],2)
    ) 
  
  
  dados_tabela[is.na(dados_tabela)] <- "-"
  
  num_categ <- dados_filtro$Fator %>% unique() %>% length()
  
  print(num_categ)
  
  metodo <- case_when(
    num_categ < 2 ~ "nenhum",
    num_categ == 2 ~ "whitney",
    num_categ > 2 ~ "kruskal",
  )
  
  if(metodo == "whitney"){
    p_valor <- wilcox.test(dados_filtro$TEMPO_ATE_CONVERSAO~dados_filtro$Fator, exact=FALSE)$p.value
  } else if (metodo == "kruskal"){
    p_valor <- kruskal.test(dados_filtro$TEMPO_ATE_CONVERSAO~dados_filtro$Fator)$p.value
  } else {
    p_valor <- NA
  }
  
  print(p_valor)
  
  saida <- 
    as.data.frame(
      matrix(
        c(
          rep(fator,num_categ),
          as.character(dados_tabela$Fator),
          paste0(dados_tabela$quartil_1," / ",dados_tabela$mediana," / ", dados_tabela$quartil_3),
          rep(p_valor, num_categ)
        ),
        nrow = num_categ
      ) 
    )
  names(saida) <- c("Fator", "Nível", "Tempo até conversão (1°Q/Mediana/3°Q)", "P-valor")
  saida
}

testar_pop2 <- function(x, dados){
  
  fator <- x
  fator_sym <- sym(x)
  
  print(fator)
  
  dados_filtro <- dados %>% 
    filter(!is.na(!!fator_sym ),!is.na(TEMPO_ATE_CONVERSAO.x)) %>% # Used the ".x" because it is the one with behavior closest to the original
    mutate(
      Fator = as.factor(!!fator_sym),
      TEMPO_ATE_CONVERSAO = as.numeric(TEMPO_ATE_CONVERSAO.x)
    ) %>% 
    select(c(TEMPO_ATE_CONVERSAO, Fator)) 
  
  dados_tabela <- dados_filtro %>% 
    group_by(Fator) %>% 
    summarise(
      quartil_1 = round(quantile(TEMPO_ATE_CONVERSAO)[2],2),
      mediana = round(median(TEMPO_ATE_CONVERSAO),2),
      quartil_3 = round(quantile(TEMPO_ATE_CONVERSAO)[4],2)
    ) 
  
  
  dados_tabela[is.na(dados_tabela)] <- "-"
  
  num_categ <- dados_filtro$Fator %>% unique() %>% length()
  
  print(num_categ)
  
  metodo <- case_when(
    num_categ < 2 ~ "nenhum",
    num_categ == 2 ~ "whitney",
    num_categ > 2 ~ "kruskal",
  )
  
  if(metodo == "whitney"){
    p_valor <- wilcox.test(dados_filtro$TEMPO_ATE_CONVERSAO~dados_filtro$Fator, exact=FALSE)$p.value
  } else if (metodo == "kruskal"){
    p_valor <- kruskal.test(dados_filtro$TEMPO_ATE_CONVERSAO~dados_filtro$Fator)$p.value
  } else {
    p_valor <- NA
  }
  
  print(p_valor)
  
  saida <- 
    as.data.frame(
      matrix(
        c(
          rep(fator,num_categ),
          as.character(dados_tabela$Fator),
          paste0(dados_tabela$quartil_1," / ",dados_tabela$mediana," / ", dados_tabela$quartil_3),
          rep(p_valor, num_categ)
        ),
        nrow = num_categ
      ) 
    )
  names(saida) <- c("Fator", "Nível", "Tempo até conversão (1°Q/Mediana/3°Q)", "P-valor")
  saida
}


serasa <- readRDS("dados/dados_lead_compra_site/dados_serasa.rds")


valores_unicos <- serasa %>% distinct(Cpf_Padr, .keep_all = T)

dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds")

lista_veiculos <- c("Todos",dados_lead$VEICULO_INTERESSE %>% na.omit() %>% unique() %>% as.character())
# no longer removed the edge from the vehicle list because it has conversation

na_char <- function(vetor){fct_explicit_na(as.factor(vetor), na_level = "NA")}

criar_tabela <- function(variavel){

  for(i in 1:length(lista_veiculos)){
    
    input_veiculo <- lista_veiculos[i]
    
    print(input_veiculo)
    
    if (variavel == "var_leads"){ 
      
      dados_lead <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds") %>% 
        mutate(
          CONVERSAO = ifelse(CONVERSAO == "Comprou", 1, 0)
        ) 
      
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
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL", "MARCA_VEICULO_ATUAL_FORD", "FORMA_PAGAMENTO",
                             "TMA","ID_PRINCIPAL","DATA_CRIACAO","COD_CONCESSIONARIA","VEICULO_INTERESSE",
                             "VALOR_PARCELA_PODE_PAGAR","VALOR_ENTRADA","TRANSMISSAO_VEICULO_INTERESSE","POSSUI_FORD_CREDIT",
                             "CATALOGO","DATA_COMPRA_CONVERSAO","MODELO_CONVERSAO",
                             "DATA_ULTIMA_COMPRA","DS_MODELO_ATUAL","CONVERSAO","TEMPO_ATE_CONVERSAO","INTERVALO_TEMPO_ATE_CONVERSAO")
        
      }
      
      if (input_veiculo == "MAVERICK"){
        
        nomes_variaveis <- c("ID","MARCA_VEICULO_ATUAL","MODELO_VEICULO_ATUAL","ANO_MODELO_ATUAL", "MARCA_VEICULO_ATUAL_FORD", "FORMA_PAGAMENTO",
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
        # The code below had to be adapted because the CPF variable did not exist ----
        inner_join(valores_unicos %>% mutate(CPF = as.character(Cpf_Padr)), by = c("ID" = "CPF")) %>%
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
    

    if(input_veiculo=="Todos"){
      dados_lead <- dados_lead
    }else{
      if (variavel == "var_leads"){
        dados_lead <- dados_lead %>%
          filter(VEICULO_INTERESSE==input_veiculo)
      } else {
        # MOne more code edited due to join ----
        dados_lead <- dados_lead %>% 
          filter(VEICULO_INTERESSE.x ==input_veiculo)
      }
    }
    
    nomes_contingencia <- names(dados_lead)
    
    if (variavel == "var_leads"){
      
      variaveis_contingencia <- subset(
        nomes_contingencia,
        !nomes_contingencia %in% nomes_variaveis
      )
      
    } else {
      
      variaveis_contingencia <- nomes_variaveis
      
    }
    
    if (variavel == "var_leads"){
    tabelas_contingencia <- 
      map_dfr(variaveis_contingencia, testar_pop, dados_lead) %>% 
      mutate(`P-valor` = round(as.numeric(as.character(`P-valor`)),3)) %>% 
      mutate(`P-valor` = case_when(
        `P-valor` == 0.0000 ~ "< 0.001",
        is.na(`P-valor`) ~ "-",
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
    } else {
      tabelas_contingencia <- 
        map_dfr(variaveis_contingencia, testar_pop2, dados_lead) %>% 
        mutate(`P-valor` = round(as.numeric(as.character(`P-valor`)),3)) %>% 
        mutate(`P-valor` = case_when(
          `P-valor` == 0.0000 ~ "< 0.001",
          is.na(`P-valor`) ~ "-",
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
    }
    
    saveRDS(tabelas_contingencia,paste0("dados/tabelas_site/dados_tabela_tempo_ate_conversao_", tolower(input_veiculo),"_",variavel ,".rds"), version = 2)
    
  }

}

criar_tabela("var_leads")
criar_tabela("var_serasa")

