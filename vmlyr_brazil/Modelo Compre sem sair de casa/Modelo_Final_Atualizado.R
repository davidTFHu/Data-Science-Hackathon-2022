###################### Website purchase analysis - Dashboard GTB Master ==============


##### Packages ----
library(tidyverse)
library(knitr)
library(kableExtra)
library(sjPlot)
library(tidymodels)
library(caret)
library(caretEnsemble)
library(modEvA)
library(ResourceSelection)
library(ROCR)
library(broom)
library(ROCit)
library(olsrr)
library(pROC)
library(rsample) # training() e testing()
library(MASS) # StepAIC
library(ROCR)



##### Functions ----
classificacao2 <- function(mod, dados, metodo){
  
  if (metodo == "ensemble"){
    dados <- dados %>%
      mutate(prob = 1 - predict(mod, dados, type = "prob"))
  } else {
    dados <- dados %>% 
      mutate(prob = predict(mod, dados, type = "response"))
  }
  
  # Calculating the cut-off point
  measure <- measureit(score = dados$prob, 
                       class = dados$CONVERSAO.x,
                       measure = c("ACC", "SENS", "SPEC"))
  ponto_corte <- data.frame(Cutoff = measure[["Cutoff"]],
                            SENS = measure[["SENS"]],
                            SPEC = measure[["SPEC"]]) %>% 
    mutate(media = (SENS + SPEC)/2) %>% 
    slice_max(order_by = media, n = 1) %>% pull(Cutoff)
  
  # Classification
  dados<- dados %>%  
    mutate(classe = ifelse(prob > ponto_corte, "Sim", "Não"))
  
  # Confusion Matrix
  mat_confusao <- table(dados %>% pull(CONVERSAO), 
                        dados %>% pull(classe))
  rownames(mat_confusao) <- paste0(rownames(mat_confusao), "_real")
  colnames(mat_confusao) <- paste0(colnames(mat_confusao), "_predito")
  
  # VPN
  vpn <- mat_confusao[1]/(mat_confusao[1] + mat_confusao[3])
  
  # VPP
  vpp <- mat_confusao[4]/(mat_confusao[2] + mat_confusao[4])
  
  # Accuracy
  library(yardstick)
  acc <- accuracy_vec(as.factor(dados %>% pull(CONVERSAO)), 
                      as.factor(dados %>% pull(classe)))
  
  # AUC
  auc <- auc(dados %>% pull(CONVERSAO), dados %>% pull(prob))
  
  return(list("Ponto de Corte" = ponto_corte, 
              "Matriz de Confusão" = mat_confusao, 
              "VPP" = vpp, "VPN" = vpn, "AUC" = auc,
              "Acurácia" = acc))
  
}

resultados_modelo <- function(class1, class2, nome1, nome2){
  
  list(
    assign(paste("Tabela -", nome1), data.frame("Métrica" = c("Ponto de Corte -", 
                                                              "VPN -",
                                                              "VPP -",
                                                              "AUC -",
                                                              "Acurácia -"),
                                                "Valores" = c(class1[["Ponto de Corte"]],
                                                              class1[["VPN"]],
                                                              class1[["VPP"]],
                                                              class1[["AUC"]],
                                                              class1[["Acurácia"]])) %>% 
             mutate(`Métrica` = paste(`Métrica`, nome1), Valores = round(Valores,4))),
    
    assign(paste("Tabela -", nome2), data.frame("Métrica" = c("Ponto de Corte -",
                                                              "VPN -",
                                                              "VPP -",
                                                              "AUC -",
                                                              "Acurácia -"),
                                                "Valores" = c(class2[["Ponto de Corte"]],
                                                              class2[["VPN"]],
                                                              class2[["VPP"]],
                                                              class2[["AUC"]],
                                                              class2[["Acurácia"]])) %>% 
             mutate(`Métrica` = paste(`Métrica`, nome2), Valores = round(Valores,4))),
    
    assign(paste("Matriz de Confusão -", nome1), class1[["Matriz de Confusão"]]),
    
    assign(paste("Matriz de Confusão -", nome2), class2[["Matriz de Confusão"]])
    
  )
  
}

roc_cond = function(predito, verdadeiro) {
  test = NULL
  PS = is.null(test)
  rnam = if (!missing(test)) deparse(substitute(test))
  
  m = as.matrix(base::table(switch(PS + 1, test, predito), verdadeiro))
  m = addmargins(rbind(0, m), 2)
  
  fv = c(-Inf, sort(unique(switch(PS + 1, test, predito))))
  nr = nrow(m)
  m = apply(m, 2, cumsum)
  
  sns = (m[nr, 2] - m[, 2])/m[nr, 2]
  spc = m[, 1]/m[nr, 1]
  res = data.frame(cbind(sns, spc, fv))
  names(res) = c("sens", "spec", rnam)
  auc = sum((res[-1,"sens"]+res[-nr,"sens"])/2*abs(diff(1-res[,"spec"])))
  
  mx = max(res[, 1] + res[, 2])
  mhv = which((res[, 1] + res[, 2]) == mx)[1]
  mxf = fv[mhv]
  
  sen = res[mhv,1]
  esp = res[mhv,2]
  aux = ifelse(predito<=mxf, 0, 1)
  tab = table(aux, verdadeiro)
  
  list(tabela=tab, AUC=auc, sensibilidade=sen, especificidade=esp, corte=mxf,
       sen_esp=res[,1:2])
}

PontosCorte <- function (x, data) {
  a <- 1
  ncols <- (min(x)+1):(max(x)-1) %>% length()
  cortes <- (min(x)+1):(max(x)-1)
  Final <- cbind(cortes,0,0,0,0)
  PC <- matrix(0L, nrow = length(x), ncol = ncols) 
  for (i in (min(x)+1):(max(x)-1)) {
    Real<- case_when(x <= i ~ 0,
                     x > i ~ 1) %>% 
      as.data.frame()
    PC[,a] <- Real[,1]
    data$Real <- Real[,1]
    ### Logistic Regressions
    m1 <- glm(Real ~  Grupo ,family = binomial, data = data)
    p1<- predict(m1, type="response") # predicting from the model
    mod1_pred<- prediction(p1, m1$y) # Object of class "prediction"
    performance(mod1_pred,"tpr","fpr") 
    mod1_perf_acc <- performance(mod1_pred, measure = "acc")
    ponto1<- unlist(mod1_perf_acc@x.values)[which(unlist(mod1_perf_acc@y.values)==max(unlist(mod1_perf_acc@y.values)))[1]]
    ponto1<- roc_cond(p1, m1$y)$corte
    pred1<- ifelse(p1>ponto1, 1, 0)
    t1<- table(pred1, m1$y)
    t2<- prop.table(t1, 2) # esp e sen
    esp<- t2[1,1]
    sen<- t2[2,2]
    media <- (esp + sen)/2
    auc<- performance(mod1_pred, "auc")@"y.values"[[1]]
    Final[a,2] <- esp # Esp
    Final[a,3] <- sen # Sens
    Final[a,4] <- media # Average among of 2
    Final[a,5] <- auc # AUC
    #### Going to the next cut point
    a <- a + 1
  }
  colnames(Final) <- c("Ponto de Corte", "Especificidade", "Sensibilidade", "Média", "AUC")
  Final
}

classificacao_leads <- function(mod, dados, metodo){
  
  if (metodo == "ensemble"){
    dados <- dados %>%
      mutate(prob = 1 - predict(mod, dados, type = "prob"))
  } else {
    dados <- dados <- dados %>% 
      mutate(prob = predict(mod, dados, type = "response"))
  }
  
  # Calculating the cut-off point
  measure <- measureit(score = dados$prob, 
                       class = dados$CONVERSAO,
                       measure = c("ACC", "SENS", "SPEC"))
  ponto_corte <- data.frame(Cutoff = measure[["Cutoff"]],
                            SENS = measure[["SENS"]],
                            SPEC = measure[["SPEC"]]) %>% 
    mutate(media = (SENS + SPEC)/2) %>% 
    slice_max(order_by = media, n = 1) %>% pull(Cutoff)
  
  # Classification
  dados<- dados %>%  
    mutate(classe = ifelse(prob > ponto_corte, "Sim", "Não"))
  
  # Confusion Matrix
  mat_confusao <- table(dados %>% pull(CONVERSAO), 
                        dados %>% pull(classe))
  rownames(mat_confusao) <- paste0(rownames(mat_confusao), "_real")
  colnames(mat_confusao) <- paste0(colnames(mat_confusao), "_predito")
  
  # VPN
  vpn <- mat_confusao[1]/(mat_confusao[1] + mat_confusao[3])
  
  # VPP
  vpp <- mat_confusao[4]/(mat_confusao[2] + mat_confusao[4])
  
  # Accuray
  library(yardstick)
  acc <- accuracy_vec(as.factor(dados %>% pull(CONVERSAO)), 
                      as.factor(dados %>% pull(classe)))
  
  # AUC
  auc <- auc(dados %>% pull(CONVERSAO), dados %>% pull(prob))
  
  return(list("Ponto de Corte" = ponto_corte, 
              "Matriz de Confusão" = mat_confusao, 
              "VPP" = vpp, "VPN" = vpn, "AUC" = auc,
              "Acurácia" = acc))
  
}

##### loading database ------------------------------------------------

# Remember to set GLMs apart from other model types

explicativas <- c("ORIGEM",
                  "MARCA_VEICULO_ATUAL_FORD",
                  #"ANO_MODELO_ATUAL",
                  "REGIAO",
                  "TRANSMISSAO_VEICULO_INTERESSE",
                  "POSSUI_FORD_CREDIT",
                  "VEICULO_INTERESSE",
                  "class_prob_recompra",
                  "SITUACAO_CADASTRAL_ENR",
                  "SEXO_ENR",
                  "ESTADO_CIVIL_ENR",
                  "FLAG_SERVIDOR_PUBLICO_ENR",
                  "ESCOLARIDADE_ENR",
                  "FLAG_PRODUTOR_RURAL_ENR",
                  "FAIXA_RENDA_ENR",
                  "TRIAGEM_RISCO_ENR",
                  "ATIVIDADE_CONSUMO_ENR",
                  "CLASSE_SOCIAL_ENR",
                  "FLAG_REPRESENTANTE_LEGAL_ENR",
                  "PROPENSAO_POSSECARTAOCREDITO_ENR",
                  "PROPENSAO_LUXO_ENR",
                  "PROPENSAO_TVASSINATURA_ENR",
                  "PROPENSAO_BANDALARGA_ENR",
                  "PROPENSAO_CREDIMOB_ENR",
                  "PROPENSAO_ECOMMERCE_ENR",
                  "PROPENSAO_CREDCONSIG_ENR",
                  "PROPENSAO_MOBILE_ENR",
                  "PROPENSAO_VIAGEMTURISMO_ENR",
                  "FLAG_ARQUIVO_NOVO")

# Experian data ----
serasa <- readRDS("dados/dados_lead_compra_site/dados_serasa.rds")
serasa %>% dim() # 44919   114 (Expected 615634, ie 570715 lines less)
valores_unicos <- serasa %>% distinct(Cpf_Padr, .keep_all = T) # I had to update the variable = 36652, IE, 8267 repited lines


# Leads data (No Experian) -----
dados_leads <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds") %>% 
  mutate(ID = as.character(ID)) %>% 
  mutate(presenca_serasa = ifelse(ID %in% valores_unicos$CPF, "Presente", "Ausente")) %>% 
  mutate(
    MARCA_VEICULO_ATUAL_FORD = case_when(
      is.na(MARCA_VEICULO_ATUAL_FORD) ~ "Não possui carro",
      TRUE ~ MARCA_VEICULO_ATUAL_FORD),
    SUB_ORIGEM = case_when(
      SUB_ORIGEM %in% c("Facebook","Instagram") ~ "Redes Sociais",
      SUB_ORIGEM %in% c("Ford Credit","Ford Sempre","Ranger Estande Virtual",
                        "Direct Sales","Venda Direta Frotista") ~ "Vendas Ford",
      SUB_ORIGEM %in% c("OLX","Hotsite","Website - KMI","Icarros","Google","Website") ~ "Sites",
      is.na(SUB_ORIGEM) ~ NA_character_,
      TRUE ~SUB_ORIGEM
    ),
    ORIGEM = ifelse(is.na(SUB_ORIGEM),ORIGEM_LEAD,SUB_ORIGEM),
    ANO_MODELO_ATUAL = ifelse(is.na(ANO_MODELO_ATUAL), "Não informado", as.character(ANO_MODELO_ATUAL))
  ) %>% 
  mutate(across(where(is.character), as.factor)) %>% #error in individual car models
  mutate(CONVERSAO = factor(ifelse(CONVERSAO==0 | is.na(CONVERSAO),"Não","Sim"),c("Não","Sim")))


#### Leads data + Experian -----
dados_filtrado_serasa <- readRDS("dados/dados_lead_compra_site/dados_lead_final_filtrado2.rds") %>% 
  mutate(ID = as.character(ID)) %>% 
  inner_join(valores_unicos %>% mutate(CPF = as.character(Cpf_Padr)), by = c("ID" = "CPF"))

na_char <- function(vetor){fct_explicit_na(as.factor(vetor), na_level = "NA")}

dados_filtrado_serasa <- dados_filtrado_serasa %>%
  mutate_if(is.character, na_char) %>% 
  mutate(CONVERSAO = ifelse(CONVERSAO.x == 0 | is.na(CONVERSAO.x), 
                            "Não comprou", "Comprou"),
         SITUACAO_CADASTRAL_ENR = ifelse(SITUACAO_CADASTRAL_ENR == "REGULAR",
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

dados_filtrado_serasa <- dados_filtrado_serasa %>% 
  mutate(
    MARCA_VEICULO_ATUAL_FORD = case_when(
      is.na(as.character(MARCA_VEICULO_ATUAL_FORD.y)) ~ "Não possui carro", # I chose the ".y" because I had less NA's
      TRUE ~ as.character(MARCA_VEICULO_ATUAL_FORD.y)),
    SUB_ORIGEM = case_when(
      as.character(SUB_ORIGEM.x) %in% c("Facebook","Instagram") ~ "Redes Sociais",
      as.character(SUB_ORIGEM.x) %in% c("Ford Credit","Ford Sempre","Ranger Estande Virtual",
                                        "Direct Sales","Venda Direta Frotista") ~ "Vendas Ford",
      as.character(SUB_ORIGEM.x) %in% c("OLX","Hotsite","Website - KMI","Icarros","Google","Website") ~ "Sites",
      is.na(SUB_ORIGEM.x) ~ NA_character_,
      TRUE ~ as.character(SUB_ORIGEM.x)
    ),
    ORIGEM = ifelse(is.na(SUB_ORIGEM),ORIGEM_LEAD.x,SUB_ORIGEM) # Correcting the cases where there were empty values ("")
  ) %>% 
  mutate(across(where(is.character), as.factor)) %>% #error in individual car models
  mutate(CONVERSAO = factor(ifelse(CONVERSAO=="Não comprou","Não","Sim"),c("Não","Sim")))


##### Other manipulations specific to the models
dados_filtrado_serasa <- dados_filtrado_serasa %>% 
  mutate(REGIAO = fct_relevel(dados_filtrado_serasa%>% pull(REGIAO.x), "Não especificado"), # x = more data
         VEICULO_INTERESSE = fct_relevel(dados_filtrado_serasa%>% pull(VEICULO_INTERESSE.x), "TERRITORY"),
         class_prob_recompra = fct_relevel(dados_filtrado_serasa%>% pull(class_prob_recompra.x ), "Não está na base recompra"), # 26.69 is the actual repurchase average
         MARCA_VEICULO_ATUAL_FORD = fct_relevel(dados_filtrado_serasa%>% pull(MARCA_VEICULO_ATUAL_FORD.y), "NA"))

dados_leads <- dados_leads %>% 
  mutate(ORIGEM = fct_relevel(dados_leads %>% pull(ORIGEM), "MF Rural"),
         REGIAO = fct_relevel(dados_leads %>% pull(REGIAO), "Não especificado"),
         VEICULO_INTERESSE = fct_relevel(dados_leads %>% pull(VEICULO_INTERESSE), "TERRITORY"),
         class_prob_recompra = fct_relevel(dados_leads %>% pull(class_prob_recompra), "Não está na base recompra"),
         MARCA_VEICULO_ATUAL_FORD = fct_relevel(dados_leads %>% pull(MARCA_VEICULO_ATUAL_FORD), "Não é ford")) # %>%  # Previously it was "Don't have a car, but they don't exist anymore NA's"
# filter(!str_detect(ORIGEM, "Website"))

set.seed(421) # old = 25

dados_mod_serasa <- initial_split(dados_filtrado_serasa, prop = 0.7)
dados_treino_serasa <- rsample::training(dados_mod_serasa) # TRAINING = train
dados_teste_serasa <- rsample::testing(dados_mod_serasa) # testing = test

dados_mod_fora <- initial_split(dados_leads, prop = 0.7)
dados_treino_fora <- rsample::training(dados_mod_fora) 
dados_teste_fora <- rsample::testing(dados_mod_fora)



##### Grouping vehicles with low frequency and making necessary recategorizations ----

### Experian
dados_filtrado_serasa$VEICULO_INTERESSE %>% table()

dados_filtrado_serasa_mod <- dados_filtrado_serasa %>% 
  mutate(VEICULO_INTERESSE_cat = case_when(
    VEICULO_INTERESSE %in% c(# "MUSTANG", 
      "MAVERICK", # New vehicle being considered
      "EDGE", 
      "TRANSIT") ~ "VEICULOS ESPECIAIS", # Mustang was removed from the categorization as it already had 972 cases
    TRUE ~ as.character(VEICULO_INTERESSE)),
    ANO_MODELO_ATUAL = case_when(
      ANO_MODELO_ATUAL.x %in% c("2022", "2023") ~ "A partir de 2022", # There were only five in 2023
      is.na(ANO_MODELO_ATUAL.x) ~ "Não informado",
      TRUE ~ as.character(ANO_MODELO_ATUAL.x)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
        ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "MF Rural" ~ "Website/Linkedin/MF Rural",
      TRUE ~ as.character(ORIGEM)
    ), 
    MARCA_VEICULO_ATUAL_FORD_cat = case_when(
      MARCA_VEICULO_ATUAL_FORD == "NA" ~ "Não informado",
      TRUE ~ as.character(MARCA_VEICULO_ATUAL_FORD)
    )
  )

dados_filtrado_serasa_mod$VEICULO_INTERESSE_cat %>% table()
dados_filtrado_serasa_mod$ANO_MODELO_ATUAL %>% table() # 2023 only five categorizations
dados_filtrado_serasa_mod$ORIGEM_cat %>% table() 
dados_filtrado_serasa_mod$MARCA_VEICULO_ATUAL_FORD_cat %>% table() 


### Leads
dados_leads$VEICULO_INTERESSE %>% table()

dados_leads_mod <- dados_leads %>% 
  mutate(FORMA_PAGAMENTO = case_when(
    is.na(FORMA_PAGAMENTO) |  FORMA_PAGAMENTO == ""~ "Não Informado",
    TRUE ~ as.character(FORMA_PAGAMENTO)),
    VALOR_PARCELA_PODE_PAGAR =  case_when(
      is.na(VALOR_PARCELA_PODE_PAGAR) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_PARCELA_PODE_PAGAR)),
    VALOR_ENTRADA = case_when(
      is.na(VALOR_ENTRADA) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_ENTRADA)),
    CONVERSAO_cat = case_when(
      CONVERSAO == "Sim" ~ 1,
      CONVERSAO == "Não" ~ 0
      ),
    ANO_MODELO_ATUAL_cat = case_when(
      ANO_MODELO_ATUAL %in% c("2022", "2023") ~ "A partir de 2022", # EThere were only six cases for 2023
      TRUE ~ as.character(ANO_MODELO_ATUAL)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
      ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "Agrofy" ~ "Website/Linkedin/Agrofy",
      TRUE ~ as.character(ORIGEM)
    )
  )

dados_leads_mod$FORMA_PAGAMENTO %>% table()
dados_leads_mod$VALOR_PARCELA_PODE_PAGAR %>% table()
dados_leads_mod$VALOR_ENTRADA %>% table()
dados_leads_mod$CONVERSAO_cat %>% table()
dados_leads_mod$ANO_MODELO_ATUAL_cat %>% table()
dados_leads_mod$ORIGEM_cat %>% table()



##### Logistics Models ----

### Experian model (Leads + Experian) ----

# Update whenever I run this code monthly ====

# Variables that were significant to the Experian model
var_sig_total <- c(
  "ORIGEM_cat", # - 1ª elimination
  "MARCA_VEICULO_ATUAL_FORD_cat",
  "ANO_MODELO_ATUAL", # more updated  - 3ª elimination
  "REGIAO",
  "TRANSMISSAO_VEICULO_INTERESSE.x", #- 7ª (?) elimination
  # "POSSUI_FORD_CREDIT.x", # - 3ª elimination
  "VEICULO_INTERESSE_cat",
  "class_prob_recompra",
  "SITUACAO_CADASTRAL_ENR", # - 7ª elimination
   #"SEXO_ENR", # - only one level
   #"ESTADO_CIVIL_ENR", # - only one level
  "FLAG_SERVIDOR_PUBLICO_ENR",
   #"ESCOLARIDADE_ENR", # - only one level
  "FLAG_PRODUTOR_RURAL_ENR",
   # "FAIXA_RENDA_ENR", # - only one level
   # "TRIAGEM_RISCO_ENR", # - only one level
  # "ATIVIDADE_CONSUMO_ENR", # - only one level
 #  "CLASSE_SOCIAL_ENR", # - only one level
  #  "FLAG_REPRESENTANTE_LEGAL_ENR", # - 2ª elimination
   "PROPENSAO_POSSECARTAOCREDITO_ENR", # - 7ª elimination (was practically significant 0.0576)
  # "PROPENSAO_LUXO_ENR", # - only one level
  # "PROPENSAO_TVASSINATURA_ENR", # - only one level
  # "PROPENSAO_BANDALARGA_ENR", # - only one level
  # "PROPENSAO_CREDIMOB_ENR", # - only one level
  #  "PROPENSAO_ECOMMERCE_ENR", # - 4ª elimination
  #  "PROPENSAO_CREDCONSIG_ENR", # - 1ª elimination
   # "PROPENSAO_MOBILE_ENR", # - only one level
  # "PROPENSAO_VIAGEMTURISMO_ENR" , # - 6ª elimination
  "FLAG_ARQUIVO_NOVO" #- 1ª elimination
)

dados_filtrado_serasa_mod %>%
  dplyr::select(var_sig_total) %>% 
  rowwise() %>%
  mutate(Count_NA = sum(is.na(cur_data()))) %>% # Counting NA's of each line
  ungroup() %>% 
  dplyr::select(Count_NA) %>% 
  table()

for (i in 1:length(var_sig_total)) {
  Tabela <- dados_filtrado_serasa_mod %>% 
    dplyr::select(var_sig_total[i]) %>% 
    table()
  print(Tabela)
}

# model itself
formula_todos = as.formula(paste('CONVERSAO ~',paste(var_sig_total, collapse =' + ')))
mod_log_serasa <- glm(formula_todos, family = binomial, data = dados_filtrado_serasa_mod)
mod_log_serasa %>% summary()

Tab <- summary(mod_log_serasa)$coefficients %>% as.data.frame()
colnames(Tab) <- c("Beta","E.P.","Z","Valor-p")
Max <- Tab[,4] %>% max() 
Tab %>% filter(`Valor-p` == Max)

# Saving models that will be displayed on the "Modeling" page
saveRDS(mod_log_serasa %>% tidy(conf.int = TRUE), 
        "atualizar_analise_site/4_modelos/todos/intervalos_log_serasa_todos_site.rds", version = 2)


### Leads model ----
dados_leads %>% 
  dplyr::select("ORIGEM","MARCA_VEICULO_ATUAL_FORD","ANO_MODELO_ATUAL","FORMA_PAGAMENTO","VALOR_PARCELA_PODE_PAGAR", "VALOR_ENTRADA", "REGIAO", "POSSUI_FORD_CREDIT", "VEICULO_INTERESSE", "class_prob_recompra") %>%
  filter(VEICULO_INTERESSE == "TRANSIT")


# Update whenever I run this code monthly ====
var_sig <- c(
   "ORIGEM_cat", # - 3ª elimination
  "MARCA_VEICULO_ATUAL_FORD",
  "ANO_MODELO_ATUAL_cat",
  "FORMA_PAGAMENTO",
  # "NAO_SEI_VALOR_ENTRADA", # - 3ª elimination
  "VALOR_PARCELA_PODE_PAGAR",
  "VALOR_ENTRADA", 
  # "REGIAO", # - 2ª elimination
  "TRANSMISSAO_VEICULO_INTERESSE",# - 3ª elimination
  # "POSSUI_FORD_CREDIT", # - 4ª elimination
  "VEICULO_INTERESSE",
  "class_prob_recompra" #,
  #  "presenca_serasa" # - 1ª elimination = only one level
)

dados_leads_mod %>%
  dplyr::select(var_sig) %>% 
  rowwise() %>%
  mutate(Count_NA = sum(is.na(cur_data()))) %>% # Counting NA's of each line
  ungroup() %>% 
  dplyr::select(Count_NA) %>% 
  table()

for (i in 1:length(var_sig)) {
  Tabela <- dados_leads_mod %>% 
    dplyr::select(var_sig[i]) %>% 
    table()
  print(Tabela)
}

formula = as.formula(paste('CONVERSAO ~',paste(var_sig,collapse =' + ')))

mod_log_leads <- glm(formula, family = binomial, data = dados_leads_mod)
mod_log_leads %>% summary()

Tab <- summary(mod_log_leads)$coefficients %>% as.data.frame()
colnames(Tab) <- c("Beta","E.P.","Z","Valor-p")
Max <- Tab[,4] %>% max() 
Tab %>% filter(`Valor-p` == Max)

# Saving models that will be displayed on the "Modeling" page
saveRDS(mod_log_leads %>% tidy(conf.int = TRUE), 
        "atualizar_analise_site/4_modelos/todos/intervalos_log_fora_todos_site.rds", version = 2)



##### Calculating and saving model metrics ----

### Experian

# Aba: Modeling - Analysis Purchase Sites
tab_log_serasa <- tab_model(mod_log_serasa, show.reflvl=TRUE, prefix.labels="label")
saveRDS(tab_log_serasa, "atualizar_analise_site/4_modelos/todos/tab_modelo_serasa_todos_site.rds", version = 2)

# training models
dados_treino_serasa_mod <- dados_treino_serasa %>% 
  mutate(VEICULO_INTERESSE_cat = case_when(
    VEICULO_INTERESSE %in% c(# "MUSTANG", 
      "MAVERICK", # New vehicle being considered
      "EDGE", 
      "TRANSIT") ~ "VEICULOS ESPECIAIS", # Mustang was removed from the categorization as it already had 972 casess
    TRUE ~ as.character(VEICULO_INTERESSE)),
    ANO_MODELO_ATUAL = case_when(
      ANO_MODELO_ATUAL.x %in% c("2022", "2023") ~ "A partir de 2022", # There were only five in 2023s
      is.na(ANO_MODELO_ATUAL.x) ~ "Não informado",
      TRUE ~ as.character(ANO_MODELO_ATUAL.x)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
      ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "MF Rural" ~ "Website/Linkedin/MF Rural",
      TRUE ~ as.character(ORIGEM)
    ), 
    MARCA_VEICULO_ATUAL_FORD_cat = case_when(
      MARCA_VEICULO_ATUAL_FORD == "NA" ~ "Não informado",
      TRUE ~ as.character(MARCA_VEICULO_ATUAL_FORD)
    )
  )
  
mod_log_treino_serasa <- glm(formula_todos, family = binomial, data = dados_treino_serasa_mod)

# Rank Metrics
class_serasa <- classificacao2(mod_log_serasa, dados_filtrado_serasa_mod, "log")

dados_teste_serasa_mod <- dados_teste_serasa %>% 
  mutate(VEICULO_INTERESSE_cat = case_when(
    VEICULO_INTERESSE %in% c(# "MUSTANG", 
      "MAVERICK", # New vehicle being considered
      "EDGE", 
      "TRANSIT") ~ "VEICULOS ESPECIAIS", # Mustang was removed from the categorization as it already had 972 cases
    TRUE ~ as.character(VEICULO_INTERESSE)),
    ANO_MODELO_ATUAL = case_when(
      ANO_MODELO_ATUAL.x %in% c("2022", "2023") ~ "A partir de 2022", # There were only five in 2023
      is.na(ANO_MODELO_ATUAL.x) ~ "Não informado",
      TRUE ~ as.character(ANO_MODELO_ATUAL.x)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
      ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "MF Rural" ~ "Website/Linkedin/MF Rural",
      TRUE ~ as.character(ORIGEM)
    ), 
    MARCA_VEICULO_ATUAL_FORD_cat = case_when(
      MARCA_VEICULO_ATUAL_FORD == "NA" ~ "Não informado",
      TRUE ~ as.character(MARCA_VEICULO_ATUAL_FORD)
    )
  )
  
class_treino_serasa <- classificacao2(mod_log_treino_serasa, 
                                      dados_teste_serasa_mod, "log")
info_serasa <- resultados_modelo(class_serasa, class_treino_serasa, "Todos", "Treino")

# Saving Rank Metrics - Modeling - Site Purchase Analysis
saveRDS(info_serasa, "dados/tabelas_site/resultados_acuracia_todos_mod_logistico_serasa.rds", version = 2)



### Leads

# Aba: Modeling - Analysis Purchase Site
tab_log_fora_serasa <- tab_model(mod_log_leads, 
                                 show.reflvl=TRUE, 
                                 prefix.labels="label")
saveRDS(tab_log_fora_serasa, "atualizar_analise_site/4_modelos/todos/tab_modelo_fora_todos_site.rds", version = 2)

# training model
dados_treino_fora_mod <- dados_treino_fora %>% 
  mutate(FORMA_PAGAMENTO = case_when(
    is.na(FORMA_PAGAMENTO) |  FORMA_PAGAMENTO == ""~ "Não Informado",
    TRUE ~ as.character(FORMA_PAGAMENTO)),
    VALOR_PARCELA_PODE_PAGAR =  case_when(
      is.na(VALOR_PARCELA_PODE_PAGAR) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_PARCELA_PODE_PAGAR)),
    VALOR_ENTRADA = case_when(
      is.na(VALOR_ENTRADA) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_ENTRADA)),
    CONVERSAO_cat = case_when(
      CONVERSAO == "Sim" ~ 1,
      CONVERSAO == "Não" ~ 0
    ),
    ANO_MODELO_ATUAL_cat = case_when(
      ANO_MODELO_ATUAL %in% c("2022", "2023") ~ "A partir de 2022", # There were only six cases for 2023
      TRUE ~ as.character(ANO_MODELO_ATUAL)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
      ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "Agrofy" ~ "Website/Linkedin/Agrofy",
      TRUE ~ as.character(ORIGEM)
    )
  )
mod_log_fora_treino_serasa <- glm(formula, family = binomial, data = dados_treino_fora_mod)


# Rank Metrics 
class_fora <- classificacao_leads(mod_log_leads, 
                                  dados_leads_mod %>% filter(presenca_serasa == "Ausente"),
                                  "log")

dados_teste_fora_mod <- dados_teste_fora %>% 
  mutate(FORMA_PAGAMENTO = case_when(
    is.na(FORMA_PAGAMENTO) |  FORMA_PAGAMENTO == ""~ "Não Informado",
    TRUE ~ as.character(FORMA_PAGAMENTO)),
    VALOR_PARCELA_PODE_PAGAR =  case_when(
      is.na(VALOR_PARCELA_PODE_PAGAR) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_PARCELA_PODE_PAGAR)),
    VALOR_ENTRADA = case_when(
      is.na(VALOR_ENTRADA) ~ "Valor não informado",
      TRUE ~ as.character(VALOR_ENTRADA)),
    CONVERSAO_cat = case_when(
      CONVERSAO == "Sim" ~ 1,
      CONVERSAO == "Não" ~ 0
    ),
    ANO_MODELO_ATUAL_cat = case_when(
      ANO_MODELO_ATUAL %in% c("2022", "2023") ~ "A partir de 2022", # There were only six cases for 2023
      TRUE ~ as.character(ANO_MODELO_ATUAL)),
    ORIGEM_cat = case_when(
      ORIGEM == "" ~ "Não informado",
      ORIGEM == "Linkedin" |
        str_detect(ORIGEM, "Website") | 
        ORIGEM == "Agrofy" ~ "Website/Linkedin/Agrofy",
      TRUE ~ as.character(ORIGEM)
    )
  )
class_treino_fora <- classificacao_leads(mod_log_fora_treino_serasa, 
                                         dados_teste_fora_mod, "log")

info_fora <- resultados_modelo(class_fora, class_treino_fora, "Todos", "Treino")

# Saving Rank Metrics - Modeling - Site Purchase Analysis
saveRDS(info_fora, "dados/tabelas_site/resultados_acuracia_todos_mod_logistico_fora.rds")




##### Aba: Lead Qualification ----


##### Recalculating the cutoff point of each model and obtaining its highest probability (75%) ----

## Experian
Predicoes.serasa <- predict(mod_log_serasa, dados_filtrado_serasa_mod, type="response")
predictions.serasa <- prediction(Predicoes.serasa,dados_filtrado_serasa_mod$CONVERSAO)
sens <- data.frame(x=unlist(performance(predictions.serasa, "sens")@x.values), # Cut OFF
                   y=unlist(performance(predictions.serasa, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions.serasa, "spec")@x.values), 
                   y=unlist(performance(predictions.serasa, "spec")@y.values))

# ROC Curve
sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.02)) +
  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none") 

# Analyzing some metrics separately
performance(predictions.serasa,"sens","spec")
performance(predictions.serasa, 'acc')

Metricas <- data.frame(
  Sensibilidade = performance(predictions.serasa, "sens")@y.values,
  Sensibilidade_corte = performance(predictions.serasa, "sens")@x.values,
  Especificidade = performance(predictions.serasa, "spec")@y.values,
  Especificidade_corte = performance(predictions.serasa, "spec")@x.values
) 
colnames(Metricas) = c("Sensibilidade","Sensibilidade_corte","Especificidade","Especificidade_corte")

Metricas_atualizada <-  Metricas %>%
  mutate(Media = (Sensibilidade + Especificidade)/2)

ponto_corte_serasa <- Metricas_atualizada %>% 
  slice_max(order_by = Media, n = 1) %>% 
  dplyr::pull(Sensibilidade_corte)

# original cut point = 0.0251
# new cut point = 0.03

## Leads 
Predicoes.leads <- predict(mod_log_leads, dados_leads_mod, type="response")
predictions.leads <- prediction(Predicoes.leads,dados_leads_mod$CONVERSAO)
sens <- data.frame(x=unlist(performance(predictions.leads, "sens")@x.values), # Cut OFF
                   y=unlist(performance(predictions.leads, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions.leads, "spec")@x.values), 
                   y=unlist(performance(predictions.leads, "spec")@y.values))

# ROC Curve
sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.02)) +
  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none") 

# Analyzing some metrics separately
performance(predictions.leads,"sens","spec")
performance(predictions.leads, 'acc')

Metricas <- data.frame(
  Sensibilidade = performance(predictions.leads, "sens")@y.values,
  Sensibilidade_corte = performance(predictions.leads, "sens")@x.values,
  Especificidade = performance(predictions.leads, "spec")@y.values,
  Especificidade_corte = performance(predictions.leads, "spec")@x.values
) 
colnames(Metricas) = c("Sensibilidade","Sensibilidade_corte","Especificidade","Especificidade_corte")

Metricas_atualizada <-  Metricas %>%
  mutate(Media = (Sensibilidade + Especificidade)/2)

ponto_corte_leads <- Metricas_atualizada %>% 
  slice_max(order_by = Media, n = 1) %>% 
  dplyr::pull(Sensibilidade_corte)

# Original cut point = 0.0167
# New cut point  = 0.0104

# Cutoff

# Removing points from the 3rd Quartile to consider as the most likely point
ponto_maior_prob_serasa <- Predicoes.serasa[Predicoes.serasa > ponto_corte_serasa] %>% quantile() %>% nth(4)
ponto_maior_prob_fora <- Predicoes.leads[Predicoes.leads > ponto_corte_leads] %>% quantile() %>% nth(4)


### Experian database
dados_filtrado_serasa_mod2 <- dados_filtrado_serasa_mod %>% 
  rename(MARCA_VEICULO_ATUAL = "MARCA_VEICULO_ATUAL.x",
         MODELO_VEICULO_ATUAL = "MODELO_VEICULO_ATUAL.x",
         ANO_MODELO_ATUAL_cat = "ANO_MODELO_ATUAL",
         FORMA_PAGAMENTO = "FORMA_PAGAMENTO.x",
         NAO_SEI_VALOR_ENTRADA = "NAO_SEI_VALOR_ENTRADA.x",
         VALOR_PARCELA_PODE_PAGAR = "VALOR_PARCELA_PODE_PAGAR.x",
         ID_PRINCIPAL = "ID_PRINCIPAL.x",
         DATA_CRIACAO = "DATA_CRIACAO.x",
         VALOR_ENTRADA = "VALOR_ENTRADA.x",
         ESTADO = "ESTADO.x",
         COD_CONCESSIONARIA = "COD_CONCESSIONARIA.x",
         TRANSMISSAO_VEICULO_INTERESSE = "TRANSMISSAO_VEICULO_INTERESSE.x",
         POSSUI_FORD_CREDIT = "POSSUI_FORD_CREDIT.x",
         TMA = "TMA.x",
         CATALOGO = "CATALOGO.x",
         DATA_COMPRA_CONVERSAO = "DATA_COMPRA_CONVERSAO.x",
         MODELO_CONVERSAO = "MODELO_CONVERSAO.x",
         DATA_ULTIMA_COMPRA = "DATA_ULTIMA_COMPRA.x",
         DS_MODELO_ATUAL = "DS_MODELO_ATUAL.x",
         TEMPO_ATE_CONVERSAO = "TEMPO_ATE_CONVERSAO.x",
         INTERVALO_TEMPO_ATE_CONVERSAO = "INTERVALO_TEMPO_ATE_CONVERSAO.x")


# Update whenever I run this code monthly ====
var_sig_total <- c(
  "ORIGEM_cat", # - 1ª elimination
  "MARCA_VEICULO_ATUAL_FORD_cat",
  "ANO_MODELO_ATUAL", # updatest  - 3ª elimination
  "REGIAO",
  "TRANSMISSAO_VEICULO_INTERESSE.x", #- 7ª (?) elimination
  # "POSSUI_FORD_CREDIT.x", # - 3ª elimination
  "VEICULO_INTERESSE_cat",
  "class_prob_recompra",
  "SITUACAO_CADASTRAL_ENR", # - 7ª elimination
  #"SEXO_ENR", # - only one level
  #"ESTADO_CIVIL_ENR", # - only one level
  "FLAG_SERVIDOR_PUBLICO_ENR",
  #"ESCOLARIDADE_ENR", # - only one level
  "FLAG_PRODUTOR_RURAL_ENR",
  # "FAIXA_RENDA_ENR", # - only one level
  # "TRIAGEM_RISCO_ENR", # - only one level
  # "ATIVIDADE_CONSUMO_ENR", # - only one level
  #  "CLASSE_SOCIAL_ENR", # - only one level
  #  "FLAG_REPRESENTANTE_LEGAL_ENR", # - 2ª elimination
  "PROPENSAO_POSSECARTAOCREDITO_ENR", # - 7ª elimination (was practically significant 0.0576)
  # "PROPENSAO_LUXO_ENR", # - only one level
  # "PROPENSAO_TVASSINATURA_ENR", # - only one level
  # "PROPENSAO_BANDALARGA_ENR", # - only one level
  # "PROPENSAO_CREDIMOB_ENR", # - only one level
  #  "PROPENSAO_ECOMMERCE_ENR", # - 4ª elimination
  #  "PROPENSAO_CREDCONSIG_ENR", # - 1ª elimination
  # "PROPENSAO_MOBILE_ENR", # - only one level
  # "PROPENSAO_VIAGEMTURISMO_ENR" , # - 6ª elimination
  "FLAG_ARQUIVO_NOVO" #- 1ª elimination
)



dados_filtrado_serasa_qualificacao <- dados_filtrado_serasa_mod2 %>% 
  mutate(PROB_LOGISTICO = predict(mod_log_serasa, type = "response"),
         presenca_serasa = "Presente") %>% 
  dplyr::select(c(names(dados_leads_mod)[c(1,31,3:6,33,8:30)],"PROB_LOGISTICO")) %>% 
  mutate(tipo_cliente = case_when(
    PROB_LOGISTICO < ponto_corte_serasa ~ "Lead Frio",
    PROB_LOGISTICO >= ponto_corte_serasa & PROB_LOGISTICO < ponto_maior_prob_serasa ~ "Lead Médio",
    TRUE ~ "Lead Quente"
  ),
  ORIGEM = ifelse(ORIGEM == "","Não informado",as.character(ORIGEM))) 

dados_filtrado_serasa_qualificacao %>% 
  group_by(tipo_cliente) %>% 
  summarise(Media = mean(PROB_LOGISTICO, na.rm = T))


### Leads database
dados_filtrado_fora_qualificacao <- dados_leads_mod %>% 
  filter(presenca_serasa == "Ausente") %>% 
  mutate(PROB_LOGISTICO = predict(mod_log_leads, dados_leads_mod %>% filter(presenca_serasa == "Ausente"), "response")) %>% 
  mutate(tipo_cliente = case_when(
    PROB_LOGISTICO < ponto_corte_leads ~ "Lead Frio",
    PROB_LOGISTICO < ponto_maior_prob_fora ~ "Lead Médio",
    TRUE ~ "Lead Quente"
  ),
  ORIGEM = ORIGEM_cat)

dados_filtrado_fora_qualificacao %>% 
  group_by(tipo_cliente) %>% 
  summarise(Media = mean(PROB_LOGISTICO, na.rm = T))

ordem_certa <- dados_leads_mod %>% pull(ID)

dados_filtrado_fora_qualificacao2 <- dados_filtrado_fora_qualificacao %>% 
  rename(ANO_MODELO_ATUAL_cat = "ANO_MODELO_ATUAL")

volta_na <- function(vetor){fct_recode(vetor, NULL = "NA")}
dados_prob <- rbind(dados_filtrado_serasa_qualificacao, dados_filtrado_fora_qualificacao[c(1,31,3:6,33,8:30,35:36)])  %>% 
  # mutate(ID = fct_relevel(as.factor(ID), ordem_certa %>% as.character())) %>% 
  mutate(ID = as.factor(ID) %>% factor(levels = c(ordem_certa %>% as.character() %>% unique()))) %>% 
  mutate_if(is.factor, volta_na) %>% 
  arrange(ID)

saveRDS(dados_prob, "dados/dados_lead_compra_site/dados_lead_final_filtrado2_presenca_serasa.rds", version = 2)  

