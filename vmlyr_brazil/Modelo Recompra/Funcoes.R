
enc2latin <- function(x) {
  Encoding(x) <- "latin1"
  return(x)
}


minerar_modelos <- function(x) {
  x %>%  
    mutate(
      modelo = case_when(
        str_detect(veiculo_interesse, "freestyle") ~ "freestyle",
        str_detect(veiculo_interesse, "se") ~ "se",
        str_detect(veiculo_interesse, "storm") ~ "storm",
        str_detect(veiculo_interesse, "titanium") ~ "titanium",
        TRUE ~ "padrão"
      ),
      motor = case_when(
        str_detect(veiculo_interesse, "2.0") ~ "2.0",
        TRUE ~ "1.5"
      ),
      automatico = case_when(
        str_detect(veiculo_interesse, "at") ~ 1,
        str_detect(veiculo_interesse, "direct") ~ 1,
        TRUE ~ 0
      ) 
    ) %>% 
    select(-veiculo_interesse)
}

classificar_modelo_anterior <- function(x) {
  case_when(
    x == "EcoSport" ~ "EcoSport",
    is.na(x) ~ "Nenhum",
    TRUE ~ "Outros"
  )
}

calcular_tempo_entre <- function(x, y) {
  diferenca <- 
    round((x - y) / 365) %>%
    as.numeric()
  case_when(
    diferenca <= 1 ~ "1 ano ou menos",
    diferenca == 2 ~ "2 anos",
    diferenca == 3 ~ "3 anos",
    diferenca == 4 ~ "4 anos",
    diferenca == 5 ~ "5 anos",
    diferenca >= 6 ~ "6 anos ou mais",
    TRUE ~ "não é cliente Ford"
  )
  
}

# data -----------------------------------------------------------------------

fatorizar <- function(x) {
  x %>% 
    mutate_if(
      function(x) all(sort(unique(x)) %in% c(0, 1)),
      function(x) ifelse(x == 1, "sim", "nao")
    ) %>% 
    mutate_all(as.factor)
}

dummificar <- function(x) {
  x %>% 
    # arrange()
    fastDummies::dummy_cols(remove_first_dummy = T) %>% # ALL correct?
    select_if(~ !is.character(.x)) %>% 
    select_if(~ !is.factor(.x))
}

# contingency ----------------------------------------------------------------

prop_fisher <- function(x) {
  tryCatch(
    prop.test(x),
    warning = function(w) fisher.test(x, simulate.p.value = TRUE),
    error = function(e) fisher.test(x, simulate.p.value = TRUE)
  )
}

testar_contingencia <- function(x) {
  fator <- x
  contingencia <- 
    dados_contingencia[, c(fator, "Recomprou")] %>% 
    na.omit() %>% 
    table()
  
  relativa <- paste0(round(contingencia / rowSums(contingencia) * 100, 2), "%")
  # p_valor <- round(prop_fisher(contingencia)$p.value, 4)
  p_valor <- prop_fisher(contingencia)$p.value
  tamanho <- dim(contingencia)[1]
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
  names(saida) <- c("Fator", "Nível", "Não recomprou", "Recomprou", "P-valor")
  saida
}

padronizar_contingencia <- function(x) {
  x %>% 
    kable(align = "c") %>% 
    kable_styling(
      bootstrap_options = c("hover", "condensed", "responsive"),
      font_size = 12
    ) %>% 
    row_spec(0, background = "#CDD2E5") %>% 
    column_spec(1:2, background = "#CDD2E5", include_thead =TRUE) %>% 
    collapse_rows(columns = c(1), valign = "middle")
}

padronizar_perfil <- function(x) {
  x %>% 
    kable(align = "c") %>% 
    kable_styling(
      bootstrap_options = c("hover", "condensed", "responsive"),
      font_size = 12
    ) %>% 
    row_spec(0, background = "#CDD2E5") %>% 
    column_spec(1, background = "#CDD2E5", include_thead =TRUE)
}

testar_contingencia_cluster <- function(x) {
  fator <- x
  dados_contingencia_cluster <- 
    dados_cluster %>% 
    arrange(cluster)
  contingencia <- 
    dados_contingencia_cluster[, c(fator, "cluster")] %>% 
    na.omit() %>% 
    table()
  
  relativa <- paste0(round(t(t(contingencia) / colSums(contingencia)) * 100, 2), "%")
  p_valor <- prop_fisher(contingencia)$p.value
  tamanho <- dim(contingencia)[1]
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
  names(saida) <-
    c("Fator", "Nível", paste0("G", dados_contingencia_cluster$cluster %>% unique), "P-valor")
  saida
}

testar_contingencia_ranking <- function(x) {
  fator <- x
  dados_contingencia_cluster <- 
    dados_ranking %>% 
    arrange(ranking)
  contingencia <- 
    dados_contingencia_cluster[, c(fator, "ranking")] %>% 
    na.omit() %>% 
    table()
  
  relativa <- paste0(round(t(t(contingencia) / colSums(contingencia)) * 100, 2), "%")
  p_valor <- prop_fisher(contingencia)$p.value
  tamanho <- dim(contingencia)[1]
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
  names(saida) <-
    c("Fator", "Nível", paste0(dados_contingencia_cluster$ranking %>% unique), "P-valor")
  saida
}




# calcular_prob_recompra_por_veiculo <- function(veiculo) {
#   # veiculo<- "RANGER"
#   
#   caminho<- paste0("./dados/veiculos/dados_", tolower(veiculo), ".rds")
#   
#   
#   lista_variaveis_excluir<- list(c("status_tributario", 
#                                    "assalariado",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "recebe_bolsa_familia", 
#                                    "representante_legal",
#                                    "comprou_online",
#                                    "possui_imovel",
#                                    "Dias_Atual", "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario", 
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "comprou_pacotes_viagem",
#                                    "possui_tv_cabo",
#                                    "cartao_credito",
#                                    "Dias_Atual", "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"),
#                                  
#                                  c("status_tributario",
#                                    "tedencia_credito", 
#                                    "possui_empregado_domestico", 
#                                    "possui_bens_luxo",
#                                    "representante_legal",
#                                    "possui_tv_cabo",
#                                    "possui_imovel",
#                                    "Dias_Atual", 
#                                    "ID", "Data_Compra","UF"))
#   
#   dados_recompra_models<- readRDS(caminho) %>% 
#     transmute(
#       ID = ID,
#       Dias_Atual = Dias_Atual, 
#       Dias_Recompra = Dias_Recompra - 2,
#       Dias_Recompra2 = Dias_Recompra^2,
#       UF = UF,
#       Data_Compra = Data_Compra,
#       Data_Compra_Anterior = Data_Compra_Anterior,
#       Modelo_Atual = Modelo_Atual,
#       Modelo_Anterior = Modelo_Anterior,
#       Sem_Registro_Serasa = ifelse(is.na(ID),1,0),
#       status_tributario = status_tributario,
#       regiao = regiao,
#       numero_residentes = numero_residentes,
#       renda_domiciliar = renda_domiciliar,
#       idade = idade,
#       genero = genero,
#       possui_companheiro = possui_companheiro,
#       possui_empregado_domestico = possui_empregado_domestico,
#       escolaridade = escolaridade,
#       recebe_bolsa_familia = recebe_bolsa_familia,
#       empreendedor = empreendedor,
#       representante_legal = representante_legal,
#       produtor_rural = produtor_rural,
#       classe_economica = classe_economica,
#       renda_estimada_media = renda_estimada_media,
#       risco_credito = risco_credito,
#       atividade_credito = atividade_credito,
#       tedencia_credito = tedencia_credito,
#       possui_bens_luxo = possui_bens_luxo,
#       comprou_pacotes_viagem = comprou_pacotes_viagem,
#       cartao_credito = cartao_credito,
#       comprou_online = comprou_online,
#       possui_imovel = possui_imovel,
#       possui_tv_cabo = possui_tv_cabo,
#       possui_internet = possui_internet,
#       assalariado = assalariado,
#       possui_smartphone = possui_smartphone,
#       mosaico = mosaico,
#       Recomprou = Recomprou
#     ) %>% 
#     mutate_if(is.character, str_to_lower) %>%
#     select(
#       -possui_companheiro, 
#       -numero_residentes, 
#       -renda_domiciliar,
#       -Data_Compra_Anterior,
#       -Modelo_Atual,
#       -Modelo_Anterior
#     ) %>% 
#     na.omit() %>% 
#     select_if(function(x) length(unique(x)) > 1) %>% 
#     mutate(renda_estimada_media = 
#              case_when(renda_estimada_media <= 1250 ~ 1250,
#                        renda_estimada_media >= 8500 ~ 8500,
#                        TRUE ~ renda_estimada_media)) %>% 
#     na.omit() %>% 
#     select_if(function(x) length(unique(x)) > 1) %>% 
#     fatorizar() %>%
#     map_dfc(droplevels) %>% 
#     mutate(Dias_Recompra = as.numeric(as.character(Dias_Recompra)),
#            Dias_Recompra2 = as.numeric(as.character(Dias_Recompra2)),
#            ID = as.numeric(as.character(ID)),
#            mosaico =  as.factor(iconv(mosaico,from="UTF-8",to="ASCII//TRANSLIT")))
#   
#   
#   Percentual<- dados_recompra_models$Recomprou %>% table %>% prop.table()
#   Percentual<- Percentual[2]*100
#   Percentual<- Percentual[[1]]
#   
#   return(Percentual)
#}