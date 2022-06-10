# FUNÇÕES TRATAMENTO VARIÁVEIS----

# F. T. COMPRE SEM SAIR DE CASA ----

# saber valor entrada

classificar_saber_valor_entrada <- function(x){
  case_when(
    x == 0 ~ "Sim",
    x == 1 ~"Não",
    TRUE ~ NA_character_
  )
}

# state

# classificar_estado <- function(x){
#   case_when(
#     x == "Bahia" ~ "BA",
#     TRUE ~x
#   )
# }

classificar_estado <- function(x){
  case_when(
    x %in% c("São Paulo","SÃO PAULO") ~ "SP",
    x %in% c("Bahia") ~ "BA",
    x %in% c("MARANHAO","Maranhão") ~ "MA",
    x %in% c("Amapá") ~ "AP",
    x %in% c("Solteiro(a)","California","andre.cenci@hotmail.com") ~ NA_character_,
    x %in% c("PARANA","Parana","Paraná") ~ "PR",
    TRUE ~x
  )
}

classificar_regiao <- function(x){
  case_when(
    x %in% c("PR", "SC", "RS") ~ "Sul",
    x %in% c("MG", "SP", "ES", "RJ") ~ "Sudeste",
    x %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
    x %in% c("BA", "PI", "MA", "CE", "RN", "PB", "PE", "AL", "SE") ~ "Nordeste",
    is.na(x) ~ "Não especificado",
    TRUE ~ "Norte"
  )
}

# vehicle brand

classificar_marca_veiculo_ford <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"FORD|FORDO|FORDE|FROD|KA|ECO SPORT|ECO ESPORT|ECOSPORT|FODR
               ECOSPORTE|FORF|ECOESPORT|FOCUS|EDGE|RANGER|NEW FIESTA|FIESTA|FUSION") ~ "Ford",
    is.na(x) ~ NA_character_,
    TRUE ~ "Não é ford" 
  )
}

classificar_marca_veiculo <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"FORD|FORDO|FORDE|FROD|KA|ECO SPORT|ECO ESPORT|ECOSPORT|FODR
               |FORF|ECOESPORT|FOCUS|EDGE|RANGER|NEW FIESTA|FIESTA") ~ "FORD",
    str_detect(x %>% str_to_upper(),"HONDA|CIVIC|HRV") ~ "HONDA",
    str_detect(x %>% str_to_upper(),"MITSUBISH|MITISUBISH|OUTLANDER") ~ "MITSUBISHI",
    str_detect(x %>% str_to_upper(),"LAND ROVER") ~ "LAND ROVER",
    str_detect(x %>% str_to_upper(),"YAMAHA") ~ "YAMAHA",
    str_detect(x %>% str_to_upper(),"TOYOTA|COROLLA|ETIOS|HILUX") ~ "TOYOTA",
    str_detect(x %>% str_to_upper(),"SUZUKI") ~ "SUZUKI",
    str_detect(x %>% str_to_upper(),"FIAT|FAIT|PALIO|TORO|PALIIO|ARGO") ~ "FIAT",
    str_detect(x %>% str_to_upper(),"JEEP|COMPASS") ~ "JEEP",
    str_detect(x %>% str_to_upper(),"VOLVO") ~ "VOLVO",
    str_detect(x %>% str_to_upper(),"RENAULT|RENALT|REMAULT|RENOU|RENUALT|RENAUT|REUNAULT|SANDERO|
               SADERO|RENNAUT|REAUTY|RANAUT|RENUT|RENAUD|KWID|DUSTER|RENAUL") ~ "RENAULT",
    str_detect(x %>% str_to_upper(),"PEUGEOT|PEGEUOT|PEUJOT|PEUGEOUT|PEGOUTT|PEGOUTE|3008|PEUGOUT") ~ "PEUGEOT",
    str_detect(x %>% str_to_upper(),"MERCEDES") ~ "MERCEDES",
    str_detect(x %>% str_to_upper(),"AUDI|AIDI") ~ "AUDI",
    str_detect(x %>% str_to_upper(),"KIA") ~ "KIA",
    str_detect(x %>% str_to_upper(),"CITROEN|CTROEN|CITROET|CITROE") ~ "CITROEN",
    str_detect(x %>% str_to_upper(),"NISSAN|VERSA|MARCH") ~ "NISSAN",
    str_detect(x %>% str_to_upper(),"CHERY") ~ "CHERY",
    str_detect(x %>% str_to_upper(),"HYUNDAI|HYNDAY|HB20|HYANDAI|HYNDUAI|HIUNDAY|CRETA|HUNDAY|HYNDAI|HIUNDAI") ~ "HYUNDAI",
    str_detect(x %>% str_to_upper(),"VOLKSWAGEN|VOLKSWAGEM|WOLSVAGEM|FOX|GOL|WOLKSWAGEN|WOKVASGEM|
               WOLK|WOKSVAGEN|WOKWAGEM|VOLKWA|VOLWAGEN|VOLWAGEM|SAVEIRO|VW|POLO
               |VOLKS|VOLWVAGEM|WOLKS|WOLSKWAGEN") ~ "VOLKSWAGEN",
    str_detect(x %>% str_to_upper(),"CHEVROLET|CHEVORET|CREVOLET|CREVROLET|CHEBROLLET|
               CHEVORLET|CHRVROLET|CHEVLOLET|CHEVROLE|CHERVOLET|CRUZE|CELTA|CORSA
               |AGILE|ONIX|CHEFROLET") ~ "CHEVROLET",
    str_detect(x %>% str_to_upper(),"JAC") ~ "JAC",
    str_detect(x %>% str_to_upper(),"BMW") ~ "BMW",
    str_detect(x %>% str_to_upper(),"LIFAN") ~ "LIFAN",
    str_detect(x %>% str_to_upper(),"SUBARU") ~ "SUBARU",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

# classify vehicle model

classificar_modelo_veiculo <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"KA SEDAN") ~ "KA SEDAN",
    str_detect(x %>% str_to_upper(),"KA") ~ "KA",
    str_detect(x %>% str_to_upper(),"ECSPORT|ECO SPORT|ECOSPORT|ECO ESPORT|ECOPORT|ECO ESPORTE|ECOESPORT") ~ "ECO SPORT",
    str_detect(x %>% str_to_upper(),"RANGER|RAGER|RAANGER|RSNGER|RANGE") ~ "RANGER",
    str_detect(x %>% str_to_upper(),"PALIO") ~ "PALIO",
    str_detect(x %>% str_to_upper(),"NEW FIESTA|FISTA|FIESTA|FIESRA|FUESTA") ~ "NEW FIESTA",
    str_detect(x %>% str_to_upper(),"FUSION|FISION|FUZION") ~ "FUSION",
    str_detect(x %>% str_to_upper(),"FOCUS|FOCOS") ~ "FOCUS",
    str_detect(x %>% str_to_upper(),"COROLLA") ~ "COROLLA",
    str_detect(x %>% str_to_upper(),"CRUZE") ~ "CRUZE",
    str_detect(x %>% str_to_upper(),"DUSTER") ~ "DUSTER",
    str_detect(x %>% str_to_upper(),"HILUX") ~ "HILUX",
    str_detect(x %>% str_to_upper(),"GRAN SIENA|GRAN SIANA") ~ "GRAN SIENA",
    str_detect(x %>% str_to_upper(),"EDGE") ~ "EDGE",
    str_detect(x %>% str_to_upper(),"GOL") ~ "GOL",
    str_detect(x %>% str_to_upper(),"ONIX") ~ "ONIX",
    str_detect(x %>% str_to_upper(),"CELTA") ~ "CELTA",
    str_detect(x %>% str_to_upper(),"PRISMA") ~ "PRISMA",
    str_detect(x %>% str_to_upper(),"COMPASS") ~ "COMPASS",
    str_detect(x %>% str_to_upper(),"CIVIC") ~ "CIVIC",
    str_detect(x %>% str_to_upper(),"RENEGADE") ~ "RENEGADE",
    str_detect(x %>% str_to_upper(),"POLO") ~ "POLO",
    str_detect(x %>% str_to_upper(),"FOX") ~ "FOX",
    str_detect(x %>% str_to_upper(),"KWID") ~ "KWID",
    str_detect(x %>% str_to_upper(),"POLO") ~ "POLO",
    str_detect(x %>% str_to_upper(),"ETIOS") ~ "ETIOS",
    str_detect(x %>% str_to_upper(),"SAVEIRO") ~ "SAVEIRO",
    str_detect(x %>% str_to_upper(),"VOYAGE") ~ "VOYAGE",
    str_detect(x %>% str_to_upper(),"HB20") ~ "HB20",
    str_detect(x %>% str_to_upper(),"UNO") ~ "UNO",
    str_detect(x %>% str_to_upper(),"MOBI") ~ "MOBI",
    str_detect(x %>% str_to_upper(),"CORSA") ~ "CORSA",
    str_detect(x %>% str_to_upper(),"SANDERO") ~ "SANDERO",
    str_detect(x %>% str_to_upper(),"TERRITORY") ~ "TERRITORY",
    str_detect(x %>% str_to_upper(),"SIENA") ~ "SIENA",
    str_detect(x %>% str_to_upper(),"CLASSIC") ~ "CLASSIC",
    str_detect(x %>% str_to_upper(),"LOGAN") ~ "LOGAN",
    str_detect(x %>% str_to_upper(),"PUNTO") ~ "PUNTO",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

# have ford credit

classificar_possui_ford_credit <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"FORD CREDIT") ~ "Sim",
    is.na(x) ~ NA_character_,
    TRUE ~ "Não"
  )
}

# average car price

classificar_preço_mediano <- function(x){
  case_when(
    x <= 50000 ~ "Menor ou igual a 50.000,00 reais",
    x>50000 & x<= 60000 ~ "Entre 50.000,01 - 60.000,00 reais",
    x>60000 & x<= 80000  ~ "Entre 60.000,01 - 80.000,00 reais",
    x>80000 ~ "Maior que 80.000,00 reais"
  )
}

# payment method

# classificar_forma_pagamento <- function(x){
#   case_when(
#     x == "Ã¡ Vista" ~ "À vista",
#     TRUE ~x
#   )
# }

classificar_forma_pagamento <- function(x){
  case_when(
    x == "á Vista" ~ "À vista",
    TRUE ~x
  )
}

# installment value

# classificar_parcela <- function(x){
#   case_when(
#     x<=300 ~ "Entre 0 - 300,00 reais",
#     x>300 & x<=600 ~ "Entre 300,01 - 600,00 reais",
#     x>600 & x<=1000 ~ "Entre 600,01 - 1000,00 reais",
#     x > 1000 ~ "Mais de 1000,00 reais",
#   )
# }

classificar_parcela <- function(x){
  case_when(
    x<=300 ~ "Entre 0 - 300,00 reais",
    x>300 & x<=700 ~ "Entre 300,01 - 700,00 reais",
    x>700 & x<=1000 ~ "Entre 700,01 - 1000,00 reais",
    x > 1000 ~ "Mais de 1000,00 reais",
    is.na(x) ~ NA_character_,
    TRUE ~ paste(x)
  )
}

classificar_parcela_filtro <- function(x){
  case_when(
    x==0 ~ "R$0,00",
    x>0 & x<=1100 ~ "Entre 0,01 - 1100,00 reais",
    x>1100 & x<=2000 ~ "Entre 1100,01 - 2000,00 reais",
    x > 2000 ~ "Mais de 2000,00 reais",
    is.na(x) ~ NA_character_,
    TRUE ~ paste(x)
  )
}

# input value

classificar_entrada <- function(x){
  case_when(
    x==0 ~ "R$0,00",
    x>0 & x<=8000 ~ "Entre 0,01 - 8000,00 reais",
    x>8000 & x<=15000 ~ "Entre 8000,01 - 15000,00 reais",
    x>15000 & x<=28000 ~ "Entre 15000,01 - 28000,00 reais",
    x > 28000 ~ "Mais de 28000,00 reais",
    is.na(x) ~ NA_character_,
    TRUE ~ paste(x)
  )
}


classificar_entrada_filtro <- function(x){
  case_when(
    x==0 ~ "R$0,00",
    x>0 & x<=18000 ~ "Entre 0,01 - 18000,00 reais",
    x>18000 & x<=50000 ~ "Entre 18000,01 - 50000,00 reais",
    x > 50000 ~ "Mais de 50000,00 reais",
    is.na(x) ~ NA_character_,
    TRUE ~ paste(x)
  )
}
# vehicle of interest

veiculo_interesse_dist <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"RANGER") ~ "RANGER",
    str_detect(x %>% str_to_upper(),"KA SEDAN|KA+ SEDAN") ~ "KA SEDAN",
    str_detect(x %>% str_to_upper(),"KA") ~ "KA",
    str_detect(x %>% str_to_upper(),"ECOSPORT") ~ "ECOSPORT",
    str_detect(x %>% str_to_upper(),"TERRITORY") ~ "TERRITORY",
    str_detect(x %>% str_to_upper(),"MUSTANG") ~ "MUSTANG",
    str_detect(x %>% str_to_upper(),"FUSION") ~ "FUSION",
    str_detect(x %>% str_to_upper(),"FIESTA") ~ "FIESTA",
    str_detect(x %>% str_to_upper(),"EDGE") ~ "EDGE",
    str_detect(x %>% str_to_upper(),"MAVERICK") ~ "MAVERICK",
    str_detect(x %>% str_to_upper(),"FOCUS") ~ "FOCUS",
    str_detect(x %>% str_to_upper(),"BRONCO") ~ "BRONCO",
    str_detect(x %>% str_to_upper(),"TRANSIT") ~ "TRANSIT",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

classificar_transmissao_veiculo <- function(x){
  case_when(
    str_detect(x %>% str_to_upper()," AT|AUTOMATIC|AUTOMÁTICA") ~ "Automático",
    is.na(x) ~ NA_character_,
    TRUE ~ "Manual"
  )
}

# conversion models

# modelo_conversao_dist <- function(x){
#   case_when(
#     str_detect(x,"RANGER") ~ "RANGER",
#     str_detect(x ,"SD") ~ "KA SEDAN",
#     str_detect(x,"KA") ~ "KA",
#     str_detect(x,"ECOSPORT") ~ "ECOSPORT",
#     str_detect(x,"TERRITORY") ~ "TERRITORY",
#     str_detect(x,"MUSTANG") ~ "MUSTANG"
#   )
# }

modelo_conversao_dist <- function(x){
  case_when(
    str_detect(x %>% str_trim(),"RANGER") ~ "RANGER",
    str_detect(x %>% str_trim(),"KA HATCH") ~ "KA",
    str_detect(x %>% str_trim() ,"KA+") ~ "KA SEDAN",
    str_detect(x %>% str_trim(),"EDGE") ~ "EDGE",
    str_detect(x %>% str_trim(),"ECOSPORT") ~ "ECOSPORT",
    str_detect(x %>% str_trim(),"TERRITORRY") ~ "TERRITORY",
    str_detect(x %>% str_trim(),"MUSTANG") ~ "MUSTANG",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}

# engine model conversion

motor_modelo_conversao <- function(x){
  case_when(
    str_detect(x, "1.0") ~ "1.0",
    str_detect(x, "1.5") ~ "1.5",
    str_detect(x, "2.0") ~ "2.0"
  )
}

# interval time conversation

classificar_intervalo_tempo_conversao <- function(x){
  case_when(
    x>0 & x<=40 ~ "Entre 0-40 dias",
    x>40 & x<=80 ~ "Entre 41-80 dias",
    x>80 & x<=120 ~ "Entre 81-120 dias",
    x>120 & x<=160 ~ "Entre 121-160 dias",
    x > 160 ~ "Mais de 160 dias",
  )
}

# F. T. REPURCHASE ----

classificar_nameplate <- function(x){
  case_when(
    str_detect(x %>% str_to_upper(),"RANGER") ~ "RANGER",
    str_detect(x %>% str_to_upper(),"RANGER") ~ "RANGER",
    str_detect(x %>% str_to_upper(),"KA SEDAN") ~ "KA SEDAN",
    str_detect(x %>% str_to_upper(),"KA") ~ "KA",
    str_detect(x %>% str_to_upper(),"ECOSPORT") ~ "ECOSPORT",
    str_detect(x %>% str_to_upper(),"TERRITORY") ~ "TERRITORY",
    str_detect(x %>% str_to_upper(),"MUSTANG") ~ "MUSTANG",
    str_detect(x %>% str_to_upper(),"FUSION") ~ "FUSION",
    str_detect(x %>% str_to_upper(),"FIESTA") ~ "FIESTA",
    str_detect(x %>% str_to_upper(),"EDGE") ~ "EDGE",
    str_detect(x %>% str_to_upper(),"FOCUS") ~ "FOCUS",
    str_detect(x %>% str_to_upper(),"F-250") ~ "F-250",
    is.na(x) ~ NA_character_,
    TRUE ~ x
  )
}