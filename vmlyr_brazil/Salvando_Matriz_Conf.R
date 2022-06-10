##################### Saving Confusion Matrix to the Server ==============


##### Required Packagess ----
library("tidyverse")
library("highcharter")



##### confusion matrix -----

### Serasa Logistics Model - Test
tipos_modelo <- c("serasa","fora")

for(i in 1:length(tipos_modelo)){
  
  input_tipo <- tipos_modelo[i]
  input_veiculo <- "todos"
  dados_tabela <- readRDS(paste0("dados/tabelas_site/resultados_acuracia_", input_veiculo,"_mod_logistico_", input_tipo,".rds"))
  dados_tabela <- dados_tabela[[3]]
  
  Matriz_conf_Teste <- dados_tabela %>% 
    as.data.frame() %>% 
    mutate(across(where(is.factor), function(x){str_replace(x, "_", " ")})) %>% 
    hchart(hcaes(x = Var2, y = Var1, value = Freq), 
           type = "heatmap", name = "Contagem") %>% 
    hc_title(text = "Matriz de Confusão - Dados de Teste") %>% 
    hc_yAxis(title = list(text = 'Valor Real'), categories = c("Não", "Sim")) %>%
    hc_xAxis(title = list(text = 'Valor Predito'), categories = c("Não", "Sim"))  %>%
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(pointFormat = paste('{point.Var2} vs {point.Var1}: <b>{point.Freq}</b><br>')) %>% 
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE)))
  
  saveRDS(Matriz_conf_Teste, paste0("dados/Matriz_Confusao/Matriz_conf_teste_",input_tipo,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Matriz do veiculo", input_veiculo, "do banco",input_tipo, "salva!"))
  
}


### Logistics Model - Training
tipos_modelo <- c("serasa","fora")

for(i in 1:length(tipos_modelo)){
  
  input_tipo <- tipos_modelo[i]
  input_veiculo <- "todos"
  dados_tabela <- readRDS(paste0("dados/tabelas_site/resultados_acuracia_", input_veiculo,"_mod_logistico_", input_tipo,".rds"))
  dados_tabela <- dados_tabela[[4]]
  
  Matriz_conf_Treino <- dados_tabela %>% 
    as.data.frame() %>% 
    mutate(across(where(is.factor), function(x){str_replace(x, "_", " ")})) %>% 
    hchart(hcaes(x = Var2, y = Var1, value = Freq), 
           type = "heatmap", name = "Contagem") %>% 
    hc_title(text = "Matriz de Confusão - Dados de Teste") %>% 
    hc_yAxis(title = list(text = 'Valor Real'), categories = c("Não", "Sim")) %>%
    hc_xAxis(title = list(text = 'Valor Predito'), categories = c("Não", "Sim"))  %>%
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(pointFormat = paste('{point.Var2} vs {point.Var1}: <b>{point.Freq}</b><br>')) %>% 
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE)))
  
  saveRDS(Matriz_conf_Treino, paste0("dados/Matriz_Confusao/Matriz_conf_treino_",input_tipo,"_",tolower(input_veiculo),".rds"), version = 2)
  
  print(paste("Matriz do veiculo", input_veiculo, "do banco",input_tipo, "salva!"))
  
}