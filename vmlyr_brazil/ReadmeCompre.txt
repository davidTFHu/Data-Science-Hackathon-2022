Folder “Modelo Compre sem sair de casa”: Responsible for presenting the step-by-step of data manipulation until the creation of graphs and statistical models used to calculate the probability of conversion of the Lead from the website "Compre sem sair de casa”. It was composed by 6 scripts of R, being 3 scripts that followed an order from 1 to 3, and 3 auxiliary scripts:

• 1_tratamento_dados_novo: code responsible for loading the database updated monthly and join it with the databases of the previous months, removing the duplicated cases and modifying the necessary variables;
• 2_tabela_conversao_editada: code used for the creation of the statistical comparisons related to the conversion in the worked databases;
• 3_tabela_tempo_ate_conversao_editada:  code used for the creation of statistical comparisons related to the time until conversion in the worked databases;
• Base_Upload_Mensal_Site: code used to certify that the bank related to the purchases made on the site that will be uploaded does not contain any errors;
• funcoes_tratamento_variaveis: code responsible for presenting some auxiliary functions that are used throughout the other scripts for treatment of the variables;
• Modelo_Final_Atualizado: code that contained all statistical models used to calculate the probability of conversion of the Lead from the website "Compre sem sair de casa”.

The scripts should be run in the following order: Scripts 1 to 3, Modelo_Final_Atualizado, Base_Upload_Mensal_Site; with the script "funcoes_tratamento_variaveis" being used as an auxiliary for the previous analyses.
