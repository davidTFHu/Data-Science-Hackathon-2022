Folder “Modelo Recompra”: Responsible for presenting the step-by-step of data manipulation until the creation of graphs and statistical models used to calculate the customer repurchase probability. It was composed by 7 R scripts, being 5 scripts that followed an order from 1 to 5, and 2 auxiliary scripts:

• 1_processando_dados_recompra: code that had the objective of loading the data updated monthly by the customer and manipulate it so that the data of interest relative to each vehicle could be obtained;
• 2_Modelagem: code used to perform the statistical modeling to calculate the customer repurchase probability;
• 3_criar_tabela_txt_veiculos: code responsible for consolidating the results obtained in the statistical models into a table format for all the vehicles studied, aggregating the data related to repurchase;
• 4_atualizar_tabela_modelo_veiculos: code that modified some details of the tables to present some data of interest;
• 5_pre_processando_dados_pagina_inicial: code that grouped data according to the region of origin of individuals to be used in descritive and exploratory analyses; 
• Base_Upload_Mensal: code used to certify that the database related to the vehicle repurchase that will be placed for Upload does not contain any errors; 
• Funcoes: code responsible for presenting some auxiliary functions that are used throughout the other scripts.

The scripts should be run in the following order: Scripts 1 to 5, Base_Upload_Mensal; the script "Funcoes " being used as an auxiliary for the previous analyses.
