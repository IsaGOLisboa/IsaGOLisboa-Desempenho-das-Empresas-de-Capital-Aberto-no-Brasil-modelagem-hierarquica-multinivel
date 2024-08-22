#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl",
             "gghalves","ggdist","tidyquant","car","nlme","lmtest",
             "fastDummies","msm","lmeInfo","jtools","gganimate","ggridges",
             "viridis","hrbrthemes","openxlsx", "stringr", "ggplot2", "cowplot", "MASS",
             "Matrix", "lme4", "DHARMa", "nortest", "MVN" )

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Algoritmo para determinação dos erros-padrão das variâncias no componente de
#efeitos aleatórios
#ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
#e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}

########################################################################################################
############## modelos sem transformação                                                      ##########
########################################################################################################
# Leitura dos dados
dados_outliers <- read.csv("dados_filtrados_hlm2.csv")
num_cnpj_unicos <- length(unique(dados_outliers$CNPJ_CIA))
cat("Número de CNPJ_CIA únicos:", num_cnpj_unicos, "\n")


# Função para remover outliers usando o método de Tukey
remover_outliers_tukey <- function(df, coluna) {
  Q1 <- quantile(df[[coluna]], 0.25)
  Q3 <- quantile(df[[coluna]], 0.75)
  IQR <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  outliers <- df[[coluna]] < limite_inferior | df[[coluna]] > limite_superior
  df_sem_outliers <- df[!outliers, ]
  
  cat("Número de outliers removidos em", coluna, ":", sum(outliers), "\n")
  return(df_sem_outliers)
}


# Removendo outliers em ROA, liquidez_corrente, margem_liquida e IPCA usando o método de Tukey
dados_sem_outliers_tukey <- remover_outliers_tukey(dados_outliers, "ROA")
dados_sem_outliers_tukey <- remover_outliers_tukey(dados_sem_outliers_tukey, "liquidez_corrente")
dados_sem_outliers_tukey <- remover_outliers_tukey(dados_sem_outliers_tukey, "margem_liquida")
dados_sem_outliers_tukey <- remover_outliers_tukey(dados_sem_outliers_tukey, "cap_terc_patrimo_liquido")


# Calculando a média e o intervalo de confiança (IC) do ROA por ano
dados_agg <- dados_sem_outliers_tukey %>%
  group_by(ano) %>%
  summarise(
    mean_ROA = mean(ROA, na.rm = TRUE),
    sd_ROA = sd(ROA, na.rm = TRUE),
    n = n(),
    se_ROA = sd_ROA / sqrt(n),
    lower = mean_ROA - 1.96 * se_ROA,
    upper = mean_ROA + 1.96 * se_ROA
  )

# Gráfico de área com banda de confiança
ggplot(dados_agg, aes(x = ano, y = mean_ROA)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Média do ROA com Banda de Confiança ao Longo do Tempo",
       x = "Ano",
       y = "Média do ROA") +
  theme_minimal()

# Selecionando algumas empresas para comparar (exemplo com 5 empresas)
empresas_selecionadas <- unique(dados_sem_outliers_tukey$CNPJ_CIA)[1:5]  # Selecione 5 empresas ou mais


# Filtrando o dataframe para incluir apenas essas empresas
dados_filtrados <- dados_sem_outliers_tukey %>%
  filter(CNPJ_CIA %in% empresas_selecionadas)

write.xlsx(dados_filtrados, "C:/Users/User/Desktop/MBA/Aulas/TCC/arq_tcc/RStudio/aderencia_normalidade/grafico_roa_empresas.xlsx")


# Gráfico de linha para cada empresa com ROA ao longo do tempo
ggplot(dados_filtrados, aes(x = ano, y = ROA, color = as.factor(CNPJ_CIA))) +
  geom_line(size = 1) +
  labs(title = "Comparação do ROA entre Empresas ao Longo do Tempo",
       x = "Ano",
       y = "ROA",
       color = "Empresa") +
  theme_minimal() +
  theme(legend.position = "bottom")



#Gráfico de Boxplot para ROA por Ano
ggplot(dados_sem_outliers_tukey, aes(x = as.factor(ano), y = ROA)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2, fill = "lightblue") +
  labs(title = "Distribuição do ROA por Ano",
       x = "Ano",
       y = "ROA") +
  theme_minimal()

colnames(dados_sem_outliers_tukey)
# Selecionar as variáveis de interesse
variaveis <- c("ROA", "margem_ebitda", "margem_liquida", "cap_terc_patrimo_liquido",
               "IPCA", "PIB", "SELIC", "divida_publica_pib", "cambio", "importacoes", "exportacoes")

# Adicionando uma coluna de IDs fictícia (opcional)
df_selecionado<- dados_outliers
df_selecionado$id <- 1:nrow(df_selecionado)
colnames(df_selecionado)[colnames(df_selecionado) == "divida_liquida_pib"] <- "divida_publica_pib"


# Selecionando apenas as colunas numéricas para padronização
df_numeric <- df_selecionado[, sapply(df_selecionado, is.numeric)]

# Padronizando as colunas numéricas
df_padronizado <- as.data.frame(scale(df_numeric))

# Adicionando uma coluna de IDs fictícia (opcional)
df_padronizado$id <- 1:nrow(df_padronizado)

# Transformando o dataframe padronizado de wide para long
df_long_padronizado <- melt(df_padronizado, id.vars = "id", variable.name = "variable", value.name = "value")

# Selecionando apenas as variáveis de interesse em df_long
df_long_filtrado <- df_long_padronizado[df_long_padronizado$variable %in% variaveis, ]

# Criando o gráfico para as variáveis filtradas
ggplot(df_long_filtrado, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Distribuição das Variáveis Selecionadas",
       x = "Variáveis",
       y = "Valores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)




# Criando as interações no DataFrame sem aplicar qualquer transformação
dados_sem_outliers_tukey$liquidez_corrente_IPCA <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$IPCA
dados_sem_outliers_tukey$margem_ebitda_IPCA <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$IPCA
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_IPCA <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$IPCA

dados_sem_outliers_tukey$liquidez_corrente_SELIC <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$SELIC
dados_sem_outliers_tukey$margem_ebitda_SELIC <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$SELIC
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_SELIC <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$SELIC

dados_sem_outliers_tukey$liquidez_corrente_PIB <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$PIB
dados_sem_outliers_tukey$margem_ebitda_PIB <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$PIB
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_PIB <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$PIB

dados_sem_outliers_tukey$liquidez_corrente_importacoes <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$importacoes
dados_sem_outliers_tukey$margem_ebitda_importacoes <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$importacoes
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_importacoes <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$importacoes


#####################################################################################################################
##########################     modelo nulo sem transformação          ###############################################
#####################################################################################################################
# Ajustando o modelo de efeitos mistos utilizando as variáveis sem transformação
modelo_nulo_st_2 <- lmer(ROA ~ 1 + 
                       (1  | empresa_factor),
                     data = dados_sem_outliers_tukey)

# Resumo do modelo ajustado
summary(modelo_nulo_st_2)

# Simulando resíduos com DHARMa para verificar a adequação do modelo
res_modelo <- simulateResiduals(fittedModel = modelo_nulo_st_2, n = 1000)

# Plotar os resíduos simulados
plot(res_modelo)

# Teste de uniformidade (normalidade dos resíduos)
testUniformity(res_modelo)

# Teste de homocedasticidade (igualdade de variâncias)
testDispersion(res_modelo)

# Plotar o histograma dos resíduos
hist(residuals(modelo_final), main = "Histograma dos Resíduos", xlab = "Resíduos", breaks = 30)

##### teste de hipóteses para variâncias dos termos de erro
# Ajustando o modelo de efeitos mistos utilizando as variáveis transformadas por Box-Cox
modelo_nulo_st <- lme(fixed = ROA ~ 1 , 
                    random = ~ 1  | empresa_factor,
                    data = dados_sem_outliers_tukey,
                    method = "REML")


# Resumo do modelo ajustado
summary(modelo_nulo_st )

stderr_nlme(modelo_nulo_st)


######Calculo LRT
# Ajuste do modelo OLS
modelo_ols <- lm(ROA ~ 1, data = dados_sem_outliers_tukey)

# Extraindo a log-verossimilhança do modelo OLS
logLik_ols <- logLik(modelo_ols)

# Extraindo a log-verossimilhança do modelo de efeitos mistos
logLik_completo <- logLik(modelo_nulo_st)

# Calculando a estatística do LRT
LRT_stat <- -2 * (logLik_ols - logLik_completo)

# Calculando o p-valor
df <- attr(logLik_completo, "df") - attr(logLik_ols, "df")
p_value <- pchisq(LRT_stat, df = df, lower.tail = FALSE)

# Exibindo os resultados
cat("LRT Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")
#####################################################################################################################
##########################     modelo inclinações aleatórias sem transformação  com dummies de setor    #############
#####################################################################################################################
dados_setor<- read.csv("dados_com_setor_economico.csv")
# Função para remover outliers usando o método de Tukey
remover_outliers_tukey <- function(df, coluna) {
  Q1 <- quantile(df[[coluna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[coluna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5 * IQR  # 1.5 é a constante padrão para detectar outliers
  limite_superior <- Q3 + 1.5 * IQR
  
  # Filtrar os dados, removendo os outliers
  df_sem_outliers <- df[df[[coluna]] >= limite_inferior & df[[coluna]] <= limite_superior, ]
  
  cat("Número de outliers removidos em", coluna, ":", nrow(df) - nrow(df_sem_outliers), "\n")
  return(df_sem_outliers)
}

# Removendo outliers nas variáveis de interesse
dados_sem_outliers <- remover_outliers_tukey(dados_setor, "ROA")
#dados_sem_outliers <- remover_outliers_tukey(dados_sem_outliers, "margem_ebitda")
#dados_sem_outliers <- remover_outliers_tukey(dados_sem_outliers, "liquidez_corrente")
#dados_sem_outliers <- remover_outliers_tukey(dados_sem_outliers, "margem_liquida")
#dados_sem_outliers <- remover_outliers_tukey(dados_sem_outliers, "cap_terc_patrimo_liquido")

# Verificando o número de observações após a remoção dos outliers
cat("Número total de observações após a remoção de outliers:", nrow(dados_sem_outliers), "\n")

# Certificando que setor_economico está como fator
dados_setor$setor_economico <- as.factor(dados_setor$setor_economico)

# Definindo a categoria de referência (por exemplo, "Setor A")
dados_setor$setor_economico <- relevel(dados_setor$setor_economico, ref = "Bens Industriais")

# Gerando as dummies novamente com a categoria de referência
dummies_setor <- model.matrix(~ setor_economico - 1, data = dados_setor)

# Convertendo para data.frame e adicionar a coluna CNPJ_CIA
dummies_setor <- as.data.frame(dummies_setor)
dummies_setor$CNPJ_CIA <- dados_setor$CNPJ_CIA

# Reordenando colunas para que CNPJ_CIA seja a primeira
dummies_setor <- dummies_setor[, c("CNPJ_CIA", setdiff(names(dummies_setor), "CNPJ_CIA"))]

# Fazendo a junção dos DataFrames usando a chave CNPJ_CIA
dados_com_dummies <- merge(dados_sem_outliers_tukey, dummies_setor, by = "CNPJ_CIA", all.x = TRUE)



colnames(dados_com_dummies)

# Função para remover acentos, substituir espaços por underscores e converter para minúsculas
ajustar_nomes_colunas <- function(nomes) {
  nomes <- tolower(nomes) # Converter para minúsculas
  nomes <- gsub(" ", "_", nomes) # Substituir espaços por underscores
  nomes <- iconv(nomes, to = "ASCII//TRANSLIT") # Remover acentos
  return(nomes)
}

# Renomeando a coluna no DataFrame
colnames(dados_com_dummies)[colnames(dados_com_dummies) == "setor_economicopetroleo,_gas_e_biocombustiveis"] <- "setor_economicopetroleo_gas_e_biocombustiveis"
# Aplicar a função aos nomes das colunas do DataFrame
colnames(dados_com_dummies) <- ajustar_nomes_colunas(colnames(dados_com_dummies))

# Exibindo os novos nomes das colunas
colnames(dados_com_dummies)
#####Estimando o modelo
##### teste de hipóteses para variâncias dos termos de erro
# Ajustando o modelo de efeitos mistos utilizando as variáveis transformadas por Box-Cox
modelo_inc_st_setor <- lme(fixed = roa ~ liquidez_corrente + margem_ebitda +  cap_terc_patrimo_liquido+
                       setor_economicocomunicacoes + 
                       setor_economicoconsumo_ciclico + 
                       setor_economicoconsumo_nao_ciclico +
                       setor_economicomateriais_basicos + 
                       setor_economicomateriais_basicos +
                         setor_economicopetroleo_gas_e_biocombustiveis +
                       setor_economicosaude + 
                       setor_economicotecnologia_da_informacao +
                       setor_economicoutilidade_publica, 
                     random = ~ 1  | empresa_factor,
                     data = dados_com_dummies,
                     method = "REML")


# Resumo do modelo ajustado
summary(modelo_inc_st_setor )

stderr_nlme(modelo_inc_st_setor)

######Calculo LRT
# Extraindo a log-verossimilhança do modelo OLS
logLik_red <- logLik(modelo_nulo_st)

# Extraindo a log-verossimilhança do modelo de efeitos mistos
logLik_completo <- logLik(modelo_inc_st_setor)

# Calculando a estatística do LRT
LRT_stat <- -2 * (logLik_red - logLik_completo)

# Calculando o p-valor
df <- attr(logLik_completo, "df") - attr(logLik_red, "df")
p_value <- pchisq(LRT_stat, df = df, lower.tail = FALSE)

# Exibindo os resultados
cat("LRT Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

###########Remoção das var categóricas
####Estimando o modelo
##### teste de hipóteses para variâncias dos termos de erro
# Ajustando o modelo de efeitos mistos utilizando as variáveis transformadas por Box-Cox
modelo_inc_st_setor_comun <- lme(fixed = roa ~ liquidez_corrente + margem_ebitda +  cap_terc_patrimo_liquido + setor_economicocomunicacoes,
                              random = ~ 1 | empresa_factor,
                           data = dados_com_dummies,
                           method = "REML")


# Resumo do modelo ajustado
summary(modelo_inc_st_setor_comun )

stderr_nlme(modelo_inc_st_setor_comun)

# Extraindo a log-verossimilhança do modelo OLS
logLik_red <- logLik(modelo_nulo_st)

# Extraindo a log-verossimilhança do modelo de efeitos mistos
logLik_completo <- logLik(modelo_inc_st_setor_comun)

# Calculando a estatística do LRT
LRT_stat <- -2 * (logLik_red - logLik_completo)

# Calculando o p-valor
df <- attr(logLik_completo, "df") - attr(logLik_red, "df")
p_value <- pchisq(LRT_stat, df = df, lower.tail = FALSE)

# Exibindo os resultados
cat("LRT Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")


#####################################################################################################################
##########################     modelo inclinações aleatórias por setor transformação    #############################
#####################################################################################################################
##### teste de hipóteses para variâncias dos termos de erro
# Criando as interações no DataFrame com as variáveis transformadas por Box-Cox
dados_sem_outliers_tukey$liquidez_corrente_IPCA <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$IPCA
dados_sem_outliers_tukey$margem_ebitda_IPCA <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$IPCA
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_IPCA <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$IPCA

dados_sem_outliers_tukey$liquidez_corrente_SELIC <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$SELIC
dados_sem_outliers_tukey$margem_ebitda_SELIC <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$SELIC
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_SELIC <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$SELIC

dados_sem_outliers_tukey$liquidez_corrente_PIB <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$PIB
dados_sem_outliers_tukey$margem_ebitda_PIB <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$PIB
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_PIB <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$PIB

dados_sem_outliers_tukey$liquidez_corrente_importacoes <- dados_sem_outliers_tukey$liquidez_corrente * dados_sem_outliers_tukey$importacoes
dados_sem_outliers_tukey$margem_ebitda_importacoes <- dados_sem_outliers_tukey$margem_ebitda * dados_sem_outliers_tukey$importacoes
dados_sem_outliers_tukey$cap_terc_patrimo_liquido_importacoes <- dados_sem_outliers_tukey$cap_terc_patrimo_liquido * dados_sem_outliers_tukey$importacoes

# Adicionando a coluna setor_economicocomunicacoes ao DataFrame dados_sem_outliers_tukey
dados_sem_outliers_tukey <- merge(dados_sem_outliers_tukey, 
                                  dados_com_dummies[, c("cnpj_cia", "setor_economicocomunicacoes")], 
                                  by.x = "CNPJ_CIA", 
                                  by.y = "cnpj_cia", 
                                  all.x = TRUE)

# Ajustando o modelo de efeitos mistos utilizando as variáveis sem transformadas por Box-Cox
# Escalando as variáveis contínuas

'''modelo_inc_st <- lme(fixed = ROA ~ liquidez_corrente + margem_ebitda +  cap_terc_patrimo_liquido+
                        margem_ebitda_PIB + margem_ebitda_SELIC + margem_ebitda_IPCA + 
                        cap_terc_patrimo_liquido_importacoes + cap_terc_patrimo_liquido_PIB + cap_terc_patrimo_liquido_SELIC +cap_terc_patrimo_liquido_IPCA,
                       random = ~ 1  + margem_ebitda + cap_terc_patrimo_liquido  | empresa_factor,
                        data = dados_sem_outliers_tukey,
                        method = "REML",
                     control = lmeControl(opt = "optim"))


# Resumo do modelo ajustado
summary(modelo_inc_st )

stderr_nlme(modelo_inc_st)'''




#######removendo variáveis#################################################################
dados_sem_outliers_tukey <- distinct(dados_sem_outliers_tukey)
#estimação do modelo
modelo_inc_st <- lme(fixed = ROA ~ liquidez_corrente + margem_ebitda +
                       margem_ebitda_PIB, 
                       random = ~ 1  + margem_ebitda  | empresa_factor,
                     data = dados_sem_outliers_tukey,
                     method = "REML",
                     control = lmeControl(opt = "optim"))


# Resumo do modelo ajustado
summary(modelo_inc_st )


stderr_nlme(modelo_inc_st)

#####################LRT

# Extraindo a log-verossimilhança do modelo OLS
logLik_red <- logLik(modelo_nulo_st)

# Extraindo a log-verossimilhança do modelo de efeitos mistos
logLik_completo <- logLik(modelo_inc_st)

# Calculando a estatística do LRT
LRT_stat <- -2 * (logLik_red - logLik_completo)

# Calculando o p-valor
df <- attr(logLik_completo, "df") - attr(logLik_red, "df")
p_value <- pchisq(LRT_stat, df = df, lower.tail = FALSE)

# Exibindo os resultados
cat("LRT Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

###########teste de aderencia

modelo_inc_st_2 <- lmer(ROA ~ liquidez_corrente + margem_ebitda +  margem_ebitda_PIB + 
                           (1  + margem_ebitda | empresa_factor),
                         data = dados_sem_outliers_tukey)

# Resumo do modelo ajustado
summary(modelo_inc_st_2)



# Extraindo os resíduos do modelo
residuos <- residuals(modelo_inc_st_2)

# Padronizando os resíduos (opcional, mas recomendado)
residuos_padronizados <- (residuos - mean(residuos)) / sd(residuos)

# Aplicando o teste de Kolmogorov-Smirnov
ks_resultado <- ks.test(residuos_padronizados, "pnorm")

# Exibindo o resultado
print(ks_resultado)

# Criando o gráfico Q-Q
qqnorm(residuos_padronizados)
qqline(residuos_padronizados, col = "red", lwd = 2)

# Adicionando título ao gráfico
title("Q-Q Plot dos Resíduos Padronizados")

Plotando o histograma dos resíduos
hist(residuals(modelo_inc_st_2), main = "Histograma dos Resíduos", xlab = "Resíduos", breaks = 30)

Extraindo os resíduos de nível 1 (resíduos idiossincráticos)
residuos<-residuals(modelo_inc_st)

resultado_sf <- sf.test(residuos)

# Exibindo o resultado
print(resultado_sf)

########################################################################################################
#######################Modelos com transformação de box-cox                                  ########## 
######################################################################################################
# Leitura dos dados
dados_outliers <- read.csv("dados_filtrados_hlm2.csv")

# Função para remover outliers usando o método de Tukey
remover_outliers <- function(df, coluna) {
  Q1 <- quantile(df[[coluna]], 0.25)
  Q3 <- quantile(df[[coluna]], 0.75)
  IQR <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5* IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  outliers <- df[[coluna]] < limite_inferior | df[[coluna]] > limite_superior
  df_sem_outliers <- df[!outliers, ]
  
  cat("Número de outliers removidos em", coluna, ":", sum(outliers), "\n")
  return(df_sem_outliers)
}

# Removendo outliers em ROA, liquidez_corrente, margem_liquida e IPCA
dados_sem_outliers <- remover_outliers(dados_outliers, "ROA")
dados_sem_outliers <- remover_outliers(dados_sem_outliers, "liquidez_corrente")
dados_sem_outliers <- remover_outliers(dados_sem_outliers, "margem_liquida")
dados_sem_outliers <- remover_outliers(dados_sem_outliers, "margem_ebitda")
dados_sem_outliers <- remover_outliers(dados_sem_outliers, "cap_terc_patrimo_liquido")
num_cnpj_unicos <- length(unique(dados_sem_outliers$CNPJ_CIA))
cat("Número de CNPJ_CIA únicos:", num_cnpj_unicos, "\n")

# Aplicando a transformação quadrada em ROA
dados_sem_outliers$ROA_quadrado <- dados_sem_outliers$ROA^2

# Aplicando a transformação de Box-Cox em ROA_quadrado
min_roa <- min(dados_sem_outliers$ROA_quadrado, na.rm = TRUE)
constante_roa <- abs(min_roa) + 1
dados_sem_outliers$adjusted_ROA <- dados_sem_outliers$ROA_quadrado + constante_roa


lambda_otimo_roa <- boxcox(lm(adjusted_ROA ~ 1, data = dados_sem_outliers))$x[
  which.max(boxcox(lm(adjusted_ROA ~ 1, data = dados_sem_outliers))$y)
]

lambda_otimo_roa
lambda_otimo_roa<- 0.15
if (lambda_otimo_roa == 0) {
  dados_sem_outliers$boxcox_ROA <- log(dados_sem_outliers$adjusted_ROA)
} else {
  dados_sem_outliers$boxcox_ROA <- (dados_sem_outliers$adjusted_ROA ^ lambda_otimo_roa - 1) / lambda_otimo_roa
}

# Criando as interações no DataFrame com as variáveis transformadas por Box-Cox
dados_sem_outliers$liquidez_corrente_IPCA <- dados_sem_outliers$liquidez_corrente * dados_sem_outliers$IPCA
dados_sem_outliers$margem_ebitda_IPCA <- dados_sem_outliers$margem_ebitda * dados_sem_outliers$IPCA
dados_sem_outliers$cap_terc_patrimo_liquido_IPCA <- dados_sem_outliers$cap_terc_patrimo_liquido * dados_sem_outliers$IPCA

dados_sem_outliers$liquidez_corrente_SELIC <- dados_sem_outliers$liquidez_corrente * dados_sem_outliers$SELIC
dados_sem_outliers$margem_ebitda_SELIC <- dados_sem_outliers$margem_ebitda * dados_sem_outliers$SELIC
dados_sem_outliers$cap_terc_patrimo_liquido_SELIC <- dados_sem_outliers$cap_terc_patrimo_liquido * dados_sem_outliers$SELIC

dados_sem_outliers$liquidez_corrente_PIB <- dados_sem_outliers$liquidez_corrente * dados_sem_outliers$PIB
dados_sem_outliers$margem_ebitda_PIB <- dados_sem_outliers$margem_ebitda * dados_sem_outliers$PIB
dados_sem_outliers$cap_terc_patrimo_liquido_PIB <- dados_sem_outliers$cap_terc_patrimo_liquido * dados_sem_outliers$PIB

dados_sem_outliers$liquidez_corrente_importacoes <- dados_sem_outliers$liquidez_corrente * dados_sem_outliers$importacoes
dados_sem_outliers$margem_ebitda_importacoes <- dados_sem_outliers$margem_ebitda * dados_sem_outliers$importacoes
dados_sem_outliers$cap_terc_patrimo_liquido_importacoes <- dados_sem_outliers$cap_terc_patrimo_liquido * dados_sem_outliers$importacoes

# Ajustando o modelo de efeitos mistos utilizando as variáveis transformadas por Box-Cox
modelo_final_2 <- lmer(boxcox_ROA ~ liquidez_corrente + margem_ebitda + margem_ebitda_PIB +
                         (1 + margem_ebitda | empresa_factor),
                       data = dados_sem_outliers)

# Resumo do modelo ajustado
summary(modelo_final_2)

# Extraindo os resíduos do modelo
residuos <- residuals(modelo_final_2)

# Padronizando os resíduos (opcional, mas recomendado)
residuos_padronizados <- (residuos - mean(residuos)) / sd(residuos)

# Aplicando o teste de Kolmogorov-Smirnov
ks_resultado <- ks.test(residuos_padronizados, "pnorm")

# Exibindo o resultado do teste KS
print(ks_resultado)

# Criando o gráfico Q-Q
qqnorm(residuos_padronizados)
qqline(residuos_padronizados, col = "red", lwd = 2)

# Adicionando título ao gráfico
title("Q-Q Plot dos Resíduos Padronizados")

# Plotando o histograma dos resíduos
hist(residuals(modelo_final_2), main = "Histograma dos Resíduos", xlab = "Resíduos", breaks = 30)

# Ajustando o modelo de efeitos mistos utilizando as variáveis transformadas por Box-Cox com controle
modelo_final <- lme(fixed = boxcox_ROA ~ liquidez_corrente + margem_ebitda  + margem_ebitda_PIB , 
                    random = ~ 1 + margem_ebitda | empresa_factor,
                    data = dados_sem_outliers,
                    method = "REML",
                    control = lmeControl(opt = "optim"))

# Resumo do modelo ajustado
summary(modelo_final)

stderr_nlme(modelo_final)

# Extraindo os resíduos de nível 1 (resíduos idiossincráticos)
residuos <- residuals(modelo_final)

# Aplicando o teste de Shapiro-Francia
resultado_sf <- sf.test(residuos)

# Exibindo o resultado
print(resultado_sf)

########################
modelo_final_nulo <- lme(fixed = boxcox_ROA ~ 1 , 
                         random = ~ 1 | empresa_factor,
                         data = dados_sem_outliers,
                         method = "REML")


# Resumo do modelo ajustado
summary(modelo_final_nulo)

stderr_nlme(modelo_final_nulo)
#LRT comparado ao modelo nulo com transformação de Box-Cox


# Extraindo os resíduos de nível 1 (resíduos idiossincráticos)
residuos<-residuals(modelo_final_nulo)

resultado_sf <- sf.test(residuos)

# Exibindo o resultado
print(resultado_sf)



# Extraindo a log-verossimilhança do modelo HLM2_final_nulo
logLik_red <- logLik(modelo_final_nulo)

# Extraindo a log-verossimilhança do modelo de efeitos mistos
logLik_completo <- logLik(modelo_final)

# Calculando a estatística do LRT
LRT_stat <- -2 * (logLik_red - logLik_completo)

# Calculando o p-valor
df <- attr(logLik_completo, "df") - attr(logLik_red, "df")
p_value <- pchisq(LRT_stat, df = df, lower.tail = FALSE)

# Exibindo os resultados
cat("LRT Statistic:", LRT_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

####Verificanco a significancia da covariância do modelo final
modelo_completo <- lme(fixed = boxcox_ROA ~ liquidez_corrente + margem_ebitda + margem_ebitda_PIB, 
                       random = ~ 1 + margem_ebitda | empresa_factor,
                       data = dados_sem_outliers,
                       method = "REML",
                       control = lmeControl(opt = "optim"))

modelo_reduzido <- lme(fixed = boxcox_ROA ~ liquidez_corrente + margem_ebitda + margem_ebitda_PIB, 
                       random = list(empresa_factor = pdDiag(~ 1 + margem_ebitda)),
                       data = dados_sem_outliers,
                       method = "REML",
                       control = lmeControl(opt = "optim"))
anova(modelo_completo, modelo_reduzido)

##################gráfico fitted values x observed
# Extraindo os valores ajustados (fitted values) do modelo
fitted_values <- fitted(modelo_final)

# Extraindo os valores observados do ROA transformado por Box-Cox
observed_values <- dados_sem_outliers$boxcox_ROA

# Criando o gráfico
plot(observed_values, fitted_values, 
     xlab = "Valores Observados", 
     ylab = "Valores Ajustados", 
     main = "Gráfico de Valores Ajustados vs. Valores Observados",
     pch = 10, col = "black")

# Adicionando uma linha de identidade (y = x) para referência
abline(0, 1, col = "red", lwd = 2)


#####################Efeitos de interceptos aleatórios

# Extraindo os efeitos aleatórios do modelo
random_effects <- as.data.frame(random.effects(modelo_final))

# Nomeando as colunas para facilitar o manuseio
colnames(random_effects) <- c("v0j", "v1j")

# Gráfico dos efeitos aleatórios de intercepto (v0j)
plot_intercept1 <- random_effects %>%
  rownames_to_column("Empresa") %>%
  slice(1:50) %>%
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>%
  arrange(Empresa) %>%
  ggplot(aes(label = format(v0j, digits = 2), hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa", y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1", "green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_intercept2 <- random_effects %>%
  rownames_to_column("Empresa") %>%
  slice(51:100) %>%
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>%
  arrange(Empresa) %>%
  ggplot(aes(label = format(v0j, digits = 2), hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa", y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1", "green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_intercept3 <- random_effects %>%
  rownames_to_column("Empresa") %>%
  slice(101:150) %>%
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>%
  arrange(Empresa) %>%
  ggplot(aes(label = format(v0j, digits = 2), hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa", y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1", "green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_intercept4 <- random_effects %>%
  rownames_to_column("Empresa") %>%
  slice(151:171) %>%
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>%
  arrange(Empresa) %>%
  ggplot(aes(label = format(v0j, digits = 2), hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa", y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1", "green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Combinando os gráficos em uma única visualização
plot_dividido_intercepts <- plot_grid(plot_intercept1, plot_intercept2, plot_intercept3, plot_intercept4,
                                      ncol = 2, nrow = 2)

# Exibindo o gráfico
print(plot_dividido_intercepts)


# Histogramas das variáveis
ggplot(dados_sem_outliers, aes(x = margem_ebitda)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(dados_sem_outliers$margem_ebitda), color = "red", linetype = "dashed") +
  labs(title = "Histograma da Margem EBITDA", x = "Margem EBITDA", y = "Frequência")

ggplot(dados_sem_outliers, aes(x = liquidez_corrente)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(dados_sem_outliers$liquidez_corrente), color = "red", linetype = "dashed") +
  labs(title = "Histograma da Liquidez Corrente", x = "Liquidez Corrente", y = "Frequência")



######Comparação LogLik dos modelos:
# Criando um data frame com os log-likelihoods dos modelos
ll_data <- data.frame(
  Modelo = c("OLS_Nulo", "HLM2_Final_Nulo", "HLM2_Final"),
  LogLik = c(logLik(modelo_ols),
             logLik(modelo_final_nulo),
             logLik(modelo_final))
)

# Ajustando os nomes das variáveis, caso necessário
ll_data$Modelo <- factor(ll_data$Modelo, levels = c("OLS_Nulo", "HLM2_Final_Nulo", "HLM2_Final"))

# Plotando a comparação dos log-likelihoods
ggplot(ll_data, aes(x = Modelo, y = abs(LogLik), fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = round(LogLik, 3)), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Log-Likelihood (LL)", 
       y = "Log-Likelihood", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25", "grey45", "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())



