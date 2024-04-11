################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl",
             "gghalves","ggdist","tidyquant","car","nlme","lmtest",
             "fastDummies","msm","lmeInfo","jtools","gganimate","ggridges",
             "viridis","hrbrthemes","openxlsx", "stringr", "ggplot2", "cowplot", 
             "corrplot", "correlation", "olsrr", "nortest")

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

########################################################################################
#                                                                                      #
#              Modelo HLM2 para empresas listadas na IBOVESPA - Jan/24                 #
#                                                                                      #
########################################################################################
#carregando e realizando etl nos dados para utlização no modelo com variaveis X
dados<- read.csv('empresas_totais_indices.csv')
dados <- dados[!duplicated(dados[c("CNPJ_CIA", "ano")]), ]
dados <- select(dados, -lucro_liquido, -ativo_total)
#substituindo INF ou -INF por NA para remoção
dados <- as.data.frame(lapply(dados, function(x) ifelse(is.infinite(x), NA, x)))
# Removendo linhas com NA
dados <- na.omit(dados)
dados_filtrados <- dados %>%
  group_by(CNPJ_CIA) %>%
  filter(n() >= 2) %>%
  ungroup()


dados_macroeconomicos<- read.csv("dados_macroeconomicos.csv")
# Remover o 'X' adicionado antes dos nomes das colunas
nomes_colunas_corrigidos <- colnames(dados_macroeconomicos)
nomes_colunas_corrigidos <- gsub("^X", "", nomes_colunas_corrigidos)
colnames(dados_macroeconomicos) <- nomes_colunas_corrigidos

# Alongar o DataFrame
dados_macroeconomicos_long <- t(dados_macroeconomicos) 
# Definir os nomes das colunas como NULL
dados_macroeconomicos_long<- as.data.frame(dados_macroeconomicos_long)
print(colnames(dados_macroeconomicos_long))
# Extrair a segunda linha como nomes de coluna
nomes_colunas <- as.character(dados_macroeconomicos_long[1, ])

# Excluir a primeira e segunda linhas do DataFrame
dados_macroeconomicos_long <- dados_macroeconomicos_long[-1, ]

# Redefinir os nomes das colunas
colnames(dados_macroeconomicos_long) <- nomes_colunas


# Transformar nomes das linhas em coluna
dados_macroeconomicos_long <- rownames_to_column(dados_macroeconomicos_long, var = "ano")
# Convertendo a coluna "ano" para inteiro
dados_macroeconomicos_long <- mutate(dados_macroeconomicos_long, ano = as.integer(ano))

# Realizando a junção
dados_filtrados <- left_join(dados_filtrados, dados_macroeconomicos_long, by = "ano")
print(colnames(dados_filtrados))
dados_filtrados <- dados_filtrados %>%
  rename(IPCA = `IPCA (%a.a)`,
         PIB_per_capta = `PIB_per_capta (R$ 2022)`,
         SELIC = `SELIC (%a.a)`)

# Convertendo CNPJ_CIA em fatores
dados_filtrados$empresa_factor <- as.factor(dados_filtrados$CNPJ_CIA)
# Convertendo os fatores em números inteiros
dados_filtrados$empresa_factor <- as.integer(dados_filtrados$empresa_factor)

dados_filtrados$IPCA <- as.numeric(dados_filtrados$IPCA)
dados_filtrados$PIB_per_capta <- as.numeric(dados_filtrados$PIB_per_capta)
dados_filtrados$cambio <- as.numeric(dados_filtrados$cambio)
dados_filtrados$divida_liquida_pib <- as.numeric(dados_filtrados$divida_liquida_pib)
dados_filtrados$SELIC <- as.numeric(dados_filtrados$SELIC)
dados_filtrados$ano<-as.numeric((dados_filtrados$ano))

balancos_comerciais<-read.csv("balancos_comerciais.csv")
# Renomear a coluna "ano" para "data"
#balancos_comerciais <- balancos_comerciais %>%
  #rename(ano = data)

# Criar uma nova coluna "ano" de 1 a 11
#balancos_comerciais$ano <- seq(1, 11)
#balancos_comerciais_ano <- balancos_comerciais %>%
 # select(-data)

dados_filtrados <- merge(dados_filtrados, balancos_comerciais, by = "ano", all.x = TRUE)
dados_filtrados <- dados_filtrados %>%
  mutate(balanca_comercial = exportacoes - importacoes)

PIB <- data.frame(
  ano = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  PIB = c(4.814760, 5.331619, 5.778953, 5.995787, 6.269328, 6.585479, 7.004141, 7.389131, 7.609597, 8.898727, 9.915317)
)
dados_filtrados <- merge(dados_filtrados, PIB, by = "ano", all.x = TRUE)

dados_filtrados$empresa_factor<-as.factor(dados_filtrados$empresa_factor)
summary(dados_filtrados)
#Verificando empresas por ano
dados_filtrados %>%
  summarise(n_uniq_denom_cia = n_distinct(DENOM_CIA)) %>%
  arrange(desc(n_uniq_denom_cia))
################################################################################
#            ESTIMAÇÃO DO MODELO nulo HLM2               #
################################################################################
modelo_nulo_hlm2<- lme(fixed = ROA ~ 1,
                       random = ~ 1 | empresa_factor,
                       data = dados_filtrados,
                       method = "REML")

#parametros do modelo nulo
summary(modelo_nulo_hlm2)

#verificando da funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)


# o p-value do voj foi estatisticamente significante (>0,05), o que caracteriza a utilização do modelo hierraquico, porque, estatisticamente este coeficiente é diferente de 0.

ICC_empresas<- 36.02839/(36.02839 + 87.13774)
ICC_empresas
ICC_tempo<- 1-ICC_empresas
ICC_tempo
################################################################################
#comparação com OLS
#Comparando com o OLS (nulo)
modelo_ols_nulo <- lm(formula = ROA ~ 1,
                      data = dados_filtrados)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())




################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################
# Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = ROA ~  exportacoes ,
                             random = ~ 1 | empresa_factor,
                             data = dados_filtrados,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)



#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Verificando a correlação entre importações e exportações com objetivo de investigar multicolinaridade
cor(dados_filtrados$importacoes, dados_filtrados$exportacoes)
dados_selecionados<-dados_filtrados%>%
  select(4, 16, 17)
dados_selecionados%>%
  correlation(method="pearson")%>%
  plot()

modelo_lm_correlacoes <- lm(formula = ROA ~ exportacoes + importacoes,
              data = dados_selecionados)

summary(modelo_lm_correlacoes)

#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo_lm_correlacoes)




################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = ROA ~  exportacoes,
                                    random = ~ importacoes  | empresa_factor,
                                    data = dados_filtrados,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Chart correlation da variáveis
chart.Correlation((dados_filtrados[4:8]), histogram = TRUE)
################################################################################
#                       ESTIMAÇÃO DO MODELO FINAL HLM2                         #
################################################################################

#Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = ROA ~ exportacoes + liquidez_corrente + margem_liquida  + margem_ebitda + 
                           liquidez_corrente:exportacoes +  margem_liquida:exportacoes + margem_ebitda:exportacoes ,
                         random = ~ importacoes | empresa_factor,
                         data = dados_filtrados,
                         method = "REML")

#Parâmetros do modelo
summary(modelo_final_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

summary(dados_filtrados)
table(dados_filtrados$empresa_factor)
#Melhor visualização dos interceptos e das inclinações aleatórios,
#para o modelo final HLM2

v_final <- data.frame(modelo_final_hlm2[["coefficients"]][["random"]][["empresa_factor"]]) %>%
  rename(v00 = 1,
         v10 = 2)
v_final$empresa_factor <- c(1:194)
v_final$empresa_factor <- as.factor(v_final$empresa_factor)

v_final %>% 
  select(empresa_factor, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0j, ou seja,
#dos interceptos aleatórios por empresa
random.effects(modelo_final_hlm2) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


random_effects<- random.effects(modelo_final_hlm2)
random_effects

# Gráfico dos efeitos aleatórios por empresa (dividos em 4 gráficos)
plot_original1 <- slice(random_effects, 1:50) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


plot_original2 <- slice(random_effects, 51:100) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_original3 <- slice(random_effects, 101:150) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_original4 <- slice(random_effects, 151:194) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Dividindo o gráfico em quatro partes
plot_dividido <- plot_grid(plot_original1, plot_original2, plot_original3, plot_original4,
                           ncol = 2, nrow = 2)

# Exibir o gráfico dividido
plot_dividido


### Intra Class correlation para o nível empresa: de fato o impacto do nível empresas no desempenho ROA
ICC_empresas<- (1.636609e+02+2.487233e-09)/(1.636609e+02+2.487233e-09+6.143889e+01)
ICC_empresas
1-ICC_empresas

#Intervalos de confianças para modelos multiníveis
intervals(modelo_final_hlm2)

# Encontrar o menor valor dos efeitos de itercepto
menor_valor <- min(random_effects[, 1])
indice_menor <- which.min(random_effects[, 1])

# Encontrar o maior valor dos efeitos de itercepto
maior_valor <- max(random_effects[, 1])
indice_maior <- which.max(random_effects[, 1])

# Imprimir os resultados
print(paste("Menor intercepto:", menor_valor))
print(paste("Empresa:", indice_menor))
print(paste("Maior valor:", maior_valor))
print(paste("Empresa:", indice_maior))



#####efeitos aleatórios de inclinações

#Para observarmos graficamente o comportamento dos valores de v1j, ou seja
#das inclinações aleatórias 
random.effects(modelo_final_hlm2)%>%
  rename(v1j = 2) %>% 
  rownames_to_column("Empresa") %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Gráfico dos efeitos aleatórios e inclinação por empresa (dividos em 4 gráficos)
plot_original_incl1 <- random.effects(modelo_final_hlm2)%>%
  rename(v1j = 2) %>% 
  rownames_to_column("Empresa") %>% 
  slice(1:50) %>%
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


plot_original_incl2 <- random.effects(modelo_final_hlm2)%>%
  rename(v1j = 2) %>% 
  rownames_to_column("Empresa") %>% 
  slice(51:100) %>%
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_original_incl3 <- random.effects(modelo_final_hlm2)%>%
  rename(v1j = 2) %>% 
  rownames_to_column("Empresa") %>% 
  slice(101:150) %>%
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

plot_original_incl4 <- random.effects(modelo_final_hlm2)%>%
  rename(v1j = 2) %>% 
  rownames_to_column("Empresa") %>% 
  slice(151:194) %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Empresa) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Empresa), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Empresa, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Empresa",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


# Dividindo o gráfico em quatro partes
plot_dividido_incl <- plot_grid(plot_original_incl1, plot_original_incl2, plot_original_incl3, plot_original_incl4,
                           ncol = 2, nrow = 2)

# Exibir o gráfico dividido
plot_dividido_incl


# Encontrar o menor valor dos efeitos de inclinação
random_effects_incl <- random.effects(modelo_final_hlm2)[[2]]
menor_valor_incl <- min(random_effects_incl)
indice_menor_incl <- which.min(random_effects_incl)

# Encontrar o maior valor dos efeitos de itercepto
maior_valor_incl <- max(random_effects_incl)
indice_maior_incl <- which.max(random_effects_incl)

# Imprimir os resultados
print(paste("Menor inclinação:", menor_valor_incl))
print(paste("Empresa:", indice_menor_incl))
print(paste("Maior inclinação:", maior_valor_incl))
print(paste("Empresa:", indice_maior_incl))


#Gerando os fitted values do modelo HLM2 Final
dados_filtrados$hlm2_fitted <- predict(modelo_final_hlm2,
                                        dados_filtrados)

# Visualizando os fitted values do modelo
predict_modelo_hlm2<-predict(modelo_final_hlm2, level = 0:1) %>% 
  mutate(desempenho = dados_filtrados$ROA,
         etjk = resid(modelo_final_hlm2)) %>% #função resid gera os termos etjk
  select(empresa_factor, desempenho, everything()) %>%
  arrange(empresa_factor) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

write.xlsx(predict_modelo_hlm2,"C:/Users/User/arq_tcc/RStudio/hlm2_tcc_final/predict_moldelo_hlm2_tc_final.xlsx") 

# Supondo 'modelo_hlm' como seu modelo ajustado
residuos <- residuals(modelo_hlm)
ggplot(data.frame(Residuos = residuos), aes(x = Residuos)) +
  geom_histogram(binwidth = .5, fill = "blue", color = "black") +
  labs(title = "Distribuição dos Resíduos", x = "Resíduos", y = "Frequência")

#Valores previstos do desempenho das empresas em função da variável exportaçoes para o 
#modelo final HLM2 com interceptos e inclinações aleatórios
dados_filtrados %>%
  mutate(fitted_empresa = predict(modelo_final_hlm2, level = 1)) %>% 
  ggplot() +
  geom_point(aes(x = exportacoes, y = fitted_empresa)) +
  geom_smooth(aes(x = exportacoes, y = fitted_empresa, color = factor(empresa_factor)), 
              method = "lm", se = F) +
  scale_colour_viridis_d() +
  labs(x = "Exportaçoes",
       y = "Desempenho da empresa - ROA (Fitted Values)") +
  theme_bw()

# Convertendo empresa_factor em numérico
dados_filtrados <- dados_filtrados %>%
  mutate(empresa_factor = as.numeric(as.character(empresa_factor)))

# Filtrar as observações onde empresa_factor está no intervalo de 1 a 30
dados_filtrados_30 <- dados_filtrados %>%
  filter(empresa_factor >= 1 & empresa_factor <= 10)

# Calcular as previsões apenas para essas observações
fitted_values_30 <- predict(modelo_final_hlm2, level = 1, newdata = dados_filtrados_30)

# Criar um novo conjunto de dados com as previsões
dados_plot_30 <- dados_filtrados_30 %>%
  mutate(fitted_empresa = fitted_values_30) 

# Criar o gráfico
dados_plot_30 %>%
  ggplot() +
  geom_point(aes(x = exportacoes, y = fitted_empresa)) +
  geom_smooth(aes(x = exportacoes, y = fitted_empresa, color = factor(empresa_factor)), 
              method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  labs(x = "Exportações",
       y = "Desempenho da empresa - ROA (Fitted Values)") +
  theme_bw()

# Criar o gráfico
dados_plot_30 %>%
  ggplot() +
  geom_point(aes(x = exportacoes, y = fitted_empresa)) +
  geom_smooth(aes(x = exportacoes, y = fitted_empresa), 
              method = "lm", se = FALSE) +
  scale_colour_viridis_d() +
  labs(x = "Exportações",
       y = "Desempenho da empresa - ROA (Fitted Values)") +
  theme_bw()
#Gráfico fitted values X desempenho
valores_ajustados <- fitted(modelo_final_hlm2)
valores_observados <- dados_filtrados$ROA
dados_grafico <- data.frame(ROA = valores_observados, ValoresAjustados = valores_ajustados)

ggplot(dados_grafico, aes(x = ROA, y = ValoresAjustados)) +
  geom_point() +  # Adiciona pontos ao gráfico
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linha de tendência
  theme_minimal() +  # Tema minimalista
  labs(x = "ROA", y = "Valores Ajustados", 
       title = "Gráfico de Valores Ajustados pelo ROA")

write.xlsx(dados_filtrados, "C:/Users/User/arq_tcc/RStudio/hlm2_tcc_final/dados_totais_tcc_final_hlm2.xlsx")

#grafico dos resíduos
residuos <- residuals(modelo_final_hlm2)
ggplot(data.frame(Residuos = residuos), aes(x = Residuos)) +
  geom_histogram(binwidth = .5, fill = "blue", color = "black") +
  labs(title = "Distribuição dos Resíduos", x = "Resíduos", y = "Frequência")

#Comparando os LLs
#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_nulo_hlm2, modelo_intercept_inclin_hlm2, modelo_final_hlm2)
