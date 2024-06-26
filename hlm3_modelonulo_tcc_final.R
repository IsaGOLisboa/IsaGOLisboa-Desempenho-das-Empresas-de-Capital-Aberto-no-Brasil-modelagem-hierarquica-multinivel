################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados:
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","gganimate",
             "ggridges","viridis","hrbrthemes", "corrplot")

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
#########################################################################################
#carregando os dfs para utilização no modelo nulo
ROA_b3<- read.csv("ROA_corrigido.csv")
empresas_ibov<- read.csv(("empresas_ibov.csv"))
ROA_edgar<-read.csv("dados_edgar_conc.csv")
ROA_edgar <- ROA_edgar[ROA_edgar[, 16] != 1, ]
ROA_edgar <- ROA_edgar[, c(1, 2, 3, 4, 9, 16, 17)]
empresas_sp500 <- read_csv("empresas_S&P500.csv")
ROA_canada <- read.csv("ROA_Canada.csv")

#formatando ROA_b3 para junção dos dfs
ROA_b3 <- ROA_b3[, -c(4, 5)]
ROA_b3 <- na.omit(ROA_b3)
ROA_b3$Pais_k <- 1
ROA_b3$Ano_t <- as.integer(factor(ROA_b3$ano))
ROA_b3 <- ROA_b3 %>%
  filter(CNPJ_CIA %in% empresas_ibov$CNPJ)
ROA_b3$Empresa <- as.integer(factor(ROA_b3$CNPJ_CIA))
ROA_b3 <- ROA_b3[, -2]

#formatando ROA_canada para junção dos dfs
ROA_canada <- ROA_canada[, -c(3, 4)]
ROA_canada <- na.omit(ROA_canada)
ROA_canada$Empresa <- as.integer(factor(ROA_canada$DENOM_CIA))
ROA_canada$Ano_t <- as.integer(factor(ROA_canada$ano))
ROA_canada <- ROA_canada %>% rename(Pais_k = pais)

#formatando ROA_edgar para junção dos dfs
ROA_edgar <- ROA_edgar %>%
  filter(CNPJ_CIA %in% empresas_sp500$CIK)
ROA_edgar <- ROA_edgar[ , -c(2,5)]
ROA_edgar$Empresa <- as.integer(factor(ROA_edgar$DENOM_CIA))
ROA_edgar <- ROA_edgar  %>% rename(ano = Ano)

#empilhando os dfs
empresas_paises_3niveis <- rbind(ROA_b3, ROA_edgar, ROA_canada)
empresas_paises_3niveis <- empresas_paises_3niveis [, -6]
empresas_paises_3niveis$Empresa <- as.integer(factor(empresas_paises_3niveis$DENOM_CIA))
empresas_paises_3niveis <- empresas_paises_3niveis %>%
  group_by(DENOM_CIA, ano) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  # Filtrar para manter apenas entradas com uma única DENOM_CIA por Ano
  filter(count == 1) %>%
  # Opcionalmente, remover a coluna count se não for mais necessária
  select(-count)

#Verificando empresas por país
empresas_paises_3niveis %>%
  group_by(Pais_k) %>%
  summarise(n_uniq_denom_cia = n_distinct(DENOM_CIA)) %>%
  arrange(desc(n_uniq_denom_cia))

#Verificando se a mesma empresa esta em mais de 1 país
resultados <- empresas_paises_3niveis %>%
  group_by(DENOM_CIA) %>%
  summarise(N_Paises = n_distinct(Pais_k)) %>%
  filter(N_Paises > 1)

# Verificando os resultados
print(resultados)  # tibble vazia significa que não há classificação cruzada

# Agrupar os dados por ano e Pais_k, e calcular a média de ROA para cada grupo
media_roa_paises <- empresas_paises_3niveis %>%
  group_by(ano, Pais_k) %>%
  summarise(ROA_medio = mean(ROA, na.rm = TRUE)) %>%
  ungroup()  # Remover o agrupamento para facilitar manipulações futuras

# Visualizar o novo dataframe
print(media_roa_paises)
write_excel_csv(media_roa_paises, "C:/Users/User/arq_tcc/RStudio/hlm3_tcc_final/media_roa_paises.xlsx")
write.csv(empresas_paises_3niveis, "C:/Users/User/arq_tcc/RStudio/hlm3_tcc_final/empresas_paises_3niveis.xlsx")

########################################################################################
modelo_nulo_hlm3 <- lme(fixed = ROA ~ 1,
                        random = list(Pais_k = ~1, Empresa = ~1),
                        data = empresas_paises_3niveis,
                        method = "REML") # restricted estimation of maximum likelihood (Gelman) deixar computacionalmente menos demandante

#Parâmetros do modelo
summary(modelo_nulo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3)


######################################################################################
#Para apenas b3 e edgar (comparação tcc)
empresas_paises_3niveis_b3_edgar<- empresas_paises_3niveis[empresas_paises_3niveis[, 4] != 3, ]
empresas_paises_3niveis_b3_edgar <- empresas_paises_3niveis_b3_edgar [, -6]
empresas_paises_3niveis_b3_edgar$Empresa <- as.integer(factor(empresas_paises_3niveis_b3_edgar$DENOM_CIA))

########################################################################################
modelo_nulo_hlm3_b3_edgar <- lme(fixed = ROA ~ 1,
                        random = list(Pais_k = ~1, Empresa = ~1),
                        data = empresas_paises_3niveis_b3_edgar,
                        method = "REML") # restricted estimation of maximum likelihood (Gelman) deixar computacionalmente menos demandante

#Parâmetros do modelo
summary(modelo_nulo_hlm3_b3_edgar)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3_b3_edgar)

