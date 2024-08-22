################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl",
             "gghalves","ggdist","tidyquant","car","nlme","lmtest",
             "fastDummies","msm","lmeInfo","jtools","gganimate","ggridges",
             "viridis","hrbrthemes","openxlsx", "stringr", "ggplot2", "cowplot")

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
ROA<- read.csv('ROA_corrigido.csv')
ROA <- subset(ROA, DENOM_CIA != "PETROBRAS DISTRIBUIDORA S/A")
dados_setor_economico<-read.csv('dados_com_setor_economico.csv')

# Realizando o merge/join para adicionar os setores aos respectivos cnpjs
ROA_com_setor <- merge(ROA, dados_setor_economico[, c("CNPJ_CIA", "setor_economico")], by = "CNPJ_CIA", all.x = TRUE)
ROA_com_setor <- ROA_com_setor[!duplicated(ROA_com_setor[c("CNPJ_CIA", "ano")]), ]

#Criando coluna para setor_economico fatorado
ROA_com_setor$setor_econ_fator <- as.integer(factor(ROA_com_setor$setor_economico))

#Criando coluna para CNPJ_CIA fatorado
ROA_com_setor$empresa_fator <- as.integer(factor(ROA_com_setor$CNPJ_CIA))

#Criando coluna para ano fatorado
ROA_com_setor$ano_fator <- as.integer(factor(ROA_com_setor$ano))



#removendo os valores NAN de ROA
ROA_com_setor <- ROA_com_setor[!is.na(ROA_com_setor$ROA), ]


#Visualização dos dados
#Visualização da base de dados
ROA_com_setor %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas e tabelas de frequências
summary(ROA_com_setor)

#removendo o valor de ROA = INF
ROA_com_setor <- ROA_com_setor[ROA_com_setor$ativo_total != 0, ]

#removendo o valor de setor_economico = NA
ROA_com_setor <- ROA_com_setor[!is.na(ROA_com_setor$setor_economico), ]

#Estatísticas descritivas e tabelas de frequências
summary(ROA_com_setor)

#as colunas fatoradas devem ser factor, não numericas para evitar erros no modelo
ROA_com_setor$setor_econ_fator <- as.factor(ROA_com_setor$setor_econ_fator)
ROA_com_setor$empresa_fator <- as.factor(ROA_com_setor$empresa_fator)



#Gráfico com distribuições da variável 'desempenho' para os meses
#função 'geom_density_ridges_gradient' do pacote 'ggridges'
ggplot(ROA_com_setor, aes(x = ROA, y = as.factor(ano), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "ROA", option = "cividis") +
  labs(
    title = "Distribuições da variável 'ROA' para os anos",
    x = "ROA",
    y = "Ano") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 5)
  )

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ROA) por ano
ggplot(ROA_com_setor, aes(x = ROA)) +
  geom_density(aes(color = ano_fator, fill = ano_fator), 
               position = "identity", alpha = 0.2) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_classic()



#Exploração visual do ROA médio
ROA_com_setor %>%
  group_by(ano) %>%
  mutate(ROA_medio = mean(ROA, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = ano, y = ROA),color = "orange", alpha = 0.5, size = 4) +
  geom_line(aes(x = ano, y = ROA_medio, 
                group = 1, color = "ROA Médio"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Ano",
       y = "ROA") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Boxplot da variável dependente (desempenho)

ggplot(ROA_com_setor, aes(x = "", y = ROA)) +
  geom_boxplot(fill = "deepskyblue",    # cor da caixa
               alpha = 0.7,             # transparência
               color = "black",         # cor da borda
               outlier.colour = "red",  # cor dos outliers
               outlier.shape = 15,      # formato dos marcadores dos outliers
               outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
  geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
  labs(y = "ROA") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position="none",
        plot.title = element_text(size=15)) +
  ggtitle("Boxplot da variável 'desempenho'") +
  xlab("")


#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (ROA), com histograma
ggplot(ROA_com_setor, aes(x = ROA)) +
  geom_density(aes(x = ROA), 
               position = "identity", color = "black", size = 1) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                 bins = 30) +
  theme_classic()


#Boxplot da variável dependente (ROA) por empresa
ggplot(ROA_com_setor, aes(x = empresa_fator,y = ROA)) +
  geom_boxplot(aes(fill = empresa_fator, alpha = 0.7)) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
  scale_fill_viridis_d() +
  labs(y = "ROA") +
  theme_classic() +
  ggtitle("Boxplots da variável 'ROA' para as empresa")



ROA_com_setor_filtrado <- subset(ROA_com_setor, ROA >= -100 & ROA <= 100)
#Gráfico de desempenho x horas (OLS)
ROA_com_setor_filtrado %>%
  ggplot(aes(x = empresa_fator, y = ROA)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point() +
  scale_colour_viridis_d() +
  labs(x = "Período avaliado",
       y = "ROA") +
  theme_bw()



#Gráfico de desempenho x horas (OLS) por escola separadamente
#(funções transition_states e animate do pacote gganimate)
ggplot(ROA_com_setor_filtrado, aes(x=ano, y=ROA, color=empresa_fator)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  transition_states(empresa_fator, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Anos",
       y = "ROA") +
  scale_color_viridis_d() +
  ggtitle("ROA no período de 2012 a 2022", subtitle = "Empresa: {closest_state}") +
  theme_minimal() -> p

animate(p, nframes = 400, fps = 100)

#Gráfico de desempenho x horas por escola (visualização do contexto)
#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO NOS DADOS!
ROA_com_setor_filtrado %>%
  ggplot(aes(x = ano, y = ROA, color = empresa_fator)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point() +
  guides(color = "none") +
  scale_colour_viridis_d() +
  labs(x = "Período avaliado",
       y = "ROA") +
  theme_bw()
#Verificando setores
ROA_com_setor %>%
  group_by(setor_economico) %>%
  summarise(n_uniq_denom_cia = n_distinct(DENOM_CIA)) %>%
  arrange(desc(n_uniq_denom_cia))
#Verificando empresas
ROA_com_setor %>%
  summarise(n_uniq_denom_cia = n_distinct(DENOM_CIA)) %>%
  arrange(desc(n_uniq_denom_cia))

################################################################################
#            ESTIMAÇÃO DO MODELO nulo HLM3   _ setor/emperas/tempo             #
################################################################################
modelo_nulo_hlm3<- lme(fixed = ROA ~ 1,
                       random = list(setor_econ_fator = ~1, empresa_fator = ~1),
                       data = ROA_com_setor,
                       method = "REML")

#parametros do modelo nulo
summary(modelo_nulo_hlm3)

#verificando da funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3)


