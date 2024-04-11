# Carregando e instalando os pacotes

pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra",
             "reshape2",
             "misc3d",
             "plot3D", 
             "cluster", 
             "factoextra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando o dataset

empresas <- read.csv("dados_com_setor.csv")


# Visualização

# Normaliza a quarta variável (ROA) para o intervalo de 1 a 100
norm_CTPT <- round(((empresas$CTPT - min(empresas$CTPT)) / diff(range(empresas$CTPT))) * 99) + 1

# Plota o gráfico 3D usando scatterplot3d, com a cor dos pontos representando ROA
scatterplot3d(x = empresas$liquidez_corrente,
              y = empresas$margem_ebitda,
              z = empresas$margem_liquida,
              color = colors[norm_CTPT],
              pch = 20, cex = 1,
              xlab = "Liquidez Corrente",
              ylab = "Margem EBITDA",
              zlab = "Margem Líquida",
              main = "Análise 3D com CTPT como Cor")

empresas <- select(empresas, -setor_economico)


empresas_sem_outliers <- empresas %>%
  # Calcula o primeiro e terceiro quartis de 'valor'
  mutate(
    Q1 = quantile(ROA, 0.25, na.rm = TRUE),
    Q3 = quantile(ROA, 0.75, na.rm = TRUE)
  ) %>%
  # Calcula o IQR (intervalo interquartil)
  mutate(IQR = Q3 - Q1) %>%
  # Filtra os dados para remover outliers
  filter(ROA >= (Q1 - 1.5 * IQR) & ROA <= (Q3 + 1.5 * IQR)) %>%
  # Remove as colunas auxiliares adicionadas para o cálculo
  select(-c(Q1, Q3, IQR))

# Removendo as duas primeiras colunas
empresas_para_kmeans <- empresas_sem_outliers[ , -(1:2)]
# Método de Elbow para identificação do número ótimo de clusters
dev.off()
fviz_nbclust(empresas_para_kmeans, kmeans, method = "wss", k.max = 9)


## Podemos concluir que 4 clusters é uma opção viável

# Elaboração da clusterização não hieráquica k-means

cluster_kmeans <- kmeans(empresas_para_kmeans,
                         centers = 4)

# Adicionando a variável

empresas_para_kmeans$cluster_K <- factor(cluster_kmeans$cluster)
empresas_sem_outliers$cluster_K <- factor(cluster_kmeans$cluster)


# Analisando por meio de estatísticas descritivas

análise <- group_by(empresas_sem_outliers, cluster_K) %>%
  summarise(limite = mean(ROA, na.rm = TRUE),
            liquidez_corrente = mean(liquidez_corrente, na.rm = TRUE),
            margem_ebitda = mean(margem_ebitda, na.rm = TRUE),
            margem_liquida = mean(margem_liquida, na.rm = TRUE),
            CTPT = mean(cap_terc_patrimo_liquido, na.rm = TRUE))

# Anovas

summary(anova_limite <- aov(formula = liquidez_corrente ~ cluster_K,
                            data = empresas_sem_outliers))

summary(anova_cartoes <- aov(formula = ROA ~ cluster_K,
                             data = empresas_sem_outliers))

summary(anova_visitas <- aov(formula = cap_terc_patrimo_liquido ~ cluster_K,
                             data = empresas_sem_outliers))

summary(anova_online <- aov(formula = Total_visits_online ~ cluster_K,
                            data = dados_padronizado))

summary(anova_liga <- aov(formula = margem_liquida ~ cluster_K,
                          data = empresas_sem_outliers))

summary(anova_liga <- aov(formula = margem_ebitda ~ cluster_K,
                          data = empresas_sem_outliers))


dados_setores<- read.csv("dados_com_setor.csv")
empresas_com_setor_cluster <- merge(empresas_sem_outliers, dados_setores[, c("CNPJ_CIA", "setor_economico")], by = "CNPJ_CIA", all.x = TRUE)

## Todas as variáveis são relevantes na criação de pelo menos um cluster

# Fim!