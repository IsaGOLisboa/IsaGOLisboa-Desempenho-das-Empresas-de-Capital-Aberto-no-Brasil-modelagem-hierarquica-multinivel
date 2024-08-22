# Séries e códigos disponíveis ipea
series_ipeadata <- ipeadatar::available_series()
series_ipeadata
# Filtrar séries com o termo "caged"
dplyr::filter(
  series_ipeadata,
  stringr::str_detect(source, stringr::regex("caged", ignore_case = TRUE))
)

##############Materiais e Métodos####################
#PAN_IPCAG - IPCA Anual (taxa total de variação (%a.a))
IPCA<- ipeadata("PAN_IPCAG",language=c("en","br"),quiet=FALSE)
IPCA<- IPCA[33:43, -c(4,5)]
IPCA$date <- as.Date(IPCA$date)
IPCA$year <- format(IPCA$date, "%Y")
media_por_ano <- IPCA %>%
  group_by(year) %>%
  summarise(media_value = mean(value, na.rm = TRUE))
media_ano<-pivot_wider(media_por_ano, names_from = year, values_from = media_value)
IPCA<- media_ano%>%
  mutate(DENOM_CIA = "IPCA (%a.a)") %>%
  select(DENOM_CIA, everything())


#GAC_PIBCAPR -  PIB per capta (R$ 2022)
PIB_per_capta_br <- ipeadata("GAC_PIBCAPR",language=c("en","br"),quiet=FALSE)
PIB_per_capta_br<- PIB_per_capta_br[113:123, -c(4,5)]
PIB_per_capta_br$date <- as.Date(PIB_per_capta_br$date)
PIB_per_capta_br$year <- format(PIB_per_capta_br$date, "%Y")
media_por_ano <- PIB_per_capta_br %>%
  group_by(year) %>%
  summarise(media_value = mean(value, na.rm = TRUE))
media_ano<-pivot_wider(media_por_ano, names_from = year, values_from = media_value)
PIB_per_capta_br<- media_ano%>%
  mutate(DENOM_CIA = "PIB_per_capta (R$ 2022)") %>%
  select(DENOM_CIA, everything())


#BM366_TJOVER366 - Taxa de juros - SELIC - fixada pelo Copom (%a.a)
SELIC<- ipeadata("BM366_TJOVER366",language=c("en","br"),quiet=FALSE)
SELIC <- SELIC[5663:9607, -c(4, 5)]
SELIC$date <- as.Date(SELIC$date)
SELIC$year <- format(SELIC$date, "%Y")
media_por_ano <- SELIC %>%
  group_by(year) %>%
  summarise(media_value = mean(value, na.rm = TRUE))
media_ano<-pivot_wider(media_por_ano, names_from = year, values_from = media_value)
SELIC<- media_ano%>%
  mutate(DENOM_CIA = "SELIC (%a.a)") %>%
  select(DENOM_CIA, everything())



#PAN_TJOVER - Taxa de juros nominal - over/SELIC (%a.a)
SELIC_nominal <- ipeadata("PAN_TJOVER",language=c("en","br"),quiet=FALSE)
SELIC_nominal <- SELIC_nominal[39:49, -c(4, 5)]
SELIC_nominal$date <- as.Date(SELIC_nominal$date)
SELIC_nominal$year <- format(SELIC_nominal$date, "%Y")
media_por_ano <- SELIC_nominal %>%
  group_by(year) %>%
  summarise(media_value = mean(value, na.rm = TRUE))
media_ano<-pivot_wider(media_por_ano, names_from = year, values_from = media_value)
SELIC_nominal<- media_ano%>%
  mutate(DENOM_CIA = "SELIC_over (%a.a)") %>%
  select(DENOM_CIA, everything())


#4536 - Dívida líquida do governo geral (%PIB)
divida_liq_pib <- GetBCBData::gbcbd_get_series(
  id = 	4536,
  first.date = "2012-01-01",
  last.date = "2022-12-31"
)
divida_liq_pib <-divida_liq_pib %>%
  subset(select = -c(3:4))

#realizando a soma dos dados de divida_liq_setor_pulico por ano

divida_liq_pib <- divida_liq_pib %>%
  mutate(ano = year(ref.date))

divida_liq_pib <- divida_liq_pib %>%
  group_by(ano) %>%
  summarise(soma_valor = mean(value, na.rm = TRUE))

divida_liq_pib <- pivot_wider(
  divida_liq_pib,
  names_from = ano,
  values_from = soma_valor
)
divida_liq_pib$DENOM_CIA <- 'divida_liquida_pib'
divida_liq_pib <- divida_liq_pib %>%
  select(DENOM_CIA, everything())

#coletando cambio dólar
dados_sgs_dolar <- GetBCBData::gbcbd_get_series(
  id = c("Dólar" = 3698, "IBC-Br" = 24363, "Resultado Primário" = 5793),
  first.date = "2012-01-01",
  last.date = "2022-12-31",
  format.data = "wide"
)
tail(dados_sgs_dolar)

cambio<- dados_sgs_dolar
cambio<- cambio%>%
  subset(select = -c(3:4))
cambio$value <- as.numeric(as.character(cambio$Dólar))

# Adicionando coluna de ano ao dataframe cambio
cambio <- cambio %>%
  mutate(ano = year(ref.date))

# Calculando a média dos valores 'value' agrupados por 'ano'
cambio <- cambio %>%
  group_by(ano) %>%
  summarise(dolar_medio = mean(value, na.rm = TRUE))

# Suponha que a coluna 'ano' seja um fator, converta-a para character
cambio$ano <- as.character(cambio$ano)

# Transpondo o dataframe soma_por_ano
cambio <- pivot_wider(
  cambio,
  names_from = ano,
  values_from = dolar_medio
)

cambio$DENOM_CIA <- 'cambio'
cambio <- cambio %>%
  select(DENOM_CIA, everything())

############################################################Verificar daqiui para baixo
dados_macroeconomicos<-bind_rows(IPCA, PIB_per_capta_br, cambio, divida_liq_pib, SELIC, SELIC_nominal)
write.csv(dados_macroeconomicos, file= 'dados_macroeconomicos.csv', row.names = FALSE)
