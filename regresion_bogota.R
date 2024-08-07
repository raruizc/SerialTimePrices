pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
library("readxl")

####### Regresión Lineal

bogota_pib <- read_excel("bogota_pib.xlsx")

modelo_bogota <- lm(formula = Precio ~ PIB,
                       data = bogota_pib)

summary(modelo_bogota)

ggplotly(
  ggplot(modelo_bogota, aes(x = PIB, y = Precio)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "PIB-Bogotá (Miles de Millones COP)",
         y = "Precio (COP)") +
    scale_color_manual("Leyenda:",
                       values = "grey50") +
    theme_bw()
)

############################### Serie de Tiempo
pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

historico_bogota <- read_excel("historico_bogota.xlsx")
historico_bogota <- historico_bogota %>% mutate(Data = as.Date(Data))
head(historico_bogota)
plot(historico_bogota)
#Transformando a base de dados em um objeto de classe ts
precios_ts <- ts(data = historico_bogota[, 2],
             start = c(2015, 1),
             end = c(2021, 12),
             frequency = 12)

plot(precios_ts)
#Fazendo a plotagem da série temporal
historico_bogota %>% ggplot() +
  geom_line(aes(x = Data, y = Precio, group = TRUE, color = "Precio"), size = 1) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")


# decompondo o PIB pelo modelo aditivo

decpib <- decompose(x = precios_ts,
                    type = "additive")



#Transformando o objeto decpib num data frame
decpib_df <- data.frame(tempo = historico_bogota$Data,
                        serie = unlist(decpib$x),
                        tendencia = unlist(decpib$trend),
                        sazonalidade = unlist(decpib$seasonal),
                        dessazonalizada = precios_ts - decpib$seasonal,
                        erro = unlist(decpib$random)) %>%
  rename(tempo = 1,
         serie = 2,
         tendencia = 3,
         sazonalidade = 4,
         dessazonalizada = 5,
         erro = 6)

#Plotando a decomposição de forma conjunta
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Serie"), size = 1.2) +
  geom_line(aes(x = tempo, y = tendencia, color = "Tendencia"), size = 1) +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Estacionlidad"), size = 1.2) +
  geom_line(aes(x = tempo, y = erro, color = "Error"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legenda:",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#440154FF", "#3CBB75FF", "#39568CFF", "#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random

#Plotando a decomposição individualmente

#a) Série
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Serie")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Serie",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#39568CFF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_serie

#b) Sazonalidade
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Estacionalidad")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Estacionalidad",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#3CBB75FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_sazonalidade

#c) Tendência
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = tendencia, color = "Tendencia")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tendencia",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_tendencia

#d) Erro
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = erro, color = "Error")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Error",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#440154FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_erro

#Dê zoom para uma visualização mais agradável
grid.arrange(decomp_serie,
             decomp_sazonalidade,
             decomp_tendencia,
             decomp_erro,
             ncol = 2)

grid.arrange(decomp_serie,
             decomp_tendencia,
             ncol = 2)

#Plotando a série dessazonalizada
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Série"), size = 1.2) +
  geom_line(aes(x = tempo, y = dessazonalizada, color = "Desestacionalida"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legenda:",
       x = NULL,
       y = NULL) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")


# decompondo o PIB pelo modelo multiplicativo

decpib = decompose(precios_ts, type = "multiplicative")
plot(decpib)


# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random

### Test Estacionaridade
##################################################
## Analisando as séries autoregressivas
##################################################
## Realizar os testes de Estacionariedade
## Precisamos do pacote URCA - Unit Root and Cointegration Test
###################################################################
# Teste de Dickey-Fuller
# Ho: A série Não é Estacionária
# H1: A série é Estacionária
install.packages("astsa")
library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)


library("urca")
plot(precios_ts)

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#División de la serie temporal
precio_entreno=window(precios_ts, start=c(2015,1), end=c(2020,12))
precio_teste=window(precios_ts, start=c(2021,1), end=c(2021,12))
length(precio_teste)
length(precio_entreno)

testeprecio=ur.df(precio_entreno)
testeprecio
summary(testeprecio)
adf.test(precios_ts, alternative = "stationary") ##Test indica no es estacional

#Con una diferencia 
#### Debe ser estacionaria
seriedifprecio=diff(precios_ts)
seriedifprecio
ggtsdisplay(seriedifprecio)
adf.test(seriedifprecio)
testeseriepreciodif = ur.df(seriedifprecio)
summary(testeseriepreciodif)
arimaprecio =auto.arima(precio_entreno, trace = T)

# 1. teste de Ljung-Box p-value = 0.5589>0.01, aceitamos H0, resíduos não são
# correlacionados


checkresiduals(arimaprecio)

# 2. Normalidade dos resíduos
ks.test(arimaprecio$residuals, "pnorm", mean(arimaprecio$residuals),
        sd(arimaprecio$residuals))

# confirmada a não existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variãncia
# verificar se existe efeitos ARCH
ArchTest(arimaprecio$residuals)

## Previsao para a série de varejo SP

prevpreciobog=forecast::forecast(arimaprecio, h=12)

autoplot(prevpreciobog) +
  theme_bw()

accuracy(prevpreciobog, precio_teste)

ggplotly(
  autoplot(precio_entreno)+
    autolayer(precio_teste,serie="Valores Reales")+
    autolayer(prevpreciobog$mean, serie="Forecast")+
    labs(x = "Tiempo",
         y = "Precio (COP)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

#Con dos diferencias
#seriepreciosdif2=diff(precios_ts, differences = 2)
#plot(seriepreciosdif2)
#adf.test(seriepreciosdif2,alternative = "stationary")



