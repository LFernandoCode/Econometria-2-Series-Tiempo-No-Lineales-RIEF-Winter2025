# Título:      Análisis de Series de Tiempo con MSM
# Autor:       Fernando Flores
# Fecha:       25/09/2025
# Descripción: 
#   Este script tiene como objetivo ilustrar el uso de modelos de Markov 
#   Switching (MSM) para analizar series macroeconómicas, como el PIB y 
#   otra serie de interés. Se muestran los pasos desde la carga y limpieza 
#   de datos hasta la estimación del modelo y la visualización de regímenes 
#   ocultos.
# # Notas importantes: Modificar las direcciónes

###############################################################################
#######                          LIBRERÍAS                               ######
###############################################################################
library(readxl)
library(tidyverse)
library(quantmod)
library(MSGARCH)
library(MSwM)


###############################################################################
#######                          Datos                               ######
###############################################################################

Direcc1 <- "C:/Users/Fernando Flores/OneDrive/Escritorio/Nueva carpeta/Presentación/Bses usadas"
Base <- "PIB_peru"
ruta <- paste0(Direcc1, "/", Base, ".xlsx")

base1 <- read_excel(ruta, sheet = "Quarterly")


###############################################################################
#######                          Procesamiento datos                              ######
###############################################################################
base_growth <- base1 %>%
      arrange(observation_date) %>%
      mutate(GDP_growth = 100 * (log(GDP) - log(lag(GDP, 4)))) %>%
      drop_na()

##Visualizar

par(mfrow=c(2,1), mar = c(4,4,1,1))
plot(base_growth$GDP_growth, type="l")
plot(base_growth$observation_date, base_growth$NBER, type="l")
######################################
     #Time Series                                 
                                     #
######################################

y_ts <- ts(base_growth$GDP_growth,
           start = c(year(base_growth$observation_date[1]),
                     quarter(base_growth$observation_date[1])),
           frequency = 4)

chartSeries(y_ts)

###############################################################################
#######                          Procesamiento datos                              ######
###############################################################################

y_s <- base_growth$GDP_growth


lrm <- lm(y_s ~ 1)

rsm <- msmFit(
      lrm,
      k = 3,
      sw = c(TRUE,TRUE),  #cambio media, varianza de e
      control = list(parallel = FALSE)#depende la Pc
)


###############################################################################
#######                         Graficos                            ######
###############################################################################


# filtered / smoothed probabilities
x11(); plotProb(rsm, which = 1)  # filtradas vs smoothed
x11(); plotProb(rsm, which = 2)  # régimen 1 y área sombreada
x11(); plotProb(rsm, which = 3)  # régimen 2 y área sombreada
x11(); plotProb(rsm, which = 4)  # régimen 3 y área sombreada


dates <- as.Date(base_growth$observation_date)
nber <- base_growth$NBER  

# Graficar con NBER
probflitrada <- rsm@Fit@filtProb[,1]
plot(dates, probflitrada, type="l", col="blue", lwd=2,
     xlab="Fecha", ylab="Probabilidad filtrada", main="Probabilidad filtrada con NBER")
# Agregar NBER como línea roja
lines(dates, nber, col = "red", lwd = 2)


