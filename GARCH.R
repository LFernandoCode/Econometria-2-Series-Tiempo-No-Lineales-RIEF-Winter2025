# Título:      Análisis de Series de Tiempo con GARCH
# Autor:       Fernando Flores
# Fecha:       25/09/2025
# Descripción: Este script realiza la lectura de series de tiempo, 
#              transforma la variable a retornos, realiza pruebas de 
#              estacionariedad, ajusta modelos ARMA y GARCH, evalúa 
#              heterocedasticidad y genera predicciones con intervalos 
#              de confianza.
#
#
###############################################################################
#######                          LIBRERÍAS                               ######
###############################################################################
library(readxl)
library(dplyr)
library(zoo)
library(tseries)
library(urca)
library(broom)
library(quantmod)
library(forecast)
library(MSGARCH)
library(FinTS)
library(rugarch)
###############################################################################
#######                              DATOS                               ######
###############################################################################
#EXCEL
direc1 <- "C:/Users/Fernando Flores/OneDrive/Escritorio/Nueva carpeta/Codes/Bses de datos/IPC_mens.xlsx"
direc2 <- "C:/Users/Fernando Flores/OneDrive/Escritorio/Nueva carpeta/Codes/Bses de datos/Tc_diario.xlsx"

b1 <- read_xlsx(direc2, sheet ="Datos")

###Procesamiento
b1 <- b1 %>%
      rename(fecha = ...1,
            val   = 2   )

b1 <- b1 %>%
      arrange(fecha) %>%
      mutate(rend = c(rep(NA), diff(log(val)))) %>%
      filter(!is.na(rend))


#######     Sub periodo    ################### #
b1 <- b1 %>%
      filter(fecha >= as.POSIXct("2009-01-01"),#
             fecha <= as.POSIXct("2021-12-31"))#2021
                                               #
###############################################


#plot(b1$rend, type = "l", main = "Log-retornos", ylab = "Rendimiento")
b1_z <- zoo(b1$rend, order.by = b1$fecha)
chartSeries(b1_z)


###############################################################################
#######             Estacionariedad                               ######
###############################################################################
# Test ADF Simple #Ho no estacionariedad, raiz unitaria
adf.test(b1_z) 

# Test ADF 
adf2 <- ur.df(b1_z, type = "none", selectlags = "AIC")
summary(adf2)
adf2@lags

e <- adf2@res
DW <- sum(diff(e)^2) / sum(e^2)
DW
###############################################################################
#######                              ARIMA                      ######
###############################################################################
par(mfrow=c(2,1), mar = c(4,4,1,1))
acf(b1$rend)
pacf(b1$rend)
auto.arima(b1$rend)
arb1<- arima(b1$rend,order = c(0,0,1))
arb1
#Test de autocorrelación 
Box.test(residuals(arb1), type = "Ljung-Box")
tsdiag(arb1)
###############################################################################
#######                             Heterocelasticidad                    ######
###############################################################################
res_ar_cu <- residuals(arb1)^2

plot(res_ar_cu,type="l")

par(mfrow=c(2,1), mar = c(4,4,1,1))
acf(res_ar_cu) #garch
pacf(res_ar_cu)  #arch

ArchTest(b1$rend,lag=1,demean=TRUE)
###############################################################################
#######                              GARCH                      ######
###############################################################################

############3

spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),    #varianza condicional, residuos cuadrado(garch,arch)
      mean.model     = list(armaOrder = c(1,1), include.mean = FALSE),
      distribution.model = "norm"  #std para t
)

garch <- ugarchfit(spec, data = b1_z)
garch


# Volatilidad condicional


sigma_t <- sigma(garch)


# Retornos vs volatilidad

par(mfrow=c(2,1), mar = c(4,4,1,1))
plot(b1_z, type="l", main="Retornos y Volatilidad condicional", ylab="Retorno")
plot(sigma_t,type="l", main = "Volatilidad condicional (sigma_t)", ylab = "Sigma", xlab = "Fecha")

###############################################################################
#######                            Predicción                              ######
###############################################################################


# 1. Predicción con GARCH 
garch_forecast <- ugarchforecast(garch, n.ahead = 24)

# 2. Extraer media y sigma
fitted_forecast <- fitted(garch_forecast)      # predicción media
sigma_forecast  <- sigma(garch_forecast)       # desviación estándar condicional

# 3. Crear fechas futuras
fechas_future <- seq(max(b1$fecha) + 1, by = "month", length.out = 24)

# 4. Ajustar límites del gráfico
x_lim <- c(min(b1$fecha), max(fechas_future))
ylim <- range(c(b1$rend,
                fitted_forecast + 3*sigma_forecast,
                fitted_forecast - 3*sigma_forecast))

# 5. Graficar serie histórica
plot(b1$fecha, b1$rend, type="l", col="black",
     main="Serie de retornos y predicción con intervalos de confianza",
     xlab="Tiempo", ylab="Retorno",
     xlim=x_lim, ylim=ylim)

# 6. Sombreado de intervalos de confianza
conf_levels <- c(0.90, 0.95, 0.99)
sigma_factors <- c(1.645, 2, 2.576)  # valores z para cada nivel de confianza

colors <- c(rgb(0,0,1,0.2), rgb(0,0,1,0.15), rgb(0,0,1,0.1))

for(i in seq_along(sigma_factors)){
      polygon(c(fechas_future, rev(fechas_future)),
              c(fitted_forecast + sigma_factors[i]*sigma_forecast,
                rev(fitted_forecast - sigma_factors[i]*sigma_forecast)),
              col = colors[i], border = NA)
}

# 7. Media pronosticada
lines(fechas_future, fitted_forecast, col="red", lwd=2)


###############################################################################
#######                             MS-GARCH                               ######
###############################################################################
spec <- CreateSpec(
       variance.spec = list(model = "sGARCH"),  # GARCH(1,1) clásico
       distribution.spec = list(distribution = "norm"), # Normal condicional
       switch.spec = list(K = 2)  # 2 regímenes (baja y alta volatilidad)
       )
sgarch <- FitML(spec = spec, data = b1$rend)
summary(sgarch)

###############################################################################
#######                             T-GARCH                               ######
###############################################################################

spec_tgarch <- ugarchspec(
      variance.model = list(
            model = "gjrGARCH",      
            garchOrder = c(1,2)     
      ),
      mean.model = list(
            armaOrder = c(2,0),      
            include.mean = FALSE      
      ),
      distribution.model = "norm"  
)

tgarch <- ugarchfit(spec = spec_tgarch, data = b1$rend)
tgarch

###############################################################################
#######                             E-GARCH                               ######
###############################################################################

spec_egarch <- ugarchspec(
      variance.model = list(
            model = "eGARCH",      # EGARCH
            garchOrder = c(1, 1)   # (p,q)
      ),
      mean.model = list(
            armaOrder = c(2, 0),   # ARMA(2,0) en la media
            include.mean = TRUE     # incluir constante en la media
      ),
      distribution.model = "norm"  # distribución normal de los errores
)


fit_egarch <- ugarchfit(spec = spec_egarch, data = b1$rend)
fit_egarch



###############################################################################
#######          Distribución de probabilidad                              ######
############################################################################### 
std_resid <- residuals(garch, standardize = TRUE)

hist(std_resid, breaks=50, main="Histograma de residuos estandarizados", xlab="Residuos")
