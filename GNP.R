## Time Series GNP Singapore
## Macroeconomics 4 
## ARIMA

## Stablish 

setwd("~/Documents/ULACIT/CUATRIMESTRE 7/Macroeconomia 4")

#Cargar librerias
library(readxl)
library(lubridate)
library(tseries)
library(lubridate)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)
library(dplyr)

## Import Data set

ARIMA <- read_excel("GDP_Variation.xlsx")
View(ARIMA)

GOODS <- read_excel("GOODS.xlsx")
View(GOODS)

Services <- read_excel("Services.xlsx")
View(Services)

ARIMA2 <- read_excel("GDP_Singapore.xlsx")
View(ARIMA2)

#Time series 

#Paso 1. Transfor to a time series

Arimar1.ts=ts(ARIMA, start = c(1976,1), frequency = 4)

print(Arimar1.ts)

class(Arimar1.ts)

start(Arimar1.ts)
end(Arimar1.ts)

## Plot

plot(Arimar1.ts,  main="Variaciones de PIB cuatrimestral de Singapore 1960-2021", ylab="Variacion", col="red")

## Tests

acf(Arimar1.ts)
pacf(Arimar1.ts)

##Descripve analysis

summary(Arimar1.ts)

#Stacionariety 

ndiffs(Arimar1.ts)
adf.test(Arimar1.ts)

## Take the AR and MA 

gdpmodel=auto.arima(Arimar1.ts,ic = "aic",trace = TRUE)

## Arima model

(fit <- auto.arima(Arimar1.ts))

fit %>% forecast() %>% autoplot()

predict(fit)

### Box Test
Box.test(residuals(fit),type="Ljung-Box")

##Residiual errors

error1 = residuals(fit)
plot(error1)

## Predictions
pronostico=forecast::forecast(fit,h=4)
pronostico
plot(pronostico)

## Model analysis

### The system determines a model of 0 AR with 1 difference and 1 MA, in addition to 0 AR, 0 differences and 1 MA with a Zero mean with drift.

## Conclusiones

### It can be inferred that the Singapore GDP is going to have an variation of 3,3% in average over 2022, with a peak of 5,5% in the IQ 
### then, it is going to have an increase of 3,5-3,7% for the last 2 quarters. This perspectives have similar number of the Singapore analysts numbers for 2022. 

