# ExamenBioinformatica_2022

library(nycflights13)
weather <- nycflights13::weather

# Pregunta 1. Comandos de Linux

## a) ¿Cuántas palabras hay en el fichero?

    ## cat genes.txt | wc -w
    
## b) ¿Cómo cambiarías todos los tabulados por guiones?

    ## cat genes.txt |  sed 's/\t/-/g'

## c) ¿Cómo cambiarías solo la primera aparición?

    ## cat genes.txt | sed 's/\t/-/'

## d) ¿Cómo buscarías todos los genes excepto el gen llamado DSTYK?

    ## cat genes.txt | -v grep DSTYK
    
# Pregunta 2. Comandos RStudio

## a) ¿Cuántas columnas y cuántos registros tiene ese dataset?

ncol(weather)
nrow(weather)

## b) ¿Cuántos "origin" diferentes existen y para cada uno de ellos cuántos registros hay?

table(weather$origin)

## c) En LGA, ¿cuáles fueron la mediana del wind_speed y la media de pressure?

LGA <- weather[weather$origin == "LGA",]

median(LGA$wind_speed, na.rm = T)
mean(LGA$pressure, na.rm = T)

## d) Después de eliminar los NA de la columna de wind_gust, calcula para cada mes la media de wind_speed y wind_gust, y el número de casos

library(lubridate)
library(tidyverse)
library(lubridate)
library(dplyr)

weather_NA <- weather[complete.cases(weather$wind_gust),]

meses <- weather_NA %>%
  group_by(month) %>%
  summarise(media_wind_speed = mean(wind_speed),
            media_wind_gust = mean(wind_gust),
            n_casos = n()
  )
  meses

# Pregunta 3. Intenta hacer este plot

## En primer lugar se crean 12 variables que contengan los doce meses del año y los diferentes orígenes

EWR1 <- weather[weather$origin == "EWR" & weather$month == 1,]
EWR2 <- weather[weather$origin == "EWR" & weather$month == 2,]
EWR3 <- weather[weather$origin == "EWR" & weather$month == 2,]
EWR4 <- weather[weather$origin == "EWR" & weather$month == 4,]
EWR5 <- weather[weather$origin == "EWR" & weather$month == 5,]
EWR6 <- weather[weather$origin == "EWR" & weather$month == 6,]
EWR7 <- weather[weather$origin == "EWR" & weather$month == 7,]
EWR8 <- weather[weather$origin == "EWR" & weather$month == 8,]
EWR9 <- weather[weather$origin == "EWR" & weather$month == 9,]
EWR10 <- weather[weather$origin == "EWR" & weather$month == 10,]
EWR11 <- weather[weather$origin == "EWR" & weather$month == 11,]
EWR12 <- weather[weather$origin == "EWR" & weather$month == 12,]

JFK1 <- weather[weather$origin == "JFK" & weather$month == 1,]
JFK2 <- weather[weather$origin == "JFK" & weather$month == 2,]
JFK3 <- weather[weather$origin == "JFK" & weather$month == 3,]
JFK4 <- weather[weather$origin == "JFK" & weather$month == 4,]
JFK5 <- weather[weather$origin == "JFK" & weather$month == 5,]
JFK6 <- weather[weather$origin == "JFK" & weather$month == 6,]
JFK7 <- weather[weather$origin == "JFK" & weather$month == 7,]
JFK8 <- weather[weather$origin == "JFK" & weather$month == 8,]
JFK9 <- weather[weather$origin == "JFK" & weather$month == 9,]
JFK10 <- weather[weather$origin == "JFK" & weather$month == 10,]
JFK11 <- weather[weather$origin == "JFK" & weather$month == 11,]
JFK12 <- weather[weather$origin == "JFK" & weather$month == 12,]

LGA1 <- weather[weather$origin == "LGA" & weather$month == 1,]
LGA2 <- weather[weather$origin == "LGA" & weather$month == 2,]
LGA3 <- weather[weather$origin == "LGA" & weather$month == 3,]
LGA4 <- weather[weather$origin == "LGA" & weather$month == 4,]
LGA5 <- weather[weather$origin == "LGA" & weather$month == 5,]
LGA6 <- weather[weather$origin == "LGA" & weather$month == 6,]
LGA7 <- weather[weather$origin == "LGA" & weather$month == 7,]
LGA8 <- weather[weather$origin == "LGA" & weather$month == 8,]
LGA9 <- weather[weather$origin == "LGA" & weather$month == 9,]
LGA10 <- weather[weather$origin == "LGA" & weather$month == 10,]
LGA11 <- weather[weather$origin == "LGA" & weather$month == 11,]
LGA12 <- weather[weather$origin == "LGA" & weather$month == 12,]


## A continuación, se procede a crear el diagrama de cajas y bigotes que contenga cada mes y temperatura en función del origen

par(mfrow = c(1,3))
boxplot(EWR1$temp, EWR2$temp, EWR3$temp, EWR4$temp, EWR5$temp, EWR6$temp, EWR7$temp, EWR8$temp, EWR9$temp, EWR10$temp, EWR11$temp, EWR12$temp, xlab = "Months", ylab = "ºC", main = "EWR", col = 2)

boxplot(JFK1$temp, JFK2$temp, JFK3$temp, JFK4$temp, JFK5$temp, JFK6$temp, JFK7$temp, JFK8$temp, JFK9$temp, JFK10$temp, JFK11$temp, JFK12$temp, xlab = "Months", ylab = "ºC", main = "JFK", col = 3)

boxplot(LGA1$temp, LGA2$temp, LGA3$temp, LGA4$temp, LGA5$temp, LGA6$temp, LGA7$temp,LGA8$temp, LGA9$temp, LGA10$temp, LGA11$temp, LGA12$temp, xlab = "Months", ylab = "ºC", main = "LGA", col = 4)

## Funcion que plotee lo anterior

plot_meteo <- function(dat = weather, meteo = "temp", titulo = "Punto de rocío", unidades = "º F")
{
dat <- data.frame(dat)
...
}

plot_meteo <-function(dat,meteo,titulo,unidad)
{
  
  dat<- data.frame(dat)
  plot(dat[,meteo],type="l",col="blue", main= titulo,xlab="Hours",ylab= unidad)
  
}
plot_meteo(dat=weather,meteo = "temp",titulo="Humedad",unidad = "Relative humidity")

plot_meteo(meteo = "humid", titulo = "Humedad", unidad = "Relative humidity")
  
plot_meteo(dat= weather,meteo = "humid", titulo = "Humedad", unidad = "Relative humidity")

## Pregunta 4.4. (2 puntos) El día de tu cumpleaños:

## a. Qué correlación tuvieron la temperatura y humedad en cada uno de los origin? Plotealo mediante puntos con ggplot



## b. Si comparas la temperatura en los origins JFK y LGA, son estadísticamente diferentes? ¿Qué p valor consigues? Plotea los boxplots.


## Pregunta 5. 

## a. ¿Cuál es el punto con FC (Fold change) más grande en valores absolutos?

### El gen con mayor Fold change en valores absolutos es aquel que se sitúa más alejado del centro sobre el eje x. Por tanto, el gen en cuestión sería el de más a la izquierda, situado más o menos por debajo del gen Csn. Este gen en cuestión no tiene nombre. Se adjunta el pdf del examen con el gen redondeado.

## b. ¿Qué gen sobreexpresado es el más significativo?

### El gen sobreexpresado más significativo es el Csn1s2b ya que es el que se sitúa más arriba en el eje y ( -log(pvalue))
