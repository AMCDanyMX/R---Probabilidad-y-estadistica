##Daniel Alejandro Morales Castillo## #IS-PRrobabilidad y estadistica##

library(e1071)
library(MASS)
library(ggplot2)


###MEDIDAS DE TENDECNIA CENTRAL###
#---Medias---#
estatura = c(1.79,1.79,1.70,1.74,1.72,1.87,1.70,1.62,1.62,1.70)
kilogramos = c(81,63,80,70,54,85,78,56,70,67)
mean(estatura) 
mean(kilogramos)
#---Medianas---#
median(estatura)
median(kilogramos) 
#---Modas---#
mfv(estatura)
mfv(kilogramos)
#---Cuartiles---#
quantile(estatura)
quantile(kilogramos)
#---Minimos y máximos---#
min(estatura)
min(kilogramos)
max(estatura)
max(kilogramos)
#---IQR---#
IQR(estatura)
IQR(kilogramos)
#---Boxplot---#
boxplot(estatura,horizontal = TRUE, xlab="Estatura", main = "Cajas y bigotes para la variable estatura")
stripchart(estatura, method = "jitter", pch = 19, add = TRUE, col = "blue")
boxplot(kilogramos,horizontal = TRUE, xlab="Kilogramos", main = "Cajas y bigotes para la variable kilogramos")
stripchart(kilogramos, method = "jitter", pch = 19, add = TRUE, col = "red")

###MEDIDAS DE DISPERSIÓN###
#---Desviación estandar---#
sd(estatura)
sd(kilogramos)
#---Kurtosis---#
kurtosis(estatura)
kurtosis(kilogramos)
#---Varianza---#
var(estatura)
var(kilogramos)
#---Sesgo---#
skewness(estatura)
skewness(kilogramos)


###CORRELACIÓN###
cor(x=estatura,y=kilogramos)

###REGRESIÓN LINEAL###

file = "D:/Programacion/Probabilidad/datos.txt"
datos = read.table(file=file, header=TRUE)
head(datos) # muestra laas primeras seis filas


ggplot(datos, aes(x=estatura, y=kilogramos)) + 
  geom_point() + theme_dark()   ### Modelo = kilogramos~N(??i??^2),??i= b0+b1*estaturai,??^2=constante###


mod1 <- lm(estatura ~ kilogramos, data=datos)
mod1 # Para imprimir el objeto mod1
summary(mod1) ##para modelo ajustado



##recta de regresión que representa el modeloa justado
ggplot(datos, aes(x=estatura, y=kilogramos)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()


##distribución normal###


valores= dnorm(estatura)
#calculo de su densidad
randDensity <- dnorm(valores)

library(ggplot2)
ggplot(data.frame(x = estatura, y = randDensity)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "variable estatura")


#Calcular la proporción (o probabilidad) de valores menores de 1.70mm en una distribución normal de tamaños de semillas, con media = 6.0 mm y desviación estándar = 1.1 mm:
pnorm(1.70, mean = 1.725, sd = 0.07720823, lower.tail = TRUE)
## [1] 0.7072795
#y la proporción de valores mayores de 1.80 mm:
pnorm(1.80, mean = 1.725, sd = 0.07720823, lower.tail = FALSE)


##Z sea variable aleatoria determinar...
#a) P(Z > 1.70).
##b) P(1.60 ??? Z ??? -1.70).
#c) P(0 ??? Z ??? 1.80).
#d) P(Z<1.70)



pnorm(1.70, mean = 1.725, sd = 0.07720823, lower.tail = F)
pnorm(c(1.60), mean = 1.725, sd = 0.07720823) - pnorm(c(-1.70), mean =1.725, sd = 0.07720823)
pnorm(c(1.73), mean = 1.725, sd = 0.07720823) - pnorm(c(0), mean =1.725, sd = 0.07720823)
pnorm(-1.70, mean = 1.725, sd = 0.07720823, lower.tail = F)

 
#Ejemplo para regresión multiple//más complejo 

#Un estudio quiere generar un modelo que permita predecir la esperanza de vida media de los habitantes de una ciudad 
#en función de diferentes variables. Se dispone de información sobre: 
#habitantes, analfabetismo, ingresos, esperanza de vida, asesinatos, universitarios, heladas, 
#área y densidad poblacional.


library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)


round(cor(x = datos, method = "pearson"), 3)


library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#Generr el modelo
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)


##T student##
##---------uso de la funcion qt---------##
qt(p = 0.95 + 0.05/2, df = 15, lower.tail = TRUE)


##ejemplo de t student## variables aleatorias##


set.seed(10)
x1 <- rnorm(100,1.725) # Variable aleatoria de media 1.725
x2 <- rnorm(100,70.4) # Variable aleatoria de media 70.4

test <- t.test(x1,x2) # Prueba t de Student

print(test)


##graficos 
boxplot(x1,x2,names=c("X1","X2"))#Muestra las diagramas
medias <- c(mean(x1),mean(x2))#Muestra la Media mediante un punto
points(medias,pch=18,col="red")#Resalta la media de un color





