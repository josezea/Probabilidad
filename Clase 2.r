library(readr)
setwd("C:/Users/usuario/Desktop")
dir()
datos <- readr::read_delim("variables_adicionales_hogar_v3.txt",
                    delim = ";")
y <- datos$INGRESOS_HOG
summary(y)
# Desviacion absoluta media
mean(abs(y - mean(y)))

DAM <- function(x){
  mean(abs(x - mean(x)))
}
DAM(datos$INGRESOS_HOG)

?mad
mad(datos$INGRESOS_HOG, constant = 1)

MAD <- function(x){
  median(abs(x-median(x)))
}
MAD(datos$INGRESOS_HOG)

# Desviaci?n estandar
sd(datos$INGRESOS_HOG) # denominador dentro raiz es n-1

nclass.Sturges(datos$INGRESOS_HOG)
nclass.FD(datos$INGRESOS_HOG)

hist(datos$INGRESOS_HOG, breaks = 2628)
hist(datos$INGRESOS_HOG, xlim = c(0, 20000000),
     breaks = 100)
library(lattice)
histogram(datos$INGRESOS_HOG, breaks = 2628)

# Coeficiente de variaci?n
100 * sd(datos$INGRESOS_HOG) / mean(datos$INGRESOS_HOG)


# Calcular las medidas de centralidad y dispersi?n b?sicas
# para EL IPM (?ndice de pobreza multidimensional)
# Histograma
hist(datos$IPM)
summary(datos$IPM)
100 * sd(datos$IPM) / mean(datos$IPM)
100 * mad(datos$IPM, constant = 1) / median(datos$IPM)
