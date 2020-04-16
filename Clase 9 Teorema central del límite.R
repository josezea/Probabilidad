

############### Simular el ingreso  #######################
# Simular la distribución del ingreso de los hogares bogotanos
# https://math.stackexchange.com/questions/3104688/method-of-moments-with-a-gamma-distribution
# alpha: forma
# beta: escala
Promedio <- 3333780
S <- 4239029
N <- 3209379
# No preocuparse de donde salen los parámetros alpha y beta por el momento
alpha <- (Promedio ^ 2) / (S ^ 2)
beta <- (S ^ 2) / Promedio 

set.seed(11022020)
ingreso <- rgamma(n = N, shape = alpha, scale = beta)
summary(ingreso)
mean(ingreso)
sd(ingreso) / mean(ingreso)
hist(ingreso, breaks = 100)

############### Simular una muestra de tamaño  1, 2, ... 10, 20, 30,.. 100,500, 1000 
nsimulaciones <- 5000
tam_mue <- 3000
datos <- matrix(NA, nrow = nsimulaciones, ncol =  tam_mue)
for(j in 1:tam_mue){  
datos[,j] <- rgamma(n = nsimulaciones, shape = alpha, scale = beta)
}

# Ver como quedan los datos

Xbarra <- rowMeans(datos)
hist(Xbarra)
summary(Xbarra)

simula_Xbarra <- function(tam_mue, nsimulaciones = 5000){
datos <- matrix(NA, nrow = nsimulaciones, ncol =  tam_mue)
for(j in 1:tam_mue){  
  datos[,j] <- rgamma(n = nsimulaciones, shape = alpha, scale = beta)
}
Xbarra <- rowMeans(datos)
Xbarra
}


# Variar tamaños de muestra
hist(simula_Xbarra( tam_mue = 2))
hist(simula_Xbarra(tam_mue = 10))
hist(simula_Xbarra(tam_mue = 100))

hist(simula_Xbarra(tam_mue = 1000))
# hist(simula_Xbarra(tam_mue = 1000, 100))


hist(simula_Xbarra(1000))
mean(simula_Xbarra(1000))
sd(simula_Xbarra(1000))
sd(ingreso) / sqrt(1000)

hist(simula_Xbarra(2000))

mean(simula_Xbarra(2000))
mean(ingreso)

sd(simula_Xbarra(2000))
sd(ingreso) / sqrt(2000)


# Ver el código comentado cuando se vea estimación por momentos 
# setwd("C:/Users/Home/Documents/Laboral2020/Universidad Externando/Estadística2/Clase 5")
# library(readr)
# library(weights) # Histograma ponderado
# hogares <- read_delim("variables_adicionales_hogar_v3.txt", delim = ";" )
# 
# prom_ingreso <- weighted.mean(hogares$INGRESOS_HOG, hogares$FEX_C)
# S2_income <- wtd.var(x = hogares$INGRESOS_HOG,
#                           w = hogares$FEX_C,
#                           na.rm = FALSE)
# prom_ingreso
# sqrt(S2_income)
# sqrt(S2_income) / prom_ingreso * 100
# N_est <- sum(hogares$FEX_C)
# 
# ?rgamma
# # Teorema central del límite
# 
# # Simular la distribución del ingreso de los hogares bogotanos
# # https://math.stackexchange.com/questions/3104688/method-of-moments-with-a-gamma-distribution
# # alpha: forma
# # beta: escala
# alpha <- (prom_ingreso ^ 2) / S2_income
# beta <- S2_income / prom_ingreso 
# 
# weights::wtd.hist(x = hogares$INGRESOS_HOG, breaks = 500,
#                     xlim = c(0, 20000000), probability = T)
# curve(dgamma(x, shape = alpha, scale = beta), 
#       add=TRUE, col='blue', from = 0, to = 20000000)
# 
