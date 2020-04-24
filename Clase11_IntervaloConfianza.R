simula_poblacion_normal <- function(tam_mue, MU, SIGMA, nsimulaciones = 5000){
  datos <- matrix(NA, nrow = nsimulaciones, ncol =  tam_mue)
  
  for(j in 1:tam_mue){  
    datos[,j] <- rnorm(n = nsimulaciones, MU, SIGMA)
  }
  
  datos  
}



simula_poblacion_gamma <- function(tam_mue, ALPHA, BETA, nsimulaciones = 5000){
  datos <- matrix(NA, nrow = nsimulaciones, ncol =  tam_mue)
  
  for(j in 1:tam_mue){  
    datos[,j] <- rgamma(n = nsimulaciones, shape = ALPHA, scale = BETA)
  }

  datos  
}



################# Ejemplo 1: Si hay  normalidad ####################
n <- 400
B <- 50000
mu <- 170
sigma <- 8
df <- simula_poblacion_normal(tam_mue = n, MU = mu, SIGMA = sigma, nsimulaciones = B)

# Estadísticos 
Xbarra <- rowMeans(df)
S2 <- apply(df, MARGIN = 1, FUN = var)
M <- apply(df, MARGIN = 1, FUN = median)

# Inicial
LI_Xbarra <- Xbarra - qnorm(0.975) * sqrt(64 / n)
LS_Xbarra <- Xbarra + qnorm(0.975) * sqrt(64 / n)

# Ejercicios 
set.seed(12345)
estaturas <- rnorm(n = 100, mean = 170, sd = 8)
xbarra <- mean(estaturas)
conf <- 0.95
alpha <- 1- conf
z <- qnorm(1-alpha/2)
LI <- xbarra - z * (8 / sqrt(100))
LS <- xbarra + z * (8 / sqrt(100))
c(LI, LS)

# Ejercicio con t - student 
ejercicios <- c(7L, 2L, 10L, 0L, 6L, 3L, 3L)
t.test(ejercicios, conf.level = 0.95)
prom <- mean(ejercicios)
t <- qt(0.975, 6)
s <- sd(ejercicios)
prom - t * (s/sqrt(7))
prom + t * (s/sqrt(7))

LI_Xbarra <- Xbarra - qt(0.975, n-1) * sqrt(S2 / n)
LS_Xbarra <- Xbarra + qt(0.975, n-1) * sqrt(S2 / n)

table(LI_Xbarra <= mu &  mu <= LS_Xbarra)
prop.table(table(LI_Xbarra <= mu &  mu <= LS_Xbarra))

rm(n)
rm(B)
rm(mu)
rm(sigma)


######## Intervalo de confianza para una proporción ##############
library(TeachingSampling)
data(Lucy)
Lucy$SPAM <- ifelse(Lucy$SPAM == "yes", 1, 0)

#IC para los que mandan SPAN
n <- 100
set.seed(12345)
muestra <- Lucy[sample(2396, 100),]
# Construir intervalo de confianza 98%  para prop de los que envían SPAM
pgorro <- mean(muestra$SPAM)
conf <- 0.98
alfa <- 1- conf
z <- qnorm(1-alfa/2)
LI <- pgorro - z * sqrt((pgorro * (1-pgorro)) / 100)
LS <- pgorro + z * sqrt((pgorro * (1-pgorro)) / 100)
c(LI, LS)


ic_prop <- function(x, confianza = 0.95) {
  p_gorro <- mean(x)
  alfa <- 1- confianza
  n <- length(x)
  z <- qnorm(1-(alfa/2))
  LI <- p_gorro - (z * sqrt((p_gorro * (1-p_gorro))  / n))
  LS <- p_gorro + (z * sqrt((p_gorro * (1-p_gorro))  / n))
  return(c(LI, LS))
}
ic_prop(muestra$SPAM, confianza = 0.98)

################# Ejemplo 2: Si no hay normalidad ####################
############### Simular el ingreso  #######################
# Simular la distribución del ingreso de los hogares bogotanos
# https://math.stackexchange.com/questions/3104688/method-of-moments-with-a-gamma-distribution
# alpha: forma
# beta: escala
mu <- 3333780
sigma <- 4239029
n <- 400
B <- 50000

# No preocuparse de donde salen los parámetros alpha y beta por el momento
alpha <- (mu ^ 2) / (sigma ^ 2)
beta <- (sigma ^ 2) / mu 

df2 <- simula_poblacion_gamma(tam_mue = n, alpha, beta, nsimulaciones = B)

# Estadísticos 
Xbarra <- rowMeans(df2)
S2 <- apply(df2, MARGIN = 1, FUN = var)
M <- apply(df2, MARGIN = 1, FUN = median)

LI_Xbarra <- Xbarra - qt(0.975, n-1) * sqrt(S2 / n)
LS_Xbarra <- Xbarra + qt(0.975, n-1) * sqrt(S2 / n)

table(LI_Xbarra <= mu &  mu <= LS_Xbarra)
prop.table(table(LI_Xbarra <= mu &  mu <= LS_Xbarra))

