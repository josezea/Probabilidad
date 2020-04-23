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
Xbarra <- rowMeans(df2)
S2 <- apply(df, MARGIN = 1, FUN = var)
M <- apply(df, MARGIN = 1, FUN = median)

LI_Xbarra <- Xbarra - qt(0.975, n-1) * sqrt(S2 / n)
LS_Xbarra <- Xbarra + qt(0.975, n-1) * sqrt(S2 / n)

table(LI_Xbarra <= mu &  mu <= LS_Xbarra)
prop.table(table(LI_Xbarra <= mu &  mu <= LS_Xbarra))

rm(n)
rm(B)
rm(mu)
rm(sigma)

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

