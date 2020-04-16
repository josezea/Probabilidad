library(boot)
library(TeachingSampling)
data("Lucy")
set.seed(12345)
# Estaturas de 1000 hombres (n = 1000)
y <- rnorm(n = 1000, mean =  170, sd =  8)
hist(y)

f_Bootmedia <- function(x, indices){
  x <- x[indices]
  mean(x)  
}

set.seed(12345)
Bootstrap <- boot(y, f_Bootmedia, R=5000)
plot(Bootstrap, index=1)

boot.ci(Bootstrap, index = 1, conf = 0.97)
t.test(y, conf = 0.97)

tableOfIndices <-boot.array(Bootstrap, indices=F)



f_Bootmediana <- function(x, indices){
  x <- x[indices]
  median(x)  
}
Bootstrap_mediana <- boot(y, f_Bootmediana, R=5000)

boot.ci(Bootstrap_mediana, index = 1, conf = 0.97)

# Ejercicio : 
# P25: quantile(x, 0.25), Mediana, P75, Promedio


estadisticos <- function(x, indices){
  x <- x[indices]
  c(
    quantile(x, 0.25),
    median(x),
    quantile(x, 0.75),
    mean(x)
  )
}
Bootstrap_est <- boot(y, estadisticos, R=2000)
boot.ci(Bootstrap_est, index = 1, conf = 0.95)
boot.ci(Bootstrap_est, index = 2, conf = 0.95)
boot.ci(Bootstrap_est, index = 3, conf = 0.95)
boot.ci(Bootstrap_est, index = 4, conf = 0.95)

############## Ejemplo 2 para datos no normales #######
library(boot)
library(TeachingSampling)
data("Lucy")
hist(Lucy$Income)
Bootstrap <- boot(Lucy$Income, f_Bootmedia, R=1000)
plot(Bootstrap, index=1)
boot.ci(Bootstrap, index = 1, conf = 0.95, 
        type = "norm")
boot.ci(Bootstrap, index = 1, conf = 0.95, 
        type = "perc") # Método más adecuado cuando no hay normalidad
        

################## Paso a paso #######################
# Estimar IC y estimación puntual para el promedio 
set.seed(12345)
y <- rnorm(n = 1000, mean =  170, sd =  8)
B = 5000
xbarra_i <- numeric(B)
for(i in 1:B){
set.seed(i)  
indices <- sample(1:length(y), length(y), replace = T )
xbarra_i[i] <- mean(y[indices])
}
summary(xbarra_i)
mu_estimado_boot <- mean(xbarra_i)
S_boot <- sd(xbarra_i)
LI <- mu_estimado_boot - qnorm(0.975) * S_boot
LS <- mu_estimado_boot + qnorm(0.975) * S_boot
c(LI, mu_estimado_boot, LS)
hist(x_barra_i)
