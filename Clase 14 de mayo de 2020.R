
# Ho: mu = 350 vs H1: mu != 350
# sigma = 10
xbarra <- 353

pnorm(q = 347, mean = 350, sd= 10 / sqrt(100)) + pnorm(q = 353, mean = 350, sd = 10 / sqrt(100), lower.tail = F) 

pnorm(-3) + pnorm(3, lower.tail = F)


# SUponga población del embotellamiento de gaseoas y suponga que es una variable normal
# mu = 349,8, sigma = 9
set.seed(12345)
x <- rnorm(n = 100, mean = 349.8, 9)
# Saco mi muestra
n <- length(x)
xbarra <-  mean(x)
s <- sd(x)  

# H0: mu = 350 vs H1: mu != 350  
# 1. Calculo estadístico de prueba
t_c <- (xbarra - 350) / (s / sqrt(n)) 
2*pt(-2, 100-1)

t.test(x, mu = 350)
# El pvalor es menor a 0.05, luego rechazo H0 (el embotellamiento no está en 350)


# Simular una muestra de 400 datos con mu = 2000000, sigma = 1000000
set.seed(12345)
x <- rnorm(n = 400, mean = 2000000, sd = 1000000)
hist(x)
mean(x)
sd(x)
pt((mean(x) - 2000000) / (sd(x) / sqrt(400)), df = 399)
?t.test
t.test(x, mu = 2000000, alternative = "less" )

###################### Discriminación ######################
set.seed(12345)
x1 = rnorm(n=40, mean = 3200000, sd = 100000)
set.seed(12345)
x2 = rnorm(n=40, mean = 3000000, sd = 98000)

t.test(x1,x2, mu = 0, alternative = "greater")
t.test(x1,x2, mu = 100000, alternative = "greater")
t.test(x1,x2, mu = 190000, alternative = "greater")
t.test(x1,x2, mu = 200000, alternative = "greater")



set.seed(12345)
x1 = rnorm(n=400, mean = 3200000, sd = 100000)
set.seed(12345)
x2 = rnorm(n=400, mean = 3000000, sd = 98000)

t.test(x1,x2, mu = 0, alternative = "greater")
t.test(x1,x2, mu = 100000, alternative = "greater")
t.test(x1,x2, mu = 190000, alternative = "greater")
t.test(x1,x2, mu = 200000, alternative = "greater")

# Pruebas de permutacion
datos <- data.frame(ID = 1:800, sexo = c(rep("M", 400),
                                         rep("F", 400)),
                    ingreso = c(x1, x2))
boxplot( ingreso ~ sexo, data = datos)

# Preliminares
prueba <- c(8, 9, 2, 5, 10, 3, 100, 6)
factorial(8)
sample(prueba, size = 8,   replace = F)

# Permutar el ingreso

datosP1 <- datos[sample(1:800, size = 800),]
datosP1$ID <- 1:800
datosP1$sexo <-  c(rep("M", 400), rep("F", 400))

boxplot( ingreso ~ sexo, data = datosP1)

################# Repetir proceso 5000 ##########
num_simula <- 5000
D <- vector(mode = "numeric", length = num_simula)
for(i in 1:num_simula){
ingreso <- datos$ingreso[sample(1:800, 800, replace = F)] # Permuta ingreso
# Diferencia de medias
D[i] <- mean(ingreso[1:400]) - mean(ingreso[401:800]) # Diferencia de medias
}
hist(D)
abline(v = mean(x1) - mean(x2), col = "red")

prop.table(table(
  datosP1 <- datos[sample(1:800, size = 800),]
  datosP1$ID <- 1:800
  datosP1$sexo <-  c(rep("M", 400), rep("F", 400))
  
  boxplot( ingreso ~ sexo, data = datosP1)
  
  ################# Repetir proceso 5000 ##########
  num_simula <- 5000
  D <- vector(mode = "numeric", length = num_simula)
  for(i in 1:num_simula){
    ingreso <- datos$ingreso[sample(1:800, 800, replace = F)] # Permuta ingreso
    # Diferencia de medias
    D[i] <- mean(ingreso[1:400]) - mean(ingreso[401:800]) # Diferencia de medias
  }
  hist(D)
  abline(v = mean(x1) - mean(x2), col = "red")
  # Pvalor
  sum(D >= (mean(x1) - mean(x2))) / 5000
  
        