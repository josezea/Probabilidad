
# Lanzar un dado y obtener el número 6

# Probabilidad puntual
dbinom(x = 2, size = 10, prob = 1/6)
choose(10, 2) * ((1/6)^2) * ((5/6)^8)

# P(X >2) = P(X >= 3) = 1 - P(X = 0) - P(X = 1) - P(X=2)
# P(X=3) + P(X=4)+...P(X = 10)
1-dbinom(x = 0, size = 10, prob = 1/6) -
  dbinom(x = 1, size = 10, prob = 1/6) -
    dbinom(x = 2, size = 10, prob = 1/6) 
# 1 - P(X <= 2)   # P(X <= 2): Prob acumulada
1-pbinom(q = 2, size = 10, prob = 1/6)    

# Valor de esperado de ganancia
#1000000*0.2247732 - 280000 * (1-0.2247732) 


# Ejercicio: P(X < 4) = P(X <= 3)
pbinom(q = 3, size = 10, prob = 1/6)    
# n = 10, P = 1/6
P(2 <= X <= 5) = ????

dbinom(x = 2, size = 10, prob = 1/6) +    
dbinom(x = 3, size = 10, prob = 1/6) +
dbinom(x = 4, size = 10, prob = 1/6) +
dbinom(x = 5, size = 10, prob = 1/6) 

pbinom(q = 5, size = 10, prob = 1/6) -
pbinom(q = 1, size = 10, prob = 1/6) 

plot(0:10,dbinom(x = 0:10, size = 10, prob = 1/6), type = "h")


# Esperado E(X) = np
10 * 1/6

# Esperado V(X) = npq = np(1-p)
10 * (1/6) * (5/6)

################### Estaturas ########################
# n = 3 000 000
set.seed(26032020)
estaturas <- rnorm(n = 3000000, mean = 170, sd = 5)
head(estaturas)
summary(estaturas)
quantile(estaturas, seq(0, 1, 0.1))

hist(estaturas) # Frec abs :(

hist(estaturas, probability = T) #


hist(estaturas, probability = T, breaks = 1000) #

# Seleccionar una persona aleatoriamente
# P(X < 160) = P(X <= 160)
pnorm(160, mean = 170, sd = 5)

# P(X > 180)
1-pnorm(180, mean = 170, sd = 5)

# d: Calcular la función de prob (discr) o la func de densidad (continua)
# p: probabilidades acumuladas
# q: percentil
#r: simula

# Ejercicio : calcular el percentil 95
qnorm(0.95, mean = 170, sd = 5)


# SImulación de valores uniformes

runif(n = 1000000)
hist(runif(n = 100))

runif(n = 100, min = 10, max = 12)
hist(runif(n = 100, min = 10, max = 12))

# Aproximar el valor esperado
mean(runif(n = 100000, min = 10, max = 12))

# Simular tiempos de vida de unos enfermos de covid
# mu = 5

?rexp
lambda <- 1 / 5
n <- 5000
y <- rexp(n, lambda)
hist(y)

# Similar a 5
mean(y)
# P( X > 7)
1-pexp(7, lambda)
