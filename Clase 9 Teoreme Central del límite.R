# P(Y > 15)
#mu = 12, sigma = 1.5

aleatorio <- rnorm(n = 10000000, 12, 1.5)
mean(aleatorio)
hist(aleatorio)
table(aleatorio >= 15)
prop.table(table(aleatorio >= 15))

1-pnorm(15, 12, 1.5)

prop.table(table(aleatorio == 15))


# Exponencial con lambda = 1/24

pexp(12, rate = 1 / 24)


hist(rexp(n = 1000000, rate = 1/24))


y <- exp(rnorm(100))
hist(y)

hist(log(y))


# P(Y = 4)

dbinom(x = 4, size = 100, prob = 0.02)

# P(Y >= 4) = 1- P(Y <= 3)  

1-pbinom(q = 3, size = 100, prob = 0.02)



# Distribución hipergeometrirca
#N = 10, K = 4 (blancas) 
#n = 5, 
#P(X = 2) # 2 blancas en la muestra
(choose(4,2) * choose(6,3)) / choose(10, 5)
dhyper(x = 2, m = 4, n = 6, k = 5)

# P(X<=2)
phyper(q = 2, m = 4, n = 6, k = 5)

# 0.55

dpois(x = 3, lambda = 1.5)
(exp(-1.5) * (1.5^3)) /  factorial(3) 

# P(X > 3) = P(X >= 4)
1-ppois(q = 3, lambda = 1.5)
