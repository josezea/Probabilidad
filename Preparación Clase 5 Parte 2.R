
# Espacio muestral del lanzamiento de dos datos
expand.grid(1:6, 1:6)


# Espacio muestral del lanzamiento de tres dados:
Omega = expand.grid(1:6, 1:6, 1:6)

# A: El primer dado es primo
A = Omega[Omega$Var1 %in% c(2, 3, 5),]
nrow(A) # 108 eventos

# B: La suma de los tres dados es menor o igual a 4:
B = Omega[rowSums(Omega) <= 4,]

 C = Omega[Omega$Var1 %in% c(2, 3, 5) &
             rowSums(Omega) <= 4,]
 C

 
 # Ejemplo enfoque frecuentista

# Lanzamiento de un dado
# 100 veces
set.seed(12345)
resultado <- sample(1:6, 100, replace = T)
sum(resultado == 6) / length(resultado)

# 1000 veces
set.seed(12345)
resultado <- sample(1:6, 1000, replace = T)
sum(resultado == 6) / length(resultado)


# 1000000 veces
set.seed(12345)
resultado <- sample(1:6, 1000000, replace = T)
sum(resultado == 6) / length(resultado)


prob <- numeric(1000)
for(i in 1:1000){
  resultado <- sample(1:6, i, replace = T)
  prob[i] <- sum(resultado == 6) / length(resultado)
  
}

plot(1:1000, prob, type = "l")

# A: obtener en la suma pares
# B: multiplicación menor a 6

5 %% 2
5 %% 2 == 0
4 %% 2 == 0
13%% 7

Omega <- expand.grid(1:6, 1:6)
condicion1 <- (rowSums(Omega) %% 2) == 0
condicion2 <- Omega$Var1 * Omega$Var2 < 6 
A = Omega[condicion1, ]
B = Omega[condicion2, ]
AinterB = Omega[condicion1 & condicion2, ]
nrow(A)
nrow(B)
nrow(AinterB)
# P(A U B) = P(A) + P(B) + P(C)
nrow(A) / 36 + nrow(B) / 36 - nrow(AinterB) / 36



# Combinación:

N = 5
k = 3

choose(5, 3)


choose(31, 27)
choose(31, 4)

# Permutación
choose(31, 4) * factorial(4)
