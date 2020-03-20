# Concepto de variable aleatoria
# X: número de hermanos, exper: seleccionar un individuo de una pob
# Calcular el valor esperada
# Generar valores aleatorias con la siguiente función de probabilidad:
# X    0       1     2      3     4      5
# P(X) 0.15   0.53  0.2    0.08  0.03  0.01
# Variable aleatoria discreta
aleatorio <- sample(0:5, size = 1000000, replace = T, 
             prob = c(0.15, 0.53, 0.2, 0.08,  0.03, 0.01))
prop.table(table(aleatorio))


X <- 0:5
P_X <- c(0.15, 0.53, 0.2, 0.08, 0.03, 0.01)
E_X <- sum(X * P_X) # Valor esperado (esperanza matemática)
plot(X, P_X, type = "h")

# Se puede aproximar con la simulación:
mean(aleatorio) 

# Calcular la varianza
sum(((X - E_X)^2) * P_X) # Varianza de la distribución

# Se puede aproximar con la simulación:
var(aleatorio)


