
# Muestreo sin reemplazo orden si es relevante
choose(5, 3) * factorial(3)

U <- c("e1","e2", "e3", "e4", "e5")
# set.seed(12345)
indica <- sample(5, 3)
U[indica][sample(3,3)] # Desordenando las posiciones de la muestra

# Muestreo sin reemplazo orden no es relevante
choose(5, 3)
U <- c("e1","e2", "e3", "e4", "e5")
set.seed(12345)
indica <- sample(5, 3)
U[indica]

# Muestreo con reemplazo orden  es relevante: 5 ^3 casos posibles
5 ^3
casos <- expand.grid(U, U, U)
set.seed(12345)
casos[sample(125, 1),]


# Muestreo con reemplazo ordes es irrelevante
choose(5+3-1, 3)
set.seed(12345)
indica <- sample(5, 3, replace = T)
U[indica]
