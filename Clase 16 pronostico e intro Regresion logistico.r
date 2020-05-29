
################# regresión logística ###########################
 # Variables de unos y ceros
data(iris)

# Quitar la especie virgina
datos <- iris[iris$Species != "virginica",]
table(datos$Species)
datos$y <- ifelse(datos$Species == "setosa", 1, 0)
table(datos$y)

# X1: Sepal.Length

modelo_log <- glm(y ~ Sepal.Length, family = "binomial", data = datos)
summary(modelo_log)
datos$prob <-  modelo_log$fitted.values
datos$pronostico <- ifelse(datos$prob >= 0.5, "setosa", "versicolor") 
datos$pronostico2 <- ifelse(datos$prob >= 0.3, "setosa", "versicolor") 

table(datos$Species, datos$pronostico)
table(datos$Species, datos$pronostico2)
