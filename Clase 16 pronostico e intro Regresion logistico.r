

library(TeachingSampling)
data(Lucy)
# Ingreso en término de los impuesto numero de empleados (no ayudaba)
# Level (Tamaño de empresa)

modelo <- lm(Income ~ Taxes + Level, data = Lucy )
summary(modelo)

datos <-data.frame(Taxes = c(3.5, 200, 70), 
                   Level =  c("Small", "Big", "Medium") )

head(predict(modelo, datos))
438.15 +  10.77 * 3.5 -198.08  
438.15 +  10.77 * 200   
438.15 +  10.77 * 70  - 15.04

# En la vida real se quieren hacer pronosticos con otros datos diferentes

## Objetivo: usar la regresión línear para predecir 
set.seed(12345)
indica_muestra <- sample(2396, round(2396 * 0.7 ))
train <- Lucy[indica_muestra,]
modelo_train <- lm(Income ~ Taxes + Level, data = train)
summary(modelo)

test <- Lucy[-indica_muestra,]
# Pronosticos por fuera de muestra
yhat <- predict(modelo_train, test)
mae <- mean(abs(test$Income - yhat))
mae 
summary(test$Income)

mse <-  mean((test$Income - yhat)^2)          
#RMSE
sqrt(mse)
plot(test$Income,yhat, pch = 20)

# Ejercicio 

# Ajustar un modelo de regresión lineal sobre una muestra de entranamiento 
# al ingreso en términos de
# Taxes (una función cuadrática), el timpo de empresa y la zona.

# y ~ x1 + I(x1^2)

#b. Validar en la muestra test que tan bueno son los pronosticos
#Calcular el mae y rmse y compararlo con el valor promedio del ingreso
# Mostrar grafico



# Nuevos datos
# predict(modelo, datosnuevos)


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
