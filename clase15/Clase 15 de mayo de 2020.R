library(TeachingSampling)
data(Lucy)

plot(Lucy$Taxes ~ Lucy$Income)
?lm
# Y ~ X
# Y es ajustado por X: Y ~ X1 + X1, Y = Beta_0 + Beta_1 X
# Y ~ X1 + X1, Y = Beta_0 + Beta_1 X1 + Beta_2 X2
# y: Ingreso, X: Impuestos
################# Modelos univariados (sólo una variable explicativa) ##############
modelo <- lm(Income ~ Taxes, data = Lucy)
modelo
summary(modelo)

plot(Lucy$Taxes,  Lucy$Income)
abline(modelo)
# modelo 2 Y: Income, X = EMployees

modelo2 <- lm(Income ~ Employees, data = Lucy)
summary(modelo2)
plot(Lucy$Employees,  Lucy$Income)
abline(modelo2)
MSE_m2 <- mean(modelo2$residuals^2)
MSE_m2

modelo3 <- lm(Income ~ Taxes + Employees , data = Lucy)
summary(modelo3)
plot(Lucy$Employees,  Lucy$Income)
abline(modelo2)
MSE_m3 <- mean(modelo3$residuals^2)
MSE_m3


################ Modelo 4: Modelo polinómico ###################

modelo4 <- lm(Income ~ Taxes + I(Taxes^2)  , data = Lucy)
summary(modelo4)
plot(Lucy$Employees,  Lucy$Income)
head(modelo4$fitted.values)
head(predict(modelo4))
head(Lucy$Income) # ygorro
head(Lucy$Income - predict(modelo4)) # errores
hist(Lucy$Income - predict(modelo4))
216.56462  + 19.91981 * 3 - -0.05125  * 9
plot(Lucy$Income, modelo4$fitted.values)
abline(a = 0, b = 1)
MSE_m4 <- mean(modelo4$residuals^2)

################ Modelo 5: Incluir variables categóricas ###################
Lucy$Medium <- ifelse(Lucy$Level == "Medium", 1, 0)
Lucy$Small <- ifelse(Lucy$Level == "Small", 1, 0)
modelo5 <- lm(Income ~ 1 + Taxes + I(Taxes^2)  + Medium + Small , data = Lucy)
summary(modelo5)

modelo5 <- lm(Income ~ Taxes + I(Taxes^2) +  factor(Level) , data = Lucy)
summary(modelo5)
MSE_m5 <- mean(modelo5$residuals^2)

predict(modelo5, data.frame(Taxes = 3.5, Level = "Medium"))


################# Regresión logistica ######################
data(iris)
iris
datos <- iris[iris$Species != "setosa",]
datos$y <- ifelse(datos$Species == "versicolor", 1, 0)
modelorg <- glm(y ~  Sepal.Length,  data = datos, family = "binomial")
summary(modelorg)

datos$yhat <- predict(modelorg, type = "response")
table(datos$Species == "versicolor", datos$yhat > 0.5)
