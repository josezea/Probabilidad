##instalar paquertes y libreri para importar datos
#install.packages("readr")
##Llamar la libreria 
library(readr)
## Una vez desde la pesta?a sesion se haya definido la ruta de trabajo se llama el archivo
datos<- read.csv2("variables_adicionales_hogar_v3.txt")
head(datos)
datos$ESTRATO_VIV
##*************** ANALISIS DE FRECUENCIAS ********************************####
table_bar<-table(datos$ESTRATO_VIV)
barplot(table_bar)

# frecuencias relativas
fr_relativa<-prop.table(table_bar)
barplot(fr_relativa)

#dataframe 

resultados_data <- data.frame(table_bar)
names(resultados_data) <- c("Estrato_vivienda","Frecuencia_absoluta")
#frecuencia relativa en el dataframe
resultados_data$frecuencia_relativa <- resultados_data$Frecuencia_absoluta/sum(resultados_data$Frecuencia_absoluta)
#frecuencia acumulada en el dataframe
resultados_data$frecuencia_acumulada <-cumsum(c(resultados_data$frecuencia_relativa))

  
plot(resultados_data$Estrato_vivienda,resultados_data$frecuencia_acumulada)

##**************informacion cuantitativa:::: Medidas de tendencia central###############

summary(datos$INGRESOS_HOG)
class(datos$INGRESOS_HOG) #Factor =  enteros que tomo como caracteres

datos$INGRESOS_HOG <- as.numeric(as.character(datos$INGRESOS_HOG))
hist(datos$INGRESOS_HOG,xlim = c(0,10000000),nclass = 10000)
mean(datos$INGRESOS_HOG,trim = 0.05)
median(datos$INGRESOS_HOG)


