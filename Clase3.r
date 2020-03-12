library(TeachingSampling)
library(moments)
data(Lucy) # Empresas
summary(Lucy)

hist(Lucy$Income)
skewness(Lucy$Income) 
skewness # Se puede ver como está programad

skewness(Lucy$Employees)
hist(Lucy$Employees)
#hist(log(Lucy$Employees))
# Asimetría Bowley - Yule

skewness_by <- function(x){
  iqr <- quantile(x, 0.75) - quantile(x, 0.25)
  num <- (quantile(x, 0.75) - quantile(x, 0.5))-
         (quantile(x, 0.5) - quantile(x, 0.25))
  num / iqr
}
skewness_by(Lucy$Income)
boxplot(Lucy$Income)

# Simular 1 millon de estaturas
mu <- 170 # cms
sigma <- 10 # cms
set.seed(20022020)
y <- rnorm(n = 1000000, mean = mu, sd = sigma)
head(y)
hist(y)
sessionInfo()
kurtosis(y) # Mesocúrtica
kurtosis(Lucy$Income)
set.seed(20022020)
y <- runif(n = 1000000, min = 5000,
           max = 50000)
hist(y)
kurtosis(y)


# Correlación 
?cor
cor(Lucy$Income, Lucy$Taxes)
plot(Lucy$Income, Lucy$Taxes)
abline(lm(Taxes ~ Income, data = Lucy))

# Tablas de contengencia
table(Lucy$Zone, Lucy$Level)
#rowSums(table(Lucy$Zone, Lucy$Level)) # ni.
#colSums(table(Lucy$Zone, Lucy$Level)) # ni.

table(Lucy$Zone, Lucy$Level)
# Perfil fila 
#sum(prop.table(table(Lucy$Zone, Lucy$Level)))
prop.table(table(Lucy$Zone, Lucy$Level), 1)
barplot(t(prop.table(table(Lucy$Zone,
                           Lucy$Level), 1)))
chisq.test(Lucy$Zone, Lucy$Level)
barplot(t(prop.table(table(Lucy$Zone,
                           Lucy$Level), 1)),
        beside = T)
        
