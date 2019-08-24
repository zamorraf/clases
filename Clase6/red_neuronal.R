#Cargar las librerías necesarias
library(lattice)
library(caTools)
library(ROCR)
library(neuralnet)
library(dplyr)

#Cargar el archivo a una variable que se llame crx usando la función read.csv
crx=read.csv(file = "datos/crx.data.txt",head=FALSE,sep=",",na.strings = '?')

crx$V1 <- factor(crx$V1)
crx$V4 <- factor(crx$V4)
crx$V5 <- factor(crx$V5)
crx$V6 <- factor(crx$V6)
crx$V7 <- factor(crx$V7)
crx$V10 <- factor(crx$V10)
crx$V12 <- factor(crx$V12)
crx$V13 <- factor(crx$V13)

crx$V16 <- as.character(crx$V16)
crx[crx$V16 == '+',]$V16 <- 'yes'
crx[crx$V16 == '-',]$V16 <- 'no'
crx$V16 <- factor(crx$V16)

#La estructura del conjunto de datos:
str(crx)
glimpse(crx)

#Dividir el conjunto de datos en uno de entrenamiento y otro de pruebas:
set.seed(5768)
splt <- sample.split(crx$V16, SplitRatio = 0.7)
crx.entrenamiento <- crx[splt, ]
crx.prueba <- crx[!splt, ]


boxplot(crx.entrenamiento$V3 ~ crx.entrenamiento$V16,
        main = 'Distribuciones de V3',
        ylab = 'V3',
        xlab = 'Tarjeta aceptada')


boxplot(log(crx.entrenamiento$V8+1) ~ crx.entrenamiento$V16,
        main = 'Distribuciones de V8',
        ylab = 'V8',
        xlab = 'Tarjeta aceptada')

boxplot(crx.entrenamiento$V11 ~ crx.entrenamiento$V16,
        main = 'Distribuciones de V11',
        ylab = 'V11',
        xlab = 'Tarjeta aceptada')



mosaicplot(~crx.entrenamiento$V16 + crx.entrenamiento$V1,main="Proporción de aprobaciones por la variable v1",ylab="V1",xlab="¿Se aprobó?")

mosaicplot(~crx.entrenamiento$V16 + crx.entrenamiento$V9,main="Proporción de aprobaciones por la variable v9",ylab="V9",xlab="¿Se aprobó?")

mosaicplot(~crx.entrenamiento$V16 + crx.entrenamiento$V10,main="Proporción de aprobaciones por la variable v10",ylab="V10",xlab="¿Se aprobó?")

mosaicplot(~crx.entrenamiento$V16 + crx.entrenamiento$V12,main="Proporción de aprobaciones por la variable v12",ylab="V12",xlab="¿Se aprobó?")

mosaicplot(~crx.entrenamiento$V16 + crx.entrenamiento$V13,main="Proporción de aprobaciones por la variable v13",ylab="V16",xlab="¿Se aprobó?")


#**Modelo de Minería de Datos**
# Para modelar este caso, se va a utilizar una regresión logística, en el primer modelo vamos a utilizar las variables V1 + V2 + V3 + V4 + V5 + V6 + V7+ V8+ V9+ V10+ V11+ V12+ V13+ V14+ V15:

m <- model.matrix(   ~V16+ V1 + V2 + V3 + V4 + V5 + V6 + V7+ V8+ V9+ V10+ V11+ V12+ V13+ V14+ V15 ,
  data = crx.entrenamiento )

crx.fit <- neuralnet(V16yes ~ V1b+V2 ,data = m,hidden =5,rep=1,linear.output=T)

#Al ver los detalles del modelo 1:

plot(crx.fit,rep="best")


#**Evaluación**

  ```{r}
mp <- model.matrix(
  ~V16+ V1 + V2 + V3 + V4 + V5 + V6 + V7+ V8+ V9+ V10+ V11+ V12+ V13+ V14+ V15 ,
  data = crx.prueba
)

predicciones.red <- neuralnet::compute(crx.fit,mp[,c("V1b","V2")])

results <- data.frame(actual = mp, prediction = predicciones.red$net.result)
results

predicciones.redClass=ifelse(predicciones.red$net.result>=0.5,1,0)
predicciones.redClass

