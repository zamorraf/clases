library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)


glass <- read.csv("datos/glass.data",
                  col.names=c("id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")) %>%
  select (-id)

###Standardize the Data

#Es ideal para estandarizar las características de los datos, especialmente con el algoritmo KNN. Vamos a seguir adelante y estandarizar. Aquí estamos usando `scale ()` para
#estandarizar las columnas de características de `glass` y asignarlas a una nueva variable. Excluya la columna de destino `Tipo` mientras escala.


standard.features <- scale(glass[,1:9])

#Une los datos estandarizados con la columna de destino
data <- cbind(standard.features,glass[10])
#Verifique si faltan valores para imputar.
anyNA(data)
# Parece que los datos están libres de NA
head(data)

data$Type<- as.factor(glass$Type)

###Visualización de datos
#La siguiente gráfica explica la relación entre las diferentes características en el conjunto de datos glass.


corrplot(cor(data[,c(1,2,3,4,5,6,7,8,9)]))

# correlacion entre RI y CA es fuerte, por lo tanto si podemos quitar una.


### División de datos de prueba y entrenamiento

#Utilizamos `caTools ()` para dividir los conjuntos de datos `data` en` train` y `test` con un` SplitRatio` = 0.70.



set.seed(123)

splt <- sample.split(data$Type,SplitRatio = 0.7)

train <- data[splt,]

test <- data[!splt,]


###KNN Model
#Usamos `knn ()` para predecir nuestra variable objetivo `Tipo` del conjunto de datos de prueba con` k = 1`.

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,test$Type)


Los resultados anteriores revelan que nuestro modelo logró una precisión de
** 'r (1 error) * 100'% **. Vamos a probar diferentes valores de 'k' y evaluar nuestro modelo.



predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)

}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))



###Elección del valor K por visualización

#Vamos a trazar `error.type` vs` k` usando `ggplot`.


ggplot(knn.error,aes(k,error.type))+
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks=1:10)+
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


#La gráfica anterior revela que el error es más bajo cuando `k = 3` y luego salta hacia atrás revelando que` k = 3` es el valor óptimo. Ahora construyamos nuestro
#modelo usando `k = 3` y evaluarlo.

###Result

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,test$Type)
```
El modelo anterior nos dio una precisión de **`r (1-error)*100` %.**
