library(caTools)
library(rpart)
library(randomForest)
library(neuralnet)
library(ROCR)
library(dplyr)

datos <- read.csv('datos/Chapter11DataSet_Training.csv', header = T, na.strings = '?')
glimpse(datos)

datos$Team_Value <- as.character(datos$Team_Value)
datos[datos$Team_Value != 'Superstar',]$Team_Value <- 0
datos[datos$Team_Value == 'Superstar',]$Team_Value <- 1
datos$Team_Value<- as.factor(datos$Team_Value)
splt <- sample.split(datos$Team_Value, SplitRatio = 0.7)
entrenamiento <- datos[splt, ]
prueba <- datos[!splt, ]


barplot(table(entrenamiento$Team_Value),
        main = 'Distribución de Valor para el Equipo',
        ylab = 'Observaciones',
        xlab = '¿Es Súper Estrella? (1 = Sí)')


boxplot(Career_TP ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Career_TP por Valor para el Equipo',
        ylab = 'Career_TP',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#La variable Career_PP se comporta de manera similar:
boxplot(Career_PP ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Career_PP por Valor para el Equipo',
        ylab = 'Career_PP',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#Y también la variable Career_Assists:
boxplot(Career_Assists ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Career_Assists por Valor para el Equipo',
        ylab = 'Career_Assists',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#Cuando se analizan las variables a nivel de la temporada anterior, el patrón
# se mantiene, aunque las diferencias son menores:
boxplot(Total_Points ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Total_Points por Valor para el Equipo',
        ylab = 'Total_Points',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#La variable Career_PP se comporta de manera similar:
boxplot(Personal_Points ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Personal_Points por Valor para el Equipo',
        ylab = 'Personal_Points',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#Y también la variable Career_Assists:
boxplot(Assists ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Assists por Valor para el Equipo',
        ylab = 'Assists',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

#Hay algunas variables que tienden a favorecer el no, como por ejemplo la
#cantidad de faltas la temporada anterior:
boxplot(Fouls ~ Team_Value,
        data = entrenamiento,
        main = 'Distribución de Fouls por Valor para el Equipo',
        ylab = 'Fouls',
        xlab = '¿Es Súper Estrella? (1 = Sí)')

# Modelo de Minería de Datos
# En este caso, se van a utilizar un bosque aleatorio
#crear modelo
set.seed(201908)

entrenamiento$Team_Value <- as.factor(entrenamiento$Team_Value)
modelo.bosque <- randomForest(Team_Value ~ Shots + Makes + Personal_Points +
                                Total_Points + Assists + Concessions+ Blocks,
                              data = entrenamiento )

#realizar predicciones
predicciones.bosque <- predict(modelo.bosque, newdata = prueba, type = 'class')
predicciones.bosque
