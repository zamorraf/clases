library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(dplyr)

bcw <- read.csv("datos/bcw.csv")
glimpse (bcw)

bcw <- bcw %>%
      rename(Class = diagnosis)

glimpse (bcw)
summary(bcw$)
dim(bcw)

table(bcw$Class)
barplot(table(bcw$Class), main = 'Distribución de las clases',
        ylab = 'Observaciones', xlab = 'Clase')

set.seed(4161)
splt <- sample.split(bcw$Class , SplitRatio = 0.7)

splt

bcw.entrenamiento <- bcw[splt,]
bcw.prueba <- bcw[!splt,]
