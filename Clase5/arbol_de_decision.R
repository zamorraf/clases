#librerías utiliadas (puede que necesiten instalar una o más de estas librerías,
# en ese caso, utilicen install.packages)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(dplyr)

#cargue el archivo a una variable que se llame bcw usando la función read.csv
#usen el parámetro col.names para pasarle un vector con los nombres de las
#columnas. nombres para las columnas: Sample.number, Thickness, Uniformity.Size,
#Uniformity.Shape, Adhesion, Epithelial.Size, Nuclei, Chromatin, Nucleoli,
#Mitoses, Class
#ejemplo: col.names = c('nombre1', 'nombre2')
#usen el parámetro na.strings = '?' para que interprete los signos de pregunta
#como valores faltantes
bcw <- read.csv('datos/bcw.csv', header = F, col.names = c(
  'Sample.number', 'Thickness', 'Uniformity.Size', 'Uniformity.Shape',
  'Adhesion', 'Epithelial.Size', 'Nuclei', 'Chromatin', 'Nucleoli', 'Mitoses',
  'Class'), na.strings = '?')

#sobreescriban la columna Class con el factor de esa columna
bcw$Class <- factor(bcw$Class)

# Renmbrar la columna diagnosis por Class
#bcw <- bcw %>%
#      rename(Class = diagnosis)

#Utilice la función str() para ver la estructura del conjunto de datos:
str(bcw)
glimpse(bcw)


# utilicen la función table() para generar un resumen de las observaciones en
# bcw por la variable clase. Deberían ver 458 valores con clase = 2 y 241 con
# clase = 4
table(bcw$Class)

# utilicen la tabla generada en el paso anterior para generar un gráfico de
# barras usando la función barplot(). Recuerden incluir los parámetros main,
# xlab y ylab para agregar el título y las etiquetas.
barplot(table(bcw$Class), main = 'Distribución de las clases',
        ylab = 'Observaciones', xlab = 'Clase')


#usen la función set.seed para establecer la semilla con el valor 4161
set.seed(4161)

# las siguientes líneas de código van a crear un vector de valores lógicos este
# vector lo vamos a utilizar para dividir nuestro conjunto de datos original en
# dos: uno de entrenamiento para nuestro modelo y uno de prueba. la división se
# va a hacer con respecto a la columna Class, y vamos a dejar 70% de las
# observaciones en el de entrenamiento y 30% en el de prueba.

#paso 1: crear el vector lógico
splt <- sample.split(bcw$Class , SplitRatio = 0.7)

# paso 2: crear el data frame de entrenamiento usando los valores TRUE del
# vector splt solo las observaciones para las cuales el vector splt es
# verdadero, y todas las columnas.
bcw.entrenamiento <- bcw[splt,]

# paso 3: crear el data frame de prueba negando los valores de splt, para usar
# las observaciones que en el paso anterior eran falsas
bcw.prueba <- bcw[!splt,]


# Utilicen la función nrow() para demostrar que en total seguimos trabajando con
# 699 registros aunque ahora tengamos 2 datasets.
nrow(bcw.entrenamiento) + nrow(bcw.prueba)

table(bcw.entrenamiento$Class)

table(bcw.prueba$Class)

# Creen dos gráficos de barra usando barplot(), uno sobre bcw.entrenamiento y
# otro bcw.prueba para demostrar que se mantiene (o es similar) la proporción de
# clase = 2 y clase = 4 en los 2 datasets.

barplot(table(bcw.entrenamiento$Class),
        main = 'Distribución de las clases en bcw.entrenamiento',
        ylab = 'Observaciones', xlab = 'Clase')

barplot(table(bcw.prueba$Class),
        main = 'Distribución de las clases en bcw.prueba',
        ylab = 'Observaciones', xlab = 'Clase')


## Modelo

# crear el modelo (esto lo veremos en detalle luego, pero debería haber algunas
# partes de la sintaxis que ya entiendan)
modelo.arbol <- rpart(Class ~ .,
                      data = bcw.entrenamiento[,-which(colnames(bcw.entrenamiento) == "Sample.number")],
                      method =  'class')

# predecir utilizando el conjunto de datos de prueba
predicciones <- predict(modelo.arbol, newdata = bcw.prueba, type = 'prob')
predicciones
rpart.plot(modelo.arbol,
           shadow.col = "gray", #Agregar sombras
           main = "Clasificación cáncer de mama \n(Arbol de decisión)\n")


## Evaluacion

# Utilicen la función table() para comparar el resultado de las predicciones con
# el valor de la columna Class en el conjunto de datos de prueba
# ejemplo: table(vector1, vector2)
# el resultado les va a decir cuántas observaciones eran realmente 2 y fueron
# clasificadas como 2, y cuántas eran 4 y fueron clasificadas como 4
# también les va a decir cuántas eran 2 y fueron clasificadas como 4, y cuáles
# eran 4 y fueron clasificadas como 2

predicciones <- predict(modelo.arbol, newdata = bcw.prueba, type = 'class')
data <- table(bcw.prueba$Class, predicciones)

# Las filas son los reales y las columnas son los predecidos.
print(data)


## Prediccion ROC
prediccionesROC = prediction(c(predicciones), c(bcw.prueba[,'Class']))
as.numeric(performance(prediccionesROC, "auc")@y.values)

plot(performance(prediccionesROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = 'Curva ROC del modelo')

