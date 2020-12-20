

#  Maquinas de Vectores de soporte-- Regresión 

# YERLIN RONIEL BERIGUETE


### Eliminar todos los objetos

rm(list=ls())

###

install.packages("e1071") #Paquete que contiene la función svm.

install.packages("caret") #Paquete que nos permite hacer divisiones en los datos

install.packages("Metrics")

# Cargar librerias

library(e1071)
library(caret)
library(Metrics)



# Importar datos

boston <- MASS::Boston


# Ver la estructura

class(boston)
str(boston)
View(boston)
dim(boston)
head(boston)

#Establecemos la semilla 123

set.seed(123) 

# Dividimos los datos de entrenamiento y prueba 

indexes <- createDataPartition(boston$medv, p = .7, list = F)

print(indexes)

datosEntrenamiento <- boston[indexes, ]

dim(datosEntrenamiento)
View(datosEntrenamiento)

datosPrueba <- boston[-indexes, ]

dim(datosPrueba)


# Implementamos la funcion svm

modelSVM_reg_linear <- svm(medv~.,kernel="linear", data=datosEntrenamiento)

modelSVM_reg_radial <- svm(medv~.,kernel="radial", data=datosEntrenamiento)
modelSVM_reg_polynomial <- svm(medv~.,kernel="polynomial", data=datosEntrenamiento,cost=1)
modelSVM_reg_sigmoid <- svm(medv~.,kernel="sigmoid", data=datosEntrenamiento)

?svm()

print(modelSVM_reg_linear)

print(modelSVM_reg_radial)
print(modelSVM_reg_polynomial)
print(modelSVM_reg_sigmoid)


summary(modelSVM_reg)

# Prediciones del modelo
pred_linear = predict(modelSVM_reg_linear, datosPrueba)

pred_radial = predict(modelSVM_reg_radial, datosPrueba)
pred_polynomial = predict(modelSVM_reg_polynomial, datosPrueba)
pred_sigmoid = predict(modelSVM_reg_sigmoid, datosPrueba)

print(pred_linear)
summary(pred_linear)

x=1:length(datosPrueba$medv)

x

## Graficamos 
plot(x, datosPrueba$medv, pch=18, col="red")
lines(x, pred_linear, lwd="2", col="blue")

plot(x, datosPrueba$medv, pch=18, col="red")
lines(x, pred_radial, lwd="2", col="blue")

plot(x, datosPrueba$medv, pch=18, col="red")
lines(x, pred_polynomial, lwd="2", col="blue")

plot(x, datosPrueba$medv, pch=18, col="red")
lines(x, pred_sigmoid, lwd="2", col="blue")



# Comprobación de precisión

cat("Comprobar precision linear", "\n")
comprobarPrecision(datosPrueba$medv,pred_linear)

cat("Comprobar precision radial", "\n")
comprobarPrecision(datosPrueba$medv,pred_radial)

cat("Comprobar precision polynomial", "\n")
comprobarPrecision(datosPrueba$medv,pred_polynomial)


cat("Comprobar precision sigmoid", "\n")
comprobarPrecision(datosPrueba$medv,pred_sigmoid)

# Description: Función que muestra los valores de precision de los modelos segun los valores predecidos
# Autor: Yerlin Beriguete
# Date:  5/9/2020

comprobarPrecision<-function(valorActual, valorPredecido){
  
  
  mse = mse(valorActual,valorPredecido)#Calcular El Error Cuadrático Medio De Un Estimador
  mae = MAE(valorActual, valorPredecido)#Error Absoluto Medio
  rmse = RMSE(valorActual, valorPredecido) #Error Cuadrático Medio  #rmse(actual, predicted)
  r2 = R2(valorActual, valorPredecido, form = "traditional")
  
  
  cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
      "RMSE:", rmse, "\n", "R-squared:", r2)
  
  
}







