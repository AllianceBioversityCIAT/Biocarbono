# ======================================================================================================================== #
# Propósito: clasificar coberturas de la tierra usando Random Forest
# ======================================================================================================================== #


# ======================================================================================================================== #
#  Limpieza de ambiente
# ======================================================================================================================== #
rm(list = ls())



# ======================================================================================================================== #
#  Carga de librerías
# ======================================================================================================================== #
pack <- c("raster","AppliedPredictiveModeling","rgdal",
          "maptools","caret","sp","randomForest","e1071",
          "readr","tidyverse","outliers","dplyr","class",
          "DescTools")

lapply(pack, require, character.only=TRUE)



# ======================================================================================================================== #
#  Datos de entrada
# ======================================================================================================================== #
# Ruta madre
path <- 'T:/.../script'
setwd(path)
rasterOptions(tmpdir = path)

# Ruta del raster
img <- raster::stack("T:/.../raster.tif")
# Cambio de nombre de las bandas. El número varía de acuerdo a las bandas de la imagen (1:n)
names(img) <- paste0('band_',1:50)
# datos de entrenamiento en formato csv
# NOTA: Los datos de entrenamiento deben tener una columna llamada "cob", que debe tener las coberturas a mapear,
datos_csv <- read.csv("T:/.../entrenamiento.csv")



# ======================================================================================================================== #
#  Datos de entrada
# ======================================================================================================================== #
# Marco de datos de entrenamiento 
datos_csv <- as.data.frame(datos_csv)
# Columna objetivo (coberturas a mapear)
datos_csv$cob <- as.factor(datos_csv$cob)
# visualización de datos
head(datos_csv)



# ======================================================================================================================== #
#  cantidad de puntos a trabajar por cobertura
# ======================================================================================================================== #
cob_1 <- datos_csv[datos_csv$cob == 1, ] #cobertura1
cob_1 <- cob_1 %>% sample_n(10000) #cantidad de muestras
cob_2 <- datos_csv[datos_csv$cob == 2, ] #cobertura2
cob_2 <- cob_2%>% sample_n(10000) #cantidad de muestras
cob_3 <- datos_csv[datos_csv$cob == 3, ] #cobertura3
cob_3 <- cob_3%>% sample_n(10000) #cantidad de muestras
cob_4 <- datos_csv[datos_csv$cob == 4, ] #cobertura4
cob_4 <- cob_4 %>% sample_n(10000) #cantidad de muestras
cob_5 <- datos_csv[datos_csv$cob == 5, ] #cobertura5
cob_5 <- cob_5 %>% sample_n(10000) #cantidad de muestras
cob_6 <- datos_csv[datos_csv$cob == 6, ] #cobertura6
cob_6 <- cob_6 %>% sample_n(10000) #cantidad de muestras
cob_7 <- datos_csv[datos_csv$cob == 7, ] #cobertura7
cob_7 <- cob_7 %>% sample_n(10000) #cantidad de muestras
cob_8 <- datos_csv[datos_csv$cob == 8, ] #cobertura8
cob_8 <- cob_8 %>% sample_n(10000) #cantidad de muestras
cob_9 <- datos_csv[datos_csv$cob == 9, ] #cobertura9
cob_9 <- cob_9 %>% sample_n(10000) #cantidad de muestras
cob_10 <- datos_csv[datos_csv$cob == 10, ] #cobertura10
cob_10 <- cob_10 %>% sample_n(10000) #cantidad de muestras
cob_11 <- datos_csv[datos_csv$cob == 11, ] #cobertura11
cob_11 <- cob_11 %>% sample_n(10000) #cantidad de muestras


# ======================================================================================================================== #
# Union de puntos de entrenamiento
# ======================================================================================================================== #
datos_csv <- rbind(cob_1,cob_2,cob_3,cob_4,cob_5,cob_6,cob_7,cob_8,cob_9,cob_10,cob11)



# ======================================================================================================================== #
# Preparación de la información de entrenamiento
# ======================================================================================================================== #
seed <- set.seed(998)
# separación de datos en entrenamiento y evaluación (70% entrenamiento / 30% evaluación)
inTraining <- createDataPartition(datos_csv$cob, p = .70, list = FALSE)

# Entrenamiento
train <- datos_csv[inTraining,]
# Evaluación
test  <- datos_csv[-inTraining,]

# Variables de entrenamiento
train_variables <- train[,4:53] #Estos números varían de acuerdo con la cantidad de variables
# Coberturas de entrenamiento
train_target<- train$cob

# Variables de evaluación
test_variables <- test[,4:53] #Estos números varían de acuerdo con la cantidad de variables
# Coberturas de evaluación
test_target <- test$cob



# ======================================================================================================================== #
# Visualización de la información
# ======================================================================================================================== #
# Cajas
featurePlot(x = train_variables[,1:50], 
            y = as.factor(train_target), 
            plot = "box",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            ## Add a key at the top
            auto.key = list(columns = 9),
            main = "Visualización de variables ")

# Densidad
featurePlot(x = train_variables[,1:50], 
            y = as.factor(train_target), 
            plot = "density",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            ## Add a key at the top
            main = "Visualización de variables",
            auto.key = list(columns = 9))



# ======================================================================================================================== #
# Selección de variables: Recursive Feature Elimination (RFE)
# ======================================================================================================================== #
set.seed(123)
# Librerías
library(mlbench)
library(caret)

# Control
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
# Método RFE
result <- rfe(train_variables, train_target, rfeControl = control)
#Predictores seleccionados
predictors_seleted <- predictors(result)
variables_selected <- predictors_seleted

cobtrain <- as.data.frame(train$cob)
cobtest <- as.data.frame(test$cob)
train_selected <- train%>%select(predictors_seleted)
test_selected <- test%>%select(predictors_seleted)
train <- cbind(cobtrain, train_selected)
test <- cbind(cobtest, test_selected)
colnames(train)[1] <- "cob"
colnames(test)[1] <- "cob"
dim(train)
dim(test)



# ======================================================================================================================== #
# Ajuste del modelo Random Forest: Librería caret
# ======================================================================================================================== #
# Control de validacíon cruzada
control_c <- trainControl(method = "cv", number = 2, search = "grid", verboseIter = T)
# Hiperparámetros a evaluar
tunegrid <- expand.grid(mtry = c(2, 5, 10, 20),
                        splitrule = "gini",
                        min.node.size = c(2, 5, 10))
# Lista vacia para guardar modelos
modellist <- list()

# Prueba de todos los hiperparámetros
for(i in c(250,500, 1000)){ #100 y 500 árboles a evaluar
  set.seed(123)
  print(paste("Cantidad de árboles: ",i))
  model <- caret::train(cob~., data = train, method = "ranger",
                        metric = "Accuracy", tuneGrid = tunegrid, trControl = control_c, 
                        num.trees= i,
                        importance = "impurity")
  key <- toString(i)
  modellist[[key]] <- model
  
}

# Gráficos y tablas del ajuste
plot(modellist$`100`) # Gráfico con cantidad #1 de árboles evaluados
plot(modellist$`500`) # Gráfico con cantidad #2 de árboles evaluados
results <- resamples(modellist)
summary(results) #Accuracy y Kappa




# ======================================================================================================================== #
# Modelo Random Forest con hiper parámetros ajustados
# ======================================================================================================================== #
ntree_ajustado <- 250
mtry_ajustado <- 10
node_ajustado <- 2


rf <- randomForest(x = train[,2:dim(train)[2]],
                    y = factor(train$cob),
                    xtest = test[,2:dim(test)[2]],
                    ytest = factor(test$cob),
                    mtry = mtry_ajustado,
                    ntree = ntree_ajustado,
                    node = node_ajustado,
                    importance = TRUE,
                    confusionMatrix = TRUE,
                    do.trace = TRUE,
                    verbose = FALSE,
                    keep.forest = TRUE)

print(rf)



# ======================================================================================================================== #
# Exportar el mapeo
# ======================================================================================================================== #
# NOTA:En las comillas poner la ruta donde va a guardar el resultado
map <- raster::predict(img[[predictors_seleted]], rf, "T:/.../resultado.tif", progress='text', type = "response", index = 1,
                       na.rm = TRUE, overwrite = TRUE)


