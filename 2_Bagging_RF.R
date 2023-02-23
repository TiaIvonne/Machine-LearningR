# Datos externos
source("./src/functions/librerias.R")
source("./src/functions/cruzada rf binaria.R")
source("./src/functions/genera_graficos.R")

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


# Carga de datos
dengue <- readRDS(file = "./data/processed/dengue.rds")

# 1. Tuneo Bagging -----------------------------------------------------------
## ---- chunk-bagging1 ----


rfgrid <- expand.grid(mtry = c(6))
set.seed(12345)
control_bagging <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

bagging <- train(
    data = dengue, 
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
    humid + humid90,
    method = "rf", 
    trControl = control_bagging, 
    tuneGrid = rfgrid, 
    linout = FALSE, 
    ntree=5000,
    sampsize = 200, 
    nodesize = 10, 
    replace = TRUE)

bagging$modelType

## ---- chunk-no ----
# 1.2 out of bag error --------------------------------------------------------
## ---- chunk-bagging2 ----
# Plotear
baggingbis <- randomForest(
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
    humid + humid90, 
    data = dengue, 
    mtry = 6, 
    ntree = 5000, 
    sampsize = 300, 
    nodesize = 10, 
    replace = TRUE)

#Se plotea la columna 1 que es OOB
plot(baggingbis$err.rate[, 1], main = "Out of bag error", ylab = "error")



# 1.3 Validacion cruzada rep bagging ------------------------------------------

## ---- chunk-bagging3 ----
# Con validacion cruzada y 4 grupos
medias6 <-cruzadarfbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""), 
    grupos = 4, 
    sinicio = 1234, 
    repe = 5,
    mtry = 6, 
    ntree =1000, 
    replace = TRUE)

medias6$modelo <- "bagging"

## ---- chunk-bagging4 ----
#  Anexo Manipulando tamanio muestral con 10 grupos no incluir en la seleccion
#  de modelos pues el grupos = es diferente
medias7 <-cruzadarfbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 10,
    sinicio = 1234,
    repe=20, 
    nodesize = 10,
    mtry = 6, 
    ntree = 3000, 
    replace = TRUE)

medias7$modelo <- "bagging_base"

# Bagging manipulando sampsize 1000
medias8 <-cruzadarfbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 10,
    sinicio = 1234,
    repe=20, 
    nodesize = 10,
    mtry = 6, 
    ntree = 3000, 
    replace = TRUE, 
    sampsize = 1000 )

medias8$modelo <- "bagging1000"

# Bagging manipulando sampsize 1250
medias9 <-cruzadarfbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 10,
    sinicio = 1234,
    repe=20, 
    nodesize = 10,
    mtry = 6, 
    ntree = 3000, 
    replace = TRUE, 
    sampsize = 1250 )

medias9$modelo <- "bagging1250"

# Bagging manipulando sampsize 1500
medias10 <-cruzadarfbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 10,
    sinicio = 1234,
    repe=20, 
    nodesize = 10,
    mtry = 6, 
    ntree = 3000, 
    replace = TRUE, 
    sampsize = 1500 )

medias10$modelo <- "bagging1500"

## ---- chunk-bagging5 ----
# genera graficos para comparar tamanio muestral de bagging
genera_graficos(medias6, medias7, medias8, medias9, medias10)

## ---- chunk-bagging6 ----
# genera graficos para comparar tamanio muestral de bagging
genera_graficos(medias1, medias2, medias3, medias4, medias5, medias6)

## ---- chunk-rf1 ----
# 2. Tuneo random forest -----------------------------------------------------
set.seed(12345)
rfgrid_rf <- expand.grid(mtry = c(3, 4, 5, 6, 7, 8, 9, 10, 11))

control_rf <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

random_forest <- train(
    factor(varObjBin) ~.,
    data = dengue,
    method = "rf",
    trControl = control_rf,
    tuneGrid = rfgrid_rf,
    linout = FALSE,
    ntree =300,
    nodesize = 10,
    replace = TRUE,
    importance = TRUE)

# Mejor valor de mtry
random_forest$bestTune$mtry
## ---- chunk-rf1bis ----
final <- random_forest$finalModel
tabla <- as.data.frame(importance(final))
tabla <- tabla[order(-tabla$MeanDecreaseAccuracy),]
barplot(tabla$MeanDecreaseAccuracy, names.arg =rownames(tabla), col = "#e7e1fd",
    main = "Importancia de variables - random forest", las = 2)

## ---- chunk-rf2 ----
# random forest solo usando las variables mas relevantes
rfgrid_rf2 <- expand.grid(mtry = c(3, 4, 5, 6))
modellist <- list()
#train with different ntree parameters
for (ntree in c(300,500,1000,1500,2000,2500)){
    set.seed(12345)
    fit <- train(
        factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + 
        temp + humid + humid90,
        data = dengue,
        method = "rf",
        trControl = control_rf,
        tuneGrid = rfgrid_rf2,
        lineout = FALSE,
        ntree = ntree,
        nodesize = 10,
        replace = TRUE,
        importance = TRUE)
    key <- toString(ntree)
    modellist[[key]] <- fit
}

results <- resamples(modellist)
dotplot(results)
## ---- chunk-rf3 ----
# Se escoge el mejor mtry para el modelo anteriormente propuesto
fit$bestTune


# 2.2 Validacion cruzada rep RF -----------------------------------------------
## ---- chunk-rf4 ----
# Probar a dejar el parametro sampsize sin valor para que lo haga x defecto
medias11 <- cruzadarfbin(data = dengue, vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax",
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234,
    repe = 10,
    nodesize = 10,
    mtry = 4, ntree = 500, replace = TRUE)

medias11$modelo <- "randomforest"
## ---- chunk-rf5 ----
# 3. Compara medias ----------------------------------------------------------
genera_graficos(medias1, medias2, medias3, medias4, medias5,medias6,medias11)

## ---- chunk-rf6 ----
## Es para sacar la matriz de confusion
set.seed(12345)
control_rf1 <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = "all",
    classProbs = TRUE)

rfgrid_rf3 <- expand.grid(mtry = 4)
final_randomForest <- train(
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + 
    temp + humid + humid90,
    data = dengue,
    method = "rf",
    trControl = control_rf1,
    tuneGrid = rfgrid_rf3,
    lineout = FALSE,
    ntree = 500,
    nodesize = 10,
    replace = TRUE,
    importance = TRUE)

## ---- chunk-rf7 ----
# Matriz de confusion
salida_rf <- final_randomForest$pred
salidarf_conf <- confusionMatrix(salida_rf$pred, salida_rf$obs)
fourfoldplot(salidarf_conf$table)

## ---- chunk-rf8 ----
confusionMatrix(salida_rf$pred, salida_rf$obs, mode = "everything")


## ---- chunk-rf9 ----
# Curva roc
curvaroc <- roc(salida_rf$obs, salida_rf$Yes)
auc <- curvaroc$auc
plot.roc(roc(response = salida_rf$obs, predictor = salida_rf$Yes), 
    main = "Curva ROC Random Forest Classifier")
