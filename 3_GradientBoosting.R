source("./src/functions/librerias.R")
source("./src/functions/cruzada gbm binaria.R")
source("./src/functions/cruzada xgboost binaria.R")
source("./src/functions/genera_graficos.R")

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Carga de datos
dengue <- readRDS(file = "./data/processed/dengue.rds")

# 1. Tuneo gradient boosting -------------------------------------------------
## ---- chunk-gbtuneo1 ----
set.seed(12345)
# El shrinkage va desde los valores 0.0001 y 0.2 
# Cuanto mas alto mas rapido pero puede resultar menos fino
# n.minobsnode mide la complejidad
# interaction.depth = c(2) arboles binarios

gb_grid <- expand_grid(
    shrinkage = c(0.1, 0.05, 0.03, 0.01, 0.001),
    n.minobsinnode = c(5,10,20),
    n.trees = c(100, 500, 1000, 2000, 3000, 5000),
    interaction.depth = c(2))

control_gb <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

gbm <- train(
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
    humid + humid90, 
    data = dengue, 
    method = "gbm", 
    trControl = control_gb,
    tuneGrid = gb_grid, 
    distribution = "bernoulli", 
    bag.fraction = 1, 
    verbose = FALSE)

gbm
plot(gbm)
gbm$bestTune
## ---- chunk-gbtuneo2 ----
# Tuneado early stopping, se cambia bag.fraction por 0.5 en vez de 1
gbmgrid <- expand.grid(
    shrinkage = c(0.05), 
    n.minobsinnode = c(5),
    n.trees = c(50, 100, 300, 500, 800, 1000, 1200, 2000, 5000), 
    interaction.depth = c(2))

gbm2 <- train(
    factor(varObjBin) ~h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
        humid + humid90,
    data = dengue, 
    method = "gbm", 
    trControl = control_gb,
    tuneGrid = gbmgrid, 
    distribution = "bernoulli", 
    bag.fraction = 0.5, 
    verbose = FALSE)

gbm2$bestTune
plot(gbm2, xlab = "Boosting iterations")

# Importancia variables
# summary(gbm2)
tabla <- summary(gbm2)
par(cex=1.5, las =2)
barplot(tabla$rel.inf, names.arg = row.names(tabla), col = "#F5E1FD", 
        main = "Importancia de variables - gradient boosting")

## ---- chunk-gbvalidacion1 ----
# 2.Validacion cruzada repetida gradient -----------------------------------------
# Segun el grafico entre 2000 y 5000 no hay tanta diferencia, se deja en 3000
medias12 <- cruzadagbmbin(
    data = dengue, 
    vardep="varObjBin", 
    listconti=c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax",
    "temp", "humid", "humid90"), 
    listclass = c(""), 
    grupos = 4,
    sinicio = 1234,
    repe = 5, 
    n.minobsinnode = 5,
    shrinkage = 0.05,
    n.trees = 2000,
    interaction.depth = 2)

medias12$modelo <- "gbm"
## ---- chunk-gbmedias1 ----
# 2.1 Compara medias -------
genera_graficos(medias1, medias2, medias3, medias4, medias5,medias6, medias11,
                medias12)

## ---- chunk-tuneoxgboost ----
## 3. Tuneo Xgboost -----------
set.seed(12345) 
xgbmgrid <- expand.grid( 
    min_child_weight = c(5, 10, 20), 
    eta = c(0.1, 0.05, 0.03, 0.01, 0.001), 
    nrounds = c(100, 500, 1000, 5000), 
    max_depth = c(2, 3, 4, 5, 6), 
    gamma = 0, 
    colsample_bytree = 1, 
    subsample = 1)

control_xgboost <-trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

xgbm <- train(
    factor(varObjBin) ~h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
    humid + humid90,
    data = dengue, 
    method = "xgbTree",
    trControl = control_xgboost,
    tuneGrid = xgbmgrid,
    verbose = FALSE, 
    verbosity = 0)

xgbm
plot(xgbm)
xgbm$bestTune

## ---- chunk-tuneoxgboost1 ---
# Early stopping
# Con los parametros entregados anteriormente en best tune
# Con las variables ya preseleccionadas
xgbmgrid2 <- expand.grid(
    eta = c(0.1), 
    min_child_weight = c(20), 
    nrounds = c(50, 100, 150, 200, 250, 300, 500, 1000), 
    max_depth = 6, 
    gamma = 0, 
    colsample_bytree = 1, 
    subsample = 1)

set.seed(12345) 
control_xgboost1 <-trainControl(method = "repeatedcv", number = 4, repeats = 5,
     savePredictions = "all", classProbs = TRUE)

xgbm2 <- train(
    factor(varObjBin) ~h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
        humid + humid90,
    data = dengue, 
    method = "xgbTree", 
    trControl = control_xgboost1, 
    tuneGrid = xgbmgrid2, 
    verbose = FALSE)

plot(xgbm2)

# Se cambia la semilla para observar variaciones
set.seed(45673) 
control_xgboost2 <-trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

xgbm3 <- train(
    factor(varObjBin) ~h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp +
        humid + humid90,
    data = dengue, 
    method = "xgbTree", 
    trControl = control_xgboost2, 
    tuneGrid = xgbmgrid2, 
    verbose=FALSE)

xgbm3$bestTune
plot(xgbm3)

# Importancia de variables
varImp(xgbm3)
plot(varImp(xgbm3))

## ---- chunk-tuneoxgboost2 ---
# Solo con las variables seleccionadas en stepwise
xgbmgrid3 <- expand.grid(
    eta = c(0.1, 0.05, 0.03, 0.01, 0.001), 
    min_child_weight = c(5, 10, 20), 
    nrounds = c(50,100,150,200,250,300), 
    max_depth = c(2,3,4,5,6), 
    gamma = 0, 
    colsample_bytree = 1,
    subsample = 1)

set.seed(12345) 
xgbm4 <- train(
    factor(varObjBin) ~ h10pix + h10pix90 + Xmax + humid90 + 
    Ymax + temp90 + Xmin,
    data = dengue, 
    method = "xgbTree",
    trControl = control_xgboost, 
    tuneGrid = xgbmgrid3, 
    verbose = FALSE)

xgbm4$bestTune
plot(xgbm4)

## ---- chunk-validacionxgboost ---
# 4. Validacion cruzada rep xgboost ---------------------------------------

medias13 <-cruzadaxgbmbin(
    data = dengue, 
    vardep = "varObjBin", 
    listconti = c("h10pix","h10pix90","Xmax","humid90","Ymax","temp90","Xmin"), 
    listclass = c(""),
    grupos = 4, 
    sinicio = 1234, 
    repe = 5, 
    min_child_weight = 5, 
    eta = 0.10, 
    nrounds = 300, 
    max_depth = 6, 
    gamma = 0, 
    colsample_bytree = 1, 
    subsample = 1, 
    alpha = 0, 
    lambda = 0, 
    lambda_bias = 0)

medias13$modelo <- "xgbm"

## ---- chunk ---
# 5. Compara medias -------------------------------------------------------
## ---- chunk-mediasxgboost ---
genera_graficos(medias1, medias2, medias3, medias4, medias5,medias6, medias11,
                medias12, medias13)

