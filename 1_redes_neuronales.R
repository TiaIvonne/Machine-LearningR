source("./src/functions/librerias.R")
source("./src/functions/genera_graficos.R")
source("./src/functions/cruzadas avnnet y log binaria.R")

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)



# Carga de datos
dengue <- readRDS(file = "./data/processed/dengue.rds")


# 1. Tuneo -------------------------------------------------------------------
## ---- chunk-redes1 ----
set.seed(12345)
control_redes <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

avnnetgrid <- expand.grid(
    size = c(5, 10, 15, 20),
    decay = c(0.01, 0.1, 0.001), bag = FALSE)

# Con las variables seleccionadas stepwiseAIC
redavnnet1 <- train(
    varObjBin ~ h10pix + temp90 + trees + trees90 + Ymin + Ymax + temp + 
    humid + humid90,
    data = dengue,
    method = "avNNet", 
    linout = FALSE, 
    maxit = 100,
    trControl = control_redes, 
    tuneGrid = avnnetgrid,
    repeats = 5,
    trace = F)

knitr::kable(redavnnet1$results, "pipe")

## ---- chunk-redes2 ----
# Con las variables seleccionadas BIC
redavnnet2 <- train(
    varObjBin ~ h10pix + temp90 ,
    data = data,
    method = "avNNet", linout = FALSE, maxit = 100,
    trControl = control_redes, tuneGrid = avnnetgrid,
    repeats = 5,
    trace = F)

knitr::kable(redavnnet2$results, "pipe")


## ---- chunk-no ----
# *****************************
# TUNEO MÁS COMPLETO CON MAXIT
# *****************************

## ---- chunk-redes3 ----
listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90")

data2 <- dengue[,c(listconti, vardep)]

control_maxit <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
        savePredictions = "all", classProbs = TRUE) 

set.seed(12345)
nnetgrid <-  expand.grid(size = c(5, 10), decay = c(0.01, 0.1, 0.001), bag = F)

completo <-data.frame()
listaiter<-c(50, 100, 200, 500, 1000, 2000, 3000)

for( iter in listaiter) 
{
    rednnet<- train(
        varObjBin~.,
        data = data2,
        method = "avNNet",
        linout = FALSE,
        maxit = iter,
        trControl = control_redes, 
        repeats = 5, 
        tuneGrid = nnetgrid,
        trace = F)
    # Añado la columna del parametro de iteraciones
    rednnet$results$itera <- iter
    # Voy incorporando los resultados a completo
    completo <- rbind(completo, rednnet$results)
}

completo <- completo[order(completo$Accuracy),]

ggplot(completo, aes(x = factor(itera), y = Accuracy, 
    color = factor(decay), pch = factor(size))) +
    theme_minimal() + 
    geom_point(position = position_dodge(width = 0.5), size = 3)

## ---- chunk-nop ----
# 2. Validacion cruzada repetida -------------------------------------------------------------------
## ---- chunk-redes4 ----
# Validacion cruzada repetida con criterio accuracy
medias3 <-cruzadaavnnetbin(
    data = dengue,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234,
    repe = 5,
    size = c(10), 
    decay = c(0.010), 
    repeticiones = 5, 
    itera = 200)

medias3$modelo <- "Avnnet1"

medias4 <- cruzadaavnnetbin(
    data = dengue,
    vardep = "varObjBin", 
    listconti = c("h10pix", "temp90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234, 
    repe = 5,
    size = c(15),
    decay = c(0.10),
    repeticiones = 5,
    itera = 200)

medias4$modelo <- "Avnnet2"

# Validacion cruzada repetida tunning maxit
medias5 <- cruzadaavnnetbin(
    data = data2,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90"),
    listclass = c(""), 
    grupos = 4, 
    sinicio = 1234,
    repe = 5, 
    repeticiones = 5,
    itera = 1000,
    size = c(10),
    decay = c(0.01))

medias5$modelo <- "Red con maxit"
## ---- chunk-nope ----
# 3. Comparacion medias ---------------------------------------------------
## ---- chunk-redes6 ----
genera_graficos(medias1, medias2, medias3, medias4, medias5)

