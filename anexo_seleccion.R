# Anexo, seleccion de variables 
# La variable independiente deber ser numerica o no funciona

source("./src/functions/cruzadas avnnet y log binaria.R")
source("./src/functions/genera_graficos.R")
dengue2 <- readRDS(file = "./data/processed/dengue2.rds")
data <- readRDS(file="./data/processed/dengue.rds")

dengue2 <- as.data.frame(dengue2)

## ---- chunk-party ----

library(party)
s1 <- cforest(varObjBin ~ . , 
    data = dengue2, 
    control = cforest_unbiased(mtry = 2, ntree = 50))
varimp(s1)


## ---- chunk-earth ----
library(earth)
s2 <- earth(varObjBin ~ ., data=dengue2) # build model
evaluacion <- evimp (s2)
plot(evaluacion)

## ---- chunk-boruta ----
library(Boruta)
boruta_model <- Boruta(varObjBin ~ ., data= dengue2, doTrace=2)  
relevancia <- names(boruta_model$finalDecision[boruta_model$finalDecision %in% c("Confirmed")])  
print(relevancia) 
plot(boruta_model, las = 2, cex.axis=0.7, xlab="")


## ---- chunk-RFE----
set.seed(7)
# Cargar datos
data(dengue2)
# Se define la funcion de control
controlrfe <- rfeControl(functions=rfFuncs, method="cv", number=4)
# Algoritmo RFE
res_rfe <- rfe(dengue2[,2:12], dengue2[,1], sizes=c(2:12), rfeControl=controlrfe)
#Resultados
print(res_rfe)
# Features escogidas
predictors(res_rfe)
# graficos
# plot(results, type=c("g", "o"))

## ---- chunk-modelos ----
# Generado con libreria party
anexo1 <- cruzadalogistica(
    data = data,
    vardep = "varObjBin", listconti = c("humid", "humid90","temp", "temp90",
    "h10pix", "h10pix90"),
    listclass = c(""), 
    grupos = 4, sinicio = 1234, repe = 5)

anexo1$modelo <- "log-party"


# Generado con MARS
anexo2 <- cruzadalogistica(
    data = data,
    vardep = "varObjBin", 
    listconti = c("h10pix", "Xmax", "temp90", "Ymax", "Ymin", "temp", "Xmin",
                  "trees"),
    listclass = c(""), 
    grupos = 4, sinicio = 1234, repe = 5)

anexo2$modelo <- "log-mars"

# Generado con Boruta
anexo3 <- cruzadalogistica(
    data = data,
    vardep = "varObjBin", 
    listconti = c("h10pix", "Xmax", "Xmin", "h10pix90", "Ymax"),
    listclass = c(""), 
    grupos = 4, sinicio = 1234, repe = 5)

anexo3$modelo <- "log-boruta"


# Generado con RFE
anexo4 <- cruzadalogistica(
    data = data,
    vardep = "varObjBin", 
    listconti = c("Xmax", "h10pix","Xmin","h10pix90","Ymin","temp" ),
    listclass = c(""), 
    grupos = 4, sinicio = 1234, repe = 5)

anexo4$modelo <- "log-RFE"




