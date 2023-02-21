# sources
source("./src/functions/librerias.R")
source ("./src/functions/cruzada arbolbin.R")
source("./src/functions/genera_graficos.R")

# Carga de datos, sin estandarizar, sin tratar na
dengue <- read.csv(file = './data/raw/dengue.csv')

# Cambios basicos antes de modelar los arboles
dengue_arbol <- dengue

# Comprueba que la variable objetivo es de tipo char
# Es integer
class(dengue_arbol$NoYes)

#Se cambia nombre columna
colnames(dengue_arbol)[10]  <- "varObjBin"

# Se transforma a categorica
dengue_arbol$varObjBin <- ifelse(dengue_arbol$varObjBin == 1, "Yes", "No")

# Se elimina columna X
dengue_arbol$X <- NULL

# La columna varObjBin pasa a ser la primera columna
dengue_arbol <- dengue_arbol %>% relocate(varObjBin)

head(dengue_arbol)
# 
# 1. Importancia de variables ------

## ---- chunk-arboles1 ----
# Modelo completo con gini para variable dependiente categorica
arbol1 <- rpart(factor(varObjBin) ~ ., 
    data = dengue_arbol, 
    minbucket = 30, 
    method = "class",
    parms = list(split = "gini"))

# Reglas de decision
rattle::asRules(arbol1)

## ---- chunk-arboles2 ----
# Importancia de variables
par(cex = 0.7)
barplot(height = arbol1$variable.importance, col = "#e7e1fd", 
    main = "Importancia de variables Arbol1")

# Ploteo del arbol
rpart.plot(arbol1, extra = 105, tweak = 1.2, type = 1, nn = TRUE)

## ---- chunk-arboles3 ----

# Modelo con maxsurrogate = 0 para que trabaje los na's
arbol2 <- rpart(factor(varObjBin) ~ ., 
    data = dengue_arbol, 
    minbucket = 30, 
    method = "class", 
    maxsurrogate = 0,
    parms = list(split = "gini"))

## ---- chunk-arboles4 ----
# Importancia de variables
barplot(height=arbol2$variable.importance, col = "#e7e1fd", 
        main = "Importancia de variables Arbol2")

# Ploteo del arbol
rpart.plot(arbol2, extra = 105, tweak = 1.2, type = 1, nn = TRUE)

## ---- chunk-no ----
# 2.Tuneo -------------------------------------------------------------------
## ---- chunk-arboles5 ----

# Tuneado sobre complejidad del arbol con Rpart

arbol_min5 <- rpart(factor(varObjBin) ~ ., data = dengue_arbol, minbucket = 5, cp = 0)
arbol_min30 <- rpart(factor(varObjBin) ~ ., data = dengue_arbol, minbucket = 30, cp = 0)
arbol_min60 <- rpart(factor(varObjBin) ~ ., data = dengue_arbol, minbucket = 60, cp = 0)

## ---- chunk-arboles6 ----

par(mfrow= c(2,2))
barplot(height = arbol_min5$variable.importance, col = "#e7e1fd", 
        main = "Importancia de variables con minbucket 5")
barplot(height = arbol_min30$variable.importance, col = "#e7e1fd", 
        main = "Importancia de variables con minbucket 30")
barplot(height = arbol_min60$variable.importance, col = "#e7e1fd", 
        main = "Importancia de variables con minbucket 60")
par(mfrow= c(2,2))
rpart.plot(arbol_min5, extra = 1)
rpart.plot(arbol_min30, extra = 1)
rpart.plot(arbol_min60, extra = 1)



## ---- chunk-arboles7 ----
# En los ejemplos anteriores se dejaron los na's para ver el comportamiento, se eliminan 
# tunning de minubucket en bucle
# Tratamiento de datos faltantes
dengue_arbol1 <- na.omit(dengue_arbol, (!is.na(dengue_arbol)))
colSums(is.na(dengue_arbol1)) > 0 

# Tunning con minbucket en bucle
set.seed(12345)
control_arbol <- trainControl(method = "cv",
    number = 4,
    classProbs = TRUE, 
    savePredictions = "all") 

arbolgrid <-  expand.grid(cp = c(0))
tabla_resultados <-c() 

for (minbu in seq(from = 5, to = 60, by = 5)){
    arbolgrid <-  expand.grid(cp=c(0))
    arbolcaret <- train(factor(varObjBin) ~ h10pix + temp90 + Ymin + Ymax + 
        temp + humid + humid90 + trees + trees90,
    data = dengue_arbol1, 
    method = "rpart", 
    minbucket = minbu, 
    trControl = control_arbol, 
    tuneGrid = arbolgrid)
    
    accuracy <- arbolcaret$results$Accuracy
    sal <- arbolcaret$pred
    salconfu <- confusionMatrix(sal$pred, sal$obs)
    curvaroc <-roc(response = sal$obs, predictor = sal$Yes)
    auc <- curvaroc$auc
    # Guarda los resultados en formato tabla
    tabla_resultados <- rbind(tabla_resultados, c(minbu, accuracy, auc))
}

# Cambia los nombres de las columnas
colnames(tabla_resultados) <- c("minibucket","accuracy","AUC")

## ---- chunk-arboles8 ----
knitr::kable(tabla_resultados, "pipe")


## ---- chunk-arboles9 ----
arbol4 <- train(factor(varObjBin) ~ h10pix + temp90 + Ymin + Ymax + temp + 
    humid + humid90 + trees + trees90,
    data = dengue_arbol1, 
    method = "rpart", 
    minbucket = 20, 
    trControl = control_arbol, 
    tuneGrid = arbolgrid)

## ---- chunk-arboles10 ----
salida_a4 <- arbol4$pred
salida4_confusion <- confusionMatrix(salida_a4$pred, salida_a4$obs)
salida4_confusion
fourfoldplot(salida4_confusion$table)

## ---- chunk-arboles11 ----
# Curva roc
curvaroc <-roc(response = salida_a4$obs, predictor = salida_a4$Yes)
auc <- curvaroc$auc
plot.roc(roc(response = salida_a4$obs, predictor = salida_a4$Yes), main = "Curva ROC arbol con minbucket = 20")


## ---- chunk-arboles12 ----
# 2. Validacion cruzada repetida -----
# Con los resultados de la tabla con iteraciones se configura la validacion cruzada
medias_arbol <-cruzadaarbolbin(
    data = dengue_arbol1,
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", "temp", 
    "humid", "humid90"),
    listclass = c(""), grupos = 4, sinicio = 1234, repe = 5,
    cp = c(0), minbucket = 20)

medias_arbol$modelo <- "Arbol"

## ---- chunk-no ----
# 3. Comparacion medias ------------------------------------------------------
## ---- chunk-arboles13 ----
#genera_graficos(medias1, medias2, medias3, medias4, medias5,medias6, medias9,medias11, medias_arbol)

genera_graficos(medias1, medias2, medias_arbol)



