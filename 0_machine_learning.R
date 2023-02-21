source('./src/functions/librerias.R')
source("./src/functions/cruzadas avnnet y log binaria.R")
source("./src/functions/genera_graficos.R")
source("./src/functions/funcion steprepetido binaria.R")
options(warn=-1)
options(scipen=999)

# Datos basicos -----------------------------------------------------------

## ---- chunk-0basico ----
# Lectura 
datos <- read.csv(file = './data/raw/dengue.csv')
knitr::kable(head(datos), "pipe")

#Distribucion 
# summary(datos)
str(datos)
dim(datos)

## ---- chunk-0EDA ----
# Exploración inicial al conjunto de datos
cuenta <- dplyr::count(datos, datos$NoYes)
porcentaje <- round(prop.table(table(datos$NoYes)), 2)
t <- cbind(cuenta, porcentaje)
t <- t[, -3]
knitr::kable(t, col.names = c('NoYes', 'cuenta', 'porcentaje'),'pipe')


# grafico incial de variable dependiente
barplot(porcentaje, col = brewer.pal(5, name = "Purples"), main = "Distribucion en % variable dependiente NoYes")

# Analisis exploratorio ---------------------------------------------------

## ---- chunk-nousar ----

# categorical plot
#eda <- inspect_cat(datos) 
#show_plot(eda)

# feature imbalance bar plot
# Para este dataset no hay pues en principio no hay categoricas
# balance <- inspect_imb(datos)
# show_plot(balance)

# ---- chunk-0Explora ----

par(mfrow = c(2,2))
# Correlaciones
correlacion <- inspect_cor(datos)
show_plot(correlacion)
corPlot(datos)


# mplot uso memoria
memoria <- inspect_mem(datos)
show_plot(memoria)

# na
na <- inspect_na(datos)
show_plot(na)

# histograma
histograma <- inspect_num(datos)
show_plot(histograma)

# Tipos de datos por columna
tipos <- inspect_types(datos)
show_plot(tipos)


## ---- chunk-0nousar ----
# Feature engineering -----------------------------------------------------
summary(datos)

# Depuracion
# No todas las categoricas han sido  asignadas a factores
#datos[, c(2,3,6,7)] <-lapply(datos[, c(2,3,6,7)], as.factor)

summary(datos)
str(datos)

# categorical plot
#eda <- inspect_cat(datos) 
#show_plot(eda)

## ---- chunk-0FeatureEngineering ----
# Se elimina la columna X
datos$X <- NULL

# Tratamiento de datos faltantes, se eliminan
datos2 <- na.omit(datos, (!is.na(datos)))
colSums(is.na(datos2)) > 0 

# Se separa la variable objetivo de la variable input
varObjBin <-datos2$NoYes
input <- as.data.frame(datos2[, -(9)])

## ---- chunk-0preprocesado ----
# Pre procesado, recategorizar, dummies, estandarizar etc
# Estandarizacion
temp <- input %>% mutate_if(is.numeric, scale)
head(temp)

# Se une la variable objetivo con los resultados(de estandarizar, dummys etc)
base <- data.frame(varObjBin, temp)

# La variable objetivo se cambia a alfanumerico yes, no
base$varObjBin <- ifelse(base$varObjBin == 1, "Yes", "No")
knitr::kable(head(base), "pipe") %>% kable_styling(latex_options="scale_down")


## ---- chunk-0seleccion ----
# Seleccion de variables -----
full <- glm(factor(varObjBin) ~., data = base, family = binomial(link = "logit"))
null <- glm(factor(varObjBin) ~ 1, data = base, family = binomial(link = "logit"))

# Seleccion de variables automatica
seleccion <- stepAIC(null, scope = list(upper = full), direction = "both", trace = F)

# Para ver las features escogidas de forma automatica
dput(names(seleccion$coefficients))

# Version formula
formula(seleccion)
summary(seleccion)


## ---- chunk-0steprepetido ----
# dput(names(base))
# Si hay columnas que han sido transformadas a dummies quitarlas
listconti <- c("h10pix", "temp90", "Ymin", "Ymax", "temp", "humid", "humid90", 
    "trees", "trees90")
vardep <- c("varObjBin")
data <- base
saveRDS(data, file = "./data/processed/dengue.rds")

## ---- chunk-0steprepetido1 ----
# Seleccion AIC
listaAIC <- steprepetidobinaria(
    data = data,
    vardep = vardep,
    listconti = listconti,
    sinicio = 12345,
    sfinal = 12355,
    porcen = 0.8,
    criterio = "AIC")

tabla1 <- listaAIC[[1]]
knitr::kable(tabla1, "pipe")
dput(listaAIC[[2]][[1]])
dput(listaAIC[[2]][[2]])

## ---- chunk-0steprepetido2 ----
# Seleccion con Bic
listaBIC <- steprepetidobinaria(
    data = data,
    vardep = vardep,
    listconti = listconti,
    sinicio = 12345,
    sfinal=12355, porcen = 0.8, criterio = "BIC")

tabla2 <- listaBIC[[1]]
knitr::kable(tabla2, "pipe")
dput(listaBIC[[2]][[1]])
dput(listaBIC[[2]][[2]])


## ---- chunk-0cruzadaslogistica ----
# Seleccion de variables AIC
medias1 <- cruzadalogistica(
    data = data,
    vardep = "varObjBin", 
    listconti = c("h10pix", "temp90", "trees", "trees90", 
    "Ymin", "Ymax", "temp", "humid", "humid90"),
    listclass = c(""), 
    grupos = 4, 
    sinicio = 1234, 
    repe = 5)

medias1$modelo <- "Logística1"

# Seleccion de variables BIC
medias2 <- cruzadalogistica(
    data = data,
    vardep="varObjBin", 
    listconti = c("h10pix", "temp90"),
    listclass = c(""), 
    grupos = 4, 
    sinicio = 1234, 
    repe = 5)

medias2$modelo <- "Logística2"

## ---- chunk-no ----
# Compara medias -------------------------------------------------------

genera_graficos(medias1, medias2)

## ---- chunk-no ----
# Para pruebas de seleccion de variables
base2 <- data.frame(varObjBin, temp)
saveRDS(base2, file = "./data/processed/dengue2.rds")


## ----- Train logistica para obtener curva roc
set.seed(12345)
control_logi <- trainControl(method = "repeatedcv", number = 4, repeats = 5,
    savePredictions = "all", classProbs = TRUE)

logistica <- train(factor(varObjBin) ~ h10pix + temp90 + Ymin + Ymax + temp + humid + 
        humid90 + trees + trees90, 
        data = data, 
        method = "glm", 
        trControl = control_logi
        )

# Matriz de confusión
salida_logi <- logistica$pred
salidalogi_conf <- confusionMatrix(salida_logi$pred, salida_logi$obs)
caret::confusionMatrix(logistica, "none")

# Curva roc
curvaroc <- roc(salida_logi$obs, salida_logi$Yes)
auc <- curvaroc$auc
plot.roc(roc(response = salida_logi$obs, predictor = salida_logi$Yes), 
         main = "Curva ROC Logistic Regression")



