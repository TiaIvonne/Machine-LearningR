#source('machine_learning.R')
source("./src/functions/cruzada SVM binaria lineal.R")
source("./src/functions/cruzada SVM binaria polinomial.R")
source("./src/functions/cruzada SVM binaria RBF.R")
source("./src/functions/librerias.R")
source("./src/functions/genera_graficos.R")

##----chunk-svmdatos----
# Carga de datos
dengue <- readRDS(file = "./data/processed/dengue.rds")

##----chunk-svmlineal1----
# 1. SVM -------------
# 1.1 SVM lineal -----

SVMgrid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10))
set.seed(12345)
control_svm <- trainControl(method = "cv", number = 4, savePredictions = "all")

svm_1 <-train(
    data = dengue,
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + 
    Ymax + temp + humid + humid90, 
    method = "svmLinear", 
    trControl = control_svm, 
    tuneGrid = SVMgrid, 
    verbose = FALSE) 

##----chunk-svmlineal2----
#svm_1
knitr::kable(svm_1$results, "pipe")
svm_1$bestTune
plot(svm_1$results$C, svm_1$results$Accuracy, xlab = "C value", ylab = "Accuracy", 
     main = "Resultados svm lineal 1")

##----chunk-svmlineal3----
SVMgrid_1 <- expand.grid(C = c(0.1, 0.2, 0.3, 0.4, 0.6, 0,7, 0.8, 0.9, 1))

svm_2 <- train(
    data = dengue, 
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + Ymin + 
    Ymax + temp + humid + humid90, 
    method = "svmLinear", 
    trControl = control_svm, 
    tuneGrid = SVMgrid_1, 
    verbose = FALSE) 

##----chunk-svmlineal4----
knitr::kable(svm_2$results, "pipe")
svm_2$bestTune
plot(svm_2$results$C, svm_2$results$Accuracy, xlab = "C value", ylab = "Accuracy", 
     main = "Resultados svm lineal 2")

##----chunk-svmPoli1----
# 1.2 SVM Polinomial ----
SVMgrid_p <- expand.grid(
    C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10),
    degree = c(2, 3),
    scale = c(0.1, 0.5, 1, 2, 5))

svm_3 <- train(
    data = dengue, 
    factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + 
    Ymin + Ymax + temp + humid + humid90, 
    method = "svmPoly", 
    trControl = control_svm, 
    tuneGrid = SVMgrid_p, 
    verbose = FALSE)

##----chunk-svmPoli2----
knitr::kable(svm_3$bestTune, "pipe")
temp <- as.data.frame(svm_3$results) # Pasa los resultados a DF para mostrar plot
ggplot(temp, aes(x = factor(C), y = Accuracy, color = factor(degree), 
    pch = factor(scale))) + geom_point(position = position_dodge(width = 0.5), 
    size = 3) + theme_minimal()

##----chunk-svmPoli3----
# En este caso se trabaja con degree 3
temp2 <- temp[temp$degree == 3, ]
# Plot con degree 3
ggplot(temp2, aes(x = factor(C), y = Accuracy, color = factor(degree), 
    pch = factor(scale))) + geom_point(position = position_dodge(width = 0.5), 
    size = 3) + theme_minimal()

##----chunk-svmRBF1----
# 1.3 SVM RBF --------
#Se agrega el parametro sigma
SVMgrid_rbf <- expand.grid(
    C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 30),
    sigma = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 30)
)

svm_4 <- train(
    data = dengue, factor(varObjBin) ~ h10pix + temp90 + trees + trees90 + 
    Ymin + Ymax + temp + humid + humid90, 
    method = "svmRadial", 
    trControl = control_svm, 
    tuneGrid = SVMgrid_rbf, 
    verbose = FALSE)

##----chunk-svmRBF2----
knitr::kable(svm_4$bestTune, "pipe")
temp3 <- as.data.frame(svm_4$results) # Pasa a df para ploteo
ggplot(temp3, aes(x = factor(C), y = Accuracy, color = factor(sigma))) + 
    geom_point(position = position_dodge(width = 0.5), size = 3)+ 
    theme_minimal()


##----chunk-svmVC----
# 2. Validacion cruzada rep SVM -------------------------------------------
#cv para lineal
medias14 <- cruzadaSVMbin(
    data = dengue, 
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", 
    "Ymin", "Ymax", "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234,
    repe = 5, 
    C = 1
)

medias14$modelo <- "svmLineal"

# cv para poli
medias15 <- cruzadaSVMbinPoly(
    data = dengue, 
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", 
    "Ymin", "Ymax", "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234,
    repe = 5, 
    C = 0.5,
    degree = 3,
    scale = 2
)

medias15$modelo <- "svmPoly"

# cv RBF
medias16 <- cruzadaSVMbinRBF(
    data = dengue, 
    vardep = "varObjBin",
    listconti = c("h10pix", "temp90", "trees", "trees90", 
    "Ymin", "Ymax", "temp", "humid", "humid90"),
    listclass = c(""),
    grupos = 4,
    sinicio = 1234,
    repe = 5, 
    C = 1,
    sigma = 0.5
)

medias16$modelo <- "svmRBF"
##----chunk-svmCompara----
# 3. Compara medias ----
genera_graficos(medias1,medias2,medias3, medias4, medias5,medias6, medias11, medias12, 
    medias13, medias14,medias15, medias16)

