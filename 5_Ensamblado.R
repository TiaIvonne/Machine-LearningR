source("./src/functions/librerias.R")
source("./src/functions/cruzadas ensamblado binaria fuente.R")
source("./src/functions/genera_graficos.R")


# Carga de datos
dengue <- readRDS(file = "./data/processed/dengue.rds")

# 1. Preparacion y tuneado ensamblado basico con caret-------------------------
# Indicar los valores que recomienda avNNet, gbm y rforest, SVM RBF, lineal 
# y poli

formula1 <- as.formula(paste("factor(","varObjBin",")","~.")) 
    
dengue$varObjBin <-as.factor(dengue$varObjBin) 

levels(dengue$varObjBin) <- make.names(levels(factor(dengue$varObjBin)))

##----chunk-ensamblado1----
# Semilla
set.seed(12345)
repeticiones <- 10

# Evaluar los modelos
stackControl <- trainControl(
    method = "repeatedcv", 
    number = 4, 
    repeats = repeticiones,
    savePredictions = TRUE,
    classProbs = TRUE)

# grid gbm
gbmGrid <- expand.grid(
    n.trees = c(2000), 
    interaction.depth = c(2), 
    shrinkage =c(0.1), 
    n.minobsinnode = c(20))

# grid random forest 
rfGrid <- expand.grid(mtry=c(3))

# grid svm lineal
svmlinGrid <- expand.grid(C=c(0.08))

# grid svm ply
svmPolyGrid <- expand.grid(C=c(0.03),degree=c(3),scale=c(2))

# grid svm radial
svmRadialGrid <- expand.grid(sigma = c(0.5), C = c(30))

# Modelado
set.seed(12345)
models <- caretList(varObjBin~., 
    data = dengue, 
    trControl = stackControl,
    tuneList = list( 
        parrf = caretModelSpec(method = "rf", maxnodes = 30, n.trees = 200, nodesize = 10, sampsize = 150, tuneGrid = rfGrid), 
        glm = caretModelSpec(method = "glm"), 
        gbm = caretModelSpec(method = "gbm", tuneGrid = gbmGrid), 
        svmlinear = caretModelSpec(method = "svmLinear", tuneGrid = svmlinGrid), 
        svmPoly = caretModelSpec(method = "svmPoly", tuneGrid = svmPolyGrid), 
        svmradial = caretModelSpec(method = "svmRadial", tuneGrid = svmRadialGrid)))

##----chunk-ensamblado2----
results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ensemble <- caretEnsemble(models)
# Aquí se recomiendan los pesos para el ensamblado # de todos los modelos y se 
# ve la tasa de aciertos de cada modelo y ensamblado

summary(ensemble)

# 2. Validacion cruzada y kit ensamblado ----
# 2.1 Leer funciones ------
# 2.2 Prepapar archivo, variables , semilla y repeticiones ----
dput(names(dengue)) 

set.seed(12345) 

archivo <- dengue 

vardep <- "varObjBin" 
listconti <- c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
    "temp", "humid", "humid90") 

listclass <- c("") 
grupos <- 4 
sinicio <- 1234 
repe <- 15


# 2.3 Obtener datos de cv repetida para cada algoritmo y 
# procesar resultado ----
medias_1<-cruzadalogistica(data = archivo,
    vardep = vardep,
    listconti=listconti,
    listclass = listclass, grupos = grupos,
    sinicio = sinicio, repe = repe)

medias1bis <- as.data.frame(medias_1[1])
medias1bis$modelo<-"logistica"
predi1<-as.data.frame(medias_1[2])
predi1$logi<-predi1$Yes

medias_2<-cruzadaavnnetbin(data=archivo,
    vardep=vardep,listconti=listconti,
    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
    size=c(10),decay=c(0.01),repeticiones=5,itera=200)

medias2bis<-as.data.frame(medias_2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias_2[2])
predi2$avnnet<-predi2$Yes

medias_3<-cruzadarfbin(data=archivo,
    vardep=vardep,listconti=listconti,
    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
    mtry=3,ntree=500,nodesize=10,replace=TRUE)

medias3bis<-as.data.frame(medias_3[1])
medias3bis$modelo<-"randomforest"
predi3<-as.data.frame(medias_3[2])
predi3$rf<-predi3$Yes

medias_4 <-cruzadagbmbin(data=archivo,
    vardep=vardep,listconti=listconti,
    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
    n.minobsinnode=5,shrinkage=0.1,n.trees=3000,interaction.depth=2)

medias4bis<-as.data.frame(medias_4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias_4[2])
predi4$gbm<-predi4$Yes

medias_5 <-cruzadaxgbmbin(data = archivo,
    vardep = vardep,listconti = listconti,
    listclass = listclass, grupos = grupos, sinicio = sinicio, repe = repe,
    min_child_weight = 5, eta = 0.10, nrounds = 250, max_depth = 6,
    gamma = 0, colsample_bytree = 1, subsample = 1,
    alpha = 0, lambda = 0, lambda_bias = 0)

medias5bis <-as.data.frame(medias_5[1])
medias5bis$modelo <-"xgbm"
predi5 <-as.data.frame(medias_5[2])
predi5$xgbm<-predi5$Yes

medias_6<-cruzadaSVMbin(data=archivo,
    vardep=vardep,listconti=listconti,
    listclass=listclass,grupos=grupos,
    sinicio=sinicio,repe=repe,C=0.08)

medias6bis<-as.data.frame(medias_6[1])
medias6bis$modelo<-"svmLinear"
predi6<-as.data.frame(medias_6[2])
predi6$svmLinear<-predi6$Yes

medias_7 <- cruzadaSVMbinPoly(data = archivo,
     vardep = vardep, listconti = listconti,
     listclass = listclass, grupos = grupos, sinicio = sinicio, repe = repe,
     C = 0.03, degree = 3, scale = 2)

medias7bis <- as.data.frame(medias_7[1])
medias7bis$modelo <- "svmPoly"
predi7 <- as.data.frame(medias_7[2])
predi7$svmPoly <- predi7$Yes

medias_8 <- cruzadaSVMbinRBF(data = archivo,
    vardep = vardep, listconti = listconti,
    listclass = listclass, grupos = grupos,
    sinicio = sinicio, repe = repe,
    C = 30, sigma = 0.5)

medias8bis<-as.data.frame(medias_8[1])
medias8bis$modelo<-"svmRadial"
predi8<-as.data.frame(medias_8[2])
predi8$svmRadial<-predi8$Yes

##----chunk-ensamblado3----
# Genera graficos
genera_graficos(medias1bis, medias2bis, medias3bis, medias4bis, medias5bis, 
                medias6bis, medias7bis, medias8bis)


##----chunk-ensamblado4----
# 2.4 Construccion de todos los ensamblados ----
unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)
ncol(unipredi)

unipredi<- unipredi[, !duplicated(colnames(unipredi))]
ncol(unipredi)

unipredi$predi9<-(unipredi$logi+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$logi+unipredi$rf)/2
unipredi$predi11<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi12<-(unipredi$logi+unipredi$xgbm)/2
unipredi$predi13<-(unipredi$logi+unipredi$svmLinear)/2
unipredi$predi14<-(unipredi$logi+unipredi$svmPoly)/2
unipredi$predi15<-(unipredi$logi+unipredi$svmRadial)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi19<-(unipredi$avnnet+unipredi$svmLinear)/2
unipredi$predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
unipredi$predi21<-(unipredi$avnnet+unipredi$svmRadial)/2
unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi23<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi24<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$predi25<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$predi26<-(unipredi$rf+unipredi$svmRadial)/2
unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi28<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi29<-(unipredi$gbm+unipredi$svmPoly)/2
unipredi$predi30<-(unipredi$gbm+unipredi$svmRadial)/2

unipredi$predi31<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$predi32<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi33<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi34<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi35<-(unipredi$logi+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi36<-(unipredi$logi+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi37<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi38<-(unipredi$logi+unipredi$rf+unipredi$xgbm)/3
unipredi$predi39<-(unipredi$logi+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi40<-(unipredi$logi+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi41<-(unipredi$logi+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi42<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi43<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi44<-(unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi45<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi46<-(unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi47<-(unipredi$logi+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi48<-(unipredi$logi+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi49<-(unipredi$logi+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi51<-(unipredi$rf+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi52<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi53<-(unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi54<-(unipredi$rf+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi55<-(unipredi$rf+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$predi56<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi57<-(unipredi$rf+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi58<-(unipredi$rf+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi59<-(unipredi$rf+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi60<-(unipredi$rf+unipredi$avnnet+unipredi$svmRadial)/3

unipredi$predi61<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi62<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi63<-(unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi64<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4
unipredi$predi65<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4
unipredi$predi66<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4

unipredi$predi67<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmLinear)/5
unipredi$predi68<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmPoly)/5
unipredi$predi69<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmRadial)/5

# 2.5 Procesado de los ensamblados ----
# Listado de modelos a considerar, cambiar al gusto

##----chunk-ensamblado5----
dput(names(unipredi))
listado<-c("logi", "avnnet", 
           "rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
           "svmRadial","predi9", "predi10", "predi11", "predi12", 
           "predi13", "predi14", "predi15", "predi16", "predi17", "predi18", 
           "predi19", "predi20", "predi21", "predi22", "predi23", "predi24", 
           "predi25", "predi26", "predi27", "predi28", "predi29", "predi30", 
           "predi31", "predi32", "predi33", "predi34", "predi35", "predi36", 
           "predi37", "predi38", "predi39", "predi40", "predi41", "predi42", 
           "predi43", "predi44", "predi45", "predi46", "predi47", "predi48", 
           "predi49", "predi50", "predi51", "predi52", "predi53", "predi54", 
           "predi55", "predi56", "predi57", "predi58", "predi59", "predi60", 
           "predi61", "predi62", "predi63", "predi64", "predi65", "predi66", 
           "predi67", "predi68", "predi69")

# Cambio a Yes, No, todas las predicciones
# Defino funcion tasafallos

tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
}

auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)

medias0<-data.frame(c())
for (prediccion in listado)
{
    unipredi$proba<-unipredi[,prediccion]
    unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
    for (repe in 1:repeticiones)
    {
        paso <- unipredi[(unipredi$Rep==repe),]
        pre<-factor(paso[,prediccion])
        archi<-paso[,c("proba","obs")]
        archi<-archi[order(archi$proba),]
        obs<-paso[,c("obs")]
        tasa=1-tasafallos(pre,obs)
        t<-as.data.frame(tasa)
        t$modelo<-prediccion
        auc<-suppressMessages(auc(archi$obs,archi$proba))
        t$auc<-auc
        medias0<-rbind(medias0,t)
    }
}

##----chunk-ensamblado6----
# 2.6 Boxplot ----
# Finalmente boxplot

# JPEG device, mostrar solo el ordenado

ggplot(medias0, aes(x=modelo, y=tasa, fill = modelo)) + 
    ggtitle ("Tasa fallos")+
    geom_boxplot(alpha=0.3) +
    theme_minimal() + 
    theme(legend.position="none") +
    xlab("Modelos") + ylab("Tasa") +
    theme(axis.text.x = element_text(angle=90))
dev.off()

# Para AUC se utiliza la variable auc del archivo medias0
ggplot(medias0, aes(x=modelo, y=auc, fill = modelo)) + 
    ggtitle ("Area under curve AUC")+
    geom_boxplot(alpha=0.3) +
    theme_minimal() + 
    theme(legend.position="none") +
    xlab("Modelos") + ylab("AUC") +
    theme(axis.text.x = element_text(angle=90))


# 2.7 Tabla medias ----
# Para tasa fallos
##----chunk-ensamblado7----
tablamedias<-medias0 %>%
    group_by(modelo) %>%
    summarise(tasa=mean(tasa))
    
tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])
knitr::kable(head(tablamedias, n = 10), "pipe")

# Para AUC
tablamedias2<-medias0 %>%
    group_by(modelo) %>%
    summarise(auc=mean(auc))     

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]
knitr::kable(head(tablamedias2, n=10), "pipe")

##----chunk-ensamblado8----
# 2.8 Boxplot ordenado -----
medias_ord <- medias0
medias_ord$modelo <- with(medias_ord,reorder(modelo,tasa, mean))
# Grafico medias ordenadas
png("./figure/ensamblado_medias0_tasa.png",width = 800, height = 800)
ggplot(medias_ord, aes(x=modelo, y=tasa, fill = modelo)) + 
    ggtitle ("Tasa fallos")+
    geom_boxplot(alpha=0.3) +
    theme_minimal() + 
    theme(legend.position="none") +
    xlab("Modelos") + ylab("Tasa") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) 
dev.off()

# Graficos auc ordenados
medias_ord$modelo <- with(medias_ord,reorder(modelo, auc, mean))
png("./figure/ensamblado_medias0_auc.png", width = 800, height = 800)
ggplot(medias_ord, aes(x=modelo, y=auc, fill = modelo)) + 
    ggtitle ("Area under curve AUC")+
    geom_boxplot(alpha=0.3) +
    theme_minimal() + 
    theme(legend.position="none") +
    xlab("Modelos") + ylab("AUC") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) 
dev.off()

# 2.9 Boxplot ordenado -----
# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("logi", "avnnet",
              "rf","gbm", "xgbm", "svmLinear",  "svmPoly",
              "svmRadial", "predi55", "predi57", "predi52",
              "predi60")
 
medias_ord$modelo<-as.character(medias_ord$modelo)
mediasver<-medias_ord[medias_ord$modelo %in% listadobis,]
mediasver$modelo <- with(mediasver,reorder(modelo, auc, median))

png("./figure/ensamblado_final.png",width = 800, height = 800)
ggplot(mediasver, aes(x=modelo, y=auc, fill = modelo)) + 
    ggtitle ("Area under curve AUC")+
    geom_boxplot(alpha=0.3) +
    theme_minimal() + 
    theme(legend.position="none") +
    xlab("Modelos") + ylab("AUC") +
    theme(axis.text = element_text(face="bold")) +
    theme(axis.text.x = element_text(size=10, face="bold", color = "black"))
dev.off()


# # Observar los mejores ensamblados
# # 2.10 Mejores ensamblados ----
# # Revisar al menos 4 mejores ensamblados

# unipredi$predi55<-(unipredi$rf+unipredi$xgbm+unipredi$svmRadial)/3
# unipredi$predi57<-(unipredi$rf+unipredi$avnnet+unipredi$xgbm)/3
# unipredi$predi60<-(unipredi$rf+unipredi$avnnet+unipredi$svmRadial)/3
# unipredi$predi52<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3

# Agregar conclusiones, parece ser que random forest sigue siendo mejor

# 3. Graficos para comparar resultados -------

# Correlaciones entre predicciones de dif algoritmos

unipredi1 <- cbind(predi1, predi2, predi3, predi4, predi5, predi6, predi7, predi8)
ncol(unipredi1)

# Elimina duplicados
unipredi1 <- unipredi1[, !duplicated(colnames(unipredi1))]
ncol(unipredi1)

# Se agregan ensamblados
unipredi1$predi55 <- (unipredi1$rf + unipredi1$xgbm + unipredi1$svmRadial)/3
unipredi1$predi57 <- (unipredi1$rf + unipredi1$avnnet + unipredi1$xgbm)/3
unipredi1$predi60 <- (unipredi1$rf + unipredi1$avnnet + unipredi1$svmRadial)/3
unipredi1$predi52 <- (unipredi1$rf + unipredi1$gbm + unipredi1$svmRadial)/3
 
unigraf <- unipredi1[unipredi1$Rep == "Rep01",]

# Correlaciones entre predicciones de algoritmos "clasicos"

solos <- c("logi", "avnnet", "rf", "gbm", "xgbm", "svmLinear", "svmPoly",
             "svmRadial")
mat <- unigraf[,solos]
matrizcorr <- cor(mat)
matrizcorr 

corrplot(matrizcorr, type = "upper", order = "hclust", tl.col = "black", 
    tl.srt = 45, is.corr = FALSE, col.lim=c( 0.8, 1))


ggplot(unigraf, aes(xgbm, rf)) +
    geom_point(aes(colour = obs), na.rm = TRUE) +
    geom_hline(yintercept = 0.5, color = "black", size = 0.5) +
    geom_vline(xintercept = 0.5, color = "black", size = 0.5) +
    theme_minimal()


ggplot(unigraf, aes(rf, predi55)) +
    geom_point(aes(colour = obs), na.rm = TRUE) +
    geom_hline(yintercept = 0.5, color = "black", size = 0.5) +
    geom_vline(xintercept = 0.5, color = "black", size = 0.5) +
    theme_minimal()

ggplot(unigraf, aes(svmLinear, svmRadial)) +
    geom_point(aes(colour = obs), na.rm = TRUE) +
    geom_hline(yintercept = 0.5, color = "black", size = 0.5) +
    geom_vline(xintercept = 0.5, color = "black", size = 0.5) +
    theme_minimal()



write.csv(dengue, "dengue1.csv", row.names=FALSE)










