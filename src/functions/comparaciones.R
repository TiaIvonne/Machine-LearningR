
dengue <- readRDS("./data/processed/dengue.rds")

archivo<-dengue
listconti<-c("h10pix", "temp90", "trees", "trees90", "Ymin", "Ymax", 
             "temp", "humid", "humid90")
listclass<-c("")
vardep<-"varObjBin"
clave<-"varObjBin"


library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing


source ("./src/functions/cruzadas avnnet y log binaria.R")
source ("./src/functions//cruzada rf binaria.R")
source ("./src/functions/cruzada gbm binaria.R")
source ("./src/functions/cruzada SVM binaria RBF.R")
source("../Todos los programas y datasets R/funcion seleccionar 2.0.R")
require(egg)
library(visualpred)


return<-seleccionar(archivo,listconti,listclass,vardep)

dataf<-return[[1]]
listconti<-return[[2]]
listclass<-return[[3]]
if (length(listclass)==0)
{listclass<-c("")}

vardep<-return[[4]]


tabla1<-as.data.frame(table(dataf[,vardep]))
tabla1<-tabla1[order(tabla1$Freq),]
minoritaria<-as.character(tabla1[1,c("Var1")])
tabla1<-tabla1[order(-tabla1$Freq),]
mayoritaria<-as.character(tabla1[1,c("Var1")])

dataf[,c(vardep)]<-ifelse(dataf[,c(vardep)]==minoritaria,"Yes","No")

formu<-formula(paste(vardep,"~."))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)


avnnetgrid <-expand.grid(size=c(5,10,15,20),
                         decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(formula(formu),data=dataf,method="avNNet",linout = FALSE,maxit=400,
                  trControl=control,tuneGrid=avnnetgrid,
                  repeats=5,trace=FALSE)

size=redavnnet$bestTune$size
decay=redavnnet$bestTune$decay


total<-length(listconti)+length(listclass)-1
set.seed(12345)
rfgrid<-expand.grid(mtry=seq(1,total,by=1))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

rf<- train(formula(formu),data=dataf,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,nodesize=10,replace=TRUE,
           importance=TRUE)

mtry=rf$bestTune$mtry

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(10),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2,4))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
classProbs=TRUE)

formu2<-paste("factor(",vardep,")~.")

gbm<-train(formula(formu2),data=dataf,method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

n.trees=gbm$bestTune$n.trees
shrink=gbm$bestTune$shrinkage
interaction.depth=gbm$bestTune$interaction.depth

SVMgrid<-expand.grid(C=c(0.0001,0.5,1,2,5,10,30),
                     sigma=c(0.0001,0.005,0.01,0.05,10))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all")

SVM<- train(formula(formu),data=dataf,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

gamma=SVM$bestTune$sigma
C=SVM$bestTune$C


archivo<-dataf
m1<-cruzadalogistica(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass, grupos=4,sinicio=1234,repe=5)

m1$modelo="Logística"


m2<-cruzadaavnnetbin(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=4,sinicio=1234,repe=5,
                          size=size,decay=decay,repeticiones=5,itera=200,trace=FALSE)

m2$modelo="avnnet"


m3<-cruzadarfbin(data=archivo, vardep=vardep,
                      listconti=listconti,
                      listclass=listclass,
                      grupos=4,sinicio=1234,repe=5,nodesize=10,
                      mtry=mtry,ntree=300,replace=TRUE)

m3$modelo="rf"


m4<-cruzadagbmbin(data=archivo, vardep=vardep,
                       listconti=listconti,
                       listclass=listclass,
                       grupos=4,sinicio=1234,repe=5,
                       n.minobsinnode=10,shrinkage=shrink,n.trees=n.trees,interaction.depth=2)

m4$modelo="gbm"

m5<-cruzadaSVMbinRBF(data=archivo, vardep=vardep,
                           listconti=listconti,
                           listclass=listclass,
                           grupos=4,sinicio=1234,repe=5,
                           C=C,sigma=gamma)

m5$modelo="SVMRBF"


stopCluster(cluster) # shut down the cluster 
registerDoSEQ(); #  force R to return to single threaded processing
GS_T1 <- Sys.time()
GS_T1-GS_T0


union1<-rbind(m1,m2,m3,m4,m5)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, median))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink",main="TASA FALLOS")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,auc, median))

par(cex.axis=1,las=1)
boxplot(data=uni,auc~modelo,col="pink",main="AUC")




dataf<-archivo
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="GLM",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="glm",classvar=0)
g1<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="NNET",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",
                         nodos=size,decay=decay,classvar=0)
g2<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="RF",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="rf",classvar=0,
                         mtry=mtry)
g3<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="GBM",ntree=n.trees,shrink=shrink,title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,
                         modelo="gbm",classvar=0)
g4<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
                         title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="svm",C=C,gamma=gamma,classvar=0)
g5<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")


# Aquí empieza

titulo<-paste(clave,", AUC",sep="")

filetejp<-paste(clave,"z.jpg",sep="")
filetebox<-paste(clave,"box.jpg",sep="")

# Guardo el ggbox
jpeg(filename =filetebox, width = 500, height = 600, quality = 100)
par(cex.axis=1.2,las=1)
boxplot(data=uni,auc~modelo,col="pink",main=titulo)
dev.off()

# 28.4x19.6

# Guardo el ggarrange
jpeg(filename =filetejp, width =900, height =1200,quality = 500)
ggarrange(g1,g2,g3,g4,g5,ncol =2,nrow=3)
dev.off()


