library(h2o)
library(doParallel)

h2o.init()
h2o.shutdown()
h2o.init(nthreads = -1)


# Pongo un solo cluster para que los resultados sean reproducibles.
# Para más velocidad se puede poner 8


# 1. Preparacion -------
# Reordeno las columnas

dput(names(dengue))

dengue2 <- dengue[,c("humid", "humid90", "temp", "temp90", "h10pix", "h10pix90",
    "trees", "trees90", "Xmin", "Xmax", "Ymin", "Ymax", "varObjBin")]

# Chequeo el tipo de dato del dataset

sapply(dengue2, class)

# Se pasan todas las variables a tipo numerico

cols <- c("humid", "humid90", "temp", "temp90", "h10pix", "h10pix90", 
    "trees", "trees90", "Xmin", "Xmax", "Ymin", "Ymax")

dengue2[,cols] <- lapply(dengue2[,cols],as.numeric)

# Ahora deberian ser todas numericas en vez de matrix
print(sapply(dengue2, class))

# Traducir el dataset a formato h20
# Para clasificación hay que definir como factor la dependiente
dengue2$varObjBin <- as.factor(dengue2$varObjBin)
train <- as.h2o(dengue2)

# 2. AutoML -------


head(dengue2[1:12])
aml <- h2o.automl(x = 1:12, y = 13, training_frame = train, max_models = 20, 
    seed = 1, keep_cross_validation_predictions = TRUE, nfolds = 4 )

lb <- h2o.get_leaderboard(aml, extra_columns = "ALL")

# Imprime todo

print(lb, n = nrow(lb))

aml@leader

# Modelo en primer lugar, se estudia
aml@leader@parameters

modelo1 <- h2o.getModel("GBM_4_AutoML_1_20230202_155248")

modelo1@allparameters

modelo1@parameters

gbm <- h2o.gbm(x = 1:12,y = 13 ,training_frame = train,seed=23,
    ntrees = 72, max_depth = 10, min_rows = 10, sample_rate = 0.8,
    col_sample_rate = 0.8, col_sample_rate_per_tree = 0.8, nfolds = 4)
gbm

modelo2 <- h2o.getModel("StackedEnsemble_AllModels_1_AutoML_1_20230130_185907")
modelo2@parameters

