rm(list=ls())
setwd('C:\\D Backup\\AV\\BlackFriday\\')
train <- fread('train.csv')
test <- fread('test.csv')
test[,Purchase := mean(train$Purchase)]
c <- list(train, test)
combin <- rbindlist(c)
combin[,prop.table(table(Gender))]
combin[,prop.table(table(Age))]
combin[,prop.table(table(City_Category))]
combin[,prop.table(table(Stay_In_Current_City_Years))]
##missing values
colSums(is.na(combin))
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2, is.na) ==    TRUE,1,0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3, is.na) ==  TRUE,1,0)]
combin[,Product_Category_2 := ifelse(is.na(Product_Category_2) == TRUE, "-999",  Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(Product_Category_3) == TRUE, "-999",  Product_Category_3)]
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) ==  "4+"] <- "4"

levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

combin$Age <- as.numeric(combin$Age)
combin[, Gender := as.numeric(as.factor(Gender)) - 1]
combin[, User_Count := .N, by = User_ID]
combin[, Product_Count := .N, by = Product_ID]
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]
source('C:\\D Backup/Codility/encoding.R')
library(dummies)
combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
sapply(combin, class)
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)
c.train <- combin[1:nrow(train),]
c.test <- combin[-(1:nrow(train)),]
c.train <- c.train[c.train$Product_Category_1 <= 18,]
library(h2o)
localH2O <- h2o.init(nthreads = -1)
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)
colnames(train.h2o)
y.dep <- 14
x.indep <- c(3:13,15:20)
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.h2o, family = "gaussian")
h2o.performance(regression.model)
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 30, mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
h2o.varimp(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
h2o.performance (gbm.model)
dlearning.model <- h2o.deeplearning(y = y.dep,
                                    x = x.indep,
                                    training_frame = train.h2o,
                                    epoch = 60,
                                    hidden = c(100,100),
                                    activation = "Rectifier",
                                    seed = 1122
)
