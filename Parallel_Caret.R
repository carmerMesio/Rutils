library(caret)
library(foreach)
library(doParallel)
library(readr)
#devtools::install_github('topepo/caret/pkg/caret')
##Anirem cambian seed a cada iteració per provar unes dades diferents en cada particio etc.
#create a list of seed, here change the seed for each resampling
set.seed(123)

#length is = (n_repeats*nresampling)+1
seeds <- vector(mode = "list", length = 11)

#(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)
for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 5) ##numero de parametres aleatoris provara cada model com nro posem segon val ara 3


soldat <- read_csv("C:/Users/David/Desktop/soldat.csv")
soldat$y<-as.factor(soldat$y)
soldat$y<-mapvalues(soldat$y, from = c("-1", "1"), to = c("minone", "one"))
#detach("package:plyr", unload=TRUE)
##Nomes centro escalo i imputo per la mediana les dades.
resp<-soldat$y
prep<-preProcess(soldat[,-73],method = c("center","scale","medianImpute"))

soldatcen<-predict(prep,newdata = soldat[,-73])
soldatcen<-cbind(soldatcen,resp)

set.seed(123)
inTrain <- createDataPartition(y=soldatcen$resp, p=.70, list=FALSE)
train <- soldatcen[inTrain,]
test <- soldatcen[-inTrain,]

#for the last model
seeds[[11]]<-sample.int(1000, 1)

#control list
myControl <- trainControl(method='cv', seeds=seeds, index=createFolds(train$resp)) ##creem 10 folds aleatoris per fer servir la cv.
grid=expand.grid(n.trees=500,interaction.depth=4,shrinkage=0.01,n.minobsinnode=20)

#run model in parallel
cl <- makeCluster(detectCores()) #demano els 8 cores del meu pc posa 8 ?
registerDoParallel(cl) #paralelitzo els seguents models el primer tarda i despres els altres els fa rapid.
model1 <- train(resp~.,data=train, method='rf', trControl=myControl)
model2 <- train(resp~., data=train, method='rpart', trControl=myControl)
model3 <- train(resp~., data=train, method='gbm',distribution='adaboost', trControl=myControl)
stopCluster(cl)

rfor<-predict(model1,test,type = "raw")
confusionMatrix(rfor,test$resp)

rpart<-predict(model2,test,type = "raw")
confusionMatrix(rpart,test$resp)

rada<-predict(model3,test,type = "raw")
confusionMatrix(rada,test$resp)

##80% accuracy el rf sobre test 70 i 30.
##rpart acuracy

#compare
all.equal(predict(model1, type='prob'), predict(model2, type='prob'))
