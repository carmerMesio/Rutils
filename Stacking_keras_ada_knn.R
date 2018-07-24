######NEt KERAS R.
####Caret R.
#####http://uc-r.github.io/feedforward_DNN##
# Keras + Caret R-Studio proves.

library(readr)
data_expl <- read_csv("C:/Users/David/TutorialPython/scripts/data_expl.csv")
#install.packages("keras")
library(caret)
library(ggplot2)
library(keras)
library(dplyr)
##environment from keras and tensorflow from anaconda.
install_keras()

##dummyfy extracting the intercept of fitted matrix
##for each client let's perform a NNet
loc <- unique(data_expl$location)
which(data_expl$location==loc[1])
data_expl <- data_expl %>% select(-c(measure,location,MAINSUPPLY,X1,HVAC,LIGHTING,OTHERS)) %>% mutate(year=as.factor(year),month=as.factor(month)) %>% as.data.frame()
data_one_hot <- model.matrix(~.,data_expl)[,-1] %>% as.data.frame()


set.seed(123)
##CARET.
trainIndex <- createDataPartition(data_one_hot$value, p = .7, 
                                  list = FALSE, 
                                  times = 1)

train <- data_one_hot[ trainIndex,]
test  <- data_one_hot[-trainIndex,]

train_x <- train %>% select(-value) %>% select(-matches("month|year|sect|sub")) %>% as.data.frame()
##unselect categorical values

mean    <- colMeans(train_x)
std     <- apply(train_x, 2, sd)
##escalado normal
train_x <- scale(train_x, center = mean, scale = std)
train_x <- cbind(train_x,train %>% select(matches("month|year|sect|sub")) %>% as.data.frame()) %>% as.matrix()
# testing features
test_x <- test %>% select(-value) %>% select(-matches("month|year|sect|sub"))
test_x <- scale(test_x, center = mean, scale = std)
test_x <- cbind(test_x,test %>% select(matches("month|year|sect|sub")) %>% as.data.frame()) %>% as.matrix()

# Create & transform response sets
train_y <- log(train$value)
test_y  <- log(test$value)

# zero variance variables (after one hot encoded) cause NaN so we need to remove
colSums(is.na(train_x))

dim(train_x)
dim(test_x)
##testing keras
val_tra_x = train_x[3001:3501,]
train_x = train_x[1:3000,]
test_x = test_x[1:500,]

val_y = train_y[3001:3501]
train_y = train_y[1:3000]
test_y = test_y[1:500]
model <- keras_model_sequential() %>%
  # network architecture
  # Agafem 10 hiddens al principi si les neurones acitven la funció tornara el valor ponderat que despres es fa sumatori i 
  #finalment es pasa per la sigmoide quan la señal acitivi la neurona. Segundo hidden 5 neuronas. Utiliza función de loss que computa
  ## Learning rate nos permitirá buscar el gradiente o la velocidad de convergencia evitando caer en minimos locales de la función.
  
  #This data have 47 column inputs so when passing from 10,5 to 16,8 neurons per hidden we see an inprovement in loss function.
    
  layer_dense(units = 50, activation = "relu", input_shape = ncol(train_x),kernel_regularizer = regularizer_l2(0.05)) %>%
  #layer_batch_normalization() %>%  #se añade una normalización que en teoria mejora el overfitting.
  layer_dropout(rate=0.1) %>% #elimina un % de los datos para prevenir estimar el ruido
  layer_dense(units = 20, activation = "relu",kernel_regularizer = regularizer_l2(0.05)) %>%
  #layer_batch_normalization() %>% 
  layer_dropout(rate=0.1) %>% 
  layer_dense(units = 1) %>%
  
  #Si se usa normalizacion no hacer dropout. RAndomly drop out of varibels to preevent overgiting.
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

# train our model. Antes de hacer esto cada vez correl el model. Nose porque se guarda los valores obtenidos en la anteriro iteracion.
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 100,
  batch_size = 32,
  validation_split = .2,
  verbose = TRUE,
  callbacks = list(
    callback_early_stopping(patience = 2),
    callback_reduce_lr_on_plateau()
  )
)

##callbacks makes stop the net if there is not improvement in the loss function over 6 epochs.

###IMPPP####
## val mean square error if metric is mse is the current value over the test splidet of 0.2.
# epochs: An epoch describes the number of times the algorithm sees the entire data set. So, 
# each time the algorithm has seen all samples in the dataset, an epoch has completed. In our training set,
# we have 2054 observations so running batches of 32 will require 64 passes for one epoch. 

learn
plot(learn)

#el dropout elimina el oberfitting bastante bien.

##Predict on test data.

(results <- model %>% evaluate(val_tra_x, val_y))

preds<-model %>% 
  predict(val_tra_x) %>% 
  broom::tidy() %>% 
  mutate(
    truth = val_y, 
    pred_tran = exp(x), 
    truth_tran = exp(truth)
  )

View(preds)
  
preds %>% yardstick::rmse(truth_tran, pred_tran) ##Metricas del paquete yardstrick.
##MAybe also a View of prediction.

##with regularization the data fits better high values than small ones.

############################
###GLMNET
library(glmnet)
library(Metrics)
set.seed(123)

##GLMnet Ridge
modelglmnetRidge <- cv.glmnet(train_x,train_y,family = "gaussian",alpha = 0,standardize = FALSE,
                              intercept = TRUE,lambda = c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),0.00075,0.0005,0.0001),type.measure = "mse")

## Predictions
pred_glmnet_rid = exp(predict(modelglmnetRidge,newx=val_tra_x))
rmse(exp(val_y), pred_glmnet_rid)

##Xgboost.
require(xgboost)
require(Matrix)

dtrain <- xgb.DMatrix(data=train_x, label=train_y)
dval <- xgb.DMatrix(data=val_tra_x )
dtest <- xgb.DMatrix(data=test_x )
set.seed(123)
#param. bons aprox 30
xgb_params = list(
  booster = "gbtree",
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.8,
  eta = 0.1,  #[2-10]/nrounds o nro de arboles
  objective = "reg:linear",
  max_depth = 8, ##mayor en funcion de la complejidad del modelo.
  num_parallel_tree = 2,
  min_child_weight = 12, ##regla thumb
  alpha=2,
  lambda=2,
  gamma=2
  
)

#alpha = 1, lambda = 1
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=500,
             nfold=6,
             early_stopping_rounds=30,
             print_every_n = 10,
             verbose= 1,
             nthread=2, #es la velocidad para los nucleos del core.
             #feval=xgeval,
             metric="rmse",
             maximize=FALSE
)

best_nrounds = res$best_iteration
####Creem el model i predim
gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
###########

predval = exp(predict(gbdt,dval))

Metrics::rmse(predval,exp(val_y))
###Stacking with subsample of train and a part of validation to then use with the last model.
stacked_val = data.frame("keras"=log(preds$pred_tran), "ridvl" = log(pred_glmnet_rid),"xgbvl" = log(predval)) %>% as.matrix()

cor(stacked_val)
View(cbind(stacked_val,exp(val_y)))

####Lasso last model.
pred_rid_test = exp(predict(modelglmnetRidge,newx=test_x))
predtest = exp(predict(gbdt,dtest))
predstest<-model %>% 
  predict(test_x) %>% 
  broom::tidy() %>% 
  mutate(pred_tran = exp(x))

stacked_test = data.frame("keras"=log(predstest$pred_tran), "ridvl" = log(pred_rid_test),"xgbvl" = log(predtest)) %>% as.matrix()

##GLMNET to stack the models.
set.seed(123)
##Lasso performing glmnet.
##10 folds crossvalidation.
modelglmnetLasso <- cv.glmnet(stacked_val,val_y,family = "gaussian",alpha = 0.6,standardize = FALSE,
                              intercept = TRUE,lambda = c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),0.00075,0.0005,0.0001),type.measure = "mse")

## Predictions
pred_glmnet_las = exp(predict(modelglmnetLasso,newx=stacked_test))
rmse(exp(test_y), pred_glmnet_las)


##Funciona con los modelos por separado hacemos un rmse de 190.000 y con el stacked de 61.179.
View(cbind(exp(test_y),pred_glmnet_las))

##TODO con train y test.
##Compare the similarities between diferent splits of data to avoid problems on perform.

