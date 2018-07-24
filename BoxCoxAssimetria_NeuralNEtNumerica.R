######Ideas para transformar data y red neuronal.

## traiem nm.col (id no util)
#prova <- prova %>% select(-Nm.Col) %>% as.data.frame()

# table(prova$MedioD)
# dim(prova)
# ee <- subs %>% filter(CantidadTotal2 < quantile(subs$CantidadTotal2,probs=0.95)) %>%  as.data.frame()

#set.seed(100) 384
#set.seed(2001515) 2000
# set.seed(300)
# idx <- seq(1,dim(prova)[1],by=1)
# idx <- sample(x = idx,size = dim(prova)[1]*0.8,replace=F)
# # Split data 80/20
#Miramos que variables numericas superan la prueva de simetria. Nos quedaremos la variable que cumplan la simetria 
# Determine skew for each numeric feature
subs$Semestre2<-as.factor(subs$Semestre2)
subs$DiaCompra3<-as.factor(subs$DiaCompra3)
str(subs)
#####################Skewness
subs2<-subs
subs2$CantidadTotal2<-log(subs2$CantidadTotal2+1)
library(moments)
library(MASS)
require(caret)

categ_feats<-names(subs2[sapply(subs2,is.factor)])
numeric_feats<-names(subs2[sapply(subs2,is.numeric)])
skewed_feats <- sapply(numeric_feats,function(x){skewness(subs2[[x]],na.rm=TRUE)})
skewed_feats
## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for(x in names(skewed_feats)) {
  bc=BoxCoxTrans(subs2[[x]],lambda = .15)
  subs2[[x]]=predict(bc,subs2[[x]])
  #combi[[x]] <- log(combi[[x]] + 1)
}

subs2<-cbind(subs2[numeric_feats],subs2[categ_feats])
str(subs2)


###########################################################################################
############################RED NEURONAL con caret cv.
##añadir barra que indica el tiempo que falta para acabar de ejecutar el modelo
install.packages("neuralnet")
library(neuralnet)

library(plyr) #para crear barra de progreso.
library(dplyr)

train <- subs %>% filter(anysalid != '2017' ) %>% as.data.frame()
test <- subs %>% filter(anysalid == '2017'  ) %>% as.data.frame()

##escalamos los datos
maxs <- apply(subs, 2, max) 
mins <- apply(subs, 2, min)

scaled <- as.data.frame(scale(subs, center = mins, scale = maxs - mins))

train_ <- scaled[subs$anysalid!='2017',]
test_ <- scaled[subs$anysalid=='2017',]

##red básica con 5 hidden layers y luego otra capa con 3.
n <- names(train_)
f <- as.formula(paste("CantidadTotal2 ~", paste(n[!n %in% "CantidadTotal2"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(8,5),linear.output=T,stepmax = 1e6,threshold = 0.001)

plot(nn)
head(nn)

pr.nn <- compute(nn,test_[,1:11])

pr.nn_ <- pr.nn$net.result*(max(subs$CantidadTotal2)-min(subs$CantidadTotal2))+min(subs$CantidadTotal2)
test.r <- (test_$CantidadTotal2)*(max(subs$CantidadTotal2)-min(subs$CantidadTotal2))+min(subs$CantidadTotal2)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

##ahora lo hacemos mediante la crosvalidación en 10 folds.
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(subs),round(0.9*nrow(subs)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(8,5),linear.output=T,stepmax = 1e6,threshold = 0.001)
  
  pr.nn <- compute(nn,test.cv[,1:11])
  pr.nn <- pr.nn$net.result*(max(subs$CantidadTotal2)-min(subs$CantidadTotal2))+min(subs$CantidadTotal2)
  
  test.cv.r <- (test.cv$CantidadTotal2)*(max(subs$CantidadTotal2)-min(subs$CantidadTotal2))+min(subs$CantidadTotal2)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}
#######################
#linear output que faci regressió.

save(nn,subs,subsdatos,file = "Datosyredbasica.RData")
##################################################################
# subs  <- subs %>% mutate(picos=ifelse(MedioD %in% c(7.9),1,0)) %>% select(everything()) %>% 
#          group_by(picos) %>% summarise(CantidadTotal2=sum(CantidadTotal2)) %>% as.data.frame()
# 
# ?everything
