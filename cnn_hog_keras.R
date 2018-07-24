##KERAS + Openimage 
library(keras)
library(OpenImageR)
library(caret)
#bonsais
j<-list.files("C:/Users/David/Desktop/ImageREc/bonsai",pattern = ".jpg")
m<-list.files("C:/Users/David/Desktop/ImageREc/brain",pattern = ".jpg")

path = "C:/Users/David/Desktop/ImageREc/bonsai/"
path2 = "C:/Users/David/Desktop/ImageREc/brain/"

minim = min(length(j),length(m))

combof = matrix(nrow=minim,ncol=81)
combof2 = matrix(nrow=minim,ncol=81)


for (i in 1:minim){
image = readImage(paste0(path,j[i]))
image = image * 255

image2 = readImage(paste0(path,m[i]))
image2 = image2 * 255

hog = HOG(image, cells = 3, orientations = 9)
hog2 = HOG(image2, cells = 3, orientations = 9)

combof[i,] = hog
combof2[i,] = hog2
}

resp <- rep(c("bonsai","brain"),each=minim)

dataimage = data.frame(rbind(combof,combof2))
dataimage = cbind(dataimage,resp)
indexi = createDataPartition(dataimage$resp,times = 1,p = 0.7,list = FALSE)

train_x = dataimage[indexi,-82] %>% as.matrix()
test_x = dataimage[-indexi,-82] %>% as.matrix()

train_y = dataimage[indexi,82] %>% as.matrix()
train_y = ifelse(train_y=="bonsai",1,0)
train_y = to_categorical(train_y)
test_y = dataimage[-indexi,82]
test_y = ifelse(test_y=="bonsai",1,0)
#test_y = to_categorical(test_y)
##KERAS IMPLEMENTATION.

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 81, activation = "relu", input_shape = ncol(train_x)) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 10, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = "sigmoid")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  train_x, train_y, 
  epochs = 50, batch_size = 128, 
  validation_split = 0.2
)

model %>% predict_classes(test_x)
test_y
