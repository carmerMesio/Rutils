##Función para generar el Json de la api proporcionada.
install.packages("jsonlite")
require(jsonlite)

Data<-fromJSON(txt = "http://api.eia.gov/category/?api_key=724b25cc097034b10d7d6ff14353b47b&category_id=40")

Data$category$childseries$series_id
categ<-c("-ALL.A","-COM.A","-RES.A","-IND.A","-OTH.A","-TRA.A")

#por cada categ tenemos que extraer todos los anuales por cada categoria.

Anuales<-Data$category$childseries$series_id[grep("[.A]$",x=Data$category$childseries$series_id)]
Anuales<-gsub("\\-.*","",x = Anuales)
url<-"http://api.eia.gov/series/?api_key=724b25cc097034b10d7d6ff14353b47b&series_id="
Base<-matrix(ncol = 2)
nombres<-vector()
categoria<-vector()

for(j in 1:length(categ)){
  for(i in 1:length(Anuales)){
BaseDatos<-fromJSON(paste0(url,Anuales[i],categ[j]))
if(is.null(names(BaseDatos$data))){
nombres<-c(nombres,BaseDatos$series$geography,BaseDatos$series$geography)
categoria<-c(categoria,categ[j],categ[j])
Base2<-matrix(unlist(BaseDatos$series$data),byrow = FALSE,ncol = 2)
Base2<-Base2[Base2[,1] %in% c("2016","2015"),]
Base<-rbind(Base,Base2)
}
}
}

BaseF<-cbind(categoria,nombres,Base[-1,])

library(dplyr)
library(tidyr)
BASE<-as.data.frame(BaseF)
names(BASE)<-c("Categoria","Estados","Ano","Precio")
library(plyr)
BASE$Categoria<-mapvalues(BASE$Categoria,c("-ALL.A","-COM.A","-RES.A","-IND.A","-OTH.A","-TRA.A"),c("All","Comercial","Residen","Industrial","Other","Transp"))
detach("package:plyr",unload = TRUE)

View(BASE)

BASE$Estados<-as.character(BASE$Estados)
##Entra en cada una de las categorias y estados y guarda para el año 2015 y 2016 el valor.
##Ahora utilizare los datos proporcionados, ya que,en la api se incluyen regiones de los EUA que no se proporcionan en la BBDD.
##################################################################################################
##Leo la BDD. Mas eficiene con fread de data table.
library(readr)
US_Dexma <- read_delim("C:/Users/David/Desktop/US_energy_Dexma_test.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

##Separamos los años en dos columnas.
USDexma<-spread(US_Dexma,key = year,value = price)
##Ordenación del data frame por la diferencia de incremento de precio (2016-2015). En caso de dos valores idénticos utilizamos el nombre del estado.
USDexma<-USDexma %>% mutate(VarPr=`2016`-`2015`) %>% arrange(desc(VarPr),state) %>% as.data.frame()

View(USDexma)
#################################CLUSTER##############################################################
library(ggplot2)
library(cluster)
library(dendextend)
library(caret)
library(gridExtra)
library(grid)
library(ggthemes)
library(RColorBrewer)
library(ggdendro)
library(factoextra)
library(NbClust)

ggplot(USDexma,aes(substr(state,1,4),VarPr,color=as.factor(sector))) + geom_point()
##Seria interesante quitar others porque aportan nula información aunque sin conocer el objetivo de la prueva lo mantendre.
USDexma %>% filter(sector!="Other") %>% 
ggplot(.,aes(substr(state,1,4),VarPr,color=as.factor(sector))) + geom_point()

##################################################
##Metodo de elbow para hallar el numero optimo de clusters. Vamos calculando la varianza y vemos en que punto el angulo de caida deja de pronunciarse.
##Asi hallaremos en que punto la varianza explicada deja de ser crucial en relación al numero de clusters utilizado.

numvars<-USDexma[,sapply(USDexma,function(x){is.numeric(x)})]

k.max<-15
wss <- sapply(1:k.max, 
              function(k){kmeans(numvars, k, nstart=50,iter.max = 15 )$tot.withinss})
wss #el primer cluster recoge la mayor parte de la variablilidad entre clusters. Vemos que a partir del 5 el peso es muy reducido.
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#claramente observamos 3 clusters, aunque podriamos utlizar 4. Decido hacer 4 debido a que un 
#cluster pertenece al sector industrial y transportes.
# Por otro lado se observa otro cluster referente al sector residencial.
#Es interesante el estado de Hawaii que presenta precios elevados en cuanto a la electricidad y como es el estado donde
#se ha producido una mayor diferencia entre los años 2015 y 2016.

#variables categoricas sector y estado.
catvar<-USDexma %>% select_if(is.character) %>% mutate_all(as.factor)


##Dissimilarity Matrix Calculation
####### Dendograma vamos a emplear la información recogida del elbow method.
dd <- dist(scale(numvars), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE) + 
  xlab('') + ylab('') + 
  theme_fivethirtyeight() +
  ggtitle('Dendograma') + 
  theme(axis.text.x = element_text(size=7))

##partimos un grid con difernte numero de clusters. Añadimos los graficos a una list para pintarlos luego.
list_plot<-list()
for(i in 2:6){
  grp<-cutree(hc, k=i)
  list_plot[[i-1]]<-fviz_cluster(list(data = numvars, cluster = grp)) + 
    ggtitle('')
}
grid.arrange(list_plot[[2]],list_plot[[3]],list_plot[[4]],list_plot[[5]],ncol=2)
list_plot[[1]] #dos clusters

##Ahora compararemos los clusters con las variables categoricas que tenemos.

#Del paquete NbClust: corte optimo calcualado por voto mayoritrario entre 30 indices. FALLA MIRAR
require(NbClust)
nb <- NbClust(scale(numvars), distance = "euclidean", min.nc = 2,max.nc = 6, method = "ward.D2", index ="all")

##GRAFICOS PARA EXPLICAR LOS CLUSTERS
####################################

ggplot(datacl,aes(kmm.cluster,price,shape=sector))+geom_jitter()
#Un cluster hace referencia al sector other debido a que son todo zeros.
#Otro cluster es principalmente del sector industrial con precios entre 5 i 9. Con algo del sector transportes
#Un tercer cluster hace referencia ha una sintesis del sector residencial y comercial

ggplot(datacl,aes(kmm.cluster,price,color=state))+geom_jitter()
#Con tantos estados no se observa nada. Es necesario crear agrupaciones y observar que pasa por regiones.
#También es interesante analizar Hawaii porque tiene un comportamiento diferenciado del resto (isla).

##Cojemos un corte de 3 clusters y vamos a pintar las variables categoricas y los clusters para ver si existe algun patron.
#k=3
grp<-cutree(hc, k=3)
numvars$group<-grp
#rebind ratings
numvars$state<-USDexma$state
numvars$sector<-USDexma$sector

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("#78B7C5", "#9C964A", "#E1AF00", "#F21A00")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + geom_jitter(position = "jitter")+ 
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)

grp<-cutree(hc, k=4)
numvars$group<-grp

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("#78B7C5", "#9C964A", "#E1AF00", "#F21A00")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + 
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)

grp<-cutree(hc, k=6)
numvars$group<-grp
#rebind ratings
numvars$state<-USDexma$state
numvars$sector<-USDexma$sector

g1<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(group)),size=3) + 
  scale_color_manual(name="clusters",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

g2<-ggplot() + geom_point(data=numvars,aes(x=`2015`,y=VarPr,color=factor(sector)),size=3) + geom_jitter()+
  scale_color_manual(name="sector",values = c("yellow", "blue", "purple", "green","red","grey")) + 
  theme_fivethirtyeight() + 
  theme(legend.position="top",legend.text=element_text(size=6)) + scale_y_continuous(limits = c(-4, 4))

grid.arrange(g1,g2,ncol=2)

