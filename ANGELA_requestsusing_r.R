## Make request with R.
library(httr)
library(RJSONIO)
library(readr)
#library(jsonlite)

#r <- GET("https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyD7qyMHVpIook1E_2N5maG-i7yFc72w0Xc")

farma_geo <- read_delim("C:/Users/David/Downloads/farma_geo.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#WITH readClipb to avoid accentos or with read csv2
farma_geo <- read.csv2("C:/Users/David/Downloads/farma_geo.csv")

list_geocode = gsub(pattern = " ",replacement = "+",farma_geo$dir)
data_def = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("lat","long"))
for(i in 1:length(list_geocode)){
r <- GET(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",list_geocode[i],"&key=AIzaSyD7qyMHVpIook1E_2N5maG-i7yFc72w0Xc"))
##conversión del objeto a txt
raise = content(r, as="text")
##conversion to list using RJSONIO
new <- fromJSON(raise)
#print(i)
##parseo guarro haciendo entrada en los elementos del JSON que ahora es una lista multiindexada
if(length(new$results)!=0){
  print(i)
  data_def[i,1]=unlist(new$results[[1]]$geometry$location)[1]
  data_def[i,2]=unlist(new$results[[1]]$geometry$location)[2]
  }

}
## NA o negativos no son correctos.
apply(data_def,2,function(x) sum(is.na(x) | x < 0))

data_def$Localiz_buscada = farma_geo$dir
data_def$nom = farma_geo$nom

save(file = "C:/Users/David/Desktop/RUTILS/localizaciones_farmageo.RData",data_def)
#Funciona
#r <- GET("https://maps.googleapis.com/maps/api/geocode/json?address=FRANCESC+MACIA+PS+86+RUBÍ+08191&key=AIzaSyD7qyMHVpIook1E_2N5maG-i7yFc72w0Xc")


load("C:/Users/David/Desktop/RUTILS/localizaciones_farmageo.RData")


#A successful request always returns a status of 200. Common errors are 404 (file not found) and 403 (permission denied). If you’re talking to web APIs you might 
#also see 500, which is a generic failure code
headers(r)
# nos dice si se ha realizado bien el request
http_status(r)
# en caso de fallar podemos lanzar excepción mediante:
warn_for_status(r)
stop_for_status(r)


content(r, "text")
content(r, "text", encoding = "ISO-8859-1")

str(content(r))
## parsea el json en listas con nombre
str(content(r, "parsed"))

##conversión del objeto a txt
raise = content(r, as="text")
##conversion to list using RJSONIO
new <- fromJSON(raise)

##parseo guarro haciendo entrada en los elementos del JSON que ahora es una lista multiindexada
new$results[[1]]$geometry$location
