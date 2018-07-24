library(jsonlite)
library(httr)
json<-GET(url = "http://api.eia.gov/category/?api_key=724b25cc097034b10d7d6ff14353b47b&category_id=40")
data<-fromJSON("http://api.eia.gov/category/?api_key=724b25cc097034b10d7d6ff14353b47b&category_id=40")
str(data)
class(data)
data$request$command
