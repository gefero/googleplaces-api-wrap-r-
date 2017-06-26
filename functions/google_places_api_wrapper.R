### QUERY BANKS IN GBA
library(MASS)
library(maptools)
library(rgeos)
library(sp)
library(spdep)
library(igraph)
library(ggmap)
library(rgdal)
library(ggrepel)
library(fpc)
library(cluster)
library(parallel)
library(doSNOW)
library(foreach)
library(plyr)
library(doParallel)
library(NbClust)
options(stringsAsFactors = F)

# Radiuos centroid generation of ENAPROSS II
setwd("/media/digitalhouse/Elements3/MTEySS/Local/grosati/Proyectos/Entorno ENAPROSS III/Cartografia")
dir<-dir(getwd(),".kml")
#dir<-dir[-2]

radios = readOGR("Muestra conurbano final.kml", "Muestra conurbano final")
#radios<-readOGR(dir[1],substr(dir[1],1,nchar(dir[1])-4))
#radios <- spTransform(radios, CRS("+init=epsg:3857"))
#radios$gid<-radios$link_INDEC
points <- gCentroid(radios, byid = TRUE)
points_coords<-points@coords

library(RCurl)
library(jsonlite)
library(geosphere)



## Function formatting url for google places api
# lat: latitude
# long: longitude
# radius: search radius
# type: type of establishment to serch (see https://developers.google.com/places/web-service/search?hl=es-419)
# key: Google API key
format_url<-function(lat,long,radius,type,key){
        return(paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", lat, ",", long, "&radius=",radius,"&type=",type,"&key=",key,sep=""))
}

## Function to get url of next page token
next_page<-function(json){
        if (!is.null(json$next_page_token)){
                token<-json$next_page_token
                url<-paste(url,"&pageToken",token,sep="")
                return(url)
        }
        else{
                return(NULL)
        }
}


key<-"AIzaSyCIsAa6qFbyca8ntlIjZTPedtGGAos-R8s"
i<-54
lat<-points_coords[i,2]
long<-points_coords[i,1]
url<-format_url(lat,long,radius=500,type="bank",key)
sdoc<-list()
data<-list()
sdoc[[1]]<-getURL(url)
data[[1]]<-fromJSON(sdoc[[1]])


next_page(data[[1]])





for i in nrow(points){
        lat<-points_coords[i,2]
        long<-points_coords[i,1]
        url<-format_url(lat,long)
        
        sdoc<-getURL(url)
        x<-fromJSON(sdoc)
        x$results$geometry
}




