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
setwd("/media/digitalhouse/Elements3/MTEySS/Local/grosati/Proyectos/googleplaces-api-r/cartografia")
dir<-dir(getwd(),".kml")
#dir<-dir[-2]

radios = readOGR("Muestra conurbano final.kml", "Muestra conurbano final")
#radios<-readOGR(dir[1],substr(dir[1],1,nchar(dir[1])-4))
#radios <- spTransform(radios, CRS("+init=epsg:3857"))
#radios$gid<-radios$link_INDEC
points <- gCentroid(radios, byid = TRUE)
points_coords<-points@coords
rm(radios,dir,points)

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
                urll<-paste(urll,"&pageToken", token, sep="")
                return(urll)
        }
        else {
                return(NULL)
        }
}

## Function that performs the query and resolves the pagination tokens
# url_query: url with the parameters to search
places_query<-function(url_query){
        iter<-0
        term_cond<-""
        sdoc<-list()
        data<-list()
        #x<-list()
        url_query<-url_query
        while(!is.null(term_cond)){
                iter<-iter+1
                #x[[iter]]<-url_query
                sdoc[[iter]]<-getURL(url_query) # EL PROBLEMA ESTA EN QUE NO ACTUALIZA ESTA URL
                data[[iter]]<-fromJSON(sdoc[[iter]])
                term_cond<-data[[iter]]$next_page_token
                url_query<-next_page(data[[iter]])
                #print(url_query)
                #if(iter==11){
                #        break
                #}
        }
        return(x)
}

key<-"AIzaSyCIsAa6qFbyca8ntlIjZTPedtGGAos-R8s"
i<-65
lat<-points_coords[i,2]
long<-points_coords[i,1]
urll<-format_url(lat,long,radius=5000,type="bank",key)
data<-places_query(urll)




data[3]


data[[4]]$next_page_token

term_cond<-data[[1]]$next_page_token

next_page(data[[1]])

xxx<-lapply(data,function(x){x$next_page_token})
duplicated(xxx)
data[[1]]$results$name

data[[1]]$results




sdoc[[1]]<-getURL(url)
data[[1]]<-fromJSON(sdoc[[1]])

sdoc[[2]]<-getURL(next_page(data[[1]]))
data[[2]]<-fromJSON(sdoc[[2]])

sdoc[[3]]<-getURL(next_page(data[[2]]))
data[[3]]<-fromJSON(sdoc[[3]])


next_page(data[[2]])

for i in nrow(points){
        lat<-points_coords[i,2]
        long<-points_coords[i,1]
        url<-format_url(lat,long)
        
        sdoc<-getURL(url)
        x<-fromJSON(sdoc)
        x$results$geometry
}




