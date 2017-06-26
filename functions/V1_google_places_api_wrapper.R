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
# query: type of query (string: "nearby", "radar", "place_details")
# place_id: if query="place_details", this parameter is obligatory
# key: Google API key


format_url<-function(query, key, type=NULL, lat=NULL, long=NULL, radius=NULL, place_id=NULL, rank_by=NULL){
        if(query=="nearby" & !is.null(rank_by)){
                return(paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", lat, ",", long, "&rankby=",rank_by,"&type=",type,"&key=",key,sep=""))
        }
        
        if(query=="radar"){
                return(paste("https://maps.googleapis.com/maps/api/place/radarsearch/json?location=", lat, ",", long, "&radius=",radius,"&type=",type,"&key=",key,sep=""))
        }
        if(query=="details"){
                return(paste("https://maps.googleapis.com/maps/api/place/details/json?placeid=", place_id, "&key=", key, sep=""))
        }
}

# Radar query
radar_query<-function(url_query){
        sdoc<-getURL(url_query) 
        data<-fromJSON(sdoc)
        return(data)
}

# Places details query
place_details_query<-function(url_query){
        sdoc<-getURL(url_query) 
        data<-fromJSON(sdoc)
        return(data)
}

# Places details query
near_by_dist_query<-function(url_query){
        sdoc<-getURL(url_query) 
        data<-fromJSON(sdoc)
        print(data$status)
        data<-data$results
        return(data)
}


#key<-"AIzaSyCIsAa6qFbyca8ntlIjZTPedtGGAos-R8s"
key<-"AIzaSyDwDbAEVCORrSKQFE5zwxLsqsITCTOyoIg"
ptm <- proc.time()
bank_dist<-list()
nrow_points<-nrow(points_coords)
for (i in 1:nrow_points){
        lat<-points_coords[i,2]
        long<-points_coords[i,1]
        print(paste("Iteration ", i, " of ", nrow_points, sep=""))
        url_dist<-format_url(query="nearby",key=key, type="bank", lat, long, rank_by = "distance")
        bank_dist[[i]]<-near_by_dist_query(url_dist)
}
proc.time() - ptm

url_dist<-format_url(query="nearby",key=key, type="bank", lat, long, rank_by = "distance")
data<-near_by_dist_query(url_dist)







#Examples of use
urll<-format_url(query="radar",key=key, type="bank", lat, long,radius=50000)
data<-radar_query(urll)

url_det<-format_url(query="details", key=key, place_id=data$results$place_id[1])
data_details<-place_details_query(url_det)

