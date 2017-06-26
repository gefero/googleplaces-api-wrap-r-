## Function that performs the query and resolves the pagination tokens
# url_query: url with the parameters to search
# Does not work yet...

places_query<-function(url_query){
        iter<-0
        token<-""
        sdoc<-list()
        data<-list()
        url_query2<-url_query
        while("next_page_token" %in% names(data[[iter]])){
                iter<-iter+1
                sdoc[[iter]]<-getURL(url_query2)
                data[[iter]]<-fromJSON(sdoc[[iter]])
                token<-data[[iter]]$next_page_token
                url_query2<-paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=", key,"&pagetoken=", token, sep="")
                #url_query2<-next_page(data[[iter]])
        }
        return(data)
}
