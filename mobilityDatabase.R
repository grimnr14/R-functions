library(htmltools)
library(rvest)
library(stringr)
library(plyr)

setwd("C:/Users/chris/Downloads/")

gh<-read.csv("https://files.mobilitydatabase.org/feeds_v2.csv",header=T)
gh<-gh[gh$location.country_code=="US",]
download.file(gh[1,"urls.direct_download"],"temp.zip")#download direct OR
download.file(paste0("https://files.mobilitydatabase.org/",gh[1,"id"],"/latest.zip"),"temp.zip")#from mdb repo latest listed

gh<-gh[as.numeric(row.names(gh))>=as.numeric(row.names(gh[gh$id==i,])),]
#outs<-list()
for(i in gh$id){
  if(!is.character(tryCatch(
    expr={
      download.file(paste0("https://files.mobilitydatabase.org/",i,"/latest.zip"),"temp.zip")#from mdb repo latest listed
    },
    error=function(e){
      return("Missing at MDB, retrieving direct from source")
    }
  ))){#!="Missing at MDB, retrieving direct from source"){
    download.file(paste0("https://files.mobilitydatabase.org/",i,"/latest.zip"),"temp.zip")#from mdb repo latest listed
    unzip("temp.zip")
    input<-NULL
    for(j in c("trips","fares","stops","routes","shapes","calendar")){
      input<-ifelse(j=="trips"&file.size("trips.txt")!=0,"trips.txt",
                    ifelse(j=="fares"&file.size("fare_attributes.txt")!=0,"fare_attributes.txt",
                           ifelse(j=="stops"&file.size("stops.txt")!=0,"stops.txt",
                                  ifelse(j=="routes"&file.size("routes.txt")!=0,"routes.txt",
                                         ifelse(j=="shapes"&file.size("shapes.txt")!=0,"shapes.txt",
                                                ifelse(j=="calendar"&file.size("calendar.txt")!=0,"calendar.txt","agency.txt"
                                                ))))))
      if(file.size(input)!=0&input!="agency.txt"&nrow(read.csv(input,header=T))>0){
        outs[j][[1]]<-rbind.fill(outs[j][[1]],cbind(master_url=i,read.csv(input,header=T)))
      }
    }
    print(i)
  }else{
    if(!is.character(tryCatch(expr={
      download.file(gh[gh$id==i,"urls.direct_download"],"temp.zip")#download direct OR
    },
    error=function(e){
      return("Missing at source and skipped.")
    }
    ))){#!="Missing at source and skipped.")){
      download.file(gh[gh$id==i,"urls.direct_download"],"temp.zip")#download direct OR
      unzip("temp.zip")
      input<-NULL
      for(j in c("trips","fares","stops","routes","shapes","calendar")){
        input<-ifelse(j=="trips"&file.size("trips.txt")!=0,"trips.txt",
                      ifelse(j=="fares"&file.size("fare_attributes.txt")!=0,"fare_attributes.txt",
                             ifelse(j=="stops"&file.size("stops.txt")!=0,"stops.txt",
                                    ifelse(j=="routes"&file.size("routes.txt")!=0,"routes.txt",
                                           ifelse(j=="shapes"&file.size("shapes.txt")!=0,"shapes.txt",
                                                  ifelse(j=="calendar"&file.size("calendar.txt")!=0,"calendar.txt","agency.txt"
                                                  ))))))
        if(file.size(input)!=0&input!="agency.txt"&nrow(read.csv(input,header=T))>0){
          outs[j][[1]]<-rbind.fill(outs[j][[1]],cbind(master_url=i,read.csv(input,header=T)))
        }
      }
      print(i)
    }
  }
  #STACK OUTS!
}

outs$gtfs<-gh
saveRDS(outs,"gtfs_selected_2023.rds")
gtfs<-readRDS("gtfs_selected_2023.rds")
for(i in names(gtfs)){
  d<-gtfs[i][[1]]
  if(i=="trips"){
    d<-d[,c("master_url","route_id","service_id","trip_id","ï..trip_id","shape_id","fare_id","trip_headsign","peak_flag","peak_offpeak","wheelchair_accessible")]
    d$trip_id<-ifelse(is.na(d$trip_id),d$ï..trip_id,d$trip_id)
    d$peak_flag<-ifelse(is.na(d$peak_flag),d$peak_offpeak,d$peak_flag)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","route_id","service_id","trip_id","shape_id","fare_id","trip_headsign","peak_flag","wheelchair_accessible")]
  }
  if(i=="fares"){
    d<-d[,c("master_url","fare_id","ï..fare_id","agency_id","price","currency_type","payment_method")]
    d$fare_id<-ifelse(is.na(d$fare_id),d$ï..fare_id,d$fare_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","fare_id","agency_id","price","currency_type","payment_method")]
  }
  if(i=="stops"){
    d<-d[,c("master_url","stop_id","ï..stop_id","stop_name","stop_lat","stop_lon","wheelchair_boarding")]
    d$stop_id<-ifelse(is.na(d$stop_id),d$ï..stop_id,d$stop_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","stop_id","stop_name","stop_lat","stop_lon","wheelchair_boarding")]
  }
  if(i=="routes"){
    d<-d[,c("master_url","route_id","ï..route_id","agency_id","route_desc","route_color","route_type","alt_route_type","route_long_name")]
    d$route_id<-ifelse(is.na(d$route_id),d$ï..route_id,d$route_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","route_id","agency_id","route_desc","route_color","route_type","alt_route_type","route_long_name")]
  }
  if(i=="calendar"){
    d<-d[,c("master_url","service_id","ï..service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
    d$service_id<-ifelse(is.na(d$service_id),d$ï..service_id,d$service_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
  }
  if(i=="shapes"){
    d<-d[,c("master_url","shape_id","ï..shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")]
    d$shape_id<-ifelse(is.na(d$shape_id),d$ï..shape_id,d$shape_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")]
  }
  if(i=="gtfs"){
    trip<-aggregate(data=gtfs$trips[,c("master_url","shape_id","route_id","service_id","fare_id","peak_flag","wheelchair_accessible")],cbind(peak_flag,wheelchair_accessible)~.,FUN="max")#agg trip info by route and shape
    trip<-merge(trip,gtfs$routes[,c("master_url","route_id","route_desc","route_type","alt_route_type")],by=c("master_url","route_id"),all.x=T)
    trip<-merge(trip,gtfs$calendar[,c("master_url","service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")],by=c("master_url","service_id"),all.x=T)
    
    fare<-aggregate(data=gtfs$fares[,c("master_url","fare_id","agency_id","price","currency_type")],price~.,FUN="mean")
    fare<-merge(fare,aggregate(data=gtfs$fares[,c("master_url","fare_id","agency_id","price","currency_type")],price~.,FUN="sd"),by=c("master_url","fare_id","agency_id","currency_type"),all.x=T)
    names(fare)<-c("master_url","fare_id","agency_id","currency_type","mean_price","std_price")
    trip<-merge(trip,fare,by=c("master_url","fare_id"),all.x=T)
    
    shape<-aggregate(data=gtfs$shapes[,c("master_url","shape_id","shape_dist_traveled")],shape_dist_traveled~.,FUN="max")
    names(shape)<-c("master_url","shape_id","max_dist_traveled")
    trip<-merge(trip,shape,by=c("master_url","shape_id"),all.x=T)
    
    trip<-trip[!duplicated(trip),]
    trip$days_per_week<-trip$monday+trip$tuesday+trip$wednesday+trip$thursday+trip$friday+trip$saturday+trip$sunday
    trip$full_weekdays<-ifelse(trip$monday+trip$tuesday+trip$wednesday+trip$thursday+trip$friday==5,1,0)
    trip$full_weekends<-ifelse(trip$saturday+trip$sunday==2,1,0)
    
    #now aggregate to shape id level price, day of week, route types, peak flag and wheelchair access
    shape<-trip[,c("master_url","shape_id","peak_flag","wheelchair_accessible","route_type","alt_route_type","mean_price","std_price","currency_type","days_per_week","full_weekdays","full_weekends","max_dist_traveled")]
    shape<-shape[!duplicated(shape),]
    shape<-aggregate(data=shape,cbind(peak_flag,wheelchair_accessible,days_per_week,full_weekdays,full_weekends,max_dist_traveled)~master_url+shape_id,FUN="max")
    outs<-list(trip=trip,map=shape,stop=gtfs$stops,shape=gtfs$shapes)
  }
}
gtfs<-outs
saveRDS(gtfs,"gtfs_selected_2023.rds")

remove(shape,shape1,trip,fare,outs,d)
gc()

































#gh<-gh%>%html_nodes("div_id")
gh<-as.character(gh)
gh<-str_split(as.character(gh),"[}],[{]")
outs<-NULL
for(i in 1:length(gh[[1]])){
  outs<-rbind(outs,data.frame(row=i,text=gh[[1]][i]))
}
outs[1,"text"]<-substr(outs[1,"text"],str_locate(outs[1,"text"],"items\":")[2]+3,nchar(outs[1,"text"]))
outs[nrow(outs),"text"]<-substr(outs[nrow(outs),"text"],1,str_locate(outs[nrow(outs),"text"],"[}]")[1])


url<-"https://files.mobilitydatabase.org/mdb-2603/mdb-2603-20250430 #april 30 is latest update
  1618/mdb-2603-202504301618.zip"
mdb<-read_html(url)
mdb
mdb%>% html_nodes("div")




"https://github.com/MobilityData/mobility-database-catalogs/tree/main/catalogs/sources/gtfs/schedule"
"https://files.mobilitydatabase.org/mdb-2603/mdb-2603-202504301618/mdb-2603-202504301618.zip"
"https://files.mobilitydatabase.org/tld-5893/tld-5893-202505050123/tld-5893-202505050123.zip"



"https://raw.githubusercontent.com/MobilityData/mobility-database-catalogs/refs/heads/main/catalogs/sources/gtfs/schedule/au-australian-capital-territory-canberra-metro-operations-gtfs-935.json"




#refresh token
curl --location 'https://api.mobilitydatabase.org/v1/tokens' --header 'Content-Type: application/json' --data '{ "refresh_token": "AMf-vBzYGbnbWo02B38WqsvrPtoRjPB5eBUZ0_Y2kPuGVnzY2hMSXoJfUB1kxUYWggsfogJcofStU38hbPIyvwEMtfS0ok97LOTT6ffeUgJMhp6hbRXL-4fS9wUMn246sslwxXx5-e-mci9wA-BinbaRzpl1cKFMPUEm-Ts3oLwobIqjdiI04IZKFcXUzxhNPEVU03JL2MHtuopzfG6Vgf7VrM4zh2gfFRwJ1hXlK3CPqSLFZyCPPKuxiVF2b-dp5uUIs_guMH3W-Lu5E5RJIqdrvkCgytLUrJcWgWnD8oXh-fa9vFxAObgd-Jfs_xmTb3hwGDEtOSTQ4JM9c_iB4bZ7KpobJUlAtbC7KkC9alOzcf8LDSqiJ3A" }'
#access token
curl --location 'https://api.mobilitydatabase.org/v1/metadata' --header 'Accept: application/json' --header 'Authorization: Bearer eyJhbGciOiJSUzI1NiIsImtpZCI6IjNmOWEwNTBkYzRhZTgyOGMyODcxYzMyNTYzYzk5ZDUwMjc3ODRiZTUiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoiQ2hyaXN0b3BoZXIgS2l0Y2hlbiIsInBpY3R1cmUiOiJodHRwczovL2F2YXRhcnMuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3UvMTQyMDM5NTY_dj00IiwiaXNzIjoiaHR0cHM6Ly9zZWN1cmV0b2tlbi5nb29nbGUuY29tL21vYmlsaXR5LWZlZWRzLXByb2QiLCJhdWQiOiJtb2JpbGl0eS1mZWVkcy1wcm9kIiwiYXV0aF90aW1lIjoxNzQ2MTM1MDI1LCJ1c2VyX2lkIjoiTlEzS09BMGVpY2FjNHI2aG83YWcycFdsZmR3MSIsInN1YiI6Ik5RM0tPQTBlaWNhYzRyNmhvN2FnMnBXbGZkdzEiLCJpYXQiOjE3NDYxNDg0ODEsImV4cCI6MTc0NjE1MjA4MSwiZW1haWwiOiJja2l0Y2hlMkBqaC5lZHUiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZmlyZWJhc2UiOnsiaWRlbnRpdGllcyI6eyJnaXRodWIuY29tIjpbIjE0MjAzOTU2Il0sImVtYWlsIjpbImNraXRjaGUyQGpoLmVkdSJdfSwic2lnbl9pbl9wcm92aWRlciI6ImdpdGh1Yi5jb20ifX0.gzcEmc42OLgJdyRr9mVGnDW-Ugptp9vTCJIP2Eis75qtlWaM22j0gbcBFHrVciK9-2Uyi_Bfw2JO-uZvsDtVTqGb_nA1kwfTPjaVD4mqQ6CZ_6afjPvAAIvhSJ12J26NZig60Dwyt5k3faLM2So1y1lP-souc97swaN6SIO7RYV2TpDu8wjhS1B_JXp-FMz4VTXZ5j4jh6ZE_1zBoLxA37ctro9WDWoJJnXzel03SysRuV1Kh_8t9PL1SUQzpuuD_mxkTWtWlOqccK0om7wbZBihOwO5JyEt1BdjYvn4J1XvYhvaTWjWQJ_PFNfXCKNCasz7I833iBgfZ-YkmqFBYQ'

txt<-"curl -X 'GET' \
  'https://api.mobilitydatabase.org/v1/datasets/gtfs/mdb-1210-202402121801' \
  -H 'accept: application/json' \
  -H 'Authorization: Bearer eyJhbGciOiJSUzI1NiIsImtpZCI6IjU5MWYxNWRlZTg0OTUzNjZjOTgyZTA1MTMzYmNhOGYyNDg5ZWFjNzIiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoiQ2hyaXN0b3BoZXIgS2l0Y2hlbiIsInBpY3R1cmUiOiJodHRwczovL2F2YXRhcnMuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3UvMTQyMDM5NTY_dj00IiwiaXNzIjoiaHR0cHM6Ly9zZWN1cmV0b2tlbi5nb29nbGUuY29tL21vYmlsaXR5LWZlZWRzLXByb2QiLCJhdWQiOiJtb2JpbGl0eS1mZWVkcy1wcm9kIiwiYXV0aF90aW1lIjoxNzQ2NDQ3ODcyLCJ1c2VyX2lkIjoiTlEzS09BMGVpY2FjNHI2aG83YWcycFdsZmR3MSIsInN1YiI6Ik5RM0tPQTBlaWNhYzRyNmhvN2FnMnBXbGZkdzEiLCJpYXQiOjE3NDY0NjgwNjIsImV4cCI6MTc0NjQ3MTY2MiwiZW1haWwiOiJja2l0Y2hlMkBqaC5lZHUiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZmlyZWJhc2UiOnsiaWRlbnRpdGllcyI6eyJnaXRodWIuY29tIjpbIjE0MjAzOTU2Il0sImVtYWlsIjpbImNraXRjaGUyQGpoLmVkdSJdfSwic2lnbl9pbl9wcm92aWRlciI6ImdpdGh1Yi5jb20ifX0.PRTbfqar1uG_9hHBhmNu26ANWQlYacZ3OWsQ4pXL6EHaP90xtTE9mDf8MhvzHN2y95fDv0e60ojPHDTaVliilVOK3tyJwBuEoczp5n3P7ERxN37qN9szU1L31LUiVrnRs2BPNLWau7sQKo6QrZ_RpeIiV4fPZj-Tn8rtSs1KA0yuUtliNTfZhqP-z8a7viIz6W8v6h8vlxcQ19JBprTbOUSrRlco6EAn7UIJaKZjseHDIMTpyYj1IaxFYl_S-DVNZUtWkPSp_614zSpq2v5NlTBdVocz2MHKVeeCWb3W8SZ72AGGjvd8w1OsQjJKiJ5gW6GavV26mvWCE11IMcWKnA'"
system(txt)

setwd("C:/users/chris/OneDrive/Desktop/GeoHealth/scripts/")
library(curl)
library(rjson)

txt<-"curl --location 'https://api.mobilitydatabase.org/v1/tokens' --header 'Content-Type: application/json' --data ' \"refresh_token\": \"AMf-vBzYGbnbWo02B38WqsvrPtoRjPB5eBUZ0_Y2kPuGVnzY2hMSXoJfUB1kxUYWggsfogJcofStU38hbPIyvwEMtfS0ok97LOTT6ffeUgJMhp6hbRXL-4fS9wUMn246sslwxXx5-e-mci9wA-BinbaRzpl1cKFMPUEm-Ts3oLwobIqjdiI04IZKFcXUzxhNPEVU03JL2MHtuopzfG6Vgf7VrM4zh2gfFRwJ1hXlK3CPqSLFZyCPPKuxiVF2b-dp5uUIs_guMH3W-Lu5E5RJIqdrvkCgytLUrJcWgWnD8oXh-fa9vFxAObgd-Jfs_xmTb3hwGDEtOSTQ4JM9c_iB4bZ7KpobJUlAtbC7KkC9alOzcf8LDSqiJ3A\" '"
system(txt)

h<-new_handle(verbose=T)
handle_setopt(h,
              '"refresh_token": "AMf-vBzYGbnbWo02B38WqsvrPtoRjPB5eBUZ0_Y2kPuGVnzY2hMSXoJfUB1kxUYWggsfogJcofStU38hbPIyvwEMtfS0ok97LOTT6ffeUgJMhp6hbRXL-4fS9wUMn246sslwxXx5-e-mci9wA-BinbaRzpl1cKFMPUEm-Ts3oLwobIqjdiI04IZKFcXUzxhNPEVU03JL2MHtuopzfG6Vgf7VrM4zh2gfFRwJ1hXlK3CPqSLFZyCPPKuxiVF2b-dp5uUIs_guMH3W-Lu5E5RJIqdrvkCgytLUrJcWgWnD8oXh-fa9vFxAObgd-Jfs_xmTb3hwGDEtOSTQ4JM9c_iB4bZ7KpobJUlAtbC7KkC9alOzcf8LDSqiJ3A"'
              )
handle_setheaders(h,
                  "Content-Type: application/json"
                  )
con<-curl(url="https://api.mobilitydatabase.org/v1/tokens",
     open="r",
     handle=h
     )
con


handle_setheaders(h,
                  "Accept: "="application/json",
                  "Authorization: "="Bearer eyJhbGciOiJSUzI1NiIsImtpZCI6IjNmOWEwNTBkYzRhZTgyOGMyODcxYzMyNTYzYzk5ZDUwMjc3ODRiZTUiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoiQ2hyaXN0b3BoZXIgS2l0Y2hlbiIsInBpY3R1cmUiOiJodHRwczovL2F2YXRhcnMuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3UvMTQyMDM5NTY_dj00IiwiaXNzIjoiaHR0cHM6Ly9zZWN1cmV0b2tlbi5nb29nbGUuY29tL21vYmlsaXR5LWZlZWRzLXByb2QiLCJhdWQiOiJtb2JpbGl0eS1mZWVkcy1wcm9kIiwiYXV0aF90aW1lIjoxNzQ2MTM1MDI1LCJ1c2VyX2lkIjoiTlEzS09BMGVpY2FjNHI2aG83YWcycFdsZmR3MSIsInN1YiI6Ik5RM0tPQTBlaWNhYzRyNmhvN2FnMnBXbGZkdzEiLCJpYXQiOjE3NDYxNDg0ODEsImV4cCI6MTc0NjE1MjA4MSwiZW1haWwiOiJja2l0Y2hlMkBqaC5lZHUiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZmlyZWJhc2UiOnsiaWRlbnRpdGllcyI6eyJnaXRodWIuY29tIjpbIjE0MjAzOTU2Il0sImVtYWlsIjpbImNraXRjaGUyQGpoLmVkdSJdfSwic2lnbl9pbl9wcm92aWRlciI6ImdpdGh1Yi5jb20ifX0.gzcEmc42OLgJdyRr9mVGnDW-Ugptp9vTCJIP2Eis75qtlWaM22j0gbcBFHrVciK9-2Uyi_Bfw2JO-uZvsDtVTqGb_nA1kwfTPjaVD4mqQ6CZ_6afjPvAAIvhSJ12J26NZig60Dwyt5k3faLM2So1y1lP-souc97swaN6SIO7RYV2TpDu8wjhS1B_JXp-FMz4VTXZ5j4jh6ZE_1zBoLxA37ctro9WDWoJJnXzel03SysRuV1Kh_8t9PL1SUQzpuuD_mxkTWtWlOqccK0om7wbZBihOwO5JyEt1BdjYvn4J1XvYhvaTWjWQJ_PFNfXCKNCasz7I833iBgfZ-YkmqFBYQ"
                  )
curl(url="https://api.mobilitydatabase.org/v1/metadata/",
     open="rb",
     handle=h
     )

setwd("C:/users/chris/Downloads/")
url<-"https://files.mobilitydatabase.org/mdb-1210/mdb-1210-202402121801/mdb-1210-202402121801.zip"
download.file(url,"temp.zip")

url<-"https://files.mobilitydatabase.org/tld-5893/tld-5893-202505050123/tld-5893-202505050123.zip"
download.file(url,"temp.zip")

library(httr)
e<-oauth_endpoint(access="https://api.mobilitydatabase.org/v1/tokens",authorize="AMf-vBzYGbnbWo02B38WqsvrPtoRjPB5eBUZ0_Y2kPuGVnzY2hMSXoJfUB1kxUYWggsfogJcofStU38hbPIyvwEMtfS0ok97LOTT6ffeUgJMhp6hbRXL-4fS9wUMn246sslwxXx5-e-mci9wA-BinbaRzpl1cKFMPUEm-Ts3oLwobIqjdiI04IZKFcXUzxhNPEVU03JL2MHtuopzfG6Vgf7VrM4zh2gfFRwJ1hXlK3CPqSLFZyCPPKuxiVF2b-dp5uUIs_guMH3W-Lu5E5RJIqdrvkCgytLUrJcWgWnD8oXh-fa9vFxAObgd-Jfs_xmTb3hwGDEtOSTQ4JM9c_iB4bZ7KpobJUlAtbC7KkC9alOzcf8LDSqiJ3A")
oauth2.0_access_token(endpoint=e$authorize[1],app="https://api.mobilitydatabase.org/v1/tokens")
