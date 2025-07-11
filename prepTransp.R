library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(tigris)
#library(censusxy)

prepTransportation<-function(year=2023,geography="county"
                   ){
  year.map<-ifelse(year>=2020,"2020","2010")
  zcta=paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_zcta_bg_fips_",year.map,".csv")
  uace.year<-ifelse(year>=2020,"2022","2019")
  uace<-paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_uace_bg_fips_",uace.year,".csv")
  #map1<-read.csv(zcta,header=T,sep="|")
  map2<-read.csv(zcta,header=F,sep=",")
  names(map2)<-c("GEOID","LAT","LONG","parse","ZCTA","NAME")
  #map2$GEOID<-map2[,str_detect(names(map2),"GEOID_TRACT")]
  #map2$ZCTA<-map2[,str_detect(names(map2),"GEOID_ZCTA5_")]
  map2<-map2[,c("GEOID","ZCTA")]
  map2<-map2[!duplicated(map2),]
  map2$GEOID<-str_pad(as.character(map2$GEOID),width=12,side="left",pad="0")
  map2$GEOID<-substr(map2$GEOID,1,11)
  map2$ZCTA<-str_pad(as.character(map2$ZCTA),width=5,side="left",pad="0")
  map2<-map2[!duplicated(map2),]

  
  map1<-read.csv(uace,header=T)
  ntd<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/shape_ntd_",year,".csv"),header=T)
  ntd<-ntd[!is.na(ntd$UACE10)&!duplicated(ntd),]
  ntd$GEOID<-str_pad(ntd$GEOID10,width=5,"left",pad="0")
  ntd<-merge(map1[,c("geoid","uace")],ntd,by.x="uace",by.y="GEOID",all.y=T)
  ntd$county<-ifelse(!is.na(ntd$geoid),substr(as.character(ntd$geoid),1,5),NA)
  ntd$tract<-ifelse(!is.na(ntd$geoid),substr(as.character(ntd$geoid),1,11),NA)
  
  #map2<-read.table(zcta,sep="|",header=T)[,c(2,10)]
  #map2$GEOID_TRACT_20<-str_pad(as.character(map2$GEOID_TRACT_20),width=11,side="left",pad=0)
  #map2$GEOID_ZCTA5_20<-str_pad(as.character(map2$GEOID_ZCTA5_20),width=5,side="left",pad=0)
  
  ntd<-merge(map2,ntd,by.x="GEOID",by.y="tract",all.x=T)
  ntd$tract<-ntd$GEOID
  ntd$zcta<-ntd$ZCTA
  ntd<-ntd[!duplicated(ntd)&!is.na(ntd$uace),]

  fars<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/fars_shapes_",year,".csv"),header=T)
  fars$GEOID<-str_pad(fars$GEOID,width=15,side="left",pad=0)
  fars$tract<-substr(as.character(fars$GEOID),1,11)
  fars$county<-paste0(str_pad(as.character(trimws(fars$STATE)),width=2,side="left",pad="0"),
                      str_pad(as.character(trimws(fars$COUNTY)),width=3,side="left",pad="0"))
#  fars$geometry<-st_as_sf(as.data.frame(matrix(c(fars$LONGITUD,fars$LATITUDE),ncol=2)),coords=c("V1","V2"))
  fars<-merge(map2,fars,by.x="GEOID",by.y="tract",all.x=T)
  fars<-fars[!duplicated(fars),]
  fars$tract<-fars$GEOID
  fars$zcta<-fars$ZCTA
  
  #roll up
  ntd$geoid<-ntd[,geography]
  ntd<-ntd[!is.na(ntd$uace),c("geoid",
              "agency_voms",
              "total_facilities","per_facilities_prior1980","per_facilities_prior2000",
              "bin_bus","bin_demandresponse","bin_light_commuterail","bin_ferry","bin_directly_operated","bin_purchased_transportation",
              "fare_revenues_earned","total_operating_expenses",
              "cost_per_hour",
              "passengers_per_hour"
              )]
  ex<-ntd[,c("geoid","per_facilities_prior1980","per_facilities_prior2000")]
  ex<-ex%>%
    group_by(geoid)%>%
    summarise_each(funs=c("max"))
  ntd<-ntd[,!names(ntd) %in% c("per_facilities_prior1980","per_facilities_prior2000")]
  ntd<-ntd[!duplicated(ntd)&!is.na(ntd$geoid),]#exclude entries that are exact duplicates
  ntd<-ntd%>%
    group_by(geoid)%>%
    summarise_each(funs=c("sum"))
  ntd<-merge(ntd,ex,by="geoid")
  ntd<-ntd[!duplicated(ntd),]
  
  fars$geoid<-fars[,geography]
  fars<-fars[!is.na(fars$ST_CASE),c("geoid",
                                    "FATALS","PEDS",
                                    "TractorTrailor",
                                    "driver_bac","driver_drug_detect"
                                    )]
  fars$driver_intox<-ifelse((fars$driver_bac>80&!is.na(fars$driver_bac))|
                              (fars$driver_drug_detect==1&!is.na(fars$driver_drug_detect)),1,0)
  
  fars<-fars[,c("geoid","FATALS","PEDS","TractorTrailor","driver_intox")]%>%
    group_by(geoid)%>%
    summarise_each(funs=c("sum"))#sum across incidents
  transp<-merge(ntd,fars,by="geoid",all=T)
  remove(fars,ntd)
  gc()
  
  transp$ua_per_facilities_prior1980<-transp$per_facilities_prior1980
  transp$ua_per_facilities_after2000<-100-transp$per_facilities_prior2000
  transp$ua_total_voms<-transp$agency_voms
  #transp$ua_voms_per_person<-transp$agency_voms#numerator only
  transp$ua_per_dr_service<-transp$bin_demandresponse/(transp$bin_bus+transp$bin_demandresponse+transp$bin_ferry+transp$bin_light_commuterail)
  transp$ua_per_do_service<-transp$bin_directly_operated/(transp$bin_directly_operated+transp$bin_purchased_transportation)
  #transp$ua_voms_per_facility<-transp$agency_voms/transp$total_facilities
  #transp$ua_oe_after_fares<-transp$total_operating_expenses-transp$fare_revenues_earned
  transp$ua_est_operating_cost_per_person<-transp$total_operating_expenses
  #transp$ua_est_daily_cost<-24*transp$cost_per_hour#multiply acs feature for total riders
  #transp$ua_cost_per_hour<-transp$cost_per_hour
  #transp$ua_passengers_per_hour<-transp$passengers_per_hour
  transp$ua_est_daily_ridership<-24*transp$passengers_per_hour#multiply acs feature for total riders
  #transp$ua_est_cost_per_passenger<-transp$cost_per_hour/transp$passengers_per_hour
  #transp$ua_est_fare_per_person<-transp$fare_revenues_earned#numerator only
  transp$fatalmvapercapita<-transp$FATALS
  #transp$fatalmvatrailerpercapita<-transp$TractorTrailor
  transp$fatalmvapedestrianstrikepercapita<-transp$PEDS
  transp$fatalintoxicationmvapercapita<-transp$driver_intox
  
  transp<-transp[!is.na(transp$geoid),c("geoid",names(transp)[str_detect(names(transp),"ua_")|str_detect(names(transp),"fatal")])]
  transp[is.na(transp)]<-0
  transp<-transp[!duplicated(transp),]
  transp
}


testing<-F
if(testing==T){
  ex<-prepTransportation(year=2022,geography="zcta")
}

