library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(tigris)
library(censusxy)

geo_locate<-function(obj,id=NULL,dims=2,benchmark="Public_AR_Current",vintage="Current_Current",geography="county",input="polygon"){#accepts an sf object
  outs<-NULL
  obj$geometry<-st_zm(obj$geometry,drop=T)
  for(i in unique(as.data.frame(obj)[,id])){
    e<-obj[as.data.frame(obj)[,id]==i,"geometry"]
    #len_m<-st_length(e)
    if(input=="point"){
      e<-unlist(e$geometry)
      e<-matrix(e,nrow=length(e)/dims,byrow=T)
    }else{
      e<-unlist(st_boundary(e$geometry))
      e<-matrix(e,nrow=length(e)/dims)#2 points assumed on all, but may be 3
    }
    if(length(e)>0){
      for(n in 1:nrow(e)){
        out<-cxy_geography(lat=e[n,2],lon=e[n,1],benchmark=benchmark,vintage=vintage)#gives all points along lines
        out<-out[,c(str_detect(tolower(names(out)),geography)&str_detect(names(out),"GEOID"))]
        out<-out[1]
        outs<-plyr::rbind.fill(outs,data.frame(row=i,point=n,GEOID=out))#,length=len_m,lat=e[n,2],lon=e[n,1]))
      }
    }
  }
  outs
}

prepTransportation<-function(year=2023,geography="county",
                   uace="https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_uace_bg_fips_2022.csv",
                   zcta="https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_zcta_tract_fips_2020.txt"){
  map1<-read.csv(uace,header=T)
  ntd<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/shape_ntd_",year,".csv"),header=T)
  ntd<-ntd[!is.na(ntd$UACE10)&!duplicated(ntd),]
  ntd$GEOID<-str_pad(ntd$GEOID10,width=5,"left",pad="0")
  ntd<-merge(map1[,c("geoid","uace")],ntd,by.x="uace",by.y="GEOID",all.y=T)
  ntd$county<-ifelse(!is.na(ntd$geoid),substr(as.character(ntd$geoid),1,5),NA)
  ntd$tract<-ifelse(!is.na(ntd$geoid),substr(as.character(ntd$geoid),1,11),NA)
  
  map2<-read.table(zcta,sep="|",header=T)[,c(2,10)]
  map2$GEOID_TRACT_20<-str_pad(as.character(map2$GEOID_TRACT_20),width=11,side="left",pad=0)
  map2$GEOID_ZCTA5_20<-str_pad(as.character(map2$GEOID_ZCTA5_20),width=5,side="left",pad=0)
  ntd<-merge(map2,ntd,by.x="GEOID_TRACT_20",by.y="tract",all.x=T)
  ntd$tract<-ntd$GEOID_TRACT_20
  ntd$zcta<-ntd$GEOID_ZCTA5_20
  ntd<-ntd[!duplicated(ntd)&!is.na(ntd$uace),]

  fars<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/fars_shapes_",year,".csv"),header=T)
  fars$GEOID<-str_pad(fars$GEOID,width=15,side="left",pad=0)
  fars$tract<-substr(as.character(fars$GEOID),1,11)
  fars$county<-paste0(str_pad(as.character(trimws(fars$STATE)),width=2,side="left",pad="0"),
                      str_pad(as.character(trimws(fars$COUNTY)),width=3,side="left",pad="0"))
#  fars$geometry<-st_as_sf(as.data.frame(matrix(c(fars$LONGITUD,fars$LATITUDE),ncol=2)),coords=c("V1","V2"))
  fars<-merge(map2,fars,by.x="GEOID_TRACT_20",by.y="tract",all.x=T)
  fars<-fars[!duplicated(fars),]
  fars$tract<-fars$GEOID_TRACT_20
  fars$zcta<-fars$GEOID_ZCTA5_20
  
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
  ntd<-ntd[,!names(ntd) %in% c("per_facilities_prior1980","per_facilities_prior2000")]%>%
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
  transp$ua_voms_per_person<-transp$agency_voms#numerator only
  transp$ua_per_dr_service<-transp$bin_demandresponse/(transp$bin_bus+transp$bin_demandresponse+transp$bin_ferry+transp$bin_light_commuterail)
  transp$ua_per_do_service<-transp$bin_directly_operated/(transp$bin_directly_operated+transp$bin_purchased_transportation)
  transp$ua_voms_per_facility<-transp$agency_voms/transp$total_facilities
  transp$ua_oe_after_fares<-transp$total_operating_expenses-transp$fare_revenues_earned
  transp$ua_est_operating_cost_per_person<-transp$total_operating_expenses
  transp$ua_est_daily_cost<-24*transp$cost_per_hour#multiply acs feature for total riders
  transp$ua_cost_per_hour<-transp$cost_per_hour
  transp$ua_passengers_per_hour<-transp$passengers_per_hour
  transp$ua_est_daily_ridership<-24*transp$passengers_per_hour#multiply acs feature for total riders
  transp$ua_est_cost_per_passenger<-transp$cost_per_hour/transp$passengers_per_hour
  transp$ua_est_fare_per_person<-transp$fare_revenues_earned#numerator only
  transp$fatalmvapercapita<-transp$FATALS
  transp$fatalmvatrailerpercapita<-transp$TractorTrailor
  transp$fatalmvapedestrianstrikepercapita<-transp$PEDS
  transp$fatalintoxicationmvapercapita<-transp$driver_intox
  
  transp<-transp[!is.na(transp$geoid),c("geoid",names(transp)[str_detect(names(transp),"ua_")|str_detect(names(transp),"fatal")])]
  transp[is.na(transp)]<-0
  transp<-transp[!duplicated(transp),]
  transp
}


testing<-F
if(testing==T){
  ex<-prepTransportation(year=2022,geography="tract")
}

