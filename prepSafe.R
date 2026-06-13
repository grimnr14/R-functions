library(stringr)
library(tigris)
library(tidyr)

prepSafety<-function(year=2022,geography="county"){
  #read in county file for nibrs
  #counties<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/safety/county level nibrs clean.csv"),header=T)
  #counties<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/county%20level%20nibrs%20clean.csv"),header=T)
  counties<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/nibrs%20final%20counts%20clean.csv"),header=T)
  counties$fips<-str_pad(counties$fips,width=5,side="left",pad="0")
  counties<-counties[counties$year==year,]
  pop<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/population.csv"),header=T)
  pop$GEOID<-str_pad(pop$GEOID,width=5,side="left",pad="0")
  pop<-pop[pop$year==year&pop$geolevel=="county",]
  counties<-merge(counties,pop[pop$year==year,c("GEOID","B01001_001")],by.x="fips",by.y="GEOID",all.x=T)
  counties[,c(5:67,70:74)]<-100000*(counties[,c(5:67,70:74)]/counties$B01001_001)#county level pop determines rate
  
  #attach the correct geographies
  if(geography=="zcta"|geography=="tract"){
    if(year<2020){
      map2<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
      map2<-data.frame(tract=str_pad(map2$GEOID,side="left",width=11,pad="0"),zcta=str_pad(map2$ZCTA5,side="left",width=5,pad="0"))
      map2$fips<-substr(map2$tract,1,5)
      map2<-map2[!duplicated(map2),]
      if(geography=="zcta"){
        map2<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        counties<-merge(counties,map2,by="fips",all.x=T)
        out<-counties[,!names(counties) %in% c("STATENAME","B01001_001","service.counties","agencies","fips","counties")]
        out<-aggregate(data=out,.~year+zcta,FUN="mean")#rate is an average across intersecting areas 
        out$geoid<-out$zcta
        out<-out[,!names(out) %in% "zcta"]
        
      }else{
        zcta<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        counties<-merge(counties,zcta,by="fips",all.y=T)
        zcta<-counties[,!names(counties) %in% c("STATENAME","B01001_001","service.counties","agencies","fips","counties")]
        zcta<-aggregate(data=zcta,.~year+zcta,FUN="max")
        tract<-map2[!duplicated(map2[,c("zcta","tract")]),c("zcta","tract")]#rate is inherited from zcta level averages
        zcta<-merge(zcta,tract,by="zcta",all.y=T)
        tract<-aggregate(data=zcta[!is.na(zcta$`X09A`),!names(zcta) %in% "zcta"],.~year+tract,FUN="mean")
        tract$geoid<-tract$tract
        out<-tract[,!names(tract) %in% "tract"]
        remove(tract,zcta)
      }
    }else{
      map2<-read.table(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),sep="|",header=T)
      map2<-data.frame(tract=str_pad(map2$GEOID_TRACT_20,side="left",width=11,pad="0"),zcta=str_pad(map2$GEOID_ZCTA5_20,side="left",width=5,pad="0"))
      map2$fips<-substr(map2$tract,1,5)
      map2<-map2[!duplicated(map2),]
      if(geography=="zcta"){
        map2<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        counties<-merge(counties,map2,by="fips",all.x=T)
        out<-counties[,!names(counties) %in% c("STATENAME","B01001_001","service.counties","agencies","fips","counties")]
        out<-aggregate(data=out,.~year+zcta,FUN="mean")#rate is an average across intersecting areas 
        out$geoid<-out$zcta
        out<-out[,!names(out) %in% "zcta"]
      }else{
        zcta<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        counties<-merge(counties,zcta,by="fips",all.y=T)
        zcta<-counties[,!names(counties) %in% c("STATENAME","B01001_001","service.counties","agencies","fips","counties")]
        zcta<-aggregate(data=zcta,.~year+zcta,FUN="max")
        tract<-map2[!duplicated(map2[,c("zcta","tract")]),c("zcta","tract")]#rate is inherited from zcta level averages
        zcta<-merge(zcta,tract,by="zcta",all.y=T)
        tract<-aggregate(data=zcta[!is.na(zcta$X09A),!names(zcta) %in% "zcta"],.~year+tract,FUN="mean")
        tract$geoid<-tract$tract
        out<-tract[,!names(tract) %in% "tract"]
        remove(tract,zcta)
      }
    }
  }else{
    out<-counties[,!names(counties) %in% c("B01001_001","service.counties","agencies","counties")]
    out$geoid<-out$fips
    out<-out[,!names(out) %in% "fips"]
  }
  remove(counties,pop)
  gc()
  
  
  #cbp<-read.csv("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/economic/CBP/cbp county level 2022.csv",header=T)#REPLACE W GITHUBN LOC
  cbp<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/cbp%20county%20level%20",year,".csv"),header=T)
  cbp<-cbp[!duplicated(cbp),]
  cbp$loc<-ifelse(substr(cbp$NAICS2017,1,4)==4453,"liquor_store",
                  ifelse(substr(cbp$NAICS2017,1,4)==7225|substr(cbp$NAICS2017,1,4)==7224,"bar_cafe_restaurant",
                         ifelse(substr(cbp$NAICS2017,1,4)==7121,"park_museum_historical",
                                ifelse(substr(cbp$NAICS2017,1,5)==92212,"police",
                                       ifelse(substr(cbp$NAICS2017,1,4)==8131,"religious_org",
                                              ifelse(substr(cbp$NAICS2017,1,4)==8134|substr(cbp$NAICS2017,1,4)==8133,"advocacy_civic_service_org",
                                                     ifelse(substr(cbp$NAICS2017,1,4)==8139,"business_labor_political_org",
                                                            ifelse(substr(cbp$NAICS2017,1,5)==62191|substr(cbp$NAICS2017,1,6)==621910,"ambulance",
                                                                   ifelse(substr(cbp$NAICS2017,1,3)==622,"hospitals",
                                                                          ifelse(substr(cbp$NAICS2017,1,3)==624,"social_assist",
                                                                                 ifelse(substr(cbp$NAICS2017,1,5)==62133|substr(cbp$NAICS2017,1,6)==621111,"mental_health_prov",NA
                                                     )))))))))))
  cbp<-cbp[!is.na(cbp$loc),]
  cbp<-aggregate(data=cbp,ESTAB~GEO_ID+YEAR+loc,FUN="max")
  cbp<-tidyr::spread(data=cbp,key=loc,value=ESTAB,fill=0)
  cbp$GEO_ID<-substr(cbp$GEO_ID,nchar(cbp$GEO_ID)-4,nchar(cbp$GEO_ID))
  if(geography=="county"){
    out<-merge(out,cbp[!is.na(cbp$GEO_ID),],by.x="geoid",by.y="GEO_ID",all.x=T)
  }
  if(geography=="zcta"){
    cbp<-merge(cbp,map2,by.x="GEO_ID",by.y="fips",all.x=T)
    cbp<-aggregate(data=cbp[,!names(cbp) %in% c("GEO_ID")],.~zcta+YEAR,FUN="mean")
    out<-merge(out,cbp[!is.na(cbp$zcta),],by.x="geoid",by.y="zcta",all=T)
  }
  if(geography=="tract"){
    map2<-map2[,c("tract","fips")]
    map2<-map2[!duplicated(map2),]
    cbp<-merge(cbp,map2,by.x="GEO_ID",by.y="fips",all.x=T)
    cbp<-aggregate(data=cbp[,!names(cbp) %in% c("GEO_ID")],.~tract+YEAR,FUN="mean")
    out<-merge(out,cbp[!is.na(cbp$tract),],by.x="geoid",by.y="tract",all=T)
  }
  out<-out[!duplicated(out),]
  remove(cbp)
  gc()

  
#####  
  #get voter data----
  #vote<-read.csv("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/safety/cps/cps_voter_summary.csv",header=T)#REPLACE W GITHUB LOC
  vote<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/cps_voter_summary.csv"),header=T)
  names(vote)<-c("fips","year","per.elig.voted","per.registered")
  vote<-vote[vote$year==ifelse(substr(as.character(year),4,4) %in% c("1","3","5","7","9"),year-1,year),]
  vote$fips<-str_pad(vote$fips,width=5,side="left",pad="0")
  if(geography=="county"){
    out<-merge(out,vote,by.x="geoid",by.y="fips",all.x=T)
  }
  if(geography=="zcta"){
    vote<-merge(vote,map2,by.x="fips",by.y="fips",all.x=T)
    vote<-vote[,!names(vote) %in% c("fips")]
    vote<-vote[!duplicated(vote),]
    vote<-aggregate(data=vote,.~zcta+year,FUN="mean")
    #now merge pieces----
    out<-merge(out,vote,by.x="geoid",by.y="zcta",all=T)
    
  }
  if(geography=="tract"){
    vote<-merge(vote,map2,by.x="fips",by.y="fips",all.x=T)
    vote<-vote[,!names(vote) %in% c("fips")]
    vote<-vote[!duplicated(vote),]
    vote<-aggregate(data=vote,.~tract+year,FUN="mean")
    #now merge pieces----
    out<-merge(out,vote,by.x="geoid",by.y="tract",all=T)
    
  }
  
  #####
  #load wonder----
#  wonder<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/Underlying%20Cause%20of%20Death%2C%20",year,".csv"),header=T)
#  wonder$County.Code<-str_pad(wonder$County.Code,side="left",pad="0",width=5)
#  wonder<-wonder[,c("County.Code","Injury.Intent","Injury.Mechanism...All.Other.Leading.Causes","Deaths")]
#  wonder<-wonder[!duplicated(wonder)&!is.na(wonder$County.Code),]
#  wonder<-spread(data=wonder,key=Injury.Mechanism...All.Other.Leading.Causes,value=Deaths,fill=0)
#  gs<-wonder[wonder$Injury.Intent=="Homicide",c("County.Code","Firearm")]
#  homicide<-wonder[wonder$Injury.Intent=="Homicide",]
#  homicide$count<-rowSums(homicide[,c(3:ncol(homicide))])
#  homicide<-homicide[,c("County.Code","count")]
#  unintentional<-wonder[wonder$Injury.Intent=="Unintentional",c("County.Code","Poisoning")]
#  wonder<-merge(gs,homicide,by="County.Code",all=T)
#  wonder<-merge(wonder,unintentional,by="County.Code",all=T)
#  names(wonder)<-c("County.Code","firearm.death","homicide.death","unintentional.poisoning")
  
#  if(geography=="county"){
#    out<-merge(out,wonder,by.x="geoid",by.y="County.Code",all.x=T)
#  }
#  if(geography=="zcta"){
#    wonder<-merge(wonder,map2,by.x="County.Code",by.y="fips",all.x=T)
#    wonder<-wonder[,!names(wonder) %in% c("fips")]
#    wonder<-wonder[!duplicated(wonder),]
#    wonder<-aggregate(data=wonder,.~zcta,FUN="mean")
#    #now merge pieces----
#    out<-merge(out,wonder,by.x="geoid",by.y="zcta",all=T)
    
 # }
#  if(geography=="tract"){
#    wonder<-merge(wonder,map2,by.x="County.Code",by.y="fips",all.x=T)
#    wonder<-wonder[,!names(wonder) %in% c("fips")]
#    wonder<-wonder[!duplicated(wonder),]
#    wonder<-aggregate(data=wonder,.~tract,FUN="mean")
#    #now merge pieces----
#    out<-merge(out,wonder,by.x="geoid",by.y="tract",all=T)
    
#  }
  
  if(year<2021){
    year<-2021
  }
  atf<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/12",substr(year,3,4),"-ffl-list.csv"),header=T)
  atf<-atf[2:nrow(atf),c("PREMISE_STATE","PREMISE_ZIP_CODE","LICENSE_NAME")]
  atf$PREMISE_ZIP_CODE<-substr(atf$PREMISE_ZIP_CODE,1,5)
  atf<-atf[!duplicated(atf),]
  atf$val<-1
  atf<-aggregate(atf[,c("PREMISE_ZIP_CODE","PREMISE_STATE","val")],val~PREMISE_ZIP_CODE+PREMISE_STATE,FUN="sum")
  if(geography=="county"|geography=="tract"){
    if(year<2020){
      map2<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
      map2<-data.frame(tract=str_pad(map2$GEOID,side="left",width=11,pad="0"),zcta=str_pad(map2$ZCTA5,side="left",width=5,pad="0"))
      map2$fips<-substr(map2$tract,1,5)
      map2<-map2[!duplicated(map2),]
      if(geography=="county"){
        map2<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        atf<-merge(atf,map2[!is.na(map2$zcta),],by.x="PREMISE_ZIP_CODE",by.y="zcta",all.x=T)
        atf<-atf[!is.na(atf$val),names(atf) %in% c("fips","val")]
        atf<-aggregate(data=out,.~fips,FUN="mean")#rate is an average across intersecting areas 
        atf$geoid<-atf$fips
        atf<-atf[,!names(atf) %in% "fips"]
        
      }else{
        map2<-map2[!duplicated(map2[,c("zcta","tract")]),c("zcta","tract")]#rate is inherited from zcta level averages
        atf<-merge(atf,map2,by.x="PREMISE_ZIP_CODE",by.y="zcta",all.x=T)
        tract<-aggregate(data=atf[!is.na(atf[,"val"]),names(atf) %in% c("tract","val")],.~tract,FUN="mean")
        tract$geoid<-tract$tract
        atf<-tract[,!names(tract) %in% "tract"]
        remove(tract,zcta)
      }
    }else{
      map2<-read.table(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),sep="|",header=T)
      map2<-data.frame(tract=str_pad(map2$GEOID_TRACT_20,side="left",width=11,pad="0"),zcta=str_pad(map2$GEOID_ZCTA5_20,side="left",width=5,pad="0"))
      map2$fips<-substr(map2$tract,1,5)
      map2<-map2[!duplicated(map2),]
      if(geography=="county"){
        map2<-map2[!duplicated(map2[,c("fips","zcta")]),c("fips","zcta")]
        atf<-merge(atf,map2[!is.na(map2$zcta),],by.x="PREMISE_ZIP_CODE",by.y="zcta",all.x=T)
        atf<-atf[!is.na(atf$val),names(atf) %in% c("fips","val")]
        atf<-aggregate(data=atf,.~fips,FUN="mean")#rate is an average across intersecting areas 
        atf$geoid<-atf$fips
        atf<-atf[,!names(atf) %in% "fips"]
        
      }else{
        map2<-map2[!duplicated(map2[,c("zcta","tract")]),c("zcta","tract")]#rate is inherited from zcta level averages
        atf<-merge(atf,map2,by.x="PREMISE_ZIP_CODE",by.y="zcta",all.x=T)
        tract<-aggregate(data=atf[!is.na(atf[,"val"]),names(atf) %in% c("tract","val")],.~tract,FUN="mean")
        tract$geoid<-tract$tract
        atf<-tract[,!names(tract) %in% "tract"]
        remove(tract,zcta)
      }
    }
  }else{
    atf<-aggregate(data=atf,val~PREMISE_ZIP_CODE,FUN="sum")
    atf$geoid<-atf$PREMISE_ZIP_CODE
    atf<-atf[,c("val","geoid")]
  }
  names(atf)<-c("dealer","geoid")
  out<-merge(out,atf,by="geoid",all.x=T)
  
  out<-out[!duplicated(out),]
  out<-out[,c("geoid","robbery","assault","homicide","sexoffense","violent","advocacy_civic_service_org","ambulance","bar_cafe_restaurant","business_labor_political_org","hospitals","liquor_store","mental_health_prov","park_museum_historical","religious_org","social_assist","per.elig.voted","per.registered",
              #"firearm.death","homicide.death","unintentional.poisoning",
              "dealer")]
  remove(map2,vote,agencies,d,outs,wonder,atf)
  gc()
  out
}


#test<-prepSafety(year=2019,geography="zcta")
#test<-prepSafety(year=2019,geography="county")
#test<-prepSafety(year=2019,geography="tract")
#flat_map2(data=test,year=2019,state="MD",geography="zcta5",geoid="geoid",var="per.registered")
