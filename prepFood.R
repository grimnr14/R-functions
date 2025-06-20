#retrieve data from github and convert to full fips and zcta mapping
#requires fips->zcta mapping file
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(tigris)
library(censusxy)

prepFood<-function(year=2019,geography="county"#,
                   #zcta="https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_zcta_tract_fips_2020.txt",
                   #year.map=2010
                   ){
  fara.year<-ifelse(year>=2019,"2019",
                    ifelse(year>=2015,"2015","2010"))
  fea.year<-ifelse(year>=2019,"2019",
                   ifelse(year>=2017,"2017",
                          ifelse(year>=2015,"2015",
                                 ifelse(year>=2014,"2015",
                                        ifelse(year>=2012,"2012","2011")))))
  year.map<-ifelse(year>=2020,"2020","2010")
  if(year.map=="2020"){
    fips.convert<-read.table("https://www2.census.gov/geo/docs/maps-data/data/rel2020/blkgrp/tab20_blkgrp20_blkgrp10_natl.txt",header=T,sep="|")[,c("GEOID_BLKGRP_20","GEOID_BLKGRP_10")]
    fips.convert$GEOID_BLKGRP_20<-str_pad(fips.convert$GEOID_BLKGRP_20,width=12,side="left",pad="0")
    fips.convert$GEOID_BLKGRP_10<-str_pad(fips.convert$GEOID_BLKGRP_10,width=12,side="left",pad="0")
    fips.convert$tract10<-substr(fips.convert$GEOID_BLKGRP_10,1,11)
    fips.convert$tract20<-substr(fips.convert$GEOID_BLKGRP_20,1,11)
    fips.convert<-fips.convert[,c("tract10","tract20")]
    fips.convert<-fips.convert[!duplicated(fips.convert),]
  }
  if(geography=="zcta"){
    zcta=paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_zcta_bg_fips_",year.map,".csv")
    #map1<-read.csv(zcta,header=T,sep="|")
    map1<-read.csv(zcta,header=T,sep=",")
    names(map1)<-c("GEOID","LAT","LONG","parse","ZCTA","NAME")
    #map1$GEOID<-map1[,str_detect(names(map1),"GEOID_TRACT")]
    #map1$ZCTA<-map1[,str_detect(names(map1),"GEOID_ZCTA5_")]
    map1<-map1[,c("GEOID","ZCTA")]
    map1<-map1[!duplicated(map1),]
    map1$GEOID<-str_pad(as.character(map1$GEOID),width=12,side="left",pad="0")
    map1$GEOID<-substr(map1$GEOID,1,11)
    map1$ZCTA<-str_pad(as.character(map1$ZCTA),width=5,side="left",pad="0")
    map1<-map1[!duplicated(map1),]
    
    if(year>=2020){
      map1<-merge(map1,fips.convert,by.x="GEOID",by.y="tract20",all.x=T)
      map1$GEOID<-map1$tract10
      map1<-map1[!is.na(map1$GEOID),c("GEOID","ZCTA")]
      map1<-map1[!duplicated(map1),]
    }
    
  }
  #pop<-pullACS(geography=geography,year=year,geometry=F)
  fara<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FARA%20estimates%20",fara.year,".csv"),header=T)
  #read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FARA%20estimates%202019.csv",header=T)
  fara<-fara[fara$year==fara.year,]
  fara<-as.data.frame(sapply(fara,as.numeric))
  fara$CensusTract<-str_pad(as.character(fara$CensusTract),width=11,side="left",pad="0")
  if(year>=2020){
    fara<-merge(fara,fips.convert,by.x="CensusTract",by.y="tract10",all.x=T)
    fara$CensusTract<-fara$tract20
    fara<-fara[,!names(fara) %in% c("tract10","tract20")]
    fara<-fara[!duplicated(fara),]
    fara<-fara%>%
      group_by(CensusTract)%>%
      summarise_each(funs=c("mean"))
    fara<-fara[!duplicated(fara),]
  }
  fara$POP2010<-(fara$lapophalf/(fara$lapophalfshare/100))

  if(geography=="county"){
    fara$CensusTract<-substr(fara$CensusTract,1,5)
    fara[is.na(fara)]<-0
    fara<-fara%>%
      group_by(CensusTract)%>%
      summarise_each(funs="sum")
  }
  if(geography=="zcta"){
    fara<-merge(fara,map1,by.x="CensusTract",by.y="GEOID",all.x=T)
    fara<-fara[!is.na(fara$ZCTA),]
    fara$CensusTract<-fara$ZCTA
    fara<-fara[,!names(fara) %in% c("ZCTA")]
    fara[is.na(fara)]<-0
    fara<-fara%>%
      group_by(CensusTract)%>%
      summarise_each(funs="sum")
  }
  fara[is.na(fara)]<-0
#  fara<-merge(fara,pop,by.x="CensusTract",by.y="GEOID",all.x=T)#we can use current year pop assuming static rate to calc estimated counts of residents by year
  
  vars<-c("CensusTract","POP2010",
          "lapophalf","lakidshalf","laseniorshalf","lasnaphalf",
          "lapop1","lakids1","laseniors1","lasnap1",
          "lapop10","lakids10","laseniors10","lasnap10",
          "lapophalfshare","lakidshalfshare","laseniorshalfshare","lasnaphalfshare",
          "lapop1share","lakids1share","laseniors1share","lasnap1share",
          "lapop10share","lakids10share","laseniors10share","lasnap10share"
          )
  fara<-fara[,vars]

  fea<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FoodEnvironmentAtlas",fea.year,".csv"),header=T)
  rnk<-as.numeric(substr(names(fea),nchar(names(fea))-1,nchar(names(fea))))
  rnk<-ordered(rnk)
  rnk<-levels(rnk)[(length(levels(rnk))-2):length(levels(rnk))]
  fea<-fea[,c("FIPS","State","County",names(fea)[substr(names(fea),nchar(names(fea))-1,nchar(names(fea))) %in% rnk])]
  names(fea)<-gsub("[0-9_]","",names(fea))
  fea$FIPS<-str_pad(as.character(fea$FIPS),width=5,side="left",pad="0")
  fea<-fea[,!duplicated(names(fea))]
  #the PTH features are all rates per 1,000 residents in corresponding year population from file
  fea$yearPop<-1000*fea$GROC/fea$GROCPTH
  if(geography=="county"){
    fea$GEOID<-fea$FIPS
  }
  if(geography=="zcta"){
    map2<-map1
    map2$GEOID<-substr(map2$GEOID,1,5)
    map2<-map2[!duplicated(map2)&!is.na(map2$ZCTA),]
    fea<-merge(fea,map2,by.x="FIPS",by.y="GEOID",all.x=T)
    fea$GEOID<-fea$ZCTA
  }
  if(geography=="tract"){
    map2<-data.frame(tract=fara$CensusTract,county=rep(NA,nrow(fara)))
    map2<-map2[!duplicated(map2),]
    map2$county<-substr(map2$tract,1,5)
    fea<-merge(fea,map2,by.x="FIPS",by.y="county",all.x=T)
    fea$GEOID<-fea$tract
  }
  
  food<-merge(fea,fara,by.x="GEOID",by.y="CensusTract",all=T)#note censustract is really the same level as geoid at this point

  food<-food[!duplicated(food),]
  food[,!names(food) %in% c("FIPS","State","County")]
}

testing<-F
if(testing==T){
  ex<-prepFood(year=2020,geography="county")
  ex<-prepFood(year=2019,geography="zcta")
}

