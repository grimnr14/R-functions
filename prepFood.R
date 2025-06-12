#retrieve data from github and convert to full fips and zcta mapping
#requires fips->zcta mapping file
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(tigris)
library(censusxy)

prepFood<-function(year=2019,geography="county",
                   zcta="https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_zcta_tract_fips_2020.txt",
                   year.map=NULL){
  fara.year<-ifelse(year>=2019,"2019",
                    ifelse(year>=2015,"2015","2010"))
  fea.year<-ifelse(year>=2019,"2019",
                   ifelse(year>=2017,"2017",
                          ifelse(year>=2015,"2015",
                                 ifelse(year>=2014,"2015",
                                        ifelse(year>=2012,"2012","2011")))))
  if(geography=="zcta"){
    map1<-read.csv(zcta,header=T,sep="|")
    map1$GEOID<-map1[,str_detect(names(map1),"GEOID_TRACT")]
    map1$ZCTA<-map1[,str_detect(names(map1),"GEOID_ZCTA5_")]
    map1<-map1[,c("GEOID","ZCTA")]
    map1<-map1[!duplicated(map1),]
    map1$GEOID<-str_pad(as.character(map1$GEOID),width=11,side="left",pad="0")
    map1$ZCTA<-str_pad(as.character(map1$ZCTA),width=5,side="left",pad="0")
  }
  pop<-pullACS(geography=geography,year=year,geometry=F)
  fara<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FARA%20estimates%20",fara.year,".csv"),header=T)
  #read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FARA%20estimates%202019.csv",header=T)
  fara<-fara[fara$year==fara.year,]
  fara<-as.data.frame(sapply(fara,as.numeric))
  fara$CensusTract<-str_pad(as.character(fara$CensusTract),width=11,side="left",pad="0")
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
  fara<-merge(fara,pop,by.x="CensusTract",by.y="GEOID",all.x=T)#we can use current year pop assuming static rate to calc estimated counts of residents by year
  
  fara$lapophalfshare<-100*(fara$lapophalf/fara$POP2010)
  fara$lakidshalfshare<-100*(fara$lakidshalf/fara$lapophalf)
  fara$laseniorshalfshare<-100*(fara$laseniorshalf/fara$lapophalf)
  fara$lasnaphalfshare<-100*(fara$lasnaphalf/fara$lapophalf)
  fara$lapop1share<-100*(fara$lapop1/fara$POP2010)
  fara$lakids1share<-100*(fara$lakids1/fara$lapop1)
  fara$laseniors1share<-100*(fara$laseniors1/fara$lapop1)
  fara$lasnap1share<-100*(fara$lasnap1/fara$lapop1)
  fara$lapop10share<-100*(fara$lapop10/fara$POP2010)
  fara$lakids10share<-100*(fara$lakids10/fara$lapop10)
  fara$laseniors10share<-100*(fara$laseniors10/fara$lapop10)
  fara$lasnap10share<-100*(fara$lasnap10/fara$lapop10)
  
  fara$lapophalf<-(fara$lapophalfshare/100)*fara$B01001_001#we can estimate annually assuming static rate
  fara$lakidshalf<-(fara$lakidshalfshare/100)*fara$lapophalf
  fara$laseniorshalf<-(fara$laseniorshalfshare/100)*fara$lapophalf
  fara$lasnaphalf<-(fara$lasnaphalfshare/100)*fara$lapophalf
  fara$lapop1<-(fara$lapop1share/100)*fara$B01001_001#we can estimate annually assuming static rate
  fara$lakids1<-(fara$lakids1share/100)*fara$lapop1
  fara$laseniors1<-(fara$laseniors1share/100)*fara$lapop1
  fara$lasnap1<-(fara$lasnap1share/100)*fara$lapop1
  fara$lapop10<-(fara$lapop10share/100)*fara$B01001_001#we can estimate annually assuming static rate
  fara$lakids10<-(fara$lakids10share/100)*fara$lapop10
  fara$laseniors10<-(fara$laseniors10share/100)*fara$lapop10
  fara$lasnap10<-(fara$lasnap10share/100)*fara$lapop10
  vars<-c("CensusTract","B01001_001",
          "lapophalfshare","lakidshalfshare","laseniorshalfshare","lasnaphalfshare",
          "lapop1share","lakids1share","laseniors1share","lasnap1share",
          "lapop10share","lakids10share","laseniors10share","lasnap10share"
          )
  fara<-fara[,vars]
  for(i in 3:ncol(fara)){
    fara[,i]<-ifelse(is.infinite(fara[,i])|is.na(fara[,i])|fara[,i]>100,NA,fara[,i])
    fara[,i]<-ifelse(fara[,i]==0&!is.na(fara[,i]),NA,fara[,i])#eliminating 0 reflects 0 imputed values that didn't actually exist
  }
  food<-fara[,!names(fara) %in% c("B01001_001")]


  fea<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FoodEnvironmentAtlas",fea.year,".csv"),header=T)
  rnk<-as.numeric(substr(names(fea),nchar(names(fea))-1,nchar(names(fea))))
  rnk<-ordered(rnk)
  rnk<-levels(rnk)[(length(levels(rnk))-2):length(levels(rnk))]
  fea<-fea[,c("FIPS","State","County",names(fea)[str_detect(names(fea),rnk[1])|str_detect(names(fea),rnk[2])|str_detect(names(fea),rnk[3])])]
  names(fea)<-gsub("[0-9_]","",names(fea))
  fea$FIPS<-str_pad(as.character(fea$FIPS),width=5,side="left",pad="0")
  #the PTH features are all rates per 1,000 residents in corresponding year population from file
  fea$yearPop<-1000*fea$GROC/fea$GROCPTH
  #rates<-fea[,c("FIPS","State","County",names(fea)[str_detect(names(fea),"PTH")])]#assumes equal rate per geography
  #counts<-fea[,c("FIPS","State","County",names(fea)[str_detect(names(fea),"PTH")==F])]
  if(geography=="county"){
    pop<-pullACS(geography="county",geometry=F,variables=c("B01001_001","B25001_001"),year=year)
    fea<-merge(fea,pop,by.x="FIPS",by.y="GEOID",all.x=T)#note this geography never changes. FEA is at county only
    fea$GEOID<-fea$FIPS
  }
  if(geography=="zcta"){
    map2<-map1
    map2$GEOID<-substr(map2$GEOID,1,5)
    map2<-map2[!duplicated(map2)&!is.na(map2$ZCTA),]
    fea<-merge(fea,map2,by.x="FIPS",by.y="GEOID",all.x=T)
    if(year>=2020){
      pop<-pullACS(geography="zcta",geometry=F,variables=c("B01001_001","B25001_001"),year=year)
    }else{
      pop<-pullACS(geography="zip code tabulation area",variables=c("B01001_001","B25001_001"),year=year,geometry=F)
    }
    fea<-merge(fea,pop,by.x="ZCTA",by.y="GEOID",all.x=T)
    fea$GEOID<-fea$ZCTA
  }
  if(geography=="tract"){
    pop<-pullACS(geography="tract",variables=c("B01001_001","B25001_001"),geometry=F,year=year)
    pop$county<-substr(pop$GEOID,1,5)
    pop$tract<-pop$GEOID
    fea<-merge(fea,pop[,!names(pop) %in% c("GEOID")],by.x="FIPS",by.y="county",all.x=T)
    fea$GEOID<-fea$tract
  }
  #head((fea$GROCPTH*fea$yearPop)/1000)#testing
  fea$GROC<-(fea$GROCPTH*fea$B01001_001)/1000#same rate, new denominator assumes equal dist of stores across geographies
  fea$CONVS<-(fea$CONVSPTH*fea$B01001_001)/1000
  fea$SUPERC<-(fea$SUPERCPTH*fea$B01001_001)/1000
  fea$SNAPS<-(fea$SNAPSPTH*fea$B01001_001)/1000
  fea$WICS<-(fea$WICSPTH*fea$B01001_001)/1000
  fea$FFR<-(fea$FFRPTH*fea$B01001_001)/1000
  fea$FSR<-(fea$FSRPTH*fea$B01001_001)/1000
  
#  fea$GROCPTH<-(fea$GROC/fea$B01001_001)*1000#new rate of groc per 1000 residents
#  fea$CONVSPTH<-(fea$CONVS/fea$B01001_001)*1000
#  fea$SUPERCPTH<-(fea$SUPERC/fea$B01001_001)*1000
#  fea$SNAPSPTH<-(fea$SNAPS/fea$B01001_001)*1000
#  fea$WICSPTH<-(fea$WICS/fea$B01001_001)*1000
#  fea$FFRPTH<-(fea$FFR/fea$B01001_001)*1000
#  fea$FSRPTH<-(fea$FSR/fea$B01001_001)*1000
  
  fea$FOODBANKSper1k<-1000*(fea$FOODBANKS/fea$B01001_001)
  fea$PCSNAPBEN<-(fea$PCSNAPBEN/100000)*fea$B01001_001#theoretical dollar spent per resident assuming funding levels are same
  fea$PCWICREDEMP<-(fea$PCWICREDEMP/100000)*fea$B01001_001
  fea<-fea[,c("GEOID","GROC","CONVS","SUPERC","SNAPS","WICS","FFR","FSR","FOODBANKSper1k",
              "REDEMPSNAPS","REDEMPWICS","PCSNAPBEN","PCWICREDEMP","FOODINSEC","VLFOODSEC")]
  
  food<-merge(fea,food,by.x="GEOID",by.y="CensusTract",all=T)#note censustract is really the same level as geoid at this point
  for(i in 2:ncol(food)){
    food[,i]<-ifelse(is.infinite(food[,i])|is.na(food[,i]),NA,food[,i])#removing missing denominator values
  }
  
  food<-food[!duplicated(food),]
  food
}

testing<-F
if(testing==T){
  ex<-prepFood(year=2022,geography="tract")
}

