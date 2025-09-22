#prepEdu
library(educationdata)
source("C:/Users/chris/OneDrive/Desktop/GeoHealth/scripts/pullACS/pullACS.R")
setwd("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/")

for(y in 2023:2012){
  ccdp1<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ccd/public K12 directory/ELSI_csv_export_6389355247124350013876.csv"),header=F)
  names(ccdp1)<-ccdp1[4,]
  ccdp1<-ccdp1[5:nrow(ccdp1),c("School Name",
                               names(ccdp1)[str_detect(names(ccdp1),"Latest available")],
                               names(ccdp1)[str_detect(names(ccdp1),as.character(y))])]
  ccdp2<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ccd/public K12 directory/ELSI_csv_export_6389355300781122703179.csv"),header=F)
  names(ccdp2)<-ccdp2[4,]
  ccdp2<-ccdp2[5:nrow(ccdp2),c("School Name",
                               names(ccdp2)[str_detect(names(ccdp2),"Latest available")],
                               names(ccdp2)[str_detect(names(ccdp2),as.character(y))])]
  ccdp<-merge(ccdp1,ccdp2,by=c("School Name",
                               "State Name [Public School] Latest available year",
                               "School ID (12-digit) - NCES Assigned [Public School] Latest available year"))
  mark1<-substr(ccdp[2,1],1,1)
  mark2<-substr(ccdp[3,1],1,1)
  for(i in 1:ncol(ccdp)){
    ccdp[,i]<-str_replace_all(ccdp[,i],mark1,"")
    ccdp[,i]<-str_replace_all(ccdp[,i],mark2,"")
  }
  ccdp<-ccdp[ccdp$`School ID (12-digit) - NCES Assigned [Public School] Latest available year`!=""&
               ccdp[,names(ccdp)[str_detect(names(ccdp),"School Type")]]!="",]
  remove(ccdp1,ccdp2)
  gc()
  
  lu<-read.csv("coordinates to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&!is.na(lu),]
  ccdp<-merge(ccdp,lu,by.x=names(ccdp)[str_detect(names(ccdp),"Longitude")|str_detect(names(ccdp),"Latitude")],
              by.y=c("V2","V1"),all.x=T)
  check<-ccdp[!ccdp[,names(ccdp)[str_detect(names(ccdp),"Longitude")]] %in% lu$V1&
                !ccdp[,names(ccdp)[str_detect(names(ccdp),"Latitude")]] %in% lu$V2,
              c(names(ccdp)[str_detect(names(ccdp),"Longitude")|str_detect(names(ccdp),"Latitude")])]
  if(nrow(check)>0){
    for(i in 1:nrow(check)){
      if(check[,names(check)[str_detect(names(check),"Longitude")]][i]!=""&
         !is.na(as.numeric(check[,names(check)[str_detect(names(check),"Longitude")]][i]))&
         !check[,names(check)[str_detect(names(check),"Longitude")]][i] %in% lu$V1&
         !check[,names(check)[str_detect(names(check),"Latitude")]][i] %in% lu$V2){
        ex<-tigris::call_geolocator_latlon(lon=as.numeric(check[,names(check)[str_detect(names(check),"Longitude")]])[i],
                                           lat=as.numeric(check[,names(check)[str_detect(names(check),"Latitude")]])[i])
      }else{
        ex<-lu[lu$v1==check[,names(check)[str_detect(names(check),"Longitude")]][i]&
                 lu$V2==check[,names(check)[str_detect(names(check),"Latitude")]][i],"V3"]
        if(length(ex)==0){
          ex<-NA
        }
      }
      out<-data.frame(lon=check[,names(check)[str_detect(names(check),"Longitude")]][i],
                      lat=check[,names(check)[str_detect(names(check),"Latitude")]][i],
                      fips=ex)
      out<-out[!duplicated(out),]
      write.table(out,"coordinates to fips lookup.csv",sep=",",col.names=F,row.names=F,append=T)
      remove(out)
      gc()
      print(100*i/nrow(check))
    }
  }
  lu<-read.csv("coordinates to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&lu$V3!="V3"&!is.na(lu$V3),]
  write.table(lu,"coordinates to fips lookup.csv",sep=",",col.names=T,row.names=F,append=F)
  ccdp<-merge(ccdp,lu,by.x=names(ccdp)[str_detect(names(ccdp),"Longitude")|str_detect(names(ccdp),"Latitude")],
              by.y=c("V2","V1"),all.x=T)
  ccdp$fips<-substr(ccdp$V3.y,1,12)
  ccdp<-ccdp[,!names(ccdp) %in% c("V3.x","V3.y")]
  #ccdp$fips<-substr(outs,1,12)
  #now attach fips mapping if earlier year is indicated
  if(y<2020){
    map<-read.table("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/tab20_blkgrp20_blkgrp10_natl.txt",sep="|",header=T)
    map$GEOID_BLKGRP_20<-str_pad(map$GEOID_BLKGRP_20,width=12,side="left",pad="0")
    map$GEOID_BLKGRP_10<-str_pad(map$GEOID_BLKGRP_10,width=12,side="left",pad="0")
    ccdp<-merge(ccdp,map[,c("GEOID_BLKGRP_20","GEOID_BLKGRP_10")],by.x="fips",by.y="GEOID_BLKGRP_20",all.x=T)
    ccdp$fips<-ccdp$GEOID_BLKGRP_10
    ccdp<-ccdp[,1:(ncol(ccdp)-1)]
  }
  #now roll up to block group so we can later map to zcta and aggregate
  ccdp$regular_pub<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"School Type")]]=="1-Regular school",1,0)
  ccdp$alt_pub<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"School Type")]]=="4-Alternative Education School",1,0)
  ccdp$special_pub<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"School Type")]]=="2-Special education school",1,0)
  ccdp$votech_pub<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"School Type")]]=="3-Career and Technical School",1,0)
  ccdp$nat_lunch_participant<-ifelse(!ccdp[,names(ccdp)[str_detect(names(ccdp),"National School Lunch")]] %in% c("","-","No"),1,0)
  ccdp$nat_lunch_CEOprov_eligible<-ifelse(!ccdp[,names(ccdp)[str_detect(names(ccdp),"National School Lunch")]] %in% c("","-","No","Yes participating without using any Provision or the CEO"),1,0)
  ccdp$pub_prek<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"Prekindergarten")]]=="1-Yes",1,0)
  #ccdp$title1<-ifelse(ccdp[,names(ccdp)[str_detect(names(ccdp),"Title I")]]=="5-Title I schoolwide school",1,0)
  #ccdp$title1_elig_target<-ifelse(!ccdp[,names(ccdp)[str_detect(names(ccdp),"Title I")]] %in% c("6-Not a Title I school","-"),1,0)
  ccdp$pub_total_students<-as.numeric(ccdp[,names(ccdp)[str_detect(names(ccdp),"Total Students")]])
  ccdp$pub_fte_teachers<-as.numeric(ccdp[,names(ccdp)[str_detect(names(ccdp),"Teachers")]])

  ccdp<-ccdp[,c("fips","regular_pub","alt_pub","special_pub","votech_pub",
                "nat_lunch_participant","nat_lunch_CEOprov_eligible",
                "pub_prek",
                #"title1","title1_elig_target",
                "pub_total_students","pub_fte_teachers")]
  ccdp<-ccdp%>%group_by(fips)%>%summarise_each(funs=c("sum"))
  
  #repeat for ccdi
  ccdi<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ccd/private K12 directory/ELSI_csv_export_6389354797637787519907.csv"),header=F)
  names(ccdi)<-ccdi[4,]
  yin<-ifelse(y>2020,2020,y)
  ccdi<-ccdi[5:nrow(ccdi),c("Private School Name",
                            names(ccdi)[str_detect(names(ccdi),"Latest available")],
                            names(ccdi)[str_detect(names(ccdi),as.character(yin-1))],
                            names(ccdi)[str_detect(names(ccdi),paste0("-",substr(as.character(yin-1),3,4)))])]
  for(i in 1:ncol(ccdi)){
    ccdi[,i]<-str_replace_all(ccdi[,i],mark1,"")
    ccdi[,i]<-str_replace_all(ccdi[,i],mark2,"")
  }
  ccdi<-ccdi[ccdi$`School ID - NCES Assigned [Private School] Latest available year`!=""&
               ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]]!="",]
  ccdi[,names(ccdi)[str_detect(names(ccdi),'Address')]]<-str_remove_all(ccdi[,names(ccdi)[str_detect(names(ccdi),'Address')]],"[./+=*#-]")
  lu<-read.csv("address to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&!is.na(lu),]
  ccdi<-merge(ccdi,lu,by.x=names(ccdi)[str_detect(names(ccdi),"Address")|str_detect(names(ccdi),"ZIP")],
              by.y=c("V2","V1"),all.x=T)
  check<-ccdi[!ccdi[,names(ccdi)[str_detect(names(ccdi),"Street")]] %in% lu$V1&
                !ccdi[,names(ccdi)[str_detect(names(ccdi),"ZIP")]] %in% lu$V2,
              c(names(ccdi)[str_detect(names(ccdi),"Street")|str_detect(names(ccdi),"ZIP")])]
  
  if(length(check)>0){
    for(i in 1:nrow(ccdi)){
      if(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]][i]!=""){
        ex<-tigris::call_geolocator(street=tolower(ccdi[,names(ccdi)[str_detect(names(ccdi),"Address")]][i]),
                                    city=tolower(ccdi[,names(ccdi)[str_detect(names(ccdi),"City")]][i]),
                                    state=substr(ccdi[,names(ccdi)[str_detect(names(ccdi),"State Code")]][i],1,2))
      }else{
        ex<-lu[lu$v1==check[,names(check)[str_detect(names(check),"Address")]][i]&
                 lu$V2==check[,names(check)[str_detect(names(check),"ZIP")]][i],"V3"]
        if(length(ex)==0){
          ex<-NA
        }
      }
      out<-data.frame(street=check[,names(check)[str_detect(names(check),"Address")]][i],
                      zip=check[,names(check)[str_detect(names(check),"ZIP")]][i],
                      fips=ex)
      out<-out[!duplicated(out),]
      write.table(out,"address to fips lookup.csv",sep=",",col.names=F,row.names=F,append=T)
      remove(out)
      gc()
      print(100*i/nrow(check))
    }
  }

  lu<-read.csv("address to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&lu$V3!="V3"&!is.na(lu$V3),]
  write.table(lu,"address to fips lookup.csv",sep=",",col.names=T,row.names=F,append=F)
  ccdi<-merge(ccdi,lu,by.x=names(ccdi)[str_detect(names(ccdi),"Address")|str_detect(names(ccdi),"ZIP")],
              by.y=c("V2","V1"),all.x=T)
  ccdi$fips<-substr(ccdi$V3.y,1,12)
  ccdi$fips<-ifelse(is.na(ccdi$fips),ccdi[,names(ccdi)[str_detect(names(ccdi),"County Code")]],ccdi$fips)
  ccdi<-ccdi[,!names(ccdi) %in% c("V3.x","V3.y")]
  #now attach fips mapping if earlier year is indicated
  if(y<2020){
    map<-read.table("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/tab20_blkgrp20_blkgrp10_natl.txt",sep="|",header=T)
    map$GEOID_BLKGRP_20<-str_pad(map$GEOID_BLKGRP_20,width=12,side="left",pad="0")
    map$GEOID_BLKGRP_10<-str_pad(map$GEOID_BLKGRP_10,width=12,side="left",pad="0")
    ccdi<-merge(ccdi,map[,c("GEOID_BLKGRP_20","GEOID_BLKGRP_10")],by.x="fips",by.y="GEOID_BLKGRP_20",all.x=T)
    ccdi$fips<-ifelse(nchar(ccdi$fips)==5,ccdi$fips,ccdi$GEOID_BLKGRP_10)
    ccdi<-ccdi[,1:(ncol(ccdi)-1)]
  }
  #now roll up to block group so we can later map to zcta and aggregate
  ccdi$regular_pri<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]]=="1-Regular Elementary or Secondary",1,0)
  ccdi$alt_pri<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]]=="4-Alternative/other",1,0)
  ccdi$special_pri<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]]=="2-Special Education",1,0)
  ccdi$votech_pri<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]]=="3-Career/technical/vocational",1,0)
  ccdi$other_private_edu<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"School Type")]] %in% c("2-Montessori","3-Special Program Emphasis","6-Alternative/other","7-Early Childhood Program/child care center"),1,0)
  ccdi$private_school<-1
  ccdi$religious<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"Religious")]] %in% c("Nonsectarian",""),0,1)
  ccdi$pri_prek<-ifelse(ccdi[,names(ccdi)[str_detect(names(ccdi),"Lowest Grade Taught")]]=="Prekindergarten",1,0)
  ccdi$pri_total_students<-as.numeric(ccdi[,names(ccdi)[str_detect(names(ccdi),"Total Students")]])
  ccdi$pri_fte_teachers<-as.numeric(ccdi[,names(ccdi)[str_detect(names(ccdi),"Teachers")]])
  
  ccdi<-ccdi[,c("fips","regular_pri","alt_pri","special_pri","votech_pri","other_private_edu",
                "private_school","religious",
                "pri_prek",
                "pri_total_students","pri_fte_teachers")]
  ccdi<-ccdi%>%group_by(fips)%>%summarise_each(funs=c("sum"))
  
  #merge ccd
  ccd<-merge(as.data.frame(ccdp),as.data.frame(ccdi),by="fips",all=T)
  ccd<-ccd[!is.na(ccd$fips),]
  ccd[is.na(ccd)]<-0
  remove(ccdi,ccdp)
  gc()
  
  write.table(ccd,paste0("ccd_clean_",y,".txt"),col.names=T,row.names=F,sep="|")
  
  #do ipeds----
  ipedsa<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ipeds/hd",y,".csv"),header=T)
  aname<-c("UNITID","LONGITUD","LATITUDE","DEATHYR","ACT","CYACTIVE",
           "UGOFFER","GROFFER","HBCU","HOSPITAL","MEDICAL","TRIBAL","LOCALE","OPENPUBL","PSEFLAG",
           "RPTMTH","INSTCAT","INSTSIZE")
  
  ipedsb<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ipeds/ic",y,"_ay.csv"),header=T)
  bname<-c("UNITID","TUITION2","TUITION3","TUITION6","TUITION7",
           "CHG5AY3")
  
  ipedsc<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ipeds/sal",y,"_nis.csv"),header=T)
  cname<-c("UNITID","SANIN01","SANIN02","SANIN12","SANIN08")
  ipedsc[is.na(ipedsc)]<-0
  ipedsc<-ipedsc[,cname]%>%
    group_by(UNITID)%>%
    summarise_each(funs=c("sum"))
  
  ipedsd<-read.csv(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/education/ipeds/sal",y,"_is.csv"),header=T)
  dname<-c("UNITID","SAINSTT","SAOUTLT","SA12MCT","SA12MAT")
  ipedsd[is.na(ipedsd)]<-0
  ipedsd<-ipedsd[,dname]%>%
    group_by(UNITID)%>%
    summarise_each(funs=c("sum"))
  
  ipeds<-merge(ipedsa[,aname],
               ipedsb[,bname],
               by="UNITID",
               all=T)
  ipeds<-merge(ipeds,
               as.data.frame(ipedsc),
               by="UNITID",
               all=T)
  ipeds<-merge(ipeds,
               as.data.frame(ipedsd),
               by="UNITID",
               all=T)
  remove(ipedsa,ipedsb,ipedsc,ipedsd)
  gc()
  
  for(k in 4:ncol(ipeds)){
    ipeds[,k]<-ifelse(ipeds[,k]<0|ipeds[,k]=="."|is.na(ipeds[,k]),0,ipeds[,k])
  }
  ipeds$inactive_win12<-ifelse(ipeds$ACT!="A"|ipeds$CYACTIVE!="A"|ipeds$DEATHYR!=0,0,1)
  ipeds$undergrad_school_prog<-ifelse(ipeds$UGOFFER=="1",1,0)
  ipeds$grad_school_prog<-ifelse(ipeds$GROFFER=="1",1,0)
  ipeds$hbcu<-ifelse(ipeds$HBCU=="1",1,0)
  ipeds$hospital<-ifelse(ipeds$HOSPITAL=="1",1,0)
  ipeds$medical_school_prog<-ifelse(ipeds$MEDICAL=="1",1,0)
  ipeds$tribal_school_prog<-ifelse(ipeds$TRIBAL=="1",1,0)
  ipeds$public_college_university<-ifelse(ipeds$OPENPUBL=="1",1,0)
  ipeds$post_secondary_institution<-ifelse(ipeds$PSEFLAG=="1",1,0)
  ipeds$degree_granting<-ifelse(ipeds$INSTCAT %in% c("1","2","3","4"),1,0)
  ipeds$grants_associates_certificate_only<-ifelse(ipeds$INSTCAT=="4",1,0)
  ipeds$not_degree_granting<-ifelse(ipeds$INSTCAT %in% c("5","6"),1,0)
  ipeds$size_lt1k<-ifelse(ipeds$INSTSIZE=="1",1,0)
  ipeds$size_1k_10k<-ifelse(ipeds$INSTSIZE %in% c("2","3"),1,0)
  ipeds$size_gt10k<-ifelse(ipeds$INSTSIZE %in% c("4","5"),1,0)
  ipeds$cost_undergrad_instate<-as.numeric(ifelse(ipeds$TUITION2=="0",NA,ipeds$TUITION2))
  ipeds$cost_undergrad_outstate<-as.numeric(ifelse(ipeds$TUITION3=="0",NA,ipeds$TUITION3))
  ipeds$cost_grad_instate<-as.numeric(ifelse(ipeds$TUITION6=="0",NA,ipeds$TUITION6))
  ipeds$cost_grad_outstate<-as.numeric(ifelse(ipeds$TUITION7=="0",NA,ipeds$TUITION7))
  ipeds$cost_oncampus_living<-as.numeric(ifelse(ipeds$CHG5AY3=="0",NA,ipeds$CHG5AY3))
  ipeds$ft_noninst_academic_staff<-as.numeric(ipeds$SANIN01)+as.numeric(ipeds$SANIN02)
  ipeds$ft_community_admin_staff<-as.numeric(ipeds$SANIN08)+as.numeric(ipeds$SANIN12)
  ipeds$total_instructional_staff<-as.numeric(ipeds$SAINSTT)
  ipeds$total_inst_staff_salary<-as.numeric(ipeds$SAOUTLT)
  ipeds$est_avg_inst_staff_salary<-ipeds$total_inst_staff_salary/ipeds$total_instructional_staff
  ipeds$no_reported_inst_staff<-ifelse(is.na(ipeds$SAINSTT),1,0)
  
  ipedsname<-c("UNITID","LONGITUD","LATITUDE",
               "inactive_win12","undergrad_school_prog","grad_school_prog","hbcu","hospital","medical_school_prog",
               "tribal_school_prog","public_college_university","post_secondary_institution","degree_granting",
               "grants_associates_certificate_only","not_degree_granting","size_lt1k","size_1k_10k","size_gt10k",
               "cost_undergrad_instate","cost_undergrad_outstate","cost_grad_instate","cost_grad_outstate",
               "cost_oncampus_living","ft_noninst_academic_staff","ft_community_admin_staff",
               "total_instructional_staff","total_inst_staff_salary","est_avg_inst_staff_salary","no_reported_inst_staff")
  ipeds<-ipeds[,ipedsname]
  
  lu<-read.csv("coordinates to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&!is.na(lu),]
  ipeds<-merge(ipeds,lu,by.x=names(ipeds)[str_detect(names(ipeds),"LONGITUD")|str_detect(names(ipeds),"LATITUDE")],
              by.y=c("V1","V2"),all.x=T)
  check<-ipeds[!ipeds[,names(ipeds)[str_detect(names(ipeds),"LONGITUD")]] %in% lu$V1&
                !ipeds[,names(ipeds)[str_detect(names(ipeds),"LATITUDE")]] %in% lu$V2,
              c(names(ipeds)[str_detect(names(ipeds),"LONGITUD")|str_detect(names(ipeds),"LATITUDE")])]
  if(nrow(check)>0){
    for(i in 1:nrow(check)){
      if(check[,names(check)[str_detect(names(check),"LONGITUD")]][i]!=""&
         !is.na(as.numeric(check[,names(check)[str_detect(names(check),"LONGITUD")]][i]))&
         !check[,names(check)[str_detect(names(check),"LONGITUD")]][i] %in% lu$V1&
         !check[,names(check)[str_detect(names(check),"LATITUDE")]][i] %in% lu$V2){
        ex<-tigris::call_geolocator_latlon(lon=as.numeric(check[,names(check)[str_detect(names(check),"LONGITUD")]])[i],
                                           lat=as.numeric(check[,names(check)[str_detect(names(check),"LATITUDE")]])[i])
      }else{
        ex<-lu[lu$v1==check[,names(check)[str_detect(names(check),"LONGITUD")]][i]&
                 lu$V2==check[,names(check)[str_detect(names(check),"LATITUDE")]][i],"V3"]
        if(length(ex)==0){
          ex<-NA
        }
      }
      out<-data.frame(lon=check[,names(check)[str_detect(names(check),"LONGITUD")]][i],
                      lat=check[,names(check)[str_detect(names(check),"LATITUDE")]][i],
                      fips=ex)
      out<-out[!duplicated(out),]
      write.table(out,"coordinates to fips lookup.csv",sep=",",col.names=F,row.names=F,append=T)
      remove(out)
      gc()
      print(100*i/nrow(check))
    }
  }
  lu<-read.csv("coordinates to fips lookup.csv",header=F,colClasses = "character")
  lu<-lu[!duplicated(lu)&lu$V3!=""&lu$V3!="V3"&!is.na(lu$V3),]
  write.table(lu,"coordinates to fips lookup.csv",sep=",",col.names=T,row.names=F,append=F)
  ipeds<-merge(ipeds,lu,by.x=names(ipeds)[str_detect(names(ipeds),"LONGITUD")|str_detect(names(ipeds),"LATITUDE")],
              by.y=c("V1","V2"),all.x=T)
  ipeds$fips<-substr(ipeds$V3.y,1,12)
  ipeds<-ipeds[,!names(ipeds) %in% c("V3.x","V3.y")]
  
  
  #now attach fips mapping if earlier year is indicated
  if(y<2020){
    map<-read.table("C:/Users/chris/OneDrive/Desktop/GeoHealth/data/tab20_blkgrp20_blkgrp10_natl.txt",sep="|",header=T)
    map$GEOID_BLKGRP_20<-str_pad(map$GEOID_BLKGRP_20,width=12,side="left",pad="0")
    map$GEOID_BLKGRP_10<-str_pad(map$GEOID_BLKGRP_10,width=12,side="left",pad="0")
    ipeds<-merge(ipeds,map[,c("GEOID_BLKGRP_20","GEOID_BLKGRP_10")],by.x="fips",by.y="GEOID_BLKGRP_20",all.x=T)
    ipeds$fips<-ipeds$GEOID_BLKGRP_10
    ipeds<-ipeds[,1:(ncol(ipeds)-1)]
  }
  ipedsSum<-ipeds[ipeds$fips!="0"&!is.na(ipeds$fips),c("fips",
                  "inactive_win12","undergrad_school_prog","grad_school_prog","hbcu","hospital","medical_school_prog","tribal_school_prog",
                  "public_college_university","post_secondary_institution","degree_granting","grants_associates_certificate_only",
                  "not_degree_granting","size_lt1k","size_1k_10k","size_gt10k",
                  "ft_noninst_academic_staff",
                  "ft_community_admin_staff","total_instructional_staff","total_inst_staff_salary","no_reported_inst_staff")]
  ipedsSum[is.na(ipedsSum)]<-"0"
  ipedsSum<-ipedsSum%>%group_by(fips)%>%summarise_each(funs=c("sum"))
  
  ipedsAvg<-ipeds[ipeds$fips!="0"&!is.na(ipeds$fips),c("fips",
                     "cost_grad_instate","cost_grad_outstate","cost_oncampus_living",
                     "cost_undergrad_instate","cost_undergrad_outstate",
                     "est_avg_inst_staff_salary")]
  ipedsAvg<-ipedsAvg%>%group_by(fips)%>%summarise_each(funs=c("mean"))
  ipeds<-merge(ipedsSum,ipedsAvg,by="fips",all=T)
  
  write.table(ipeds,paste0("ipeds_clean_",y,".txt"),col.names=T,row.names=F,sep="|")
  
}
