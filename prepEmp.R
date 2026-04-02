#prepEmp
library(stringr)
library(dplyr)
library(tigris)
#library(ipumsr)

geo_impute<-function(x,geoid="geoid",from,to,type="percent",year=2019){#requires fips coding
  #  ipums.key<-"59cba10d8a5da536fc06b59d66e936288d8a4d7eb286182ea94a5c84"
  #  set_ipums_api_key(api_key=ipums.key)
  #hierarchy----
  hierarchy<-factor(c("county","tract","bg"),ordered=T,levels=rev(c("county","tract","bg")))
  #load geographies from ipumsr #tigris----
  geos<-c(from,to)
  geos<-ifelse(geos=="block group","blck_grp",
               ifelse(geos=="zip code tabulation area","zcta",geos))
  #  files<-get_metadata_nhgis('shapefiles',api_key=ipums.key)
  #  files<-files[files$year==year&
  #                 (str_detect(files$name,paste0("us_",geos[1]))|
  #                    str_detect(files$name,paste0("us_",geos[2]))),]$name
  
  if("county" %in% geos){
    county<-tigris::counties(year=year)
    #    extract<-ipumsr::define_extract_nhgis(description=paste0("county_shp_",year),
    #                                          shapefiles=paste0("us_county_",year,"_tl",year))
    #    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    #    county<-read_ipums_sf(sf)
    #    file.remove(sf)
    #    remove(sf)
    gc()
    
  }
  if("tract" %in% geos){
    tract<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::tracts(state=i,year=year,resolution="500k"))[,c("STATEFP","GEOID")]
      tract<-rbind(tract,out)
    }
    #    extract<-ipumsr::define_extract_nhgis(description=paste0("tract_shp_",year),
    #                                          shapefiles=paste0("us_tract_",year,"_tl",year))
    #    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    #    tract<-read_ipums_sf(sf)
    #    file.remove(sf)
    #    remove(sf)
    gc()
  }
  if("bg" %in% geos){
    bg<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::block_groups(state=i,year=year))[,c("STATEFP","GEOID")]
      bg<-rbind(bg,out)
    }
    #    extract<-ipumsr::define_extract_nhgis(description=paste0("blck_grp_shp_",year),
    #                                          shapefiles=paste0("us_blck_grp_",year,"_tl",year))
    #    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    #    bg<-read_ipums_sf(sf)
    #    file.remove(sf)
    #    remove(sf)
    gc()
  }
  #create spine on from data----
  if(from=="county"){
    county<-merge(as.data.frame(county[,c("STATEFP","GEOID")]),
                  x,by.x="GEOID",by.y=geoid,all.x=T)
  }
  if(from=="tract"){
    tract<-merge(as.data.frame(tract[,c("STATEFP","GEOID")]),
                 x,by.x="GEOID",by.y=geoid,all.x=T)
  }
  if(from=="bg"){
    bg<-merge(as.data.frame(bg[,"GEOID"]),
              x,by.x="GEOID",by.y=geoid,all.x=T)
  }
  #expand to next level using to geographies-----
  if(from=="county"&to=="tract"){
    tract$tract<-tract$GEOID
    tract$GEOID<-substr(tract$GEOID,1,5)
    county<-merge(county,tract[,c("GEOID","tract")],by="GEOID",all.x=T)#hangs here
    out<-county[,!names(county) %in% c("geometry","STATEFP","GEOID")]
  }
  if(from=="county"&to=="bg"){
    bg$bg<-bg$GEOID
    bg$GEOID<-substr(bg$GEOID,1,5)
    county<-merge(county,bg[,c("GEOID","bg")],by="GEOID",all.x=T)
    out<-county[,!names(county) %in% c("geometry","STATEFP","GEOID")]
  }
  if(from=="tract"&to=="bg"){
    bg$bg<-bg$GEOID
    bg$GEOID<-substr(bg$GEOID,1,11)
    tract<-merge(tract,bg[,c("GEOID","bg")],by="GEOID",all.x=T)
    out<-tract[,!names(tract) %in% c("geometry","STATEFP","GEOID")]
  }
  if(from=="bg"&to=="tract"){
    bg$bg<-bg$GEOID
    bg$GEOID<-substr(bg$GEOID,1,11)
    bg<-merge(bg,tract[,"GEOID"],by="GEOID",all.x=T)
    out<-bg[,!names(bg) %in% c("geometry","STATEFP","GEOID")]
    
  }
  if(from=="bg"&to=="county"){
    bg$bg<-bg$GEOID
    bg$GEOID<-substr(bg$GEOID,1,5)
    bg<-merge(bg,county[,"GEOID"],by="GEOID",all.x=T)
    out<-bg[,!names(bg) %in% c("geometry","STATEFP","GEOID")]
    
  }
  if(from=="tract"&to=="county"){
    tract$tract<-tract$GEOID
    tract$GEOID<-substr(tract$GEOID,1,5)
    tract<-merge(tract,county[,c("GEOID")],by="GEOID",all.x=T)
    out<-tract[,!names(tract) %in% c("geometry","STATEFP","GEOID")]
    
  }
  
  if(type=="percent"|type=="rate"){#if percent going up average, going down impute
    if(hierarchy[hierarchy==from]>hierarchy[hierarchy==to]){#going down
      out<-out[!is.na(out[,to])&!is.na(out[,names(x)[!names(x) %in% geoid]]),]
    }else{#going up
      span<-ifelse(to=="county",5,
                   ifelse(to=="tract",11,
                          ifelse(to=="bg",12)))
      out[,from]<-substr(out[,from],1,span)
      out<-out[!is.na(out[,from])&!is.na(out[,names(x)[!names(x) %in% geoid]]),]
      m<-aggregate(data=out,formula(paste0(names(x)[!names(x) %in% geoid],"~",from)),FUN="mean")
      out<-m
    }
  }
  if(type=="count"){#if count going up sum, going down split
    if(hierarchy[hierarchy==from]>hierarchy[hierarchy==to]){#going down
      span<-ifelse(from=="county",5,
                   ifelse(from=="tract",11,
                          ifelse(from=="bg",12)))
      out$geo<-substr(out[,to],1,span)
      subs<-data.frame(geo=out[,"geo"],val=rep(1,nrow(out)))
      subs<-aggregate(data=subs,val~geo,FUN="sum")
      out<-merge(out,subs,by="geo",all.x=T)
      out<-out[!is.na(out[,to])&!is.na(out[,names(x)[!names(x) %in% geoid]]),]
      out[,names(x)[!names(x) %in% geoid]]<-as.numeric(out[,names(x)[!names(x) %in% geoid]])/out$val
      out<-out[,c(names(x)[!names(x) %in% geoid],to)]
      #      names(out)<-c(names(x)[!names(x) %in% geoid],to)
      out<-out[!duplicated(out),]
      
    }else{#going up
      span<-ifelse(to=="county",5,
                   ifelse(to=="tract",11,
                          ifelse(to=="bg",12)))
      out[,from]<-substr(out[,from],1,span)
      out<-out[!is.na(out[,from])&!is.na(out[,names(x)[!names(x) %in% "geoid"]]),]
      m<-aggregate(data=out,formula(paste0(names(x)[!names(x) %in% "geoid"],"~",from)),FUN="sum")
      out<-m
    }
    
  }
  if(type=="binary"){#if binary going up mode, going down impute binary value
    if(hierarchy[hierarchy==from]>hierarchy[hierarchy==to]){#going down
      out<-out[!is.na(out[,to])&!is.na(out[,names(x)[!names(x) %in% "geoid"]]),]
    }else{#going up
      span<-ifelse(to=="county",5,
                   ifelse(to=="tract",11,
                          ifelse(to=="bg",12)))
      out[,from]<-substr(out[,from],1,span)
      out<-out[!is.na(out[,from])&!is.na(out[,names(x)[!names(x) %in% "geoid"]]),]
      m<-aggregate(data=out,formula(paste0(names(x)[!names(x) %in% "geoid"],"~",from)),FUN="mean")
      m[,names(x)[!names(x) %in% "geoid"]]<-ifelse(m[,names(x)[!names(x) %in% "geoid"]]>0.5,1,0)
      out<-m
    }
    
  }
  
  out
}


prepEmp<-function(year=2020,geography="tract"){
  if(year>2022){
    year<-2022
  }
  #do qwi vars----
  qwi<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/qwi_final_selected.csv"),header=T)
  qwi$geography<-str_pad(qwi$geography,width=5,side="left",pad="0")
  qwi<-qwi[qwi$industry=="00"&qwi$year==year,]
  if(geography!="county"){#if not county convert to tract then adjust to zcta if needed
    es<-data.frame(tract=NA)
    vars<-c("Emp","EmpEnd","EmpS","EmpStart","EmpAnnualch","FrmJbCS_sd","FrmJbLsS_sd")
    for(i in vars){
      e<-geo_impute(x=qwi[,c("geography",i)],geoid="geography",from="county",to="tract",type="count",year=ifelse(year>2019,2020,2019))
      es<-merge(es,e,by="tract",all=T)
    }
    es<-es[!is.na(es$tract),]
    if(geography=="tract"){
      es$geography<-es$tract
    }
    if(geography=="zcta"){
      if(year<2020){
        map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
        map$GEOID<-str_pad(map$GEOID,width=11,side="left",pad="0")
        map$ZCTA5<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
        
        es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="tract",by.y="GEOID")
        es<-es[!duplicated(es)&!is.na(es$ZCTA5),]
        es<-es[,!names(es) %in% c("GEOID","tract")]%>%
          group_by(ZCTA5)%>%
          summarise_each(funs=c(sum))
        
      }else{
        map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
        map$GEOID<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
        map$ZCTA5<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")

        es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="tract",by.y="GEOID")
        es<-es[!duplicated(es)&!is.na(es$ZCTA5)&es$ZCTA5!="",]
        es<-es[,!names(es) %in% c("GEOID","tract")]%>%
          group_by(ZCTA5)%>%
          summarise_each(funs=c(sum))
      }
      es$geography<-es$ZCTA5
    }
    qwi<-as.data.frame(es[,!names(es) %in% c("tract","GEOID","ZCTA5")])
    remove(e,es)
    gc()
  }
  
  #do lodes----
  lodes<-read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/lodes_all_years_wide.csv",header=T)
  lodes$geoid<-str_pad(lodes$geoid,width=11,side="left",pad="0")
  lodes<-lodes[lodes$year==year,]
  es<-lodes
  es$geography<-es$geoid
  if(geography=="county"){
#    es<-data.frame(tract=NA)
    vars<-c("C000","CA01","CA02","CA03","CFA01","CFA02","CFA03","CFA04","CFS01","CFS02","CFS03","CFS04","SI01","SI02","SI03","CE01","CE02","CE03")
    es$geography<-substr(es$geography,1,5)
    es[is.na(es)]<-0
    es<-es[,c("geography",vars)]%>%
      group_by(geography)%>%
      summarise_each(funs=c(sum))
    es<-as.data.frame(es)
#    for(i in vars){
#      e<-geo_impute(x=lodes[,c("geoid",i)],geoid="geoid",from="tract",to="county",type="count",year=ifelse(year>2019,2020,2019))
#      es<-merge(es,e,by="tract",all=T)
#    }
    
#    es$geography<-es$tract
#    es<-es[!is.na(es$tract),]
  }
  if(geography=="zcta"){
    if(year<2020){
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
      map$GEOID<-str_pad(map$GEOID,width=11,side="left",pad="0")
      map$ZCTA5<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
      
      es<-merge(lodes,map[,c("GEOID","ZCTA5")],by.x="geoid",by.y="GEOID")
      es<-es[!duplicated(es)&!is.na(es$ZCTA5),]
      es[is.na(es)]<-0
      es<-es[,!names(es) %in% c("GEOID","geoid","geography","year","state")]%>%
        group_by(ZCTA5)%>%
        summarise_each(funs=c(sum))
      
    }else{
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
      map$GEOID<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
      map$ZCTA5<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
      
      es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="geoid",by.y="GEOID")
      es<-es[!duplicated(es)&!is.na(es$ZCTA5)&es$ZCTA5!="",]
      es<-es[,!names(es) %in% c("GEOID","geoid","geography","year","state")]%>%
        group_by(ZCTA5)%>%
        summarise_each(funs=c(sum))
    }
    es$geography<-es$ZCTA5
    es<-es[,!names(es) %in% c("ZCTA5")]
  }
  
  lodes<-as.data.frame(es[,!names(es) %in% c("tract","geoid","GEOID","ZCTA5")])
  remove(e,es,map)
  gc()
  
  #combine and print out----
  out<-merge(lodes[,!names(lodes) %in% c("year","state")],qwi,by="geography",all=T)
  remove(lodes,qwi)
  out
}


#prepEmp(year=2022,"zcta")