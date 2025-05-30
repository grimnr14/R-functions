#creating a flexible function for adjusting level of geography using several heuristics for splitting and aggregating

#x is a geoid and variable data.frame
geo_impute<-function(x,geoid="geoid",from,to,type="percent",year=2019){#requires fips coding
  #hierarchy
  hierarchy<-factor(c("county","tract","bg"),ordered=T,levels=rev(c("county","tract","bg")))
  #load geographies from tigris
  geos<-c(from,to)
  if("county" %in% geos){
    county<-tigris::counties(year=year)
  }
  if("tract" %in% geos){
    tract<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::tracts(state=i,year=year,resolution="500k"))[,c("STATEFP","GEOID")]
      tract<-rbind(tract,out)
    }
  }
  if("bg" %in% geos){
    bg<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::block_groups(state=i,year=year))[,c("STATEFP","GEOID")]
      bg<-rbind(bg,out)
    }
  }
  #create spine on from data
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
  #expand to next level using to geographies
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

testing<-F
if(testing==T){
  d<-d1[d1$geo=="county",c("state","county","vehicle")]
  d$geoid<-paste0(str_pad(d$state,width=2,side="left",pad="0"),str_pad(d$county,width=3,side="left",pad="0"))
  ex<-geo_impute(x=d[,c("vehicle","geoid")],geoid="geoid",from="county",to="tract",type="rate",year=2019)
  
  d<-d1[d1$geo=="tract",c("state","county","tract","vehicle")]
  d$geoid<-paste0(str_pad(d$state,width=2,side="left",pad="0"),str_pad(d$county,width=3,side="left",pad="0"),str_pad(d$tract,width=6,side="left",pad="0"))
  ex<-geo_impute(x=d[,c("vehicle","geoid")],geoid="geoid",from="tract",to="county",type="rate",year=2019)
  
}