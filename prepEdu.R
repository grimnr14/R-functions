library(stringr)
library(tigris)

geo_impute<-function(x,geoid="geoid",from,to,type="percent",year=2019){#requires fips coding
  ipums.key<-"59cba10d8a5da536fc06b59d66e936288d8a4d7eb286182ea94a5c84"
  set_ipums_api_key(api_key=ipums.key)
  #hierarchy----
  hierarchy<-factor(c("county","tract","bg"),ordered=T,levels=rev(c("county","tract","bg")))
  #load geographies from ipumsr #tigris----
  geos<-c(from,to)
  geos<-ifelse(geos=="block group","blck_grp",
               ifelse(geos=="zip code tabulation area","zcta",geos))
  files<-get_metadata_nhgis('shapefiles',api_key=ipums.key)
  files<-files[files$year==year&
                 (str_detect(files$name,paste0("us_",geos[1]))|
                    str_detect(files$name,paste0("us_",geos[2]))),]$name
  
  if("county" %in% geos){
    #county<-tigris::counties(year=year)
    extract<-ipumsr::define_extract_nhgis(description=paste0("county_shp_",year),
                                          shapefiles=paste0("us_county_",year,"_tl",year))
    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    county<-read_ipums_sf(sf)
    file.remove(sf)
    remove(sf)
    gc()
    
  }
  if("tract" %in% geos){
    #tract<-NULL
    #for(i in state.abb){
    #  out<-as.data.frame(tigris::tracts(state=i,year=year,resolution="500k"))[,c("STATEFP","GEOID")]
    #  tract<-rbind(tract,out)
    #}
    extract<-ipumsr::define_extract_nhgis(description=paste0("tract_shp_",year),
                                          shapefiles=paste0("us_tract_",year,"_tl",year))
    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    tract<-read_ipums_sf(sf)
    file.remove(sf)
    remove(sf)
    gc()
  }
  if("bg" %in% geos){
    #bg<-NULL
    #for(i in state.abb){
    #  out<-as.data.frame(tigris::block_groups(state=i,year=year))[,c("STATEFP","GEOID")]
    #  bg<-rbind(bg,out)
    #}
    extract<-ipumsr::define_extract_nhgis(description=paste0("blck_grp_shp_",year),
                                          shapefiles=paste0("us_blck_grp_",year,"_tl",year))
    sf<-download_extract(wait_for_extract(submit_extract(extract)))
    bg<-read_ipums_sf(sf)
    file.remove(sf)
    remove(sf)
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

prepEducation<-function(year=2023,geography="county"){
  ccd<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/ccd_clean_",year,".txt"),header=T,sep="|")
  
  ct<-ccd[nchar(ccd$fips)<=5&!is.na(ccd$fips),
          c("fips","regular_pri","alt_pri","special_pri","votech_pri","other_private_edu","private_school","pri_prek","pri_total_students","pri_fte_teachers")]
  ct$fips<-str_pad(ct$fips,width=5,side="left",pad="0")
  
  if(geography=="tract"){#takes ccd private institution counts stuck at county and imputes to lower level assigned in geography
    outs<-data.frame(fips=NA)
    for(n in names(ct)[2:ncol(ct)]){
      out<-geo_impute(ct[,c("fips",n)],geoid="fips",from="county",to=geography,type="count",year=year)
      names(out)<-c(n,"fips")
      outs<-merge(outs,out,by="fips",all=T)
      outs<-outs[!is.na(outs$fips),]
      print(n)
    }
    ct<-outs[!duplicated(outs),]
    remove(outs,out)
    gc()
  }
  if(geography=="zcta"){
    if(year>=2020){
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_county20_natl.txt"),header=T,sep="|")
      map$zcta<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID_COUNTY_20,width=5,side="left",pad="0")
    }else{
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_county_rel_10.txt"),header=T,sep=",")
      map$zcta<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID,width=5,side="left",pad="0")
    }
    ct<-merge(ct,map[,c("zcta","fips")],by="fips",all.x=T)
    map$val<-1
    map<-aggregate(data=map[,c("fips","val")],val~.,FUN="sum")
    ct<-merge(ct,map,by="fips",all.x=T)
    for(n in 2:(ncol(ct)-2)){
      ct[,n]<-ct[,n]/ct[,"val"]
    }
    ct$fips<-ct$zcta
    ct<-ct[,1:(ncol(ct)-2)]
    ct<-ct[!is.na(ct$fips),]#fips are now zcta for this table
  }
  
  bg<-ccd[nchar(ccd$fips)>=11&!is.na(ccd$fips),]
  bg$fips<-str_pad(bg$fips,width=12,side="left",pad="0")
  if(geography=="tract"){
    bg$fips<-substr(bg$fips,1,11)
  }
  if(geography=="zcta"|geography=="zip code tabulation area"){
    bg$fips<-substr(bg$fips,1,11)#move bg to tract fips to join map
    if(year>=2020){
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
      map$zcta<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
    }else{
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T,sep=",")
      map$zcta<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID,width=11,side="left",pad="0")
    }
    bg<-merge(bg,map[!duplicated(map)&!is.na(map$zcta),c("zcta","fips")],by="fips",all.x=T)
    bg$fips<-bg$zcta
    bg<-bg[,1:(ncol(bg)-1)]
  }
  if(geography=="county"){
    bg$fips<-substr(bg$fips,1,5)
  }
  bg<-bg[!duplicated(bg)&!is.na(bg$fips),]%>%
    group_by(fips)%>%
    summarise_each(funs=c("sum"))
  ccd<-plyr::rbind.fill(bg,ct)
  ccd<-ccd[!duplicated(ccd)&!is.na(ccd$fips),]%>%
    group_by(fips)%>%
    summarise_each(funs=c("sum"))
  ccd[is.na(ccd)]<-0
  names(ccd)<-c("GEOID",names(ccd)[2:ncol(ccd)])
  ccd<-as.data.frame(ccd)
  
  ipeds<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/ipeds_clean_",year,".txt"),header=T,sep="|")
  ipeds$fips<-str_pad(ipeds$fips,width=12,side="left",pad="0")
  if(geography=="tract"){
    ipeds$fips<-substr(ipeds$fips,1,11)
  }
  if(geography=="zcta"|geography=="zip code tabulation area"){
    ipeds$fips<-substr(ipeds$fips,1,11)#move ipeds to tract fips to join map
    if(year>=2020){
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
      map$zcta<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
    }else{
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T,sep=",")
      map$zcta<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
      map$fips<-str_pad(map$GEOID,width=11,side="left",pad="0")
    }
    ipeds<-merge(ipeds,map[!duplicated(map)&!is.na(map$zcta),c("zcta","fips")],by="fips",all.x=T)
    ipeds$fips<-ipeds$zcta
    ipeds<-ipeds[,1:(ncol(ipeds)-1)]
  }
  if(geography=="county"){
    ipeds$fips<-substr(ipeds$fips,1,5)
  }
  
  ipedsSum<-ipeds[ipeds$fips!="0"&!is.na(ipeds$fips),c("fips",
                                                       "inactive_win12","undergrad_school_prog","grad_school_prog","hbcu","hospital","medical_school_prog","tribal_school_prog",
                                                       "public_college_university","post_secondary_institution","degree_granting","grants_associates_certificate_only",
                                                       "not_degree_granting","size_lt1k","size_1k_10k","size_gt10k",
                                                       "ft_noninst_academic_staff",
                                                       "ft_community_admin_staff","total_instructional_staff","total_inst_staff_salary","no_reported_inst_staff")]
  ipedsSum[is.na(ipedsSum)]<-0
  ipedsSum<-ipedsSum%>%group_by(fips)%>%summarise_each(funs=c("sum"))
  
  ipedsAvg<-ipeds[ipeds$fips!="0"&!is.na(ipeds$fips),c("fips",
                                                       "cost_grad_instate","cost_grad_outstate","cost_oncampus_living",
                                                       "cost_undergrad_instate","cost_undergrad_outstate",
                                                       "est_avg_inst_staff_salary")]
  ipedsAvg<-ipedsAvg%>%group_by(fips)%>%summarise_each(funs=c("mean"))
  ipeds<-merge(ipedsSum,ipedsAvg,by="fips",all=T)
  names(ipeds)<-c("GEOID",names(ipeds)[2:ncol(ipeds)])
  ipeds<-as.data.frame(ipeds)
  
  out<-merge(ccd,ipeds,by="GEOID",all=T)
  remove(ipeds,ccd)
  out
}
#ex<-prepEducation(year=2023,geography="county")
