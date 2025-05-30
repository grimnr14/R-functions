#Census shapefile extraction for GeoHealth

library(tigris)
library(sf)
library(stringr)
library(censusxy)

geo_locate<-function(obj,id=NULL,dims=2,benchmark="Public_AR_Current",vintage="Current_Current",geography="counties"){#accepts an sf object
  outs<-NULL
  obj$geometry<-st_zm(obj$geometry,drop=T)
  for(i in unique(as.data.frame(obj)[,id])){
    e<-obj[as.data.frame(obj)[,id]==i,"geometry"]
    #len_m<-st_length(e)
    e<-unlist(st_boundary(e$geometry))
    e<-matrix(e,nrow=length(e)/dims)#2 points assumed on all, but may be 3
    if(nrow(e)>0){
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

#geo_locate(obj=rd[1:100,],id="LINEARID",geography="counties")

pullSHP<-function(year=2023,state=state.abb,geography="county",outdir=NULL,scope="light"){
  for(y in year){
    for(s in state){
      hw<-primary_secondary_roads(state=s,year=y)
      lm<-landmarks(state=s,year=y)#major highways and interstates
      lmlu<-data.frame(
        MTFCC=c("C3081","K1231"),
        type=c("locality point","hospital,hospice,urgent care"))
      lm<-lm[lm$MTFCC %in% c("K1231")|
               (lm$MTFCC=="C3081"&str_detect(lm$FULLNAME,"Metro Sta")),]
      ct<-counties(state=s,year=y)
      for(g in geography){
        exp<-NULL
        for(u in unique(ct$COUNTYFP)){
          if(g=="tracts"){
            gp<-tracts(state=s,year=y)
            gp<-gp[substr(gp$GEOID,3,5)==u,]
          }
          if(g=="zcta"){
            gp<-zctas(state=s,year=2010)
            out<-NULL
            for(i in 1:nrow(gp)){
              ints<-st_intersects(ct[ct$COUNTYFP==u,]$geometry,gp[i,])
              if(!is.na(as.numeric(ints))){
                out<-rbind(out,gp[i,])
              }
            }
            gp<-out
            gp$GEOID<-gp$GEOID10
            remove(out)
            gc()
          }
          if(g=="county"){
            gp<-counties(state=s,year=y)
            gp<-gp[substr(gp$GEOID,3,5)==u,]
          }
          if(g=="block group"){
            gp<-block_groups(state=s,year=y)
            gp<-gp[substr(gp$GEOID,3,5)==u,]
          }
#          gp<-gp[1,]#if testing one geo
          
          rn<-address_ranges(state=s,year=y,county=u)
          rn<-rn[as.numeric(st_length(rn$geometry))>100,]
          ex<-st_intersects(rn,gp,sparse=F)
          ex<-matrix(data=as.numeric(ex),nrow=nrow(rn),byrow=F)
          rn<-rn[rowSums(ex)>0,]
          
          rd<-roads(state=s,year=y,county=u)#all roads
          rd<-rd[as.numeric(st_length(rd$geometry))>100,]#100m is about 1 block
          ex<-st_intersects(rd,gp,sparse=F)
          ex<-matrix(data=as.numeric(ex),nrow=nrow(rd),byrow=F)
          rd<-rd[rowSums(ex)>0,]
          
          #find all roads intersecting or within geographies
          outs<-NULL
          for(i in 1:nrow(rd)){
            ints<-st_intersects(rd[i,]$geometry,gp)
            withs<-st_within(rd[i,]$geometry,gp)
            size<-nrow(gp[c(ints[[1]],withs[[1]]),])
            
            if(size>0){
              print(as.data.frame(gp[c(ints[[1]],withs[[1]]),"GEOID"]))
              out<-NULL
              for(k in 1:size){
                out<-rbind(out,as.data.frame(rd[i,c("LINEARID","FULLNAME","geometry")]))
              }#duplicates k times for cbind
              out<-cbind(as.data.frame(gp[c(ints[[1]],withs[[1]]),c("GEOID","ALAND","geometry")]),out)
              if(length(withs[[1]])>0){
                out$st_type<-ifelse(rownames(out) %in% withs[[1]],"within","intersects")
              }else{
                out$st_type<-"intersects"
              }
              outs<-rbind(outs,out)
              par(mfrow=c(1,1))
              plot(out[,3],col="lightgrey",main=paste0("Intersecting road ",s,": ",g))
              lines(matrix(unlist(out[1,6]),
                           nrow=length(unlist(out[1,6]))/2,
                           ncol=2,
                           byrow=F),
                    type="l",lty=1,lwd=3,
                    col="darkred")#identifies small area points
            }
          }
          
          rn$HNNAME<-paste0(rn$LFROMHN," to ",rn$LTOHN," ",rn$FULLNAME)
          
          #find all intersecting address ranges
          outs2<-NULL
          for(i in unique(rn[rn$LINEARID %in% unique(rd$LINEARID)&
                             as.character(rn$geometry)!="GEOMETRYCOLLECTION EMPTY",]$LINEARID)){
            ints<-st_intersects(rn[rn$LINEARID==i,]$geometry,gp)
            withs<-st_within(rn[rn$LINEARID==i,]$geometry,gp)
            ex<-gp[c(ints[[1]],withs[[1]]),]
            size<-nrow(ex)
            
            if(size>0){
              print(as.data.frame(ex[,"GEOID"]))
              out<-NULL
              for(k in 1:size){
                ex2<-as.data.frame(rn[rn$LINEARID==i,c("LINEARID","HNNAME","geometry")])
                for(e in 1:nrow(ex2)){
                  out<-rbind(out,
                             cbind(as.data.frame(ex[k,c("GEOID","ALAND","geometry")]),
                                   ex2[e,]))
                }
              }#duplicates k times for cbind
#              ex<-as.data.frame(gp[c(ints[[1]],withs[[1]]),c("GEOID","ALAND")])
#              out<-merge(ex,out,by=c("GEOID","ALAND"))
              remove(ex,ex2)
              gc()
              if(length(withs[[1]])>0){
                out$st_type<-ifelse(rownames(out) %in% withs[[1]],"within","intersects")
              }else{
                out$st_type<-"intersects"
              }
              outs2<-rbind(outs2,out)#THROWS ERROR
              par(mfrow=c(1,1))
              plot(out[,3],col="lightgrey",main=paste0("Address ranges ",s,": ",g))
              lines(matrix(unlist(out[1,6]),
                           nrow=length(unlist(out[1,6]))/2,
                           ncol=2,
                           byrow=F),
                    type="l",lty=1,lwd=3,
                    col="darkgreen")#identifies first point only
            }
          }
          
          outs<-merge(outs,outs2[,c(1,4,5,6,7)],by=c("GEOID","LINEARID"),all=T)
          
          names(outs)<-c("GEOID","LINEARID","ALAND","geometry","FULLNAME","geometry","road_st_type","HNNAME","geometry","address_st_type")
          outs<-outs[!duplicated(outs),]
          
          #get length of lines
          outs$length_road<-st_length(outs[,6])#col6 is roads
          outs$length_address<-st_length(outs[,9])
          #calculate distances and intersections on roads
          if(scope=="full"){
            ex<-NULL
            ex2<-NULL
            ex3<-NULL
            ex4<-NULL
            ex5<-NULL
            for(i in 1:nrow(outs)){
              ex<-c(ex,mean(st_distance(st_centroid(outs[i,4]),outs[i,6])))
              ex2<-c(ex2,max(st_distance(outs[i,6],st_centroid(outs[i,9]))))
              ex3<-c(ex3,min(st_distance(st_centroid(outs[i,9]),lm[lm$MTFCC=="K1231",]$geometry)))#closest hospital to residence
              ex4<-c(ex4,min(st_distance(st_centroid(outs[i,9]),lm[lm$MTFCC=="3081"|str_detect(lm$FULLNAME,"Metro Sta"),]$geometry)))#closest hospital to residence
              ex5<-c(ex5,min(st_distance(st_centroid(outs[i,9]),hw$geometry)))#closest hospital to residence
              
              print(paste0(round(100*i/nrow(outs),1),"%"))
            }
            outs$meancentroid_2road_distance<-ex
            outs$maxaddressctr_2road_dististance<-ex2
            outs$minaddressctr_2hospital_distance<-ex3
            outs$minaddressctr_2metro_distance<-ex4
            outs$minaddressctr_2highway_distance<-ex5
            
            out<-NULL
            for(i in unique(outs$GEOID)){#for each group
              print(i)
              d<-outs[outs$GEOID==i,c(1,2,4,6)]
              d<-d[!duplicated(d$LINEARID),]
              d$unique_road<-length(unique(d$LINEARID))
              if(nrow(d)>1&length(unique(d$LINEARID))>1){
                for(j in unique(d$LINEARID)){#each line
                  l<-d[d$LINEARID==j,]
                  k<-d[d$LINEARID!=j&row(d)[,1] %in% c(st_intersects(l$geometry.1,d$geometry.1)[[1]]),]
                  for(n in 1:nrow(k)){#for other lines in d
                    size<-st_intersection(l[1,4],k[n,4])
                    if(length(st_as_text(size))>0){#intersects 1+
                      pt<-st_as_text(st_multipoint(st_coordinates(st_cast(size,"POINT"))))
                      par(mfrow=c(1,1))
                      plot(k[n,4],col="steelblue",main=paste0("GEOID:",i,"; LINE:",j),lwd=2)
                      lines(matrix(unlist(l[1,4]),
                                   nrow=length(unlist(l[1,4]))/2,
                                   ncol=2,
                                   byrow=F),
                            type="l",lwd=2,lty=2,
                            col="black")#identifies first point only
                    }else{
                      pt<-NA
                    }
                    print(paste0(j,"; ",n))
                    out<-rbind(out,data.frame(GEOID=i,LINEARID=j,unique_roads=rep(k$unique_road[1]),point=pt))
                    out<-out[!is.na(out$point),]
                  }
                }
              }
              out<-out[!is.na(out$point)&!duplicated(out$point),]#all intersections
            }
            out<-out[!is.na(out$point)&!duplicated(out$point),]#all intersections
            out$point<-st_as_sfc(out$point)
            out$point<-st_centroid(out$point)
            out$point<-st_zm(out$point)#TEST THIS, NEED NO XYM or XYZM
            outs<-merge(outs,out[,c("GEOID","LINEARID","point")],by=c("GEOID","LINEARID"),all.x=T)
            
            ex<-NULL
            ex2<-NULL
            ###POTENTIAL PROBLEM VVVV
            for(i in 1:nrow(outs)){#if this keeps throwing error, try subsetting removed null addresses and merging back by geoid
              ex<-c(ex,min(st_distance(st_centroid(outs[i,9]),st_set_crs(out$point,value="NAD83"))))
              ex2<-c(ex2,min(st_distance(st_centroid(outs[i,9]),hw$geometry)))
              print(paste0(round(100*i/nrow(outs),1),"%"))
            }
            outs$minaddressctr_2intersection_distance<-ex
            outs$minaddressctr_2highway_distance<-ex2
            
            out$value<-1
            ex<-aggregate(data=out[,c("GEOID","unique_roads","value")],value~.,FUN="sum")
            names(ex)<-c("GEOID","total_roads","total_road_intersections")
            outs<-merge(outs,ex,by="GEOID",all.x=T)
            
          }
          print(u)
          exp<-rbind(exp,as.data.frame(outs)[!names(outs) %in% c("geometry")])
        }
        saveRDS(exp,paste0(outdir,s,"_",g,"_intersects_",y,".rds"))
        print(paste0(s,"_",g,"_",y,"_",u,": ",Sys.time()))
        remove(ints,exp,out,outs,outs2,withs,ex,ex2,ex3,ex4,ex5)
        gc()
      }
    }
  }
}

testing<-F
#testing
if(testing==T){
  pullSHP(year=2023,state="DE",geography="block group",outdir="C:/Users/chris/OneDrive/Documents/GitHub/shpfiles/transportation shp/")
  pullSHP(year=2023:2012,state=state.abb,geography=c("block group"),
          outdir="C:/Users/chris/OneDrive/Documents/GitHub/shpfiles/transportation shp/")
}
