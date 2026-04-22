library(stringr)
library(tigris)
library(tidyr)

geo_impute<-function(x,geoid="geoid",from,to,type="percent",year=2019){#requires fips coding
  #hierarchy----
  hierarchy<-factor(c("county","tract","bg"),ordered=T,levels=rev(c("county","tract","bg")))
  
  #load geographies from ipumsr #tigris----
  geos<-c(from,to)
  geos<-ifelse(geos=="block group","blck_grp",
               ifelse(geos=="zip code tabulation area","zcta",geos))
  
  if("county" %in% geos){
    county<-tigris::counties(year=year)
    gc()
    
  }
  if("tract" %in% geos){
    tract<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::tracts(state=i,year=year,resolution="500k"))[,c("STATEFP","GEOID")]
      tract<-rbind(tract,out)
    }
    gc()
  }
  if("bg" %in% geos){
    bg<-NULL
    for(i in state.abb){
      out<-as.data.frame(tigris::block_groups(state=i,year=year))[,c("STATEFP","GEOID")]
      bg<-rbind(bg,out)
    }
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

prepEconomic<-function(year=2022,geography="county"){
  out<-fips_codes
  out$fips<-paste0(out$state_code,out$county_code)
  pop<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/population.csv"),header=T)
  pop<-pop[pop$year==year&pop$geolevel=="county",]
  pop$GEOID<-str_pad(pop$GEOID,5,"left","0")
  pop$state<-substr(pop$GEOID,1,2)
  st<-pop[,c("state","B01001_001")]
  st<-aggregate(data=st,.~state,FUN="sum")
  names(st)<-c("state","statepop")
  pop<-merge(pop,st,by="state",all.x=T)
  remove(st)
  geopop<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/population.csv"),header=T)
  geopop<-geopop[geopop$year==year&geopop$geolevel==geography,]
  geopop<-geopop[!duplicated(geopop),]
  
  soi<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/irs_soi_2013_2022.csv"),header=T)
  soi<-soi[soi$year==year,]
  if(nrow(soi)==0&year>2022){
    soi<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/irs_soi_2013_2022.csv"),header=T)
    soi<-soi[soi$year==2022,]
  }
  soi$STATEFIPS<-str_pad(soi$STATEFIPS,width=2,side="left",pad="0")
  soi$COUNTYFIPS<-str_pad(soi$COUNTYFIPS,width=3,side="left",pad="0")
  soi$FIPS<-paste0(soi$STATEFIPS,soi$COUNTYFIPS)
  names(soi)<-c("STATE","STATEFIPS","COUNTYFIPS","agi_stub","count.returns","count.individuals","AGI","countreturns.wtotalincome","totalincome.amount","countreturns.wtotalpayments","totalpayments.amount","countreturns.wtaxdue","taxdue.amount","countreturns.wtaxpaid","taxpaid.amount","countreturns.wtaxableincome","taxableincome.amount","countreturns.wtaxliability","taxliability.amount","year","FIPS")
  soi<-soi[complete.cases(soi),]
  
  lt25k<-soi[soi$agi_stub<4,!names(soi) %in% c("agi_stub")]
  lt25k<-aggregate(data=lt25k,.~STATE+STATEFIPS+COUNTYFIPS+FIPS+year,FUN="sum")
  lt25k$ratioReturnsIndividuals<-lt25k$count.returns/lt25k$count.individuals
  lt25k$avgAGIperReturn<-lt25k$AGI/lt25k$count.returns#in $1000
  lt25k$avgAGIperIndividual<-lt25k$AGI/lt25k$count.individuals#in $1000
  lt25k$avgTotalIncomeperReturn<-lt25k$totalincome.amount/lt25k$countreturns.wtotalincome#in $1000
  lt25k$avgTotalPaymentperReturn<-lt25k$totalpayments.amount/lt25k$countreturns.wtotalpayments
  lt25k$avgTaxDueperReturn<-lt25k$taxdue.amount/lt25k$countreturns.wtaxdue
  lt25k$avgTaxPaidperReturn<-lt25k$taxpaid.amount/lt25k$countreturns.wtaxpaid
  lt25k$avgTaxableIncomeperReturn<-lt25k$taxableincome.amount/lt25k$countreturns.wtaxableincome
  lt25k$avgTaxLiabilityperReturn<-lt25k$taxliability.amount/lt25k$countreturns.wtaxliability
  
  gte200k<-soi[soi$agi_stub==8,!names(soi) %in% c("agi_stub")]
  gte200k$ratioReturnsIndividuals<-gte200k$count.returns/gte200k$count.individuals
  gte200k$avgAGIperReturn<-gte200k$AGI/gte200k$count.returns#in $1000
  gte200k$avgAGIperIndividual<-gte200k$AGI/gte200k$count.individuals#in $1000
  gte200k$avgTotalIncomeperReturn<-gte200k$totalincome.amount/gte200k$countreturns.wtotalincome#in $1000
  gte200k$avgTotalPaymentperReturn<-gte200k$totalpayments.amount/gte200k$countreturns.wtotalpayments
  gte200k$avgTaxDueperReturn<-gte200k$taxdue.amount/gte200k$countreturns.wtaxdue
  gte200k$avgTaxPaidperReturn<-gte200k$taxpaid.amount/gte200k$countreturns.wtaxpaid
  gte200k$avgTaxableIncomeperReturn<-gte200k$taxableincome.amount/gte200k$countreturns.wtaxableincome
  gte200k$avgTaxLiabilityperReturn<-gte200k$taxliability.amount/gte200k$countreturns.wtaxliability
  
  soi<-soi[soi$agi_stub<4,!names(soi) %in% c("agi_stub")]
  soi<-aggregate(data=soi,.~STATE+STATEFIPS+COUNTYFIPS+FIPS+year,FUN="sum")
  soi$ratioReturnsIndividuals<-soi$count.returns/soi$count.individuals
  soi$avgAGIperReturn<-soi$AGI/soi$count.returns#in $1000
  soi$avgAGIperIndividual<-soi$AGI/soi$count.individuals#in $1000
  soi$avgTotalIncomeperReturn<-soi$totalincome.amount/soi$countreturns.wtotalincome#in $1000
  soi$avgTotalPaymentperReturn<-soi$totalpayments.amount/soi$countreturns.wtotalpayments
  soi$avgTaxDueperReturn<-soi$taxdue.amount/soi$countreturns.wtaxdue
  soi$avgTaxPaidperReturn<-soi$taxpaid.amount/soi$countreturns.wtaxpaid
  soi$avgTaxableIncomeperReturn<-soi$taxableincome.amount/soi$countreturns.wtaxableincome
  soi$avgTaxLiabilityperReturn<-soi$taxliability.amount/soi$countreturns.wtaxliability
  
  ex<-merge(lt25k,gte200k,by=c("FIPS","year"),all=T)
  soi<-merge(soi,ex,by=c("FIPS","year"),all=T)
  names(soi)<-str_replace_all(names(soi),"[.]x","_lt25k")
  names(soi)<-str_replace_all(names(soi),"[.]y","_gte200k")
  soi<-soi[,c("FIPS","year",names(soi)[str_detect(names(soi),"avg")|str_detect(names(soi),"ratio")])]
  remove(gte200k,lt25k,ex)
  out<-merge(out,soi,by.x="fips",by.y="FIPS",all.x=T)
  
  gdp<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/bea_regional_gdp_county_",year,".csv"),header=T)
  gdp<-gdp[,c("GeoFips","year","value","CL_UNIT","key")]
  gdp$value<-ifelse(gdp$CL_UNIT=="Dollars",as.numeric(gdp$value)/1000,gdp$value)
  gdp$CL_UNIT<-ifelse(gdp$CL_UNIT=="Dollars"|gdp$CL_UNIT=="Thousands of chained 2017 dollars","Thousands of dollars",gdp$CL_UNIT)
  gdp<-gdp[!duplicated(gdp)&gdp$key!="key",]
  gdp<-spread(gdp[,names(gdp)[!names(gdp) %in% c("CL_UNIT")]],key=key,value=value,fill=0)
  names(gdp)<-c("GeoFips","year","summary.gdp","county.perchangegdp","county.gdp","chained.quantity.index","county.realgdp","personalincome.percapita")
  out<-merge(out,gdp,by.x="fips",by.y="GeoFips",all.x=T)

  pce<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/state%20level%20regional%20pce.csv"),header=T)
  pce<-pce[,c("Code","GeoFips","year","CL_UNIT","table",names(pce)[str_detect(names(pce),as.character(year))])]
  pce<-pce[!duplicated(pce)&pce$GeoFips!=0&pce$year==year,]
  pce[,paste0("DataValue_",year)]<-ifelse(pce$CL_UNIT=="Dollars",pce[,paste0("DataValue_",year)]/1000,pce[,paste0("DataValue_",year)]*1000)
  pce$lineCode<-ifelse(str_detect(pce$Code,"SAPCE"),substr(pce$Code,8,nchar(pce$Code)),substr(pce$Code,7,nchar(pce$Code)))
  pce$GeoFips<-str_pad(pce$GeoFips,side="left",pad="0",width=5)
  pce$description<-ifelse(pce$lineCode==1,"All",ifelse(pce$lineCode==2,"Goods",ifelse(pce$lineCode==13,"Services",ifelse(pce$lineCode==16,"HealthCare",ifelse(pce$lineCode==8,"NonDurableGoods",NA)))))
  pce$description<-ifelse(pce$table=="SARPI"&pce$lineCode==1,"R-PI",ifelse(pce$table=="SARPI"&pce$lineCode==2,"R-perCapPI",ifelse(pce$table=="SARPI"&pce$lineCode==3,"R-PCE",ifelse(pce$table=="SARPI"&pce$lineCode==4,"R-perCapPCE",pce$description))))
  pce$description<-ifelse(pce$table=="SARPP"&pce$lineCode==1,"RPP-All",ifelse(pce$table=="SARPP"&pce$lineCode==2,"RPP-Goods",pce$description))
  pce<-pce[!is.na(pce$description),!names(pce) %in% c("CL_UNIT","Code")]
  pce<-spread(pce,key=table,value=names(pce)[str_detect(names(pce),"DataValue_")],fill=NA)#unit now in thousands
  names(pce)<-c("GeoFips","year","LineCode","Description","State.PCE.1k","Individual.PCE.1k","LowLevelProduct.State.PCE.1k","SARPI","RPP")
  all.pce<-spread(data=pce[is.na(pce$SARPI)&is.na(pce$RPP),c("GeoFips","year","Description","State.PCE.1k")],key=Description,value=State.PCE.1k,fill=NA)
  names(all.pce)<-c("GeoFips","year",paste0("total_",names(all.pce[3:ncol(all.pce)])))
  ind.pce<-spread(data=pce[is.na(pce$SARPI)&is.na(pce$RPP),c("GeoFips","year","Description","Individual.PCE.1k")],key=Description,value=Individual.PCE.1k,fill=NA)
  names(ind.pce)<-c("GeoFips","year",paste0("ind_",names(all.pce[3:ncol(all.pce)])))
  rpi<-spread(data=pce[!is.na(pce$SARPI),c("GeoFips","year","Description","SARPI")],key="Description",value=SARPI,fill=NA)
  rpp<-spread(data=pce[!is.na(pce$RPP),c("GeoFips","year","Description","RPP")],key="Description",value=RPP,fill=NA)
  pce<-merge(all.pce,ind.pce,by=c("GeoFips","year"),all.x=T)
  pce<-merge(pce,rpi,by=c("GeoFips","year"),all.x=T)
  pce<-merge(pce,rpp,by=c("GeoFips","year"),all.x=T)
  remove(rpi,rpp,all.pce,ind.pce)
  pce$fips<-substr(pce$GeoFips,1,2)
  pce<-merge(pce,pop[,c("state","GEOID","statepop","B01001_001")],by.x="fips",by.y="state",all.x=T)
  pce$propPop<-pce$B01001_001/pce$statepop
  for(i in c("total_All","total_Goods","total_HealthCare","total_NonDurableGoods","total_Services","ind_total_All","ind_total_Goods","ind_total_HealthCare","ind_total_NonDurableGoods","ind_total_Services","R-PCE","RPP-All","RPP-Goods","R-PI")){
    pce[,i]<-pce$propPop*pce[,i]#adjusts each area's pce by population, ASSUMES EQUAL CONSUMPTION PER PERSON ACROSS ALL STATE
  }
  for(i in c("R-perCapPCE","R-perCapPI","RPP-All","RPP-Goods")){
    pce[,i]<-pce[,i]/1000
  }
  pce$realPersonalResidual<-pce$`R-PI`-pce$`R-PCE`#county level left over income after pce
  out<-merge(out,pce,by.x=c("fips","state_code"),by.y=c("GEOID","fips"),all.x=T)

  if(year>=2020){
    stc<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FY",year,"-STC-Category-Table-Transposed.csv"),header=T,skip=4)
  }else{
    stc<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/FY",year,"-STC-Category-Table-Transposed.csv"))
  }
  stc<-gather(data=stc,key=state,value=value,United.States:Wyoming)
  stc<-spread(data=stc[,c("state","Tax.Type","value")],key=Tax.Type,value=value,fill="-")
  stc<-stc[stc$state!="United.States",c("state","Income Taxes","License Taxes","Other Taxes","Property Taxes","Sales and Gross Receipts Taxes","Total Taxes")]
  stc[stc=="X"]<-0
  stc$abb<-c(state.abb[1:7],"DC",state.abb[8:50])
  stc$fips<-fips_codes[!duplicated(fips_codes[,c("state","state_code")]),]$state_code[1:51]
  stc$fips<-ifelse(stc$abb=="DC","11",ifelse(stc$abb=="DE","10",stc$fips))
  stc<-merge(stc,pop[,c("state","GEOID","statepop","B01001_001")],by.x="fips",by.y="state",all.x=T)
  stc$IncomeTax<-(as.numeric(stc$`Income Taxes`)/stc$statepop)
  stc$LicenseTax<-(as.numeric(stc$`License Taxes`)/stc$statepop)
  stc$OtherTax<-(as.numeric(stc$`Other Taxes`)/stc$statepop)
  stc$PropertyTax<-(as.numeric(stc$`Property Taxes`)/stc$statepop)
  stc$SalesReceiptsTax<-(as.numeric(stc$`Sales and Gross Receipts Taxes`)/stc$statepop)
  stc$TotalTax<-(as.numeric(stc$`Total Taxes`)/stc$statepop)
  out<-merge(out,stc,by.x=c("state","fips"),by.y=c("abb","GEOID"),all.x=T)
  
  debt<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/State_Debt_tables_dy",year,".csv"),header=T,skip=3)
  names(debt)<-c("state","1k.households","per.wdebt","per.secureddebt","per.homedebt","per.vehicledebt","per.unsecuredebt","per.creditcarddebt")
  debt<-debt[str_detect(debt$state,"Total")==F&
               str_detect(debt$state,"State")==F&
               str_detect(debt$state,"Source")==F&
               str_detect(debt$state,"NOTE")==F&
               str_detect(debt$state,"Internet")==F&
               str_detect(debt$state,"Footnotes")==F&
               debt$state!="",]
  debt$abb<-c(state.abb[1:7],"DC",state.abb[8:50])
  debt$householdswdebt<-(debt$per.wdebt/100)*(debt$`1k.households`*1000)
  debt$householdssecureddebt<-(debt$per.secureddebt/100)*(debt$`1k.households`*1000)
  debt$householdshomedebt<-(debt$per.homedebt/100)*(debt$`1k.households`*1000)
  debt$householdsvehicledebt<-(debt$per.vehicledebt/100)*(debt$`1k.households`*1000)
  debt$householdsunsecuredebt<-(debt$per.unsecuredebt/100)*(debt$`1k.households`*1000)
  debt$householdscreditcarddebt<-(debt$per.creditcarddebt/100)*(debt$`1k.households`*1000)
  out<-merge(out,debt,by.x="state",by.y="abb",all.x=T)
  
  out<-merge(out,pop,by.x="fips",by.y="GEOID",all.x=T)
  remove(debt,gdp,pce,soi,stc)
  
  if(year>2022){
    y<-2022
  }else{
    y<-year
  }
  #do qwi vars----
  qwi<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/qwi_final_selected.csv"),header=T)
  qwi$geography<-str_pad(qwi$geography,width=5,side="left",pad="0")
  qwi<-qwi[qwi$industry=="00"&qwi$year==y,]
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
      if(y<2020){
        map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
        map$GEOID<-str_pad(map$GEOID,width=11,side="left",pad="0")
        map$ZCTA5<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
        
        es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="tract",by.y="GEOID")
        es<-es[!duplicated(es)&!is.na(es$ZCTA5),]
        es<-as.data.frame(aggregate(data=es[,!names(es) %in% c("GEOID","tract")],.~ZCTA5,FUN="sum"))
      }else{
        map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
        map$GEOID<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
        map$ZCTA5<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
        
        es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="tract",by.y="GEOID")
        es<-es[!duplicated(es)&!is.na(es$ZCTA5)&es$ZCTA5!="",]
        es<-as.data.frame(aggregate(data=es[,!names(es) %in% c("GEOID","tract")],.~ZCTA5,FUN="sum"))
      }
      es$geography<-es$ZCTA5
    }
    qwi<-as.data.frame(es[,!names(es) %in% c("tract","GEOID","ZCTA5")])
    remove(e,es)
    gc()
  }
  
  lodes<-read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/lodes_all_years_wide.csv",header=T)
  lodes$geoid<-str_pad(lodes$geoid,width=11,side="left",pad="0")
  lodes<-lodes[lodes$year==y,]
  es<-lodes
  es$geography<-es$geoid
  if(geography=="county"){
    #    es<-data.frame(tract=NA)
    vars<-c("C000","CA01","CA02","CA03","CFA01","CFA02","CFA03","CFA04","CFS01","CFS02","CFS03","CFS04","SI01","SI02","SI03","CE01","CE02","CE03")
    es$geography<-substr(es$geography,1,5)
    es[is.na(es)]<-0
    es<-as.data.frame(aggregate(data=es[,c("geography",vars)],.~geography,FUN="sum"))
    es<-as.data.frame(es)
  }
  if(geography=="zcta"){
    if(y<2020){
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
      map$GEOID<-str_pad(map$GEOID,width=11,side="left",pad="0")
      map$ZCTA5<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
      
      es<-merge(lodes,map[,c("GEOID","ZCTA5")],by.x="geoid",by.y="GEOID")
      es<-es[!duplicated(es)&!is.na(es$ZCTA5),]
      es[is.na(es)]<-0
      es<-as.data.frame(aggregate(data=es[,!names(es) %in% c("GEOID","geoid","geography","year","state")],.~ZCTA5,FUN="sum"))
    }else{
      map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
      map$GEOID<-str_pad(map$GEOID_TRACT_20,width=11,side="left",pad="0")
      map$ZCTA5<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
      
      es<-merge(es,map[,c("GEOID","ZCTA5")],by.x="geoid",by.y="GEOID")
      es<-es[!duplicated(es)&!is.na(es$ZCTA5)&es$ZCTA5!="",]
      es<-as.data.frame(aggregate(data=es[,!names(es) %in% c("GEOID","geoid","geography","year","state")],.~ZCTA5,FUN="sum"))
    }
    es$geography<-es$ZCTA5
    es<-es[,!names(es) %in% c("ZCTA5")]
  }
  
  lodes<-as.data.frame(es[,!names(es) %in% c("tract","geoid","GEOID","ZCTA5")])
  remove(e,es,map)
  gc()
  
  lodes<-lodes[,c("geography","C000","CFA01","CFA02","CFA03","CFA04","CFS01","CFS02","CFS03","CFS04")]
  qwi<-qwi[,c("geography","Emp","EmpEnd","EmpS")]
  lodes<-merge(lodes,qwi,by="geography")
  
  remove(qwi)
  gc()
  
  #merge----  
  county.cnts<-c("summary.gdp","county.gdp","county.realgdp",
                 "total_All","total_Goods","total_HealthCare","total_NonDurableGoods","total_Services",
                 "ind_total_All","ind_total_Goods","ind_total_HealthCare","ind_total_NonDurableGoods","ind_total_Services",
                 "R-PCE","R-PI","realPersonalResidual",
                 "IncomeTax","LicenseTax","OtherTax","PropertyTax","SalesReceiptsTax","TotalTax"
                 )
  prcs<-c("ratioReturnsIndividuals","avgAGIperReturn","avgAGIperIndividual","avgTotalIncomeperReturn","avgTotalPaymentperReturn","avgTaxDueperReturn","avgTaxPaidperReturn","avgTaxableIncomeperReturn","avgTaxLiabilityperReturn",
          "ratioReturnsIndividuals_lt25k","avgAGIperReturn_lt25k","avgAGIperIndividual_lt25k","avgTotalIncomeperReturn_lt25k","avgTotalPaymentperReturn_lt25k","avgTaxDueperReturn_lt25k","avgTaxPaidperReturn_lt25k","avgTaxableIncomeperReturn_lt25k","avgTaxLiabilityperReturn_lt25k",
          "ratioReturnsIndividuals_gte200k","avgAGIperReturn_gte200k","avgAGIperIndividual_gte200k","avgTotalIncomeperReturn_gte200k","avgTotalPaymentperReturn_gte200k","avgTaxDueperReturn_gte200k","avgTaxPaidperReturn_gte200k","avgTaxableIncomeperReturn_gte200k","avgTaxLiabilityperReturn_gte200k",
          "county.perchangegdp","chained.quantity.index","personalincome.percapita",
          "per.wdebt","per.secureddebt","per.homedebt","per.vehicledebt","per.unsecureddebt","per.creditcarddebt",
          "R-perCapPCE","R-perCapPI","RPP-All","RPP-Goods"
  )
  runs<-c(names(out)[!names(out) %in% c("state","fips","county","year")])
  skip<-c("year.x","year.y","state.x","state.y","county_code","GeoFips","state_code","state_name","geolevel","NAME","statepop.x","statepop.y","fips.x","fips.y")

  if(year<2020){
    map<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/zcta_tract_rel_10.txt"),header=T)
    map$zcta<-str_pad(map$ZCTA5,width=5,side="left",pad="0")
    map<-map[,c("GEOID","zcta")]
    map<-map[!duplicated(map),]
  }else{
    map<-read.table(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/tab20_zcta520_tract20_natl.txt"),header=T,sep="|")
    map$zcta<-str_pad(map$GEOID_ZCTA5_20,width=5,side="left",pad="0")
    map$GEOID<-map$GEOID_TRACT_20
    map<-map[,c("GEOID","zcta")]
    map<-map[!duplicated(map),]
  }

  if(geography=="tract"){
    base<-data.frame(GEOID=str_pad(map$GEOID,width=11,side="left",pad="0"),county=substr(str_pad(map$GEOID,width=11,side="left",pad="0"),1,5))
    base<-base[!duplicated(base),]
    geopop$GEOID<-str_pad(geopop$GEOID,width=11,side="left",pad="0")
    base<-merge(base,geopop[,c("GEOID","B01001_001")],by.x="GEOID",by.y="GEOID",all.x=T)
    out<-merge(base,out,by.x="county",by.y="fips",all.x=T)
    for(i in c(county.cnts)){
      out[,i]<-as.numeric(out[,i])/out$B01001_001.x#adjust for each geography's population by higher level rate, county or state
    }
  }
  if(geography=="zcta"){
    base<-data.frame(GEOID=str_pad(map$zcta,width=5,side="left",pad="0"),county=substr(str_pad(map$GEOID,width=11,side="left",pad="0"),1,5))
    base<-base[!duplicated(base)&!is.na(base$GEOID),]
    geopop$GEOID<-str_pad(geopop$GEOID,width=5,side="left",pad="0")
    base<-merge(base,geopop[!duplicated(geopop[,c("GEOID","B01001_001")]),c("GEOID","B01001_001")],by.x="GEOID",by.y="GEOID",all.x=T)
    out<-merge(base,out,by.x="county",by.y="fips",all.x=T)
    for(i in c(county.cnts)){
      out[,i]<-as.numeric(out[,i])/out$B01001_001.x#adjust for each geography's population by higher level rate, county or state
    }
  }
  if(geography=="county"){
    base<-data.frame(GEOID=str_pad(map$GEOID,width=11,side="left",pad="0"),county=substr(str_pad(map$GEOID,width=11,side="left",pad="0"),1,5))
    base$GEOID<-substr(base$GEOID,1,5)
    base<-base[!duplicated(base),]
    out<-merge(base,out,by.x="GEOID",by.y="fips",all.x=T)
    
  }
  remove(pop,base,geopop,map,county.cnts,prcs,runs)
  out<-out[,!names(out) %in% c(skip,"county","county.y","county.x","B01001_001.x","B01001_001.y")]
  out<-out[!duplicated(out),]
  for(i in 2:78){
    out[,i]<-as.numeric(out[,i])
  }
  out<-out[!is.na(out$GEOID),]
  out<-merge(out,lodes,by.x="GEOID",by.y="geography",all.x=T)
  
}

#ex<-prepEconomic(year=2021,geography="zcta")
#ex<-prepEconomic(year=2019,geography="tract")
#flat_map(ex,year=2019,state="MD",geography="tract",geoid="GEOID",var="summary.gdp",type="flat")
#flat_map(ex,year=2019,state="MD",geography="tract",geoid="GEOID",var="per.creditcarddebt",type="flat")
