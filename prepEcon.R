library(stringr)
library(tigris)
library(tidyr)
#library(ipumsr)

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
  
  #cagdp1 county gdp summary
  #cagdp11 contributions to percent change in real gdp
  #cagdp2 gdp by county
  #cagdp9 real gdp by county
  #cagdp8 chain type quantity indexes for real gdp by county
  #cainc1 personal income summary per capita personal income
  
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
  
  #SAPCE1 personal consumption expenditures by major type of product
  #SAPCE2 per capita pce by type
  #SAPCE3 pce by state by type
  #SARPI regional price index relative to us as a whole
  #SARPP 
  
#  qtax<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/qtax%20county%20level.csv"),header=T)
#  qtax<-qtax[,c("state","GEO_LEVEL_CODE","CATEGORY_CODE","CELL_VALUE","DATA_TYPE_CODE","TIME_SLOT_DATE")]
#  qtax$year<-substr(qtax$TIME_SLOT_DATE,1,4)
#  qtax<-qtax[qtax$DATA_TYPE_CODE=="T40",]
#  qtax$state<-str_pad(qtax$state,width=2,side="left",pad="0")
#  qtax<-aggregate(data=qtax,CELL_VALUE~state+GEO_LEVEL_CODE+year,FUN="sum")
#  summary(as.factor(qtax$GEO_LEVEL_CODE))
  
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
  stc$IncomeTax<-(as.numeric(stc$`Income Taxes`)/stc$statepop)*stc$B01001_001
  stc$LicenseTax<-(as.numeric(stc$`License Taxes`)/stc$statepop)*stc$B01001_001
  stc$OtherTax<-(as.numeric(stc$`Other Taxes`)/stc$statepop)*stc$B01001_001
  stc$PropertyTax<-(as.numeric(stc$`Property Taxes`)/stc$statepop)*stc$B01001_001
  stc$SalesReceiptsTax<-(as.numeric(stc$`Sales and Gross Receipts Taxes`)/stc$statepop)*stc$B01001_001
  stc$TotalTax<-(as.numeric(stc$`Total Taxes`)/stc$statepop)*stc$B01001_001
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
  remove(pop,base,geopop,map,county.cnts,prcs,runs)
  out<-out[,!names(out) %in% c(skip,"county","county.y","county.x","B01001_001.x","B01001_001.y")]
  out<-out[!duplicated(out),]
  out[!is.na(out$GEOID),]
}

#ex<-prepEconomic(year=2021,geography="zcta")
#ex<-prepEconomic(year=2019,geography="tract")
#flat_map(ex,year=2019,state="MD",geography="tract",geoid="GEOID",var="summary.gdp",type="flat")
#flat_map(ex,year=2019,state="MD",geography="tract",geoid="GEOID",var="per.creditcarddebt",type="flat")
