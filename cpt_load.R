#load cpts
library(stringr)
load_cpt<-function(year=2026){
  setwd("../Downloads")
  or<-dir(".")
  curl::curl_download(paste0("https://hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/CCS-Services_Procedures_v",year,"-1.zip"),"temp.zip")
  unzip("temp.zip")
  ex<-dir(".")
  ex2<-dir(paste0("CCS-Services_Procedures_v",year,"-1"))
  file<-read.csv(paste0("CCS-Services_Procedures_v",year,"-1/",ex2[str_detect(ex2,"CCS_services_procedures")&str_detect(ex2,".csv")]),header=T,skip=1)
  file$start<-substr(file$Code.Range.,2,6)
  file$stop<-substr(file$Code.Range.,8,12)
  file$exp<-ifelse(file$start!=file$stop,1,0)
  expand<-file[file$exp==1,]
  expand$range<-ifelse(!is.na(as.numeric(expand$start)),as.numeric(expand$stop)-as.numeric(expand$start),"character")
  outs<-NULL
  for(i in expand[expand$range!="character",]$start){
    out<-expand[expand$start==i,]
    for(j in 1:out$range){
      out2<-out
      out2$start<-as.numeric(out2$start)+j
      outs<-rbind(outs,out2)
    }
  }
  file<-file[!file$Code.Range. %in% unique(outs$Code.Range.),]
  file$range<-0
  file<-rbind(file,outs)
  file.remove(ex[!ex %in% or])
  file.remove(paste0(paste0("CCS-Services_Procedures_v",year,"-1/"),ex2))
  unlink(paste0("CCS-Services_Procedures_v",year,"-1"),recursive=T)
  gc()
  file
}

ex<-load_cpt(2026)
write.table(ex,"C:/users/chris/OneDrive/Desktop/cpt list 10-19-26.csv",sep=",",col.names=T,row.names=F)