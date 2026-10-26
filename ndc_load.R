library(jsonlite)
library(stringr)
library(openFDA)

key<-"c2VpGMqkO9YjwxHLbLMUKzVhyBnQ9MTmoQibyj0g"
set_api_key(key)

collapse<-function(x,sep=","){
  out<-NULL
  for(i in x){
    if(is.null(out)){
      out<-paste0(out,i)
    }else{
      out<-paste0(out,sep,i)
    }
  }
  out
}

#read ndcs from openFDA api----
load_ndc<-function(key=key){
  results<-NULL
  for(i in 1000*(740:1000)){
    if(i<26000){
      ex<-openFDA(endpoint = "drug-ndc",skip=i,api_key=key)
      ex<-read_json(path=ex$url)
    }else{
      g<-round(i/25000,1)
      ex<-openFDA(endpoint = "drug-ndc",api_key=key)
      ex<-read_json(path=paste0(ex$url,"&search_after=skip:",g*25000))
    }
    meta<-data.frame(last_updated=ex$meta$last_updated)[[1]]
    for(j in ex$results){
      result<-data.frame(skip=i,
                         product_ndc=j$product_ndc,
                         marketing_end_date=ifelse(is.null(j$marketing_end_date),NA,as.Date(as.numeric(j$marketing_end_date),origin="1970-01-01")),
                         generic_name=ifelse(is.null(j$generic_name),NA,j$generic_name),
                         labeler_name=ifelse(is.null(j$labeler_name),NA,j$labeler_name),
                         brand_name=ifelse(is.null(j$brand_name),NA,j$brand_name),
                         active_ingredients=ifelse(is.null(j$active_ingredients),NA,collapse(unlist(j$active_ingredients),sep="||")),
                         finished=ifelse(is.null(j$finished),NA,j$finished),
                         packaging=ifelse(is.null(j$packaging)|length(j$packaging)==0,NA,collapse(unlist(j$packaging),sep="||")),
                         openfda=ifelse(is.null(j$openfda)|length(j$openfda)==0,NA,collapse(unlist(j$openfda),sep="||")),
                         marketing_category=ifelse(is.null(j$marketing_category),NA,j$marketing_category),
                         dosage_form=ifelse(is.null(j$dosage_form),NA,j$dosage_form),
                         spl_id=ifelse(is.null(j$spl_id),NA,j$spl_id),
                         product_type=ifelse(is.null(j$product_type),NA,j$product_type),
                         route=ifelse(is.null(j$route),NA,collapse(unlist(j$route),sep="||")),
                         marketing_start_date=ifelse(is.null(j$marketing_start_date),NA,j$marketing_start_date),
                         product_id=ifelse(is.null(j$product_id),NA,j$product_id),
                         brand_name_base=ifelse(is.null(j$brand_name_base),NA,j$brand_name_base),
                         pharm_class=ifelse(is.null(j$pharm_class),NA,collapse(unlist(j$pharm_class),sep="||"))
      )
      result$package_ndc=substr(result$packaging,1,str_locate(result$packaging,"[|]")[,1]-1)
      result$labeler_ndc<-substr(result$package_ndc,1,str_locate(result$package_ndc,"-")[,1]-1)
      result$product_ndc<-substr(result$package_ndc,
                                  nchar(result$labeler_ndc)+2,
                                  nchar(result$labeler_ndc)+1+str_locate(str_remove(result$package_ndc,paste0(result$labeler_ndc,"-")),"-")[,1]-1)
      result$package_code<-substr(result$package_ndc,
                                   nchar(result$labeler_ndc)+nchar(result$product_ndc)+3,
                                   nchar(result$package_ndc))
      result$format<-ifelse(nchar(result$labeler_ndc)==5&nchar(result$product_ndc)==4&nchar(result$package_code)==1,"5-4-1",
                            ifelse(nchar(result$labeler_ndc)==5&nchar(result$product_ndc)==3&nchar(result$package_code)==2,"5-3-2",
                                   ifelse(nchar(result$labeler_ndc)==4&nchar(result$product_ndc)==4&nchar(result$package_code)==2,"4-4-2",
                                   NA)))
      result$ndc11<-ifelse(result$format=="5-4-1",paste0(result$labeler_ndc,result$product_ndc,str_pad(result$package_code,width=2,side="left",pad="0")),
                           ifelse(result$format=="5-3-2",paste0(result$labeler_ndc,str_pad(result$product_ndc,side="left",width=4,pad="0"),result$package_code),
                                  ifelse(result$format=="4-4-2",paste0(str_pad(result$labeler_ndc,side="left",width=5,pad="0"),result$product_ndc,result$package_code),
                                  NA)))
      result<-result[,c("skip","ndc11","package_ndc","generic_name","labeler_name","brand_name","brand_name_base","active_ingredients","pharm_class","route","dosage_form")]
      
      results<-plyr::rbind.fill(results,result)
    }
  }
  results
  
}

ex<-load_ndc(key=key)

saveRDS(results,"C:/users/chris/OneDrive/Desktop/ndc list 10-19-26.rds")
write.table(results,"C:/users/chris/OneDrive/Desktop/ndc list 10-19-26.csv",sep=",",col.names=T,row.names=F)