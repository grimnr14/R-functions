#rm(list = ls())
library(stringr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(data.table)
library(tidyverse)
library(here)
library(usmap)
library(ggplot2)
library(psych)
library(dplyr)
library(conflicted)
library(comorbidity)
library(table1)
library(liver)
library(fastICA)
#library(sf)
#library(maptools)


makeIndex<-function(x,#x is a data.frame without NA values for features used in the factor analysis
                    geoid="geoid",#any coded value for geography level, which must be specified by variable name
                    state=NULL,#subsets values according to a string for lower case state name
                    cols=NULL,#optional vector of variable names to include in factor analysis, else the full data frame is used
                    nfactors=4,#number of factors solved for in setting the variable groupings
                    exclude=NULL,#a vector of variable names to exclude from final pca scoring of index
                    set.factor=NULL#a list of variable names to replace $factor_members, allowing a manual group assigment for scoring
                    ){
  #Step 1: locate relevant features and load into a data.frame x that includes geography NAME and geoid----
  if(!is.null(state)){#if this is a state specific analysis, you may declare a value for state (full state name)
    x<-x[stringr::str_detect(tolower(x[,"NAME"]),state)==T,]
  }
  if(!is.null(cols)){
    x<-x[,c("NAME",geoid,cols)]
  }
  #Step 2: standardize inputs----
  x<-na.omit(x)#if missing ANYTHING, remove observation
  labs<-as.data.frame(cbind(x[,c("NAME",geoid)],rowid=rownames(x)))
  labs<-labs[order(labs$rowid),]
  x <- x %>% dplyr::mutate_at(vars(-c("NAME",geoid)), as.numeric)#ensure everything is numeric
  
  x<-x[,!names(x) %in% c(geoid,"NAME")]
  x<- liver::transform(x, method = "zscore") #x is now normalized
  
  
  #Step 3: Explore and prune irrelevant, redundant features----
  corr_matrix <- cor(x)
  #p1<-corrplot::corrplot(corr_matrix, type = "upper")#get a bivariate corrplot for features
  
  res.pca <- FactoMineR::PCA(x,ncp=ncol(x),graph=FALSE)#conduct a pca
  # Eigenvalue
  eig.val <- data.frame(factoextra::get_eigenvalue(res.pca))
  t1<-round(eig.val,2)#store eigenvalues
  
  #Eigenvector
  eigenvectors<- res.pca$ind$coord
  t2<-eigenvectors#store eigenvectors for solution
  
  # Scree Plot, optional, using eigenvalue-1 criteria
  p2<-ggplot(data=eig.val,aes(y=eigenvalue,x=1:nrow(eig.val)))+
    geom_line(stat="identity")+
    geom_point(stat="identity")+
    geom_hline(yintercept=1,lty=2,col="darkred")+
    theme_minimal()+
    xlab("Dimensions")+
    ylab("Eigenvalue")

  # Plot the eigenvalues/variances against the number of dimensions
  var <- factoextra::get_pca_var(res.pca)
  
  # PCA plotting (Correlation circle)
  # - Positively correlated variables are grouped together.
  # - Negatively correlated variables are positioned on opposite sides
  # - Variables that are away from the origin are well represented on the factor map
  # - Color represents the contribution of variables
  # var$coord # correlation
  
  # var$cos2 # quality of representation
  #p3<-corrplot::corrplot(var$cos2, is.corr=FALSE)#stored correlation of eigenvectors
  
  # compute varimax-rotated principle components, the number of components can be specified.
  
  res.pca.rotated <- psych::principal(x, rotate="varimax",nfactors=nfactors,scores=T)
  #print(res.pca.rotated)#this is part of the output
  
  # Perform exploratory factor analysis
  # check each variable's loading against each factor
  fa_result <- psych::fa(x,nfactors = nfactors, rotate="varimax")
  #print(fa_result)#this is also part of the output
  
  #loadings<-fa_result$loadings#use loadings to create a fa diagram, except this does not store well and needs to be outputted
  
  #Check single solution
  fa_result2 <- psych::fa(x,nfactors = 1, rotate="varimax")
  #print(fa_result2)
  
  # Finalize the list of variables, remove the irrelevant indicators

  # Map Subcategories from fa loadings:
  ex<-fa_result$loadings
  outs<-NULL
  for(i in 1:nrow(ex)){
    outs<-c(outs,max(abs(ex[i,])))
  }
  maxs<-outs
  outs<-list()
  for(i in 1:nfactors){
    ex<-rownames(fa_result$loadings)[round(abs(fa_result$loadings[,i]),3)>0.3&
                                       !rownames(fa_result$loadings) %in% unlist(outs)&
                                       abs(fa_result$loadings[,i])==maxs]#we need some heuristic to decide loadings
    outs<-append(outs,list(ex))
  }
  factor_membership<-outs#store this for output
  # visualize the factor model
  #fa.diagram(fa_result$loadings)
  
  #Step 4: Generate the composite score using PCA----
  score <- x[,!names(x) %in% exclude]
  pca_result <- psych::principal(score)
  final_x <- as.data.frame(unclass(pca_result$loadings))
  score$full_score <- as.numeric(pca_result$scores)
  
  #four subcategories scoring
  if(!is.null(set.factor)){
    factor_membership<-set.factor
  }
  for(i in 1:nfactors){
    ex<-subset(x,select=factor_membership[[i]])
    fit<-psych::principal(ex)
    score$ex<-as.numeric(fit$scores)
    names(score)<-c(names(score)[1:(ncol(score)-1)],paste0("factor_",i))
  }
  
  #Step 5: Rescale scores by multiplyin *20, adding 100
  # zip level
  score$full_score <- round(((20*(score$full_score-mean(score$full_score))/sd(score$full_score)) + 100),2)
  nams<-names(score)[(ncol(score)-nfactors+1):ncol(score)]
  for(i in nams){
    score[,i]<-round(((20*(score[,i]-mean(score[,i]))/sd(score[,i])) + 100),2)
  }
  #merge back to geoid labels
  score$rowid<-rownames(score)
  score<-score[order(score$rowid),]
  score<-merge(labs,score,by="rowid",all.x=T)
  
  #Step 6: validate using external data----
  #NOT PERFORMED
  #Step 7: Perform Rankings of overall score at national (or state)
  score$ranked_score<-rank((score$full_score)/nrow(score),ties.method='min')
  #score$ranked_percentile<-100*((score$full_score-min(score$full_score)))/(max(score$full_score)-min(score$full_score))
  score$ranked_percentile<-100*((score$ranked_score-min(score$ranked_score)))/(max(score$ranked_score)-min(score$ranked_score))
  score$ranked_decile<-ifelse(score$ranked_percentile<10,1,
                              ifelse(score$ranked_percentile<20,2,
                                     ifelse(score$ranked_percentile<30,3,
                                            ifelse(score$ranked_percentile<40,4,
                                                   ifelse(score$ranked_percentile<50,5,
                                                          ifelse(score$ranked_percentile<60,6,
                                                                 ifelse(score$ranked_percentile<70,7,
                                                                        ifelse(score$ranked_percentile<80,8,
                                                                               ifelse(score$ranked_percentile<90,9,
                                                                                      ifelse(score$ranked_percentile>=90,10,NA
                                                                                      ))))))))))
  
  #END----
  output<-list(score=score[,c("NAME",geoid,"full_score","ranked_score","ranked_percentile","ranked_decile")],
               sub_score=score[,c("NAME",geoid,names(score)[(ncol(score)-(2+nfactors)):(ncol(score)-3)])],
               norm.val=x,
               corr.mat=corr_matrix,
               eigen.val=t1,
               eigen.vec=t2,
               scree=p2,
               eigen.cor=var$cos2,
               fa_result=fa_result,
               fa_single=fa_result2,
               factor_members=factor_membership,
               pca_result=pca_result
  )
  output
}

test<-F
if(test==T){
  d1<-readRDS(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/Census extract full final 2023-11-10.rds"))
  d1$geoid<-ifelse(d1$geo=="zcta5",d1$`zip code tabulation area`,
                   stringr::str_replace_all(
                     paste0(d1$state,d1$county,d1$tract,d1$`block group`),
                     "NA",""
                   ))
  vars<-readxl::read_xlsx(paste0("C:/Users/chris/OneDrive/Desktop/GeoHealth/GeoHealth Variables v2.xlsx"),sheet=8)
  vars<-vars[vars$group=="housing",]$Variable[1:18]
  d1<-d1[d1$geo=="zcta5"&d1$year=="2020",c("NAME","geoid",vars)]#the input object should consist of just the geo name/fips plus relevant features
  
  obj<-makeIndex(x=d1,geoid="geoid",state="maryland",nfactors=5)
  head(obj$score)
  obj$scree
  obj$factor_members
  
}
