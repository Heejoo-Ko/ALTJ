library(data.table);library(magrittr);library(DT);library(jstable);library(dplyr);library(stats);library(readxl)

# setwd("/home/heejooko/ShinyApps/ALTJ")

rd<-as.data.table(read_excel("raw data v1.2.xlsx"))[,1:26,]
rd %>% setnames(c("No","hoispital","5-year\r\nlymphedema"),c("NO","hospital","5-year lymphedema"))

change_columns<-c("NO","CTx","hospital","RT","regionalRT","surgery")
rd[,(change_columns):=lapply(.SD, as.factor), .SDcols=change_columns]
rd[,start_date:=as.Date(start_date),]
rd[,end_date:=as.Date(end_date),]
# sapply(rd, class) %>%  unlist

rd[,regionalRT_yesno:=as.factor(ifelse(regionalRT==0 | regionalRT==1,"0","1")),]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cc<-as.data.frame(read_excel("anal data.xlsx",sheet = 1))
cd<-data.table(x=NA, y=NA, z=NA)
for(i in 1:13){
  for(j in 1:13){
    cd<-rbind(cd,data.table(x=cc[j,1],y=colnames(cc)[i+1],z=cc[j,i+1]))
  }
}
cd<-cd[-1,,]
cd<-cd[,z:=ifelse(z=="NA",NA,z),]
cd<-cd[,x:=factor(x,levels=c("ALTJDmax","ALTJDmean","ALTJDmin","ALTJV5","ALTJV10","ALTJV15","ALTJV20","ALTJV25","ALTJV30","ALTJV35","ALTJV40","ALTJV45","ALTJV50")),]
cd<-cd[,y:=factor(y,levels=c("ALTJDmax","ALTJDmean","ALTJDmin","ALTJV5","ALTJV10","ALTJV15","ALTJV20","ALTJV25","ALTJV30","ALTJV35","ALTJV40","ALTJV45","ALTJV50")),]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cc<-as.data.frame(read_excel("anal data.xlsx",sheet = 2))
co<-data.frame("Variable"=cc[,1],"Cutoffpoint"=cc[,2])
co$Cutoffpoint<-as.numeric(co$Cutoffpoint)
