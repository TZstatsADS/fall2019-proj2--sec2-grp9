#df_result_omit;score_dist.RData
dt1 = read.csv("~/Downloads/DOHMH_New_York_City_Restaurant_Inspection_Results (1).csv")
dt1 = na.omit(dt1,cols = ZIPCODE)
dt1  = dt1[dt1$INSPECTION.DATE != 01/01/1900,]
dt1 = dt1[dt1$GRADE %in% c("A","B","C"),]

colnames(df_result) = c("ZIPCODE","YEAR","A","B","C","AVERAGE_SCORE")
var1 = as.character(unlist(lapply(unique(dt1$ZIPCODE),function(x) rep(x,5))))
df_result = cbind(var1,rep(c(2015,2016,2017,2018,2019),length(var1)/5))
grades = data.frame(matrix(nrow = length(var1),ncol = 3))
for(i in 1:length(var1)){
  grades[i,1] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,11)==df_result[i,2])&(dt1$GRADE =="A"),])
  grades[i,2] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,11)==df_result[i,2])&(dt1$GRADE =="B"),])
  grades[i,3] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,11)==df_result[i,2])&(dt1$GRADE =="C"),])
}
score = apply(df_result, 1 ,function(x) mean(dt1[(dt1$ZIPCODE == x[1])&(substring(dt1$INSPECTION.DATE,7,11)==x[2]),]$SCORE))
df_result = cbind(df_result,grades,score)
colnames(df_result) = c("zipcode","year","A","B","C","average_score")

df_result_omit=na.omit(df_result,cols = avaerage_score)

#df_result2;cuisine_score.RData
dt2 = read.csv("~/Downloads/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
dt2 = dt2[dt2$ZIPCODE != "",]
dt2  = dt2[dt2$INSPECTION.DATE != 01/01/1900,]
dt2 = dt2[dt2$CUISINE.DESCRIPTION %in% c('American','Chinese','Café/Coffee/Tea','Pizza','Italian','Mexican','Latin (Cuban, Dominican, Puerto Rican, South & Central American)','Japanese','Bakery','Caribbean'),]
library(lubridate)

toplist=c('American','Chinese','Café/Coffee/Tea','Pizza','Italian','Mexican','Latin (Cuban, Dominican, Puerto Rican, South & Central American)','Japanese','Bakery','Caribbean')
var2 = unlist(lapply(toplist,function(x) rep(x,5)))
df_result2 = as.data.frame(cbind(var2,rep(c(2015,2016,2017,2018,2019),length(var2)/5)))
score = apply(df_result2, 1 ,function(x) mean(dt2[(dt2$CUISINE.DESCRIPTION == x[1])&(year(mdy(dt2$INSPECTION.DATE))==x[2]),]$SCORE,na.rm = T))
df_result = cbind(df_result2,score)
colnames(df_result2) = c("CUISINE","YEAR","AVERAGE_SCORE")

#dt3;violation.RData
dt3 = read.csv("~/Downloads/DOHMH_New_York_City_Restaurant_Inspection_Results (1).csv")
dt3= dt3[,c('VIOLATION.CODE','VIOLATION.DESCRIPTION','INSPECTION.DATE','CUISINE.DESCRIPTION')]
library(dplyr)
top10violations <- top_n(data.frame(table(dt3$VIOLATION.CODE)),10,Freq)
library(ggplot2)
dt2015=dt3[(dt3$VIOLATION.CODE %in% top10violations$Var1)&(substring(dt3$INSPECTION.DATE,7,11)=='2015'),]
dt2016=dt3[(dt3$VIOLATION.CODE %in% top10violations$Var1)&(substring(dt3$INSPECTION.DATE,7,11)=='2016'),]
dt2017=dt3[(dt3$VIOLATION.CODE %in% top10violations$Var1)&(substring(dt3$INSPECTION.DATE,7,11)=='2017'),]
dt2018=dt3[(dt3$VIOLATION.CODE %in% top10violations$Var1)&(substring(dt3$INSPECTION.DATE,7,11)=='2018'),]
dt2019=dt3[(dt3$VIOLATION.CODE %in% top10violations$Var1)&(substring(dt3$INSPECTION.DATE,7,11)=='2019'),]
ggplot(dt2015, aes(x= VIOLATION.CODE)) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(dt2016, aes(x= VIOLATION.CODE)) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(dt2017, aes(x= VIOLATION.CODE)) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(dt2018, aes(x= VIOLATION.CODE)) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(dt2019, aes(x= VIOLATION.CODE)) + geom_bar(aes(y = (..count..)/sum(..count..)))

#top10violations;top10violations.RData
dt4=dt3[dt3$VIOLATION.CODE %in% top10violations$Var1,]
dt4=dt4[,c('VIOLATION.CODE','VIOLATION.DESCRIPTION')]
top10violations=dt4[!duplicated(dt4),]
