etwd("~/desktop")
dataset = read.csv("~/desktop/prj2/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

dt1 = na.omit(dataset,cols = ZIPCODE)
dt1  = dt1[dt1$INSPECTION.DATE != 01/01/1900,]
dt1 = dt1[dt1$GRADE %in% c("A","B","C"),]
df_result<-matrix(NA,nrow=2000,ncol=6)
colnames(df_result) = c("ZIPCODE","YEAR","A","B","C","AVERAGE_SCORE")
var1 = as.character(unlist(lapply(unique(dt1$ZIPCODE),function(x) rep(x,5))))
df_result = cbind(var1,rep(c(2015,2016,2017,2018,2019),length(var1)/5))
grades = data.frame(matrix(nrow = length(var1),ncol = 3))
for(i in 1:length(var1)){
  grades[i,1] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,10)==df_result[i,2])&(dt1$GRADE =="A"),])
  grades[i,2] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,10)==df_result[i,2])&(dt1$GRADE =="B"),])
  grades[i,3] = nrow(dt1[(dt1$ZIPCODE == df_result[i,1])&(substring(dt1$INSPECTION.DATE,7,10)==df_result[i,2])&(dt1$GRADE =="C"),])
}
score = apply(df_result, 1 ,function(x) mean(dt1[(dt1$ZIPCODE == x[1])&(substring(dt1$INSPECTION.DATE,7,10)==x[2]),]$SCORE))
df_result = cbind(df_result,grades,score)
colnames(df_result) = c("zipcode","year","A","B","C","average_score")

df_result_omit<-na.omit(df_result)

