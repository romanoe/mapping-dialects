library(foreign)
library(dplyr)
library(sp)
#  ------------------------------------------------------------------------
setwd('C:/Users/noemi/Documents/GitHub/master-thesis/R project')
#places (as SPDF)
raw_data<-load("orte.Rdata")
#Orte@data
#Orte@coords

#raw dialect data
test<-read.dbf("Gesamttabelle_68phen_alleSprecher.dbf")
test<-test[,-1]
coordinates_cantons<-read.csv('coordinates_cantons.csv')
                                     #test<-test %>% group_by(BFS_NR) %>% summarise_all(sum)


#filter out those places that "do not exist"
test<-test[test$BFS_NR%in%Orte$id,]
test.agg.s<-test[,-1]


#get pattern from attribute name
patterns<-strsplit(names(test.agg.s), "_")
patterns<-sapply(patterns,function(x){x[1]})
patterns.s <- unique(patterns)
patterns<-paste("^",patterns.s,"_",sep="")

#add each feature to a list
allAbs<-list()
for(i in 1:length(patterns)){
  allAbs[[length(allAbs)+1]]<-test.agg.s[,grep(patterns[i], names(test.agg.s))]
}


names(allAbs)<-patterns.s

#select only the first element
#data_1variable<-as.data.frame(sapply(allAbs, '[[', 1))
test<-data.frame(test[,1],data_1variable)
colnames(test)[colnames(test)=='test...1.']<-'BFS_NR'

#get coordinates and id
data<-Orte@data
coordinate<-as.data.frame(Orte@coords)
coordinates_wgs84<-read.table('coordinates_wgs84.txt',header = T)
data_coord<-data.frame(data,coordinate)

library(sqldf)
data2<-sqldf('select a.*,b.* from data_coord b,test a where a.BFS_NR=b.id')
data2$id<-NULL
data2[,c('x','y')]<-coordinates_wgs84
#plot(new_df$x,new_df$y)
#drops<-c('IV3ES','IV3IHM','IV25','III28')
#data2<-data2[ , !(names(data2) %in% drops)]


data_ready<-data2[c(1,211:212,2:210)]
data_ready[,c('x','y')]<-coordinates_wgs84
data_ready<-data.frame(data_ready,coordinates_cantons)
data_ready<-data_ready[c(1,216,2:3,4:215)]
#data_ready[,5:215]<-lapply(data_ready[,5:215],factor)

data_ready$NAME<-gsub(" ", "", data_ready$NAME, fixed = TRUE)

data_ready[,c('x.1','y.1','id')]<-NULL
new_df<-data_ready[,-c(1:3)]
#Drop columns

write.table(coordinates,'coordinates.txt',row.names = F)
write.table(new_df, "mydata.txt", row.names = F) 
write.table(df_reduced,'mydata_reduced.txt',row.names = F)
write.table(data_ready,'mydata.txt',row.names = F)


#linkage disequilibrium, Cramer's V coefficient
library(vcd)
empty_m <- matrix(ncol = length(new_df),
                  nrow = length(new_df),
                  dimnames = list(names(new_df), 
                                  names(new_df)))
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}
cor_matrix <- calculate_cramer(empty_m ,new_df)
  
library(corrplot)
library(caret)

hc <- findCorrelation(cor_matrix, cutoff=0.1) # putt any value as a "cutoff" 
hc <- sort(hc)
corrplot(cor_matrix,method='square')

reduced_Data <- cor_matrix[-c(hc),-c(hc)]
corrplot(reduced_Data,method='square')


df_reduced<-data_ready[,colnames(reduced_Data)]
df_reduced<-cbind(data_ready[,1:4],df_reduced)

#df_reduced[,5:29]<-lapply(df_reduced[,5:29],as.numeric)

write.table(df_reduced,'mydata.txt',row.names = F)


