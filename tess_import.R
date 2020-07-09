#Set working directory
setwd('C:/Users/noemi/Documents/GitHub/master-thesis/R project')

#Install packages
install.packages(c("Cairo","devtools","ggplot2","gridExtra","gtable","tidyr"),dependencies=T)
#devtools::install_github('royfrancis/pophelper',force=T)
library(pophelper)
library(dplyr)
#Load functions
source('functions.R')


#Directory Tess results. Change if does not correspond!
dirNoSpatial<-'C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/nospatial/'
dirTrend<-'C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/trend/'
dirFullTrend<-'C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/fulltrend/'


# Collect DIC, TESS runs, plot DIC ----------------------------------------
#Organize folders PATH example trend->kj->i
collectDIC(dirNoSpatial)
collectDIC(dirFullTrend)
collectDIC(dirTrend)

collectAdmixture(dirNoSpatial)
collectAdmixture(dirFullTrend)
collectAdmixture(dirTrend)

#Create DIC dataframe in order to plot together. dfDIC function in functions.R
DIC.trend<-dfDIC(dirTrend,model='Trend')
DIC.nospatial<-dfDIC(dirNoSpatial,model='No-spatial')
DIC.fulltrend<-dfDIC(dirFullTrend,model='Full trend')
DIC_all<-rbind(DIC.trend,DIC.nospatial,DIC.fulltrend)
DIC_all.list<-split(DIC_all,DIC_all[,c('Model')])

DIC_all.list<-lapply(DIC_all.list,function(x){ x %>%
  group_by(K) %>%
  top_n(n = -1, wt = V1)})

#Aggregation
dic.mean<-lapply(DIC_all.list,function(x){aggregate(x$V1, by=list(x$K), FUN=mean)})
dic.sd<-lapply(DIC_all.list,function(x){aggregate(x$V1, by=list(x$K), FUN=sd)})


dic<-list()
for(i in 1:3){
  dic[[i]]<-merge(dic.mean[[i]],dic.sd[[i]],by=c('Group.1'))
  colnames(dic[[i]])<-c('K','DIC','sd')
}

names(dic)<-c('Full trend','Non spatial','Trend')

for(i in 1:3){
  dic[[i]]$Model<-names(dic)[i]
}

#Final df_dic
df_dic<-do.call(rbind,dic)

#Plot DIC
library(ggplot2)
ggplot(df_dic, aes(x=K, y=DIC)) +
  geom_line(aes(group=Model, color=Model)) +
  geom_point(aes(group=Model, color=Model))+
  #geom_errorbar(aes(ymin=DIC-sd,ymax=DIC+sd,color=Model),position = position_dodge(0.05),width=0.2)+
  ggtitle("Deviance Information Criterion (DIC)")+
  labs(x='K',y='DIC',subtitle = "Average DIC of 5 best runs")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))+
  labs(color='TESS models')


# Keep TESS runs with the 5 lowest DIC ----------------------------------- 
DIC_all.list<-lapply(DIC_all.list,function(x){ x %>%
    group_by(K) %>%
    top_n(n = -5, wt = V1)})



#Export 5 lowest DIC runs with CLUMPP, Trend model
trend<-DIC_all.list$Trend
trend<-trend[trend$K==5,]
keep<-paste0('k5-adm-3','.txt')
qlist<-readQ(paste0(dirTrend,'Admixture proportions/',keep))
data<-cbind(coord,qlist$`k5-adm-3`)
write.table(data,'k5_3.txt',row.names = F)
setwd('C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/trend/')
clumppExport(qlist=qlist, useexe=T)


#Export 5 lowest DIC runs with CLUMPP, non-spatial Model
nospatial<-DIC_all.list$`No-spatial`
nospatial<-nospatial[nospatial$K==6,]
keep<-paste0('k6-adm-',nospatial$Run,'.txt')
qlist<-readQ(paste0(dirNoSpatial,'Admixture proportions/',keep))
setwd('C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/nospatial/')
clumppExport(qlist=qlist, useexe=T)


#Export 5 lowest DIC runs with CLUMPP, full-trend model
full_trend<-DIC_all.list$`Full trend`
full_trend<-full_trend[full_trend$K==5,]
keep<-paste0('k5-adm-',full_trend$Run,'.txt')
qlist<-readQ(paste0(dirFullTrend,'Admixture proportions/',keep))
setwd('C:/Users/noemi/Desktop/EPFL/Master thesis/TESS/06-08-2018/fulltrend/')
clumppExport(qlist=qlist, useexe=T)




