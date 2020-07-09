beta.1<-read.table('./beta_trend/beta_trend_1.txt')
beta.2<-read.table('./beta_trend/beta_trend_2.txt')
beta.3<-read.table('./beta_trend/beta_trend_3.txt')
beta.4<-read.table('./beta_trend/beta_trend_4.txt')
beta.5<-read.table('./beta_trend/beta_trend_5.txt')
beta.fulltrend<-read.table('./beta_trend/beta_fulltrend.txt')


colnames(beta.fulltrend)<-c('int_3','long_3','lat_3','int_5','long_5','lat_5','int_4','long_4','lat_4','int_2','long_2','lat_2','int_1','long_1','lat_1')



# colnames(beta.2)<-c('int_4','long_4','lat_4','int_1','long_1','lat_1','int_5','long_5','lat_5','int_2','long_2','lat_2','int_3','long_3','lat_3')
# colnames(beta.2)<-c('int_4','long_4','lat_4','int_1','long_1','lat_1','int_5','long_5','lat_5','int_2','long_2','lat_2','int_3','long_3','lat_3')
# hist(beta.3$V3)

# beta.trend$K[1:3,]<-1
# beta.trend$K[4:6,]<-2
# beta.trend$K[7:9,]<-3
# beta.trend$K[10:12,]<-4
# beta.trend$K[13:15,]<-5

library("bayesplot")
library("rstanarm")
library("ggplot2")
library('coda')

median(s)
quantile(beta.fulltrend$long_1, c(0.05, 0.95))
quantile(beta.fulltrend$lat_1, c(0.05, 0.95))
quantile(beta.fulltrend$long_2, c(0.05, 0.95))
quantile(beta.fulltrend$lat_2, c(0.05, 0.95))
quantile(beta.fulltrend$long_3, c(0.05, 0.95))
quantile(beta.fulltrend$lat_3, c(0.05, 0.95))
quantile(beta.fulltrend$long_4, c(0.05, 0.95))
quantile(beta.fulltrend$lat_4, c(0.05, 0.95))
quantile(beta.fulltrend$long_5, c(0.05, 0.95))
quantile(beta.fulltrend$lat_5, c(0.05, 0.95))




[HPDinterval(mcmc(beta.2$lat_1), 0.95)
HPDinterval(mcmc(beta.2$long_1), 0.95)

HPDinterval(mcmc(beta.2$lat_2), 0.95)
HPDinterval(mcmc(beta.2$long_2), 0.95)
HPDinterval(mcmc(beta.2$lat_3), 0.95)
HPDinterval(mcmc(beta.2$long_3), 0.95)
HPDinterval(mcmc(beta.2$lat_4), 0.95)
HPDinterval(mcmc(beta.2$long_4), 0.95)
HPDinterval(mcmc(beta.2$lat_5), 0.95)
HPDinterval(mcmc(beta.2$long_5), 0.95)

HPDinterval(mcmc(beta.trend$long_2), 0.95)

quantile(beta.3$lat_1, c(0.025, 0.975))

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
myblue<-rev(c("#1F78B4",'#3B88BC','#5798C4','#73A8CC','#8FB8D4','#ACC9DD'))
mygreen<- rev(c("#33A02C","#49AB43","#60B65B","#77C172","#8ECC8A","#A5D7A2"))
myorange<-rev(c("#FF7F00","#FC8C1D","#FA993A","#F7A757","#F5B474","#F3C291"))
mypurple<-rev(c("#6A3D9A","#7C53A7","#8E6AB5","#A181C3","#B398D1","#C6AFDF"))

mycolors<-list(myblue,mygreen,'red',myorange,mypurple)
color_scheme_set(myorange)
#Only latitude and longitude
beta_lat<-beta.trend[,grep('lat',colnames(beta.trend))]
beta_long<-beta.trend[,grep('long',colnames(beta.trend))]


#Remove burnin period
beta_lat<-beta_lat[-c(1:10000),]
beta_long<-beta_long[-c(1:10000),]

k <- with(beta_long_lat,MASS:::kde2d(lat_1,long_1))
filled.contour(k)

ggplot(beta_long_lat,aes(x=lat_1,y=long_1))+geom_density2d()


ggplot(beta_long_lat,aes(x=long_5,y=lat_5))+
  stat_density2d(aes(alpha=..level..), geom="polygon",fill=mycolors[[5]][1]) +
  scale_alpha_continuous(limits=c(0,1))+
  geom_vline(xintercept = 0,color='red',alpha=1)+
  geom_hline(yintercept = 0,color='red',alpha=1)+
  theme_bw()+
  labs(x='Longitude',y='Latitude')

mycolors[[1]][1]

k<-seq(from=0,to=8,by=2)
j<-seq(from=1,to=9,by=2)
p<-list()
for(i in 1:5){
  #Set color scheme
  color_scheme_set(mycolors[[i]])
  
  #Plot
  p[[i]]<-mcmc_areas(beta_lat[,i],prob = 0.95)+
          xlim(-10,10)+
          geom_vline(xintercept = 0,color='grey',alpha=1/5)
  #Save
  ggsave(paste0('p',i,'_lat_CI.pdf'),plot=p[[i]])
}

lat<-list()

ggplot()+
  geom_density(data=beta_lat,aes(x=beta_lat[paste0('lat_',3)]),alpha=1/2,fill='#ff7b7b',colour=mycolors[[3]][1])+
  geom_density(data=beta_long,aes(x=beta_long[paste0('long_',3)]),alpha=1/2,fill='#ff0000',colour=mycolors[[3]][1],inherit.aes = F)+
  geom_vline(xintercept = 0,color='red')+
  xlim(-5,5)+
  ylim(0,3)
 

ggplot()+
  geom_density(data=beta_long,aes(x=beta_long[paste0('long_',1)]),alpha=1/2,fill=mycolors[[1]][1],colour=mycolors[[1]][1])+
  geom_vline(xintercept = 0,color='red',alpha=1/2)+
  xlim(-5,5)+
  ylim(0,3)    

   
print(head(beta_lat[paste0('lat_',i)]))
     


lat[[1]]
lat[[2]]
lat[[3]]
lat[[4]]




library(statsr)
credible_interval_app(beta_lat)

quantile(beta_lat[,1],seq(0,1,0.05))


hist(beta_lat[,1])
mcmc_areas(beta_lat[,1:2])

p[[1]]
p[[2]]
p[[3]]
p[[4]]
p[[5]]

