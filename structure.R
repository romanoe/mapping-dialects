install.packages(c("fields","RColorBrewer","mapplots"))
source("http://bioconductor.org/biocLite.R")
biocLite("LEA")

source("http://membres-timc.imag.fr/Olivier.Francois/Conversion.R")
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")

library(pophelper)

dirStructure<-'C:/Users/noemi/Desktop/EPFL/Master thesis/STRUCTURE/master_thesis/par_1/Results/'
setwd('C:/Users/noemi/Desktop/EPFL/Master thesis/STRUCTURE/master_thesis/par_1/Results/')
sfiles <- list.files()
slist <- readQ(files=sfiles)
head(tabulateQ(slist))
head(summariseQ(tabulateQ(slist)))
head(tabulateQ(slist))
sr1<-summariseQ(tabulateQ(slist))


# 
# sr1
p <- evannoMethodStructure(data=sr1,exportplot=F,returnplot=F)


ggplot(p,aes(x=k,y=elpdmean))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=elpdmean-elpdsd,ymax=elpdmean+elpdsd,width=0.2))+
  labs(x='K',y='MEAN L(K) \u00b1 SD')+
  ggtitle('Logarithm of evidence')+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
#   
ggplot(p,aes(x=k,y=deltaK))+
  geom_line()+
  geom_point()+
  labs(x='K',y=expression(paste(Delta,'K')))+
  ggtitle('Evanno Method')+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
#  
# 
library(gridExtra)
library(grid)
grid.arrange(p_1,p_2,ncol=2)

#Keep only k=6
keep<-paste0('par_1_run_',61:75,'_f')
slist<-readQ(paste0(dirStructure,keep))
clumppExport(qlist=slist, useexe=T)

