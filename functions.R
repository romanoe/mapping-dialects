#Find the 2 dominant populations for every individual
pop_dominants<-function(df){
  for(i in 1:nrow(df)){
    dom1<-order(-df[i,8:12])[1]
    dom2<-order(-df[i,8:12])[2]
    df$dominant[i]<-paste0('p',dom1,'_p',dom2)
    #df$colors[i]<-df[paste0('p',dom1,'_p'dom2)]
    if(dom1<dom2){
      df$d1[i]<-unlist(df[i,8:12][dom1]/sum(df[i,8:12][c(dom1,dom2)]))
      df$d2[i]<-unlist(df[i,8:12][dom2]/sum(df[i,8:12][c(dom1,dom2)]))
    } else {
      df$d1[i]<-unlist(df[i,8:12][dom2]/sum(df[i,8:12][c(dom1,dom2)]))
      df$d2[i]<-unlist(df[i,8:12][dom1]/sum(df[i,8:12][c(dom1,dom2)]))
    }
  }
  return(df)
}

names_dom<-function(df){
  #Change dominant names 
  df$color<-NA
  df[df$dominant == 'p2_p1',]$dominant<-'p1_p2'
  df[df$dominant == 'p1_p2',]$color<-1
  # df[df$dominant == 'p3_p1',]$dominant<-'p1_p3'
  # df[df$dominant == 'p1_p3',]$color<-2
  df[df$dominant == 'p4_p1',]$dominant<-'p1_p4'
  df[df$dominant == 'p1_p4',]$color<-3
  df[df$dominant == 'p5_p1',]$dominant<-'p1_p5'
  df[df$dominant == 'p1_p5',]$color<-4
  df[df$dominant == 'p3_p2',]$dominant<-'p2_p3'
  df[df$dominant == 'p2_p3',]$color<-5
  df[df$dominant == 'p4_p2',]$dominant<-'p2_p4'
  df[df$dominant == 'p2_p4',]$color<-6
  df[df$dominant == 'p5_p2',]$dominant<-'p2_p5'
  df[df$dominant == 'p2_p5',]$color<-7
  df[df$dominant == 'p4_p3',]$dominant<-'p3_p4'
  df[df$dominant == 'p3_p4',]$color<-8
  df[df$dominant == 'p5_p3',]$dominant<-'p3_p5'
  df[df$dominant == 'p3_p5',]$color<-9
  df[df$dominant == 'p5_p4',]$dominant<-'p4_p5'
  df[df$dominant == 'p4_p5',]$color<-10
  return(df)
}


#Assign categories
assign_cat<-function(df){
  #Category of color
  for(i in 1:nrow(df)){
    if(df$d1[i]>0.9){
      df$cat[i]<-paste0(df$dominant[i],'_1')
      df$col_pos[i]<-1
    } else if (df$d1[i]<=0.9 && df$d1[i]>0.8){
      df$cat[i]<-paste0(df$dominant[i],'_2')
      df$col_pos[i]<-2
    } else if (df$d1[i]<=0.8 && df$d1[i]>0.7){
      df$cat[i]<-paste0(df$dominant[i],'_3')
      df$col_pos[i]<-3
    } else if (df$d1[i]<=0.7 && df$d1[i]>0.6){
      df$cat[i]<-paste0(df$dominant[i],'_4')
      df$col_pos[i]<-4
    } else if (df$d1[i]<=0.6 && df$d1[i]>0.5){
      df$cat[i]<-paste0(df$dominant[i],'_5')
      df$col_pos[i]<-5
    } else if (df$d1[i]==0.5){
      df$cat[i]<-paste0(df$dominant[i],'_6')
      df$col_pos[i]<-6
    } else if (df$d1[i]<0.5 && df$d1[i]>0.4){
      df$cat[i]<-paste0(df$dominant[i],'_7')
      df$col_pos[i]<-7
    } else if (df$d1[i]<=0.4 && df$d1[i]>0.3){
      df$cat[i]<-paste0(df$dominant[i],'_8')
      df$col_pos[i]<-8
    } else if (df$d1[i]<=0.3 && df$d1[i]>0.2){
      df$cat[i]<-paste0(df$dominant[i],'_9')
      df$col_pos[i]<-9
    } else if (df$d1[i]<=0.2 && df$d1[i]>0.1){
      df$cat[i]<-paste0(df$dominant[i],'_10')
      df$col_pos[i]<-10
    } else if (df$d1[i]<=0.1){
      df$cat[i]<-paste0(df$dominant[i],'_11')
      df$col_pos[i]<-11
    }
  }
  return(df)
  }

collectDIC<-function(directory){
  files_dir<-list.files(directory)
  for (i in 1:15) {
    for(j in 1:length(files_dir)){
      dir.create(paste0(directory,paste0('Run ',i)))
      dirs<-paste0(directory,files_dir[j],'/',i,'/')
      dir.run<-paste0(directory,paste0('Run ',i))
      print(dirs)
      setwd(dirs)
      files<-list.files()
      sel <- grep("\\w+DIC.txt",files)
      file.copy(from=paste0(dirs,"/",files[sel]),to=paste0(directory,"/Run ",i)) 
      file.rename(from=paste0(dir.run,"/",files[sel]),to=paste0(dir.run,'/',files_dir[j],'-',i,'.txt'))
    }
  }
}

collectAdmixture<-function(directory){
  files_dir<-list.files(directory)
  dir.create(paste0(directory,'Admixture proportions'))
  for (i in 1:15) {
    for(j in 1:length(grep("^k",files_dir))){
      dirs<-paste0(directory,files_dir[j],'/',i,'/')
      dir.run<-paste0(directory,paste0('Run ',i))
      dir.adm<-paste0(directory,'Admixture proportions')
      print(dirs)
      setwd(dirs)
      files<-list.files()
      sel <- grep("\\w+TR.txt",files)
      file.copy(from=paste0(dirs,"/",files[sel]),to=paste0(directory,'Admixture proportions')) 
      file.rename(from=paste0(dir.adm,"/",files[sel]),to=paste0(dir.adm,'/',files_dir[j],'-adm-',i,'.txt'))
    }
  }
}

#Create df of DIC Run
dfDIC<-function(directory,model){
  DIC.list<-list()
  for(i in 1:15){
    txt_files <- list.files(path=paste0(directory,'Run ',i), pattern="*.txt");
    DIC <- lapply(paste0(directory,'Run ',i,'/',txt_files), function(x) {read.table(file = x)})
    DIC.df <- do.call("rbind", lapply(DIC, as.data.frame)) 
    DIC.df$K<-2:8
    DIC.df$Run<-i
    DIC.list[[i]]<-DIC.df
  }
  
  DIC.df_all <- do.call("rbind", lapply(DIC.list, as.data.frame)) 
  DIC.df_all$Model<-model
  return(DIC.df_all)
}
