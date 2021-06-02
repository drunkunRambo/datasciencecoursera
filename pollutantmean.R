pollutantmean<- function(directory, pollutant, id=1:332){
  
  file_name<-list.files(directory,full.names = TRUE)
  dat_a<-data.frame()
  for(i in id){
    dat_a<-rbind(dat_a,read.csv(file_name[i]))
    
  }
  
  pollutan<-dat_a[,pollutant]
  mean_poll<-mean(pollutan,na.rm = TRUE)
  mean_poll
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
