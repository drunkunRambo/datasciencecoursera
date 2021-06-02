corr<- function(directory, threshold = 0){
  max_data<<-data.frame()
  fin<<-c(0)
  file_nam<-list.files("specdata",full.names = TRUE)
  nw_data<-data.frame()
  daa<-data.frame(id=c(0),noob=c(0))
  for(j in 1:332){
    dota<- read.csv(file_nam[j])
    
    total<-0
    for(i in 1:dim(dota)[1]){
      
      if(!is.na(dota[i,"sulfate"] && dota[i,"nitrate"])){
        total<-total+1
      }
    }
    
    daa<-rbind(daa,c(j,total))
  }
  max_data<<-daa
  max_data$noob>threshold
  sel_data<-max_data[which(max_data$noob>threshold),]
  sel_data
  
  
  for(k in 1:dim(sel_data)[1]){
    if(dim(sel_data)[1]==0){
      break
    }
    boat<-read.csv(file_nam[sel_data$id[k]])
    neat<-boat[which(!is.na(boat$nitrate) & !is.na(boat$sulfate)),]
    nit<-neat$nitrate
    nit
    sul<-neat$sulfate
    fin[k]<<-cor(nit,sul)
    
  }
  fin
}

cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
cr
summary(cr)
cr <- corr("specdata")
summary(cr)
cr <- corr("specdata", 150)
head(cr)

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)



cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
































