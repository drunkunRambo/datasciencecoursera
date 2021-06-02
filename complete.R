complete<- function(directory, id = 1:332){
  file_nam<-list.files("specdata",full.names = TRUE)
  nw_data<-data.frame()
  daa<-data.frame(id="",noob="")
  for(j in id){
    dota<- read.csv(file_nam[j])
    
    total<-0
    for(i in 1:dim(dota)[1]){
      
      if(!is.na(dota[i,"sulfate"] && dota[i,"nitrate"])){
        total<-total+1
      }
    }
    
    daa<-rbind(daa,c(j,total))
  }
  daa
}

complete("specdata", 3)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$noob)
cc <- complete("specdata", 54)
print(cc$noob)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "noob"])















