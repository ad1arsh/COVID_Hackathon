
library(tidyverse)
file<-read.csv("data.csv",header= TRUE, sep=",")
dim(file)
head(file,N=6L)
tail(file, N=-6L)
fin_res<-file[2340,"Residue.."]
fin_start<-file[1,"Residue.."]
res_vector<-fin_start:fin_res
print(res_vector)


tdiff1<-list()
tdiff2<-list()
mut1<-vector()
mut2<-vector()
original<-vector()


for (num in res_vector) {
  bool2<-file[file$Residue..==num, ]
  if (is.null(bool2)) {
    next
  }
  
  tdif1=abs(bool2$Top_rep1-bool2$Bottom_rep1)
  tdif2=abs(bool2$Top_rep2-bool2$Bottom_rep2)
  
  original<-c(original, bool2[tdif1==0, "Substitution"])
  
  tdiff1<-c(tdiff1,bool2$Residue..[1],abs(bool2$Top_rep1-bool2$Bottom_rep1))
  tdiff2<-c(tdiff2,bool2$Residue..[1],tdif2)
  
  
  
  mut1<-c(mut1,bool2$Substitution[which.max(tdif1)])
  mut2<-c(mut2,bool2$Substitution[which.max(tdif2)])
  
  print(str(num) + str('=') + str(mut1))

} 

