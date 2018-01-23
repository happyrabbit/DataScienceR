# Trend detection using permutation

yr<-c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
coe<-rep(NA,nrow(cornunit))
for (i in 1:nrow(cornunit)){
  us<-t(cornunit[i,])
  coe[i]<-coef(lm(us~yr))[2]
}


cornunit<-cbind(cornunit,coe)


set.seed(2019)
com<-combn(1:10, 6,simplify = FALSE)
sizen<-200
idx<-sample(1:length(com),sizen,replace=FALSE)
subcom<-com[idx]


############################################
timestamp()
coe_sim<-NULL
for (k in 1:length(subcom)){
  coe0<-NULL
  idx<-subcom[[k]]  
  for (i in 1:nrow(cornunit)){
    us<-t(cornunit[i,1:10])
    coe0<-c(coe0,coef(lm(us[idx]~yr[idx]))[2])
  }
  coe_sim<-cbind(coe_sim,coe0)
}
timestamp()

#coe_sim<-NULL
#b<-20
coe_sim<-data.frame(coe_sim)
