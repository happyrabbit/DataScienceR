########################outliers
v2<-x-median(na.omit(x))
mad<-median(na.omit(abs(v2)))
length(idx<-which(0.6745*(x-median(na.omit(v2)))/mad>3.5))
x[idx]<-1
dat$AGRPct2011<-x
