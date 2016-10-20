#' @export
########################outliers
out_mad<-function(x){
v2<-x-median(na.omit(x))
mad<-median(na.omit(abs(v2)))
idx<-which(0.6745*(x-median(na.omit(v2)))/mad>3.5)
return(idx)
}

# x<-c(seq(1:1000),20000)
# out_mad(x)
