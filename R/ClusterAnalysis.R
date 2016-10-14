read.csv("L:/MARKET RESEARCH/Segmentation/Hui-Segmentation/RawData/finaldata.csv")->finaldata
source("L:/MARKET RESEARCH/Segmentation/Hui-Segmentation/Rcode/multiplot.R")
#install.packages("vegan")
library(vegan)
library(ggplot2)
#########################################################
ans<-finaldata[,c(2:11)]
vegdist(ans,method="euclidean")->dist_t
#vegdist(ans,method="manhattan")->dist_t
hclust(dist_t,method="ward")->clust
plot(clust, main="Cluster Dendrogram of Segment Survey")
cutree(clust,k=5)->g_seg