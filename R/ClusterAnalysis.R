clust_ana <- function(ans,k,distm="euclidean",hclustm="ward"){
dist_t<-vegdist(ans,method=distm)
clust<-hclust(dist_t,method=hclustm)
plot(clust, main="Cluster Dendrogram of Segment Survey")
return(cutree(clust,k=k))
}


# data(varechem)
# clust_ana(varechem,k=3)
