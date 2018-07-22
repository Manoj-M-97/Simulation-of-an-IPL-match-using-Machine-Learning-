#Clustering using k-means.
    # nstart depicts the number of different random starting assignments and then select the one with the lowest within cluster variation.
    # The second parameter represents the desired number of clusters.As there are 10 digits,10 clusters are chosen.
library(readr)
data<-read.csv("Bowler.csv")
data
data_cluster <- kmeans(data[,2:11],10, nstart = 200)
cluster_points<-data_cluster$cluster
if(0)
{
a<-c(1:10)

for(i in a)
{

    cluster<-data[cluster_points==i,]
    cluster_no<-rep(i+10,nrow(cluster))
    cluster<-cbind(cluster,cluster_no)	
    print(cluster_no)
    print(cluster)
    write_csv(cluster,"BowlingClusters.csv", append = TRUE, col_names = TRUE)

	

}
}
#Clustering of Batsman

data<-read.csv("Batsman.csv")
data_cluster <- kmeans(data[,2:13],10, nstart = 200)
cluster_points<-data_cluster$cluster
a<-c(1:10)

for(i in a)
{
    cluster<-data[cluster_points==i,]
    cluster_no<-rep(i,nrow(cluster))
    cluster<-cbind(cluster,cluster_no)	
    print(cluster_no)
    print(cluster)
    write_csv(cluster,"BattingClusters.csv", append = TRUE, col_names = TRUE)

}

