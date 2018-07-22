batting<-read.csv("BattingClusters.csv")
bowling<-read.csv("BowlingClusters.csv")
p2p<-read.csv("p2p_probabilities.csv")
#colnames(p2p)

final_probs<-data.frame()
for (i in c(1:10))
{
for(y in c(11:20))
{
	newdata_batting<-batting[which(batting[[14]]==i),]
	newdata_bowling<-bowling[which(bowling[[13]]==y),]
	l<-c()
	l<-append(l,as.character("Dummy"))	
	l<-append(l,as.character("Dummy"))
	for(z in c(1:8))
	{
		l<-append(l,0)
	}

	probs<-data.frame()
	for (j in c(1:nrow(newdata_batting)))
	{
		for (k in c(1:nrow(newdata_bowling)))
		{
			if((newdata_batting[j,1] %in% p2p[[1]]) && (newdata_bowling[k,1] %in% p2p[[2]]))
			{
				m<-p2p[which(p2p[[1]]==newdata_batting[j,1]),]

				m<-m[which(m[[2]]==newdata_bowling[k,1]),]
				probs<-rbind(probs,m)		

			}
			
		}

	}
	l<-c()
	l<-append(l,i)
	l<-append(l,(y))

	for(z in c(3:11))
	{
		mean<-mean(probs[,z])
		l<-append(l,mean)		
	}
	final_probs<-rbind(final_probs,l)


}
}

names<-colnames(p2p)
names[1]<-"BatsmanCluster"
names[2]<-"BowlerCluster"

colnames(final_probs)<-names
write.csv(final_probs, file = "cluster_probabilities_try.csv",row.names=FALSE)

