data<-read.csv("p2p.csv")

data<-data[-c(2406),]

names<-colnames(data)
names<-names[-c(10,12,13,14)]
names<-append(names,"prob_not_out")
names
p2p_probabilities<-data.frame()
for (i in c(1:nrow(data)))
{
	l<-c()
	l<-append(l,as.character(data[i,1]))
	l<-append(l,as.character(data[i,2]))
	for(j in c(3:9))
	{
		sum<-data[i,j]/data[i,10]
		l<-append(l,sum)

	}
	sum<-data[i,11]/data[i,10]
	l<-append(l,sum)
	l<-append(l,(1-sum))
    	p2p_probabilities<-rbind(p2p_probabilities,l,stringsAsFactors=FALSE)
}

colnames(p2p_probabilities)<-names
#p2p_probabilities

write.csv(p2p_probabilities, file = "p2p_probabilities.csv",row.names=FALSE)
