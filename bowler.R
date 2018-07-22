data<-read.csv("p2p.csv")

st<-data[[2]]
st<-unique(st)

bowler<-data.frame()
for (i in st)
{
	l<-c()

	newdata<-data[which(data[[2]]==i),]
	
	l<-append(l,as.character(newdata[1,2]))

	for (j in c(3:13))
	{
		sum<-newdata[,j]
		sum<-sum(sum)
		l<-append(l,as.numeric(sum))
	}

    	bowler<-rbind(bowler,l,stringsAsFactors=FALSE)
	
} 
names<-colnames(data)
names<-names[-c(1,14)]

colnames(bowler)<-names
bowler<-bowler[-c(108),]
bowler

write.csv(bowler, file = "Bowler.csv",row.names=FALSE)
