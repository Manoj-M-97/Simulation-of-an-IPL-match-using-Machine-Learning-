data<-read.csv("p2p.csv")

st<-data[[1]]
st<-unique(st)

batsman<-data.frame()
for (i in st)
{
	l<-c()

	newdata<-data[which(data[[1]]==i),]
	l<-append(l,as.character(newdata[1,1]))

	for (j in c(3:13))
	{
		sum<-newdata[,j]
		sum<-sum(sum)
		l<-append(l,as.numeric(sum))
	}
	avg<-newdata[,14]
	avg<-mean(avg)
	l<-append(l,as.double(avg))	
    	batsman<-rbind(batsman,l,stringsAsFactors=FALSE)
	
} 
names<-colnames(data)
names<-names[-c(2)]

colnames(batsman)<-names
batsman<-batsman[-c(137),]
batsman
write.csv(batsman, file = "Batsman.csv",row.names=FALSE)
