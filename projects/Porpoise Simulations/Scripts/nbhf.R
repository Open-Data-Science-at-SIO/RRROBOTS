
get.nbhf<-function(data,pod.in,pod.out){

 new.time<- strptime(data$Time,"%d/%m/%Y %H:%M",tz="GMT")

# date.time<-matrix(unlist(strsplit(toString(data$Time)," ")),ncol=2,byrow=TRUE)
# dmy<-matrix(unlist(strsplit(date.time[,1],"/")),ncol=3,byrow=TRUE)
# hm<-matrix(unlist(strsplit(date.time[,2],":")),ncol=2,byrow=TRUE)
# 
# gmt.time<-ISOdatetime(
# 	as.numeric(dmy[,3]),
# 	as.numeric(dmy[,2]),
# 	as.numeric(dmy[,1]),
# 	as.numeric(hm[,1]),
# 	as.numeric(substr(hm[,2],1,2)),
# 	00,tz="GMT")

# copy data to replace timestamp
newdata<-data
newdata$Time<-new.time
# limit to records from when the CPOD went in and out of the water
truncated.trains<-newdata[which(newdata$Time>pod.in & newdata$Time<pod.out),]
# filter to include only high quality porpoise clicks
nbhf.high<-truncated.trains[
	which(truncated.trains$SpClass == "NBHF" & truncated.trains$TrClass == "High"),]
# nbhf.high contains all data for records that match these criteria
nbhf.high
	}
	
	