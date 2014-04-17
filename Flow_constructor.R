##########################
#
# CPS Flow Constructor
# 
# This script constructs longitudinally linked CPS basic monthly files
# for gross flows analysis.
# Assumes you've already downloaded data and parsed it using the
# microdata loader, and saved files as "CPS_mmyy"

library(reshape2)

# Enter months you wish to use. These MUST be entered in chronological order
files<-as.list(c("feb14","mar14"))
x <- length(files) -1

loop = seq(1:x)
i=NULL
for (i in loop){

FirstMonth <- as.character(files[i])
SecondMonth <- as.character(files[i +1])

#Load relevant objects
load(paste("CPS_",FirstMonth,sep=""))
load(paste("CPS_",SecondMonth,sep=""))

#Assign months as m1 and m2
m1 <- eval(as.name(FirstMonth))
m2 <- eval(as.name(SecondMonth))

#Construct flow. m1 will be 'x'; m2 wil be 'y'
flow<- merge(m1,m2,by="personid")

# We want to eliminate records that are obvious errors, namely those with inconsistent
# ages or sexes, or those without longitudinal weights.
flow<-flow[flow$pesex.x==flow$pesex.y,]
flow<-flow[!(flow$prtage.y < flow$prtage.x),]
flow<-flow[!(flow$prtage.y > (flow$prtage.x +1)),]
flow<-subset(flow,pwlgwgt.y>0)

# It's much easier to work with these if we have "U","E","N" designations.
# But for other work (with, say, "want a job" vs "don't want a job"),
# other data are preserved.
# Column m1 will be first-month LF status. Column m2 will be second month.

flow$m1[flow$pemlr.x==1 | flow$pemlr.x==2]<-"E"
flow$m1[flow$pemlr.x==3 | flow$pemlr.x==4]<-"U"
flow$m1[flow$pemlr.x==5 | flow$pemlr.x==6 | flow$pemlr.x==7]<-"N"
flow$m2[flow$pemlr.y==1 | flow$pemlr.y==2]<-"E"
flow$m2[flow$pemlr.y==3 | flow$pemlr.y==4]<-"U"
flow$m2[flow$pemlr.y==5 | flow$pemlr.y==6 | flow$pemlr.y==7]<-"N"
flow<-subset(flow,!is.na(m1) & !is.na(m2))

#column "flow" will be U-U, U-E, etc.
flow$flow<-paste(flow$m1,flow$m2,sep="")

#adjust weights
flow$weight<-(as.numeric(flow$pwlgwgt.y))/10000

#save flow by name of SECOND month
assign(eval(SecondMonth),flow)
save(list=SecondMonth,file=paste("Flow_",SecondMonth,sep=""))

rm(list=FirstMonth)

}
