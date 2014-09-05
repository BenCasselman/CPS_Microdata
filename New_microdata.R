################################
#
# Code for newly released Microdata
#
################################

################################
# 
# This is the code I run each month to parse
# newly released microdata, construct flows,
# and update my regular analysis files. It
# assumes you're updating existing files, using
# the same format.
# Note: This is raw code, with minimal effort
# made to clean it up for general use. Apologies
# for relative lack of annotation.
# Comments/questions welcome: ben.casselman@fivethirtyeight.com


###################
#
#Data entry
#
###################
#Enter desired month, in this format
Month<-"jul14"
PriorMonth<-"jun14"

# First, download, parse microdata

#Prep
setwd()
library(reshape2)
memory.limit(size=50000)

load("AgeFlow3.RData")
load("AgeFlow4.RData")
load("DurFlow4.RData")
load("FlowByDuration.RData")

#Get column header info
#Make sure to use correct file for data you'll be downloading
series_ids<-read.csv("ColNames.csv",strip.white=TRUE, stringsAsFactors=FALSE) #May need to modify headers for pre-Jan. 2013 files
headers<-as.list(series_ids$series_id)
lengths<-as.list(series_ids$series_length)


#########################
#
#Data retrieval
#
#########################
#Now get the data
temp<-tempfile()
URL<-paste("http://thedataweb.rm.census.gov/pub/cps/basic/201401-/",Month,"pub.zip",sep="")
FileName<-paste(Month,"pub.dat",sep="")

#Download file. Enter target destination in in quotes.
download.file(URL,temp)
#Read it as FWF
#test>-read.fwf(unz(temp,FileName),widths=series_ids$series_length,col.names=headers,stringsAsFactors=FALSE,na.strings=c("-1",-1),n=10)

raw<-read.fwf(unz(temp,FileName),widths=c(15,2,4,2,3,2,2,2,2,2,2,2,2,2,2,10,2,
                                          2,2,2,2,2,2,5,2,1,2,2,2,2,2,2,2,2,1,
                                          5,3,1,1,1,1,3,3,2,2,2,2,2,1,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,3,2,2,2,2,2,2,2,2,2,
                                          2,3,2,5,2,2,2,2,2,2,2,2,2,2,2,2,2,3,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,3,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,6,2,2,6,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,
                                          4,4,1,2,8,1,4,8,8,1,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,10,10,10,10,10,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,
                                          2,2,2,2,2,2,2,2,2,2,2,2,5,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,10,4,4,4,4,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                          2,2,2,2,2,2,2,2,2,2,15),
              col.names=headers,stringsAsFactors=FALSE,na.strings=c("-1",-1))
unlink(temp)
#create unique person identifier.
raw_clean<-raw
raw_clean$personid <-paste(raw_clean$hrhhid,
                           raw_clean$hrhhid2,raw_clean$pulineno,sep="-")
#Drop records where pulineno is NA
raw_clean<-subset(raw_clean,!is.na(pulineno))
#create date
raw_clean$date<-as.Date(paste(raw_clean$hryear4,
                              raw_clean$hrmonth,"01",sep="-"),"%Y-%m-%d")

#Save object as Month and save to file as CPS_Month.
#Also save raw file separately, in case you need to fix it later
#(trust me, you'll be glad of this).
assign(eval(Month),raw_clean)
save(list=Month,file=paste("CPS_",Month,sep=""))
save(raw,file=paste("Raw_",Month,sep=""))

rm(raw_clean,raw)

#####################################################################
# Now construct flow

months<-as.list(c(Month,PriorMonth))

#Load relevant objects
load(paste("CPS_",PriorMonth,sep=""))

#Assign months as m1 and m2
m1 <- eval(as.name(PriorMonth))
m2 <- eval(as.name(Month))

#Construct flow. m1 will be 'x'; m2 wil be 'y'
flow<- merge(m1,m2,by="personid")

# We want to eliminate records that are obvious errors, namely those with inconsistent
# ages or sexes, or those without longitudinal weights.
flow<-flow[flow$pesex.x==flow$pesex.y,]
flow<-flow[!(as.numeric(flow$prtage.y) < as.numeric(flow$prtage.x)),]
flow<-flow[!(as.numeric(flow$prtage.y) > (as.numeric(flow$prtage.x) +1)),]
flow<-subset(flow,pwlgwgt.y>0)

#It's much easier to work with these if we have "U","E","N" designations.
#But for other work (with, say, "want a job" vs "don't want a job"),
#other data are preserved.
#Column m1 will be first-month LF status. Column m2 will be second month.

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
assign(eval(Month),flow)
save(list=Month,file=paste("Flow_",Month,sep=""))

########################################################################
# Now calculate various flows

load("FlowByDuration.RData")
load("DurFlow4.RData")
load("AgeFlow3.RData")
load("AgeFlow4.RData")

#Enter file names here
working <- eval(as.name(Month))
  
#This is a big file, so pare it down to what we want:
#Age, sex, labor force status (pemlr), want/don't want job (prwntjob), 
#duration of unemployment, and the m1/m2/flow categories from earlier
#file.

working<-subset(working,select=c("personid","date.x","date.y","prtage.x",
                                 "prtage.y","pesex.x","pesex.y","pemaritl.x","pemaritl.y","pemlr.x",
                                 "prunedur.x","prunedur.y","pemlr.y","prwntjob.x","prwntjob.y",
                                 "weight","m1","m2","flow"))

#This file already has three-way states, but we need to create a new
#four-way flow based on prwntjob. We'll call these 'm1b' and 'm2b', 
#and the flow 'flowb'

working$m1b[working$m1 == "E"]<-"E"
working$m1b[working$m1 == "U"]<-"U"
working$m1b[(working$m1 == "N")&(working$prwntjob.x == 1)]<-"W"
working$m1b[(working$m1 == "N")&(working$prwntjob.x == 2)]<-"D"

working$m2b[working$m2 == "E"]<-"E"
working$m2b[working$m2 == "U"]<-"U"
working$m2b[(working$m2 == "N")&(working$prwntjob.y == 1)]<-"W"
working$m2b[(working$m2 == "N")&(working$prwntjob.y == 2)]<-"D"

working$flowb<-paste(working$m1b,working$m2b,sep="")

#We also need age categories.
#We'll set categories as: 16-18,19-24, 25-54,55-64,65+.
#We'll also use ages from FIRST month.
working$age[working$prtage.x<19]<-"16-19"
working$age[working$prtage.x>18 & working$prtage.x<25]<-"19-24"
working$age[working$prtage.x>24 & working$prtage.x<55]<-"25-54"
working$age[working$prtage.x>54 & working$prtage.x<65]<-"55-64"
working$age[working$prtage.x>64]<-"65+"

#This is the full file. 
#Save it for later use as "Flow4_Month"
assign(eval(Month),working)
save(list=Month,file=paste("Flow4_",Month,sep=""))

#########################
#
# Now we construct our various flows (by duration, etc)


###################
#
#Construct flow by duration
#

dur3<- subset(working,prunedur.x>=0 & m1=="U")

dur3$durrange[dur3$prunedur.x>=0 & dur3$prunedur.x<5]<-"1. Less than 5 weeks"
dur3$durrange[dur3$prunedur.x>4 & dur3$prunedur.x<15]<-"2. 5-14 weeks"
dur3$durrange[dur3$prunedur.x>14 & dur3$prunedur.x<27]<-"3. 15-26 weeks"
dur3$durrange[dur3$prunedur.x>26 & dur3$prunedur.x<53]<-"4. 27-52 weeks"
dur3$durrange[dur3$prunedur.x>52]<-"5. 53+ weeks"

dur3$fields<-paste(dur3$durrange,dur3$flow,sep="-")
dur3<-dcast(dur3,date.y ~ fields,value.var="weight",sum)
colnames(dur3)[1]<-"date"

#add totals columns
dur3$TotalUE<-sum(working$weight[working$flow=="UE"])
dur3$TotalUU<-sum(working$weight[working$flow=="UU"])
dur3$TotalUN<-sum(working$weight[working$flow=="UN"])

#add column for prior month unemployed
dur3$Unemp<-sum(working$weight[working$m1=="U"])

#and construct finding/exit rates
dur3$find<-dur3$TotalUE/dur3$Unemp
dur3$leave<-dur3$TotalUN/dur3$Unemp

#Add it to file with rest of flow by duration
x<-nrow(FlowByDuration)
FlowByDuration<-rbind(FlowByDuration[1:x,],dur3,FlowByDuration[-(1:x),])
FlowByDuration<-FlowByDuration[order(FlowByDuration$date),]

# First we'll do a flow by age. Unlike with flows by duration,
# we want flows from employed as well as from unemployed, and flows
# into the LF as well as out of it.
# We'll call this file "age3" to distinguish from "age4" (the four-state
# flow).

#Construct flow by age
age3<-working
age3$fields<-paste(age3$age,age3$flow,sep="-")
age3<-dcast(age3,date.y ~ fields,value.var="weight",sum)
colnames(age3)[1]<-"date"

#add totals columns
age3$TotalUE<-sum(working$weight[working$flow=="UE"])
age3$TotalUN<-sum(working$weight[working$flow=="UN"])
age3$TotalUU<-sum(working$weight[working$flow=="UU"])
age3$TotalEE<-sum(working$weight[working$flow=="EE"])
age3$TotalEN<-sum(working$weight[working$flow=="EN"])
age3$TotalEU<-sum(working$weight[working$flow=="EU"])
age3$TotalNE<-sum(working$weight[working$flow=="NE"])
age3$TotalNN<-sum(working$weight[working$flow=="NN"])
age3$TotalNU<-sum(working$weight[working$flow=="NU"])

#add column for prior month unemployed
age3$Unemp<-sum(working$weight[working$m1=="U"])

#and construct finding/exit rates
age3$find<-age3$TotalUE/age3$Unemp
age3$leave<-age3$TotalUN/age3$Unemp

#Add it to file with rest of flow by age
x<-nrow(AgeFlow3)
AgeFlow3<-rbind(AgeFlow3[1:x,],age3,AgeFlow3[-(1:x),])
AgeFlow3<-AgeFlow3[order(AgeFlow3$date),]

#########
# We now do the same thing, just with a four-month flow.
# This one will be age4

age4<-working
age4$fields<-paste(age4$age,age4$flowb,sep="-")
age4<-dcast(age4,date.y ~ fields,value.var="weight",sum)
colnames(age4)[1]<-"date"

#add totals columns
age4$TotalUE<-sum(working$weight[working$flowb=="UE"])
age4$TotalUU<-sum(working$weight[working$flowb=="UU"])
age4$TotalUW<-sum(working$weight[working$flowb=="UW"])
age4$TotalUD<-sum(working$weight[working$flowb=="UD"])
age4$TotalEE<-sum(working$weight[working$flowb=="EE"])
age4$TotalEU<-sum(working$weight[working$flowb=="EU"])
age4$TotalEW<-sum(working$weight[working$flowb=="EW"])
age4$TotalED<-sum(working$weight[working$flowb=="ED"])
age4$TotalWE<-sum(working$weight[working$flowb=="WE"])
age4$TotalWU<-sum(working$weight[working$flowb=="WU"])
age4$TotalWW<-sum(working$weight[working$flowb=="WW"])
age4$TotalWD<-sum(working$weight[working$flowb=="WD"])
age4$TotalDE<-sum(working$weight[working$flowb=="DE"])
age4$TotalDU<-sum(working$weight[working$flowb=="DU"])
age4$TotalDW<-sum(working$weight[working$flowb=="DW"])
age4$TotalDD<-sum(working$weight[working$flowb=="DD"])

#Add it to file with rest of flow by age
x<-nrow(AgeFlow4)
AgeFlow4<-rbind(AgeFlow4[1:x,],age4,AgeFlow4[-(1:x),])
AgeFlow4<-AgeFlow4[order(AgeFlow4$date),]

#########
# Lastly we do a four-month flow by duration, as dur4.
# For this one, we just want people who are unemployed in
# the first month. Works like the normal flow by duration, but
# with four states instead of three.

dur4<- subset(working,prunedur.x>=0 & m1=="U")

dur4$durrange[dur4$prunedur.x>=0 & dur4$prunedur.x<5]<-"1. Less than 5 weeks"
dur4$durrange[dur4$prunedur.x>4 & dur4$prunedur.x<15]<-"2. 5-14 weeks"
dur4$durrange[dur4$prunedur.x>14 & dur4$prunedur.x<27]<-"3. 15-26 weeks"
dur4$durrange[dur4$prunedur.x>26 & dur4$prunedur.x<53]<-"4. 27-52 weeks"
dur4$durrange[dur4$prunedur.x>52]<-"5. 53+ weeks"

dur4$fields<-paste(dur4$durrange,dur4$flowb,sep="-")
dur4<-dcast(dur4,date.y ~ fields,value.var="weight",sum)
colnames(dur4)[1]<-"date"

#add totals columns
dur4$TotalUE<-sum(working$weight[working$flowb=="UE"])
dur4$TotalUU<-sum(working$weight[working$flowb=="UU"])
dur4$TotalUW<-sum(working$weight[working$flowb=="UW"])
dur4$TotalUD<-sum(working$weight[working$flowb=="UD"])

#Add it to file with rest of flow by duration
x<-nrow(DurFlow4)
DurFlow4<-rbind(DurFlow4[1:x,],dur4,DurFlow4[-(1:x),])
DurFlow4<-DurFlow4[order(DurFlow4$date),]

rm(list=Month)

####################################################################
# 
# Might want to check before saving these

save(AgeFlow3,file="AgeFlow3.RData") #save it
save(AgeFlow4,file="AgeFlow4.RData") #save it
save(DurFlow4,file="DurFlow4.RData") #save it
save(FlowByDuration,file="FlowByDuration.RData") #save it

#Also export to CSVs
write.csv(AgeFlow3,file="AgeFlow3.csv")
write.csv(AgeFlow4,file="AgeFlow4.csv")
write.csv(DurFlow4,file="DurFlow4.csv")
write.csv(FlowByDuration,file="FlowByDuration.csv")
