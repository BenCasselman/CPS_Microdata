#########################################
#
# CPS Microdata Downloader
# Downloads CPS microdata and outputs as monthly dataframe
#
#########################################

# Prep
library(reshape2)
setwd() #set working directory here
# Get column header info--only have to do this once.
series_ids<-read.csv("ColNames.csv",strip.white=TRUE, stringsAsFactors=FALSE) #May need to modify headers for pre-Jan. 2013 files
headers<-as.list(series_ids$series_id)
lengths<-as.list(series_ids$series_length)

###################
#
# Data entry
#
###################

# Enter desired month, in this format
Month<-"oct13"

#########################
#
# Data retrieval
#
#########################

temp<-tempfile()
URL<-paste("http://thedataweb.rm.census.gov/pub/cps/basic/201301-/",Month,"pub.zip",sep="")
FileName<-paste(Month,"pub.dat",sep="")

# Download file. Enter target destination in in quotes.
download.file(URL,temp)
# Read it as FWF
raw<-read.fwf(unz(temp,FileName),widths=as.list(lengths),
col.names=headers,stringsAsFactors=FALSE,na.strings=c("-1",-1))
unlink(temp)
# create unique person identifier. Drops records with pulineno as NA
raw_clean<-raw
raw_clean$personid <-paste(raw_clean$hrhhid,
	raw_clean$hrhhid2,na.omit(raw_clean$pulineno),sep="-")

# Drop records where pulineno is NA
raw_clean<-subset(raw_clean,!is.na(pulineno))

raw_clean$date<-as.Date(paste(raw_clean$hryear4,
	raw_clean$hrmonth,"01",sep="-"),"%Y-%m-%d")

# Save object as Month and save to file as CPS_Month

assign(eval(Month),raw_clean)
save(list=Month,file=paste("CPS_",Month,sep=""))