library(sqldf)
library(dplyr)
library(tidyr)

rawOTP<-read.csv("AllRawOTP.csv",stringsAsFactors = FALSE)
#renaming columns to get rid of .
colnames(rawOTP)<-c("Date", "Route", "Bus", "PlannedStartTime", "PlannedAnchorTime", 
                    "AnchorTimeConverted", "ActualArrivalTime", "ArrivalDelay", "AMPM", 
                    "TimeTier", "TypeOfBus","PercentOffRoute","DepartureDelay",
                    "DepartedOnTime", "School", "Yard", "TierBreakdown", "MonitorIssue",
                    "Monitor", "CR", "DtD", "TotalRider", "AverageSpeed", "TripLength",
                    "TripDistance", "AverageActualSpeed", "DeparteOnTime", "Driver")
#We only care for the AM data, specifically the time tiers 7:30, 8:30, 9:30
OTP<-rawOTP[rawOTP$AMPM=="AM",]
OTP<-OTP[OTP$TimeTier %in% c("7:30","8:30","9:30"),]
#dropping 10/17/2017 due to bad data
OTP<-OTP[OTP$Date!="10/17/2017",]

#Fill in missing values based on route number
#Starting off with filling in missing school name.  Need to separate first 4 digits of route number
OTP$schoolNum<-substr(as.character(OTP$Route),1,4)
#creating a reference table of school code and school name
school<-OTP[OTP$School!="",c(29,15)]
dupschool<-duplicated(school$schoolNum)
school<-school[!dupschool,]

#Cycling though data frame to replace missing School names.  For ones that had no school name, the value is "BPS Unknown"
x<-length(OTP[,1])
i<-1
for(i in 1:x){
  if (OTP[i,15]==""){
      OTP[i,15]<-school[school$schoolNum==OTP[i,29],2]
  }
}


#Filling in data that is unique to route (CR, DtD, TotalRider, AverageSpeed, TripLength)
#creating a reference table of route number and route data
route<-OTP[OTP$Route!="",c(2,20:25)]
duproute<-duplicated(route$Route)
route<-route[!duproute,]

#Cycling though data frame to replace missing information.  
x<-length(OTP[,1])
i<-1
for(i in 1:x){
  if (is.na(OTP[i,20])){
    OTP[i,20]<-route[route$Route==OTP[i,2],2]
  }
  if (is.na(OTP[i,21])){
    OTP[i,21]<-route[route$Route==OTP[i,2],3]
  }
  if (is.na(OTP[i,22])){
    OTP[i,22]<-route[route$Route==OTP[i,2],4]
  }
  if (is.na(OTP[i,23])){
    OTP[i,23]<-route[route$Route==OTP[i,2],5]
  }
  if (is.na(OTP[i,24])){
    OTP[i,24]<-route[route$Route==OTP[i,2],6]
  }
  if (is.na(OTP[i,25])){
    OTP[i,25]<-route[route$Route==OTP[i,2],7]
  }
}


#bringing in weather data
weather<-read.csv("weatherfinal.csv", stringsAsFactors = FALSE)
OTP<-sqldf("select 
           OTP.*, 
           wea.vis as Visibility,
           wea.fog, wea.rain, wea.snow, wea.hail, wea.thunder, wea.tornado
           from OTP 
           join weather wea on OTP.Date==wea.Date and OTP.TimeTier==wea.Timetier")

write.csv(OTP,"AlldaysOTP.csv")
