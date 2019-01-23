#combining multiple individual .csv files of animal GPS data into one Dataset
library(tidyr)
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("dplyr","tidyr","ggplot2","lubridate","fuzzyjoin","purrr","readr","stringr","rgdal","sp")

#run function to install packages
ipak(packages)

# myMergedData <- 
#   do.call(rbindlist,
#           lapply(list.files(path = "E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Lotek/"), read.csv))



#d<-read.csv("E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Lotek/GPS_01998_OR54_15Mar2018_HM.csv")
Lotekfiles<-list.files(path = "E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Lotek/")
combLotekGPSData<-Lotekfiles %>%
  map_df(~read_csv(paste("E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Lotek/",.x,sep=""), col_types = cols(.default = "c"))) %>% 
  mutate(FxDate_Time_GMT=as.POSIXct(strptime(paste(GMT_DATE,GMT_TIME,sep=" "),format="%d.%m.%Y %H:%M:%S", tz="GMT"))) %>% 
  select(LINE_NO,GMT_DATE,GMT_TIME, LATITUDE,LONGITUDE,HEIGHT,DOP, TEMP,COLLAR_ID,Elk.ID2,FxDate_Time_GMT) 
head(combLotekGPSData)

Vectronicfiles<-list.files(path = "E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Vectronic/")
combVectronicGPSData<-Vectronicfiles %>%
  map_df(~read_csv(paste("E:/YHTGPSCollarData/GPSDATA_2017_2018_HM/Vectronic/",.x,sep=""), col_types = cols(.default = "c"))) 

colnames(combVectronicGPSData)<-c("No","CollarID","UTC_Date","UTC_Time","LMT_Date","LMT_Time","Origin","SCTS_Date","SCTS_Time","ECEF_X ","ECEF_Y","ECEF_Z","Latitude","Longitude", "Height", "DOP","FixType","3D_Error","Sats","Sat","C/N","Sat_1","C/N_1","Sat_2","C/N_2","Sat_3","C/N_3","Sat_4","C/N_4","Sat_5", "C/N_5" ,"Sat_6","C/N_6", "Sat_7","C/N_7","Sat_8", "C/N_8","Sat_9","C/N_9","Sat_10","C/N_10","Sat_11","C/N_11","Mort. Status","Activity","Main","Beacon","Temp","No_1", "No_2","Elk.ID2")     

Lotek_Vectronic_2017GPSData<-combVectronicGPSData %>% select(No,UTC_Date,UTC_Time,Latitude,Longitude,Height, DOP, Temp, CollarID, Elk.ID2) %>% 
  rename(LINE_NO=No,GMT_DATE=UTC_Date,GMT_TIME=UTC_Time,LATITUDE=Latitude,LONGITUDE=Longitude,HEIGHT=Height, DOP=DOP, TEMP=Temp, COLLAR_ID=CollarID, Elk.ID2=Elk.ID2)%>% 
  mutate(FxDate_Time_GMT=as.POSIXct(strptime(paste(GMT_DATE,GMT_TIME,sep=" "),format="%m/%d/%Y %I:%M:%S %p", tz="GMT"))) %>%  
  bind_rows(combLotekGPSData)


#checking the length of time collars were on individual animals and merging it with the GPS data.
head(Lotek_Vectronic_2017GPSData)
mortdata<-read.csv("tMORTALITY_DATA29March2018.csv",stringsAsFactors = F)
collarIDKEY<-read.csv("GPS_Mort_Immob_Key_June_2018.csv", stringsAsFactors = F)
gpsdata<-Lotek_Vectronic_2017GPSData
gpselkIDYearCollarID<-gpsdata %>% mutate(Year=format(FxDate_Time_GMT, "%Y")) %>% distinct(COLLAR_ID, Elk.ID2)# %>% left_join(collarIDKEY, by=c("COLLAR_ID"="Animal.IDHans")) %>% left_join(mortdata, by=c("MortAlias"="Name.Ear.Tag")) %>% select()
write.csv(gpselkIDYearCollarID,"YHTElkGPSMetadata.csv")
#distinct(FxDate_Time_GMT)
#change TelemDate to POSIXct. There cannot be any NAs here....
d<-gpsdata %>% 
  filter(!is.na(LATITUDE)) %>% #remove NAs from Latitude
  filter(!is.na(LONGITUDE)) %>% #remove NAs from Longitude 
  filter(LATITUDE!="N/A") %>% 
  filter(LONGITUDE!="N/A")
  #mutate(Longitude=as.numeric(LONGITUDE), Latitude=as.numeric(LATITUDE))


d$Longitude<-as.numeric(d$LONGITUDE)
d$Latitude<-as.numeric(d$LATITUDE)

#write.csv(d,"GPSCollarData2016_2017.csv")

# make spatially aware
#d <- data
coordinates(d) <- c("Longitude","Latitude") #need to change based on your coordinate columns
proj4string(d) <- CRS("+proj=longlat +ellps=WGS84") #same as above
plot(d)
nrow(d)
readOGR()

# turn easting and northing into UTMs to calculate movement parameters
library(rgdal)
#Datums:
# Idaho Transverse Mercator (UTM): "+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#NAD 83 Zone 11: 
dUTM <- spTransform(d, CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs+towgs84=0,0,0"))
#dUTM <- spTransform(d, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
dUTM <- cbind(d[,c("Elk.ID2","FxDate_Time_GMT")],coordinates(dUTM))

cbind(d[,c(1,5)],coordinates(dUTM)) #just grab animal ID, date-time column, and x coordinates and y coordinates
names(dUTM) <- c("id","date","x","y") # need to change columns to "id", "date", "x", and "y" to calculate movement parameters.

#figure out fix rate
data <- dUTM[order(dUTM$id, dUTM$date),]
#first look at hrs and minutes
hr <- as.numeric(strftime(data$date, format = "%H", tz = "GMT"))
hist(hr)
min <- as.numeric(strftime(data$date, format = "%M", tz = "GMT"))
hist(min)

#--------------------------------------------------------#
#fix rate ====
#--------------------------------------------------------#
#calculate fix interval using the TelemDate column (now "date") in seconds
dt <- c(diff(as.numeric(data$date)/3600),NA) #3600 seconds in an hour
hist(dt) #full histogram
#histogram where dt is less than 25 but greater than -1
hist(dt[dt < 25 & dt > -1]) #~2 hrs is the fix rate, some 3+ hrs fixes
hist(dt[dt < 12 & dt > -1]) #closer look (less than 12, greater than -1)
rm(dt) #remove object from enviro


#--------------------------------------------------------#
#move params ====
#--------------------------------------------------------#
# calculate movement parameters
#mac: source("/Users/brendanoates/Box Sync/UWCourses/JerodsMoveClass/Mov_modeling/MiscFunctions/creat.burst.R")
source("file path where you have create.burst script saved/creat.burst.R")
data <- data[order(data$id, data$date),] #order database first
data$burst <- creat.burst(data=data, id = TRUE, Tmax = 36000) #set Tmax to 10 hours, a little more than double the fix rate
length(unique(data$burst))
source("file path where you have mov.param script saved/mov.param.R")
data <- mov.param(data=data, burst=data$burst)
rm(creat.burst, mov.param)


#column headers:
# burst = a set of sequential locations that were within the "Tmax" threshold you set 
#         in create.burst() function. All the movement parameters are calculated relative to burst.
# dt = difference in time from previous location
# dist = distance from prevoius location
# abs.angle = absolute angle
# rel.angle = relative angle



head(data)
hist(data$abs.angle) # if there is any pattern here, should be because of the shape of your study area
hist(data$rel.angle) #should be able to see forward motion here if GPS fix rate is small enough
hist(data$dist/1000) #step length distribution in km
hist(data$dist[data$dist < 5000]/1000) #step length distribution in km
hist(data$dt/3600) #dividing by 3600 seconds shows your fix rate really well
hist(data$speed) # in meters/sec
hist(data$speed*3600/1000) # in km/hour

#----------------------------------------------------------#
# false morts ====
#----------------------------------------------------------#
# check for mortality problems
source("file path where you have mort.check script saved/mort.check.R")
morts <- mort.check(data=data, dist_thresh = 30, time_thresh = 24) #distance threshold in m, time threshold in hrs
rm(morts, mort.check)
head(data)
str(data)


#----------------------------------------------------------#
#find problem pts ====
#----------------------------------------------------------#
#Source Jerod's find.problem.pts code to search for locs that are likely erroneous
# because speed and distance covered between consecutive locations is almost impossible
source("file path where you have find.problem.pts script saved/find.problem.pts.R")
badpts <- find.problem.pts(data = data, burst = data$burst, speedlim=3) #speedlim is meters per second moved.
badpts[badpts==TRUE]

table(badpts) #TRUE points are those that should be omitted. 
# FALSE  TRUE 
# 89095    48

#query the original dataset where the bad points==TRUE matches the 
# locations in the original data
data[badpts==TRUE,] #there are 4 locs that have a dist of > 8300 m from the previous loc
#Omit the locations that have dist >8300 m
data <- data[!data$dist > 8300,]
#make spatially aware again
d <- data
#remove NA values from x column (no coordinates can have NAs)
d <- d[is.na(d$x | d$y)==FALSE,]
coordinates(d) <- c("x","y")
proj4string(d) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
plot(d)


