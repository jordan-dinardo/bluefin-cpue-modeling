# Identification of unique trip events in CPFV fisheries data (single trips as identified by CDFW)
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(purrr);library(lubridate);library(stringr)

## Clean catch and effort data ##

# load raw fisheries CPFV catch and effort data 

######## insert file path of where raw cpfv dfs are located ##################
catch_data <- list.files("#### insert file path of where raw cpfv dfs are located ###",pattern = ".csv",full.names = T) 
##############################################################################
catch_data <- do.call(rbind,lapply(catch_data,read.csv)) #merge raw datasets 

catch_data$SpeciesCode<-as.numeric(catch_data$SpeciesCode) #change Species code to "numeric"
catch_data<-catch_data %>% 
  dplyr::mutate(TripType = "is.na<-"(TripType, TripType=="")) #change "" to NAs in TripType var

#coarse removal of NAs from VesselID, Species Code, Number kept, and LogDate (which are strictly necessary in CPUE standardization process)
catch_data <- catch_data[!(is.na(catch_data$VesselID)) & 
                     !(is.na(catch_data$LogDate))&
                     !(is.na(catch_data$SpeciesCode))& 
                     !(is.na(catch_data$NumberKept)),]  

catch_data$LogDate<-as.Date(catch_data$LogDate, format="%m/%d/%Y",tz="GMT") #change LogDate car to a Date var

catch_data<-subset(catch_data,TripType=="Single Day") #subset to data to single day trip type (as deined by CDFW)

#Format Departure/Return times to be consistent in the form HH:MM:SS (to be converted to Date variable)
catch_data<-catch_data%>%
  dplyr::mutate(
    dep_t=DepartureTime,
    dep_t=ifelse(str_count(dep_t,"[0-9]")==1,paste0("0",dep_t,"00"),dep_t),
    dep_t=ifelse(str_count(dep_t,"[0-9]")==2&dep_t<=24,paste0(dep_t,"00"),dep_t),
    dep_t=ifelse(str_count(dep_t,"[0-9]")==2&dep_t>24,paste0("0",dep_t,"0"),dep_t),#makes the most sense in my mind
    dep_t=ifelse(str_count(dep_t,"[0-9]")==3,paste0("0",dep_t),dep_t),
    ret_t=ReturnTime,
    ret_t=ifelse(str_count(ret_t,"[0-9]")==1,paste0("0",ret_t,"00"),ret_t),
    ret_t=ifelse(str_count(ret_t,"[0-9]")==2&ret_t<=24,paste0(ret_t,"00"),ret_t),
    ret_t=ifelse(str_count(ret_t,"[0-9]")==2&ret_t>24,paste0("0",ret_t,"0"),ret_t),#makes the most sense in my mind
    ret_t=ifelse(str_count(ret_t,"[0-9]")==3,paste0("0",ret_t),ret_t))

#add colons to differentiate hours, minutes, seconds
catch_data$dep_t<-sub( '(?<=.{2})', ':', catch_data$dep_t, perl=TRUE )
catch_data$ret_t<-sub( '(?<=.{2})', ':', catch_data$ret_t, perl=TRUE )
catch_data$dep_t<-paste0(catch_data$dep_t,":00")
catch_data$ret_t<-paste0(catch_data$ret_t,":00")

#change 00:00:00 and 24:00:00 to 01:00:00 (both are equivalent to 1 AM; allows for proper manipulation with POSIXct)
catch_data$dep_t<-ifelse(catch_data$dep_t=="00:00:00"|catch_data$dep_t=="24:00:00","01:00:00",catch_data$dep_t)
catch_data$ret_t<-ifelse(catch_data$ret_t=="00:00:00"|catch_data$ret_t=="24:00:00","01:00:00",catch_data$ret_t)

#string date and time together for both Departure/Return Times and convert to a Date object using POSIXct
catch_data$dep_date_t<-paste(paste(catch_data$LogYear,catch_data$LogMonth,catch_data$LogDay,sep="-"),catch_data$dep_t)
catch_data$dep_date_t<-as.POSIXct(catch_data$dep_date_t,format="%Y-%m-%d %H:%M:%S",tz="UTC")

catch_data$ret_date_t<-paste(paste(catch_data$LogYear,catch_data$LogMonth,catch_data$LogDay,sep="-"),catch_data$ret_t)
catch_data$ret_date_t<-as.POSIXct(catch_data$ret_date_t,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#Identify unique trips based on dep_t of consecutive days using a lag on dept_t across rows
catch_data<-catch_data%>%
  arrange(VesselID,LogDate)%>%
  group_by(VesselID)%>%
  mutate(
    diff_date = c(NA,diff(dep_date_t)),
    trip_split=ifelse(diff_date==0,0,1),
    trip_split=ifelse(is.na(trip_split),1,trip_split)
  )

catch_data$trip_id<-cumsum(catch_data$trip_split)

#calculate number of unique return times and blocks per trip
catch_data<-catch_data%>%
  group_by(trip_id)%>%
  mutate(
    n_returntimes=length(unique(ret_t)),
    n_blocks=length(unique(Block)),
    n_hoursfished=length(unique(HoursFished)),
    n_anglers=length(unique(NumberOfFishers))
    
  )

catch_data<-catch_data%>%
  arrange(VesselID,LogDate,DepartureTime,ReturnTime, Block,NumberOfFishers)

catch_data$trip_event_id <- cumsum(!duplicated(catch_data[c("VesselID", "LogDate","DepartureTime", "ReturnTime", "HoursFished", "Block", "trip_id", "NumberOfFishers", "n_returntimes","n_blocks", "n_hoursfished", "n_anglers")]))

#Update return times and hours fished based on n_returntimes and n_blocks and n_anglers

#find distinct entries by trip_event
distinct_dat<-catch_data[,c("VesselID","LogDate","DepartureTime","ReturnTime","HoursFished","trip_id","n_returntimes","n_blocks","n_anglers","n_hoursfished","Block","NumberOfFishers","trip_event_id","TripType")]
distinct_dat<-distinct_dat%>%
  distinct()


#calculate total hours fished per trip (because fishermen enter different hours fished based on blocks,number of anglers, etc.)
distinct_dat_sum<-distinct_dat%>%
  group_by(trip_id)%>%
  summarise(
    hrs_fished_all=sum(HoursFished)
  )


#merge df with summarised fishing hours with df of distinct entries by ID
distinct_dat<-merge(distinct_dat,distinct_dat_sum,by="trip_id")

#Use maximum return time when trip includes multiple return times
distinct_dat<-distinct_dat%>%
  group_by(trip_id)%>%
  mutate(
    ReturnTime=ifelse(n_returntimes>1,max(ReturnTime),ReturnTime))

#Re-format Departure and new Return Time based on max return time of trip (when multiple return times are present)
distinct_dat$DepartureTime<-as.character(distinct_dat$DepartureTime)
distinct_dat$ReturnTime<-as.character(distinct_dat$ReturnTime)
distinct_dat<-distinct_dat%>%
  mutate(
    dep_t=DepartureTime,
    dep_t=ifelse(str_count(dep_t,"[0-9]")==1,paste0("0",dep_t,"00"),dep_t),
    dep_t=ifelse(str_count(dep_t,"[0-9]")==2&dep_t<=24,paste0(dep_t,"00"),dep_t),
    dep_t=ifelse(str_count(dep_t,"[0-9]")==2&dep_t>24,paste0("0",dep_t,"0"),dep_t),#makes the most sense in my mind
    dep_t=ifelse(str_count(dep_t,"[0-9]")==3,paste0("0",dep_t),dep_t),
    ret_t=ReturnTime,
    ret_t=ifelse(str_count(ret_t,"[0-9]")==1,paste0("0",ret_t,"00"),ret_t),
    ret_t=ifelse(str_count(ret_t,"[0-9]")==2&ret_t<=24,paste0(ret_t,"00"),ret_t),
    ret_t=ifelse(str_count(ret_t,"[0-9]")==2&ret_t>24,paste0("0",ret_t,"0"),ret_t),#makes the most sense in my mind
    ret_t=ifelse(str_count(ret_t,"[0-9]")==3,paste0("0",ret_t),ret_t))

#add colons to differentiate hours, minutes, seconds
distinct_dat$dep_t<-sub( '(?<=.{2})', ':', distinct_dat$dep_t, perl=TRUE )
distinct_dat$ret_t<-sub( '(?<=.{2})', ':', distinct_dat$ret_t, perl=TRUE )
distinct_dat$dep_t<-paste0(distinct_dat$dep_t,":00")
distinct_dat$ret_t<-paste0(distinct_dat$ret_t,":00")

#change 00:00:00 and 24:00:00 to 01:00:00 (both are equivalent to 1 AM; allows for proper manipulation with POSIXct)
distinct_dat$dep_t<-ifelse(distinct_dat$dep_t=="00:00:00"|distinct_dat$dep_t=="24:00:00","01:00:00",distinct_dat$dep_t)
distinct_dat$ret_t<-ifelse(distinct_dat$ret_t=="00:00:00"|distinct_dat$ret_t=="24:00:00","01:00:00",distinct_dat$ret_t)

#string date and time together for both Departure/Return Times and convert to a Date object using POSIXct
distinct_dat$dep_date_t<-paste(distinct_dat$LogDate,distinct_dat$dep_t)
distinct_dat$dep_date_t<-as.POSIXct(distinct_dat$dep_date_t,format="%Y-%m-%d %H:%M:%S",tz="UTC")

distinct_dat$ret_date_t<-paste(distinct_dat$LogDate,distinct_dat$ret_t)
distinct_dat$ret_date_t<-as.POSIXct(distinct_dat$ret_date_t,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#merge distinct df by ID (with new return times) and original df of trips
catch_data<-merge(catch_data,distinct_dat,by=c("trip_event_id","trip_id","VesselID","LogDate","DepartureTime","dep_date_t","dep_t","Block","HoursFished","NumberOfFishers","n_returntimes","n_blocks","n_anglers","n_hoursfished","TripType"),all.x=T)

#subset vars to identify unique trips for ease/efficiency
catch_sub1<-data.frame(catch_data$VesselID,catch_data$LogDate,catch_data$LogYear,catch_data$LogMonth,catch_data$LogDay,catch_data$trip_id,catch_data$dep_t,catch_data$ret_t.x,catch_data$ret_t.y,catch_data$dep_date_t,catch_data$ret_date_t.y,catch_data$HoursFished,catch_data$hrs_fished_all,catch_data$Block,catch_data$DepartureTime,catch_data$ReturnTime.x,catch_data$ReturnTime.y,catch_data$trip_event_id,catch_data$n_hoursfished,catch_data$n_blocks,catch_data$n_anglers,catch_data$n_returntimes,catch_data$TripType,catch_data$NumberOfFishers,catch_data$SpeciesCode,catch_data$NumberKept)
colnames(catch_sub1)<-c("VesselID","LogDate","LogYear","LogMonth","LogDay","trip_id","dep_t","ret_t","ReturnTime_Constant","dep_date_time","ret_date_time","HoursFished","hrs_fished_all","Block","DepartureTime_int","ReturnTime_int_Original","ReturnTime_int_Constant","trip_event_id","n_hoursfished","n_blocks","n_anglers","n_returntimes","TripType_CDFW","NumberOfFishers","SpeciesCode","NumberKept")

#Check Trip ID (count number of number of vessels, days within each trip (n_dates) and unique days with departure and return time (n_date_time))
catch_sub1$LogDate<-as.Date(catch_data$LogDate, format="%m/%d/%Y",tz="UTC")

catch_sub2<-catch_sub1%>%
  group_by(trip_id,VesselID)%>%
  mutate(
    n_vessel=n_distinct(VesselID))

catch_sub2<-catch_sub2%>%
  group_by(trip_id)%>%
  mutate(
    n_dates=n_distinct(LogDate))

#Calculate number of hours at sea (difference between Return and Departure Times) 
catch_sub2$t_sea<-difftime(catch_sub2$ret_date_time,catch_sub2$dep_date_time,tz="GMT",units="hours")
catch_sub2$t_sea<-as.numeric(catch_sub2$t_sea)
catch_sub2$ret_date_DIN<-as.Date(catch_sub2$ret_date_time)

sub_change<-subset(catch_sub2,t_sea<=0&!is.na(t_sea)&!is.na(hrs_fished_all)) #entries where a vessel is returning at an earlier time than departure
sub_change1<-subset(catch_sub2,!is.na(hrs_fished_all)&!is.na(t_sea)) 
sub_change1<-subset(sub_change1,t_sea>0&(hrs_fished_all-abs(t_sea))>2) #entries where a vessel is returning at a later time than departure time  and anglers are fishing for more hours than time at sea (add a day to return date)
sub_keep1<-subset(catch_sub2,!is.na(hrs_fished_all)&!is.na(t_sea))
sub_keep1<-subset(sub_keep1,t_sea>0&(hrs_fished_all-abs(t_sea))<=2) #entries where a vessel is returning at a later time than departure time and anglers are fishing for less time than time at sea (add a day to return date)
sub_keep2<-subset(catch_sub2,is.na(hrs_fished_all)|is.na(t_sea)) #entries where Hours fished or/and Time at sea is NA

#--------------
#create new return date based on Time at Sea and hours fished; if time at sea and hours fished indicate vessel 
#returned the following day of departure change return date to match (ret_date_new)
sub_change$ret_date_new<-sub_change$ret_date_DIN%m+%days(1) #add a day to return date
sub_change1$ret_date_new<-sub_change1$ret_date_DIN%m+%days(1) #add a day to return date
sub_keep1$ret_date_new<-sub_keep1$ret_date_DIN #keep return date consistent
sub_keep2$ret_date_new<-sub_keep2$ret_date_DIN #keep return date consistent
catch_sub2<-rbind(sub_change,sub_change1,sub_keep1,sub_keep2)

#format and convert new return date to a Date object
catch_sub2$ret_date_new<-paste(catch_sub2$ret_date_new,catch_sub2$ReturnTime_Constant)
catch_sub2$ret_date_new<-as.POSIXct(catch_sub2$ret_date_new,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#Calculate more accurate time at sea (with updated return dates)
catch_sub2$t_sea_accurate<-difftime(catch_sub2$ret_date_new,catch_sub2$dep_date_time,tz="GMT",units="hours")

#change data variable to numeric types to compare correctly
catch_sub2$t_sea_accurate<-as.numeric(catch_sub2$t_sea_accurate)

#calculate difference in hours fished and the updated ("more accurate") time at sea metric
catch_sub2$diff_hrs_fished_DIN<-catch_sub2$t_sea_accurate-catch_sub2$hrs_fished_all
##noticed there are some entries where difference is still negative, which means that we need to add another day to those entries and 
##there are a lot of entries with NA in Departure/Return/HoursFished...OMIT FROM ANALYSES

sub1<-subset(catch_sub2,!is.na(hrs_fished_all)&!is.na(t_sea)&!is.na(t_sea_accurate)) 
sub1<-subset(sub1,(hrs_fished_all-t_sea_accurate)>2) #buffer of 2 hours ; trips where hours fished surpassed time at sea (at present)
sub2<-subset(catch_sub2,!is.na(hrs_fished_all)&!is.na(t_sea)&!is.na(t_sea_accurate))
sub2<-subset(sub2,(hrs_fished_all-t_sea_accurate)<=2) #buffer of 2 hours; trips where hours fished has not surpassed time at sea
sub3<-subset(catch_sub2,is.na(hrs_fished_all)|is.na(t_sea)|is.na(t_sea_accurate)) #trips where hours fished and/or time at sea is NA 


sub1$ret_date_new<-as.Date(sub1$ret_date_DIN)%m+%days(2) #add two days to trips where hours fished surpassed time at sea (at present)

#create new return date (format and convert to Date object)
sub1$ret_date_new<-paste(sub1$ret_date_new,sub1$ReturnTime_Constant)
sub1$ret_date_new<-as.POSIXct(sub1$ret_date_new,format="%Y-%m-%d %H:%M:%S",tz="UTC")
sub1$t_sea_accurate<-difftime(sub1$ret_date_new,sub1$dep_date_time,tz="GMT",units="hours")
sub1$t_sea_accurate<-as.numeric(sub1$t_sea_accurate)
catch_sub2<-rbind(sub1,sub2,sub3)

catch_sub2$diff_hrs_fished_DIN<-(catch_sub2$t_sea_accurate-catch_sub2$hrs_fished_all) #calc diff in time at sea and hrs fished just to check

#quick check of NAs in dataframe
sapply(catch_sub2, function(x) sum(is.na(x)))

#set twilight trip guidelines
twilight_dep<-"17:00:00"
twilight_ret1<-"20:00:00"
twilight_ret2<-"04:00:00"
twilight_dep<-format(as.POSIXct(twilight_dep,format="%H:%M:%S",tz="UTC"))
twilight_ret1<-format(as.POSIXct(twilight_ret1,format="%H:%M:%S",tz="UTC"))
twilight_ret2<-format(as.POSIXct(twilight_ret2,format="%H:%M:%S",tz="UTC"))
twilight_dep_time <- sub(".* ", "", twilight_dep)
twilight_ret_time1 <- sub(".* ", "", twilight_ret1)
twilight_ret_time2 <- sub(".* ", "", twilight_ret2)

#Make new variable of trip_type_DIN
catch_sub2$trip_type_DIN<-NA

#Identify trip type (trip_type_DIN) using new time at sea var (t_sea_accurate)
trip_id_single<-catch_sub2%>%
  mutate(
    trip_type_DIN=ifelse(t_sea_accurate>0&t_sea_accurate<=4&!is.na(hrs_fished_all),"1QUART",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>4&t_sea_accurate<=9&!is.na(hrs_fished_all),"HALF",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>9&t_sea_accurate<=15&!is.na(hrs_fished_all),"FULL",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>15&t_sea_accurate<=30&!is.na(hrs_fished_all),"OVERNIGHT",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>30&t_sea_accurate<=37&!is.na(hrs_fished_all),"DAY_N_HALF",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>37&t_sea_accurate<=42&!is.na(hrs_fished_all),"DAY_N_3QUART",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>42&t_sea_accurate<=50&!is.na(hrs_fished_all),"TWO_DAY",trip_type_DIN),
    trip_type_DIN=ifelse(is.na(t_sea_accurate)|is.na(hrs_fished_all),"UNDEFINED",trip_type_DIN)) #entries that have a NA for departure/return time or have a time that doesn't make sense (e.g.2599, 1780)

trip_id_single$trip_type_DIN<-ifelse(trip_id_single$trip_type_DIN=="1QUART"&as.character(trip_id_single$dep_t)>=twilight_dep_time,"TWILIGHT",trip_id_single$trip_type_DIN)
trip_id_single$trip_type_DIN<-ifelse(trip_id_single$trip_type_DIN=="HALF"&as.character(trip_id_single$dep_t)>=twilight_dep_time,"TWILIGHT_EXTENDED",trip_id_single$trip_type_DIN)

#quick check of NAs in dataframe
sapply(trip_id_single, function(x) sum(is.na(x)))

unique(trip_id_single$trip_type_DIN)

#Check frequency of each trip type
trip_freq <- trip_id_single %>% 
  group_by(trip_type_DIN) %>% summarise(n_trips=n_distinct(trip_id))%>%
  mutate(
    freq=n_trips/n_distinct(trip_id_single$trip_id)
  )

#Omit trips where trip type is undefined
trip_id_single2<- trip_id_single%>%
  subset(trip_type_DIN!="UNDEFINED")%>%
  subset(!is.na(t_sea_accurate))

#define trip event (unique day, block, number of anglers)
trip_id_single2$trip_event_id <- cumsum(!duplicated(trip_id_single2[c("VesselID", "LogDate","dep_t", "ret_t", "HoursFished", "Block", "NumberOfFishers", "n_returntimes","n_blocks", "n_hoursfished", "n_anglers")]))

#find distinct entries by trip_event_id
distinct_id<-trip_id_single2[,c("VesselID","LogDate","LogYear","LogMonth","LogDay","dep_t","ret_t","HoursFished","n_returntimes","n_blocks","n_anglers","n_hoursfished","Block","NumberOfFishers","trip_event_id","trip_id")]
distinct_id<-distinct_id%>%
  distinct()

#calculate anglers hours
distinct_id<-distinct_id%>%
  mutate(
    angler_hrs=HoursFished*NumberOfFishers
  )


#calculate total hours fished per trip (because fishermen enter different hours fished based on blocks,number of anglers, etc.)
distinct_id_summary<-distinct_id%>%
  group_by(trip_id,Block,NumberOfFishers,LogDate)%>% #trip_event
  summarise(
    hrs_fished_trip_blk=sum(HoursFished),
    angler_hrs_trip_blk=sum(angler_hrs)
  )


distinct_id_summary2<-distinct_id%>%
  group_by(trip_id) %>%
  summarise(
    angler_hrs_trip=sum(angler_hrs)
  )

distinct_id_summary<-merge(distinct_id_summary,distinct_id_summary2,by="trip_id")

#merge df with summarized fishing hours with df of distinct entries by trip_event_id
distinct_id<-merge(distinct_id,distinct_id_summary,by=c("trip_id","Block","NumberOfFishers","LogDate"))

trip_id_single3<-merge(trip_id_single2,distinct_id,by=c("VesselID","LogDate","LogYear","LogMonth","LogDay","dep_t","ret_t","HoursFished","trip_id","n_returntimes","n_blocks","n_anglers","n_hoursfished","Block","NumberOfFishers","trip_event_id"))

#Update t_sea_accurate for those with NAs to be consistent with unique LogDates in a given trip (e.g.2_DAY trip equates t_sea_accurate of 48 hrs)
trip_id_single3<-trip_id_single3%>%
  group_by(trip_id)%>%
  mutate(
    t_sea_accurate=ifelse(is.na(t_sea_accurate),24*length(unique(LogDate)),t_sea_accurate)
  )

#Calculate catch of PBF per trip event
trip_id_single3<-trip_id_single3%>%
  group_by(trip_event_id)%>%
  mutate(
    pbf_catch=ifelse(SpeciesCode==4,NumberKept,0)
  )

catch_summary<-trip_id_single3%>%
  group_by(trip_event_id)%>%
  summarise(
    VesselID=unique(VesselID),
    blk=unique(Block),
    n_anglers=max(unique(NumberOfFishers)),
    pbf_catch=sum(pbf_catch),
    trip_type=unique(trip_type_DIN),
    month=min(LogMonth),
    day=min(LogDay),
    year=min(LogYear),
    t_sea=unique(t_sea_accurate),
    angler_hrs_blk=unique(angler_hrs_trip_blk),
    angler_hrs_trip=unique(angler_hrs_trip))

#merge df to summary (which only includes trip_event_id)
catch_summary<-merge(unique(trip_id_single3[,c("trip_id","trip_event_id")]),catch_summary,by="trip_event_id")

catch_summary<-merge(catch_summary,distinct_id,by.x=c("trip_id","trip_event_id","VesselID","n_anglers","angler_hrs_blk","angler_hrs_trip","month","day","year"),by.y=c("trip_id","trip_event_id","VesselID","NumberOfFishers","angler_hrs_trip_blk","angler_hrs_trip","LogMonth","LogDay","LogYear"))

catch_summary <- select(catch_summary,c(trip_id,trip_event_id,trip_type,LogDate,year,month,day,VesselID,blk,n_anglers,angler_hrs_blk,angler_hrs_trip,t_sea,pbf_catch)) #select vars of interest
colnames(catch_summary)[c(1,4,8)] <- c("trip_id","date","vessel_id")

#write catch_summary df to csv file
write.csv(catch_summary,file="./FisheriesData/Clean/single_trips_clean.csv",row.names = F)

