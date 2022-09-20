# Identification of unique trip events in CPFV fisheries data (multi-day trips as identified by CDFW)
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(purrr);library(lubridate);library(stringr);library(hms)

## Clean catch and effort data ##

#load raw fisheries CPFV catch and effort data 

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

catch_data<-subset(catch_data,TripType=="Multi-Day") #subset to data to multi-day trip type (as deined by CDFW)

#format Departure/Return times to be consistent in the form HH:MM:SS (to be converted to Date variable)
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

#convert dep_t and ret_t to date objects using POSIXct
catch_data$dep_t <- as_hms(format(catch_data$dep_date_t, "%H:%M:%S"))
catch_data$ret_t <- as_hms(format(catch_data$ret_date_t, "%H:%M:%S"))

#arrange data by vessel id and departure date/time
catch_data<-catch_data%>%
  arrange(VesselID,LogDate,dep_date_t)

#identify unique trips based on LogDate using lag of consecutive days
catch_data$day_diff<-c(NA,diff(catch_data$LogDate))
catch_data$trip_split<-0
catch_data[which(catch_data$day_diff>1|catch_data$day_diff<0),"trip_split"]<-1
catch_data$trip_id<-cumsum(catch_data$trip_split)

#subset vars to identify unique trips for ease/efficiency
catch_sub1<-data.frame(catch_data$VesselID,catch_data$LogDate,catch_data$trip_id,catch_data$dep_t,catch_data$ret_t,catch_data$dep_date_t,catch_data$ret_date_t,catch_data$HoursFished,catch_data$Block,catch_data$DepartureTime,catch_data$ReturnTime,catch_data$day_diff)
colnames(catch_sub1)<-c("VesselID","LogDate","TripID","dep_t","ret_t","dep_date_t","ret_date_t","HoursFished","Block","dep_t_int","ret_t_int","day_diff")

#Check Trip ID (time diff (return time-departure time of following day; count number of number of vessels)
catch_sub2<-catch_sub1%>%  
  group_by(TripID)%>%
  mutate(time_diff = lag(lead(dep_t_int) - ret_t_int),
         n_vessel=n_distinct(VesselID))

summary(catch_sub2)
#noticed that two trips (by different vessels) were being grouped into one trip (rare event)

#fix those extremely rare events where two trips where inappropriately identified as a single trip
catch_sub2$TripID<-ifelse(catch_sub2$VesselID==32639&catch_sub2$TripID==11507,19971,catch_sub2$TripID)

#Check Trip ID (count number of number of vessels, number of days (by LogDate), and number of days by LogDate,dep_t, and ret_t)
catch_sub2<-catch_sub2%>%
  group_by(TripID)%>%
  mutate(
    n_vessel=n_distinct(VesselID),
    n_dates=n_distinct(LogDate),
    n_date_time=n_distinct(LogDate,dep_t,ret_t)
  )

summary(catch_sub2)

#identify unique trip (revised based on time difference between return time and leading departure time (following day))
catch_sub2<-catch_sub2%>%
  group_by(TripID)%>%
  mutate(
    trip_split=ifelse(time_diff==0,0,1)
  )

#edit trip split variable to revise Trip ID 
catch_sub2$trip_split<-ifelse(catch_sub2$trip_split==1&catch_sub2$day_diff==0,0,catch_sub2$trip_split) 
catch_sub2$trip_split<-ifelse(is.na(catch_sub2$trip_split),1,catch_sub2$trip_split) #set NAs to 1; NAs tend to represent a new vessel and thus a trip split
catch_sub2$trip_split<-ifelse(catch_sub2$trip_split==0&catch_sub2$day_diff==0&catch_sub2$time_diff!=0&catch_sub2$n_dates!=catch_sub2$n_date_time,1,catch_sub2$trip_split)

#create new trip ID (revised from original)
catch_sub2$new_tripid<-cumsum(catch_sub2$trip_split)

#Manually change Tripid where (same) two vessels are beign lumped together
catch_sub2<-catch_sub2%>%
  group_by(new_tripid)%>%
  mutate(
    n_vessel=n_distinct(VesselID)
  )

catch_sub2$new_tripid<-ifelse(catch_sub2$VesselID==32639&catch_sub2$new_tripid==18750,max(catch_sub2$new_tripid)+1,catch_sub2$new_tripid)

#count number of days (continual thread of return and departure times (return-leading departure==0)) within each trip
catch_sub2<-catch_sub2%>%
  group_by(new_tripid,LogDate,dep_t,ret_t)%>%
  mutate(
    sum_0=n_distinct(LogDate[time_diff==0],na.rm = T),
    n_duplicates=n(),
    prop=sum_0/n_duplicates)

catch_sub2<-catch_sub2%>%
  group_by(new_tripid)%>%
  mutate(sum_0_trips=sum(prop), #count number of return-departure==0 per trip
         n_dates=n_distinct(LogDate), #count number of days per trip
         n_date_time=n_distinct(LogDate,dep_t,ret_t)) #count number of unique day and times per trip

catch_sub2$tripcheck<-catch_sub2$n_dates-catch_sub2$n_date_time
catch_sub2$id_check_date<-catch_sub2$n_dates-catch_sub2$sum_0_trips

#Identify unique trip type based on number of days at sea
catch_sub2$trip_type_DIN<-paste0(catch_sub2$n_dates,"_DAY")

unique(catch_sub2$trip_type_DIN)

#check number number of unique trip types per trip (Should be 1) and check Blocks 
test_id_check<-catch_sub2%>%
  group_by(new_tripid)%>%
  summarise(
    n_trip_type=length(unique(trip_type_DIN)),
    n_blocks_trip=length(unique(Block)),
    n_vessel_new=n_distinct(VesselID))

test_id_check2<-catch_sub2%>%
  group_by(new_tripid,LogDate)%>%
  summarise(
    n_blocks_trip_date=length(unique(Block)),
    n_hours_trip_date=length(unique(HoursFished))
  )

#NOTES: if there are more than one unique hours fished per tripid and date it's not an issue when it comes to id of trip type 
##because that is dependent on number of consecutive days, It will however matter for 1_DAY trips

#Identify trip defined as "1-DAY" to level of single day trips (1QUART, HALF, TWILIGHT, etc.)

#subset "1_DAY" trips
one_day<-subset(catch_sub2,trip_type_DIN=="1_DAY")

#arrange data by VesselID, LogDate, and Departure Time
one_day<-one_day%>%
  arrange(VesselID,LogDate,dep_t)

#identify unique trips
one_day$day_diff<-c(NA,diff(one_day$LogDate))
one_day$trip_split_date<-0

one_day[which(one_day$day_diff>=1|one_day$day_diff<0),"trip_split_date"]<-1

one_day$trip_id_date<-cumsum(one_day$trip_split_date)

one_day$diff_dep<-c(NA,diff(one_day$dep_t))
one_day$diff_ret<-c(NA,diff(one_day$ret_t))

one_day<-one_day%>%  
  group_by(trip_id_date)%>%
  mutate(n_vessel_bydate=n_distinct(VesselID))

summary(one_day)

one_day<-one_day%>%
  mutate(
    trip_split_new=ifelse(trip_split_date==0&trip_split==1&diff_dep==0&diff_ret==0,0,trip_split))
one_day$trip_split_new<-ifelse(is.na(one_day$trip_split_new),0,one_day$trip_split_new)

one_day$trip_id_new2<-cumsum(one_day$trip_split_new)

#add max number of identified trips (new_tripid) to trip_id_new2 so ensure no repeat in tripID across columns (need for when combining non-1_day trips and 1-day trips)
one_day$trip_id_new2<-one_day$trip_id_new2+(max(catch_sub2$new_tripid)+1)

#count number of days (continual thread of return and departure times (return-leading departure==0)) within each trip
one_day<-one_day%>%
  group_by(trip_id_new2)%>%
  mutate(
    sum_0_date=n_distinct(LogDate,na.rm = T),
    n_duplicates_date=n(),
    prop_date=sum_0_date/n_duplicates_date)

one_day<-one_day%>%
  group_by(trip_id_new2)%>%
  mutate(sum_0_trips_date=sum(prop_date), #count number of return-departure==0 per trip
         n_dates_date=n_distinct(LogDate), #count number of days per trip
         n_date_time_date=n_distinct(LogDate,dep_t,ret_t)) #count number of unique day and times per trip

#Identify unique trip type based on number of days at sea
one_day$trip_type_DIN<-paste0(one_day$n_dates_date,"_DAY")

unique(one_day$trip_type_DIN) #check to ensure all come out to be 1_DAY

#calculate hours fished overall
#find distinct entries by ID
one_day_distinct<-one_day[,c("VesselID","LogDate","dep_t","ret_t","HoursFished","trip_id_new2","Block")]

one_day_distinct<-one_day_distinct%>%
  distinct()


one_day_distinct<-one_day_distinct%>%
  group_by(trip_id_new2)%>%
  mutate(
    hrs_fished_all=sum(HoursFished)
  )

one_day<-merge(one_day,one_day_distinct,by=c("VesselID","LogDate","dep_t","ret_t","HoursFished","trip_id_new2","Block"))


#Calculate number of hours at sea (difference between Return and Departure Date/Times) 
one_day$t_sea<-difftime(one_day$ret_date_t,one_day$dep_date_t,tz="GMT",units="hours")
one_day$t_sea<-as.numeric(one_day$t_sea)
one_day$ret_date_DIN<-as.Date(one_day$ret_date_t)

#check to see how many unique time at sea entries are included in each trip (should be 1)
one_day<-one_day%>%
  group_by(trip_id_new2)%>%
  mutate(
    n_t_sea=n_distinct(t_sea)
  )%>%
  ungroup()

#Don't know how to make sense of trips where n_t_sea==2 so omit from analysis

######
sub_change<-subset(one_day,t_sea<=0&!is.na(t_sea)&!is.na(hrs_fished_all)) #entries where a vessel is returning at an earlier time than departure
sub_change1<-subset(one_day,!is.na(hrs_fished_all)&!is.na(t_sea)) 
sub_change1<-subset(sub_change1,t_sea>0&(hrs_fished_all-abs(t_sea))>2) #entries where a vessel is returning at a later time than departure time  and anglers are fising for more hours than time at sea (add a day to return date)
sub_keep1<-subset(one_day,!is.na(hrs_fished_all)&!is.na(t_sea))
sub_keep1<-subset(sub_keep1,t_sea>0&(hrs_fished_all-abs(t_sea))<=2) #entries where a vessel is returning at a later time than departure time and anglers are fishing for less time than time at sea (add a day to return date)
sub_keep2<-subset(one_day,is.na(hrs_fished_all)|is.na(t_sea)) #entries where Hours fished or/and Time at sea is NA

#create new return date based on t_sea and hours fished; if time at sea and hours fished indicate vessel 
#returned the following day of departure change return date to match (ret_date_new)
sub_change$ret_date_new<-sub_change$ret_date_DIN%m+%days(1) #add a day to return date
sub_change1$ret_date_new<-sub_change1$ret_date_DIN%m+%days(1) #add a day to return date
sub_keep1$ret_date_new<-sub_keep1$ret_date_DIN #keep return date consistent
sub_keep2$ret_date_new<-sub_keep2$ret_date_DIN #keep return date consistent
one_day<-rbind(sub_change,sub_change1,sub_keep1,sub_keep2)

#format and convert new return date to a Date object
one_day$ret_date_new<-paste(one_day$ret_date_new,one_day$ret_t)
one_day$ret_date_new<-format(as.POSIXct(one_day$ret_date_new,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

#Calculate more accurate time at sea (with updated return dates)
one_day$t_sea_accurate<-difftime(one_day$ret_date_new,one_day$dep_date_t,tz="GMT",units="hours")


#quick check of NAs in dataframe
sapply(one_day, function(x) sum(is.na(x)))

#change data variable to numeric types to compare correctly
one_day$t_sea_accurate<-as.numeric(one_day$t_sea_accurate)

#calculate difference in hours fished and the updated ("more accurate") time at sea metric
one_day$diff_hrs_fished_DIN<-one_day$hrs_fished_all-one_day$t_sea_accurate 
##noticed there are some entries where difference is still negative, which means that we need to add another day to those entries.

sub1<-subset(one_day,!is.na(hrs_fished_all)&!is.na(t_sea)&!is.na(t_sea_accurate))
sub1<-subset(sub1,(hrs_fished_all-t_sea_accurate)>2)
sub2<-subset(one_day,!is.na(hrs_fished_all)&!is.na(t_sea)&!is.na(t_sea_accurate))
sub2<-subset(sub2,(hrs_fished_all-t_sea_accurate)<=2)
sub3<-subset(one_day,is.na(hrs_fished_all)|is.na(t_sea)|is.na(t_sea_accurate))


sub1$ret_date_new<-as.Date(sub1$ret_date_DIN)%m+%days(2) #add two days to entries

#create new return date (format and convert to Date object)
sub1$ret_date_new<-paste(sub1$ret_date_new,sub1$ret_t)
sub1$ret_date_new<-format(as.POSIXct(sub1$ret_date_new,format="%Y-%m-%d %H:%M:%S",tz="UTC"))
sub1$t_sea_accurate<-difftime(sub1$ret_date_new,sub1$dep_date_t,tz="GMT",units="hours")
sub1$t_sea_accurate<-as.numeric(sub1$t_sea_accurate)
one_day<-rbind(sub1,sub2,sub3)

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
one_day$trip_type_DIN<-NA

#Identify trip type using Time at sea (accurate)
trip_id_one_day<-one_day%>%
  mutate(
    trip_type_DIN=ifelse(t_sea_accurate>0&t_sea_accurate<=4,"1QUART",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>4&t_sea_accurate<=9,"HALF",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>9&t_sea_accurate<=15,"FULL",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>15&t_sea_accurate<=30,"OVERNIGHT",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>30&t_sea_accurate<=37,"DAY_N_HALF",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>37&t_sea_accurate<=45,"DAY_N_3QUART",trip_type_DIN),
    trip_type_DIN=ifelse(t_sea_accurate>45&t_sea_accurate<=51,"2_DAY",trip_type_DIN),
    trip_type_DIN=ifelse(is.na(t_sea_accurate)|is.na(hrs_fished_all),"UNDEFINED",trip_type_DIN)) #entries that have a NA for departure/return time or have a time that doesn't make sense (e.g.2599, 1780)

trip_id_one_day$trip_type_DIN<-ifelse(trip_id_one_day$trip_type_DIN=="1QUART"&as.character(trip_id_one_day$dep_t)>=twilight_dep_time,"TWILIGHT",trip_id_one_day$trip_type_DIN)
trip_id_one_day$trip_type_DIN<-ifelse(trip_id_one_day$trip_type_DIN=="HALF"&as.character(trip_id_one_day$dep_t)>=twilight_dep_time,"TWILIGHT_EXTENDED",trip_id_one_day$trip_type_DIN)

#quick check of NAs in dataframe
sapply(trip_id_one_day, function(x) sum(is.na(x)))

unique(trip_id_one_day$trip_type_DIN)

#check identification of trip type; note that there are entries where trip was identified as two different trip types
check_id_one_day<-trip_id_one_day%>%
  group_by(trip_id_new2)%>%
  summarise(
    n_trip_type=length(unique(trip_type_DIN)),
    n_blocks_trip=length(unique(Block)))

summary(check_id_one_day)

long_range<-subset(catch_sub2,trip_type_DIN!="1_DAY")
long_range$t_sea<-NA
long_range$t_sea_accurate<-NA
long_range$ret_date_new<-NA
long_range$diff_hrs_fished_DIN<-NA
long_range$ret_date_DIN<-NA
long_range$n_t_sea<-NA
long_range$hrs_fished_all<-NA

trip_id_multi<-rbind(long_range,trip_id_one_day)

unique(trip_id_multi$trip_type_DIN)

#quick check of NAs in dataframe
sapply(trip_id_multi, function(x) sum(is.na(x)))

#merge trip id into trip_id_new2
trip_id_multi$trip_id_new2<-ifelse(is.na(trip_id_multi$trip_id_new2),trip_id_multi$new_tripid,trip_id_multi$trip_id_new2)

#Check frequency of each trip type
trip_freq <- trip_id_multi %>% 
  group_by(trip_type_DIN) %>% 
  summarise(n_trips=n_distinct(trip_id_new2))%>%
  mutate(
    freq=n_trips/n_distinct(trip_id_multi$trip_id_new2)
  )

trip_id_multi <- trip_id_multi%>%
  ungroup()

###

#column bind original multi-day df (catch_data) and trip_id_multi (only columns of interest)
catch_sub3<-subset(catch_data,select=c(LogMonth,LogDay,LogYear,LogDate,VesselID,TripType,Block,DepartureTime,ReturnTime,HoursFished,NumberOfFishers,SpeciesCode,Species,NumberKept,NumberReleased,dep_t,ret_t,dep_date_t,ret_date_t,trip_id))
trip_id_multi_sub1<-subset(trip_id_multi,select=c(VesselID,LogDate,dep_t:ret_t_int,trip_type_DIN,t_sea_accurate,trip_id_new2,hrs_fished_all,n_t_sea))

catch_sub3<-catch_sub3%>%
  ungroup()%>%
  arrange(VesselID,LogDate,DepartureTime)


trip_id_multi_sub1<-trip_id_multi_sub1%>%
  ungroup()%>%
  arrange(VesselID,LogDate,dep_t)

colnames(trip_id_multi_sub1) <- paste("merge", colnames(trip_id_multi_sub1), sep = "_")

trip_id_multi_merge<-cbind(trip_id_multi_sub1,catch_sub3)

#Omit trips where trip type is undefined
trip_id_multi_merge<- trip_id_multi_merge%>%
  subset(merge_trip_type_DIN!="UNDEFINED")%>%
  subset(merge_n_t_sea!=2|is.na(merge_t_sea_accurate))


#calculate number of unique return times and blocks per trip and hours fished per trip
trip_id_multi_merge2<-trip_id_multi_merge%>%
  group_by(merge_trip_id_new2)%>%
  mutate(
    n_returntimes=length(unique(merge_ret_t)),
    n_blocks=length(unique(Block)),
    n_hoursfished=length(unique(merge_HoursFished)),
    n_anglers=length(unique(NumberOfFishers))
    
  )

#define trip event (unique day, block, number of anglers)
trip_id_multi_merge2$trip_event_id <- cumsum(!duplicated(trip_id_multi_merge2[c("VesselID", "LogDate","merge_dep_t", "merge_ret_t", "merge_HoursFished", "Block", "merge_trip_id_new2", "NumberOfFishers", "n_returntimes","n_blocks", "n_hoursfished", "n_anglers")]))

#find distinct entries by trip_event_id
distinct_id<-trip_id_multi_merge2[,c("VesselID","LogDate","merge_dep_t","merge_ret_t","merge_HoursFished","merge_trip_id_new2","n_returntimes","n_blocks","n_anglers","n_hoursfished","Block","NumberOfFishers","trip_event_id")]
distinct_id<-distinct_id%>%
  distinct()

#calculate anglers hours
distinct_id<-distinct_id%>%
  mutate(
    angler_hrs=merge_HoursFished*NumberOfFishers
  )


#calculate total hours fished per trip (because fishermen enter different hours fished based on blocks,number of anglers, etc.)
distinct_id_summary<-distinct_id%>%
  group_by(merge_trip_id_new2,Block,NumberOfFishers,LogDate)%>% #tripevent; done to test work thus far..all good
  summarise(
    hrs_fished_trip_blk=sum(merge_HoursFished),
    angler_hrs_trip_blk=sum(angler_hrs)
  )


distinct_id_summary2<-distinct_id%>%
  group_by(merge_trip_id_new2) %>%
  summarise(
    angler_hrs_trip=sum(angler_hrs)
  )

distinct_id_summary<-merge(distinct_id_summary,distinct_id_summary2,by="merge_trip_id_new2")

#merge df with summarized fishing hours with df of distinct entries by trip_event_id
distinct_id<-merge(distinct_id,distinct_id_summary,by=c("merge_trip_id_new2","Block","NumberOfFishers","LogDate"))

trip_id_multi_merge3<-merge(trip_id_multi_merge2,distinct_id,by=c("VesselID","LogDate","merge_dep_t","merge_ret_t","merge_HoursFished","merge_trip_id_new2","n_returntimes","n_blocks","n_anglers","n_hoursfished","Block","NumberOfFishers","trip_event_id"))

#Update t_sea_accurate for those with NAs to be consistent with unique LogDates in a given trip (e.g.2_DAY trip equates t_sea_accurate of 48 hrs)
trip_id_multi_merge3<-trip_id_multi_merge3%>%
  group_by(merge_trip_id_new2)%>%
  mutate(
    merge_t_sea_accurate=ifelse(is.na(merge_t_sea_accurate),24*length(unique(LogDate)),merge_t_sea_accurate)
  )

#Calculate catch of PBF per trip event
trip_id_multi_merge3<-trip_id_multi_merge3%>%
  group_by(trip_event_id)%>%
  mutate(
    pbf_catch=ifelse(SpeciesCode==4,NumberKept,0)
  )

catch_summary<-trip_id_multi_merge3%>%
  group_by(trip_event_id)%>%
  summarise(
    VesselID=unique(VesselID),
    blk=unique(Block),
    n_anglers=max(unique(NumberOfFishers)),
    pbf_catch=sum(pbf_catch),
    trip_type=unique(merge_trip_type_DIN),
    month=min(LogMonth),
    day=min(LogDay),
    year=min(LogYear),
    t_sea=unique(merge_t_sea_accurate),
    angler_hrs_blk=unique(angler_hrs_trip_blk),
    angler_hrs_trip=unique(angler_hrs_trip))


#merge df to summary (which only includes trip_event_id)
catch_summary<-merge(unique(trip_id_multi_merge3[,c("merge_trip_id_new2","trip_event_id")]),catch_summary,by="trip_event_id")

catch_summary<-merge(catch_summary,distinct_id,by.x=c("merge_trip_id_new2","trip_event_id","VesselID","n_anglers","angler_hrs_blk","angler_hrs_trip"),by.y=c("merge_trip_id_new2","trip_event_id","VesselID","NumberOfFishers","angler_hrs_trip_blk","angler_hrs_trip"))

catch_summary <- select(catch_summary,c(merge_trip_id_new2,trip_event_id,trip_type,LogDate,year,month,day,VesselID,blk,n_anglers,angler_hrs_blk,angler_hrs_trip,t_sea,pbf_catch)) #select vars of interest
colnames(catch_summary)[c(1,4,8)] <- c("trip_id","date","vessel_id")

#write catch_summary df to csv file
write.csv(catch_summary,file="./FisheriesData/Clean/multi_trips_clean.csv",row.names = F)
