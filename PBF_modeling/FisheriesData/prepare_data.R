# Prepare CPFV catch data for modeling phase
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse)

#Read in clean_trips_combined_nonconfidential.csv
catch_dat <- read.csv("./FisheriesData/Clean/Combined/clean_trips_combined.csv",stringsAsFactors = F)
catch_dat$year_month_blk <- paste(catch_dat$year,catch_dat$month,catch_dat$blk,sep="_")

#Read in environmental data (broadscale and finescale)
env_dat_broad <- read.csv("./EnvironmentalData/Broadscale/envdata_broadscale.csv",stringsAsFactors = F)
# Still need to prepare finescale environmental data
#env_dat_fine <- read.csv("./EnvironmentalData/Finescale/envdata_finescale_block.csv",stringsAsFactors = )
#env_dat_fine$year_month_blk <- paste(env_dat_fine$year,env_dat_fine$month,env_dat_fine$Block,sep="_")

#Read in blk information
blk_dat <- read.csv("./CDFWBlocks/block_locaiton.csv",stringsAsFactors = F)

#Read in blk regions information
regions_dat <- read.csv("./CDFWBlocks/finescale_regions.csv",stringsAsFactors = F)

#Merge environmental data to catch data
catch_dat <- merge(catch_dat,env_dat_broad,by=c("year","month")) #broadscale env data (merge by year and month)
catch_dat<- merge(catch_dat,env_dat_fine,by.x=c("year","month","blk","year_month_blk"),by.y=c("year","month","Block","year_month_blk")) #finescale env data (merge by year, month, and block)

#Merge blk info to catch data
catch_dat <- merge(catch_dat,blk_dat,by.x="blk", by.y= "Block")

#Prep and merge blk regions data 

  #pivot dataframe to long format
regions_dat <- pivot_longer(regions_dat,cols=Mainland_Nearshore_North:Offshore_South,names_to="region",values_to = "blk")
  #merge dfs
catch_dat <- merge(catch_dat,regions_dat,by="blk")

#change certain blocks to Mexico based on block number (not in finescale_regions.csv file)
catch_dat$region <- ifelse(catch_dat$blk>900&catch_dat$blk!=950,"Mexico",catch_dat$region)

#Calculate nominal CPUE and presence per trip event
catch_dat <- catch_dat%>%
  mutate(
    nominal_cpue=(pbf_catch/angler_hrs_blk), #nominal CPUE (individuals/angler hours)
    pbf_presence=ifelse(pbf_catch==0,0,1) # presence
  )

write.csv(catch_dat,"./FisheriesData/Prepared/pbf_nominal_cpue_data.csv")

