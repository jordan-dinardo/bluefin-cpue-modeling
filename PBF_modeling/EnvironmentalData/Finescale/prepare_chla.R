# Preparing chlorphyll-a  data  for GAM modeling 
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(raster);library(ncdf4);library(stringr);library(tidyverse)


#Read in aqua modis and seawifis nc files
seawifis_ras<-list.files("./EnvironmentalData/Finescale/CHL_SEAWIFIS",pattern = ".nc",full.names = T)
aquamodis_ras<-list.files("./EnvironmentalData/Finescale/CHL_AQUAMODIS",pattern = ".nc",full.names = T)

#Read in block location df
blks<-read.csv("./CDFWBlocks/block_locaiton.csv")

#Prepare chla rasters
chla_dat <- list(seawifis_ras,aquamodis_ras) # combine nc files into a list
start_stop <- vector(mode="list",length=length(chla_dat)) 
names <- vector(mode="list",length=length(chla_dat))
start_stop[[1]] <- c(44,59) #define start location for raster file
start_stop[[2]] <- c(45,61) #define stop position for raster file 

# grab names of each raster
for (i in 1:length(chla_dat)){
  for (j in 1:length(chla_dat[[i]])){
    names[[i]][j] <- substr(chla_dat[[i]][j],start=start_stop[[i]][1],stop=start_stop[[i]][2]) # define names of raster based on satellite, year, and month
  }
}
  
year_month_seawifis <- substr(names[[1]],start=10,stop=16) #define year and month only 
year_month_aquamodis <- substr(names[[2]],start=11,stop=17) #define year and month only 
duplicates <- c(year_month_aquamodis,year_month_seawifis)[duplicated(c(year_month_aquamodis,year_month_seawifis))] #find years, months of overlap
names_combined <- c(names[[1]],names[[2]])
names_combined <- c(names[[1]][!year_month_seawifis%in%duplicates],names[[2]]) #during years of overlap, use aqua modis data rather than seawifis
names_combined_seawifis <- paste("./EnvironmentalData/Finescale/CHL_SEAWIFIS/",names_combined,".nc",sep="")
names_combined_aquamodis <- paste("./EnvironmentalData/Finescale/CHL_AQUAMODIS/",names_combined,".nc",sep="")
seawifis_ras_new <- seawifis_ras[seawifis_ras%in%names_combined_seawifis]
aquamodis_ras_new <- aquamodis_ras[aquamodis_ras%in%names_combined_aquamodis]
chla_dat_new <- c(seawifis_ras_new,aquamodis_ras_new)
chla_stack_new <- raster::stack(chla_dat_new,varname="chlor_a") #combine all rasters (seawifis and aquamodis)
names(chla_stack_new) <- paste("chl_",str_sub(gsub("_","",names_combined),start=-6,end=-1),sep="") #define names for ssh data
chla_stack_new<-crop(chla_stack_new, extent(-127,-114,20,49)) #crop raster stack to study region

#Prepare ssh df 
chla_df<-data.frame(raster::extract(chla_stack_new,blks[,c(1:2)])) #extract ssh data for fishing block
chla_df<-cbind(blks,chla_df) #bind block info with ssh data


chla_df_long<-gather(chla_df,key="date",value="value",-c(Longitude,Latitude,Block)) #change df from wide to long format
chla_df_long$year<-substr(chla_df_long$date,start=5,stop=8) #define year var
chla_df_long$month<-substr(chla_df_long$date,start=9,stop=10) #define month var
names(chla_df_long)[names(chla_df_long) == 'value'] <- c("chl") #change "value" var to specific ssh var
chla_df_long <- subset(chla_df_long,select=-c(date)) #omit date var from dfs

#write df to csv file
write.csv(chla_df_long,file="./EnvironmentalData/Finescale/CHL_COMBINED/chla_prepared_data_block.csv",row.names=F)

#####
#####

## required packages ##
library(tiff);library(raster);library(ncdf4);library(stringr);library(tidyr);library(purrr);library(tidyverse)

## preparation of finescale environmental data ##

# load finescale environmental data
ssh<-list.files("./EnvironmentalData/Finescale/SSH_Copernicus",pattern = ".nc",full.names = T)
sst<-list.files("./EnvironmentalData/Finescale/SST_Copernicus",pattern = ".nc",full.names = T)
#chl<-list.files("./EnvironmentalData/Finescale/CHL_Copernicus",pattern = ".nc",full.names = T)

# load block location df
blks<-read.csv("./CDFWBlocks/block_locaiton.csv")

# load region df
regions_dat <- read.csv("./CDFWBlocks/finescale_regions.csv",stringsAsFactors = F)

# create lists to use in data prep
env_data <- list(ssh,sst) #create list off all env data
env_var <- c("ssh","sst") #create string of env var names
env_stringext <- c("\\_\\d{1,8}\\_","\\/\\d{1,8}") #define patterns to extract date from file path
env_ras_varname <-c("adt","analysed_sst")
names <- vector(mode="list",length=length(env_data)) #create list for raster layer names (var_year,month)
env_stack <- vector(mode="list",length=length(env_data)) #create list of raster stacks
env_df <- vector(mode="list",length=length(env_data)) #create list of df for env data
env_df_long <- vector(mode="list",length=length(env_data)) #create list of df for env data in long format

# prepare data
for (i in 1:length(env_data)){
  for (j in 1:length(env_data[[i]])){
    names[[i]][[j]] <- str_extract(env_data[[i]][j],env_stringext[i])  #create names for env data raster layers based on date
    names[[i]][[j]] <- substr(names[[i]][j],start=2,stop=9)
    names[[i]][j]<-paste(env_var[[i]],names[[i]][j],sep="_")
  }
  env_stack[[i]] <- raster::stack(env_data[[i]],varname=env_ras_varname[i]) #stack env raster into RasterStacks by env var
  names(env_stack[[i]])<-names[[i]] #define names for env data
  
}

env_names_start <- c(names[[1]][1],names[[2]][1]) #define first layer name for each env var 
env_names_end <- c(names[[1]][length(names[[1]])],names[[2]][length(names[[2]])]) #define last layer name for each env var

env_stack[[1]] <- projectRaster(env_stack[[1]],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # project crs of ssh stack to match sst and chl stacks

for (i in 1:length(env_data)){
  
  env_stack[[i]]<-crop(env_stack[[i]], extent(-127,-114,20,49)) #crop raster stack to study region
}


names(chla_stack_new) <- paste("chl_",str_sub(gsub("_","",names_combined),start=-6,end=-1),sep="")
env_stack[[3]] <- chla_stack_new

for (i in 1:length(env_stack)){
  
  env_df[[i]]<-data.frame(raster::extract(env_stack[[i]],blks[,c(1:2)])) #extract env data for fishing block
  env_df[[i]]<-cbind(blks,env_df[[i]]) #bind block info with env data
  
}

env_var <- c(env_var,"chl")

for(i in 1:length(env_var)){ #loop over env var
  env_df_long[[i]]<-gather(env_df[[i]],key="date",value="value",-c(Longitude,Latitude,Block)) #change df from wide to long format
  env_df_long[[i]]$year<-substr(env_df_long[[i]]$date,start=5,stop=8) #define year var
  env_df_long[[i]]$month<-substr(env_df_long[[i]]$date,start=9,stop=10) #define month var
  names(env_df_long[[i]])[names(env_df_long[[i]]) == 'value'] <- env_var[i] #change "value" var to specific env var
  env_df_long[[i]] <- subset(env_df_long[[i]],select=-c(date)) #omit date var from dfs
}

env_df_long[[2]]$sst <- env_df_long[[2]]$sst-273.15 #convert sst from Kelvin to degrees celsius

env_df_all <- env_df_long%>%reduce(dplyr::full_join,by=c("Longitude","Latitude","Block","year","month")) #merge all env df
env_df_all <- env_df_all[,c("Longitude","Latitude","Block","year","month","ssh","sst","chl")] #reorder columns 

env_df_all <- dplyr::select(env_df_all,!c(Longitude,Latitude)) #remove long and lat data
env_df_all$year <- as.numeric(env_df_all$year)
env_df_all$month <- as.numeric(env_df_all$month)

env_df_all_summary <- env_df_all%>% #summarise over unique year, month, and block
  subset(Block!=999)%>%
  group_by(year,month,Block)%>%
  summarise(
    ssh=mean(ssh,na.rm=T),
    sst=mean(sst,na.rm=T),
    chla=mean(chl,na.rm=T),
  )


write.csv(env_df_all_summary,file="./EnvironmentalData/Finescale/envdata_finescale_block.csv",row.names=F)


