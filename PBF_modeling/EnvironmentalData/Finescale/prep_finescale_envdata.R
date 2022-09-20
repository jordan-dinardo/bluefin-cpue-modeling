# Preparing finescale environmental data (sea surface height, sea surface temperature, 
  # and chlorophyll-a) for GAM modeling 
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tiff);library(raster);library(ncdf4);library(stringr);library(tidyr);library(purrr);library(tidyverse)

## preparation of finescale environmental data ##

  # load finescale environmental data
ssh<-list.files("./EnvironmentalData/Finescale/SSH_Copernicus",pattern = ".nc",full.names = T)
sst<-list.files("./EnvironmentalData/Finescale/SST_Copernicus",pattern = ".nc",full.names = T)
chl<-list.files("./EnvironmentalData/Finescale/CHL_Copernicus",pattern = ".nc",full.names = T)

  # load block location df
blks<-read.csv("./CDFWBlocks/block_locaiton.csv")

  # create lists to use in data prep
env_data <- list(ssh,sst,chl) #create list off all env data
env_var <- c("ssh","sst","chla") #create string of env var names
env_stringext <- c("\\_\\d{1,8}\\_","\\/\\d{1,8}","\\-\\d{1,8}\\-") #define patterns to extract date from file path
env_ras_varname <-c("adt","analysed_sst","chlor_a")
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

env_names_start <- c(names[[1]][1],names[[2]][1],names[[3]][1]) #define first layer name for each env var 
env_names_end <- c(names[[1]][length(names[[1]])],names[[2]][length(names[[2]])],names[[3]][length(names[[3]])]) #define last layer name for each env var

env_stack[[1]] <- projectRaster(env_stack[[1]],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # project crs of ssh stack to match sst and chl stacks

for (i in 1:length(env_data)){
  
  extent(env_stack[[i]])<-c(-127,-114,20,49) #define extent of raster stacks

  env_df[[i]]<-data.frame(raster::extract(env_stack[[i]],blks[,c(1:2)])) #extract env data for fishing block
  env_df[[i]]<-cbind(blks,env_df[[i]]) #bind block info with env data
  
}
  
for(i in 1:length(env_data)){ #loop over env var
  env_df_long[[i]]<-gather(env_df[[i]],key="date",value="value",-c(Longitude,Latitude,Block)) #change df from wide to long format
  env_df_long[[i]]$year<-substr(env_df_long[[i]]$date,start=5,stop=8) #define year var
  env_df_long[[i]]$month<-substr(env_df_long[[i]]$date,start=9,stop=10) #define month var
  names(env_df_long[[i]])[names(env_df_long[[i]]) == 'value'] <- env_var[i] #change "value" var to specific env var
  env_df_long[[i]] <- subset(env_df_long[[i]],select=-c(date)) #omit date var from dfs
}

env_df_long[[2]]$sst <- env_df_long[[2]]$sst-273.15 #convert sst from Kelvin to degrees celsius

env_df_all <- env_df_long%>%reduce(dplyr::full_join,by=c("Longitude","Latitude","Block","year","month")) #merge all env df
env_df_all <- env_df_all[,c("Longitude","Latitude","Block","year","month","ssh","sst","chla")] #reorder columns 

env_df_all <- dplyr::select(env_df_all,!c(Longitude,Latitude)) #remove long and lat data
env_df_all$year <- as.numeric(env_df_all$year)
env_df_all$month <- as.numeric(env_df_all$month)

env_df_all_summary <- env_df_all%>% #summarise over unique year, month, and block
  subset(Block!=999)%>%
  group_by(year,month,Block)%>%
  summarise(
    ssh=mean(ssh,na.rm=T),
    sst=mean(sst,na.rm=T),
    chla=mean(chla,na.rm=T),
  )

## finescale environmental data output ##
write.csv(env_df_all_summary,file="./EnvironmentalData/Finescale/envdata_finescale.csv",row.names=F)


