# Preparing sst  data  for GAM modeling 
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(raster);library(ncdf4);library(stringr);library(tidyverse);library(data.table)


#Read in aqua modis and seawifis nc files
#pathfinder_ras<-list.files("./EnvironmentalData/Finescale/SST_COPERNICUS",pattern = ".nc",full.names = T)
copernicus_ras <- list.files("./EnvironmentalData/Finescale/SST_COPERNICUS",pattern = ".nc",full.names = T)
aquamodis_ras<-list.files("./EnvironmentalData/Finescale/SST_AQUAMODIS",pattern = ".nc",full.names = T)

#Read in block location df
blks<-read.csv("./CDFWBlocks/block_locaiton.csv")

#Define names of raster files
#pathfinder_names <- substr(pathfinder_ras,start=57,stop=63)
#pathfinder_names <- sub('\\-', '_', pathfinder_names)

copernicus_names <- str_extract(copernicus_ras,pattern="\\/\\d{1,8}")  #create names for ssh data raster layers based on date
copernicus_names <- substr(copernicus_names,start=2,stop=7)
#aquamodis_ras <- aquamodis_ras[c(6:length(aquamodis_ras))] #index raster to 2003-2022 for ease
aquamodis_names <- substr(aquamodis_ras,start=55,stop=61)

#Prepare sst rasters
sst_dat <- list(copernicus_ras,aquamodis_ras) # combine nc files into a list
varnames <-c("analysed_sst","sst")
sst_stack <- vector(mode="list",length=length(sst_dat))

#Extract ras stacks by block locations
for(i in 1:length(sst_dat)){
  
  sst_stack[[i]] <- raster::stack(sst_dat[[i]],varname=varnames[i]) #combine all rasters (seawifis and aquamodis)
  sst_stack[[i]]<-crop(sst_stack[[i]], extent(-127,-114,20,49)) #crop raster stack to study region
  
}

#Assign names of ras stacks (year_month)
names(sst_stack[[1]]) <- copernicus_names
names(sst_stack[[2]]) <- aquamodis_names

#Create df of raster values associated with block lat/liong
sst_df <- vector(mode="list",length=length(sst_stack))
sst_df_long <- vector(mode="list",length=length(sst_stack))

for(i in 1:length(sst_stack)){
  
  sst_df[[i]]<-data.frame(raster::extract(sst_stack[[i]],blks[,c(1:2)])) #extract ssh data for fishing block
  sst_df[[i]]<-cbind(blks,sst_df[[i]]) #bind block info with ssh data
  sst_df_long[[i]]<-gather(sst_df[[i]],key="date",value="value",-c(Longitude,Latitude,Block)) #change df from wide to long format
  sst_df_long[[i]]$year<-substr(sst_df_long[[i]]$date,start=2,stop=5) #define year var
  sst_df_long[[i]]$month<-substr(sst_df_long[[i]]$date,start=7,stop=8) #define month var
  names(sst_df_long[[i]])[names(sst_df_long[[i]]) == 'value'] <- c("sst") #change "value" var to specific ssh var
  sst_df_long[[i]] <- subset(sst_df_long[[i]],select=-c(date)) #omit date var from dfs
  
  
}

sst_df_long[[1]]$sst <- sst_df_long[[1]]$sst-273.15 #convert sst from Kelvin to degrees celsius


#bind sst by block dfs
sst_df_long <-rbindlist(sst_df_long)

#write df to csv file
write.csv(sst_df_long,file="./EnvironmentalData/Finescale/SST_COMBINED/sst_prepared_data_block.csv",row.names=F)
