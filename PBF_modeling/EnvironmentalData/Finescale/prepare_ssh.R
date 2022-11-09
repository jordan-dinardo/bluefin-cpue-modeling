# Standardization of PBFT CPUE using delta-GAMM (positive cpue models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(raster);library(ncdf4);library(stringr);library(tidyverse)

#Read in and ssh nc files
ssh_ras<-list.files("./EnvironmentalData/Finescale/SSH_COPERNICUS_NEW",pattern = ".nc",full.names = T)

#Read in block location df
blks<-read.csv("./CDFWBlocks/block_locaiton.csv")

#Prepare ssh rasters
names <- str_extract(ssh_ras,pattern="\\_\\d{1,8}\\_")  #create names for ssh data raster layers based on date
names <- substr(names,start=2,stop=9)
ssh_stack <- raster::stack(ssh_ras,varname='adt') #stack ssh rasters into RasterStacks 
names(ssh_stack)<-names #define names for ssh data
ssh_stack <- projectRaster(ssh_stack,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # project crs of ssh stack 
ssh_stack<-crop(ssh_stack, extent(-127,-114,20,49)) #crop raster stack to study region
  
#Prepare ssh df 
ssh_df<-data.frame(raster::extract(ssh_stack,blks[,c(1:2)])) #extract ssh data for fishing block
ssh_df<-cbind(blks,ssh_df) #bind block info with ssh data


ssh_df_long<-gather(ssh_df,key="date",value="value",-c(Longitude,Latitude,Block)) #change df from wide to long format
ssh_df_long$year<-substr(ssh_df_long$date,start=2,stop=5) #define year var
ssh_df_long$month<-substr(ssh_df_long$date,start=6,stop=7) #define month var
names(ssh_df_long)[names(ssh_df_long) == 'value'] <- c("ssh") #change "value" var to specific ssh var
ssh_df_long <- subset(ssh_df_long,select=-c(date)) #omit date var from dfs

#write df to csv file
write.csv(ssh_df_long,file="./EnvironmentalData/Finescale/SSH_COPERNICUS_NEW/ssh_prepared_data_block.csv",row.names=F)






