# Extract lat/long info from CDFW fishing blocks (midpoint)
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file


library(rgdal)

#read in map of blocks
blks<-readOGR('./CDFWBlocks/fishingblocks_shp/union_fishing_blocks_all.shp')

#convert projection to WGS84
blks<-spTransform(blks,CRS("+proj=longlat +datum=WGS84"))

#convert spatial dataframe to regular dataframe with midpoint coordinates
blks.df<-data.frame(x=coordinates(blks)[,1], y=coordinates(blks)[,2], blks@data)
blks.df<-blks.df%>%
  dplyr::select(x,y,BLOCK10_ID)
colnames(blks.df)<-c("Longitude","Latitude","Block")

#For those blocks that have multiple values (idk why this happens) take the average lat and long
blks.df <- blks.df%>%
  group_by(Block)%>%
  mutate(
    Longitude=mean(Longitude,na.rm=T),
    Latitude=mean(Latitude,na.rm=T)
  )

blks.df <- blks.df%>%
  subset(Block!=999)%>%
  distinct()

#write blocks df to csv file
write.csv(blks.df,file="./CDFWBlocks/block_locaiton.csv",row.names = F)


