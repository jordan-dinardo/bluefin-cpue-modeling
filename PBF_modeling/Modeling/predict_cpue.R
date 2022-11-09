# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(ggplot2);library(mgcv);library(tidymv);library(rgdal);library(raster);library(RColorBrewer);library(rcartocolor)


# read binary and positive models 
load("./Modeling/model_outputs/binary_model_fits.RData")
load("./Modeling/model_outputs/positive_model_fits.RData")

n_models <- 6 #define number of models 

# identify and index best model (for binary and positive separately)

best_binary <-vector(length=n_models)
best_pos <-vector(length=n_models)

for (i in 1:n_models){
  best_binary[i] <- ifelse(fits_binary[[i]]$aic==min(model_aic_binary),T,F) #identify best binary model based on AIC
  best_pos[i] <- ifelse(fits_pos[[i]]$aic==min(model_aic_pos),T,F) #identify best positive model based on AIC
}

best_binary <- fits_binary[best_binary] #index best binary model
best_pos <- fits_pos[best_pos] #index best positive model

#Read in prepared data
dat <- read.csv("./FisheriesData/Prepared/pbf_nominal_cpue_data_block.csv")
dat <- na.omit(dat)

#change class of var for modeling 
var.factor<-c("trip_type","trip_id","new_vessel_id","year_month_blk","trip_event_id","block")
var.numeric<-c("year","month","n_anglers","angler_hrs_trip","angler_hrs_blk","nominal_cpue","pbf_presence","pdo","enso","long","lat")

dat[,var.factor] <- lapply(dat[,var.factor],as.factor)
dat[,var.numeric] <- lapply(dat[,var.numeric],as.numeric)  

#subset data where pbf is present
dat_pos <- subset(dat,pbf_presence>0)

# Predict binary and positive CPUE using best models 
dat$pred_binary <- predict.gam(best_binary,type="response")
dat_pos$pred_pos <- predict.gam(best_pos,type="response")

# Merge dataframes to combine standardized CPUE from binary and postive GAMs 
dat_combine <- merge(dat,dat_pos)

dat_combine$pred_pos <- exp(dat_combine$pred_pos)

#combine predictions of binary and positive GAMs by multiplying 
dat_combine$pred <- dat_combine$pred_binary*dat_combine$pred_pos

#Prepare data to visualize timeseries of PBF CPUE (annual mediam CPUE with 95% quantile range)
pred_summary <- dat_combine%>%
  group_by(year)%>%
  summarize(
    median_cpue = median(pred,na.rm = T), #calculate annual median CPUE
    q_025 = quantile(pred, probs=0.025), #calculate .025% quantile 
    q_975 = quantile (pred, probs=0.975) #calculate 0.957% quantile
  )

#Visualize CPUE timeseries (annual mediam CPUE with 95% quantile range)
#set text format
text_format=element_text(face="bold",size=20,color="black")

#write timesereis to pdf file
pdf("./Modeling/model_outputs/cpue_timeseries.pdf",height=5,width=9)
ggplot(data=pred_summary)+
  geom_point(aes(x=year,y=median_cpue))+
  geom_path(aes(x=year,y=median_cpue,group=1))+
  geom_ribbon(aes(x=year,ymin=q_025,ymax=q_975),alpha=0.4)+
  theme_classic()+
  xlab("Year")+
  ylab("Median CPUE")+
  scale_x_continuous(breaks = seq(from = 1995, to = max(dat_combine$year), by = 2),guide = guide_axis(n.dodge=2))+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))
dev.off()

#Prepare data to visualize annual CPUE in space (annual mediam CPUE and 95% quantile range)
pred_list <- vector(mode="list",length=length(unique(dat_combine$year))) #create list for annual predictions (dfs)
sp_pred_list <- vector(mode="list",length=length(unique(dat_combine$year))) #create list for annual spredictions (spatial dfs)

years <- sort(unique(dat_combine$year)) #create vector of years to loop over

#calc median and 95% quantile range of CPUE by block and year
for(i in 1:length(unique(dat_combine$year))){
  
  pred_list[[i]] <- dat_combine%>%
    filter(year==years[i])%>%
    group_by(block,lat,long)%>%
    summarize(
      median_cpue = median(pred,na.rm = T), #calculate annual median CPUE
      q_025 = quantile(pred, probs=0.025), #calculate .025% quantile 
      q_975 = quantile (pred, probs=0.975), #calculate 0.957% quantile
      qr_95 = q_975-q_025
    )
}

# Prepare spatial maps for visualizaiton
#read in map of blocks
blks<-readOGR('./CDFWBlocks/fishingblocks_shp/union_fishing_blocks_all.shp')

#convert projection to WGS84
blks<-spTransform(blks,CRS("+proj=longlat +datum=WGS84"))


#read in world polygon shapefile and manipulate
west_coast_poly<-readOGR('./AuxillaryMaps/ca_poly.shp')
plot(west_coast_poly)

#merge blks with spatial summary data
for(i in 1:length(unique(dat_combine$year))){
  
  sp_pred_list[[i]]<-sp::merge(blks,pred_list[[i]],by.x="BLOCK10_ID",by.y="block") #convert spatial cpue dfs into spatial dfs
  sp_pred_list[[i]]@bbox<-matrix(c(-123,-115,31,37),nrow=2,ncol=2,byrow = T) #set new extent of map
  
}


#set up layout of maps
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(-122, 31.5), scale = 0.75)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(-122.4, 31.3), scale = 1, fill = c("transparent", "black"))
txt1 <- list("sp.text", c(-122.4, 31.2), "0") 
txt2 <- list("sp.text", c(-121.4, 31.2), "1°") 
map.layout <- list(arrow, scale,txt1,txt2)

#set up layout of color bar (may need to change depending on range of predictions)
at_num=c(0,0.00375,0.0075,0.01125,0.015,0.01875,0.0225,0.02625,0.03,0.03375,0.0375,0.04125,0.045,0.04875,0.0525,0.05625,0.06,0.06375,0.0675,0.07125,0.075,0.07875,0.0825,0.08625,0.09)
at=c(0.00375,0.01125,0.01875,0.02625,0.03375,0.04125,0.04875,0.05625,0.06375,0.07125,0.07875,0.08625,0.09375,0.10125,0.10875,0.11625,0.12375,0.13125,0.13875,0.14625,0.15375,0.16125,0.16875,0.17625,0.18375)
col.regions = carto_pal(n=7,"Emrld")
col=colorRampPalette(col.regions)(25)

labels_at<-c(0.0075,0.015,0.0225,0.03,0.0375,0.045,0.0525,0.06,0.0675,0.075,0.0825,0.09,0.0975,0.105,0.1125,0.12,0.1275,0.135,0.1425,0.15,0.1575,0.165,0.1725,0.18,0.1875)

labels_labels2<-c("0","0.00375","0.0075","0.01125","0.015","0.01875","0.0225","0.02625",
                  "0.03","0.03375","0.0375","0.04125","0.045","0.04875","0.0525","0.05625","0.06","0.06375",
                  "0.0675","0.07125","0.075","0.07875","0.0825","0.08625","0.09")


#Develop plots for median and quantile range of cpue in space and time and write to pdf files
med_pred_maps <- vector(mode="list",length=length(unique(dat_combine$year)))
qr_pred_maps <- vector(mode="list",length=length(unique(dat_combine$year)))

years <- as.character(years)

for(i in 1:length(years)){
  
  pdf(paste("./Modeling/model_outputs/spatial_medcpue_",years[i],".pdf",sep=""),height=7,width=7)
  med_pred_maps[[i]] <- spplot(sp_pred_list[[i]],zcol="median_cpue",scales=list(draw = TRUE),
                             sp.layout=map.layout,col.regions=col,at=at_num,
                             colorkey=list(at=at,height = 1, labels =list(at=labels_at,labels=labels_labels2,rot=45),space="bottom"))+
    latticeExtra::layer(sp.polygons(west_coast_poly, fill="grey"))
  
  print(med_pred_maps[[i]])
  dev.off()
  
  pdf(paste("./Modeling/model_outputs/spatial_95qrcpue_",years[i],".pdf",sep=""),height=7,width=7)
  qr_pred_maps[[i]]<-spplot(sp_pred_list[[i]],zcol="qr_95",scales=list(draw = TRUE),sp.layout=map.layout,col.regions=col,at=at_num,colorkey=list(at=at,height = 1, labels =list(at=labels_at,labels=labels_labels2,rot=45),space="bottom"))+
    latticeExtra::layer(sp.polygons(west_coast_poly, fill="grey"))
  print(qr_pred_maps[[i]])
  dev.off()
}

#save prediction dfs tp .RData file (in case of future editing)
save(pred_summary,pred_list,file="./Modeling/predict_cpue.RData")

