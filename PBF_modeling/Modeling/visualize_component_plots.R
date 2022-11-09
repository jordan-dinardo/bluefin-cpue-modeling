# Visualize component plots of PBF CPUE standardization model 
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(ggplot2);library(mgcv);library(tidymv);library(mgcViz)

# read binary and positive models 
load("./Modeling/model_outputs/binary_model_fits.RData")
load("./Modeling/model_outputs/positive_model_fits.RData")
load("./Modeling/testrun.RData")
# identify best model (for binary and positive)

best_binary <- fits_binary[[3]]
best_pos <- fits_pos[[3]]
  
  #Read in prepared data
dat <- read.csv("./FisheriesData/Prepared/pbf_nominal_cpue_data_block.csv")
dat <- na.omit(dat)

colnames(dat) <- c("X","block", "year", "month", "trip_type", "date", "day", "n_anglers", "angler_hrs_blk", "angler_hrs_trip", "t_sea", "pbf_catch", "trip_id", "trip_event_id", "new_vessel_id", "year_month_block", "enso", "pdo", "long", "lat", "region", "nominal_cpue", "pbf_presence")

dat$year_month <- paste(dat$year, sep="_", dat$month)

#change class of var for modeling 
var.factor<-c("trip_type","trip_id","new_vessel_id","year_month","year_month_block","trip_event_id","block")
var.numeric<-c("year","month","n_anglers","angler_hrs_trip","angler_hrs_blk","nominal_cpue","pbf_presence","pdo","enso","long","lat")

dat[,var.factor] <- lapply(dat[,var.factor],as.factor)
dat[,var.numeric] <- lapply(dat[,var.numeric],as.numeric)  

#Visualize components of best performing models using mgcViz package

#set text format
text_format=element_text(face="bold",size=20,color="black")

#convert gam object to gamViz object (for positive GAM)
pos_vis <- getViz(best_pos)

#month
plot(pos_vis, allTerms = T,select=1)+
  xlab("Year")+
  xlim(1995,max(dat$year))+
  scale_x_continuous(breaks = seq(from = 1995, to = max(dat_combine$year), by = 2),guide = guide_axis(n.dodge=2))+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))

#month
plot(pos_vis, allTerms = T,select=2)+
  xlab("Month")+
  xlim(1,12)+
  scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1))+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))


#space
plot(pos_vis, allTerms = T,select=3)+
  xlab("Longitude")+ylab("Latitude")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))



#enso
plot(pos_vis, allTerms = T,select=4)+
  xlab("ENSO Index")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))


#pdo
plot(pos_vis, allTerms = T,select=5)+
  xlab("PDO Index")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))

#sst
ggplot(data=dat_combine)+
  geom_smooth(aes(x=sst,y=pred),method="gam",color='black')+
  theme_classic()+
  labs(x="Sea Surface Temperature (°C)",y="Bluefin CPUE")+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))

#ssh
ggplot(data=dat_combine)+
  geom_smooth(aes(x=ssh,y=pred),method="gam",color='black')+
  theme_classic()+
  labs(x="Sea Surface Height (m)",y="Bluefin CPUE")+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))

#chla
chl_exp <- expression(bold(Sea~Surface~Chlorophyll~(mg/m^3)))
ggplot(data=dat_combine)+
  geom_smooth(aes(x=chl,y=pred),method="gam",color='black')+
  theme_classic()+
  labs(x=chl_exp,y="Bluefin CPUE")+
  theme(axis.title.x = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face="bold",size=20,color="black",vjust=0.5,margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = text_format,
        axis.text.y = text_format,
        plot.margin = unit(c(1,1,1,1),'cm'))

#convert gam object to gamViz object (for binary GAM)
binary_vis <- getViz(best_binary)

