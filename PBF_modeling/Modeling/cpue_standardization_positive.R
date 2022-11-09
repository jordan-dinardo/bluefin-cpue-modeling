# Standardization of PBFT CPUE using delta-GAMM (positive cpue models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(mgcv);library(zoo)

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

#Fit models

n_models <- 6 #define number of models 

fits_pos <- vector(mode="list",length=n_models) #create list of models

# Phase 1. Base model
fits_pos[[1]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML")

# Phase 2. Base model + global env var 
fits_pos[[2]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

fits_pos[[3]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

# Phase 3. Base model + finescale env var
fits_pos[[4]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                      data=dat_pos,family=gaussian(link="identity"),method="REML")

# Phase 4. Base model + global env var + finescale env var
fits_pos[[5]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML")

fits_pos[[6]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML")


#save models
save(fits_pos,"./Modeling/model_outputs/binary_model_fits.RData") #save model fits thus far

# Conduct gam.check and aic comparison of all models 

model_checks_pos <- vector(mode="list",length = n_models)
model_aics_pos <- vector(mode="list",length = n_models)
model_deviance_pos <- vector(mode="list",length = n_models)

for(i in 1:n_models){
  
  model_checks_pos[[i]] <- gam.check(fits_pos[[i]]) #check model diagnostics (including knots)
  model_aics_pos[[i]] <- fits_pos[[i]]$aic #extract aic
  model_deviance_pos[[i]] <- fits_pos[[i]]$deviance #extract deviance
}


save(fits_pos,model_checks_pos,model_aics_pos,model_deviance_pos,"./Modeling/model_outputs/positive_model_fits.RData")