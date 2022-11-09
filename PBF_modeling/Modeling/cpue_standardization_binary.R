# Standardization of PBFT CPUE using delta-GAMM (Binary models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(mgcv);library(zoo);library(DHARMa)

#Read in prepared data
dat <- read.csv("./FisheriesData/Prepared/pbf_nominal_cpue_data_block.csv")
dat <- na.omit(dat)

#change class of var for modeling 
var.factor<-c("trip_type","trip_id","new_vessel_id","year_month_blk","trip_event_id","block")
var.numeric<-c("year","month","n_anglers","angler_hrs_trip","angler_hrs_blk","nominal_cpue","pbf_presence","pdo","enso","long","lat")

dat[,var.factor] <- lapply(dat[,var.factor],as.factor)
dat[,var.numeric] <- lapply(dat[,var.numeric],as.numeric)  

#Fit models

n_models <- 6 #define number of models 
fits_binary <- vector(mode="list",length=n_models) #create list of models

# Phase 1. Base model
fits_binary[[1]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(Longitude,Latitude,bs="ds"),
                      data=dat,family=binomial(link="logit"),method="fREML")

# Phase 2. Base model + global env var 
fits_binary[[2]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp"),
                      data=dat,family=binomial(link="logit"),method="fREML") 

fits_binary[[3]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp"),
                      data=dat,family=binomial(link="logit"),method="fREML") 

# Phase 3. Base model + finescale env var 
fits_binary[[4]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                      data=dat_pos,family=gaussian(link="logit"),method="fREML")
                      
# Phase 4. Base model + global env var + finescale env var 
fits_binary[[5]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                      data=dat_pos,family=gaussian(link="logit"),method="fREML")

fits_binary[[6]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp")+s(sst,bs="tp")+s(ssh,bs="tp")+s(chla,bs="tp"),
                      data=dat_pos,family=gaussian(link="logit"),method="fREML")

#save models
save(fits_binary,"./Modeling/model_outputs/binary_model_fits.RData") #save model fits thus far

# Conduct gam.check and aic comparison of all models 

model_checks_binary <- vector(mode="list",length = n_models)
model_aics_binary <- vector(mode="list",length = n_models)
model_deviance_binary <- vector(mode="list",length = n_models)

for(i in 1:n_models){
  
  model_checks_binary[[i]] <- gam.check(fits_binary[[i]]) #check model diagnostics (including knots)
  simulated_residuals_binry[[i]] <- simulateResiduals(fits_binary[[i]]) #simulate residuals to check model diagnostics for binary data (using DHARMa pkg)
  model_aics_binary[[i]] <- fits_binary[[i]]$aic #extract aic
  model_deviance_binary[[i]] <- fits_binary[[i]]$deviance #extract deviance
}

save(fits_binary,model_checks_binary,model_aics_binary,model_deviance_binary,"./Modeling/model_outputs/binary_model_fits.RData")
