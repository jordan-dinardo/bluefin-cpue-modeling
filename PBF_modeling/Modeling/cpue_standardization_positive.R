# Standardization of PBFT CPUE using delta-GAMM (positive cpue models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(mgcv);library(zoo)

#Read in prepared data
dat <- read.csv("./FisheriesData/Prepared/pbf_nominal_cpue_data_block.csv")
dat_pos <- subset(dat,pbf_presence>0)
dat_pos <- na.omit(dat_pos)
#change class of var for modeling 
var.factor<-c("trip_type","trip_id","new_vessel_id","year_month","year_month_block","trip_event_id","block")
var.numeric<-c("year","month","n_anglers","angler_hrs_trip","angler_hrs_blk","nominal_cpue","pbf_presence","pdo","enso","long","lat")

dat_pos[,var.factor] <- lapply(dat_pos[,var.factor],as.factor)
dat_pos[,var.numeric] <- lapply(dat_pos[,var.numeric],as.numeric)  

#Fit models

n_models <- 5 #define number of models 
fits_pos <- vector(mode="list",length=n_models) #create list of models

# Phase 1. Base model
fits_pos[[1]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML")

# Phase 2. Base model + global env var 
fits_pos[[2]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

fits_pos[[3]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc",=12)+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp"),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

# Phase 3. Base model + global env var (with temporal interaction)
fits_pos[[4]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp")+ti(year,month,pdo,bs=c("tp","cc","tp")),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

fits_pos[[5]]<-gam(log(nominal_cpue)~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp")+ti(year,month,enso,bs=c("tp","cc","tp")),
                   data=dat_pos,family=gaussian(link="identity"),method="REML") 

# # Phase 4. Base model + finescale env var 
# # Phase 5. Base model + finescale env var (with temporal interaction)
# # Phase 6. Base model + finescale env var (with spatial interaction)
# # Phase 7. Base model + global env var + finescale env var 
# # Phase 8. Base model + global env var (with temporal interaction) + finescale env var (with temporal interaction)
# # Phase 9. Base model + global env var (with temporal interaction) + finescale env var (with spatial interaction)

#save models
save(fits_pos,"./Modeling/model_outputs/binary_model_fits.RData") #save model fits thus far

# Conduct gam.check and aic comparison of all models 

model_checks <- vector(mode="list",length = n_models)
model_aics <- vector(mode="list",length = n_models)
model_deviance <- vector(mode="list",length = n_models)

for(i in 1:n_models){
  
  model_checks[[i]] <- gam.check(fits_pos[[i]]) #check model diagnostics (including knots)
  model_aics[[i]] <- fits_pos[[i]]$aic #extract aic
  model_deviance[[i]] <- fits_pos[[i]]$deviance #extract deviance
}


save(fits_pos,model_checks,model_aics,model_deviance,"./Modeling/model_outputs/binary_model_fits.RData")





