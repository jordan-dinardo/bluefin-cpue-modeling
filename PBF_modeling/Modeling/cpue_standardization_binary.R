# Standardization of PBFT CPUE using delta-GAMM (Binary models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse);library(mgcv);library(zoo)

#Read in prepared data
dat <- read.csv("./FisheriesData/Prepared/pbf_nominal_cpue_data_block.csv")

#change class of var for modeling 
var.factor<-c("trip_type","trip_id","new_vessel_id","year_month","year_month_block","trip_event_id","block")
var.numeric<-c("year","month","n_anglers","angler_hrs_trip","angler_hrs_blk","nominal_cpue","pbf_presence","pdo","enso","long","lat")

dat[,var.factor] <- lapply(dat[,var.factor],as.factor)
dat[,var.numeric] <- lapply(dat[,var.numeric],as.numeric)  

#Fit models

n_models <- 5 #define number of models 
fits_binary <- vector(mode="list",length=n_models) #create list of models

# Phase 1. Base model
fits_binary[[1]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds"),
                      data=dat,family=binomial(link="logit"),method="fREML")

# Phase 2. Base model + global env var 
fits_binary[[2]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp"),
                      data=dat,family=binomial(link="logit"),method="fREML") 

fits_binary[[3]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp"),
                      data=dat,family=binomial(link="logit"),method="fREML") 

# Phase 3. Base model + global env var (with temporal interaction)
fits_binary[[4]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(pdo,bs="tp")+ti(year,month,pdo,bs=c("cr","cc","cr")),
                      data=dat,family=binomial(link="logit"),method="fREML") 

fits_binary[[5]]<-bam(pbf_presence~s(year,bs="tp")+s(month,bs="cc")+s(new_vessel_id,bs="re")+s(long,lat,bs="ds")+s(enso,bs="tp")+ti(year,month,enso,bs=c("cr","cc","cr")),
                      data=dat,family=binomial(link="logit"),method="fREML") 

# Phase 4. Base model + finescale env var 
# Phase 5. Base model + finescale env var (with temporal interaction)
# Phase 6. Base model + finescale env var (with spatial interaction)
# Phase 7. Base model + global env var + finescale env var 
# Phase 8. Base model + global env var (with temporal interaction) + finescale env var (with temporal interaction)
# Phase 9. Base model + global env var (with temporal interaction) + finescale env var (with spatial interaction)

#save models
save(fits_binary,"./Modeling/model_outputs/binary_model_fits.RData") #save model fits thus far

# Conduct gam.check and aic comparison of all models 

model_checks <- vector(mode="list",length = n_models)
model_aics <- vector(mode="list",length = n_models)
model_deviance <- vector(mode="list",length = n_models)

for(i in 1:n_models){
  
  model_checks[[i]] <- gam.check(fits_binary[[i]]) #check model diagnostics (including knots)
  model_aics[[i]] <- fits_binary[[i]]$aic #extract aic
  model_deviance[[i]] <- fits_binary[[i]]$deviance #extract deviance
}

save(fits_binary,model_checks,model_aics,model_deviance,"./Modeling/model_outputs/binary_model_fits.RData")






