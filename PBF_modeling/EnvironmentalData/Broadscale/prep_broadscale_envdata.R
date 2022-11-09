# Preparing finescale environmental data (sea surface height, sea surface temperature, 
# and chlorophyll-a) for PBF CPUE modeling 
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(stringr);library(tidyr);library(purrr)

## prepare broadscale environmental data ##

  # load broadscale environmental data
enso <- read.csv("./EnvironmentalData/Broadscale/ENSO_Timeseries.csv")
pdo <- read.csv("./EnvironmentalData/Broadscale/PDO_Timeseries.csv")

  # create lists to use in data prep
env_data <- list(enso,pdo)
env_df <- vector(mode="list",length=length(env_data))
env_var <- c("enso","pdo")
  
  # prepare data

for(i in 1:length(env_data)){
  env_df[[i]] <- gather(env_data[[i]],key="month",value="value",-c(year)) #change df from wide to long format 
  env_df[[i]]$value <- ifelse(env_df[[i]]$value==-999,NA,env_df[[i]]$value)
  names(env_df[[i]])[names(env_df[[i]]) == 'value'] <- env_var[i] #change "value" var to specific env var
  env_df[[i]]$month <- as.numeric(substring(env_df[[i]]$month, 2))# omit 'X' before month value and convert var to numeric
}

env_df_all <- env_df%>%reduce(dplyr::full_join,by=c("year","month")) #merge env dfs

#### broadscale environmental data output ##
write.csv(env_df_all,file="./EnvironmentalData/Broadscale/envdata_broadscale.csv",row.names=F)


