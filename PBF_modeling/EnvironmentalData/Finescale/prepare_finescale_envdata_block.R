
#Read in prepared finescale env data
sst_dat <- read.csv("./EnvironmentalData/Finescale/SST_COMBINED/sst_prepared_data_block.csv")
chl_dat <- read.csv("./EnvironmentalData/Finescale/CHL_COMBINED/chla_prepared_data_block.csv")
ssh_dat <- read.csv("./EnvironmentalData/Finescale/SSH_COPERNICUS_NEW/ssh_prepared_data_block.csv")

#scale each variable for modeling 
dat_list <- list(sst_dat,chl_dat,ssh_dat)
vars <- c("sst","chl","ssh")
for(i in 1:length(dat_list)){
  dat_list[[i]] <- dat_list[[i]]%>%
    mutate(
      value_scale = as.vector(scale(dat_list[[i]][,vars[i]]))
    )%>%
    select(c(year,month,Block,Longitude,Latitude,value_scale))
  colnames(dat_list[[i]]) <- c("year","month","block","long","lat",vars[i])
  
  #colnames(dat_list[[i]]) <- c("long","lat","block",vars[i],"year","month")    
}

env_df_all <- dat_list%>%reduce(dplyr::full_join,by=c("year","month","block","long","lat")) #merge all env df
env_df_all <- env_df_all[, c("year", "month", "block","long","lat","sst","chl","ssh")]

#write df to csv file 
write.csv(env_df_all,"./EnvironmentalData/Finescale/envdata_finescale_block.csv",row.names = F)

