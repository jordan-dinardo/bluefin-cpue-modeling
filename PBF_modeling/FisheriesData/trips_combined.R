# Combine trip_id dfs (single, multi, and na trip_id csv files)
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(tidyverse)

#Read in trip_id csv files 
trips <- list.files("./FisheriesData/Clean",pattern = ".csv",full.names = T) 
trips <- lapply(trips,read.csv) 

new_trip_ids <- c(1:(length(unique(trips[[1]]$trip_id))+length(unique(trips[[2]]$trip_id))+length(unique(trips[[3]]$trip_id))))
new_trip_event_ids <- c(1:(length(unique(trips[[1]]$trip_event_id))+length(unique(trips[[2]]$trip_event_id))+length(unique(trips[[3]]$trip_event_id))))

new_trip_id_1 <- data.frame(trip_id=unique(trips[[1]]$trip_id),new_trip_id=new_trip_ids[c(1:(length(unique(trips[[1]]$trip_id))))])
new_trip_id_2 <- data.frame(trip_id=unique(trips[[2]]$trip_id),new_trip_id=new_trip_ids[c((length(unique(trips[[1]]$trip_id))+1):(length(unique(trips[[1]]$trip_id))+length(unique(trips[[2]]$trip_id))))])
new_trip_id_3 <- data.frame(trip_id=unique(trips[[3]]$trip_id),new_trip_id=new_trip_ids[c((length(unique(trips[[1]]$trip_id))+length(unique(trips[[2]]$trip_id)))+1:length(unique(trips[[3]]$trip_id)))])

new_trip_event_id_1 <- data.frame(trip_event_id=unique(trips[[1]]$trip_event_id),new_event_trip_id=new_trip_event_ids[c(1:(length(unique(trips[[1]]$trip_event_id))))])
new_trip_event_id_2 <- data.frame(trip_event_id=unique(trips[[2]]$trip_event_id),new_event_trip_id=new_trip_event_ids[c((length(unique(trips[[1]]$trip_event_id))+1):(length(unique(trips[[1]]$trip_event_id))+length(unique(trips[[2]]$trip_event_id))))])
new_trip_event_id_3 <- data.frame(trip_event_id=unique(trips[[3]]$trip_event_id),new_event_trip_id=new_trip_event_ids[c((length(unique(trips[[1]]$trip_event_id))+length(unique(trips[[2]]$trip_event_id)))+1:length(unique(trips[[3]]$trip_event_id)))])

new_trip_id_list <- list(new_trip_id_1,new_trip_id_2,new_trip_id_3)
new_trip_event_id_list <- list(new_trip_event_id_1,new_trip_event_id_2,new_trip_event_id_3)


#harmonize trip_id and trip_event_id
for(i in 1:length(trips)){
  
  trips[[i]] <- merge(trips[[i]],new_trip_id_list[[i]],by="trip_id")
  trips[[i]] <- merge(trips[[i]],new_trip_event_id_list[[i]],by="trip_event_id")
  trips[[i]] <- dplyr::select(trips[[i]],!c(trip_id,trip_event_id)) #omit old trip_id and trip_event_id
  colnames(trips[[i]])[c(13,14)] <- c("trip_id","trip_event_id")
}


#unlist tripa bind trip dfs together
trips <- bind_rows(trips, .id = "column_label")
trips <- dplyr::select(trips,!column_label)

#randomize Vessel ID for confidentiality purposes
vesselid <- data.frame(VesselID=unique(trips$vessel_id))

#function to generate unique id
create_unique_ids <- function(n, char_len = 5){
  
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = T), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = T), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

vesselid$new_vessel_id <- create_unique_ids(length(vesselid$VesselID))#create unique vessel id

trips <- merge(trips,vesselid,by.x="vessel_id",by.y ="VesselID") #merge new vessel id with catch_summary df
trips <- dplyr::select(trips,!vessel_id)

#omit trips where n_anglers is greater than 100 (unlikely)
trips <- trips%>%
  filter(n_anglers<100)%>%
  drop_na(angler_hrs_blk,n_anglers)%>%
  subset(n_anglers>0&angler_hrs_blk>0)  

#Check frequency of trips with PBF catch by trip type. Would expect for a frequency of trips with PBF to increase as trip duration increases
pbf_trip_freq <- trips %>% 
  group_by(trip_type) %>% 
  summarise(n_trips=n_distinct(trip_id),
            n_PBF_trips=n_distinct(trip_id[pbf_catch>0]),
            med_n_anglers=median(n_anglers),
            mean_n_anglers=mean(n_anglers))%>%
  mutate(
    freq_PBF_trips=n_PBF_trips/n_trips,
    freq_PBF_trips_over_all=n_PBF_trips/sum(n_trips))
  
write.csv(trips,file="./FisheriesData/Clean/clean_trips_combined.csv",row.names=F)

  




