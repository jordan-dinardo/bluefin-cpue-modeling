# Standardization of PBFT CPUE using delta-GAMM (positive cpue models) - at the block level
# Jordan DiNardo (jdinardo@ucsd.edu)

# Note: Be sure to set working directory to PBF_modeling file

## required packages ##
library(formula.tools); library(gt);library(patchwork);library(mgcv)

#Load in binary and positive models 
load("/Volumes/LACIE SHARE/BluefinTuna/bluefin-cpue-modeling/PBF_modeling/Modeling/testrun_pos.RData")
load("/Volumes/LACIE SHARE/BluefinTuna/bluefin-cpue-modeling/PBF_modeling/Modeling/testrun_binary.RData")
#load("/Volumes/LACIE SHARE/BluefinTuna/CDFW_CPFV/DeltaGAM/PositiveCPUE_ModelRuns_April.RData")

n_models <- 4 #define number of models 

## Create vector of model formulas
binary_formulas <- vector(length = n_models)
pos_formulas <- vector(length = n_models)


for(i in 1:n_models){
  binary_formulas[i] <- gsub("\"", "'",paste(get.vars(fits_binary[[i]]$formula),collapse = " + "),fixed=T)
  pos_formulas[i] <-gsub("\"", "'",paste(get.vars(fits_pos[[i]]$formula),collapse = " + "),fixed=T)
  binary_formulas[i] <- sub("+", "~", binary_formulas[i], fixed = TRUE)
  pos_formulas[i] <- sub("+", "~", pos_formulas[i], fixed = TRUE)
  binary_formulas[i] <- sub("pbf_presence", "PBF presence", binary_formulas[i], fixed = TRUE)
  pos_formulas[i] <- sub("~log(nominal_cpue)", "log(PBF CPUE)", pos_formulas[i], fixed = TRUE)
  #binary_formulas[i]<-paste(mod_no[i],binary_formulas[i],sep="")
  #pos_formulas[i]<-paste(mod_no[i],pos_formulas[i],sep="")
}


## Create a vector of % deviance explained
binary_dev_exp <- vector(length = n_models)
pos_dev_exp <- vector(length = n_models)

for(i in 1:n_models){
  binary_dev_exp[i] <- round((((fits_binary[[i]]$null.deviance - fits_binary[[i]]$deviance) / fits_binary[[i]]$null.deviance)*100),2)
  pos_dev_exp <- round((((fits_pos[[i]]$null.deviance - fits_pos[[i]]$deviance) / fits_pos[[i]]$null.deviance)*100),2)
}

#####
model_aic_binary <- vector(length=n_models)
model_aic_pos <- vector(length=n_models)
model_deviance_binary <- vector(length=n_models)
model_deviance_pos <- vector(length=n_models)

for (i in 1:n_models){
  model_aic_binary[i] <- fits_binary[[i]]$aic
  model_aic_pos[i] <- fits_pos[[i]]$aic
  model_deviance_binary[i] <- fits_binary[[i]]$deviance
  model_deviance_pos[i] <- fits_pos[[i]]$deviance
  
}
 
#bind data into a dataframe to create table
vars <-c("Model","Deviance","% Deviance explained","AIC","Type")

stats_binary <- data.frame(Model=binary_formulas,dev=model_deviance_binary,dev_exp=binary_dev_exp,aic=model_aic_binary,type=rep("Proportion of positive catch rates",times=n_models))
colnames(stats_binary)<-vars

stats_pos <- data.frame(Model=pos_formulas,dev=model_deviance_pos,dev_exp=pos_dev_exp,aic=model_aic_pos,type=rep("Positive catch rates",times=n_models))
colnames(stats_pos)<-vars

stats <- rbind(stats_binary,stats_pos)

#create table
stats_tbl <- gt(data = stats,rowname_col = "Model",groupname_col = "Type")%>%
  # tab_style(       #### can't figure out how to highlight row of best model...will work on this when we determine the best model
  #   style = cell_fill(color="lightcyan"),
  #   locations=cells_body(
  #     #columns = vars(Model,Deviance,`% Deviance explained`,AIC),
  #     rows=AIC==min(AIC)))%>%
  cols_width(vars(Model) ~ px(1400),
             vars('% Deviance explained') ~ px(100))%>% 
  tab_style(
    locations = cells_column_labels(everything()),
    style     = list(
      cell_text(weight = "bold", size = 24))
  )%>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(10),
    source_notes.font.size = 12,
    heading.align = "left",
    #Adjust grouped rows to make them stand out
    row_group.background.color = "grey")%>%
  cols_align(align="center")

stats_tbl

gt::gtsave(stats_tbl,path="./Modeling/model_outputs",file="model_selection_table.png")
