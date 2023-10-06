################################################################################
## 006
## Summary statistics
################################################################################
#date: 27-03-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({
  library(readr)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(fuzzyjoin)
  library(fs)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load list of RF metrics
file_names<-list.files(path=paste0(Dir,"Results/Models/Products/"),
  pattern="*_RF_final_metrics.csv")
file_names<-gsub("_RF_final_metrics.csv$","",file_names)
suppressMessages(list_csv<-lapply(list.files(path=paste0(
    Dir,"Results/Models/Products/"),
    pattern="*_RF_final_metrics.csv",
    full.names=TRUE),read_csv,show_col_types=FALSE))
names(list_csv)<-file_names

# Load cleaned tree database
db<-read_csv(paste0(Dir,"Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Create directory
if (!dir.exists(paste0(Dir,"Results/Figures/Growth Curves/Weak/"))){
  dir.create(paste0(Dir,"Results/Figures/Growth Curves/Weak/"))
}
if (!dir.exists(paste0(Dir,"Results/Figures/Growth Curves/Selected/"))){
  dir.create(paste0(Dir,"Results/Figures/Growth Curves/Selected/"))
}

#### Consolidate Random Forest metrics ####
# Add species name to metric csv
list_csv_format<-list()
for(name in file_names){
  # Select csv file
  csv<-list_csv[[name]]

  # Reshape columns
  csv<-csv %>%
    dplyr::select(c(2,4)) %>%
    spread(key=.metric,value=.estimate)
  
  # Add species name to column
  csv$Species<-as.character(name)

  # Add to list
  list_csv_format[[name]]<-csv
  rm(name,csv)
}

# Squash list
Species_Metrics<-as_tibble(do.call(base::rbind,list_csv_format)) %>%
  arrange(Species)
rm(file_names,list_csv,list_csv_format)

#### Add additional database summary statistics ####
# Calculate max DBH per species
DBH_max<-db %>%
  group_by(Species) %>%
  summarise(max_DBH=max(DBH_cm)) %>%
  arrange(Species)
Species_Metrics$max_DBH<-DBH_max$max_DBH
rm(DBH_max)

# Calculate number of obs per species
Freq<-table(db$Species) %>%
  as.data.frame() %>%
  rename(Species=Var1,Frequency=Freq) %>%
  arrange(Species)
Species_Metrics$Frequency<-Freq$Frequency
rm(Freq)

# Calculate the DBH range of the species
DBH_range<-db %>%
  group_by(Species) %>%
  summarise(range_DBH=max(DBH_cm)-min(DBH_cm)) %>%
  arrange(Species)
Species_Metrics$range_DBH<-DBH_range$range_DBH
rm(DBH_range)

# Calculate the days range of the species
Days<-db %>%
  group_by(Species) %>%
  summarise(range_days=max(days)-min(days)) %>%
  arrange(Species)
Species_Metrics$range_days<-Days$range_days
rm(Days)

# Calculate SD of DBH
DBH_sd<-db %>%
  group_by(Species) %>%
  summarise(sd_DBH=sd(DBH_cm)) %>%
  arrange(Species)
Species_Metrics$sd_DBH<-DBH_sd$sd_DBH
rm(DBH_sd)

# Calclate SD of days
days_sd<-db %>%
  group_by(Species) %>%
  summarise(sd_days=sd(days)) %>%
  arrange(Species)
Species_Metrics$sd_days<-days_sd$sd_days
rm(days_sd)

# Create binned days
db$days_bin<-ceiling(db$days/365)*365

# Calculate mean DBH per binned days
mean_DBH<-db %>%
  group_by(Species,days_bin) %>%
  summarise(mean_DBH=mean(DBH_cm))

# Remove 0's
mean_DBH<-mean_DBH %>%
  dplyr::filter(days_bin>0)

# Grab mean_DBH of max and min bin
list<-list()
for(sp in as.list(as.character(unique(mean_DBH$Species)))){
  # Filter to species
  df<-mean_DBH %>%
    dplyr::filter(Species==sp)

  # Check is enough points for model
  if(nrow(df)>4){
    # Calculate decreasing trend using GAM
    fit<-suppressWarnings(mgcv::gam(mean_DBH ~ s(days_bin,bs="cr",k=2),data=df))
    df$Poly_DBH<-as.numeric(predict(fit,df))
    df_bot<-df %>%
      slice_tail(n=2) %>%
      as.data.frame() %>%
      dplyr::select(c("Poly_DBH"))
    is.decreasing<-all(diff(df_bot$Poly_DBH)< -10)

    # Compare DBH in lowest and highest days bin
    df_min_max<-df %>%
      filter(mean_DBH==min(mean_DBH) |
            mean_DBH==max(mean_DBH))
    is.decreasingg<-all(diff(df_min_max$mean_DBH)<0)

    # Create data frame and add to list
    dff<-data.frame(Species=sp,
      DecreaseTrend=as.character(is.decreasing),
      MinMaxDif=as.character(is.decreasingg))
    list[[sp]]<-dff
    rm(dff,df,sp,df_bot,is.decreasingg,is.decreasing,df_min_max,fit)
  } else{
    # Compare DBH in lowest and highest days bin
    df_min_max<-df %>%
      filter(mean_DBH==min(mean_DBH) |
            mean_DBH==max(mean_DBH))
    is.decreasingg<-all(diff(df_min_max$mean_DBH)<0)

    dff<-data.frame(Species=sp,DecreaseTrend=NA,MinMaxDif=is.decreasingg)
    list[[sp]]<-dff
    rm(dff,sp,df,df_min_max,is.decreasingg)
  }  
}
df<-as_tibble(do.call(base::rbind,list)) %>%
  arrange(Species)
Species_Metrics<-merge(Species_Metrics,df,by="Species")
db$days_bin<-NULL
rm(mean_DBH,list,df)

# Calculate number of obs per species within each Adm1 zone
Freq_Species_Adm1<-db %>%
  group_by(Adm1,Species) %>%
  count() %>%
  arrange(Adm1,Species)
Quebec<-Freq_Species_Adm1 %>%
  dplyr::filter(Adm1=="Québec")
France<-Freq_Species_Adm1 %>%
  dplyr::filter(Adm1=="Nouvelle-Aquitaine")
BC<-Freq_Species_Adm1 %>%
  dplyr::filter(Adm1=="British Columbia")

# Add species which are missing from each Adm1 zone
sp<-Species_Metrics %>%
  dplyr::select(c("Species")) %>%
  as_tibble()
Quebec<-merge(Quebec,sp,by="Species",all=TRUE) %>%
  as_tibble() %>%
  dplyr::select(c(1,3)) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  arrange(Species)
France<-merge(sp,France,by="Species",all=TRUE) %>%
  as_tibble() %>%
  dplyr::select(c(1,3)) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  arrange(Species)
BC<-merge(BC,sp,by="Species",all=TRUE) %>%
  as_tibble() %>%
  dplyr::select(c(1,3)) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  arrange(Species)
rm(sp)

# Add to Species_Metrics
Species_Metrics$Quebec_Frequ<-Quebec$n
Species_Metrics$Bourdeaux_Frequ<-France$n
Species_Metrics$BC_Frequ<-BC$n
rm(Quebec,France,BC,Freq_Species_Adm1,db)

#### Export data ####
# Save to disk
write_csv(Species_Metrics,paste0(Dir,"Results/Tables/Species_Metrics.csv"))

#### Move growth curves to "weak" species folder ####
# Grab species names
unique_sp<-unique(Species_Metrics$Species)
unique_sp<-data.frame(name=unique_sp)

# Remove weak performing species
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(max_DBH>=20)         # remove species with max DBH lower than 20
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(mae<=8)              # keep species with MAE of 8 or smaller
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(mae>0.02)            # remove species which overfit
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(ccc>=0.8)            # keep species with CCC of 80% or greater
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(range_DBH>=8)        # keep species with a DBH range of 8 or greater
Species_Metrics<-Species_Metrics %>%    # keep species with a days range of 1825 (5 years) or greater
     dplyr::filter(range_days>=4745)    # We include the 8 years of growth prior to planting
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(                     # remove species where the oldest DBH
          MinMaxDif==FALSE)             # is smaller than the youngest DBH
Species_Metrics<-Species_Metrics %>%
     dplyr::filter(                     # remove species showing severe negative
          DecreaseTrend==FALSE)         # DBH growth curve

# Grab good species species
good_species<-as.character(Species_Metrics$Species)
good_species<-data.frame(name=good_species)

# Identify weak species
weak_species<-anti_join(unique_sp,good_species,by="name")
rm(unique_sp)

# Convert to vectors
good_species<-unique(good_species$name)
weak_species<-unique(weak_species$name)

# Move png of weak species
source_directory<-paste0(Dir,"Results/Figures/Growth Curves/")
destination_directory<-paste0(Dir,"Results/Figures/Growth Curves/Weak/")
for (file_name in weak_species) {
  file_name<-paste0(file_name,".png")
  source_file_path<-file.path(source_directory,file_name)
  dest_file_path<-file.path(destination_directory,file_name)
  if (file.exists(source_file_path)) {
    file_move(source_file_path,dest_file_path)
    cat("Moved",file_name,"\n")
  } else {
    cat("File", file_name, "not found in the source directory.\n")
  }
}

# Move png of good species
source_directory<-paste0(Dir,"Results/Figures/Growth Curves/")
destination_directory<-paste0(Dir,"Results/Figures/Growth Curves/Selected/")
for (file_name in good_species) {
  file_name<-paste0(file_name,".png")
  source_file_path<-file.path(source_directory,file_name)
  dest_file_path<-file.path(destination_directory,file_name)
  if (file.exists(source_file_path)) {
    file_move(source_file_path,dest_file_path)
    cat("Moved",file_name,"\n")
  } else {
    cat("File", file_name, "not found in the source directory.\n")
  }
}

# Clean environment
rm(Dir,Species_Metrics,source_directory,destination_directory,weak_species,
  file_name,source_file_path,dest_file_path,good_species)

#### End of script ####