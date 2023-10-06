################################################################################
## 003
## Exploratory data analysis
################################################################################
#date: 17-01-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({ 
  library(plyr)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(DescTools)
  library(corrplot)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load file
db<-read_csv(paste0(Dir,"Results/Tables/Urban_Tree_Growth_ExpVar_database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Create directories
if (!dir.exists(paste0(Dir,"Results/Figures/"))){
  dir.create(paste0(Dir,"Results/Figures/"))
}
if (!dir.exists(paste0(Dir,"Results/Figures/Cleaned noisy points/"))){
  dir.create(paste0(Dir,"Results/Figures/Cleaned noisy points/"))
}

#### Correct classes ####
db<-as_tibble(db) 
db<-db %>%
  mutate(Climate_Envolope=dplyr::case_when(
      Climate_Envolope=="0"~"NA",
      Climate_Envolope=="15"~"Cfb",
      Climate_Envolope=="25"~"Dfa",
      Climate_Envolope=="26"~"Dfb",
      Climate_Envolope=="9"~"Csb"))
db<-db %>%
  mutate(Soil_Texture=dplyr::case_when(
      Soil_Texture=="0"~"NA",
      Soil_Texture=="4"~"ClLo",
      Soil_Texture=="6"~"SaClLo",
      Soil_Texture=="7"~"Lo",
      Soil_Texture=="9"~"SaLo"))
db<-db %>%
  mutate(Urban_Climate_Zone=dplyr::case_when(
      Urban_Climate_Zone=="0"~"Water",
      Urban_Climate_Zone=="1"~"Compact_highrise",
      Urban_Climate_Zone=="10"~"Heavy_industry",
      Urban_Climate_Zone=="11"~"Dense_trees",
      Urban_Climate_Zone=="12"~"Scattered_trees",
      Urban_Climate_Zone=="14"~"Low_plants",
      Urban_Climate_Zone=="17"~"Water",
      Urban_Climate_Zone=="2"~"Compact_midrise",
      Urban_Climate_Zone=="3"~"Compact_lowrise",
      Urban_Climate_Zone=="4"~"Open_highrise",
      Urban_Climate_Zone=="5"~"Open_midrise",
      Urban_Climate_Zone=="6"~"Open_lowrise",
      Urban_Climate_Zone=="8"~"Large_lowrise",
      Urban_Climate_Zone=="9"~"Sparsely_built"))
db<-db %>%
  mutate_if(is.character,as.factor)

# Remove incorrect values
db$Soil_Texture[db$Soil_Texture == "NA"]<-NA
db$Climate_Envolope[db$Climate_Envolope == "NA"]<-NA
db$Urban_Climate_Zone[db$Urban_Climate_Zone == "NA"]<-NA
db<-drop_na(db)
db$Soil_Texture<-as.character(db$Soil_Texture)
db$Climate_Envolope<-as.character(db$Climate_Envolope)
db$Urban_Climate_Zone<-as.character(db$Urban_Climate_Zone)
db$Soil_Texture<-as.factor(db$Soil_Texture)
db$Climate_Envolope<-as.factor(db$Climate_Envolope)
db$Urban_Climate_Zone<-as.factor(db$Urban_Climate_Zone)

#### Remove outliers ####
# Round DBH
db$DBH_cm<-round(db$DBH_cm,1)

# Remove obs with days of 0 and DBH >5
db<-subset(db,!(days==0 & DBH_cm>5))

# Remove observations with < 3 cm dbh
db<-db %>%
  filter(!DBH_cm<3)

# Add average time spent at nurseries (~ 8 years)
db$days<-db$days+2920

# Remove species with low DBH variation
sp<-db %>%
  group_by(Species) %>%
  summarize(variation = sd(DBH_cm)) %>%
  dplyr::filter(variation>0.3) %>%
  dplyr::select("Species")
db<-db %>%
  dplyr::filter(Species %in% sp$Species)
rm(sp)

# Remove species with low days variation
sp<-db %>%
  group_by(Species) %>%
  summarize(variation = sd(days)) %>%
  dplyr::filter(variation>400) %>%
  dplyr::select("Species")
db<-db %>%
  dplyr::filter(Species %in% sp$Species)
rm(sp)

# Reset factors
db<-db %>%
  mutate(across(where(is.factor),as.character)) %>%
  mutate(across(where(is.character),as.factor))

# Remove outlier days and DBH grouped by species
list<-list()
for(sp in as.list(as.character(unique(db$Species)))){
  # Filter to species
  df<-db %>%
    dplyr::filter(Species==sp)
    
  # Remove DBH outliers using z-score
  z_scores<-as.data.frame(sapply(as.data.frame(df$DBH_cm),
    function(data) (abs(data-mean(data))/sd(data))))
  df$z_scores<-z_scores
  df<-subset(df,(z_scores<4))
  df$z_scores<-NULL
  df<-drop_na(df,DBH_cm)

  # Remove days outliers using z-score
  z_scores<-as.data.frame(sapply(as.data.frame(df$days),
    function(data) (abs(data-mean(data))/sd(data))))
  df$z_scores<-z_scores
  df<-subset(df,(z_scores<4))
  df$z_scores<-NULL
  df<-drop_na(df,days)

  # Add to list 
  list[[sp]]<-df
  rm(z_scores,df,sp)
}

# Squash list
db<-as_tibble(do.call(base::rbind,list))
db<-db %>%
  mutate(across(where(is.factor),as.character)) %>%
  mutate(across(where(is.character),as.factor))
db<-drop_na(db)
rm(list)

# Per species, use GAM to exclude 'noisy' points
list<-list()
for(sp in as.list(as.character(sort(unique(db$Species))))){
  # Filter to species
  df<-db %>%
    dplyr::filter(Species==sp)

  # Dont remove outliers from species with low observations (<50)
  if(nrow(df)>49){
    # Plot 'noisy' data
    P<-ggplot(df, aes(x = days)) +
      geom_point(aes(y = DBH_cm))

    # Perform locally weighted scatterplot smoothing
    fit<-suppressWarnings(mgcv::gam(DBH_cm ~ s(days,bs="cr",k=2),data=df))
    
    # Predict the fitted values
    df$Pred_DBH_cm<-predict(fit,df)

    # Find the residuals
    df$residuals<-df$DBH_cm - df$Pred_DBH_cm  

    # Remove noise around fitted line
    df<-df %>%
      dplyr::filter(abs(residuals)<3*sd(residuals))
  
    # Create a scatter plot of the filtered data and fitted line
    P<-P +
      geom_point(aes(y = DBH_cm), color = "blue",data=df) +
      geom_point(aes(y = Pred_DBH_cm), color = "red",data=df) +
      labs(title=paste0("Relationship between DBH and age (days) for ",sp)) +
      theme(plot.title=element_text(size=12))
    ggsave(paste0(Dir,
      "Results/Figures/Cleaned noisy points/",
      sp,".png"),P,width=7,height=5,dpi=300)

    # Remove new columns
    df$Pred_DBH_cm<-NULL
    df$residuals<-NULL
  
    # Add to list
    list[[sp]]<-df
    #message(paste0(as.character(sp), " has been completed"))
    rm(df,sp,fit,P)
  } else{
    # Add to list
    list[[sp]]<-df
    #message(paste0(as.character(sp), " has been completed"))
    rm(df,sp)
  }
}

# Squash list
db<-as_tibble(do.call(base::rbind,list))
db<-db %>%
  mutate(across(where(is.factor),as.character)) %>%
  mutate(across(where(is.character),as.factor))
db<-na.omit(db)
rm(list)

#### Explore relationships ####
# Visualize the distribution of numeric variables
db %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key,scales="free")

# Correlation plot
corrplot(cor(db%>%select_if(is.numeric))) # Climate variables correlated

# Prepair new database
cat<-db %>%
  select_if(is.factor)

# Visualize the distribution of categorical variables
Desc(cat,plotit=TRUE)

# Explore relationship between categorical variables
Desc(Climate_Envolope~Soil_Texture,data=cat)
Desc(Climate_Envolope~Urban_Climate_Zone,data=cat)
Desc(Soil_Texture~Urban_Climate_Zone,data=cat)
rm(cat)

#### Remove 'weak' species from database ####
# Remove species with less than 100
db<-db %>%
  group_by(Species) %>%
  filter(n()>99) %>%
  ungroup()

# Remove genus from species column
nwords<-function(string,pseudo=F){
  ifelse(pseudo,
    pattern<-"\\S+", 
    pattern<-"[[:alpha:]]+"
    )
  str_count(string,pattern)
}
db$Words<-nwords(db$Species,pseudo=TRUE)
db<-subset(db,Words>1)
db$Words<-NULL
rm(nwords)

# Reset factors
db<-db %>%
  mutate(across(where(is.factor),as.character)) %>%
  mutate(across(where(is.character),as.factor))

#### Add dummy observations ####
# Take a 15% sample per species
dummy_sample<-db %>%
  group_by(Species) %>%
  sample_frac(0.15) %>%
  ungroup()

# Change days and DBH to 0
dummy_sample$days<-0
dummy_sample$DBH_cm<-0

# Merge back into dataset
db<-as_tibble(rbind(db,dummy_sample))

# Suffel data
db<-db[sample(nrow(db)),]

#### Export data ####
write_csv(db,paste0(Dir,"Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"))
rm(Dir,db,dummy_sample)

#### End of script ####
