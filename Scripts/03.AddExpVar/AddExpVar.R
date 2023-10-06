################################################################################
## 002
## Extracting environmental covariates
################################################################################
#date: 17-01-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({ 
  library(plyr)
  library(dplyr)
  library(readr)
  library(terra)
  library(sf)
  library(exactextractr)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load file
db<-read_csv(paste0(Dir,"Data/Urban_Tree_Growth_database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Define load and save directories
loadDir<-paste0(Dir,"Data/Urb_Tree_Growth_Variables_Processed/")
saveDir<-paste0(Dir,"Results/")
if (!dir.exists(saveDir)){
  dir.create(saveDir)
}
saveDir<-paste0(saveDir,"Tables/")
if (!dir.exists(saveDir)){
  dir.create(saveDir)
}

# Load explanatory variables into list
files<-list.files(path=loadDir,pattern="*.tif")
list_ExpVars<-lapply(list.files(path=loadDir,
  pattern="*.tif",full.names=TRUE),terra::rast)
names(list_ExpVars)<-files
rm(files)

# Rename item names in list
names(list_ExpVars)<-c("Annual_Mean_Temperature","Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter","Annual_Precipitation",
  "Precipitation_of_Wettest_Month","Precipitation_of_Driest_Month",
  "Precipitation_Seasonality","Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter","Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter","Mean_Diurnal_Range",
  "Isothermality","Temperature_Seasonality","Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month","Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter","Mean_Temperature_of_Driest_Quarter",
  "Climate_Envolope","Human_Population","Soil_Compaction",
  "Soil_Texture","Urban_Climate_Zone")

#### Loop and extract points per Adm1 ####
# Split rasters into cat and num variables
num<-list_ExpVars[c(1:19,21:22)]
num_names<-names(num)
cat<-list_ExpVars[c(20,23:24)]
cat_names<-names(cat)
rm(list_ExpVars)

# Filter and loop over admin
admNames<-as.list(unique(db$Adm1))
adm_list<-list()
for(adm in admNames){
  # Filter points to Admin
  points<-db %>%
    dplyr::filter(Adm1==adm)
  
  # Loop over species and extract data
  SpeciesNames<-as.list(unique(points$Species))
  Species_list<-list()
  for(species in SpeciesNames){
    # Filter to Species
    point<-points %>%
      dplyr::filter(Species==species)
    
    # Loop over year and extract data
    yearsNames<-as.list(unique(point$years))
    year_list<-list()
    for(year in yearsNames){
      # Filter to year
      pnt<-point %>%
        dplyr::filter(years==year)
      
      # Make spatial
      pnt<-st_as_sf(pnt,coords=c("X","Y"),crs=4326) %>%
        vect()

      # Create buffers
      point_1km_Buf<-terra::buffer(pnt,width=1000) %>%
        st_as_sf()
      point_250m_Buf<-terra::buffer(pnt,width=250) %>%
        st_as_sf()
      
      # Convert to sf object
      pnt<-st_as_sf(pnt)

      # Extract cat data per variable
      p<-list()
      j<-1
      for (file in cat){
        # Grab name
        name<-cat_names[[j]]

        # ID resolution
        res<-res(file)
        res<-res[1]

        # Logic to use different sized buffers
        if(res>0.0082){
          # Extract values over buffer
          file<-exact_extract(file,point_1km_Buf,progress=FALSE,'mode')
          file<-as_tibble(file)
          colnames(file)<-c(name)

          # Add to list
          p[[name]]<-file
          j<-j+1
          rm(file,name)
        } else{
          # Extract values over buffer
          file<-exact_extract(file,point_250m_Buf,progress=FALSE,'mode')
          file<-as_tibble(file)
          colnames(file)<-c(name)

          # Add to list
          p[[name]]<-file
          j<-j+1
          rm(file,name)
        }
     }

     # Squash list
     cat_extract<-as_tibble(do.call(base::cbind,p))
     rm(j,p)

     # Extract num data per variable
     p<-list()
     j<-1
     for (file in num){
       # Grab name
       name<-num_names[[j]]

       # ID resolution
       res<-res(file)
       res<-res[1]

       # Logic to use different sized buffers
       if(res>0.0082){
        # Extract values over buffer
          file<-exact_extract(file,point_1km_Buf,progress=FALSE,'mean')
          file<-as_tibble(file)
          colnames(file)<-c(name)

          # Add to list
          p[[name]]<-file
          j<-j+1
          rm(file,name)
       } else{
        # Extract values over buffer
          file<-exact_extract(file,point_250m_Buf,progress=FALSE,'mean')
          file<-as_tibble(file)
          colnames(file)<-c(name)

          # Add to list
          p[[name]]<-file
          j<-j+1
          rm(file,name)
       }       
     }  

     # Squash list
     num_extract<-as_tibble(do.call(base::cbind,p))
     rm(j,p)

     # Merge cat and num variables
     extract<-as_tibble(base::cbind(num_extract,cat_extract))
     rm(num_extract,cat_extract,res)

     # Merge spatial data
     XY<-st_coordinates(pnt) %>%
      as_tibble()
     pnt<-st_drop_geometry(pnt)
     extract<-as_tibble(base::cbind(pnt,extract,XY))     

     # Add to list
     year_list[[as.character(year)]]<-extract
    }

    # Squash list
    extract<-as_tibble(do.call(base::rbind,year_list))
    Species_list[[species]]<-extract

    # Print message
    message(paste0("Data for ",species," within ",adm," has been extracted"))
   }

  # Squash list
  extract<-as_tibble(do.call(base::rbind,Species_list))
  adm_list[[adm]]<-extract
}
rm(admNames,cat_names,cat,num_names,num,Species_list,year_list,yearsNames,SpeciesNames,
  extract,adm,point,points,XY,point_250m_Buf,point_1km_Buf,year,pnt,species)

# Merge into extracted database
db<-as_tibble(do.call(base::rbind,adm_list))
rm(adm_list)

# Reorder variables
db<-db %>%
  dplyr::select(c("Species","DBH_cm","Country",
  "Adm1","Adm2","Adm3","days","months","years","X","Y",  
  "Annual_Mean_Temperature","Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter","Annual_Precipitation",
  "Precipitation_of_Wettest_Month","Precipitation_of_Driest_Month",
  "Precipitation_Seasonality","Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter","Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter","Mean_Diurnal_Range","Isothermality",
  "Temperature_Seasonality","Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month","Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter","Mean_Temperature_of_Driest_Quarter",
  "Soil_Compaction","Human_Population","Climate_Envolope","Soil_Texture","Urban_Climate_Zone"))

#### Export data ####
write_csv(db,paste0(saveDir,"Urban_Tree_Growth_ExpVar_database.csv"))
rm(loadDir,saveDir,db)

#### End of script ####