################################################################################
## 007
## Incremental growth predictions
################################################################################
#date: 02-03-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({
     library(sf)
     library(terra)
     library(exactextractr)
     library(readr)
     library(plyr)
     library(dplyr)
     library(tidyr)
     library(stringr)
     library(ggplot2)
     library(tidymodels)
     library(vetiver)
     library(pins)
     library(ranger)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Create directory
if (!dir.exists(paste0(Dir,"Results/ToGird/"))){
  dir.create(paste0(Dir,"Results/ToGird/"))
}
if (!dir.exists(paste0(Dir,"Results/ToGird/Difference/"))){
  dir.create(paste0(Dir,"Results/ToGird/Difference/"))
}
if (!dir.exists(paste0(Dir,"Results/ToGird/Current_DBH/"))){
  dir.create(paste0(Dir,"Results/ToGird/Current_DBH/"))
}
if (!dir.exists(paste0(Dir,"Results/ToGird/Future_DBH/"))){
  dir.create(paste0(Dir,"Results/ToGird/Future_DBH/"))
}
if (!dir.exists(paste0(Dir,"Results/ToGird/Shapefile/"))){
  dir.create(paste0(Dir,"Results/ToGird/Shapefile/"))
}

# Load cleaned tree database
db<-read_csv(paste0(Dir,
  "Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Species metrics
Species_Metrics<-read_csv(paste0(Dir,
  "Results/Tables/Species_Metrics.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Load AOI
AOI<-st_read(paste0(Dir,
  "Data/AOI.gpkg"),quiet=TRUE)

# Define the the board location containing all models
board<-board_folder(paste0(Dir,"Results/Models/Products/"))

# Load explanatory variables into list
files<-list.files(path=paste0(
  Dir,"Data/Urb_Tree_Growth_Variables_Processed/"),
  pattern="*.tif")
list_ExpVars<-lapply(list.files(
  path=paste0(Dir,"Data/Urb_Tree_Growth_Variables_Processed/"),
  pattern="*.tif",full.names=TRUE),terra::rast)
names(list_ExpVars)<-files
rm(files)

# Rename item names in list
list_ExpVars[["245_2021-2040_ACCESS-CM2_bio.tif"]]<-NULL
names(list_ExpVars)<-c("Annual_Mean_Temperature","Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter","Annual_Precipitation",
  "Precipitation_of_Wettest_Month","Precipitation_of_Driest_Month",
  "Precipitation_Seasonality","Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter","Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter","Mean_Diurnal_Range","Isothermality",
  "Temperature_Seasonality","Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month","Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter","Mean_Temperature_of_Driest_Quarter",  
  "Climate_Envolope","Human_Population","Soil_Compaction",
  "Soil_Texture","Urban_Climate_Zone")

# Load future climate data
fut_clim<-terra::rast(
     paste0(Dir,
      "Data/Urb_Tree_Growth_Variables_Processed/245_2021-2040_ACCESS-CM2_bio.tif")) %>%
     split(c(1:19))
Clim_names<-c("Annual_Mean_Temperature","Mean_Diurnal_Range","Isothermality",
     "Temperature_Seasonality","Max_Temperature_of_Warmest_Month",
     "Min_Temperature_of_Coldest_Month","Temperature_Annual_Range",
     "Mean_Temperature_of_Wettest_Quarter","Mean_Temperature_of_Driest_Quarter",
     "Mean_Temperature_of_Warmest_Quarter","Mean_Temperature_of_Coldest_Quarter",
     "Annual_Precipitation","Precipitation_of_Wettest_Month",
     "Precipitation_of_Driest_Month","Precipitation_Seasonality",
     "Precipitation_of_Wettest_Quarter","Precipitation_of_Driest_Quarter",
     "Precipitation_of_Warmest_Quarter","Precipitation_of_Coldest_Quarter")
names(fut_clim)<-Clim_names
rm(Clim_names)

#### Create points over AOI ####
# Create grid
grid<-terra::rast(ext=ext(AOI),
  resolution=as.numeric(0.00449), # ~500m
  crs=crs(AOI))
values(grid)<-1
grid<-grid %>%
  terra::crop(AOI) %>%
  terra::mask(AOI)

# Convert to points
Points_list<-list()
for(name in unique(AOI$NAME_1)){
  # Select first province
  aoi<-dplyr::filter(AOI,NAME_1==name)

  # Clip and mask to province
  g<-grid %>%
    terra::crop(aoi) %>%
    terra::mask(aoi)
  
  # Add admin information
  g$Adm1<-name

  # Convert to points
  point<-terra::as.points(g)

  # Add to list
  Points_list[[name]]<-point
  rm(aoi,g,name,point)
}
rm(grid)

#### Pre-processing ####
# Drop unnecessary variables
db<-db %>%
  dplyr::select(!c('Country','Adm2','Adm3',
    'months','years','X','Y')) %>%
  mutate(across(where(is.character),as.factor))

#### Remove 'weak' species from database ####
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

# Remove 'weak' species
weak_species<-as.character(Species_Metrics$Species)
db<-subset(db,Species %in% weak_species)
rm(weak_species,Species_Metrics)

# Reset factors
db<-db %>%
  mutate(across(where(is.factor),as.character)) %>%
  mutate(across(where(is.character),as.factor))

#### Extract data ####
# Loop over species
Species_list<-as.list(as.character(sort(unique(db$Species))))
for(sp in Species_list){
  # Filter
  df<-dplyr::filter(db,Species==sp) %>%
    dplyr::select(!c("Species"))
  message(paste0("Starting with ",sp,""))

  # Reset factor levels
  df<-df %>%
    mutate(across(where(is.factor),as.character)) %>%
    mutate(across(where(is.character),as.factor))
  
  # Split data into cat and num variables
  hist_num<-list_ExpVars[c(1:19,21:22)]
  fut_num<-fut_clim
  fut_num<-c(fut_num,hist_num[c(20:21)])
  hist_cat<-list_ExpVars[c(20,23:24)]

  # Order lists
  hist_num<-hist_num[order(names(hist_num))]
  fut_num<-fut_num[order(names(fut_num))]
  hist_cat<-hist_cat[order(names(hist_cat))]
  hist_cat_num_list<-c(hist_cat,hist_num)
  fut_cat_num_list<-c(hist_cat,fut_num)
  rm(hist_cat,hist_num,fut_num)

  # Split rasters into cat and num variables
  num<-hist_cat_num_list[c(4:24)]
  num_names<-names(num)
  cat<-hist_cat_num_list[c(1:3)]
  cat_names<-names(cat)
  fut_num<-fut_cat_num_list[c(4:24)]
  fut_num_names<-names(fut_num)
  rm(hist_cat_num_list,fut_cat_num_list)

  # Loop over points list and extract
  point_names<-as.list(names(Points_list))
  for(point_name in point_names){
    # Grab point file
    points<-Points_list[[point_name]]
    message(paste0("Processing ",sp," within ",point_name))
    
    # Create buffers
    point_250m_Buf<-terra::buffer(points,width=250) %>%
      st_as_sf()   
      
    # Convert to sf object
    points<-st_as_sf(points)

    # Extract cat data per variable
    p<-list()
    j<-1
    for (file in cat){
      # Grab name
      name<-cat_names[[j]]      
      
      # Extract values over buffer
      file<-exact_extract(file,point_250m_Buf,
        progress=FALSE,'mode')
      file<-as_tibble(file)
      colnames(file)<-c(name)

      # Add to list
      p[[name]]<-file
      j<-j+1
      rm(file,name)
    }

    # Squash list
    cat_extract<-as_tibble(do.call(base::cbind,p))
    rm(j,p)

    # Extract num data per variable for historic data
    p<-list()
    j = 1
    for (file in num){
      # Grab name
      name<-num_names[[j]]

      # Extract values over buffer
      file<-exact_extract(file,point_250m_Buf,
        progress=FALSE,'mean')
      file<-as_tibble(file)
      colnames(file)<-c(name)

      # Add to list
      p[[name]]<-file
      j<-j+1
      rm(file,name)
    }

    # Squash list
    num_extract<-as_tibble(do.call(base::cbind,p))     
    rm(p,j)

    # Extract num data per variable for future data
    p<-list()
    j = 1
    for (file in fut_num){
      # Grab name
      name<-fut_num_names[[j]]

      # Extract values over buffer
      file<-exact_extract(file,point_250m_Buf,
        progress=FALSE,'mean')
      file<-as_tibble(file)
      colnames(file)<-c(name)

      # Add to list
      p[[name]]<-file
      j<-j+1
      rm(file,name)
    }

    # Squash list
    num_extract_fut<-as_tibble(do.call(base::cbind,p))
    rm(p,j)

    # Merge cat and num variables
    extract<-as_tibble(base::cbind(num_extract,cat_extract))
    extract_fut<-as_tibble(base::cbind(num_extract_fut,cat_extract))
    rm(num_extract,cat_extract,point_250m_Buf,num_extract_fut)

    # Merge spatial data
    XY<-st_coordinates(points) %>%
      as_tibble()
    extract<-as_tibble(base::cbind(extract,XY))
    extract_fut<-as_tibble(base::cbind(extract_fut,XY))
    rm(XY,points)

    # Add administrative boundaries to data
    extract$Adm1<-point_name
    extract_fut$Adm1<-point_name
    message(paste0("Raster data for ",sp," within ",
      point_name," has been extracted"))

    # Add ID
    extract$ID<-1:nrow(extract)
    extract_fut$ID<-1:nrow(extract_fut)

    # Correct variable classes
    cat_vars<-extract %>%
      dplyr::select(c("Climate_Envolope",
      "Soil_Texture","Urban_Climate_Zone","Adm1")) %>%
      mutate_all(factor)
    num_vars<-extract %>%
      dplyr::select(!c("Climate_Envolope",
      "Soil_Texture","Urban_Climate_Zone","Adm1")) %>%
      mutate_all(as.numeric)
    num_fut_vars<-extract_fut %>%
      dplyr::select(!c("Climate_Envolope",
      "Soil_Texture","Urban_Climate_Zone","Adm1")) %>%
      mutate_all(as.numeric)
    extract<-as_tibble(base::cbind(cat_vars,num_vars))
    extract_fut<-as_tibble(base::cbind(cat_vars,num_fut_vars))
    rm(cat_vars,num_vars,num_fut_vars)

    # Reclassify
    extract<-as_tibble(extract)
    extract_fut<-as_tibble(extract_fut)
    extract<-extract %>%
      mutate(Climate_Envolope=dplyr::case_when(
        Climate_Envolope=="0"~"NA",
        Climate_Envolope=="15"~"Cfb",
        Climate_Envolope=="25"~"Dfa",
        Climate_Envolope=="26"~"Dfb",
        Climate_Envolope=="9"~"Csb"))
    extract_fut<-extract_fut %>%
      mutate(Climate_Envolope=dplyr::case_when(
        Climate_Envolope=="0"~"NA",
        Climate_Envolope=="15"~"Cfb",
        Climate_Envolope=="25"~"Dfa",
        Climate_Envolope=="26"~"Dfb",
        Climate_Envolope=="9"~"Csb"))    
    extract<-extract %>%
      mutate(Soil_Texture=dplyr::case_when(
        Soil_Texture=="0"~"NA",
        Soil_Texture=="4"~"ClLo",
        Soil_Texture=="6"~"SaClLo",
        Soil_Texture=="7"~"Lo",
        Soil_Texture=="9"~"SaLo"))
    extract_fut<-extract_fut %>%
      mutate(Soil_Texture=dplyr::case_when(
        Soil_Texture=="0"~"NA",
        Soil_Texture=="4"~"ClLo",
        Soil_Texture=="6"~"SaClLo",
        Soil_Texture=="7"~"Lo",
        Soil_Texture=="9"~"SaLo"))    
    extract<-extract %>%
      mutate(Urban_Climate_Zone=dplyr::case_when(
        Urban_Climate_Zone=="0"~"NA",
        Urban_Climate_Zone=="1"~"Compact_highrise",
        Urban_Climate_Zone=="10"~"Heavy_industry",
        Urban_Climate_Zone=="11"~"Dense_trees",
        Urban_Climate_Zone=="12"~"Scattered_trees",
        Urban_Climate_Zone=="14"~"Low_plants",
        Urban_Climate_Zone=="17"~"NA",
        Urban_Climate_Zone=="2"~"Compact_midrise",
        Urban_Climate_Zone=="3"~"Compact_lowrise",
        Urban_Climate_Zone=="4"~"Open_highrise",
        Urban_Climate_Zone=="5"~"Open_midrise",
        Urban_Climate_Zone=="6"~"Open_lowrise",
        Urban_Climate_Zone=="8"~"Large_lowrise",
        Urban_Climate_Zone=="9"~"Sparsely_built"))
    extract_fut<-extract_fut %>%
      mutate(Urban_Climate_Zone=dplyr::case_when(
        Urban_Climate_Zone=="0"~"NA",
        Urban_Climate_Zone=="1"~"Compact_highrise",
        Urban_Climate_Zone=="10"~"Heavy_industry",
        Urban_Climate_Zone=="11"~"Dense_trees",
        Urban_Climate_Zone=="12"~"Scattered_trees",
        Urban_Climate_Zone=="14"~"Low_plants",
        Urban_Climate_Zone=="17"~"NA",
        Urban_Climate_Zone=="2"~"Compact_midrise",
        Urban_Climate_Zone=="3"~"Compact_lowrise",
        Urban_Climate_Zone=="4"~"Open_highrise",
        Urban_Climate_Zone=="5"~"Open_midrise",
        Urban_Climate_Zone=="6"~"Open_lowrise",
        Urban_Climate_Zone=="8"~"Large_lowrise",
        Urban_Climate_Zone=="9"~"Sparsely_built"))
    extract<-extract %>%
      mutate_if(is.character,as.factor)
    extract_fut<-extract_fut %>%
      mutate_if(is.character,as.factor)

    # Remove incorrect values
    extract$Soil_Texture[extract$Soil_Texture == "NA"]<-NA
    extract$Climate_Envolope[extract$Climate_Envolope == "NA"]<-NA
    extract$Urban_Climate_Zone[extract$Urban_Climate_Zone == "NA"]<-NA
    extract<-drop_na(extract)
    extract$Soil_Texture<-as.character(extract$Soil_Texture)
    extract$Climate_Envolope<-as.character(extract$Climate_Envolope)
    extract$Urban_Climate_Zone<-as.character(extract$Urban_Climate_Zone)
    extract$Soil_Texture<-as.factor(extract$Soil_Texture)
    extract$Climate_Envolope<-as.factor(extract$Climate_Envolope)
    extract$Urban_Climate_Zone<-as.factor(extract$Urban_Climate_Zone)    
    extract_fut$Soil_Texture[extract_fut$Soil_Texture == "NA"]<-NA
    extract_fut$Climate_Envolope[extract_fut$Climate_Envolope == "NA"]<-NA
    extract_fut$Urban_Climate_Zone[extract_fut$Urban_Climate_Zone == "NA"]<-NA
    extract_fut<-drop_na(extract_fut)
    extract_fut$Soil_Texture<-as.character(extract_fut$Soil_Texture)
    extract_fut$Climate_Envolope<-as.character(extract_fut$Climate_Envolope)
    extract_fut$Urban_Climate_Zone<-as.character(extract_fut$Urban_Climate_Zone)
    extract_fut$Soil_Texture<-as.factor(extract_fut$Soil_Texture)
    extract_fut$Climate_Envolope<-as.factor(extract_fut$Climate_Envolope)
    extract_fut$Urban_Climate_Zone<-as.factor(extract_fut$Urban_Climate_Zone)

    # Ensure same number of rows
    matching_ids<-extract_fut$ID %in% extract$ID
    extract_fut<-extract_fut[matching_ids,]
    matching_ids<-extract$ID %in% extract_fut$ID
    extract<-extract[matching_ids,]
    extract_fut$ID=NULL
    extract$ID=NULL
    rm(matching_ids)

    # Filter aoi
    aoi<-AOI %>%
      dplyr::filter(NAME_1==point_name)
    
    # Grab data for saving
    XY<-extract %>%
      dplyr::select(c("X","Y"))

    # Add mean growing days
    extract$days<-as.numeric(round(mean(df$days)))
    extract_fut$days<-as.numeric(round(mean(df$days)))
    
    # Load model
    model<-vetiver_pin_read(board, paste0(sp,"_RF_model"))

    # Predict
    extract_Predicted<-predict(model,extract,type="numeric")
    extract_Predicted<-rename(extract_Predicted,Current_DBH=.pred)
    extract_fut_Predicted<-predict(model,extract_fut,type="numeric")
    extract_fut_Predicted<-rename(extract_fut_Predicted,Future_DBH=.pred)
    rm(model,extract,extract_fut)

    # Prepare data for normalization
    fut_min<-min(extract_fut_Predicted$Future_DBH)
    fut_max<-max(extract_fut_Predicted$Future_DBH)
    cur_min<-min(extract_Predicted$Current_DBH)
    cur_max<-max(extract_Predicted$Current_DBH)
    max<-max(c(fut_max,cur_max))
    min<-min(c(fut_min,cur_min))
    rm(fut_min,fut_max,cur_min,cur_max)

    # Scale predictions for difference product
    extract_fut_Predicted<-extract_fut_Predicted %>%
      mutate(Scaled_Future_DBH=(Future_DBH-min)/(max-min))
    extract_Predicted<-extract_Predicted %>%
      mutate(Scaled_Current_DBH=(Current_DBH-min)/(max-min))    

    # Calculate difference in predictions
    Diff<-as_tibble(extract_fut_Predicted$Scaled_Future_DBH-extract_Predicted$Scaled_Current_DBH)
    colnames(Diff)<-"DBH_Difference_Fut_Hist"
    Diff$Current_DBH<-extract_Predicted$Current_DBH
    Diff$Future_DBH<-extract_fut_Predicted$Future_DBH
    rm(extract_fut_Predicted,extract_Predicted)

    # Create spatial object
    Diff<-base::cbind(Diff,XY)
    Diff<-st_as_sf(Diff,coords=c("X","Y"),crs=4326)

    # Create grid
    grid<-terra::rast(ext=ext(aoi),resolution=as.numeric(0.00449),crs=crs(aoi))
    rm(aoi)

    # Convert to raster
    Difference<-terra::rasterize(Diff,grid,field="DBH_Difference_Fut_Hist")
    Current_DBH<-terra::rasterize(Diff,grid,field="Current_DBH")
    Future_DBH<-terra::rasterize(Diff,grid,field="Future_DBH")

    # Save to disk
    writeRaster(Difference,paste0(Dir,"Results/ToGird/Difference/",sp,"_",point_name,"_difference.tif"))
    writeRaster(Current_DBH,paste0(Dir,"Results/ToGird/Current_DBH/",sp,"_",point_name,"_current_DBH.tif"))
    writeRaster(Future_DBH,paste0(Dir,"Results/ToGird/Future_DBH/",sp,"_",point_name,"_future_DBH.tif"))
    suppressWarnings(st_write(Diff,paste0(Dir,"Results/ToGird/Shapefile/",sp,"_",point_name,"_DBH.gpkg"),quiet=TRUE))

    # Print message
    message(paste0("DBH difference map for ",sp," within ",point_name," has been saved"))

    # Clean environment
    rm(point_name,Diff,XY,grid,Difference,Current_DBH,Future_DBH)
  }
}

# Clean environment
rm(AOI,db,board,Dir,point_names,Points_list,df,cat,fut_clim,fut_num,
  fut_num_names,list_ExpVars,num,num_names,sp,Species_list,cat_names)

#### End of script ####
