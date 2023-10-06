################################################################################
## 001
## Preprocessing of environmental covariates
################################################################################
#date: 17-01-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({  
  library(sf)
  library(plyr)
  library(dplyr)
  library(readr)
  library(terra)
  library(exactextractr)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load area of interest (AOI)
AOI<-st_read(paste0(Dir,"Data/AOI.gpkg"),quiet=TRUE)
AOI<-st_buffer(AOI,dist=1000)

# Download raster data
# Save to local folder "Data/Urb_Tree_Growth_Variables/"
# Modify code to represent your selected variables

# Define load and save directories
loadDir<-paste0(Dir,"Data/Urb_Tree_Growth_Variables/")
saveDir<-paste0(Dir,"Data/Urb_Tree_Growth_Variables_Processed/")
if (!dir.exists(saveDir)){
  dir.create(saveDir)
}

#### Process climate data ####
AOII<-st_buffer(AOI,dist=10000)

# Create template for clipping
tempRast<-terra::rast(ext=ext(AOII),resolution=0.008333333,crs=crs(AOII)) # ~1000 m

# Load BioClim
BioClim<-terra::rast(paste0(loadDir,"WorldClim_Bio.tif"))

# Loop, clip and save
for(i in 1:19){
  BC<-BioClim[[i]]
  BC<-BC %>%
  terra::crop(AOII) %>%
  terra::mask(AOII) %>%  
  terra::project(tempRast,method="bilinear") %>%
  writeRaster(paste0(saveDir,"BioClimate_",as.character(i),".tif"),overwrite=TRUE)
  i<-i+1
}
rm(i,BC,name,names,list_BioClimate,tempRast,AOII,BioClim)

#### Process climate envolope ####
# Create template for clipping
tempRast<-terra::rast(ext=ext(AOI),resolution=0.008333333,crs=crs(AOI)) # ~1000 m

# List
names<-list.files(path=loadDir,pattern="*Beck_KG_V1_present_0p0083")
names<-gsub(".*Beck_KG_V1_","",names)
names<-gsub("_0p0083.tif.*", "",names)
list_ClimateEnvolope<-lapply(list.files(path=loadDir,
  pattern="*Beck_KG_V1_present_0p0083",
  full.names=TRUE),terra::rast)
names(list_ClimateEnvolope)<-names

# Loop, clip and save
rclmat<-matrix(c(0,NA),ncol=2,byrow=TRUE)
i<-1
for(CE in list_ClimateEnvolope){
  name<-names[[i]]
  CE<-CE %>%
  terra::crop(AOI) %>%
  terra::mask(AOI) %>%  
  terra::project(tempRast,method="near") %>%
  terra::classify(rclmat,include.lowest=TRUE) %>%
  writeRaster(paste0(saveDir,"Climate_Envolope_",name,".tif"),overwrite=TRUE)
  i<-i+1
}
rm(i,CE,name,names,list_ClimateEnvolope,tempRast,rclmat)

#### Process Human population ####
# Create template for clipping
tempRast<-terra::rast(ext=ext(AOI),resolution=0.002245,crs=crs(AOI)) # ~250 m

# List
list_Human_Pop<-lapply(list.files(path=loadDir,
  pattern="*Human_Population_1975_2015",
  full.names=TRUE),terra::rast)
Human_Pop<-terra::sprc(list_Human_Pop) %>%
  terra::merge()

# Clip
Human_Pop<-Human_Pop %>%
  terra::crop(AOI) %>%
  terra::mask(AOI) %>%  
  terra::project(tempRast,method="bilinear")

# Save
writeRaster(terra::subset(Human_Pop,1),
  paste0(saveDir,"Human_Population_1975.tif"),
  overwrite=TRUE)
rm(list_Human_Pop,tempRast,Human_Pop)

#### Process Urban climate zone ####
# Create template for clipping
tempRast<-terra::rast(ext=ext(AOI),resolution=0.0008983,crs=crs(AOI)) # ~100 m

# List
list_Urb_Clim<-lapply(list.files(path=loadDir,
  pattern="*Local_Urban_Climate_Zone_2018",
  full.names=TRUE),terra::rast)

# Crop, mask, project
list_Urb_Clim<-lapply(list_Urb_Clim,function(x) crop(x,AOI))
list_Urb_Clim<-lapply(list_Urb_Clim,function(x) mask(x,AOI))
list_Urb_Clim<-lapply(list_Urb_Clim,function(x) project(x,tempRast,method="near"))

# Merge and save
Urb_Clim<-terra::sprc(list_Urb_Clim) %>%
  terra::mosaic(fun="max") %>%
  writeRaster(paste0(saveDir,
    "Urban_Climate_Zone_2018.tif"),
    overwrite=TRUE)
rm(list_Urb_Clim,tempRast,Urb_Clim)

#### Process Soil data ####
# Create template for clipping
tempRast<-terra::rast(ext=ext(AOI),resolution=0.002245,crs=crs(AOI)) # ~250 m

# Soil texture
rclmat<-matrix(c(0,NA),ncol=2,byrow=TRUE)
terra::rast(paste0(loadDir,"Soil_Texture_2018.tif")) %>%
  terra::crop(AOI) %>%
  terra::mask(AOI) %>%
  terra::project(tempRast,method="near") %>%
  terra::classify(rclmat,include.lowest=TRUE) %>%
  writeRaster(paste0(saveDir,
    "Soil_Texture_2018.tif"),
    overwrite=TRUE)

# Soil compaction
rclmat<-matrix(c(0,70,NA),ncol=3,byrow=TRUE)
terra::rast(paste0(loadDir,"Soil_Compaction_2018.tif")) %>%
  terra::crop(AOI) %>%
  terra::mask(AOI) %>%
  terra::project(tempRast,method="bilinear") %>%
  terra::classify(rclmat,include.lowest=TRUE) %>%
  writeRaster(paste0(saveDir,
    "Soil_Compaction_2018.tif"),
    overwrite=TRUE)
rm(tempRast,loadDir,saveDir,AOI,rclmat)

#### End of script ####