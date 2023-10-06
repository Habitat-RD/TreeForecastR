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
  library(geodata)
})

# Set working directory
Dir<-"G:/My Drive/Github/2023_MEI_TreGrwth/"
#Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load inventory
df<-read_csv(paste0(Dir,"Data/Urban_Tree_Growth.csv"),
  col_names=TRUE,show_col_types=FALSE)

#### Add and correct spatial data ####
# Set variables
iso3<-"CAN"
Name<-"Montreal"

# Check if XY data is available
if("X" %in% colnames(df)){
  # Add flag for duplicate GPS points
  Duplicated<-df %>%
    dplyr::select(c("X","Y")) %>%
    duplicated()
  df$Duplicated<-Duplicated
  rm(Duplicated)

  # See if X or Y has NA's
  if(sum(is.na(df$X))<sum(is.na(df$Y))){
    df$X[is.na(df$Y)]<-NA
  }else{
    df$Y[is.na(df$X)]<-NA
  }

  # Keep information with NA XY
  df_xy_na<-df %>%
    dplyr::filter(is.na(X))
  df<-df %>%
    dplyr::filter(!is.na(X))
    
  # Make spatial  
  df<-st_as_sf(df,coords=c("X","Y"),crs=4326)

  # Download and add administration boundary information
  message("Downloading level 3 ...")
  aoi3<-try(gadm(country=iso3,level=3,path=tempdir(),resolution=1),silent=TRUE)
  if(is.null(nrow(aoi3))){
    message("Level 3 is unavailable. Downloading level 2 ...")
    aoi3<-NULL
    aoi2<-gadm(country=iso3,level=2,path=tempdir(),resolution=1)
  }
    
  # Process
  if(is.null(aoi3)){
    # Extract data using GPS
    message("Level 2 downloaded")
    message(paste0("Extracting level 2 data for ",Name," ..."))
    admin_Info<-terra::extract(aoi2,vect(df))
    df$Country<-admin_Info$COUNTRY
    df$Adm1<-admin_Info$NAME_1
    df$Adm2<-admin_Info$NAME_2
              
    # Merge other NA XY data
    dfxy<-df %>%
      st_coordinates() %>%
      as.data.frame()
    df<-df %>%
      st_drop_geometry()

    # Binding data frames
    df<-base::cbind(df,dfxy)
    df<-rbind.fill(df,df_xy_na)

    # Set erronious GPS to NA
    df$X[is.na(df$Country)]<-NA
    df$Y[is.na(df$Country)]<-NA

    # Remove dublicates
    df<-df[!(df$Duplicated==TRUE),]
    df$Duplicated<-NULL
    rm(aoi2,dfxy,df_xy_na,admin_Info,aoi3)
    }else{
      # Extract data using GPS
      message("Level 3 downloaded")
      message(paste0("Extracting level 3 data for ",Name," ..."))
      admin_Info<-terra::extract(aoi3,vect(df))
      df$Country<-admin_Info$COUNTRY
      df$Adm1<-admin_Info$NAME_1
      df$Adm2<-admin_Info$NAME_2
      df$Adm3<-admin_Info$NAME_3       
              
      # Merge other NA XY data
      dfxy<-df %>%
        st_coordinates() %>%
        as.data.frame()
      df<-df %>%
        st_drop_geometry()

      # Binding data frames
      df<-base::cbind(df,dfxy)
      df<-rbind.fill(df,df_xy_na)

      # Set erronious GPS to NA
      df$X[is.na(df$Country)]<-NA
      df$Y[is.na(df$Country)]<-NA

      # Remove dublicates
      df<-df[!(df$Duplicated==TRUE),]
      df$Duplicated<-NULL
      rm(aoi3,dfxy,df_xy_na,admin_Info)
      }
}else{
  message(paste0("There is no lat long information for: ",Name))
  df<-df
}
rm(Name)

#### Filter AOI ####
# Specify areas to retain in AOI
areas<-c("Anjou","Côte-Saint-Luc","Dollard-des-Ormeaux","Hampstead",
     "Kirkland","L'Île-Bizard","Lachine","LaSalle","Mont-Royal",
     "Montréal","Montréal-Est","Montréal-Nord","Montréal-Ouest",
     "Outremont","Pierrefonds","Roxboro","Saint-Laurent","Saint-Léonard",
     "Sainte-Geneviève","Verdun","Westmount","Laval")

# Download AOI
AOI<-gadm(country=iso3,level=3,path=tempdir(),resolution=1) %>%
     st_as_sf()

# Create extent area
min_x<- -73.94126
min_y<- 45.40982
max_x<- -73.47284
max_y<- 45.70193

# Create a matrix representing the bounding box polygon
bbox_matrix<-matrix(c(min_x,min_y,max_x,min_y,max_x,
     max_y,min_x,max_y,min_x,min_y),ncol=2,byrow=TRUE)

# Convert the matrix to an sf polygon
bbox_sf<-st_sfc(st_polygon(list(bbox_matrix))) %>%
     st_as_sf()

# Define the coordinate reference system (CRS)
st_crs(bbox_sf) <- 4326
rm(min_x,min_y,max_x,max_y,bbox_matrix)

# Filter AOI
AOI<-AOI %>%
     dplyr::filter(NAME_3 %in% areas) %>%
     st_intersection(bbox_sf)
rm(iso3,areas,bbox_sf)

#### Export data ####
write_csv(df,paste0(Dir,"Data/Urban_Tree_Growth_database_NEW.csv"))
st_write(AOI,paste0(Dir,"Data/AOI.gpkg"),quiet=TRUE)
rm(df,AOI)

#### End of script ####