################################################################################
## 007
## Incremental growth predictions
################################################################################
#date: 27-03-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({
     library(sf)
     library(terra)
     library(geodata)
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
     library(fuzzyjoin)
     library(lubridate)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load file
db<-read_csv(paste0(Dir,
     "Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"),
     col_names=TRUE,show_col_types=FALSE)

# Species metrics
Species_Metrics<-read_csv(paste0(Dir,
     "Results/Tables/Species_Metrics.csv"),
     col_names=TRUE,show_col_types=FALSE)

# Load AOI
AOI<-st_read(paste0(Dir,
     "Data/AOI.gpkg"),
     quiet=TRUE)

# Create directory
if (!dir.exists(paste0(Dir,"Results/Figures/Predicted Growth Curves/"))){
  dir.create(paste0(Dir,"Results/Figures/Predicted Growth Curves/"))
}
if (!dir.exists(paste0(Dir,"Results/Predictions/"))){
  dir.create(paste0(Dir,"Results/Predictions/"))
}
if (!dir.exists(paste0(Dir,"Results/Predictions/Future Incremental Growth/"))){
  dir.create(paste0(Dir,"Results/Predictions/Future Incremental Growth/"))
}
if (!dir.exists(paste0(Dir,"Results/Predictions/Current Incremental Growth/"))){
  dir.create(paste0(Dir,"Results/Predictions/Current Incremental Growth/"))
}
if (!dir.exists(paste0(Dir,"Results/Figures/RF error boxplots/"))){
  dir.create(paste0(Dir,"Results/Figures/RF error boxplots/"))
}

# Define the the board location containing all models
board<-board_folder(paste0(Dir,"Results/Models/Products/"))

# Download Future climate
#download<-paste0(Dir,"Data/Urb_Tree_Growth_Variables_Processed/")
#cmip6_world(
#     model="ACCESS-CM2",
#     ssp="245",time="2021-2040",
#     var="bioc",res=2.5,
#     path=tempdir()) %>%
#     terra::crop(AOI,mask=TRUE) %>%
#     writeRaster(paste0(download,
#          "245_2021-2040_ACCESS-CM2_bio.tif"),
#          overwrite=TRUE)
#rm(download)

# Download historic DBH measurments
# https://www.donneesquebec.ca/recherche/dataset/vmtl-arbres/resource/6c217c92-63c8-49e8-a6ef-afda6121656d
#url<-"https://montreal-prod.storage.googleapis.com/resources/6c217c92-63c8-49e8-a6ef-afda6121656d/arbres-dhp.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=test-datapusher-delete%40amplus-data.iam.gserviceaccount.com%2F20230727%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20230727T124544Z&X-Goog-Expires=604800&X-Goog-SignedHeaders=host&x-goog-signature=34f38eb7a076fbd90c2f28816565ee100e02de89510028bb549c0196f39331ccb5c045a03ff117b9eca13957f35973fec05553200866669d0466f5eea0555716fd4946b4794e3c703e66a0f228dfda6acf238b8cf2f73f297d0cc5ab35400fdc25e92b71ef1a604f5fddc63f3ac0023cfe4422c0f20a461a111cc113203757e199a1c9556305669afb3d30d60653e7e2761e0a087da244c2e430e9c14cf233ff7e13557eb184a922684662436730306413cc559b045c055ffc25dbf72d504abc5c2273ab850a0718f6cfb66018b5948e67b00fa93b8ab09887e2359ecd7881a264e7d3a2e4673f8109dec103c57fabea1ab0bfe9cefb88528abb69df070dfd3f"
#destfile<-paste0(Dir,"Data/arbres-dhp.csv")
#download.file(url,destfile)
#rm(url, destfile)

# Load future climate data
fut_clim<-terra::rast(
     paste0(paste0(Dir,
          "Data/Urb_Tree_Growth_Variables_Processed/"),
     "245_2021-2040_ACCESS-CM2_bio.tif")) %>%
     terra::crop(AOI) %>%
     terra::mask(AOI) %>%
     split(c(1:19))
Clim_names<-c("Annual_Mean_Temperature","Mean_Diurnal_Range","Isothermality",
     "Temperature_Seasonality","Max_Temperature_of_Warmest_Month","Min_Temperature_of_Coldest_Month",
     "Temperature_Annual_Range","Mean_Temperature_of_Wettest_Quarter","Mean_Temperature_of_Driest_Quarter",
     "Mean_Temperature_of_Warmest_Quarter","Mean_Temperature_of_Coldest_Quarter","Annual_Precipitation",
     "Precipitation_of_Wettest_Month","Precipitation_of_Driest_Month","Precipitation_Seasonality",
     "Precipitation_of_Wettest_Quarter","Precipitation_of_Driest_Quarter","Precipitation_of_Warmest_Quarter",
     "Precipitation_of_Coldest_Quarter")
names(fut_clim)<-Clim_names
rm(AOI,Clim_names)

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

#### Extract future data and predict ####
# Per species predict future growth
Species<-as.list(unique(as.character(db$Species)))
Predicted_list<-list()
for(sp in Species){
     # Filter database
     df<-dplyr::filter(db,Species==sp) %>%
          dplyr::select(!c("Species"))
     df$ID<-1:nrow(df)

     # ID max DBH
     df$Max_DBH<-as.numeric(max(df$DBH_cm))

     # Grab data for saving
     Saving<-df %>%
          dplyr::select(c('ID','X','Y','Climate_Envolope',
               'days','years','DBH_cm','Max_DBH'))
     Saving$Species<-as.character(sp)

     # Drop unnecessary variables
     df<-df %>%
          dplyr::select(!c('Country','Adm2','Adm3',
               'months','DBH_cm','Max_DBH')) %>%
          mutate(across(where(is.character), as.factor))

     # Reset factor levels
     df<-df %>%
          mutate(across(where(is.factor),as.character)) %>%
          mutate(across(where(is.character),as.factor))

     # Create historic data frame
     hist_df<-df %>%
          dplyr::select(!c('years','X','Y','ID'))

     # Extract and create future data frame
     admNames<-as.list(unique(df$Adm1))
     adm_list<-list()
     
     # Filter and loop over admin
     for(adm in admNames){
          # Filter points to Admin
          point<-df %>%
               dplyr::filter(Adm1==adm)
          
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
      
               # Convert to sf object
               pnt<-st_as_sf(pnt)

               # Extract future data per variable
               p<-list()
               j<-1
               Clim_names<-names(fut_clim)
               for (file in fut_clim){
                    # Grab name
                    name<-Clim_names[[j]]

                    # Extract values over buffer
                    file<-exact_extract(file,point_1km_Buf,progress=FALSE,'mean')
                    file<-as_tibble(file)
                    colnames(file)<-c(name)

                    # Add to list
                    p[[name]]<-file
                    j<-j+1
                    rm(file,name)
               }
               # Squash list
               Future_Climate_Extract<-as_tibble(do.call(base::cbind,p))
               rm(j,p)
               
               # Drop historic climate data
               pnt<-st_drop_geometry(pnt)
               pnt<-pnt[,c(1:2,23:28)]
                    
               # Merge future climate data
               Future_Climate_Extract<-as_tibble(base::cbind(pnt,Future_Climate_Extract))               

               # Add to list
               year_list[[as.character(year)]]<-Future_Climate_Extract
          }

          # Squash list
          extract<-as_tibble(do.call(base::rbind,year_list))
          adm_list[[adm]]<-extract
     }
     message(paste0("Data for ",sp," has been extracted"))
     rm(admNames,year_list,yearsNames,Clim_names,extract,adm,
          point,point_1km_Buf,year,pnt,df)

     # Merge into extracted database
     Future_Climate_Extract<-as_tibble(do.call(base::rbind,adm_list))
     Future_Climate_Extract<-Future_Climate_Extract[order(
          Future_Climate_Extract$ID),]
     Future_Climate_Extract$ID<-NULL
     rm(adm_list)

     # Load model
     model<-vetiver_pin_read(board, paste0(sp,"_RF_model"))

     # Predict and save
     Hist_days_Predicted<-predict(model,hist_df,type="numeric")
     Hist_days_Predicted<-rename(Hist_days_Predicted,Current_DBH=.pred)
     Fut_days_Predicted<-predict(model,Future_Climate_Extract,type="numeric")
     Fut_days_Predicted<-rename(Fut_days_Predicted,Future_DBH=.pred)
     rm(model)

     # Megre new data frame
     Predictions<-as_tibble(base::cbind(Saving,Hist_days_Predicted,Fut_days_Predicted))
     rm(Hist_days_Predicted,Fut_days_Predicted,hist_df,Saving,Future_Climate_Extract)

     # Fit GAM to future DBH and predict
     fit<-suppressWarnings(mgcv::gam(Future_DBH ~ s(days,bs="cr",k=2),data=Predictions))
     Predictions$GAM_Fut_DBH<-as.numeric(predict(fit,Predictions))
     fit2<-suppressWarnings(mgcv::gam(Current_DBH ~ s(days,bs="cr",k=2),data=Predictions))
     Predictions$GAM_Cur_DBH<-as.numeric(predict(fit2,Predictions))
     P<-ggplot(Predictions,aes(x=days)) +
          geom_point(aes(y=DBH_cm,color="Reference"),show.legend=TRUE) +
          suppressWarnings(geom_smooth(aes(y=DBH_cm,color="Reference"),method="gam",
               formula=y~s(x,bs="cr",k=2),se=FALSE,show.legend=TRUE)) +
          geom_point(aes(y=GAM_Fut_DBH,color="Future predicted"),show.legend=TRUE) +
          suppressWarnings(geom_smooth(aes(y=GAM_Fut_DBH,color="Future predicted"),method="gam",
               formula=y~s(x,bs="cr",k=2),se=FALSE,show.legend=TRUE)) +
          geom_point(aes(y=GAM_Cur_DBH,color="Current predicted"),show.legend=TRUE) +
          suppressWarnings(geom_smooth(aes(y=GAM_Cur_DBH,color="Current predicted"),method="gam",
               formula=y~s(x,bs="cr",k=2),se=FALSE,show.legend=TRUE)) +
          labs(title=paste(sp),x="Days",y="DBH",color="") +
          scale_color_manual(values=c("Future predicted"="red",
               "Current predicted"="blue","Reference"="black"))
     ggsave(paste0(Dir,"Results/Figures/Predicted Growth Curves/",
          sp,".png"),P,width=8,height=6,dpi=300)
     rm(P)

     # Identify the mean DBH incriment per year under future climate
     Predictions$years<-ceiling(Predictions$days/365)
     Increment_list<-list()
     hist_dbh<-0
     for(y in 0:max(unique(Predictions$years))){
          # Check if reference data contains year
          years<-sort(unique(Predictions$years))
          if(y %in% years){
               # Filter to year
               df<-Predictions %>%
                    dplyr::filter(years==y)
          
               # Calculate the mean dbh
               mean_dbh<-df %>%
                    dplyr::summarize(mean_dbh = mean(GAM_Fut_DBH)) %>%
                    dplyr::select(c("mean_dbh"))
          
               # Calculate growth increment
               increment_dbh<-mean_dbh-hist_dbh
               colnames(increment_dbh)<-c("increment_dbh")
               if(increment_dbh<0){
                    increment_dbh=tibble(increment_dbh=0)
               }

               # Add year
               increment_dbh$GrowthYear<-as.numeric(y)

               # Add mean DBH
               increment_dbh$MeanDBH<-as.numeric(mean_dbh)
               increment_dbh<-as.data.frame(increment_dbh)
     
               # Add to list
               Increment_list[[as.character(y)]]<-increment_dbh

               # Update hist_dbh
               hist_dbh<-mean_dbh

               # Clean environment
               rm(df,mean_dbh,increment_dbh,y)
          } else{
               # Impute missing year
               days<-as_tibble((y*365)+365)
               colnames(days)<-"days"
               Impute_Fut_DBH<-as_tibble(predict(fit,newdata=days))
               colnames(Impute_Fut_DBH)<-"Impute_Fut_DBH"

               # Calculate growth increment
               increment_dbh<-Impute_Fut_DBH-hist_dbh
               colnames(increment_dbh)<-c("increment_dbh")
               if(increment_dbh<0){
                    increment_dbh=tibble(increment_dbh=0)
               }

               # Add year
               increment_dbh$GrowthYear<-as.numeric(y)

               # Add mean DBH
               increment_dbh$MeanDBH<-as.numeric(Impute_Fut_DBH)
               increment_dbh<-as.data.frame(increment_dbh)
     
               # Add to list
               Increment_list[[as.character(y)]]<-increment_dbh

               # Update hist_dbh
               hist_dbh<-Impute_Fut_DBH

               # Clean environment
               rm(Impute_Fut_DBH,increment_dbh,y,days)
          }          
     }

     # Squash list
     Increment_DBH_Future<-as_tibble(do.call(base::rbind,Increment_list))
     Increment_DBH_Future$Species<-sp
     Predictions$GAM_Fut_DBH=NULL
     mean<-Increment_DBH_Future %>%
          filter(increment_dbh!=0) %>%
          summarise(mean_increment_dbh=mean(increment_dbh))
     Increment_DBH_Future<-Increment_DBH_Future %>%
          mutate(increment_dbh=ifelse(increment_dbh==0,
               mean$mean_increment_dbh,increment_dbh))
     write_csv(Increment_DBH_Future,paste0(Dir,
          "Results/Predictions/Future Incremental Growth/Increment_DBH_Future_",
          as.character(sp),".csv"))
     rm(hist_dbh,Increment_list,years,mean)

     # Identify the mean DBH incriment per year under current climate
     Increment_list<-list()
     hist_dbh<-0
     for(y in 0:max(unique(Predictions$years))){
          # Check if reference data contains year
          years<-sort(unique(Predictions$years))
          if(y %in% years){
               # Filter to year
               df<-Predictions %>%
                    dplyr::filter(years==y)
          
               # Calculate the mean dbh
               mean_dbh<-df %>%
                    dplyr::summarize(mean_dbh = mean(GAM_Cur_DBH)) %>%
                    dplyr::select(c("mean_dbh"))
          
               # Calculate growth increment
               increment_dbh<-mean_dbh-hist_dbh
               colnames(increment_dbh)<-c("increment_dbh")
               if(increment_dbh<0){
                    increment_dbh=tibble(increment_dbh=0)
               }

               # Add year
               increment_dbh$GrowthYear<-as.numeric(y)

               # Add mean DBH
               increment_dbh$MeanDBH<-as.numeric(mean_dbh)
               increment_dbh<-as.data.frame(increment_dbh)
     
               # Add to list
               Increment_list[[as.character(y)]]<-increment_dbh

               # Update hist_dbh
               hist_dbh<-mean_dbh

               # Clean environment
               rm(df,mean_dbh,increment_dbh,y)
          } else{
               # Impute missing year
               days<-as_tibble((y*365)+365)
               colnames(days)<-"days"
               Impute_Cur_DBH<-as_tibble(predict(fit2,newdata=days))
               colnames(Impute_Cur_DBH)<-"Impute_Cur_DBH"

               # Calculate growth increment
               increment_dbh<-Impute_Cur_DBH-hist_dbh
               colnames(increment_dbh)<-c("increment_dbh")
               if(increment_dbh<0){
                    increment_dbh=tibble(increment_dbh=0)
               }

               # Add year
               increment_dbh$GrowthYear<-as.numeric(y)

               # Add mean DBH
               increment_dbh$MeanDBH<-as.numeric(Impute_Cur_DBH)
               increment_dbh<-as.data.frame(increment_dbh)
     
               # Add to list
               Increment_list[[as.character(y)]]<-increment_dbh

               # Update hist_dbh
               hist_dbh<-Impute_Cur_DBH

               # Clean environment
               rm(Impute_Cur_DBH,increment_dbh,y,days)
          }          
     }

     # Squash list
     Increment_DBH_Current<-as_tibble(do.call(base::rbind,Increment_list))
     Increment_DBH_Current$Species<-sp
     Predictions$GAM_Cur_DBH=NULL
     mean<-Increment_DBH_Current %>%
          filter(increment_dbh!=0) %>%
          summarise(mean_increment_dbh=mean(increment_dbh))
     Increment_DBH_Current<-Increment_DBH_Current %>%
          mutate(increment_dbh=ifelse(increment_dbh==0,
               mean$mean_increment_dbh,increment_dbh))
     write_csv(Increment_DBH_Current,paste0(Dir,
          "Results/Predictions/Current Incremental Growth/Increment_DBH_Currente_",
          as.character(sp),".csv"))
     rm(hist_dbh,Increment_list,years,mean)
     
     # Calculate the projected tree growth 20 years into the future
     Predictions<-Predictions %>%
          dplyr::filter(DBH_cm>0)
     Predictions_list<-split(as.data.frame(Predictions),seq(nrow(Predictions)))
     New_list<-list()
     x<-1
     for(df in Predictions_list){
          # ID reference DBH
          Ref_DBH<-df$DBH_cm
          Ref_Fut_DBH<-df$Future_DBH
          Ref_max_DBH<-df$Max_DBH

          # Match to MeanDBH in Increment_DBH_Future
          closest_index<-Increment_DBH_Future$MeanDBH[which.min(abs(
               Increment_DBH_Future$MeanDBH-Ref_Fut_DBH))]
          Increment_DBH_Future_subset<-subset(Increment_DBH_Future,
               MeanDBH>=as.numeric(closest_index))
          
          # Clamp to max 20 years
          if(nrow(Increment_DBH_Future_subset)<20){
               # Count rows
               n<-nrow(Increment_DBH_Future_subset)
               n<-20-n

               # Calculate mean increment
               m_inc<-mean(Increment_DBH_Future_subset$increment_dbh)

               # Grab GrowthYear
               gYear<-max(Increment_DBH_Future_subset$GrowthYear)+1

               # Create tibble to fill up to 20 years
               fill_tib<-tibble(increment_dbh=rep(m_inc,n)) %>%
                    mutate(GrowthYear=gYear+row_number()-1) %>%
                    mutate(MeanDBH=Ref_max_DBH) %>%
                    mutate(Species=as.character(sp))
               
               # Merge
               Increment_DBH_Future_subset<-rbind(
                    Increment_DBH_Future_subset,
                    fill_tib)
               rm(n,m_inc,gYear,fill_tib)               
          } else{
               Increment_DBH_Future_subset<-slice_head(
                    Increment_DBH_Future_subset,n=20)
          }

          # Per year, add incremental growth, calculate years at max
          Years_Max<-0
          for(i in 1:nrow(Increment_DBH_Future_subset)){
               increment<-Increment_DBH_Future_subset[i,1]
               Ref_DBH_incr<-Ref_DBH+increment
               if(Ref_DBH_incr>=Ref_max_DBH){
                    Years_Max<-Years_Max+1
                    Ref_DBH_incr<-Ref_DBH
               }
               Ref_DBH<-Ref_DBH_incr
          }

          # Merge and add to list
          df$Years_Max<-as.numeric(Years_Max)
          df$Ref_DBH_incr<-as.numeric(Ref_DBH_incr)
          New_list[[x]]<-df
          x<-x+1

          # Clean environment
          rm(Increment_DBH_Future_subset,Ref_DBH,Ref_max_DBH,
               Years_Max,Ref_DBH_incr,i,df,increment)
     }

     # Squash list
     Predictions<-as_tibble(do.call(base::rbind,New_list))
     rm(New_list,x,Predictions_list,Increment_DBH_Future,Ref_Fut_DBH)
     message(paste0("Future growth for ",sp," has been calculated"))

     # Rename and reorder columns
     Predictions<-Predictions %>%
          dplyr::select(c('X','Y','Species','days','years','DBH_cm','Max_DBH',
               'Current_DBH','Future_DBH','Years_Max','Ref_DBH_incr'))
     colnames(Predictions)<-c('X','Y','Species','Age_days','Age_years',
          'Ref_DBH','Max_DBH','Pred_hist_DBH','Pred_Fut_DBH',
          'Pred_Years_Max_DBH','Pred_fut_growth')
     
     # Add to list
     Predicted_list[[sp]]<-Predictions
     rm(Predictions,sp,fit,closest_index)
}

# Squash list
Extracted<-as_tibble(do.call(base::rbind,Predicted_list))
Extracted$Max_DBH<-round(Extracted$Max_DBH,1)
Extracted$Pred_hist_DBH<-round(Extracted$Pred_hist_DBH,1)
Extracted$Pred_Fut_DBH<-round(Extracted$Pred_Fut_DBH,1)
Extracted$Pred_fut_growth<-round(Extracted$Pred_fut_growth,1)
rm(Predicted_list,board,Species,db,fut_clim)

# ID slow and fast growth
Extracted$GrowthResponse<-Extracted$Pred_Fut_DBH-Extracted$Pred_hist_DBH

# Make spatial
Extracted_sf<-st_as_sf(Extracted,coords=c("X","Y"),crs=4326)

# Save to disk
write_csv(Extracted,paste0(Dir,"Results/Predictions/DBH_Predictions.csv"))
st_write(Extracted_sf,paste0(Dir,"Results/Predictions/DBH_Predictions.gpkg"),quiet=TRUE)
rm(Extracted_sf,fit2,Increment_DBH_Current,Extracted)

#### Validate predicted incremental growth ####
# Load predicted growth
Extracted<-read_csv(paste0(Dir,"Results/Predictions/DBH_Predictions.csv"),
     col_names=TRUE,show_col_types=FALSE)

# Load historic DBH measurements
hist_dbh<-read_csv(paste0(Dir,
     "Data/arbres-dhp.csv"),
     col_names=TRUE,show_col_types=FALSE) %>%
     dplyr::mutate(ID=paste0(RAD_TYPE,RAD_EMP_NO)) %>%
     dplyr::select(-c(RAD_TYPE,RAD_EMP_NO)) %>% 
     dplyr::filter(RAD_DATE_PRISE>"1900-01-01" & RAD_DATE_PRISE<"2023-06-30") %>%
     dplyr::filter(!RAD_DHP<3) %>%
     dplyr::filter(RAD_DHP<200) %>%
     distinct()

# Remove individuals with single observation
hist_dbh<-hist_dbh %>%
     group_by(ID) %>%
     filter(n()>=2) %>%
     ungroup()

# Grab unique names
hist_sp<-as_tibble(unique(hist_dbh$ESSENCE_LATIN)) %>%
     rename(ESSENCE_LATIN=value) %>%
     mutate(ESSENCE_LATIN=str_replace_all(
          ESSENCE_LATIN,"'",""))
Ext_sp<-as_tibble(unique(Extracted$Species)) %>%
     rename(Species=value)
rm(Extracted)

# Match names
Fuzzy_match<-hist_sp %>%
  stringdist_left_join(Ext_sp,
     by=c(ESSENCE_LATIN="Species"),
     max_dist=1) %>%
     drop_na()
rm(hist_sp,Ext_sp)

# Filter historic DBH measurements
hist_dbh<-hist_dbh %>%
     mutate(ESSENCE_LATIN=str_replace_all(
          ESSENCE_LATIN,"'","")) %>%
     subset(ESSENCE_LATIN %in% as.character(
          Fuzzy_match$ESSENCE_LATIN))

# Correct names
hist_dbh<-hist_dbh %>%
     left_join(Fuzzy_match,by="ESSENCE_LATIN") %>%
     mutate(ESSENCE_LATIN=Species) %>%
     dplyr::select(-Species)

# Prepare data for calculating mean DBH per year
prep<-hist_dbh %>%
     group_by(ID) %>%
     mutate(min_date=min(RAD_DATE_PRISE),
          max_date=max(RAD_DATE_PRISE),
          min_dbh=min(RAD_DHP),
          max_dbh=max(RAD_DHP),
          growth_cm=max_dbh-min_dbh) %>%
     ungroup() %>%
     dplyr::select(ID,ESSENCE_LATIN,min_date,max_date,
          max_dbh,min_dbh,growth_cm) %>%
     distinct() %>%
     dplyr::filter(growth_cm>0)
prep$growth_years<-abs(trunc((prep$min_date %--% prep$max_date)/years()))
prep$days<-abs(trunc((prep$min_date %--% prep$max_date)/days()))
prep<-prep %>%
     dplyr::filter(days>0 & growth_years>0)

# Calculate mean DBH growth per year
Ref_dbh<-prep %>%
     mutate(mean_DBH=growth_cm/growth_years) %>%
     group_by(ESSENCE_LATIN) %>%
     mutate(mean_DBH=mean(mean_DBH)) %>%
     dplyr::select(ESSENCE_LATIN,mean_DBH) %>%
     ungroup() %>%
     distinct()     
rm(Fuzzy_match,hist_dbh)

# Load predicted growth increments
csv_files_list<-list()
csv_files<-list.files(path=paste0(Dir,
     "Results/Predictions/Current Incremental Growth/"),
     pattern="\\.csv$",full.names=TRUE)
for (file in csv_files) {
     file_name<-basename(file)
     pattern<-"^Increment_DBH_Currente_(.*?)\\.csv$"
     file_name<-gsub(pattern,"\\1",file_name)
     csv_files_list[[file_name]]<-read_csv(file,show_col_types=FALSE)
}
rm(file,file_name,pattern,csv_files)

# Remove unmatched species
match<-unique(prep$ESSENCE_LATIN)
csv_files_list<-csv_files_list[intersect(names(csv_files_list),match)]
rm(match,prep)

# Per species calculate mean dbh growth
mean_list<-list()
mean_list<-lapply(csv_files_list,function(df) {
     mean_increment<-mean(df$increment_dbh,na.rm=TRUE)
     species<-unique(df$Species)  
     result_df<-data.frame(Species=species,
          Mean_Increment_DBH=mean_increment)
     return(result_df)
})
Pred_dbh<-as_tibble(do.call(base::rbind,mean_list))
rm(mean_list,csv_files_list)

# Merge and save table
Ref_dbh<-Ref_dbh %>%
     rename(Species=ESSENCE_LATIN,Ref_mean_dbh=mean_DBH)
Pred_dbh<-Pred_dbh %>%
     rename(Pred_mean_dbh=Mean_Increment_DBH)
Val_dbh<-left_join(Ref_dbh,Pred_dbh,by="Species")
Val_dbh<-mutate(Val_dbh,Difference=Pred_mean_dbh-Ref_mean_dbh)
write_csv(Val_dbh,paste0(Dir,"Results/Tables/Validation_DBH.csv"))
rm(Pred_dbh,Ref_dbh,Val_dbh)

#### Boxplots for Random Forest error per DBH bin ####
# Load predicted growth
predictions<-read_csv(paste0(Dir,
     "Results/Predictions/DBH_Predictions.csv"),
     col_names=TRUE,show_col_types=FALSE)

# Create binned DBH
predictions$DBH_bin<-ceiling(predictions$Ref_DBH/5)*5
predictions$DBH_bin<-paste0(predictions$DBH_bin,"cm")

# Add ID column
predictions$ID<-1:nrow(predictions)

# Per bin DBH calculate error in RF prediction
for(sp in as.list(as.character(unique(predictions$Species)))){
     # Filter to bin and select columns
     df<-predictions %>%
          subset(Species==sp) %>%
          dplyr::select(c("ID","DBH_bin",
               "Ref_DBH","Pred_hist_DBH")) %>%
          arrange(DBH_bin)

     # Create x axis for ordering
     df<-df %>%
          mutate(DBH_order=as.numeric(
               sub("cm$","",DBH_bin))) %>%
          arrange(DBH_order)

     # Calculate difference between predicted and reference DBH
     df$Error<-df$Ref_DBH-df$Pred_hist_DBH

     # Per DBH bin, produce boxplot, save to disk  
     p<-ggplot(df,aes(x=DBH_order,y=Error,group=DBH_order)) +
          geom_boxplot() +
          labs(x="DBH Bin cm",y="Prediction error") +
          scale_x_continuous(n.breaks=length(unique(df$DBH_bin))) +
          ggtitle(paste0("Error distribution by binned DBH for ",sp))
     ggsave(paste0(Dir,"Results/Figures/RF error boxplots/",
          sp,"_Prediction_Error.png"),p,width=10,height=8,dpi=300)

     message(as.character(sp)," is completed")
     # Clean environment
     rm(df,p,sp)
}

# Clean environment
rm(Dir,predictions)

#### End of script ####