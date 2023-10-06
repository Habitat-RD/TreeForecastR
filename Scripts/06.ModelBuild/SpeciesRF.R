################################################################################
## 005
## Random forest models
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
     library(VSURF)
     library(tidymodels)
     library(doParallel)
     library(vip)
     library(vetiver)
     library(pins)
     library(ranger)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load file
db<-read_csv(paste0(Dir,"Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Create directories
if (!dir.exists(paste0(Dir,"Results/Models/"))){
  dir.create(paste0(Dir,"Results/Models/"))
}
if (!dir.exists(paste0(Dir,"Results/Models/Products/"))){
  dir.create(paste0(Dir,"Results/Models/Products/"))
}

#### Preprocessing data ####
# Drop unnecessary variables
db<-db %>%
     dplyr::select(!c('Country','Adm2','Adm3','months','years',
          'X','Y','Climate_Envolope','Adm1')) %>%
     mutate(across(where(is.character), as.factor))

# Set up board for saving
board<-board_folder(paste0(Dir,"Results/Models/Products/"),versioned=TRUE)

#### Loop Random Forest model ####
# Per species build random forest model
Freq<-table(db$Species) %>%
     as.data.frame()
Species<-as.list(as.character(unique(db$Species)))
# Loop over species
for(sp in Species){
     # Filter
     df<-dplyr::filter(db,Species==sp) %>%
          dplyr::select(!c("Species"))
     message(paste0("Starting with ",sp))

     # Subset for variable selection
     set.seed(44345)
     Frequ<-dplyr::filter(Freq,Var1==sp) %>%
          dplyr::select(Freq) %>%
          as.numeric()
     if(Frequ>3000){
          sub<-df %>%          
               dplyr::sample_n(size=3000)
     }else{
          sub<-df
     }
     rm(Frequ)

     # Variable selection
     climate<-sub[,c(1,3:21)]
     selection<-suppressWarnings(VSURF(DBH_cm~.,data=climate,verbose=FALSE))
     sel<-sort(selection[["varselect.pred"]])
     
     # Select important climate variables
     climate<-df[,c(3:21)]
     df<-df[,c(1:2,22:25)]
     climate<-climate %>%
          dplyr::select(all_of(c(sel)))

     # Merge data frames
     df<-as_tibble(base::cbind(df,climate))
     rm(selection,sel,sub,climate)
     message(paste0("Climate variable selection done for: ",sp))

     # Reset factor levels
     df<-df %>%
          mutate(across(where(is.factor),as.character)) %>%
          mutate(across(where(is.character),as.factor))

     # Split data
     set.seed(501)
     split<-initial_split(df,strata=Urban_Climate_Zone)
     train<-training(split)
     test<-testing(split)
     rm(df)
     message(paste0("Data splitting done for: ",sp))

     # Create recipe
     Recipe<-recipe(DBH_cm~.,data=train) %>%
          step_novel(Urban_Climate_Zone,Soil_Texture,-all_outcomes()) %>%
          step_normalize(all_numeric(),-all_outcomes())

     # Set up validation
     set.seed(234)
     fold<-vfold_cv(train,v=5,strata=Urban_Climate_Zone)

     # Set up model
     rf_spec<-rand_forest(
          mtry=parsnip::tune(),
          min_n=parsnip::tune(),
          trees=1000) %>%
          set_engine("ranger",importance="impurity") %>%
          set_mode("regression")
     
     # Set up workflow
     rf_wflow<-workflow() %>%
          add_recipe(Recipe,blueprint=hardhat::default_recipe_blueprint(
               allow_novel_levels=TRUE)) %>%
          add_model(rf_spec)
  
     # Set up tuning grid
     rf_grid<-grid_regular(
          min_n(),
          finalize(mtry(),dplyr::select(train,-DBH_cm)),
          levels=5)
     message(paste0("Model set up done for: ",sp))

     # Model tuning
     set.seed(5568)
     doParallel::registerDoParallel(cores=5)
     rf_res<-tune_grid(
          rf_wflow,
          resamples=fold,
          metrics=metric_set(rmse,mae,rsq,ccc),
          control=control_resamples(save_pred=TRUE),
          grid=rf_grid)
     stopImplicitCluster()
     rf_res %>% 
          collect_metrics(summarize=TRUE) %>% 
          as.data.frame() %>%
          write.csv(paste0(Dir,"Results/Models/Products/",sp,"_RF_tuning_metrics.csv"),row.names=FALSE)
     message(paste0("Model tunning done for: ",sp))

     # Select best parameters for final model
     rf_best<-select_best(rf_res,"mae")
     rf_final<-finalize_workflow(rf_wflow,rf_best)
     rf_last_fit<-last_fit(rf_final,split,metrics=metric_set(rmse,mae,rsq,ccc))
     rm(rf_best,rf_final,fold,rf_grid,rf_res,rf_spec,rf_wflow)
     message(paste0("Final model done for: ",sp))
  
     # Final model performance
     rf_last_fit %>%
          collect_metrics() %>%
          as.data.frame() %>%
          write.csv(paste0(Dir,"Results/Models/Products/",sp,"_RF_final_metrics.csv"))
     
     # Create fitted model object to save
     rf_wf_model<-extract_workflow(rf_last_fit)
     Fitted_model<-vetiver_model(rf_wf_model,
          paste0(sp,"_RF_model"),
          metadata=list(metrics=rf_last_fit %>%
               collect_metrics() %>%
               dplyr::select(-.config)))

     # Version and save     
     suppressMessages(board %>% vetiver_pin_write(Fitted_model))
     message(paste0("Model saved for: ",sp))
  
     # Variable importance
     rf_last_fit %>% 
          pluck(".workflow",1) %>%
          extract_fit_parsnip() %>%
          vip()
     ggsave(paste0(Dir,"Results/Models/Products/",sp,"_RF_VarImp.png"),
          units="mm",width=150,height=110,dpi=300)
     
     # Clean environment
     rm(rf_last_fit,split,test,train,Fitted_model,Recipe,rf_wf_model,sp)
}

# Clean script
rm(db,board,Dir,Species,Freq)

#### End of script ####