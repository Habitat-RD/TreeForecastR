################################################################################
## 004
## Growth curves
################################################################################
#date: 23-02-2023
#author: Jurie Theron

#### Set the working directory ####
# Load all the relevant project libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
})

# Set working directory
Dir<-"Set/your/home/path/here/"

#### Load data ####
# Load file
db<-read_csv(paste0(Dir,"Results/Tables/Urban_Tree_Growth_ExpVar_Clean_Database.csv"),
  col_names=TRUE,show_col_types=FALSE)

# Create directories
if (!dir.exists(paste0(Dir,"Results/Figures/Growth Curves/"))){
  dir.create(paste0(Dir,"Results/Figures/Growth Curves/"))
}

#### Producing growth curves ####
# Loop and plot growth curves
for(sp in as.list(as.character(unique(db$Species)))){
  tree<-db %>%
    dplyr::filter(Species==sp)
  p<-ggplot(tree,aes(x=days,y=DBH_cm)) +
    geom_point() + 
    suppressWarnings(geom_smooth(method="gam",formula=y~s(x,bs="cr",k=2))) +
  labs(title=paste(sp),x="Days since planting",y="DBH cm") +
  theme(text=element_text(size=14))
  ggsave(paste0(Dir,"Results/Figures/Growth Curves/",
    sp,".png"),p,width=6,height=4,dpi=300)
}
rm(tree,p,sp,db,Dir)

#### End of script ####