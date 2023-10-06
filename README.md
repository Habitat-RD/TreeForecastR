<img align="centre" width="1100" height="240" src="https://github.com/Habitat-RD/TreeForecastR/blob/main/docs/Habitat_banner.png?raw=true">

# Generalized urban tree growth 

Author: Kasselman Jurie Theron

Contributors: Sylvia Wood, Éva Delmas, Kyle Martins, Françoise Vanoverbeke, Eugénie Desrousseaux

Contact: info@habitat-nature.com

# Context and problem

To effectively manage the urban canopy into the future and under different scenarios of climate, urban forest managers need information on expected tree growth rates and performance. This information is currently lacking in the literature. To address this gap we develop a tool, `TreeForecastR`, to estimate species-specific growth rates of urban trees and project their expected future growth under different climate scenarios. 

The approach involves analyzing generalized growth patterns of various tree species across multiple cities and geographies. By doing so, valuable insights into the adaptive capabilities of urban trees can be gained and provide researchers and forestry professionals with valuable insights to promote the development of resilient urban tree infrastructure, thereby safeguarding the many benefits urban trees bestow upon cities and their inhabitants.

# TreeForecastR description

The `TreeForecastR` tool estimates urban tree species growth rates based on machine learning (`Random Forest`) and draws on an ever expanding database of collected and cleaned municipal tree inventory databases. The tool combines urban tree inventories, for which information on individual tree age is recorded, along with a number of common environmental variables to predict annual growth rates under current and future climate scenarios using WorldClim.  

The `TreeForecastR` tool was developed and made available with funding from the Quebec Ministry of Economy and Innovation. In this repository we provide the code developed for this project workflow with a dataset based on the public inventory for Montreal for users to test the tool. This tool was built in the R programming language.


# Workflow

The tool can be summarized into the following workflow.

## <u> Inventory cleaning </u>
The workflow begins by cleaning the urban tree inventory (in this case the Montreal inventory) to form the basis of a comprehensive database. The data is preprocessed by cleaning tree species names and removing erroneous dates and DBH values. This database includes essential information such as tree species (`species`), diameter at breast height (`dbh`), tree age (`days`), and GPS coordinates (`X`,`Y`). The cleaned inventory is located in the `Data` folder of this repository. Tree age is measured as the number of days since the tree was planted and when the `dbh` measurement was taken. The cleaned dataset is provided in the repository (`Data/Urban_Tree_Growth.csv`).

## <u> Input data extraction and preprocessing </u>
Environmental covariates from sources such as `Google Earth Engine` and `WorldClim` are added to the dataset. Specifically, covariates that influence tree growth were selected which include the WorldClim bioclimatic variables, soil compaction and texture, urban climate zones, climate envelope, and human population. All environmental covariates are extracted for each tree location within the database using buffers of different sizes depending on the resolution of the covariate. We recommend that users employ an exploratory data analysis step to better understand the distribution of data before variable selection and model building to detect and remove outliers. Finally, it is recommended that the  `dbh` and `days` relationship is smoothed as a final preprocessing step.

## <u> Variable selection and model building </u>
Before building predictive `Random Forest` models of species-specific growth rates, variable selection should be performed to identify the most influential environmental covariates to reduce the complexity of the model and to ensure the model learns from influential covariates only. Hyperparameters can be tuned, and the `Random Forest` models can be validated to select the best performing models for prediction. Fine-tuned and validated `Random Forest` models are then used for predicting DBH. This is performed separately for each species.

## <u> Automatic species removal </u>
To ensure reliable model predictions, species with low observations, low covariate variability, and weak model performance, are automatically identified by the tool and removed before making predictions. Thus, only species with high model accuracies and reliable data are used to make predictions. For selected tree species, two products are generated using their respective models and described below. The metrics used to identify which species to remove can be changed before creating products (see `Scripts/08.GrowthPredict/SpeciesPredict.R` and `Scripts/09.PrepGrid/ToGrid.R`).

## Product 1: Predicting future growth per individual </u>
The first product predicts `dbh` using historical and future climate data per individual tree. `dbh` predictions under historic and future climate provides a useful indicator of how an individual tree will respond under future climate conditions (good vs bad growing conditions). These predictions are then used in a Generalized Additive Model (`GAM`) to determine annual incremental growth, which provides information on the amount of growth an individual tree will experience under future climate conditions given its current location. In this example, the predicted annual growth is validated using historic `dbh` measurements from Montreal.

## <u> Product 2: Predicting DBH distribution per species </u>
The second product involves predicting `dbh`  based on the mean `age` of the species across a given region of interest, using historical and future climate data. This produces a tree growth distribution map, highlighting areas where a particular species will thrive or experience slow or stunted growth. By comparing historic and future tree growth distribution maps, we can identify suitable locations for replanting specific species or areas where certain species might face significant climatic stress.

![alt text](https://github.com/Habitat-RD/TreeForecastR/blob/main/docs/UrbTreeGrowthFlowMap.JPG?raw=true)

# Products

Below is an example of the two different outputs this workflow produces.

## Product 1: Incremental growth under future climate
It uses a predicted growth curve on future climate to calculate the summed annual incremental growth of an individual tree. By comparing the predicted future and historic growth curves we can visualize whether an individual tree will thrive under the future climate, or experience climate stress. Red indicates climate stress whereas green indicates favorable growth.

![alt text](https://github.com/Habitat-RD/TreeForecastR/blob/main/docs/Product1.png?raw=true)

## Product 2: Diameter at breast height distribution map
It extends the model predictions over a large region of interest. The amount of days used to make predictions are standardized per species. Darker greens indicate higher `dbh` values attributed to the environmental covariates.

### Historic DBH distribution map for Acer saccharinum

![alt text](https://github.com/Habitat-RD/TreeForecastR/blob/main/docs/Product2_hist.png?raw=true)

### Future DBH distribution map for Acer saccharinum

![alt text](https://github.com/Habitat-RD/TreeForecastR/blob/main/docs/Product2_fut.png?raw=true)

# Funding

This research is funded by the Quebec Ministry of Economy and Innovation with grant reference number 55453.

# Data sources

This workflow makes use of the [WorldClim](https://worldclim.org/) and [GADM](https://gadm.org/index.html) data, which is for research use only. Other datasets used in this project are licensed as follows:
* [OpenLandMap Soil Data](https://zenodo.org/record/2525665) [CC-BY-SA-4.0](https://creativecommons.org/licenses/by-sa/4.0/)
* [Global map of Local Climate Zones](https://zenodo.org/record/6364594) [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)
* [Global 1‑km climate classification maps](https://www.gloh2o.org/koppen/) [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)
* [Montreal urban tree inventory](https://www.donneesquebec.ca/recherche/dataset/vmtl-arbres) [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)

# License

This project is released under the [GPL3](https://www.gnu.org/licenses/gpl-3.0.en.html) license. Copyright © 2023 Habitat
