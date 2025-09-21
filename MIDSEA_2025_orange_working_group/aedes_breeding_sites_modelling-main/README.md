# Exploring links between Aedes mosquito breeding sites and heterogeneous urban landscapes variables from fine-scale satellite-derived data 

## Description
This project gather the data and the scripts needed to perform a Multiple Factor Analysis over potential breeding sites variables and urban landscapes variables in order to explor the links between them. It also allows to perform random forests to predict potential breeding sites over French Guina.

## Usage
In data directory, you will find

"input_grid_variables.gpkg" is the grid with :
- all response variables (all potential breeding sites and each types or categories)
- all explanatory variables (texture, heights, spectra, landscape metrics for vegetation, landscape metrics for buildings, building information)

"grid_for_prediction" is the grid over all Cayenne Island with :
- all explanatory variables (texture, heights, spectra, landscape metrics for vegetation, landscape metrics for buildings, building information)

To run the code, you need to change the path to access the file directly in the script

###  For Multiple Factor Analysis and Random Forest
Change the path to load the input gpkg file "input_grid_variables.gpkg"  
- in AFM.R  (line 24)
- in RF_models_absolute_potential breeeding sites_predictions.R (line 23)
- in RF_models_normalize_potential breeeding sites.R   (line 24)

###  For prediction
Change the path to load the gpkg file "grid_for_prediction"  
- in RF_models_prediction_absolute_potential breeeding sites.R  (line 384)
Change the path to save the results of the prediction in line 423 


"grid_results_prediction" is the grid over all Cayenne Island with results of the article. 

## Support

Claire Teillet @claire.teillet@ird.fr
