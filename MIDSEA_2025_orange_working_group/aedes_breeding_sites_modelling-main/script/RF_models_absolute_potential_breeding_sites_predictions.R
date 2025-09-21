## Script to do Random Forest models with potential breeding sites variables and urban landscapes variables and predictions
## Author : Claire Teillet

library(caret)
library(ggplot2)
library(dplyr)
library(esquisse) 
library(Metrics) # for rmse 
library(caret) # for trainControl
library(MASS)
library(sf)
library(gridExtra)
library(FactoMineR) # afm
library(explor)
library("factoextra")
library(randomForest)
library(mgcv)
library(reshape2) 
library(spdep) # autocorrelation spatiale
library(RColorBrewer) # colors

# import data with response and explained variables
path = "MIDSEA_2025_orange_working_group/aedes_breeding_sites_modelling-main/data/input_grid_variables.gpkg"
data_brut<- read_sf("MIDSEA_2025_orange_working_group/aedes_breeding_sites_modelling-main/data/input_grid_variables.gpkg")

#### MODELS ####

# selection of grid cell where more than 5 houses where visited
data_subset <- subset(data_brut, nb_vd > 5)

#remove geometry  
data_subset <- st_drop_geometry(data_subset)

## choose variables response to test (can separate absolute and norm)
variables_reponse = c(
  "nb_potential_bs", 
  "nb_barrels_pbs",
  "nb_containers_under_200_L_pbs",
  "nb_containers_over_200_L_pbs",
  "nb_wells_pbs",
  "nb_green_plants_pbs",
  "nb_other_plants_pbs",
  "nb_large_volumes_pbs",
  "nb_tires_pbs",
  "nb_small_waste_pbs",
  "nb_big_waste_pbs",
  "nb_watercraft_pbs",
  "nb_buildings_materials_pbs",
  "nb_manholes_pbs",
  "nb_protection_pbs",
  "nb_troughs_pbs",
  "nb_pit_pbs",
  "nb_canals_pbs",
  "nb_gutters_pbs",
  "nb_others_pbs",
  "nb_containers_pbs",
  "nb_plants_pbs",
  "nb_waste_pbs",
  "nb_WMI_pbs"
)

# making groups of explanatory variables
text <- c("~ C1B201_mean + C2B201_mean + C3B201_mean")
hauteur <- c("~ h_vegetation_mean + h_buildings_mean") 
lm_veget <- c("~ lm_vegetation_ca + lm_vegetation_contig_mn + lm_vegetation_cpland + lm_vegetation_lpi + lm_vegetation_lsi + lm_vegetation_pd + lm_vegetation_shape_mn" )
lm_bati <- c("~ lm_buildings_ca + lm_buildings_contig_mn + lm_buildings_cpland + lm_buildings_lpi + lm_buildings_lsi + lm_buildings_pd + lm_buildings_shape_mn")
bati <- c("~ number_buildings + mean_size_buildings") 
spectral <- c("~ ndvi_P_mean + ndwi_P_mean")

all <- c("~ C1B201_mean + C2B201_mean + C3B201_mean + h_vegetation_mean + h_buildings_mean +
lm_vegetation_ca + lm_vegetation_contig_mn + lm_vegetation_cpland + lm_vegetation_lpi + lm_vegetation_lsi + lm_vegetation_pd + lm_vegetation_shape_mn +
lm_buildings_ca + lm_buildings_contig_mn + lm_buildings_cpland + lm_buildings_lpi + lm_buildings_lsi + lm_buildings_pd + lm_buildings_shape_mn +
number_buildings + mean_size_buildings + ndvi_P_mean + ndwi_P_mean")

# selected variables defined with dashed line represented the mean of each contribution axis, combine with variable above cos2 > 0.1
# afm  <- c("~ C1B201_mean + h_vegetation_mean + ndwi_P_mean + ndvi_P_mean + lm_vegetation_cpland + lm_vegetation_lsi + lm_buildings_ca  + lm_buildings_lsi + number_buildings") # selection contributive mean + seuil 0.1

# selected variables with only one variable per group of variables
afm  <- c("~ lm_buildings_ca + number_buildings + lm_vegetation_lpi + ndvi_P_mean + ndwi_P_mean + C1B201_mean + h_vegetation_mean")

# variables to test in separate group
variables_explicative <- c(text,hauteur,lm_veget, lm_bati, bati, spectral, afm , all)

variables_explicative_nom <- c("texture", "heights", "landscape metrics for vegetation", "landscape metrics for buildings", 
                                "buildings information", "spectral", "MFA variables", "All variables")

# create iterations
### set seeds
set.seed(123)
seeds=round(runif(100)*1000)
# seeds_save <- seeds
# write.table(seeds_save, "G:/0_PHD/RECHERCHE/AXE_2/random_seeds_1.txt")
# seeds <- read.table("G:/0_PHD/RECHERCHE/AXE_2/random_seeds_1.txt")
# seeds <- as.list(seeds)

# create results array with the dimensios of response variables and explanatory variables and number of seed (100)
resultats_array<-array(dim=c(length(variables_reponse)*length(variables_explicative), 9, length(seeds)))

#### MAIN LOOP #### 

train_effectifs_par_seed=c()
test_effectifs_par_seed=c()

for (s in (1:length(seeds))){
  
  # empty data frame to put results
  resultats <- data.frame(
    variable_reponse = character(0),
    r2 = numeric(0),
    r = numeric(0),
    rmse = numeric(0),
    rmse_normalized_maxmin = numeric(0),
    rmse_normalized_sd = numeric(0),
    rmse_normalized_quantile = numeric(0),
    effectif_entrainement = numeric(0),
    effectif_validation = numeric(0),
    range = numeric(0),
    variable_explicative = character(0)
  )
  
set.seed(seeds[s])
    # cross validation
    train_index <- sample(1:nrow(data_subset), 0.7 * nrow(data_subset))
    
    train_data <- data_subset[train_index, ]
    test_data <- data_subset[-train_index, ]  
    
    train_effectifs_par_seed=rbind(train_effectifs_par_seed, apply(train_data,2,function(x){sum(x>0)}))
    test_effectifs_par_seed=rbind(test_effectifs_par_seed, apply(test_data,2,function(x){sum(x>0)}))
    
j=1


for (y in variables_explicative){ 
  for (i in variables_reponse) {
    print(i)
    
    ## formula
    formula <- as.formula(paste(i, y))
    
    # Random forest model :
    model <- randomForest(formula, data= train_data, importance = TRUE, na.action=na.roughfix,ntree=500)
    
    # Generalized linear model :
    # model <- glm(formula, data=train_data, family= poisson)
    # use the model to predict over the test dataset
    predictions <- predict(model, newdata = test_data)
    predictions <- as.numeric(predictions)
    
    # calculate R²
    r <- cor.test(test_data[[i]], predictions)
    r2 <- r$estimate^2
    
    #get observed value from test dataset
    obs <- test_data[[i]]
    obs <- as.numeric(unlist(obs))
    
    # calculate RMSE (Root Mean Squared Error)
    rmse <- RMSE(predictions, obs, na.rm=TRUE)
    
    # calcule the range of the response variables in the training data
    response_range <- max(train_data[[i]]) - min(train_data[[i]])
    response_sd <- sd(train_data[[i]])
    response_quantile <- (quantile(train_data[[i]], 0.75) - quantile(train_data[[i]], 0.25))
    
    #normalized RMSE
    rmse_normalized_maxmin <- rmse / response_range
    rmse_normalized_sd <- rmse / response_sd
    rmse_normalized_quantile <- rmse / response_quantile

    
    # fill the empty results data frame
    resultats <- rbind(resultats, data.frame(
      variable_reponse = i,
      r2 = r2,
      r = r$estimate,
      rmse = rmse, 
      rmse_normalized_maxmin = rmse_normalized_maxmin,
      rmse_normalized_sd = rmse_normalized_sd,
      rmse_normalized_quantile = rmse_normalized_quantile,
      effectif_entrainement = dim(train_data)[1],
      effectif_validation = dim(test_data)[1], 
      range = response_range,
      variable_explicative = variables_explicative_nom[j]
    ))
    
  }
  j=j+1
}

resultats_array[,,s]<-as.matrix(resultats[,(2:10)])

}


# filter to keep only response variables 
train_effectifs_par_seed_filtered <- train_effectifs_par_seed[, variables_reponse, drop = FALSE]
test_effectifs_par_seed_filtered <- test_effectifs_par_seed[, variables_reponse, drop = FALSE]

########### FILTERING  #####
# BY SUM OF POTENTIAL BREEDING SITES ABOVE 0 
#get sum of each types of breeding sites over 0
effectifs<-apply(data_subset[,variables_reponse],2,function(x){sum(x>0)})
effectifs

# create a matrix corresponding to 100 seeds and 24 types of breeding sites 
matrice_effectifs<-matrix(rep(effectifs, each=100), 100, 24)

# get 70% of each sum 
matrice_effectifs_70=0.7*matrice_effectifs
matrice_effectifs_70

# selections of seeds where 70% of cells have values above 0 
selection = train_effectifs_par_seed_filtered > matrice_effectifs_70
selection

# create 8 slides of selection 
selection_resultats_array_2D=c()
for (i in (1:8)){selection_resultats_array_2D<-rbind(selection_resultats_array_2D,t(selection))}

# create a cube to match with the dimension of the results dataframe
selection_resultats_array_3D=array(dim=dim(resultats_array))
for (j in (1:9)){selection_resultats_array_3D[,j,]<-selection_resultats_array_2D}
selection_resultats_array_3D[,,1]

# remplace all FALSE by NA  
selection_resultats_array_3D[!selection_resultats_array_3D]<-NA
selection_resultats_array_3D

# filtered the results array with the selection 
resultats_array_filtered=resultats_array*selection_resultats_array_3D
resultats_array_filtered # results with only seed where we are sure that there is no no value of types of breeding sites

## MEAN over results array non filtered ###
resultats_array_mean<-apply(X=resultats_array, MARGIN=c(1,2), FUN=mean, na.rm=T)

svg_resultats_array_mean<-resultats_array_mean

#### MEAN over results array filtered ####
resultats_array_filtered_mean<-apply(X=resultats_array_filtered, MARGIN=c(1,2), FUN=mean, na.rm=T)

#rename variables response 
noms_vars_fig <- c("all potential breeding sites", "barrels", "containers under 200 L",
                   "containers over 200 L","wells","green plants","other plants","large volumes",
                   "tires", "small waste","big waste",
                   "watercraft", "buildings materials", "manholes",
                   "protection","troughs", "pit", "canals", "gutters",  "others",
                   "Containers", "Plants", "Waste",
                   "Water management \n infrastructure")

# add variables names and results array
resultats_array_filtered_mean_df=data.frame(variable_reponse = rep(noms_vars_fig, length(variables_explicative)),
                                   as.data.frame(resultats_array_filtered_mean),
                                   variable_explicative = rep(variables_explicative_nom,each=length(variables_reponse)))

# rename colunms 
names(resultats_array_filtered_mean_df)<-c("variable_reponse","r2", "r", "rmse", "rmse_normalized_maxmin",
                                  "rmse_normalized_sd", "rmse_normalized_quantile", "effectif_entrainement",
                                  "effectif_validation", "range", "names")


#### MED over results array filtered ####
resultats_array_filtered_med<-apply(X=resultats_array_filtered, MARGIN=c(1,2), FUN=median, na.rm=T)

resultats_array_filtered_med_df=data.frame(variable_reponse = rep(noms_vars_fig, length(variables_explicative)),
                                   as.data.frame(resultats_array_filtered_med),
                                   variable_explicative = rep(variables_explicative_nom,each=length(variables_reponse)))

names(resultats_array_filtered_med_df)<-c("variable_reponse","r2", "r", "rmse", "rmse_normalized_maxmin",
                                  "rmse_normalized_sd", "rmse_normalized_quantile", "effectif_entrainement",
                                  "effectif_validation", "range", "names")

#### MAX over results array filtered ####
resultats_array_filtered_max<-apply(X=resultats_array_filtered, MARGIN=c(1,2), FUN=max, na.rm=T)

resultats_array_filtered_max_df=data.frame(variable_reponse = rep(noms_vars_fig, length(variables_explicative)),
                                  as.data.frame(resultats_array_filtered_max),
                                  variable_explicative = rep(variables_explicative_nom,each=length(variables_reponse)))

names(resultats_array_filtered_max_df)<-c("variable_reponse","r2", "r", "rmse", "rmse_normalized_maxmin",
                                 "rmse_normalized_sd", "rmse_normalized_quantile", "effectif_entrainement",
                                 "effectif_validation", "range", "names")

#### SD over results array filtered ####
resultats_array_filtered_sd<-apply(X=resultats_array_filtered, MARGIN=c(1,2), FUN=sd, na.rm=T)

resultats_array_filtered_sd_df=data.frame(variable_reponse = rep(noms_vars_fig, length(variables_explicative)),
                                  as.data.frame(resultats_array_filtered_sd),
                                  variable_explicative = rep(variables_explicative_nom,each=length(variables_reponse)))

names(resultats_array_filtered_sd_df)<-c("variable_reponse","r2", "r", "rmse", "rmse_normalized_maxmin",
                                 "rmse_normalized_sd", "rmse_normalized_quantile", "effectif_entrainement",
                                 "effectif_validation", "range", "names")



### Function ####


figures=function(resultats=resultats)
{

#reoder with R²
resultats$variable_reponse <- reorder(resultats$variable_reponse, resultats$r2)

### plot par categories
resultats_cat <- resultats %>%
  filter(variable_reponse %in% c("Containers",
                                 "Plants",
                                 "Waste",
                                 "Water management \n infrastructure"))

# Create ggplot with the filtered data
plot_rf_cat <- ggplot(resultats_cat, aes(x=r2, y=variable_reponse, color = names, size=rmse_normalized_maxmin))+
  geom_point()+
  theme(axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 16),  
        legend.title = element_text(size = 14),
        title = element_text(size = 14))+ 
  scale_size_continuous(name = "NRMSE", labels = scales::comma) + 
  guides(color = guide_legend(title = "Categories of explanatory variables", label.theme = element_text(size = 12)))+
  labs(title = "a", x = "R²", y = "Potential Breeding Sites variables")+
  labs(color= "Types of explanatory variables", size=14)

# plot par types
resultats_typ <- resultats %>%
  filter(variable_reponse %in% c("all potential breeding sites", 
                                 "barrels",
                                 "containers under 200 L",
                                 "containers over 200 L",
                                 "wells",
                                 "green plants",
                                 "other plants",
                                 "large volumes",
                                 "tires",
                                 "small waste",
                                 "big waste",
                                 "watercraft",
                                 "buildings materials",
                                 "manholes",
                                 "protection",
                                 "troughs",
                                 "pit",
                                 "canals",
                                 "gutters",
                                 "others"))

# Create ggplot with the filtered data
plot_RF_typ = ggplot(resultats_typ, aes(x=r2, y=variable_reponse, color = names, size=rmse_normalized_maxmin))+
  geom_point()+
  theme(axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 16),  
        legend.title = element_text(size = 14),
        title = element_text(size = 18))+ 
  scale_size_continuous(name = "NRMSE", labels = scales::comma) + 
  guides(color = guide_legend(title = "Categories of explanatory variables", label.theme = element_text(size = 12)))+
  labs(title= "b", x = "R²", y = "Potential breeding sites variables")+
  labs(color= "Types of explanatory variables", size=14)



grid.arrange(plot_RF_typ, plot_rf_cat, ncol = 1)

}

# use the function
figures(resultats=resultats_array_filtered_sd_df)

##histogram analyses
x11();hist(resultats_array[152,1,])


#### PREDICTIONS ####

# import grid with variables already calculated over all study area
# grid_predict <- read_sf("G:/0_PHD/RECHERCHE/AXE_2/TRAITEMENT_nov_2023/grid_to_predict_surf_bati_vars_nb_buildings_lm_veget_buildings_english.gpkg")

grid_predict <- read_sf("G:/0_PHD/RECHERCHE/AXE_2/Soumission_Article/test/grid_for_prediction_ok.gpkg")

grid_predict_with_obs <- grid_predict[complete.cases(grid_predict$nb_potential_bs), ]

# launch the model with all the dataset (no separation train/test)
set.seed(123) # fix for reproductibility

# use variables of the MFA to integer remote sensing variables
model_train <- randomForest(nb_potential_bs ~ lm_buildings_ca + number_buildings + lm_vegetation_lpi + ndvi_P_mean + ndwi_P_mean + C1B201_mean + h_vegetation_mean, data=grid_predict_with_obs, importance = TRUE, na.action=na.roughfix,ntree=500)

predictions_full_model <- predict(model_train, newdata = grid_predict) # predict over the new grid

grid_predict$nb_potential_bs_predicted <- predictions_full_model # adding prediction value to the origin grid

# select only cells with observations
prediction_validation <- grid_predict[complete.cases(grid_predict$nb_potential_bs), ]

# R and R² for observed and predicted values
r <- cor.test(prediction_validation$nb_potential_bs, prediction_validation$nb_potential_bs_predicted)
r2 <- r$estimate^2

## Plot of observed and predicted values over all grid cells

full_predictions <- ggplot(prediction_validation, aes(x = nb_potential_bs, y = nb_potential_bs_predicted)) +
  geom_point() +
  geom_smooth(aes(x = nb_potential_bs, y = nb_potential_bs_predicted), method = "lm", se = TRUE, color = "red") +
  labs(title = "b", x = "Observed potential breeding sites", y = "Predicted potential breeding sites") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16), 
    title = element_text(size = 18) 
  ) +   annotate("text", x = 50, y = 250, label = paste("R² =", round(r2, 2)), hjust = 1, vjust = 1, size = 6, color = "red")

full_predictions 

#save grid predict

write_sf(grid_predict,"G:/0_PHD/RECHERCHE/AXE_2/Soumission_Article/test/grid_results_prediction.gpkg")


# to save all variables 
save(list = ls(all.names = TRUE), file = "svg_100seeds_v6_filtered_effectifs_abs_26_01.Rdata", envir = .GlobalEnv)

