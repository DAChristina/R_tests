## Script to do Multiple Factor Analysis over potential breeding sites variables and urban landscapes variables
## Author : Claire Teillet

###############################################################################

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

# import data with response and explained variables
path = "MIDSEA_2025_orange_working_group/aedes_breeding_sites_modelling-main/data/input_grid_variables.gpkg"

data_brut<- read_sf(path)


##### AFM ####
# choose variables to do AFM (absolute number (nb) or normalized number (norm)) and all urban variable
data_mfa <- subset(data_brut[, c(
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
  "nb_WMI_pbs",
  # "norm_positive_bs",
  # "norm_negative_bs",
  # "norm_potential_bs",
  # "norm_barrels_pbs",
  # "norm_containers_under_200_L_pbs",
  # "norm_containers_over_200_L_pbs",
  # "norm_wells_pbs",
  # "norm_green_plants_pbs",
  # "norm_other_plants_pbs",
  # "norm_large_volumes_pbs",
  # "norm_tires_pbs", "norm_small_waste_pbs",
  # "norm_big_waste_pbs",
  # "norm_watercraft_pbs",
  # "norm_buildings_materials_pbs",
  # "norm_manholes_pbs",
  # "norm_protection_pbs",
  # "norm_troughs_pbs",
  # "norm_pit_pbs",
  # "norm_canals_pbs",
  # "norm_gutters_pbs",
  # "norm_others_pbs",
  # "norm_containers_pbs",
  # "norm_plants_pbs",
  # "norm_waste_pbs",
  # "norm_WMI_pbs",
  "C1B201_mean" ,"C2B201_mean","C3B201_mean","h_vegetation_mean", "h_buildings_mean","ndwi_P_mean" , "ndvi_P_mean",
  "lm_vegetation_ca","lm_vegetation_contig_mn" ,
  "lm_vegetation_cpland" , "lm_vegetation_lpi"   ,
  "lm_vegetation_lsi", "lm_vegetation_pd", "lm_vegetation_shape_mn",
  "lm_buildings_ca"  , "lm_buildings_contig_mn" , "lm_buildings_cpland",
  "lm_buildings_lpi","lm_buildings_lsi" , "lm_buildings_pd", "lm_buildings_shape_mn" , 
  "number_buildings" , "mean_size_buildings"
)])


# drop geometry for processing
data_mfa <- st_drop_geometry(data_mfa) 

write.csv(data_mfa, "MIDSEA_2025_orange_working_group/aedes_breeding_sites_modelling-main/data/input_grid_variables.csv")

# change names for plot
names(data_mfa)<-c("all potential breeding sites", 
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
                   "others",
                   "Containers",
                   "Plants",
                   "Waste",
                   "Water management \n infrastructure",
                   "texture PC1" ,
                   "texture PC2",
                   "texture PC3",
                   "vegetation height",
                   "buildings height",
                   "NDWI" ,
                   "NDVI",
                   "CA vegetation","CONTIG vegetation" ,
                   "CPLAND vegetation" , "LPI vegetation"   ,
                   "LSI vegetation", "PD vegetation", "SHAPE vegetation",
                   "CA buildings"  , "CONTIG buildings" , "CPLAND buildings",
                   "LPI buildings","LSI buildings" , "PD buildings", "SHAPE buildings" , 
                   "number of buildings" , "mean size of buildings")

# process MFA
res.mfa <- MFA(data_mfa, group=c(24,23), name.group = c("Breeding Sites","Urban Landscapes"), graph = FALSE)  

quanti.var <- res.mfa$quanti.var
quanti.var 
# 
# # empty list
# num_dimensions <- ncol(res.mfa$quanti.var$coord)
# positives <- list()
# negatives <- list()
# 
# # Boucle à travers chaque dimension
# for (dim in 1:num_dimensions) {
#   # Obtenir les noms des variables positives et négatives
#   positive_vars <- names(res.mfa$quanti.var$coord[res.mfa$quanti.var$coord[, dim] > 0, dim])
#   negative_vars <- names(res.mfa$quanti.var$coord[res.mfa$quanti.var$coord[, dim] < 0, dim])
#   
#   # Stocker les résultats dans la liste
#   positives[[paste("Dimension", dim)]] <- positive_vars
#   negatives[[paste("Dimension", dim)]] <- negative_vars
# }
# print(positives)
# print(negatives)


# for absolute number
#contribution of variables for each axis, define top = number of the variables above mean contribution by axis (dashed line in red)
contrib_axe1 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 19,
                             palette = "jco",  repel.text.size = 10 , orientation = "horizontal", sort.val = "asc") +
  theme(title= element_text(size = 15), 
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13, angle = 0), 
        axis.text.y = element_text(size = 12),
        legend.position = "none") + ggtitle("a - Axis 1") 
contrib_axe1 <- contrib_axe1 + labs(y = "Contribution (%)", x = " ")
contrib_axe1

contrib_axe2 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 18,
                             palette = "jco",  orientation = "horizontal" , sort.val = "asc") + 
  theme(title= element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13, angle = 0), 
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 20),  
        legend.title = element_text(size = 20),
        legend.position = "none")+
  ggtitle("b - Axis 2")
contrib_axe2 <- contrib_axe2 + labs(y = "Contribution (%)", x = " ")
contrib_axe2


contrib_axe3 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 9,
                             palette = "jco" , orientation = "horizontal", sort.val = "asc")  +
  theme(title= element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13, angle = 0), 
        axis.text.y = element_text(size = 12), 
        legend.text = element_text(size = 15),  
        legend.title = element_text(size = 15),
        legend.position = "bottom") + ggtitle("c - Axis 3")  

contrib_axe3 <- contrib_axe3 + labs(y = "Contribution (%)", x = " ")
contrib_axe3 

grid.arrange(contrib_axe1, contrib_axe2, contrib_axe3, ncol = 1)


#for normalized number
#contribution of variables for each axis, define top with the variables above mean contribution by axis (dashed line in red)
# contrib_axe1 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
#                              palette = "jco", group="env", repel.text.size = 10)   + theme(axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
# 
# 
# contrib_axe2 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 18,
#                              palette = "jco", group="env" ) + theme(axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
# 
# contrib_axe3 <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 8,
#                              palette = "#EFC000FF")  + theme(axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
# 
# grid.arrange(contrib_axe1, contrib_axe2, contrib_axe3, ncol = 3)

# correlation circle with cos2
cos2_axis12 <- fviz_mfa_var(res.mfa, "quanti.var", col.var = "cos2", 
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            labelsize=5, 
                            col.var.sup = "violet", repel = TRUE,axe = c(1,2),
                            select.var = list(cos2 = 0.1, contrib = 15)) + ggtitle("a")
cos2_axis23 <- fviz_mfa_var(res.mfa, "quanti.var", col.var = "cos2", 
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            labelsize=5, geom.param = list(box.padding = 5),
                            col.var.sup = "violet", repel = TRUE , axe = c(2,3), 
                            select.var = list(cos2 = 0.1, contrib= 15))  + ggtitle("b")

cos2_axis12  <- cos2_axis12  + theme(axis.text.x = element_text(size = 13),  
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),  
        axis.title.y = element_text(size = 13),legend.title = element_text(size = 13),
        title= element_text(size = 18)) + labs(x = "Axis 1 (20.1%)", y = "Axis 2 (11.4%)" )
cos2_axis23  <- cos2_axis23 + theme(axis.text.x = element_text(size = 13),   
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13), legend.title = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        title= element_text(size = 18))  + labs(x = "Axis 2 (11.4%)", y = "Axis 3 (7.1%)" )



grid.arrange(cos2_axis12,cos2_axis23, ncol = 1)

# selection variables over the AFM to use in Random Forest
# select variable dim 1 cos > 0.1
cos2_dim1 <- res.mfa$quanti.var$cos2[, "Dim.1"]

variables_selectionnees_dim1 <- names(cos2_dim1[cos2_dim1 > 0.1])

print(variables_selectionnees_dim1)

# select variable dim 2 cos > 0.1
cos2_dim2 <- res.mfa$quanti.var$cos2[, "Dim.2"]

variables_selectionnees_dim2 <- names(cos2_dim2[cos2_dim2 > 0.1])

print(variables_selectionnees_dim2)

# select variable dim 3 cos >0.1
cos2_dim3 <- res.mfa$quanti.var$cos2[, "Dim.3"]

variables_selectionnees_dim2 <- names(cos2_dim2[cos2_dim2 > 0.1])

print(variables_selectionnees_dim3)
