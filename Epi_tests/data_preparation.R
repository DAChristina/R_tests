# Create hypothetical data coz' genomic and epidemiological data are separated

# Test script using dataset downloaded on:
# https://www.kaggle.com/datasets/ankushpanday2/oral-cancer-prediction-dataset

library(tidyverse)
library(epitools)

dat <- read.csv("Epi_tests/raw_data/oral_cancer_prediction_dataset.csv")
colnames(dat)
view(dat)
summary(dat)

set.seed(123)
dat_gen <- dat %>% 
  dplyr::filter(Oral_Cancer_Diagnosis == "Yes") %>% 
  dplyr::mutate(Sero_rand = sample(c(1:100), n(), replace = T),
                GPSC_rand = sample(c(1:200), n(), replace = T),
                MLST_rand = sample(c(30:1000), n(), replace = T)) %>% 
  dplyr::select(ID, Sero_rand, GPSC_rand, MLST_rand)

write.csv(dat_gen, "Epi_tests/raw_data/generate_genomics.csv", row.names = F)

################################################################################

meta <- dplyr::left_join(
  read.csv("Epi_tests/raw_data/oral_cancer_prediction_dataset.csv"),
  read.csv("Epi_tests/raw_data/generate_genomics.csv")
  ,
  by = "ID"
)


