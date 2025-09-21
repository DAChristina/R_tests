# gpkg converter

storage <- "MIDSEA_2025_orange_working_group/aedes_breeding_sites_modelling-main/data/"
file_list <- list.files(storage,
                        pattern = "\\.gpkg$",
                        full.names = T)
for (f in file_list) {
  gpkg <- sf::read_sf(f)
  gpkg_df <- sf::st_drop_geometry(gpkg)
  
  csv_name <- sub("\\.gpkg$", ".csv", basename(f))
  csv_path <- file.path(storage, csv_name)
  
  write.csv(gpkg_df,
            csv_path,
            row.names = FALSE)
  
}
