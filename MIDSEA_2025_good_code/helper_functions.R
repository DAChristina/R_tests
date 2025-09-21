convert_abbrev <- function(abbrev_col) {
  mapping <- read.csv("school_mappings.csv")
  full_names <- mapping$full_name[match(abbrev_col, mapping$abbreviation)]
  return(full_names)
}