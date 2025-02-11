library(tidyverse)
library(writexl)
library(readxl)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Castanea")

list.files()
list.files(path = "./data")

pheno_data <- read_csv("./data/Castanea ozarkensis phenotype data - 2024_0702.csv", na = c("", "NA", "N/A", "DNR"))
  
pheno_data_date <- pheno_data %>%
  select(starts_with("Date."))%>%
  .[1,] %>%
  mutate_all(function(x){str_replace(x, "/24$", "/2024")})%>%
  mutate_all(function(x){as.Date(x, "%m/%d/%Y")}) %>%
  mutate_all(function(x){as.numeric(x)})%>%
  pivot_longer(cols = everything(), names_prefix = "Date.", values_to = "Date_value", names_to = "Date_index")
  
pheno_data_vector <- pheno_data_date$Date_value
names(pheno_data_vector) <- pheno_data_date$Date_index


append_date_to_colname <- function(x, date_vector){
  date_index <- sapply(x, function(x){str_split_i(x, "[.]", 2)})
  date_value <- date_vector[date_index]
  trait_name <- sapply(x, function(x){str_split_i(x, "[.]", 1)})
  return(paste(trait_name, date_value, sep = "_"))
} 

append_date_to_colname(c("Date", "trait.2"), pheno_data_vector)

pheno_data_renamed <- pheno_data %>%
  select(-starts_with(c("Date.", "L.stg."))) %>%
  rename_with(function(x){append_date_to_colname(x, pheno_data_vector)}, .cols = 7:ncol(.))

LCMS_data <- read_csv("./data/Castanea_livingcollections.csv")

merged_data <- pheno_data_renamed %>%
  filter(Surv_19983 == "Y")%>%
  left_join(LCMS_data, by = c("Accession", "Planting"))%>%
  select("Accession", "Planting","Country","State","County", "Google_Latitude", "Google_Longitude", everything())%>%
  arrange(Accession, Planting)


colnames(merged_data)
write_csv(merged_data, "./data/Castanea_merged_data_20241015.csv")
write_xlsx(merged_data, "./data/Castanea_merged_data_20241015.xlsx")
