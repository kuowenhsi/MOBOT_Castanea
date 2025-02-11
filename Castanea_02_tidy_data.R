library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Castanea")

Castanea_data <- read_csv("./data/Castanea_merged_data_20241015.csv")%>%
  mutate_at(vars(starts_with("Notes")), .funs = "as.character")

Castanea_data
str(Castanea_data)

# date_info <- Castanea_data %>%
#   select(str_c("Date.", 1:8))%>%
#   .[1,]%>%
#   pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
#   mutate(Date_index = str_remove(Date_index, "Date."))%>%
#   mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

# check the dataset

Castanea_data %>% select(starts_with("stNum"))%>%
  str()
Castanea_data%>%select(starts_with("stLeng"))%>%
  str()
Castanea_data%>%select(starts_with("basDia"))%>%
  str()
Castanea_data%>%select(starts_with("Notes"))%>%
  str()
Castanea_data%>%
  str()


colnames(Castanea_data)
colnames(Castanea_data_corrected)

traits <- colnames(Castanea_data)[12:59]
unique_traits <- unique(str_split_i(traits, "_", 1))
unique_traits
num_uniq_traits <- unique_traits[!(unique_traits %in% c("Notes", "Surv", "sEmerge", "lEmerge", "sEmergeDate", "lEmergeDate"))]
num_uniq_traits

chr_uniq_traits <- unique_traits[(unique_traits %in% c("Notes", "Surv", "sEmergeDate", "lEmergeDate"))]
chr_uniq_traits

Castanea_data_1_num <- Castanea_data %>%
  select(1:11, starts_with(num_uniq_traits))%>%
  pivot_longer(cols = starts_with(num_uniq_traits), names_sep = "_", names_to = c("num_traits", "Date"),values_to = "num_values")%>%
  arrange(PosNum, num_traits, Date)%>%
  mutate(Date = as.Date(as.integer(Date)))

Castanea_data_1_chr <- Castanea_data %>%
  select(1:2, matches(paste0(chr_uniq_traits, "_")))%>%
  pivot_longer(cols = starts_with(chr_uniq_traits), names_sep = "_", names_to = c("chr_traits", "Date"),values_to = "chr_values")%>%
  mutate(Date = as.Date(as.integer(Date)))%>%
  pivot_wider(names_from = chr_traits, values_from = chr_values)

Castanea_data_1_chr_emerge <- Castanea_data_1_chr %>%
  filter(is.na(Date))%>%
  select(-c(3,4,5))%>%
  mutate(sEmergeDate = as.Date(sEmergeDate, "%m/%d/%y"), lEmergeDate = as.Date(lEmergeDate, "%m/%d/%y"))

Castanea_data_1_chr_other <- Castanea_data_1_chr %>%
  filter(!is.na(Date))%>%
  select(1:5)


Castanea_data_1 <- Castanea_data_1_num %>%
  left_join(Castanea_data_1_chr_other, by = c("Accession", "Planting", "Date"))%>%
  left_join(Castanea_data_1_chr_emerge, by = c("Accession", "Planting"))


# check the result
unique(Castanea_data_1$label)
unique(Castanea_data_1$Date)

write_csv(Castanea_data_1, "./data/Castanea_merged_data_tidy_20241015.csv")
