rm(list = ls())

library(dplyr)

path_data <- "./01__Data/"

# Reading in data --------------------------------------------------------------
tracker <- haven::read_sav(file.path(path_data, "01__Raw/Tracker.sav"))
health <- haven::read_sav(file.path(path_data, "01__Raw/Health.sav"))
mod_v <- haven::read_sav(file.path(path_data, "01__Raw/Mod_v.sav"))

# Data manipulation ------------------------------------------------------------
# Filtering to procrastination participants and getting their scores
mod_v_filtered  <- mod_v %>%
  filter(!is.na(RV155)) %>%
  select(HHID, PN, RV156:RV167) %>%
  rename(
    P_1 = "RV156",
    P_2 = "RV157",
    P_3 = "RV158",
    P_4 = "RV159",
    P_5 = "RV160",
    P_6 = "RV161",
    P_7 = "RV162",
    P_8 = "RV163",
    P_9 = "RV164",
    P_10 = "RV165",
    P_11 = "RV166",
    P_12 = "RV167"
  )

# Getting smoking and alcohol data for procrastination participants
health_filtered <- health %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, RC116:RC138) %>%
  rename(
    # Smoking variables
    Smoke_ever = "RC116",
    Smoke_current = "RC117",
    Num_cig = "RC118",
    Age_smoke_start = "RC120",
    Year_smoke_start = "RC121",
    Year_smoke_start_ago = "RC122",
    Num_cig_most = "RC123",
    Year_smoke_stop_ago = "RC125",
    Year_smoke_stop = "RC126",
    Age_smoke_stop = "RC127",
    # Drinking variables
    Drink_ever = "RC128",
    Days_drink = "RC129",
    Amount_drink_day = "RC130",
    Binge_drink = "RC131",
    Drink_12 = "RC134",
    Cut_down_drink = "RC135",
    Criticize_drink = "RC136",
    Guilt_drink = "RC137",
    Morning_drink = "RC138"
  )

# Getting demographic data
tracker_filtered <- tracker %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, GENDER, BIRTHYR, RAGE) %>%
  rename(
    Gender = "GENDER",
    Birth_year = "BIRTHYR",
    Age = "RAGE"
  )

# Joining data frames
health_data <- list(tracker_filtered, health_filtered, mod_v_filtered) %>%
  purrr::reduce(full_join, by = c("HHID", "PN")) %>%
  mutate(ID = seq(1:nrow(mod_v_filtered)), .before = HHID) %>%
  select(!c(HHID, PN))

# Removing redundant datasets
rm(health, health_filtered, 
   tracker, tracker_filtered, 
   mod_v, mod_v_filtered)

# Handling missing values ------------------------------------------------------
health_data <- health_data %>%
  mutate(
    # Smoking Variables
    Smoke_ever = ifelse(Num_cig_most > 0, 1, 0), # If someone has a value for number of max cigarettes smoken then they have smoken before
    Num_cig = ifelse(Smoke_current == 5, 0, Num_cig), # If someone doesn't currently smoke then they should have a 0 value for number of cigarettes currently smoken   
    Year_smoke_start = Birth_year + Age_smoke_start, # If we know the age someone started smoking at we can figure out the year they started
    Year_smoke_start_ago = 2020 - Year_smoke_start, # Hence, if we know the year they started we can figure out how long ago that was
    Year_smoke_stop = 2020 - Year_smoke_stop_ago, # If we know how many years ago someone stopped smoking we can figure out the exact year
    Age_smoke_stop = Age - Year_smoke_stop_ago, # If we know the year someone stopped smoking we can figure out the age they stopped at
    
    # Drinking Variables
    Drink_ever = ifelse(Drink_12 %in% c(1, 2), 1, 0) # Fixing "Every drank alcohol" variable
  ) %>%
  mutate(Days_no_drink = 7 - Days_drink, .after = Days_drink)

# Re-coding and scoring variables ----------------------------------------------
health_data <- health_data %>%
  mutate(
    Gender = recode(Gender, '1' = 0, '2' = 1),
    Smoke_current = recode(Smoke_current, '5' = 0),
    Num_cig_most = ifelse(Num_cig_most %in% c(-8, 998), NA, Num_cig_most),
    # Fixing major outliers in year stopped smoking
    Year_smoke_stop = ifelse(Year_smoke_stop <= 1924, NA, Year_smoke_stop),
    Year_smoke_stop_ago = ifelse(Year_smoke_stop_ago %in% c(96, 98), NA, Year_smoke_stop_ago),
    Age_smoke_stop = ifelse(Age_smoke_stop < 0, NA, Age_smoke_stop),
    Days_drink = ifelse(Days_drink %in% c(-8, 8), NA, Days_drink),
    Days_no_drink = ifelse(Days_no_drink %in% c(-1, 15), NA, Days_no_drink),
    Amount_drink_day = ifelse(Amount_drink_day == 98, NA, Amount_drink_day),
    Binge_drink = ifelse(Binge_drink > 60, NA, Binge_drink),
    Drink_12 = ifelse(Drink_12 %in% c(5, 6), 0, 1),
    Cut_down_drink = recode(Cut_down_drink, '5' = 0),
    Criticize_drink = recode(Criticize_drink, '5' = 0),
    Guilt_drink = recode(Guilt_drink, '5' = 0),
    Morning_drink = recode(Morning_drink, '5' = 0),
    
    # Procrastination
    across(starts_with("P_"), ~ ifelse(. %in% c(-8, 8, 9), NA, .))
  ) %>%
  mutate(
    Total_procrastination = rowSums(select(., starts_with("P_")), na.rm = TRUE),
    across("Total_procrastination", ~ ifelse(. %in% 0, NA, .))
    )

# Exporting --------------------------------------------------------------------
writexl::write_xlsx(health_data, path = file.path(path_data, "02__Processed/Health_data.xlsx"))
