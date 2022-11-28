## Bird Data Cleaning Script

# project file structure 
# raw_data is saved in a directory "raw_data"
# cleaned_data is saved in a directory "clean_data"

# Data Input
# original file, seabirds.xls contains 4 tables
# two of the tables contain data and two contain data codes

# Data Verification Checks 
# could use assertr() for data verification
#  - where there is a mismatch in abundance and the abundance columns?

# Data Cleaning
# where abundance is zero but bird data is available - set abundance to 1
# this is the minimum possible value but ignoring these data would lose more 
# information

# Notation issues
# there is inconsistent notation in species names 
# this has been tidied up to help data analysis
# all instances of sp have a full stop : sp.
# all instances of _sensu lato_ have been coded as (unidentified)
# (Unidentified has been coded as (unidentified)
# *Note that other inconsistencies remain but are not important for analysis

#load libraries
library(tidyverse)
library(readxl)
library(here)
library(skimr)
library(janitor)

# there was an error reading sex column in bird_data as logical so avoid this by setting all columns to text
ship_data  <- read_excel(here::here("raw_data/seabirds.xls"), sheet = "Ship data by record ID")
bird_data  <- read_excel(here::here("raw_data/seabirds.xls"), 
                         sheet = "Bird data by record ID",
                         col_type = c("text"))

# tidy up variable names
bird_clean <- clean_names(bird_data) 
ship_clean <- clean_names(ship_data) 

# set numeric variable types back as needed
bird_clean <- bird_clean %>% 
  mutate(record_id = as.numeric(record_id)) %>% 
  mutate(record = as.numeric(record)) %>% 
  mutate(count = as.numeric(count)) %>% 
  
# rename 'species' columns in bird_clean to make them easier to work with
  rename(species_common_name = 
         species_common_name_taxon_age_sex_plumage_phase) %>% 
  rename(species_science_name =
           species_scientific_name_taxon_age_sex_plumage_phase) %>% 
  # do this because count gets confusing when we count birds later
  rename(abundance = count)


# Section 2 Dealing with missing values ----------------------------------------

bird_clean <- bird_clean %>%
  
  # recode [NO BIRDS RECORDED] as NA 
  mutate(species_common_name = na_if(species_common_name,"[NO BIRDS RECORDED]")) %>% 
  #drop_na(species_common_name) %>% 
  
  # where bird details are available but abundance is NA - set to 1
  mutate(abundance = coalesce(abundance, 1)) 


# Section 3 remove unnecessary data codes --------------------------------------

## create a function to remove multiple strings from a columns
strip_strings <- function(old_column, strings){
  cleaned <- old_column
  
  # order strings longest first so "string within string" is not a problem
  order <- sort(nchar(strings), decreasing=TRUE, index.return=TRUE)
  strings <- strings[order$ix]
  
  for (badstring in strings){
    cleaned <- str_remove(cleaned,str_c(" ",badstring)) 
    # adding this doesn't allow for a string to be 'embedded'
    # in this case it fixes problem of accidentally removing 
    # 'F' and 'M' at the start of species name
    cleaned <- str_trim(cleaned)
  }
  return(cleaned)  
  }

  
## extract extra codes from species_abbreviation columnn
extra_codes <- bird_clean %>%
  select(species_abbreviation) %>% 
  separate(species_abbreviation, 
             c("spec_abbrev", "extra1","extra2")," ")
  # union finds unique values 
combined_codes <- as_tibble (union_all(extra_codes$extra1,extra_codes$extra2)) %>% 
  drop_na() %>% 
  distinct() %>% 
  pull()
  
#now use codes to clean species_common_name
bird_clean <- bird_clean %>% 
  mutate(species_common_clean = strip_strings(species_common_name, combined_codes)) %>% 
  mutate(species_science_clean = strip_strings(species_science_name, combined_codes)) %>% 
  mutate(species_abbrev_clean = strip_strings(species_abbreviation, combined_codes)) 

# Section 4 Tidy for common notation --------------------------------------
bird_clean <- bird_clean %>% 
  mutate(species_science_clean = 
           str_replace(species_science_clean,"sp.","sp")) %>% 
  mutate(species_science_clean = 
           str_replace(species_science_clean,"sp","sp.")) %>%   
  mutate(species_common_clean = 
           str_replace(species_common_clean,"sensu lato","(unidentified)")) %>% 
  mutate(species_common_clean = 
           str_replace(species_common_clean,"Unidentified","unidentified"))  
      
  
# Section 5 - save cleaned bird  data -------------------------------------------

# tidy workspace
rm(bird_data, ship_data, extra_codes, combined_codes) 
rm(strip_strings)  
    
# first save original files so they can be used at a later date if needed  
write_csv(bird_clean, here(
  "clean_data", "clean_bird_data.csv"))  
write_csv(ship_clean, here(
  "clean_data", "clean_ship_data.csv"))
    
# now join data ready for final analysis  
joined_bird_data <- left_join(bird_clean, ship_clean, 'record_id') %>% 
  select(species_common_clean, species_science_clean, species_abbrev_clean,
         abundance, lat) %>% 
  #drop na values here as they wont be needed in final analysis  
  drop_na(species_common_clean)  

write_csv(joined_bird_data, here("clean_data", "joined_bird_data.csv"))
 
rm(bird_clean, ship_clean)   

print("clean data script completed")
  
  
    