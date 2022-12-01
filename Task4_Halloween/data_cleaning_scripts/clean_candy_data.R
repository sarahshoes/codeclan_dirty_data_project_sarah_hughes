
#  Notes and Assumptions ------------------------------------------------------

# candy types
# some candy names changed between years. Where an obvious match was identified
# candy names were adjusted. The following candy_names were assumed to be 
# equivalent
#
# "boxo_raisins" = "box_o_raisins",                 
# "dark_chocolate_hershey" = "hersheys_dark_chocolate",
# "100_grand_bar" = "x100_grand_bar",
# "bonkers" = "bonkers_the_candy",
# "sweetums_a_friend_to_diabetes" = "sweetums",
# "hershey_s_kissables" = "hersheys_kisses",
# "licorice" = "licorice_yes_black"
# 
# in 2015 there were candy_types of licorice and licorice_not_black
# later this was clarified to licorice_yes_black and licorice_not_black
# therefore the candy_type licorice for 2015 was renamed to licorice_yes_black
# 
# in 2015 and 2016 the category of
# "anonymous_brown_globs_that_come_in_black_and_orange_wrappers"
# was merged with the category of "mary_janes" to create a new category 
# "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_aka_mary_janes"
#
# Due to changes in survey design, 20 candy_types only appeared in 1 year and 
# 16 candy_types only appeared in 2 years

# country names
# cascadia? probably us but left out

# age notes
# so much text in age script! had a first go at cleaning but its not right.

# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(janitor)
library(assertr)
library(here)
library(skimr)
library(readxl)

# installed package !!
# devtools::install_github("fsingletonthorn/words_to_numbers")
# library(wordstonumbers)


# Function for identifying candy columns ----------------------------------

get_candy_cols <- function(candy_data_frame) {
  # first use summarise to count how many values 
  # are either JOY, DESPAIR or MEH
  count_candy <- candy_data_frame %>% 
    summarise(across(everything(),   ~{sum(.x %in% c("JOY","DESPAIR","MEH"))}  ))
  # then filter out low values - which are just random instances of the words
  candy_cols <- which(count_candy>0.25*nrow(candy_data_frame)) 
}

# Load and clean candy data for each year ------------------------------------------
for (iyear in c(2015,2016,2017)){
  
  #load and clean the datafile
  candy_year <- read_excel(here("raw_data", paste0("boing-boing-candy-",iyear,".xlsx")))
  candy_year <- clean_names(candy_year)
  
  # create a unique identifier for rater
  idlength <- nchar(as.character(nrow(candy_year)))
  sformat <- paste0("%0",idlength,"d")
  candy_year <- candy_year %>% 
    mutate(id = paste0(as.character(iyear),".",sprintf(sformat,row_number())))  
  
  #remove 'q_' numbers from columnn names - only in 2017
  #should probably be able to do this with dplyr?  
  org_names <-  names(candy_year)
  for (cols in 1:length(candy_year)) {
    names(candy_year)[names(candy_year)==org_names[cols]] <- str_remove(org_names[cols],"[q][1-9]+[_]")
  }  
  
  # identify and rename standard demographic questions = annoyingly these are different for each year
  if (iyear==2015){
    candy_year <- candy_year %>% 
      rename(age_demog_q = how_old_are_you) %>% 
      rename(tot_demog_q = 
               are_you_going_actually_going_trick_or_treating_yourself) }
  
  if (iyear==2016){
    candy_year <- candy_year %>% 
      rename(age_demog_q = how_old_are_you) %>% 
      rename(tot_demog_q = 
               are_you_going_actually_going_trick_or_treating_yourself) %>% 
      rename(country_demog_q = which_country_do_you_live_in) %>% 
      rename(gender_demog_q = your_gender)}
  
  if (iyear==2017){
    candy_year <- candy_year %>% 
      rename(age_demog_q = age) %>% 
      rename(tot_demog_q = going_out) %>% 
      rename(country_demog_q = country) %>% 
      rename(gender_demog_q = gender)} 
  
  candy_cols <- get_candy_cols(candy_year)
  candy_year <- candy_year %>% 
    pivot_longer(candy_cols, 
                 names_to ="candy_type", 
                 values_to ="rating") %>% 
    select(id,age_demog_q,tot_demog_q,candy_type,rating) %>% 
    mutate(year = iyear) # add a year column
  
  eval(str2lang(paste0("candy",iyear," <- candy_year")))
  rm(candy_year)
  
}

# Joining 2015,2016 and 2017 datasets together ----------------------------

joined_candy <- full_join(candy2015,candy2016)
joined_candy <- full_join(joined_candy,candy2017)

candy_list <- joined_candy  %>% 
  select(candy_type) %>% 
  group_by(candy_type) %>% 
  summarise(count = n()) %>% 
  arrange(candy_type) %>% 
  pull()
rm(clean_candy_2015,clean_candy_2016,clean_candy_2017)

# Cleaning joined candy data ----------------------------------------------

## recoding for mismatched candy names

joined_candy %>% 
          select(year,candy_type) %>% 
          group_by(year) %>% 
          distinct(candy_type,year)

# first checking all candy_type that appears in less than 3 years
joined_candy %>% 
  select(year,candy_type) %>% 
  group_by(candy_type) %>% 
  distinct(year, candy_type) %>% 
  arrange(candy_type) %>% 
  summarise(count_year = n()) %>% 
  filter(count_year<3)

#code to investigate potential matches
joined_candy %>% 
  select(candy_type, year) %>% 
  #filter(str_detect(candy_type, "raisins")) %>% 
  #filter(str_detect(candy_type, "hershey")) %>% 
  #filter(str_detect(candy_type, "100_grand")) %>% 
  #filter(str_detect(candy_type, "bonkers")) %>% 
  #filter(str_detect(candy_type, "sweetums")) %>% 
  #filter(str_detect(candy_type, "mary_janes")) %>% 
  #filter(str_detect(candy_type, "m_m")) %>% 
  filter(str_detect(candy_type, "licorice")) %>%   
  distinct(year, candy_type)  

# redcoding candy types where there has been an obvious name change
joined_candy <- joined_candy %>%
  mutate(candy_type = recode(candy_type,
   "boxo_raisins" = "box_o_raisins",                 
   "dark_chocolate_hershey" = "hersheys_dark_chocolate",
   "100_grand_bar" = "x100_grand_bar",
   "bonkers" = "bonkers_the_candy",
   "sweetums_a_friend_to_diabetes" = "sweetums",
   "hershey_s_kissables" = "hersheys_kisses",
   "licorice" = "licorice_yes_black"
  ))

## cleaning country columns

# useful for testing regex
testdata <- joined_candy %>% 
  select(country_demog_q) %>% 
  filter(!(is.na(country_demog_q))) %>% 
  pull()
ind <-  which(str_detect(testdata,"u(?=n)\\w{1,}\\ss(?=t)\\w{1,}"))
unique(testdata[ind])
rm(testdata)

# identify countries as usa, canada and uk
# all other countries (including non-countries) set to other

# the following have been determined as valid aliases of US
USA_aliases=c("amerca","ahem....amerca","murica","murrika","alaska","california","new york","north carolina","new jersey","the yoo ess of aaayyyyyy","trumpistan","pittsburgh","Fear and Loathing","cascadia")

# the following have been determined as valid aliases of UK
UK_aliases=c("scotland","wales","england","uk")


joined_candy <- joined_candy %>% 
  mutate(country_demog_q = str_to_lower(country_demog_q)) %>% 
  mutate(country_clean = case_when(
    is.na(country_demog_q) ~ NA_character_,
    #captures annoying response!
    str_detect(country_demog_q,"not the usa or canada") ~ "usa",
    #captures us and usa excludes australia
    str_detect(country_demog_q,"u(?=s(?!t))") ~ "usa",
    # captures america and merica
    str_detect(country_demog_q,"merica") ~ "usa",
    # captures variations on u.s and u s
    str_detect(country_demog_q,"[u][\\.|\\s][s]") ~ "usa",
    # captures cases where a state was identified - assume all US states
    str_detect(country_demog_q,"[1-9][1-9]") ~ "usa", 
    #captures all versions of un... with space followed by st...
    str_detect(country_demog_q,
               "u(?=n)\\w{1,}\\ss\\w{1,}") ~ "usa",
    #everything else I gave up trying on
    (country_demog_q %in% USA_aliases) ~ "usa",
    #all versions of canada start with can
    str_detect(country_demog_q,"^can") ~ "canada",
    # captures variations on u.k and u k
    str_detect(country_demog_q,"[u][\\.|\\s][k]") ~ "uk",
    #captures all versions of un... with space followed by ki...
    str_detect(country_demog_q,
               "u(?=n)\\w{1,}\\sk(?=i)\\w{1,}") ~ "uk", 
    #identifies nations within UK as UK
    (country_demog_q %in% UK_aliases) ~"uk",
    # allows for misspellings of england
    str_detect(country_demog_q,"^en") ~ "uk", 
    TRUE               ~ "other")
  )

#check matches for a particular county
#joined_candy %>% 
#  select(country_clean,country_demog_q) %>% 
#  filter(country_clean == "usa") %>% 
#  distinct(country_demog_q)

#quick summary to sense check results
#joined_candy %>% 
#  select(country_clean) %>% 
#  group_by(country_clean) %>% 
#  summarise(count = n())

#check 'other' list doesn't actually contain anything from US, UK or Canada
#joined_candy %>% 
#  filter(country_clean == "other") %>% 
#  select(country_demog_q) %>% 
#  group_by(country_demog_q) %>% 
#  summarise(count = n())

# cleaning age column ** needs fixing **

joined_candy <- joined_candy %>% 
  mutate(age_clean = case_when(
  is.na(age_demog_q) 
              ~ NA_real_,
  #first deal with all words - no numbers
  str_detect(age_demog_q,"\\d", negate = TRUE) 
              ~ NA_real_,
  #then deal with all numbers
  str_detect(age_demog_q,"\\D") 
              ~ as.numeric(age_demog_q),
  #then deal with approximate ages - NOT WORKING
  #str_detect(age_demog_q,"\\d(ish|s|'|approx|>)") ~ as.numeric    
  #then extract any numbers from a string
  str_detect(age_demog_q,"\\d") 
              ~ as.numeric(str_extract(age_demog_q,"\\d{1,2}")), 
         TRUE ~ NA_real_)
  )

# remove age above and below expected limits
# set between 1 and 99 for now
  joined_candy <- joined_candy %>% 
    mutate(age_clean = case_when(
      age_clean >= 99 ~ NA_real_,
      age_clean < 1   ~ NA_real_,
                 TRUE ~ age_clean))
  
# gender and going out? columns are all clean
  
# Write out cleaned data -------------------------------------------------------
write_csv(joined_candy, here(
    "clean_data", "clean_candy_data.csv"))
  
  print("clean data script completed")
  
  
  