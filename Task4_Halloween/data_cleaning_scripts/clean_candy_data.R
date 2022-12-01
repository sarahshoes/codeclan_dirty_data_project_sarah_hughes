
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

# the country column was a free text field in the survey and as a
# result county data i not well controlled. many entries were not real 
# countries
# for this analysis the focus was identifying countries of USA, Canada   # and UK, All other countries, including 'unknown' or 'non-countries' 
# have been assigned  as 'other'
#
# the following have been determined as valid aliases of US
# USA_aliases=c("amerca","ahem....amerca","murica","murrika","alaska"
# ,"california","new york","north carolina","new jersey",
# "the yoo ess of aaayyyyyy","trumpistan","pittsburgh",
# "fear and loathing","cascadia")

# the following have been determined as valid aliases of UK
# UK_aliases=c("scotland","wales","england","uk")

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
  
  # create a unique identifier for rater and a column for year
  idlength <- nchar(as.character(nrow(candy_year)))
  sformat <- paste0("%0",idlength,"d")
  candy_year <- candy_year %>% 
    mutate(year= iyear) %>% 
    mutate(rater_id = paste0(as.character(iyear-2000),".",sprintf(sformat,row_number())))  
  
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
          are_you_going_actually_going_trick_or_treating_yourself) %>%  
      mutate(country_demog_q = NA) %>% 
      mutate(gender_demog_q = NA) 
    }
  
  if (iyear==2016){
    candy_year <- candy_year %>% 
      rename(age_demog_q = how_old_are_you) %>% 
      rename(tot_demog_q = 
          are_you_going_actually_going_trick_or_treating_yourself) %>% 
      rename(country_demog_q = which_country_do_you_live_in) %>% 
      rename(gender_demog_q = your_gender)
    }
  
  if (iyear==2017){
    candy_year <- candy_year %>% 
      rename(age_demog_q = age) %>% 
      rename(tot_demog_q = going_out) %>% 
      rename(country_demog_q = country) %>% 
      rename(gender_demog_q = gender)
    } 
  
# function for candy
  candy_cols <- get_candy_cols(candy_year)
  
# remove empty records - where no ratings given  
    candy_year <- candy_year %>% 
    # sums across rows counting number of non NA values   
    mutate(rating_check = rowSums(across(candy_cols, ~!is.na(.x)))) %>%  
    filter(rating_check > 0) 
  
# extract candy data and rating data  
  candy_year_data <- candy_year %>% 
    pivot_longer(candy_cols, 
                 names_to ="candy_type", 
                 values_to ="rating") %>% 
    select(rater_id,year,candy_type,rating) 

  
  rater_year_data <- candy_year %>% 
    select(rater_id,age_demog_q,tot_demog_q,
           country_demog_q,gender_demog_q) 

  
  # save outputs to a named year variable
  eval(str2lang(paste0("candy",iyear," <- candy_year_data")))
  eval(str2lang(paste0("rater",iyear," <- rater_year_data")))
  
  rm(candy_year,candy_year_data,rater_year_data)
  
}

# Joining 2015,2016 and 2017 datasets together ----------------------------

joined_candy <- full_join(candy2015,candy2016)
joined_candy <- full_join(joined_candy,candy2017)

joined_rater <- full_join(rater2015,rater2016)
joined_rater <- full_join(joined_rater,rater2017)

candy_list <- joined_candy  %>% 
  select(candy_type) %>% 
  group_by(candy_type) %>% 
  summarise(count = n()) %>% 
  arrange(candy_type) %>% 
  pull()
rm(candy2015,candy2016,candy2017)
rm(rater2015,rater2016,rater2017)

# Cleaning joined candy data ----------------------------------------------

## recoding for mismatched candy names

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

## cleaning joined rater data

# useful for testing regex
testdata <- joined_rater %>% 
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


joined_rater <- joined_rater %>% 
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


# cleaning age column  - simple version

joined_rater <- joined_rater %>% 
  mutate(age_clean = as.numeric(age_demog_q)) 
 
# limited set of regex for specific situations
# ignore numeric data from other text
#joined_rater <- joined_rater %>%
  #filter(is.na(age_clean)) %>%  #don't mutate if we already have an age detected
  #mutate(age_clean = case_when (
  # an age range (average)
  #str_detect(age_demog_q,"[0-9]{1,3}-[0-9]{1,3}")
  #~ ((as.numeric(str_extract(age_demog_q,"[0-9]{1,3}$"))
  #    + as.numeric(str_extract(age_demog_q,"^[0-9]{1,3}")))/2),
  # then deal with approximate ages (add 5)
  #str_detect(age_demog_q,"over [1-9]") 
  #~ (5 + as.numeric(str_extract(age_demog_q,"[0-9]{1,3}"))),
  #str_detect(age_demog_q,"ish|'s|>|\\+") 
  #~ (5 + as.numeric(str_extract(age_demog_q,"[0-9]{1,3}"))),
  #TRUE ~ age_clean))  

# remove age above and below expected limits
# set between 1 and 120 for expected age of humans
joined_rater <- joined_rater %>% 
    mutate(age_clean = case_when(
          age_clean >= 120 ~ NA_real_,
          age_clean < 1   ~ NA_real_,
                 TRUE ~ age_clean))
  
# gender and going out? columns are all clean
  
#tidy and check data types before writing out
joined_candy<- joined_candy %>% 
     mutate(rater_id = as.double(rater_id))
  
 joined_rater<- joined_rater %>% 
      rename(gender_clean = gender_demog_q, tot_clean = tot_demog_q) %>%  
      select(rater_id,age_clean,country_clean,tot_clean,gender_clean) %>% 
      mutate(rater_id = as.double(rater_id), age_clean = as.numeric(age_clean))
  
# Write out cleaned data -------------------------------------------------------
write_csv(joined_candy, here(
    "clean_data", "clean_candy_data.csv"))
write_csv(joined_rater, here(
    "clean_data", "clean_rater_data.csv")) 

print("clean data script completed")
  
  
  