
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


#  Section for notes ------------------------------------------------------

# in 2015 and 2016 there were rating for two types of sweet
  # anonymous_brown_globs_that_come_in_black_and_orange_wrappers	
  # mary_janes	
# in 2017 these two types were merged to one category  
# ?anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes

# country notes
# cascadia? probably us but left out

# age notes
# so much text in age script! had a first go at cleaning but its not right.

# Function for identifying candy columns ----------------------------------

get_candy_cols <- function(candy_data_frame) {
  # first use summarise to count how many values 
  # are either JOY, DESPAIR or MEH
  count_candy <- candy_data_frame %>% 
    summarise(across(everything(),   ~{sum(.x %in% c("JOY","DESPAIR","MEH"))}  ))
  # then filter out low values - which are just random instances of the words
  candy_cols <- which(count_candy>0.25*nrow(candy_data_frame)) 
}

# Load and clean 2015 candy data ------------------------------------------
#candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") 
candy_2015 <- read_excel(here("raw_data", "boing-boing-candy-2015.xlsx"))
candy_2015 <- clean_names(candy_2015)

# identify and rename standard demographic questions
candy_2015 <- candy_2015 %>% 
  rename(age_demog_q = how_old_are_you) %>% 
  rename(tot_demog_q = are_you_going_actually_going_trick_or_treating_yourself)

candy_cols <- get_candy_cols(candy_2015)
clean_candy_2015 <- candy_2015 %>% 
  pivot_longer(candy_cols, names_to ="candy_type", values_to ="rating") %>% 
  select(age_demog_q,tot_demog_q,candy_type,rating) %>% 
  mutate(year = 2015) # add a year column
rm(candy_2015)

# Load and clean 2016 candy data ------------------------------------------
candy_2016 <- read_excel(here("raw_data", "boing-boing-candy-2016.xlsx"))
candy_2016 <- clean_names(candy_2016)

# identify and rename standard demographic questions
candy_2016 <- candy_2016 %>% 
  rename(age_demog_q = how_old_are_you) %>% 
  rename(tot_demog_q = are_you_going_actually_going_trick_or_treating_yourself) %>% 
  rename(country_demog_q = which_country_do_you_live_in) %>% 
  rename(gender_demog_q = your_gender)

candy_cols <- get_candy_cols(candy_2016)
clean_candy_2016 <- candy_2016 %>% 
  pivot_longer(candy_cols, names_to ="candy_type", values_to ="rating") %>% 
  select(age_demog_q,tot_demog_q,country_demog_q,gender_demog_q,candy_type,rating) %>% 
 mutate(year = 2016) # add a year column
rm(candy_2016)

# Load and clean 2017 candy data ------------------------------------------
candy_2017 <- read_excel(here("raw_data", "boing-boing-candy-2017.xlsx"))
candy_2017 <- clean_names(candy_2017)

#remove 'q_' numbers from columnn names
org_names2017 <-  names(candy_2017)
for (cols in 1:length(candy_2017)) {
  names(candy_2017)[names(candy_2017)==org_names2017[cols]] <- str_remove(org_names2017[cols],"[q][1-9]+[_]")
}

# identify and rename standard demographic questions
candy_2017 <- candy_2017 %>% 
  rename(age_demog_q = age) %>% 
  rename(tot_demog_q = going_out) %>% 
  rename(country_demog_q = country) %>% 
  rename(gender_demog_q = gender) 

candy_cols <- get_candy_cols(candy_2017)
clean_candy_2017 <- candy_2017 %>% 
  pivot_longer(candy_cols, names_to ="candy_type", values_to ="rating") %>% 
  select(age_demog_q,tot_demog_q,country_demog_q,gender_demog_q,candy_type,rating) %>% 
  mutate(year = 2017) # add a year column
rm(candy_2017)


# Joining 2015,2016 and 2017 datasets together ----------------------------

joined_candy <- full_join(clean_candy_2015,clean_candy_2016)
joined_candy <- full_join(joined_candy,clean_candy_2017)

candy_list <- joined_candy  %>% 
  select(candy_type) %>% 
  group_by(candy_type) %>% 
  summarise(count = n()) %>% 
  arrange(candy_type) %>% 
  pull()
rm(clean_candy_2015,clean_candy_2016,clean_candy_2017)

# Cleaning joined candy data ----------------------------------------------

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
  is.na(age_demog_q) ~ NA_real_,
  #first deal with all words - no numbers
  str_detect(age_demog_q,"\\d", negate = TRUE) ~ NA_real_,
  #then deal with all numbers
  str_detect(age_demog_q,"\\D") ~ as.numeric(age_demog_q),
  #then deal with approximate ages
  #str_detect(age_demog_q,"\\d(ish|s|'|approx|>)") ~ as.numeric(str_extract(age_demog_q  ,"\\d"{1,2})) + 5 
  #then extract any numbers from a string
  str_detect(age_demog_q,"\\d") ~ as.numeric(str_extract(age_demog_q,"\\d{1,2}")), 
  TRUE               ~ NA_real_)
  )

# remove age above and below expected limits
# set between 1 and 99 for now
  joined_candy <- joined_candy %>% 
    mutate(age_clean = case_when(
      age_clean >= 99 ~ NA_real_,
      age_clean < 1   ~ NA_real_,
                 TRUE ~ age_clean))
  
# gender and going out? columns seem to be more controlled
  
# Write out cleaned data -------------------------------------------------------
write_csv(joined_candy, here(
    "clean_data", "clean_candy_data.csv"))
  
  print("clean data script completed")
  
  
  