

library(readxl)
library(tidyverse)
library(janitor)
library(skimr)

# Notes
# in 2015 and 2016 there were rating for two types of sweet
  # anonymous_brown_globs_that_come_in_black_and_orange_wrappers	
  # mary_janes	
# in 2017 these two types were merged to one category  
# anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes

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
candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") 
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


# Load and clean 2016 candy data ------------------------------------------
candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") 
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

# Load and clean 2017 candy data ------------------------------------------
candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") 
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



joined_candy <- full_join(clean_candy_2015,clean_candy_2016)
joined_candy <- full_join(joined_candy,clean_candy_2017)

candy_list <- joined_candy  %>% 
  select(candy_type) %>% 
  group_by(candy_type) %>% 
  summarise(count = n()) %>% 
  arrange(candy_type) %>% 
  pull()

