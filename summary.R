library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
incarcceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## What 
king_county_data <- incarcceration_df %>% 
  select(county_name, year, total_pop_15to64, female_pop_15to64, male_pop_15to64,
         black_pop_15to64,latinx_pop_15to64, native_pop_15to64, white_pop_15to64, 
         aapi_pop_15to64, other_race_prison_pop, ) %>% 
  filter(county_name == "King County")
king_county_data = na.omit(king_county_data)

## What is the biggest race in prison in WA state
WA_prison_demographics <- incarcceration_df %>% 
  select(state, year, total_pop, white_prison_pop, black_prison_pop,latinx_prison_pop,
          native_prison_pop, aapi_prison_pop, other_race_prison_pop) %>% 
  filter(state == "WA")
WA_prison_demographics = na.omit(WA_prison_demographics)

## How has age change in prison form 1970 to 2018 in WA
WA_age_prison <- incarcceration_df %>% 
  select(state, year, county_name, total_pop_15to64, total_pop) %>% 
  filter(state == "WA")
WA_age_prison = na.omit(WA_age_prison)

## Are there more men in prison than women or the same amount in WA
WA_gender_in_priosn <- incarcceration_df %>% 
  select(state, year, county_name, female_pop_15to64, male_pop_15to64, 
         total_pop, female_prison_pop, male_prison_pop) %>% 
  filter(state == "WA")
WA_gender_in_priosn = na.omit(WA_gender_in_priosn)

## How does jail demographics compare to prison demographics in WA
WA_jail_to_prison <- incarcceration_df %>% 
  select(state, year, county_name, total_jail_pop, female_jail_pop,
         male_jail_pop,black_jail_pop, white_jail_pop, 
         latinx_jail_pop, native_jail_pop, aapi_jail_pop) %>% 
  filter(state == "WA")
WA_jail_to_prison = na.omit(WA_jail_to_prison)
    
    
    
    
    
    
