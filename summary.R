library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
incarcceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Claim: African Americans are imprisoned/jailed the most over any other race in WA state.

## What is the population of African Americans in prison
max_black_population_WA <- incarcceration_df %>% 
  select(state, year, black_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(black_prison_pop == max(black_prison_pop)) %>% 
  pull(black_prison_pop)

## What is the population of latinx in prison
max_latinx_population_WA <- incarcceration_df %>% 
  select(state, year, latinx_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(latinx_prison_pop == max(latinx_prison_pop)) %>% 
  pull(latinx_prison_pop)

## How has age change in prison form 1970 to 2018 in WA
max_aapi_population_WA <- incarcceration_df %>% 
  select(state, year, aapi_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(aapi_prison_pop == max(aapi_prison_pop)) %>% 
  pull(aapi_prison_pop) 

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
    
    
    
    
    
    
