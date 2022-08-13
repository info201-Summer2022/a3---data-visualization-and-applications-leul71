library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
incarcceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Claim: African Americans are imprisoned/jailed the most over any other race in WA state.

## What is the population of African Americans in prison in WA state
max_black_population_WA <- incarcceration_df %>% 
  select(state, year, black_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(black_prison_pop == max(black_prison_pop)) %>% 
  pull(black_prison_pop)

## What is the population of latinx in prison in WA state
max_latinx_population_WA <- incarcceration_df %>% 
  select(state, year, latinx_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(latinx_prison_pop == max(latinx_prison_pop)) %>% 
  pull(latinx_prison_pop)

## What is the population of asian americans in prison in WA state
max_aapi_population_WA <- distinct(incarcceration_df) %>% 
  select(state, year, aapi_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(aapi_prison_pop == max(aapi_prison_pop)) %>% 
  pull(aapi_prison_pop)

## What is the max population of naive in prison in WA
max_native_population_WA <- incarcceration_df %>% 
  select(state, year, native_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(native_prison_pop == max(native_prison_pop)) %>% 
  pull(native_prison_pop)

## What is the max  population of white in prison in WA
max_white_population_WA <- incarcceration_df %>% 
  select(state, year, white_prison_pop) %>% 
  filter(state == "WA") %>% 
  drop_na() %>% 
  filter(white_prison_pop == max(white_prison_pop)) %>% 
  pull(white_prison_pop)
  
## A chart that shows trends over time for a variable of the above choices
## Time trend
chart_data <- incarcceration_df %>% 
  drop_na()
second_chart_data <- chart_data %>% 
  filter(year >= 1970) %>% 
  group_by(year) %>% 
  summarise(
    black_prison_pop = sum(black_prison_pop),
    latinx_prison_pop = sum(latinx_prison_pop),
    native_prison_pop = sum(native_prison_pop),
    aapi_prison_pop = sum(aapi_prison_pop),
    white_prison_pop = sum(white_prison_pop),
    year = year,
    .groups = "drop"
  )

## gathering data
data_for_chart <- second_chart_data %>% 
  gather(key = race, value = result, -year) %>% 
  group_by(year, race) %>% 
  summarise(
    result = sum(result), 
    .groups = "drop"
  )

## creating the chart 
create_plot <- ggplot( data = data_for_chart) +
    geom_line(mapping = aes(x = year, y = result, color = race))

## Chart that compares two














