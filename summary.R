library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
library(mapproj)
library(plotly)
incarcceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Claim: African Americans are imprisoned/jailed the most over any other race in CA state.

## What is the population of African Americans in prison in CA state
max_black_population_CA <- incarcceration_df %>% 
  select(state, year, black_prison_pop) %>% 
  filter(state == "CA") %>% 
  drop_na() %>% 
  filter(black_prison_pop == max(black_prison_pop)) %>% 
  pull(black_prison_pop)

## What is the population of latinx in prison in CA state
max_latinx_population_CA <- incarcceration_df %>% 
  select(state, year, latinx_prison_pop) %>% 
  filter(state == "CA") %>% 
  drop_na() %>% 
  filter(latinx_prison_pop == max(latinx_prison_pop)) %>% 
  pull(latinx_prison_pop)

## What is the population of asian americans in prison in CA state
max_aapi_population_CA <- distinct(incarcceration_df) %>% 
  select(state, year, aapi_prison_pop) %>% 
  filter(state == "CA") %>% 
  drop_na() %>% 
  filter(aapi_prison_pop == max(aapi_prison_pop)) %>% 
  pull(aapi_prison_pop)

## What is the max population of naive in prison in CA
max_native_population_CA <- incarcceration_df %>% 
  select(state, year, native_prison_pop) %>% 
  filter(state == "CA") %>% 
  drop_na() %>% 
  filter(native_prison_pop == max(native_prison_pop)) %>% 
  pull(native_prison_pop)

## What is the max  population of white in prison in CA
max_white_population_CA <- incarcceration_df %>% 
  select(state, year, white_prison_pop) %>% 
  filter(state == "CA") %>% 
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

# Chart that compares two variables 
# Two variables I chose to compare is the population of black people and year

black_population <- ggplot(data = chart_data) + 
  geom_col(mapping = aes(x = year, y = black_prison_pop))

# map that shows the measure of interest varies geographically
# first find data to show on the map, this would be amount of black people incarcerated
black_in_prison <- incarcceration_df %>% 
  select(state, year, county_name, black_prison_pop, fips) %>% 
  filter(state == "CA") %>% 
  drop_na()

join_counties <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")

join_black_pop <- join_counties %>% 
  left_join(chart_data) %>% 
  filter(state == "CA")
  
# Code to remove x nd y axis from the map
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), #remove axis lines 
    axis.text = element_blank(), # remove axis labels 
    axis.ticks = element_blank(), # remove ticks
    axis.title = element_blank(), #remove titles 
    plot.background = element_blank(), #remove gray background 
    panel.grid.major = element_blank(), #remove major grid lines
    panel.grid.minor = element_blank(), #remove minor grid lines
    panel.border = element_blank() #remove border around plot 
  )

# create the map
cases_map <- ggplot(join_black_pop) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop)
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(join_black_pop$black_jail_pop)), na.value = "white",
                        low = "yellow", high = "red")+
  blank_theme
  












