# Load required packages
require(tidyverse)
require(tigris)
require(sf)
require(ggthemes)

# Read in data, add standardized citizenship variable, and make CensusId character
acs_data = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week5_acs2015_county_data.csv") %>%
  mutate(stand_citiz = round((Citizen/TotalPop)*100, digits = 1)) %>%
  mutate(CensusId = as.character(CensusId))

# Create quantiles for Census defined "Hard to Count" populations
black = quantile(acs_data$Black, probs = 0.75, type = 3, names = FALSE)
hispanic = quantile(acs_data$Hispanic, probs = 0.75, type = 3, names = FALSE)
income = quantile(acs_data$Income, probs = 0.25, type = 3, na.rm = TRUE, names = FALSE)
income_per_cap = quantile(acs_data$IncomePerCap, probs = 0.25, type = 3, na.rm = TRUE, names = FALSE)
poverty = quantile(acs_data$Poverty, probs = 0.75, type = 3, na.rm = TRUE, names = FALSE)
unemployment = quantile(acs_data$Unemployment, probs = 0.75, type = 3, na.rm = TRUE, names = FALSE)
stand_citiz_quant = quantile(acs_data$stand_citiz, probs = 0.25, type = 3, na.rm = TRUE, names = FALSE)

# Create filtered object for each "Hard to Count" measure
acs_black = acs_data %>% filter(Black >= black)
acs_hispanic = acs_data %>%  filter(Hispanic >= hispanic)
acs_income = acs_data %>%  filter(Income <= income)
acs_income_per_cap = acs_data %>%  filter(IncomePerCap <= income_per_cap)
acs_poverty = acs_data %>%  filter(Poverty >= poverty)
acs_unemploy = acs_data %>%  filter(Unemployment >= unemployment)
acs_citiz = acs_data %>%  filter(stand_citiz <= stand_citiz_quant)

# Merge filtered objects and then count occurrences to create HTC Factor by county
merge_acs_county = rbind(acs_black, acs_hispanic, acs_income, acs_income_per_cap, acs_poverty, acs_unemploy, acs_citiz) %>%
  select(CensusId, State, County) %>%
  group_by(CensusId, State, County) %>%
  count() %>%
  ungroup() %>%
  rename(htc_factor = n)

# Create counties simple features object and match name of ID variable
us_counties = counties(year = 2015) %>%
  st_as_sf() %>%
  mutate(CensusId = as.integer(GEOID)) %>%
  mutate(CensusId = as.character(CensusId)) %>%
  select(CensusId)

# Join data to simple features object
final_for_map = left_join(merge_acs_county, us_counties)

# Filter Hawaii, Puerto Rico, and Alaska for viewing and create plots
final_for_map %>%
  filter(State != "Hawaii") %>%
  filter(State != "Puerto Rico") %>%
  filter(State != "Alaska") %>%
  ggplot() +
  geom_sf(aes(fill = htc_factor)) +
  scale_fill_gradientn("HTC Factor", colors = sf.colors()) +
  labs(title = "Crafting A Hard-To-Count Measure",
       subtitle = "Identifying counties that will be hardest to count in the upcoming census") +
  theme_gdocs()
