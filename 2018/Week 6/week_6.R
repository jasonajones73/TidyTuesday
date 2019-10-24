# Getting the data
library(httr)

url = "https://github.com/rfordatascience/tidytuesday/raw/master/data/week6_coffee_chains.xlsx"

GET(url = url, write_disk(tf <- tempfile(fileext = ".xlsx")))


# Reading the data
require(readxl)

starbucks = read_xlsx(tf, sheet = "starbucks")
dunkin = read_xlsx(tf, sheet = "dunkin")
tim_hortons = read_xlsx(tf, sheet = "timhorton")

# Cleaning up some things
unlink(tf)
rm(tf)
rm(url)

# Write for easy retrieval
require(tidyverse)

dir.create(path = "C:/Users/jjones6/Desktop/tidy_tuesday_week_6/data")

write_csv(dunkin, "C:/Users/jjones6/Desktop/tidy_tuesday_week_6/data/dunkin.csv", na = "", append = FALSE)
write_csv(starbucks, "C:/Users/jjones6/Desktop/tidy_tuesday_week_6/data/starbucks.csv", na = "", append = FALSE)
write_csv(tim_hortons, "C:/Users/jjones6/Desktop/tidy_tuesday_week_6/data/tim_hortons.csv", na = "", append = FALSE)

# Clean Dunkin data
dunkin_clean = dunkin %>%
  rename(City = e_city, State = e_state) %>%
  mutate(City = str_to_upper(City)) %>%
  mutate(State = str_to_upper(State)) %>%
  select(City, State) %>%
  mutate(Coffee = "Dunkin") %>%
  mutate(City = str_replace(City, "[:digit:]", "")) %>%
  mutate(City = str_replace(City, "[:punct:]", "")) %>%
  mutate(City = str_trim(City, side = "both"))

# Clean Starbucks
starbucks_clean = starbucks %>%
  filter(Country == "US") %>%
  filter(Brand == "Starbucks") %>%
  rename(State = `State/Province`) %>%
  mutate(City = str_to_upper(City)) %>%
  mutate(State = str_to_upper(State)) %>%
  select(City, State) %>%
  mutate(Coffee = "Starbucks")

# Clean Tim Hortons
tim_hortons_clean = tim_hortons %>%
  filter(country == "us") %>%
  rename(City = city, State = state) %>%
  mutate(City = str_to_upper(City)) %>%
  mutate(State = str_to_upper(State)) %>%
  select(City, State) %>%
  mutate(Coffee = "Tim Hortons")

# Where do I want to move?
coffee_combined = rbind(dunkin_clean, starbucks_clean, tim_hortons_clean) %>%
  mutate(City = str_replace(City, "[:digit:]", "")) %>%
  mutate(City = str_replace(City, "[:punct:]", "")) %>%
  group_by(City, State, Coffee) %>%
  count() %>%
  rename(`Coffee Count` = n) %>%
  ungroup()

require(ggthemes)
require(ggalt)

coffee_combined %>%
  filter(Coffee == "Starbucks") %>%
  filter(State == "NC") %>%
  top_n(10, `Coffee Count`) %>%
  ggplot(aes(reorder(City, `Coffee Count`),`Coffee Count`)) +
  geom_col(fill = "#006341") +
  coord_flip() +
  theme_gdocs() +
  labs(title = "Where do I want to live in NC?", subtitle = "Starbucks count by city", x = "City", y = "Number of Locations", caption = "#TidyTuesday Week 6")

coffee_combined %>%
  filter(Coffee == "Starbucks") %>%
  filter(State == "NC") %>%
  top_n(10, `Coffee Count`) %>%
  ggplot(aes(reorder(City, `Coffee Count`),`Coffee Count`)) +
  geom_lollipop(aes(color = "#006341")) +
  coord_flip() +
  theme_gdocs() +
  labs(title = "Where do I want to live in NC?", subtitle = "Starbucks count by city", x = "City", y = "Number of Locations", caption = "#TidyTuesday Week 6")

coffee_combined %>%
  filter(Coffee == "Dunkin") %>%
  filter(State == "NC") %>%
  top_n(10, `Coffee Count`) %>%
  ggplot(aes(reorder(City, `Coffee Count`),`Coffee Count`)) +
  geom_col(fill = "#ee4699") +
  coord_flip() +
  theme_gdocs() +
  labs(title = "Where do I want to live in NC?", subtitle = "Dunkin Donuts count by city", x = "City", y = "Number of Locations", caption = "#TidyTuesday Week 6")

coffee_combined %>%
  filter(Coffee == "Tim Hortons") %>%
  top_n(20, `Coffee Count`) %>%
  ggplot(aes(reorder(City, `Coffee Count`),`Coffee Count`)) +
  geom_col(fill = "#a10000") +
  coord_flip() +
  theme_gdocs()

