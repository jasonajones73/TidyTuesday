library(tidyverse)
library(extrafont)
library(ggchicklet)

theme_set(theme_minimal() +
            theme(text = element_text(family = "Segoe UI"),
                  panel.grid.major.x = element_line(linetype = "dotted")))


big_epa_cars <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv",
                         col_types = cols(.default = "c"))


big_epa_cars <- big_epa_cars %>%
  mutate(createdOn = str_remove(createdOn, "EST")) %>%
  mutate(modifiedOn = str_remove(modifiedOn, "EST")) %>%
  mutate(createdOn = as.POSIXct(createdOn,
                                format = "%a %b %d %T %Y", tz = "EST")) %>%
  mutate(modifiedOn = as.POSIXct(modifiedOn,
                                 format = "%a %b %d %T %Y", tz = "EST"))

x <- big_epa_cars %>%
  select(year, make, model) %>%
  group_by(year) %>%
  distinct(model) %>%
  ungroup() %>%
  mutate(model = str_remove_all(model, "[:digit:]")) %>%
  mutate(model = str_remove_all(model, "[:punct:]")) %>%
  mutate(str_len = str_length(model)) %>%
  filter(str_len > 0) %>%
  mutate(letters = str_extract_all(str_to_lower(model), "[:alpha:]")) %>%
  unnest(cols = c(letters))

total_letters <- x %>%
  group_by(year) %>%
  mutate(total_letters = n()) %>%
  ungroup() %>%
  select(year, total_letters) %>%
  distinct(year, .keep_all = TRUE)

for_chart <- x %>%
  group_by(year, letters) %>%
  summarise(letter_count = n()) %>%
  ungroup() %>%
  left_join(total_letters) %>%
  mutate(prop = letter_count / total_letters) %>%
  mutate(letters = str_to_upper(letters))

for_chart %>%
  filter(prop > 0.049) %>%
  ggplot(aes(reorder(year, desc(year)), prop, fill = letters)) +
  geom_chicklet(width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Letters", palette = "Spectral") +
  labs(title = "What's In A Name?",
       subtitle = "Letter usage in vehicle model names over the years as a proportion of total letters. \nPlot displaying only letters accounting for 5% or greater of yearly total.",
       x = NULL,
       y = NULL,
       caption = "Author: Jason Jones\n Twitter: @packpridejones")