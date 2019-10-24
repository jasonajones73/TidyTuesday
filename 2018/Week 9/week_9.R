require(fivethirtyeight)
require(tidyverse)
require(ggthemes)

# Grab data
comic_characters = comic_characters

# Filter for undefined and plot eye color by sex
comic_characters %>%
  filter(is.na(eye) == TRUE) %>%
  filter(str_detect(sex, "^Female") == TRUE | str_detect(sex, "^Male")) %>%
  group_by(year, eye, sex) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(aes(year, total, fill = sex)) +
  geom_col() +
  facet_wrap(~sex) +
  scale_fill_manual(guide = "none", values = c("#2b80aa", "#80bf40")) +
  labs(title = "Undefined Eye Color By Sex",
       subtitle = "Exploring undefined/undocumented eye color by sex over time",
       caption = "Source: FiveThirtyEight") +
  theme_fivethirtyeight() +
  theme(panel.background = element_blank(),
        text = element_text(family = "Roboto"))


# Filter for undefined and plot hair color by sex
comic_characters %>%
  filter(is.na(hair) == TRUE) %>%
  filter(str_detect(sex, "^Female") == TRUE | str_detect(sex, "^Male")) %>%
  group_by(year, hair, sex) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(aes(year, total, fill = sex)) +
  geom_col() +
  facet_wrap(~sex) +
  scale_fill_manual(guide = "none", values = c("#2b80aa", "#80bf40")) +
  labs(title = "Undefined Hair Color By Sex",
       subtitle = "Exploring undefined/undocumented hair color by sex over time",
       caption = "Source: FiveThirtyEight") +
  theme_fivethirtyeight() +
  theme(panel.background = element_blank(),
        text = element_text(family = "Roboto"))


