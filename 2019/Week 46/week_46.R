
library(tidyverse)

cran_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

cran_dat %>%
  group_by(language) %>%
  summarise(code_lines = sum(code, na.rm = TRUE),
            total = n()) %>%
  ungroup() %>%
  mutate(lines_per = code_lines/total) %>%
  filter(lines_per > 500) %>%
  mutate(is_R = language == "R") %>%
  ggplot(aes(reorder(language, lines_per), lines_per)) +
  geom_col(aes(fill = is_R)) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dark grey", "#75AADB"), guide = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Lines per Script by Language",
       caption = "\nAuthor: Jason Jones\nSource: #TidyTuesday Week 46") +
  theme_minimal()

ggsave(filename = "plot.png", device = "png", path = "2019/Week 46/",
       width = 7, height = 10, dpi = 320)

