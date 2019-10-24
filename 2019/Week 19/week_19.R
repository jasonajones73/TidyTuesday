



library(tidyverse)
library(extrafont)
library(gganimate)

student_ratio <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

ani <- student_ratio %>%
  mutate(year = as.character(year)) %>%
  ggplot(aes(indicator, student_ratio)) +
  geom_jitter(color = "#112E51", alpha = 0.7) +
  geom_hline(color = "#FF7043", yintercept = mean(student_ratio$student_ratio, na.rm = TRUE)) +
  coord_flip() +
  labs(title = "Year: {closest_state}", x = NULL, y = "Student Ratio", caption = "Author: Jason Jones") +
  theme(panel.background = element_blank(),
        text = element_text(family = "Roboto")) +
  transition_states(states = year) +
  enter_fade() +
  exit_fade()

anim_save("student_ratio.gif", ani, width = 5, height = 5, units = "in", res = 300)
