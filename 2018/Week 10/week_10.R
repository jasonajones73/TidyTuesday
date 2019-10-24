# Load packages
require(tidyverse)
require(lubridate)

# Create a vector of files to read in
files = list.files(path = "data/", full.names = TRUE)

# Map read function to vector of files and default all column types to character
trip_data = map_df(
  .x = files,
  .f = ~ read_csv(.x, col_types = cols(.default = "c"))
)

# Create start and end time stamps, calculate trip hours, and calculate mph for trip
updated_trip_data = trip_data %>%
  mutate(start_stamp = as.POSIXct(sprintf("%s %s", StartDate, StartTime), format = "%m/%d/%Y %H:%M")) %>%
  mutate(end_stamp = as.POSIXct(sprintf("%s %s", EndDate, EndTime), format = "%m/%d/%Y %H:%M")) %>%
  mutate(hours = as.numeric(((end_stamp - start_stamp)/60)/60)) %>%
  mutate(Distance_Miles = as.numeric(Distance_Miles)) %>%
  mutate(mph = Distance_Miles/hours)
  
# Plot 1
updated_trip_data %>%
  filter(!is.na(mph)) %>%
  filter(mph > 0) %>%
  mutate(start_stamp = floor_date(start_stamp, unit = "1 month")) %>%
  group_by(start_stamp, PaymentPlan) %>%
  summarise(med_mph = median(mph)) %>%
  ungroup() %>%
  ggplot(aes(start_stamp, med_mph)) +
  geom_area(aes(fill = factor(PaymentPlan, levels = c("Subscriber", "Casual"))), alpha = 0.7, position = "identity") +
  scale_fill_manual("Payment Plan", values = c("#80bf40", "#2b80aa")) +
  scale_y_continuous(limits = c(0, 8)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major =  element_line(color = "light gray"),
        legend.position = "bottom",
        legend.key = element_blank()) +
  labs(x = "",
       y = "Miles Per Hour (mph)",
       title = "Subscribers Ride Faster",
       subtitle = "Median trip miles-per-hour by payment plan") 


# Plot 2
updated_trip_data %>%
  filter(!is.na(mph)) %>%
  filter(mph > 0) %>%
  filter(PaymentPlan == "Subscriber") %>%
  group_by(StartHub) %>%
  summarise(med_mph = median(mph)) %>%
  top_n(20, med_mph) %>%
  ungroup() %>%
  ggplot(aes(reorder(StartHub, med_mph), med_mph)) +
  geom_col(fill = "#2b80aa", alpha = 0.8) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major.x = element_line(color = "gray"),
        panel.grid.minor.x = element_line(color = "light gray"),
        axis.text.y = element_text(face = "bold")) +
  labs(x = "",
       y = "Miles Per Hour (mph)",
       title = "Fastest Hubs for Subscribers",
       subtitle = "20 Start Hubs with fastest median mph") +
  coord_flip()


# Plot 3
updated_trip_data %>%
  filter(!is.na(mph)) %>%
  filter(mph > 0) %>%
  filter(PaymentPlan == "Subscriber") %>%
  group_by(StartHub) %>%
  summarise(med_mph = median(mph)) %>%
  top_n(-20, med_mph) %>%
  ungroup() %>%
  ggplot(aes(reorder(StartHub, -med_mph), med_mph)) +
  geom_col(fill = "#80bf40", alpha = 0.8) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major.x = element_line(color = "gray"),
        panel.grid.minor.x = element_line(color = "light gray"),
        axis.text.y = element_text(face = "bold")) +
  labs(x = "",
       y = "Miles Per Hour (mph)",
       title = "Slowest Hubs for Subscribers",
       subtitle = "20 Start Hubs with slowest median mph") +
  coord_flip()
