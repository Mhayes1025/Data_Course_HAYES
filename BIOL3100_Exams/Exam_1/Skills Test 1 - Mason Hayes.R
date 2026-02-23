data <- read.csv("cleaned_covid_data.csv")

library(dplyr)
library(stringr)

A_states <- data %>%
  filter(str_starts(Province_State, "A"))

library(dplyr)
library(ggplot2)
library(lubridate)

A_states <- A_states %>%
  mutate(Last_Update = as.Date(Last_Update))

ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(
    title = "Deaths Over Time (States Beginning with A)",
    x = "Date",
    y = "Deaths"
  ) +
  theme_minimal()

library(dplyr)

state_max_fatality_rate <- data %>%
  group_by(Province_State) %>%
  summarise(
    peak_case_fatality_rate = max(Case_Fatality_Ratio, na.rm = TRUE)
  )

state_max_fatality_rate <- data %>%
  group_by(Province_State) %>%
  summarise(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State, levels = Province_State))

library(ggplot2)

ggplot(state_max_fatality_rate, 
       aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(
    title = "Maximum Case Fatality Ratio by State",
    x = "Province_State",
    y = "Maximum_Fatality_Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

library(dplyr)

us_cumulative_deaths <- data %>%
  group_by(Last_Update) %>%
  summarise(
    total_deaths = sum(Deaths, na.rm = TRUE)
  ) %>%
  arrange(Last_Update)

library(dplyr)
library(ggplot2)

us_cumulative_deaths <- data %>%
  group_by(Last_Update) %>%
  summarise(total_deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(Last_Update)

ggplot(us_cumulative_deaths, aes(x = Last_Update, y = total_deaths)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  labs(
    title = "Cumulative COVID-19 Deaths in the United States Over Time",
    x = "Date",
    y = "Total Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))