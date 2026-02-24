library(tidyverse)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

dat <- dat %>%
  pivot_longer(
    cols = matches("^[A-H][0-9]+$"),
    names_to = "well",
    values_to = "od"
  ) %>%
  mutate(
    row = str_extract(well, "^[A-H]"),
    col = as.numeric(str_extract(well, "[0-9]+"))
  )

names(dat)

library(tidyverse)
library(stringr)

biolog_long <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "OD"
  ) %>%
  mutate(
    Time = as.numeric(str_remove(Time, "Hr_"))
  )

biolog_long <- biolog_long %>%
  mutate(
    Source = case_when(
      str_detect(tolower(`Sample ID`), "soil") ~ "Soil",
      str_detect(tolower(`Sample ID`), "water") ~ "Water",
      TRUE ~ NA_character_
    )
  )

biolog_summary <- biolog_long %>%
  filter(Dilution == 0.1, !is.na(Source)) %>%
  group_by(Substrate, Time, Source) %>%
  summarise(Absorbance = mean(OD, na.rm = TRUE), .groups = "drop")

ggplot(biolog_summary, aes(x = Time, y = Absorbance, color = Source)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Substrate, scales = "free_y") +
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type"
  ) +
  theme_minimal()

install.packages("gganimate")
install.packages("gifski")

library(gganimate)

itaconic_summary <- biolog_long %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(`Sample ID`, Dilution, Time) %>%
  summarise(
    Mean_absorbance = mean(OD, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(itaconic_summary,
            aes(x = Time,
                y = Mean_absorbance,
                color = `Sample ID`,
                group = `Sample ID`)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Dilution) +
  labs(
    y = "Mean_absorbance",
    x = "Time"
  ) +
  theme_minimal() +
  transition_reveal(Time)

animate(p, width = 800, height = 600, fps = 5)