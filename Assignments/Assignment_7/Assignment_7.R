getwd()
list.files()

# Import the Utah religions dataset from the CSV file into R
religion_data <- read.csv("Utah_Religions_by_County.csv")
# Inspect the dataset to confirm it loaded correctly and view the column structure
names(religion_data)

head(religion_data)

# Convert the dataset from wide format to tidy format so each religion becomes a row instead of a column
library(tidyverse)

religion_tidy <- religion_data %>%
  pivot_longer(
    cols = -c(County, Pop_2010, Religious, Non.Religious),
    names_to = "Religion",
    values_to = "Count"
  )

# Calculate the total number of adherents for each religion across all counties
religion_tidy %>%
  group_by(Religion) %>%
  summarize(total = sum(Count, na.rm = TRUE))
religion_tidy %>%
  group_by(Religion) %>%
  summarize(total = sum(Count, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Religion, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Adherents by Religion in Utah",
    x = "Religion",
    y = "Total Adherents"
  )

# Create a bar chart comparing total religious adherents across counties
religion_tidy %>%
  group_by(Religion) %>%
  summarize(total = sum(Count)) %>%
  slice_max(total, n = 10) %>%
  ggplot(aes(x = reorder(Religion, total), y = total)) +
  geom_col() +
  coord_flip()

# Create a stacked bar chart showing the composition of different religions within each county
ggplot(religion_tidy, aes(x = County, y = Count, fill = Religion)) +
  geom_col() +
  labs(title = "Religion Composition by County",
       x = "County",
       y = "Adherents") +
  theme(axis.text.x = element_text(angle = 90))

# Create faceted bar charts to compare the number of adherents for each religion across counties
ggplot(religion_tidy, aes(x = County, y = Count)) +
  geom_col() +
  facet_wrap(~Religion) +
  coord_flip() +
  labs(title = "Adherents by County for Each Religion")

# Identify the 10 counties with the largest total number of religious adherents
top_counties <- religion_tidy %>%
  group_by(County) %>%
  summarize(total = sum(Count)) %>%
  slice_max(total, n = 10)

# Filter the dataset to the top 10 counties and create faceted bar charts showing adherents by religion in those counties
religion_tidy %>%
  filter(County %in% top_counties$County) %>%
  ggplot(aes(x = County, y = Count)) +
  geom_col() +
  facet_wrap(~Religion) +
  coord_flip()

# Identify the counties with the highest total religious populations
top_counties <- religion_tidy %>%
  group_by(County) %>%
  summarize(total = sum(Count, na.rm = TRUE)) %>%
  slice_max(total, n = 10)

# Create plots to explore religion distributions in the largest counties
religion_tidy %>%
  filter(County %in% top_counties$County) %>%
  ggplot(aes(x = County, y = Count)) +
  geom_col() +
  facet_wrap(~Religion) +
  coord_flip()

# Does population of a county correlate with the proportion of any specific religious group in that county?
religion_prop <- religion_data %>%
  select(County, Pop_2010, LDS, Catholic, Evangelical, Non.Religious) %>%
  mutate(
    LDS_prop = LDS / Pop_2010,
    Catholic_prop = Catholic / Pop_2010,
    Evangelical_prop = Evangelical / Pop_2010,
    NonRel_prop = Non.Religious / Pop_2010
  )

# Create a scatterplot to explore whether county population is related to the proportion of LDS adherents
ggplot(religion_prop, aes(x = Pop_2010, y = LDS_prop)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "County Population vs LDS Proportion",
    x = "County Population",
    y = "Proportion LDS"
  )

# Calculate correlation coefficients to examine whether county population is related to the proportion of different religious groups
cor(religion_prop$Pop_2010, religion_prop$LDS_prop)
cor(religion_prop$Pop_2010, religion_prop$Catholic_prop)
cor(religion_prop$Pop_2010, religion_prop$Evangelical_prop)

# Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?
ggplot(religion_prop, aes(x = LDS_prop, y = NonRel_prop)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "LDS Proportion vs Non-Religious Proportion",
    x = "Proportion LDS",
    y = "Proportion Non-Religious"
  )

# Calculate correlation coefficients to examine whether the proportion of certain religions is related to the proportion of non-religious residents
cor(religion_prop$LDS_prop, religion_prop$NonRel_prop)
cor(religion_prop$Catholic_prop, religion_prop$NonRel_prop)
