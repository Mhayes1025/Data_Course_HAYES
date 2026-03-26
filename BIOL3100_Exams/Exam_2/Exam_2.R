#Load dataset
data <- read.csv("unicef-u5mr.csv")

#Load tidyverse
library(tidyverse)

#Tidy the data
data_long <- data %>%
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(gsub("U5MR.", "", Year))
  )

#Mean U5MR per continent per year
#Create summary data
library(ggplot2)

#Plot
ggplot(data_long, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~Continent) +
  theme_minimal()

#Save the plot
ggsave("Hayes_Plot_1.png")

#Mean U5MR per continenet per year
mean_data <- data_long %>%
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

#Generate a plot
ggplot(mean_data, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line() +
  theme_minimal()

#Save the plot
ggsave("Hayes_Plot_2.png")

#Clean up the data for the models
data_clean <- data_long %>%
  drop_na(U5MR)

#Build models
mod1 <- lm(U5MR ~ Year, data = data_clean)
mod2 <- lm(U5MR ~ Year + Continent, data = data_clean)
mod3 <- lm(U5MR ~ Year * Continent, data = data_clean)

#Compare models
AIC(mod1, mod2, mod3)

# mod3 is best because it has the lowest AIC

#Plot predictions
data_clean$pred1 <- predict(mod1)
data_clean$pred2 <- predict(mod2)
data_clean$pred3 <- predict(mod3)

#Generate a plot for the 3 models
pred_plot_data <- data_clean %>%
  select(CountryName, Continent, Year, pred1, pred2, pred3) %>%
  pivot_longer(
    cols = starts_with("pred"),
    names_to = "model",
    values_to = "pred"
  )

ggplot(pred_plot_data, aes(x = Year, y = pred, color = Continent)) +
  geom_line() +
  facet_wrap(~model) +
  theme_minimal()

#Ecuador prediction
ecuador_2020 <- data.frame(
  Year = 2020,
  Continent = "Americas"
)

predict(mod3, newdata = ecuador_2020)

#Ecuador calculation
pred <- predict(mod3, newdata = ecuador_2020)

real <- 13

difference <- abs(pred - real)

difference