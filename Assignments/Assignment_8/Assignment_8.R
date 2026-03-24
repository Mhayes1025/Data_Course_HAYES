library(tidyverse)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)

data <- read.csv("mushroom_growth.csv")

ggplot(data, aes(x=Light, y=GrowthRate)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal()

mod1 <- lm(GrowthRate ~ Light, data=data)
mod2 <- lm(GrowthRate ~ Temperature, data=data)
mod3 <- lm(GrowthRate ~ Light + Temperature, data=data)
mod4 <- lm(GrowthRate ~ Light * Temperature, data=data)

mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)


best_mod <- mod3


newdata <- data.frame(
  Light = c(10, 20, 30),
  Temperature = c(15, 20, 25)
)

predictions <- predict(best_mod, newdata=newdata)


data_pred <- data %>%
  add_predictions(best_mod)

ggplot(data_pred, aes(x=Light, y=GrowthRate)) +
  geom_point() +
  geom_point(aes(y=pred), color="blue") +
  theme_minimal()