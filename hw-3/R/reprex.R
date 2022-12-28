library(tidyverse)
library(readr)
library(ggplot2)
library(modelr)
library(knitr)
library(broom)

# Create a dataframe mocking height and income
x <- c(5.4, 5.5, 5.5, 5.7, 5.9, 6.1, 6.1, 6.6)
y <- c(36, 42, 44, 56, 60, 65, 66, 75)
df <- data.frame(x,y)

# Regress income on height, then plot
mod <- lm(y~x, df)
ggplot(df, aes(x,y)) +
  geom_point() +
  geom_line(data = augment(mod), aes(y=.fitted))


# Add a column with height x 3
df$x3 <- df$x * 3

# Regress income on height x 3
mod3 <- lm(y~x3, df)
ggplot(df, aes(x3,y)) +
  geom_point() +
  geom_line(data = augment(mod3), aes(y=.fitted))


# Add a column multiplying income x 3
df$y3 <- df$y * 3
mody3 <- lm(y3~x, df)

ggplot(df, aes(x,y3)) +
  geom_point() +
  geom_line(data = augment(mody3), aes(y=.fitted))

# Regress both x3
mod33 <- lm(y3~x3, df)



