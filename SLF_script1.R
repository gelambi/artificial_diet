### SLF data ###

# Clear Workspace
rm(list = ls())

# Load library 
library(dplyr)
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(ggeffects)
library(ggplot2)

### if you have questions about packages or functions 
###?dplyr
###?read.csv

# get dataset
vector_dskjfhdjskf <- c(1,2,3,4)

data_survival <- read.csv("data_July16.csv")
glimpse(data_survival) 

# Start with the 'data_survival' data frame and pipe it through a series of transformations
data_survival <- data_survival %>%
  # Convert the 'instar' column to a factor variable (categorical data)
  mutate(instar = factor(instar)) %>%
  # Convert the 'treatment' column to a factor variable
  mutate(treatment = factor(treatment)) %>%
  # Convert the 'insectID' column to a factor variable
  mutate(insectID = factor(insectID))

###

### base R graphs 
plot()
hist(data_survival$diet_mass)
plot(data_survival$day ~ data_survival$difference)

graph_1 <- ggplot(data_survival, aes(x = day, y = difference, color = treatment)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(group = insect_ID), alpha = 0.3) + 
  labs(
    title = "Amount Eaten Over Time by Treatment",
    x = "Day",
    y = "Amount Eaten",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

graph_1


############

### If you want to practice: 
data(iris)  # optional, not strictly necessary
head(iris)  # shows the first 6 rows

plot(iris$Sepal.Length ~ iris$Sepal.Width)



