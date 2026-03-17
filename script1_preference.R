# ======================================================================
# SCRIPT 1 - PREFERENCE
# ======================================================================
# This script analyzes feeding behavior of *Lycorma delicatula* nymphs reared
# on different liquid diets by quantifying average food consumption per day.
# It processes survival and diet data, calculates per-individual intake,
# fits generalized linear models to test treatment effects for third-
# and fourth-instar nymphs, and generates figures
# with estimated marginal means and significance groupings.
# ======================================================================
# Author: Mariana Gelambi (gelambi@vt.edu)
# Last updated: 03-17-2026

# Clear Workspace
rm(list = ls())

# Load libraries
library(dplyr)
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(effects)
library(performance)
library(multcompView)

# --------------------------------------------
# Read data
# --------------------------------------------

data_survival <- read.csv("clean_data_liquitdiet.csv")

# Format key categorical variables
data_survival <- data_survival %>%
  mutate(
    instar    = factor(instar),
    treatment = factor(treatment),
    insectID  = factor(insectID)
  )

# Keep only rows with valid values in `different`
# Criteria:
#   - not missing
#   - greater than or equal to 0
#   - less than or equal to 1000
data_survival_filtered <- data_survival %>%
  filter(!is.na(different), different >= 0, different <= 1000)

# Inspect filtered dataset structure
glimpse(data_survival_filtered)

# Subset living insects and calculate food consumption metrics
data_alive <- data_survival_filtered %>%
  filter(
    status == 0,   # keep only living insects
    instar == 3,   # keep only third instar
    day < 15       # keep observations before day 15
  ) %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  mutate(
    # Daily food consumed = previous diet amount - current diet amount
    food_eaten = lag(diet) - diet,
    
    # Replace missing or negative values with 0
    food_eaten = if_else(is.na(food_eaten) | food_eaten < 0, 0, food_eaten),
    
    # Cumulative food consumption within each insect
    food_eaten_cum = cumsum(food_eaten)
  ) %>%
  ungroup() %>%
  mutate(
    # Small constant added to avoid zeros in Gamma models
    food_eaten_cum_adj = food_eaten_cum + 0.01
  )

# Inspect processed dataset
glimpse(data_alive)

# Note:
# Cumulative consumption may not be ideal for downstream analyses because
# repeated cumulative values are not independent across days. A better
# response variable may be mean food consumed per day or daily food
# consumption analyzed directly.

# --------------------------------------------
# Average eaten, third instar
# --------------------------------------------

# Calculate average food eaten per day for each insect using the last
# recorded cumulative value divided by the number of days lived
avg_food <- data_alive %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  summarise(
    total_food = food_eaten_cum[which.max(day)],  # cumulative food at the last recorded day
    days_lived = max(day),
    treatment  = first(treatment),
    .groups    = "drop"
  ) %>%
  mutate(
    avg_food_per_day       = total_food / days_lived,
    avg_food_per_day_gamma = avg_food_per_day + 0.001  # small constant added for model fitting
  )

# Relabel and reorder treatment levels for plotting and manuscript order
levels(avg_food$treatment) <- c("Diet A", "Diet B + ailanthone", "Diet B")
avg_food$treatment <- factor(
  avg_food$treatment,
  levels = c("Diet A", "Diet B", "Diet B + ailanthone")
)

# Define color palette in the same treatment order
pal <- c(
  "Diet A"              = "#E2736FFF",
  "Diet B"              = "#E9A66BFF",
  "Diet B + ailanthone" = "#089392FF"
)

# Inspect the resulting dataset
glimpse(avg_food)

# Remove extreme average values before fitting the model
avg_food <- avg_food %>%
  filter(avg_food_per_day < 500)

# Fit model using per-insect average food consumption
model1 <- glmmTMB(
  avg_food_per_day_gamma ~ treatment,
  data = avg_food
)

summary(model1)
Anova(model1)

# Check model diagnostics
check_model(model1)

res <- simulateResiduals(model1)
plot(res)

check_residuals(model1)  # Simulated residuals appear uniformly distributed (p = 0.139)

# Calculate estimated marginal means on the response scale
emm_resp <- emmeans(model1 , ~ treatment, type = "response")
emm_df_resp <- as.data.frame(emm_resp)

# Standardize confidence interval column names
if ("upper.CL" %in% names(emm_df_resp)) {
  emm_df_resp <- rename(emm_df_resp, ymin = lower.CL, ymax = upper.CL)
} else {
  emm_df_resp <- rename(emm_df_resp, ymin = asymp.LCL, ymax = asymp.UCL)
}

# Create compact letter display from Tukey-adjusted pairwise comparisons
pairs_df <- as.data.frame(pairs(emm_resp, adjust = "tukey"))
pvals <- pairs_df$p.value

# Clean contrast labels to the format expected by multcompLetters()
clean_contr <- pairs_df$contrast |>
  gsub("[()]", "", x = _) |>
  gsub("\\s*/\\s*", "-", x = _) |>
  gsub("\\s*-\\s*", "-", x = _)

names(pvals) <- clean_contr

letters_map <- multcompView::multcompLetters(pvals)$Letters

# Add significance letters and y positions for plotting
emm_df_resp$treatment <- factor(
  emm_df_resp$treatment,
  levels = levels(avg_food$treatment)
)
emm_df_resp$letters <- letters_map[as.character(emm_df_resp$treatment)]
emm_df_resp$label_y <- emm_df_resp$ymax + 50  # vertical position for significance letters

# Plot average food eaten per day
preference_plot_third <- ggplot(data = avg_food, aes(x = treatment)) +
  geom_point(
    aes(y = avg_food_per_day, color = treatment),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.6,
    size = 1.5
  ) +
  geom_errorbar(
    data = emm_df_resp,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1,
    color = "black",
    linewidth = 0.4
  ) +
  geom_point(
    data = emm_df_resp,
    aes(y = emmean),
    shape = 21,
    fill = "black",
    size = 1.5
  ) +
  geom_text(
    data = emm_df_resp,
    aes(y = label_y, label = letters),
    color = "black",
    size = 4
  ) +
  scale_color_manual(
    values = pal,
    breaks = levels(avg_food$treatment),
    labels = levels(avg_food$treatment),
    name = "Treatment"
  ) +
  labs(
    title = "A. Third-instar nymphs",
    x = NULL,
    y = "Average diet (mg) eaten per day"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

preference_plot_third

ggsave(
  "Figure2.png",
  plot = preference_plot_third,
  width = 4,
  height = 3,
  dpi = 1000
)

# --------------------------------------------
# Average eaten, fourth instar
# --------------------------------------------

# Inspect filtered dataset
glimpse(data_survival_filtered)

# Keep only alive fourth-instar insects before day 15 and calculate
# daily and cumulative food consumption
data_alive <- data_survival_filtered %>%
  filter(status == 0) %>%   # keep only alive rows
  filter(instar == 4) %>%
  filter(day < 15) %>%      # keep only days < 15
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  mutate(
    food_eaten = lag(diet) - diet,
    food_eaten = ifelse(is.na(food_eaten) | food_eaten < 0, 0, food_eaten),
    food_eaten_cum = cumsum(food_eaten)
  ) %>%
  ungroup() %>%
  mutate(
    treatment = as.factor(treatment),
    food_eaten_cum_adj = food_eaten_cum + 0.01   # small constant for Gamma model
  )

glimpse(data_alive)

# Drop unused treatment levels
data_alive$treatment
data_alive$treatment <- droplevels(data_alive$treatment)
data_alive$treatment
unique(data_alive$treatment)

# Calculate average food eaten per day for each insect using the last
# recorded cumulative value divided by the number of days lived
avg_food <- data_alive %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  summarise(
    total_food = food_eaten_cum[which.max(day)],  # cumulative food at the last recorded day
    days_lived = max(day),
    treatment  = first(treatment),
    .groups    = "drop"
  ) %>%
  mutate(
    avg_food_per_day       = total_food / days_lived,
    avg_food_per_day_gamma = avg_food_per_day + 0.001  # small constant added for model fitting
  )

avg_food$treatment

# Relabel and reorder treatment levels for plotting and manuscript order
avg_food$treatment <- factor(
  avg_food$treatment,
  levels = c("A_ctrl", "B_ctrl"),
  labels = c("Diet A", "Diet B")
)

avg_food$treatment

# Define color palette in the same treatment order
pal <- c(
  "Diet A" = "#E2736FFF",
  "Diet B" = "#E9A66BFF"
  ### "Diet B + ailanthone"  = "#089392FF"
)

# Fit model using per-insect average food consumption
mo_gaussian <- glmmTMB(
  avg_food_per_day_gamma ~ treatment,
  data = avg_food
)

summary(mo_gaussian)

# Check model diagnostics
check_model(mo_gaussian)   # looks ok
res_gamma <- simulateResiduals(mo_gaussian)
plot(mo_gaussian)          # looks ok
check_residuals(mo_gaussian)  # Simulated residuals appear uniformly distributed (p = 0.664)

# Calculate estimated marginal means
emm_resp <- emmeans(mo_gaussian, ~ treatment)
emm_df_resp <- as.data.frame(emm_resp)

# Standardize confidence interval column names
if ("upper.CL" %in% names(emm_df_resp)) {
  emm_df_resp <- rename(emm_df_resp, ymin = lower.CL, ymax = upper.CL)
} else {
  emm_df_resp <- rename(emm_df_resp, ymin = asymp.LCL, ymax = asymp.UCL)
}

pairs_df <- as.data.frame(pairs(emm_resp, adjust = "tukey"))

# Extract p-value from model summary (glmmTMB, Wald z-test)
pvals <- summary(mo_gaussian)$coefficients$cond[2, "Pr(>|z|)"]

# Clean contrast names to "A-B" format for multcompLetters()
clean_contr <- pairs_df$contrast |>
  gsub("[()]", "", x = _) |>
  gsub("\\s*/\\s*", "-", x = _) |>
  gsub("\\s*-\\s*", "-", x = _)

names(pvals) <- clean_contr

letters_map <- multcompView::multcompLetters(pvals)$Letters

# Add significance letters and y positions for plotting
emm_df_resp$treatment <- factor(
  emm_df_resp$treatment,
  levels = levels(avg_food$treatment)
)
emm_df_resp$letters <- letters_map[as.character(emm_df_resp$treatment)]
emm_df_resp$label_y <- emm_df_resp$ymax + 15  # vertical position for significance letters

# Plot average food eaten per day
preference_plot_fourth <- ggplot(data = avg_food, aes(x = treatment)) +
  geom_point(
    aes(y = avg_food_per_day, color = treatment),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.6,
    size = 1.5
  ) +
  geom_errorbar(
    data = emm_df_resp,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1,
    color = "black",
    linewidth = 0.4
  ) +
  geom_point(
    data = emm_df_resp,
    aes(y = emmean),
    shape = 21,
    fill = "black",
    size = 1.5
  ) +
  geom_text(
    data = emm_df_resp,
    aes(y = label_y, label = letters),
    color = "black",
    size = 4
  ) +
  scale_color_manual(
    values = pal,
    breaks = levels(avg_food$treatment),
    labels = levels(avg_food$treatment),
    name = "Treatment"
  ) +
  labs(
    title = "B. Fourth-instar nymphs",
    x = NULL,
    y = "Average diet (mg) eaten per day"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

preference_plot_fourth

ggsave(
  "Figure2_4.png",
  plot = preference_plot_fourth,
  width = 4,
  height = 3,
  dpi = 500
)

Figure3_preference <- ggarrange(
  preference_plot_third,
  preference_plot_fourth,
  ncol = 1,
  nrow = 2,
  align = "hv",
  common.legend = FALSE,
  legend = "none"
)

Figure3_preference

ggsave(
  file = "Figure_4.jpg",
  plot = Figure3_preference,
  width = 4,
  height = 6,
  units = "in",
  dpi = 1000
)
