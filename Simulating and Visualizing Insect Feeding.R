# ------------------------------------------------------------------------
# Simulating and Visualizing Insect Feeding
# ------------------------------------------------------------------------
# This script generates two synthetic datasets representing responses of 
# *Lycorma delicatula* nymphs and adults to liquid diets.
# ------------------------------------------------------------------------

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
library(effects)

# --------------------------------------------
# Read data
# --------------------------------------------
gs4_auth()
# Authenticate your Google account so R can access your Sheets
# This will open a browser window the first time you run it and ask you to sign in
# After you approve access, the credentials will be cached for later sessions
# Really cool! 

# Read data from a Google Sheet
# Arguments:
#   1) The full URL (or just the spreadsheet ID) of the Google Sheet
#   2) sheet = 2 specifies that we want the *second tab* in the spreadsheet
# The result is stored in the data frame 'data_survival'
data_survival <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1QLzJDluKkMFspEWSnbZq_xxjnj_scNjHkz6HKomqhNg",
  sheet = 2
)

# Rename the columns to short, consistent names for easier use in the analysis
data_survival <- data_survival %>%
  rename(
    insectID  = `Insect ID`,
    cage      = Cage,
    instar    = Instar,
    treatment = Treatment,
    day       = Day,
    date      = `Date (YYMMDD)`,
    status    = `Alive? (0 = dead, 1 = alive)`,
    diet      = `Diet Mass (mg)`,
    different = `difference (mg)`,
    drips     = `Drips?`,
    ants      = Ants,
    molted    = Molted
  )
glimpse(data_survival)
data_survival <- data_survival %>% 
  slice(-n())

unique(data_survival$instar) # 3 4, looks good. 
unique(data_survival$treatment) # [1] "A_ctrl" "B_ctrl" "B_crl"  "B_ail". B_crl is a typo. 
# Fix typo in treatment column
data_survival <- data_survival %>%
  mutate(treatment = case_when(
    treatment == "B_crl" ~ "B_ctrl",
    TRUE ~ treatment
  ))
unique(data_survival$treatment) # Typo fixed. "A_ctrl" "B_ctrl" "B_ail" 

data_survival <- data_survival %>%
  mutate(instar = factor(instar))
# Flip status values: 1 becomes 0, and 0 becomes 1
data_survival <- data_survival %>%
  mutate(status = ifelse(status == 1, 0, 1))

data_survival <- data_survival %>%
  mutate(instar = factor(instar)) %>%
  mutate(treatment = factor(treatment)) %>%
  mutate(insectID = factor(insectID))

glimpse(data_survival)
ggplot(data_survival_filtered, aes(x = treatment, y = different, fill = treatment)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2, shape = 21, stroke = 0.3) +
  labs(
    title = "Diet per Treatment",
    x = "Treatment",
    y = "Diet (units)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Create a new dataframe keeping only rows where:
# 1. 'different' is not NA
# 2. 'different' is > 0
# 3. 'different' is less than or equal to 1000

data_survival_filtered <- data_survival %>%
  filter(!is.na(different), different >= 0, different <= 1000)
# ------------------------------
# Clean and prepare data
# ------------------------------
glimpse(data_survival_filtered)
data_alive <- data_survival_filtered %>%
  filter(status == 0) %>%                      # Keep only alive rows
  filter(instar ==3) %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  mutate(
    food_eaten = lag(diet) - diet,
    food_eaten = ifelse(is.na(food_eaten) | food_eaten < 0, 0, food_eaten),
    amount_eaten = cumsum(food_eaten)
  ) %>%
  ungroup() %>%
  mutate(
    treatment = as.factor(treatment),
    amount_eaten_adj = amount_eaten + 0.01     # Add small constant for Gamma model
  )
glimpse(data_alive)

avg_daily_per_insect

ggplot(data_alive, aes(x = day, y = amount_eaten, color = treatment)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(group = insectID), alpha = 0.3) +
  labs(
    title = "Cumulative Amount Eaten Over Time by Treatment",
    x = "Day",
    y = "Cumulative Amount Eaten (adjusted)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggplot(data_alive, aes(x = day, y = food_eaten, color = treatment)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(group = insectID), alpha = 0.3) +
  labs(
    title = "Daily Net Amount Eaten by Treatment",
    x = "Day",
    y = "Net Food Eaten (per day)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# ------------------------------------
# Fit GLMM using glmmTMB
# ------------------------------------
# Filter the data
data_alive_filtered <- data_alive %>%
  filter(day <= 14)

glmm_diets <- glmmTMB(
  food_eaten ~ treatment + (1|insectID) ,
  data = data_alive_filtered
)
# Simulate residuals
sim_res <- simulateResiduals(fittedModel = glmm_diets, plot = TRUE)

data_alive <- data_alive %>%
  mutate(food_eaten_gamma = food_eaten + 0.001)

glmm_diets_gamma <- glmmTMB(
  food_eaten_gamma ~ treatment + (1 | insectID),
  data = data_alive,
  family = Gamma(link = "log")
)
summary(glmm_diets_gamma)
plot(allEffects(glmm_diets_gamma))

# Get emmeans from gamma model
emmeans_gamma_df <- as.data.frame(
  emmeans(glmm_diets_gamma, ~ treatment, type = "response")
)

emmeans_gamma_df <- emmeans_gamma_df %>%
  mutate(treatment = factor(treatment,
                            levels = c("A_ctrl", "B_ail", "B_ctrl"),
                            labels = c("Diet A", "Diet B + ailanthone", "Diet B")))

data_alive_filtered <- data_alive_filtered %>%
  mutate(treatment = factor(treatment,
                            levels = c("A_ctrl", "B_ail", "B_ctrl"),
                            labels = c("Diet A", "Diet B + ailanthone", "Diet B")))

third_instar <- ggplot() +
  # Raw data points (colored by treatment)
  geom_jitter(
    data = data_alive,
    aes(x = treatment, y = food_eaten_gamma, color = treatment),
    width = 0.1,
    height = 0,
    alpha = 0.7,
    size = 1.5
  ) +
  # Model predictions (black)
  geom_point(
    data = emmeans_gamma_df,
    aes(x = treatment, y = response),
    color = "black",
    size = 3
  ) +
  geom_linerange(
    data = emmeans_gamma_df,
    aes(x = treatment, ymin = asymp.LCL, ymax = asymp.UCL),
    color = "black",
    linewidth = 0.6
  ) +
  labs(
    x = "Treatment",
    y = "Food Eaten per Day (mg)",
    color = "Diet"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(),
    plot.title = element_text(size = 15, hjust = 0.5),
  )
third_instar



glmm_diets <- glmmTMB(
  amount_eaten ~ treatment * day + (1|insectID),
  data = data_alive_filtered
)
plot(allEffects(glmm_diets))
summary(glmm_diets)               # Model coefficients
Anova(glmm_diets)                 # Type II ANOVA
sim_glmm <- simulateResiduals(glmm_diets)
plot(sim_glmm)                    # Residual diagnostics
plot(allEffects(glmm_diets))
# Create grid of predictions
new_data <- expand.grid(
  day = unique(data_alive_filtered$day),
  treatment = unique(data_alive_filtered$treatment)
)

# Predict from model (population-level, no random effects)

preds <- predict(glmm_diets, newdata = new_data, se.fit = TRUE, type = "response", re.form = NA)

new_data$fit <- preds$fit
new_data$lwr <- preds$fit - 1.96 * preds$se.fit
new_data$upr <- preds$fit + 1.96 * preds$se.fit

# Plot
ggplot(data_alive_filtered, aes(x = day, y = amount_eaten_adj, color = treatment)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_ribbon(
    data = new_data,
    aes(x = day, ymin = lwr, ymax = upr, fill = treatment),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  # Custom color palette for data points
  scale_color_manual(
    values = c(
      "Diet A" = "#E2736FFF",
      "Diet B + ailanthone" = "#089392FF",
      "Diet B" = "#E9A66BFF"
    )
  ) +
  geom_line(data = new_data, aes(x = day, y = fit, color = treatment), size = 1) +
  labs(
    x = "Day",
    y = "Cumulative Amount Eaten",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

####

# Step 1: Filter for relevant treatments and days ≤ 14
df_filtered <- data_survival %>%
  filter(day <= 14, 
         insectID != 59,
         instar == 3)

# Step 2: Create alive column (status == 1 means alive)
df_filtered <- df_filtered %>%
  mutate(alive = status == 1)

# Step 3: Summarize per insect: total days alive and total consumption only on days alive
summary_df <- df_filtered %>%
  filter(alive) %>%
  group_by(insectID, treatment) %>%
  summarise(
    days_alive = n(),
    total_eaten = sum(different, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(average_eaten = total_eaten / days_alive)

# Step 4: Fit the model with treatment and days_alive as predictors
model <- glmmTMB(average_eaten ~ treatment*days_alive, data = summary_df)

# Step 5: View model summary
summary(model)
plot(allEffects(model))
emmeans(model, pairwise ~ treatment)

emm <- emmeans(model, ~ treatment)
emm_df <- as.data.frame(emm)

# Plot
ggplot() +
  # Raw data (from summary_df)
  geom_jitter(data = summary_df, aes(x = treatment, y = average_eaten), 
              width = 0.1, alpha = 0.6, size = 2, color = "gray40") +
  
  # Model-predicted means (from emm_df)
  geom_point(data = emm_df, aes(x = treatment, y = emmean), 
             size = 3, color = "black") +
  
  # Confidence intervals (from emm_df)
  geom_errorbar(data = emm_df, aes(x = treatment, ymin = lower.CL, ymax = upper.CL), 
                width = 0.15, size = 0.8, color = "black") +
  
  labs(x = "Treatment", y = "Average Daily Consumption (mg)", 
       title = "Effect of Ailanthone on SLF Feeding Preference") +
  theme_minimal(base_size = 14)

library(glmmTMB)
library(ggplot2)
library(dplyr)
library(DHARMa)
library(car)

# Step 1: Prepare data
data_alive <- data_alive %>%
  mutate(
    amount_eaten_adj = amount_eaten + 0.01,      # Avoid zeros for Gamma
    treatment = factor(treatment),               # Ensure treatment is factor
    day = as.numeric(day)                        # Ensure day is numeric
  )

# Step 2: Fit Gamma GLMM with interaction
glmm_diets <- glmmTMB(
  amount_eaten_adj ~ day*treatment + (1 | insectID),
  data = data_alive_filtered,
  family = Gamma(link = "log")
)

# Step 3: Diagnostics
summary(glmm_diets)
Anova(glmm_diets)
sim_glmm <- simulateResiduals(glmm_diets)
plot(sim_glmm)

# Step 4: Generate prediction grid only for observed (day × treatment) combos
new_data <- data_alive_filtered %>%
  group_by(day, treatment) %>%
  summarise(.groups = "drop")  # keep only real combinations

# Step 5: Predict (marginal/fixed effects only)
preds <- predict(
  glmm_diets,
  newdata = new_data,
  se.fit = TRUE,
  type = "response",
  re.form = NA
)

# Step 6: Add predictions and 95% CI
new_data$fit <- preds$fit
new_data$lwr <- pmax(preds$fit - 1.96 * preds$se.fit, 0)  # clip lower CI at 0
new_data$upr <- preds$fit + 1.96 * preds$se.fit

# Step 7: Plot actual points + predicted trend + CI
ggplot(data_alive_filtered, aes(x = day, y = amount_eaten_adj, color = treatment)) +
  geom_point(alpha = 0.6, size = 2) + 
  geom_ribbon(
    data = new_data,
    aes(x = day, ymin = lwr, ymax = upr, fill = treatment),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = new_data,
    aes(x = day, y = fit, color = treatment),
    size = 1
  ) +
  labs(
    title = "Gamma GLMM: Predicted Cumulative Amount Eaten Over Time",
    x = "Day",
    y = "Cumulative Amount Eaten (adjusted + 0.01)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


