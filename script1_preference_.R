
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
library(googledrive)
library(googlesheets4)

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

# Create a new dataframe keeping only rows where:
# 1. 'different' is not NA
# 2. 'different' is > 0
# 3. 'different' is less than or equal to 1000

data_survival_filtered <- data_survival %>%
  filter(!is.na(different), different >= 0, different <= 1000)

glimpse(data_survival_filtered)

glimpse(data_survival_filtered)
data_alive <- data_survival_filtered %>%
  filter(status == 0) %>%                      # Keep only alive rows
  filter(instar ==3) %>%
  filter(day <15) %>%               # Only keep days >= 15
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
    food_eaten_cum_adj =food_eaten_cum + 0.01     # Add small constant for Gamma model
  )
glimpse(data_alive)

### Since the cumulative metric is problematic, I will calculate the avrage eaten per day. 

# --------------------------------------------
# Average eaten, third instar
# --------------------------------------------

# Per-insect average food/day
# Using the last recorded cumulative value per insect divided by days lived
avg_food <- data_alive %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  summarise(
    total_food = food_eaten_cum[which.max(day)],  # cumulative at max day
    days_lived = max(day),
    treatment  = first(treatment),
    .groups = "drop"
  ) %>%
  mutate(
    avg_food_per_day       = total_food / days_lived,
    avg_food_per_day_gamma = avg_food_per_day + 0.001  # small constant for Gamma
  )

# Relabel & reorder treatment for paper order: Diet A, Diet B, Diet B + ailanthone
levels(avg_food$treatment) <- c("Diet A", "Diet B + ailanthone", "Diet B")
avg_food$treatment <- factor(avg_food$treatment,
                             levels = c("Diet A", "Diet B", "Diet B + ailanthone"))

# Palette in the same order
pal <- c(
  "Diet A"               = "#E2736FFF",
  "Diet B"               = "#E9A66BFF",
  "Diet B + ailanthone"  = "#089392FF"
)

glimpse(avg_food)
avg_food <- avg_food %>%
  filter(avg_food_per_day < 500)

# GLM (means per insect)
mod_gamma <- glmmTMB(
  avg_food_per_day_gamma ~ treatment,
  data   = avg_food
)

summary(mod_gamma)
Anova(mod_gamma)

# Diagnostics
??check_model
check_model(mod_gamma)
res_gamma <- simulateResiduals(mod_gamma)
plot(res_gamma)
check_residuals(mod_gamma) # OK: Simulated residuals appear as uniformly distributed (p = 0.139).

# Estimated marginal means & letters
# EMMs on the response scale
?emmeans
emm_resp    <- emmeans(mod_gamma, ~ treatment, type = "response")
emm_df_resp <- as.data.frame(emm_resp)

# Handle CI column names (emmeans may use lower.CL/upper.CL or asymp.LCL/asymp.UCL)
if ("upper.CL" %in% names(emm_df_resp)) {
  emm_df_resp <- rename(emm_df_resp, ymin = lower.CL, ymax = upper.CL)
} else {
  emm_df_resp <- rename(emm_df_resp, ymin = asymp.LCL, ymax = asymp.UCL)
}

# Build compact letter display from Tukey p-values (without cld())
pairs_df <- as.data.frame(pairs(emm_resp, adjust = "tukey"))
pvals    <- pairs_df$p.value
# Clean contrast names to "A-B" format for multcompLetters()
clean_contr <- pairs_df$contrast |>
  gsub("[()]", "", x = _) |>
  gsub("\\s*/\\s*", "-", x = _) |>
  gsub("\\s*-\\s*", "-", x = _)
names(pvals) <- clean_contr

letters_map <- multcompView::multcompLetters(pvals)$Letters

# Attach letters and a y-position just above the CI tops
emm_df_resp$treatment <- factor(emm_df_resp$treatment,
                                levels = levels(avg_food$treatment))
emm_df_resp$letters <- letters_map[as.character(emm_df_resp$treatment)]
emm_df_resp$label_y <- emm_df_resp$ymax + 50  # adjust spacing if needed

# Plot 
preference_plot_third <- ggplot(data = avg_food, aes(x = treatment)) +
  # jittered individual points
  geom_point(
    aes(y = avg_food_per_day, color = treatment),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.6, size = 1.5
  ) +
  # error bars (CI or SD) from model
  geom_errorbar(
    data = emm_df_resp,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1, color = "black", linewidth = 0.4
  ) +
  # predicted means
  geom_point(
    data = emm_df_resp,
    aes(y = emmean),
    shape = 21, fill = "black", size = 1.5
  ) +
  # significance letters
  geom_text(
    data = emm_df_resp,
    aes(y = label_y, label = letters),
    color = "black", size = 4
  ) +
  scale_color_manual(
    values = pal,
    breaks = levels(avg_food$treatment),
    labels = levels(avg_food$treatment),
    name = "Treatment"
  ) +
  labs(title = "A. Third-instar nymphs", x = NULL, y = "Average diet (mg) eaten per day") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

preference_plot_third
ggsave("Figure2.png", plot = preference_plot_third, width = 4, height = 3, dpi = 500)

# --------------------------------------------
# Average eaten, fourth instar
# --------------------------------------------

glimpse(data_survival_filtered)
data_alive <- data_survival_filtered %>%
  filter(status == 0) %>%                      # Keep only alive rows
  filter(instar ==4) %>%
  filter(day <15) %>%               # Only keep days < 15
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
    food_eaten_cum_adj =food_eaten_cum + 0.01     # Add small constant for Gamma model
  )
glimpse(data_alive)

data_alive$treatment
data_alive$treatment <- droplevels(data_alive$treatment)
data_alive$treatment
unique(data_alive$treatment)

# Per-insect average food/day
# Using the last recorded cumulative value per insect divided by days lived
avg_food <- data_alive %>%
  arrange(insectID, day) %>%
  group_by(insectID) %>%
  summarise(
    total_food = food_eaten_cum[which.max(day)],  # cumulative at max day
    days_lived = max(day),
    treatment  = first(treatment),
    .groups = "drop"
  ) %>%
  mutate(
    avg_food_per_day       = total_food / days_lived,
    avg_food_per_day_gamma = avg_food_per_day + 0.001  # small constant for Gamma
  )
avg_food$treatment

# Relabel & reorder treatment for paper order: Diet A, Diet B, Diet B + ailanthone
avg_food$treatment <- factor(avg_food$treatment,
                             levels = c("A_ctrl", "B_ctrl"),
                             labels = c("Diet A", "Diet B"))
avg_food$treatment
# Palette in the same order
pal <- c(
  "Diet A"               = "#E2736FFF",
  "Diet B"               = "#E9A66BFF"
 ### "Diet B + ailanthone"  = "#089392FF"
)

# GLM (means per insect)
mo_gaussian <- glmmTMB(
  avg_food_per_day_gamma ~ treatment,
  data   = avg_food
)
summary(mo_gaussian)

# Diagnostics
check_model(mo_gaussian) # loos ok 
res_gamma <- simulateResiduals(mo_gaussian)
plot(mo_gaussian) # looks ok!
check_residuals(mo_gaussian) # OK: Simulated residuals appear as uniformly distributed (p = 0.664).

# Estimated marginal means & letters
# EMMs on the response scale
emm_resp    <- emmeans(mo_gaussian, ~ treatment)
emm_df_resp <- as.data.frame(emm_resp)

# Handle CI column names (emmeans may use lower.CL/upper.CL or asymp.LCL/asymp.UCL)
if ("upper.CL" %in% names(emm_df_resp)) {
  emm_df_resp <- rename(emm_df_resp, ymin = lower.CL, ymax = upper.CL)
} else {
  emm_df_resp <- rename(emm_df_resp, ymin = asymp.LCL, ymax = asymp.UCL)
}
pairs_df <- as.data.frame(pairs(emm_resp, adjust = "tukey"))
# Extract p-value from model summary (glmmTMB, Wald z-test)
pvals  <- summary(mo_gaussian)$coefficients$cond[2, "Pr(>|z|)"]


# Clean contrast names to "A-B" format for multcompLetters()
clean_contr <- pairs_df$contrast |>
  gsub("[()]", "", x = _) |>
  gsub("\\s*/\\s*", "-", x = _) |>
  gsub("\\s*-\\s*", "-", x = _)
names(pvals) <- clean_contr

letters_map <- multcompView::multcompLetters(pvals)$Letters

# Attach letters and a y-position just above the CI tops
emm_df_resp$treatment <- factor(emm_df_resp$treatment,
                                levels = levels(avg_food$treatment))
emm_df_resp$letters <- letters_map[as.character(emm_df_resp$treatment)]
emm_df_resp$label_y <- emm_df_resp$ymax + 15  # adjust spacing if needed

# Plot 
preference_plot_fourth <- ggplot(data = avg_food, aes(x = treatment)) +
  # jittered individual points
  geom_point(
    aes(y = avg_food_per_day, color = treatment),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.6, size = 1.5
  ) +
  # error bars (CI or SD) from model
  geom_errorbar(
    data = emm_df_resp,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1, color = "black", linewidth = 0.4
  ) +
  # predicted means
  geom_point(
    data = emm_df_resp,
    aes(y = emmean),
    shape = 21, fill = "black", size = 1.5
  ) +
  # significance letters
  geom_text(
    data = emm_df_resp,
    aes(y = label_y, label = letters),
    color = "black", size = 4
  ) +
  scale_color_manual(
    values = pal,
    breaks = levels(avg_food$treatment),
    labels = levels(avg_food$treatment),
    name = "Treatment"
  ) +
  labs(title = "B. Fourth-instar nymphs", x = NULL, y = "Average diet (mg) eaten per day") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
preference_plot_fourth

ggsave("Figure2_4.png", plot = preference_plot_fourth, width = 4, height = 3, dpi = 500)

Figure3_preference <- ggarrange(preference_plot_third,
                              preference_plot_fourth,
                              ncol = 1,
                              nrow = 2,
                              align = "hv",
                              common.legend = FALSE,
                              legend = "none")

Figure3_preference

ggsave(file="Figure3_preference.png", 
       plot=Figure3_preference,
       width=4, height=6, units="in", dpi=500)



