# ======================================================================
# SCRIPT 2 - SURVIVAL
# ======================================================================
# This script analyzes survival of *Lycorma delicatula* nymphs across diet
# treatments and host plants. It processes survival data, corrects instar
# transitions using molting information, fits Kaplan–Meier survival curves,
# performs log-rank and pairwise comparisons, and generates
# survival curves and proportional survival bar plots across time.
# ======================================================================
# Author: Mariana Gelambi (gelambi@vt.edu)
# Last updated: 03-17-2026

# Clear Workspace
rm(list = ls())

# Load required packages
library(survival) 
library(survminer)    
library(dplyr)       
library(glmmTMB) 
library(paletteer)

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

# --------------------------------------------
# Prepare data for instar 3
# --------------------------------------------

df_instar3 <- data_survival %>%
  filter(instar == 3)

glimpse(df_instar3)

# Identify individuals that never died
never_died <- df_instar3 %>%
  group_by(insectID) %>%
  filter(all(status == 0)) %>%
  ungroup()

unique(never_died$insectID)

# Correct instar using molting information
df_instar3 <- df_instar3 %>%
  mutate(
    day = as.numeric(day),
    molted_clean    = tolower(trimws(replace(molted, is.na(molted), ""))),
    molted_binomial = as.integer(molted_clean == "molted")
  ) %>%
  group_by(insectID) %>%
  mutate(
    molted_day = ifelse(
      any(molted_binomial == 1, na.rm = TRUE),
      min(day[molted_binomial == 1], na.rm = TRUE),
      NA_real_
    ),
    instar_numeric   = as.numeric(as.character(instar)),
    instar_corrected = ifelse(
      !is.na(molted_day) & day >= molted_day,
      instar_numeric + 1L,
      instar_numeric
    )
  ) %>%
  ungroup()

glimpse(df_instar3)

# --------------------------------------------
# Prepare data for instar 4
# --------------------------------------------

df_instar4 <- data_survival %>%
  filter(instar == 4)

glimpse(df_instar4)

# Correct instar using molting information
df_instar4 <- df_instar4 %>%
  mutate(
    day = as.numeric(day),
    molted_clean    = tolower(trimws(replace(molted, is.na(molted), ""))),
    molted_binomial = as.integer(molted_clean == "molted")
  ) %>%
  group_by(insectID) %>%
  mutate(
    molted_day = ifelse(
      any(molted_binomial == 1, na.rm = TRUE),
      min(day[molted_binomial == 1], na.rm = TRUE),
      NA_real_
    ),
    instar_numeric   = as.numeric(as.character(instar)),
    instar_corrected = ifelse(
      !is.na(molted_day) & day >= molted_day,
      instar_numeric + 1L,
      instar_numeric
    )
  ) %>%
  ungroup()

# --------------------------------------------
# Survival curves: instar 3
# --------------------------------------------

surv_obj3 <- Surv(time = df_instar3$day, event = df_instar3$status)
fit3 <- survfit(surv_obj3 ~ treatment, data = df_instar3)

plot3 <- ggsurvplot(
  fit3,
  data = df_instar3,
  pval = TRUE,
  pval.method = TRUE,
  title = "A. Third-instar nymphs",
  xlab = "Days",
  ylab = "Survival Probability",
  legend.labs = c("Diet A", "Diet B + ailanthone", "Diet B"),
  palette = c("#E2736FFF", "#089392FF", "#E9A66BFF"),
  legend = "bottom",
  legend.title = "",
  ggtheme = theme_bw(base_size = 16) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

print(plot3)

ggsave(
  "survival_instar3.png",
  plot = plot3$plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Log-rank test and pairwise comparisons
surv_diff3 <- survdiff(surv_obj3 ~ treatment, data = df_instar3)
print(surv_diff3)

pairwise_results3 <- pairwise_survdiff(
  Surv(day, status) ~ treatment,
  data = df_instar3,
  p.adjust.method = "bonferroni"
)

print(pairwise_results3)

# --------------------------------------------
# Survival curves: instar 4
# --------------------------------------------

surv_obj4 <- Surv(time = df_instar4$day, event = df_instar4$status)
fit4 <- survfit(surv_obj4 ~ treatment, data = df_instar4)

plot4 <- ggsurvplot(
  fit4,
  data = df_instar4,
  pval = TRUE,
  pval.method = TRUE,
  title = "B. Fourth-instar nymphs",
  xlab = "Days",
  ylab = "Survival Probability",
  legend.labs = c("Diet A", "Diet B"),
  palette = c("#E2736FFF", "#E9A66BFF"),
  legend = "bottom",
  legend.title = "",
  ggtheme = theme_bw(base_size = 16) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

print(plot4)

ggsave(
  "survival_instar4.png",
  plot = plot4$plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Log-rank test
surv_diff4 <- survdiff(surv_obj4 ~ treatment, data = df_instar4)
print(surv_diff4)

# --------------------------------------------
# Survival curves: host plants
# --------------------------------------------

data_survival_TOH <- read.csv("survival_TOHandgrape.csv")
glimpse(data_survival_TOH)

surv_obj5 <- Surv(time = data_survival_TOH$day, event = data_survival_TOH$status)
fit5 <- survfit(surv_obj5 ~ plant, data = data_survival_TOH)

plot5 <- ggsurvplot(
  fit5,
  data = data_survival_TOH,
  pval = TRUE,
  pval.method = TRUE,
  title = "C. First- through fourth-instar nymphs reared on host plants",
  xlab = "Days",
  ylab = "Survival Probability",
  legend = "bottom",
  legend.labs = c("Grape", "Tree of Heaven"),
  legend.title = "",
  palette = c("#844A8E", "#9FBC5A"),
  xlim = c(0, 105),
  ggtheme = theme_bw(base_size = 16) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

print(plot5)

ggsave(
  "survival_TOH_grape.png",
  plot = plot5$plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Combine survival plots
Figure1_survival <- ggarrange(
  plot3$plot,
  plot4$plot,
  plot5$plot,
  ncol = 1,
  nrow = 3,
  align = "hv",
  common.legend = FALSE
)

Figure1_survival

ggsave(
  file = "Figure_2.jpg",
  plot = Figure1_survival,
  width = 8,
  height = 13,
  units = "in",
  dpi = 500
)

# ==============================================================================
# Bar plots: proportional survival by instar over time
# ==============================================================================

# Prepare data for instar 3
initial_counts <- df_instar3 %>%
  filter(day == 0) %>%
  group_by(treatment) %>%
  summarise(initial_n = n_distinct(insectID), .groups = "drop")

df_alive <- df_instar3 %>%
  filter(status == 0) %>%
  group_by(day, treatment, instar_corrected) %>%
  summarise(n_alive = n_distinct(insectID), .groups = "drop") %>%
  left_join(initial_counts, by = "treatment") %>%
  mutate(proportion_alive = n_alive / initial_n)

df_alive_named <- df_alive %>%
  mutate(
    treatment = dplyr::recode(
      treatment,
      "A_ctrl" = "Diet A",
      "B_ail"  = "Diet B + ailanthone",
      "B_ctrl" = "Diet B"
    ),
    instar_corrected = dplyr::recode(
      as.character(instar_corrected),
      "1" = "1st instar",
      "2" = "2nd instar",
      "3" = "3rd instar",
      "4" = "4th instar"
    )
  )

plot_3rd_instar <- ggplot(
  df_alive_named %>% filter(day %% 7 == 0),
  aes(x = factor(day), y = proportion_alive, fill = instar_corrected)
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ treatment) +
  labs(
    title = "A. Third-instar nymphs",
    x = "Day",
    y = "Proportion of Individuals Alive",
    fill = "Instar"
  ) +
  scale_fill_manual(values = c(
    "1st instar" = "#6FC0BAFF",
    "2nd instar" = "#F4C659FF",
    "3rd instar" = "#D9792EFF",
    "4th instar" = "#AF2213FF"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

plot_3rd_instar

# Prepare data for instar 4
initial_counts4 <- df_instar4 %>%
  filter(day == 0) %>%
  group_by(treatment) %>%
  summarise(initial_n = n_distinct(insectID), .groups = "drop")

df_alive4 <- df_instar4 %>%
  filter(status == 0) %>%
  group_by(day, treatment, instar_corrected) %>%
  summarise(n_alive = n_distinct(insectID), .groups = "drop") %>%
  left_join(initial_counts4, by = "treatment") %>%
  mutate(proportion_alive = n_alive / initial_n)

df_alive_named4 <- df_alive4 %>%
  mutate(
    treatment = dplyr::recode(
      treatment,
      "A_ctrl" = "Diet A",
      "B_ail"  = "Diet B + ailanthone",
      "B_ctrl" = "Diet B"
    ),
    instar_corrected = dplyr::recode(
      as.character(instar_corrected),
      "1" = "1st instar",
      "2" = "2nd instar",
      "3" = "3rd instar",
      "4" = "4th instar"
    )
  )

plot_4th_instar <- ggplot(
  df_alive_named4 %>% filter(day %% 7 == 0),
  aes(x = factor(day), y = proportion_alive, fill = instar_corrected)
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ treatment) +
  labs(
    title = "B. Fourth-instar nymphs",
    x = "Day",
    y = "Proportion of Individuals Alive",
    fill = "Instar"
  ) +
  scale_fill_manual(values = c(
    "1st instar" = "#6FC0BAFF",
    "2nd instar" = "#F4C659FF",
    "3rd instar" = "#D9792EFF",
    "4th instar" = "#AF2213FF"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

plot_4th_instar

# Prepare data for host plant bar plot
host_data <- read.csv("survival_TOHandgrape.csv")

host_alive <- host_data %>%
  filter(status == 0, instar != "unknown") %>%
  group_by(day, plant, instar) %>%
  summarise(alive_count = n_distinct(insectID), .groups = "drop")

host_initial <- host_alive %>%
  filter(day == 0) %>%
  group_by(plant) %>%
  summarise(initial_total = sum(alive_count), .groups = "drop")

host_final <- host_alive %>%
  left_join(host_initial, by = "plant") %>%
  mutate(
    relative_alive = alive_count / initial_total,
    group = "host",
    plant = dplyr::recode(plant, "TOH" = "Tree of Heaven", "grape" = "Grape")
  )

host_plants <- ggplot(host_final, aes(x = factor(day), y = relative_alive, fill = instar)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ plant) +
  labs(
    title = "C. First- through fourth-instar nymphs reared on host plants",
    x = "Day",
    y = "Proportion of Individuals Alive",
    fill = "Instar"
  ) +
  scale_fill_manual(values = c(
    "1st instar" = "#6FC0BAFF",
    "2nd instar" = "#F4C659FF",
    "3rd instar" = "#D9792EFF",
    "4th instar" = "#AF2213FF"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

host_plants

# Combine bar plots
Figure2_survival <- ggarrange(
  plot_3rd_instar,
  plot_4th_instar,
  host_plants,
  ncol = 1,
  nrow = 3,
  align = "hv",
  common.legend = FALSE,
  legend = "right"
)

Figure2_survival

ggsave(
  file = "Figure_3.jpg",
  plot = Figure2_survival,
  width = 8,
  height = 12,
  units = "in",
  dpi = 300
)
