# ==============================================================================
# Survival of *Lycorma delicatula* Nymphs Under Diet and Host Plant
# ==============================================================================

# ------------------------------------------------------------------------------
# This script processes and visualizes survival data for *Lycorma delicatula*
# nymphs across two types of experiments:
# (1) Survival under artificial diet treatments (with/without ailanthone)
#     for 3rd- and 4th-instar nymphs.
# (2) Survival on two host plants (Tree of Heaven and Grape) for 1st–4th instars.
#
# It generates Kaplan–Meier survival curves and bar plots showing proportional
# survival by instar over time. Molting events are tracked 
# to correct instar classification across timepoints.
# ------------------------------------------------------------------------------

# Clear Workspace
rm(list = ls())

# Load required packages
library(survival) 
library(survminer)    
library(dplyr)       
library(glmmTMB) 
library(paletteer)
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

# Filter for instar 3
df_instar3 <- data_survival %>%
  filter(instar == 3)
glimpse(df_instar3)

never_died <- df_instar3 %>%
  group_by(insectID) %>%
  filter(all(status == 0)) %>%  # keep only IDs with no deaths
  ungroup()
unique(never_died$insectID)

df_instar3 <- df_instar3 %>%
  mutate(
    day = as.numeric(day),
    # normalize 'molted' and avoid NAs
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
    instar_corrected = ifelse(!is.na(molted_day) & day >= molted_day,
                              instar_numeric + 1L,
                              instar_numeric)
  ) %>%
  ungroup()

glimpse(df_instar3)
# Filter for instar 4
df_instar4 <- data_survival %>%
  filter(instar == 4)
glimpse(df_instar4)

df_instar4 <- df_instar4 %>%
  mutate(
    day = as.numeric(day),
    # normalize 'molted' and avoid NAs
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
    instar_corrected = ifelse(!is.na(molted_day) & day >= molted_day,
                              instar_numeric + 1L,
                              instar_numeric)
  ) %>%
  ungroup()

# ----------------------------
# Survival curve for Instar 3
# ----------------------------
surv_obj3 <- Surv(time = df_instar3$day, event = df_instar3$status)
fit3 <- survfit(surv_obj3 ~ treatment, data = df_instar3)

plot3 <- ggsurvplot(fit3,
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
                      theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            legend.position = "bottom"))

print(plot3)
ggsave("survival_instar3.png", plot = plot3$plot, width = 7, height = 5, dpi = 300)

# Log-rank test for Instar 3
surv_diff3 <- survdiff(surv_obj3 ~ treatment, data = df_instar3)
print(surv_diff3)

# Pairwise comparisons for Instar 3
pairwise_results3 <- pairwise_survdiff(Surv(day, status) ~ treatment, data = df_instar3, p.adjust.method = "bonferroni")
print(pairwise_results3)

# ----------------------------
# Survival curve for Instar 4
# ----------------------------
surv_obj4 <- Surv(time = df_instar4$day, event = df_instar4$status)
fit4 <- survfit(surv_obj4 ~ treatment, data = df_instar4)

plot4 <- ggsurvplot(fit4,
                    data = df_instar4,
                    pval = TRUE,
                    pval.method = TRUE,
                    title = "B. Fourth-instar nymphs",
                    xlab = "Days",
                    legend.labs = c("Diet A", "Diet B"),
                    palette = c("#E2736FFF", "#E9A66BFF"),
                    ylab = "Survival Probability",
                    legend = "bottom",
                    legend.title = "",
                    ggtheme = theme_bw(base_size = 16) +
                      theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            legend.position = "bottom"))

print(plot4)
ggsave("survival_instar4.png", plot = plot4$plot, width = 7, height = 5, dpi = 300)

# Log-rank test for Instar 4
surv_diff4 <- survdiff(surv_obj4 ~ treatment, data = df_instar4)
print(surv_diff4)

#####

data_survival_TOH <- read.csv("survival_TOHandgrape.csv")
glimpse(data_survival_TOH)

surv_obj5 <- Surv(time = data_survival_TOH$day, event = data_survival_TOH$status)
fit5 <- survfit(surv_obj5 ~ plant, data = data_survival_TOH)
summary(fit5)
summary(data_survival_TOH$day)
# Plot with custom colors
plot5 <- ggsurvplot(fit5,
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
                    xlim = c(0, 105),  # Extend beyond 105
                    ggtheme = theme_bw(base_size = 16) +
                      theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            legend.position = "bottom"))

# Print and save
print(plot5)
ggsave("survival_TOH_grape.png", plot = plot4$plot, width = 7, height = 5, dpi = 300)

Figure1_survival <- ggarrange(plot3$plot,
                              plot4$plot,
                              plot5$plot,
                              ncol = 1,
                              nrow = 3,
                              align = "hv",
                              common.legend = FALSE)
Figure1_survival
ggsave(file="Figure1.png", 
       plot= Figure1_survival,
       width=8,height=13,units="in",dpi=500)

# ==============================================================================
# Bar Plots: Proportional Survival by Instar Over Time
# ==============================================================================

# ----------------------------------------------------------
# Prepare data for Instar 3 bar plot
# ----------------------------------------------------------
# Get initial counts per treatment
initial_counts <- df_instar3 %>%
  filter(day == 0) %>%
  group_by(treatment) %>%
  summarise(initial_n = n_distinct(insectID), .groups = "drop")

# Alive counts per day/treatment/instar
df_alive <- df_instar3 %>%
  filter(status == 0) %>%
  group_by(day, treatment, instar_corrected) %>%
  summarise(n_alive = n_distinct(insectID), .groups = "drop") %>%
  left_join(initial_counts, by = "treatment") %>%
  mutate(proportion_alive = n_alive / initial_n)

df_alive_named <- df_alive %>%
  mutate(
    treatment = recode(treatment,
                       "A_ctrl" = "Diet A",
                       "B_ail"  = "Diet B + ailanthone",
                       "B_ctrl" = "Diet B"
    ),
    instar_corrected = recode(as.character(instar_corrected),
                              "1" = "1st instar",
                              "2" = "2nd instar",
                              "3" = "3rd instar",
                              "4" = "4th instar"
    )
  )

plot_3rd_instar <- ggplot(
  df_alive_named %>% dplyr::filter(day %% 7 == 0), 
  aes(x = factor(day), y = proportion_alive, fill = instar_corrected)
) +
  geom_bar(stat = "identity") +             # same as geom_col()
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
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
  )

plot_3rd_instar

# ----------------------------------------------------------
# Prepare data for Instar 4 bar plot
# ----------------------------------------------------------
# Initial counts per treatment (from df_instar4)
initial_counts4 <- df_instar4 %>%
  dplyr::filter(day == 0) %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(initial_n = dplyr::n_distinct(insectID), .groups = "drop")

# Alive counts per day/treatment/instar (from df_instar4)
df_alive4 <- df_instar4 %>%
  dplyr::filter(status == 0) %>%
  dplyr::group_by(day, treatment, instar_corrected) %>%
  dplyr::summarise(n_alive = dplyr::n_distinct(insectID), .groups = "drop") %>%
  dplyr::left_join(initial_counts4, by = "treatment") %>%
  dplyr::mutate(proportion_alive = n_alive / initial_n)

# Labels and nice names
df_alive_named4 <- df_alive4 %>%
  dplyr::mutate(
    treatment = dplyr::recode(treatment,
                              "A_ctrl" = "Diet A",
                              "B_ail"  = "Diet B + ailanthone",
                              "B_ctrl" = "Diet B"
    ),
    instar_corrected = dplyr::recode(as.character(instar_corrected),
                                     "1" = "1st instar",
                                     "2" = "2nd instar",
                                     "3" = "3rd instar",
                                     "4" = "4th instar"
    )
  )

# Plot (match style of your 3rd-instar plot)
plot_4th_instar <- ggplot(
  df_alive_named4 %>% dplyr::filter(day %% 7 == 0),
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

# ----------------------------------------------------------
# Prepare data for host plant bar plot
# ----------------------------------------------------------
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
  scale_fill_manual(values = c("1st instar" = "#6FC0BAFF", 
                               "2nd instar" = "#F4C659FF", 
                               "3rd instar" = "#D9792EFF", 
                               "4th instar" = "#AF2213FF")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
host_plants

# Combine into one figure
Figure2_survival <- ggarrange(plot_3rd_instar, 
                              plot_4th_instar,
                              host_plants,
                              ncol = 1,
                              nrow = 3,
                              align = "hv",
                              common.legend = FALSE,
                              legend = "right")

Figure2_survival

ggsave(file="Figure2.jpg", 
       plot=Figure2_survival,
       width=8, height=12, units="in", dpi=300)

