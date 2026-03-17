# ======================================================================
# SCRIPT 3 - PILOT STUDY
# ======================================================================
  # This script analyzes mass difference in *Lycorma delicatula* under two
  # related experimental questions. First, it tests whether mass change differs
  # between evaporation controls and insect feeding samples. Second, it evaluates
  # the effects of ailanthone treatment, life stage, and their interaction on
  # mass difference after excluding control-only observations.
  #
  # The script fits generalized linear mixed models, checks model diagnostics,
  # calculates estimated marginal means and pairwise contrasts, and generates
  # publication-ready figures showing treatment effects on mass difference.
# ======================================================================
# Author: Mariana Gelambi (gelambi@vt.edu)
# Last updated: 03-17-2026

# Clear Workspace
rm(list = ls())

# Load required packages
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
library(forcats)

# --------------------------------------------
# Read data
# --------------------------------------------

data <- read.csv("clean_data_pilot.csv") %>%
  mutate(
    ailanthone = dplyr::recode(as.character(ailanthone),
                               "Y" = "Ailanthone",
                               "N" = "Control"),
    life_stage = dplyr::recode(as.character(life_stage),
                               "4th" = "Fourth-instar nymph",
                               "adult" = "Adults"),
    sex = factor(sex),
    ailanthone = factor(ailanthone),
    life_stage = factor(life_stage),
    evaporation = factor(evaporation)
  )
glimpse(data)

model0 <- glmmTMB(
  mass_difference_mg ~ evaporation,
  data = data
)

# Model summary and diagnostics
summary(model0)
Anova(model0)
check_model(model0)
res0 <- simulateResiduals(model0)
plot(res0)

data <- data %>%
  mutate(evaporation = fct_recode(evaporation,
                                  "Evaporation control" = "control",
                                  "Insect feeding" = "insect"
  ))

# Estimated marginal means
emm_evap <- emmeans(model0, ~ evaporation)
emm_df <- as.data.frame(emm_evap)

# Rename for plotting
emm_df <- emm_df %>%
  rename(
    emmean = emmean,
    ymin = lower.CL,
    ymax = upper.CL
  )

# Pairwise comparisons
pairs_df <- as.data.frame(pairs(emm_evap, adjust = "tukey"))
pvals <- pairs_df$p.value
names(pvals) <- gsub(" - ", "-", pairs_df$contrast)
letters_map <- multcompView::multcompLetters(pvals)$Letters
emm_df$letters <- letters_map[as.character(emm_df$evaporation)]
emm_df$label_y <- emm_df$ymax + 5  # label spacing


# Plot
evap_plot <- ggplot(data, aes(x = evaporation)) +
  geom_point(
    aes(y = mass_difference_mg, color = evaporation),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.8, size = 2
  ) +
  geom_errorbar(
    data = emm_df,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1, color = "black", linewidth = 0.4
  ) +
  geom_point(
    data = emm_df,
    aes(y = emmean),
    shape = 21, fill = "black", size = 3
  ) +
  geom_text(
    data = emm_df,
    aes(y = label_y, label = letters),
    color = "black", size = 4
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Mass difference (mg)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

evap_plot

# Save plot
ggsave(
  filename = "Figure_S1.jpg",
  plot = evap_plot,
  width = 4,
  height = 4,
  units = "in",
  dpi = 500
)




#####
#####
#####

data <- data %>%
  filter(life_stage != "control")

# Gaussian
model1 <- glmmTMB(
  mass_difference_mg ~ ailanthone*life_stage,
  data = data
)

summary(model1)
Anova(model1)

# Diagnostics

check_model(model1)
res_gamma <- simulateResiduals(model1)
plot(res_gamma)
check_residuals(model1) # OK: Simulated residuals appear as uniformly distributed (p = 0.522).


# Estimated marginal means and pairwise contrasts
emm_resp <- emmeans(model1, ~ ailanthone | life_stage, type = "response")
emm_df_resp <- as.data.frame(emm_resp)
emm_df_resp
# Rename CI columns if needed
if ("upper.CL" %in% names(emm_df_resp)) {
  emm_df_resp <- rename(emm_df_resp, ymin = lower.CL, ymax = upper.CL)
} else {
  emm_df_resp <- rename(emm_df_resp, ymin = asymp.LCL, ymax = asymp.UCL)
}

# Pairwise comparisons (Tukey adjusted)
pairs_df <- as.data.frame(pairs(emm_resp, adjust = "tukey"))
pvals <- pairs_df$p.value
clean_contr <- gsub("[()]", "", pairs_df$contrast) |>
  gsub("\\s*/\\s*", "-", x = _) |>
  gsub("\\s*-\\s*", "-", x = _)
names(pvals) <- clean_contr
letters_map <- multcompView::multcompLetters(pvals)$Letters

# Set palette
pal <- RColorBrewer::brewer.pal(n = 3, name = "Set1")

# Reorder factor levels for plotting
data <- data %>%
  mutate(
    life_stage = fct_relevel(life_stage, "Fourth-instar nymph", "Adults"),
    ailanthone = fct_relevel(ailanthone, "Control", "Ailanthone")
  )

emm_df_resp <- emm_df_resp %>%
  mutate(
    life_stage = fct_relevel(life_stage, "Fourth-instar nymph", "Adults"),
    ailanthone = fct_relevel(ailanthone, "Control", "Ailanthone")
  )

emm_df_resp <- emm_df_resp %>%
  mutate(
    life_stage = fct_relevel(life_stage, "Fourth-instar nymph", "Adults"),
    ailanthone = fct_relevel(ailanthone, "Control", "Ailanthone"),
    letters = case_when(
      life_stage == "Fourth-instar nymph" ~ "a",
      life_stage == "Adults" & ailanthone == "Control" ~ "a",
      life_stage == "Adults" & ailanthone == "Ailanthone" ~ "b"
    ),
    label_y = ymax + 5  # spacing above the error bar
  )


# Plot
mass_plot <- ggplot(data, aes(x = ailanthone)) +
  geom_point(
    aes(y = mass_difference_mg, color = ailanthone),
    position = position_jitter(width = 0.2, height = 0, seed = 1),
    alpha = 0.6, size = 2
  ) +
  geom_errorbar(
    data = emm_df_resp,
    aes(ymin = ymin, ymax = ymax),
    width = 0.1, color = "black", linewidth = 0.4
  ) +
  geom_point(
    data = emm_df_resp,
    aes(y = emmean),
    shape = 21, fill = "black", size = 3
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  facet_wrap(~ life_stage) +  # free Y-axis
  labs(
    x = NULL,
    y = "Mass difference (mg)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  geom_text(
    data = emm_df_resp,
    aes(y = label_y, label = letters),
    color = "black", size = 4
  )

mass_plot

ggsave(file="Figure_S2.jpg", 
       plot=mass_plot,
       width=6, height=4, units="in", dpi=500)

