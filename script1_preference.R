# ------------------------------------------------------------------------
# Simulating and Visualizing Insect Feeding, Survival, and Growth
# ------------------------------------------------------------------------
# This script generates two synthetic datasets representing responses of 
# *Lycorma delicatula* nymphs and adults to liquid diets. It simulates:
#  - Amount consumed
#  - Survival (days)
#  - Growth (mg)
# for 3 life stages and 2 diet treatments per dataset.
# Results are visualized using violin plots stratified by life stage.
# ------------------------------------------------------------------------

# Clear Workspace
rm(list = ls())

# Load required packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)      
library(dplyr)   
library(DHARMa)     
library(emmeans)   
library(ggplot2)    
library(ggpubr)    
library(glmmTMB)   
library(sjPlot)  

# Set seed for reproducibility
set.seed(1994)

# -----------------------------------
# Define Experimental Parameters
# -----------------------------------

stages <- c("3rd_instar", "4th_instar", "adult")
diets1 <- c("A", "B")                          # Artificial Diets
diets2 <- c("control", "ailanthone")          # Ailanthone Treatments
n_replicates <- 15                             # Individuals per group

# -----------------------------------
# Function to Simulate Dataset
# -----------------------------------

generate_data <- function(stages, diets, n_replicates, dataset_type) {
  data <- data.frame()
  
  for (stage in stages) {
    for (diet in diets) {
      for (rep in 1:n_replicates) {
        
        # Default values by stage
        if (stage == "3rd_instar") {
          amount_consumed <- rnorm(1, 10, 2)
          survival <- rnorm(1, 20, 5)
          growth <- rnorm(1, 5, 1)
        } else if (stage == "4th_instar") {
          amount_consumed <- rnorm(1, 15, 3)
          survival <- rnorm(1, 18, 4)
          growth <- rnorm(1, 10, 2)
        } else if (stage == "adult") {
          amount_consumed <- rnorm(1, 20, 4)
          survival <- rnorm(1, 15, 3)
          growth <- rnorm(1, 3, 1)
        }
        
        # Modify consumption by diet type
        if (dataset_type == "dataset1") {
          if (diet == "A") amount_consumed <- rnorm(1, 15, 3)
          if (diet == "B") amount_consumed <- rnorm(1, 7.5, 2)
        } else if (dataset_type == "dataset2") {
          if (diet == "control") amount_consumed <- rnorm(1, 30, 5)
          if (diet == "ailanthone") amount_consumed <- rnorm(1, 10, 2)
        }
        
        # Store row
        data <- rbind(data, data.frame(stage, diet, amount_consumed, survival, growth))
      }
    }
  }
  
  return(data)
}

# -----------------------------------
# Generate Synthetic Datasets
# -----------------------------------

dataset1 <- generate_data(stages, diets1, n_replicates, "dataset1")
dataset2 <- generate_data(stages, diets2, n_replicates, "dataset2")

# -----------------------------------
# Visualization for Objective 1
# -----------------------------------
# Dataset 1: Diet A vs. Diet B

### Example of the model 
# Fit a Generalized Linear Model (GLM) for amount consumed
# Fixed effects: diet, stage, and their interaction
diet_amount_consumed_GLMM <- glmmTMB(
  amount_consumed ~ diet * stage,
  data = dataset1
)
# Type II Wald chi-square tests for fixed effects
Anova(diet_amount_consumed_GLMM)
# Simulate residuals for model diagnostics
sim <- simulateResiduals(diet_amount_consumed_GLMM)
# Plot residual diagnostics: check for uniformity and absence of patterns
plot(sim)
# Compute estimated marginal means (EMMs) for each diet and stage combination
emm_amount <- emmeans(diet_amount_consumed_GLMM, ~ diet * stage)
emm_amount_df <- as.data.frame(emm_amount)
emm_amount_df
# Conduct pairwise comparisons between diets within each stage
# Adjustment method: Tukey HSD
emm_gen_by_treat <- emmeans(diet_amount_consumed_GLMM, pairwise ~ diet | stage, adjust = "tukey")
emm_gen_by_treat$contrasts

####

# Aesthetic settings
diet1_colors <- c("A" = "yellowgreen", "B" = "palevioletred3")
facet_labels <- c("3rd_instar" = "3rd instar", "4th_instar" = "4th instar", "adult" = "Adult")

# Consumption plot
consumption_graph <- ggplot(dataset1, aes(x = diet, y = amount_consumed, fill = diet, color = diet)) +
  theme_classic(base_size = 14) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet1_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = " ", y = "Amount Consumed (mg)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right",
  ) +
  theme(legend.position = "none")

consumption_graph
# Survival plot
survival_graph <- ggplot(dataset1, aes(x = diet, y = survival, fill = diet, color = diet)) +
  theme_classic(base_size = 14) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet1_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = " ", y = "Survival (days)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right"
  ) +
  theme(legend.position = "null") 
survival_graph

# Growth plot
growth_graph <- ggplot(dataset1, aes(x = diet, y = growth, fill = diet, color = diet)) +
  theme_classic(base_size = 14) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet1_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = "Diet", y = "Growth (mg)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right",
  ) +
  theme(legend.position = "none")

# Combine into single figure
objective1_graph <- ggarrange(consumption_graph, survival_graph, growth_graph,
                              ncol = 1, nrow = 3, align = "hv", legend = NULL)

# Display and save
objective1_graph
ggsave("objective1_graph.jpg", plot = objective1_graph, width = 10, height = 12, units = "in", dpi = 300)

# -----------------------------------
# 6. Visualization for Objective 2
# -----------------------------------
# Dataset 2: Ailanthone vs. Control

diet2_colors <- c("control" = "yellowgreen", "ailanthone" = "#489fa7")

# Consumption
consumption_graph <- ggplot(dataset2, aes(x = diet, y = amount_consumed, fill = diet, color = diet)) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet2_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = " ", y = "Amount Consumed (mg)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right",
  ) +
  theme(legend.position = "none")

# Survival
survival_graph <- ggplot(dataset2, aes(x = diet, y = survival, fill = diet, color = diet)) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet2_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = " ", y = "Survival (days)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right",
  ) +
  theme(legend.position = "none")

# Growth
growth_graph <- ggplot(dataset2, aes(x = diet, y = growth, fill = diet, color = diet)) +
  geom_violin(color = "white", fill = "lightgrey", alpha = 0.3) +
  geom_point(size = 2, position = position_jitter(width = 0.1)) +
  facet_wrap(~stage, labeller = as_labeller(facet_labels)) +
  scale_color_manual(values = diet2_colors) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, color = "black") +
  labs(x = "Diet", y = "Growth (mg)", fill = "Diet", color = "Diet") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "right",
  ) +
  theme(legend.position = "none")

# Combine and export
objective2_graph <- ggarrange(consumption_graph, survival_graph, growth_graph,
                              ncol = 1, nrow = 3, align = "hv", legend = NULL)

objective2_graph
ggsave("objective2_graph.jpg", plot = objective2_graph, width = 10, height = 12, units = "in", dpi = 600)
