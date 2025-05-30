# -------------------------------------------------------------------------
# Simulating and Analyzing Insect Survival under Different Diet Treatments
# -------------------------------------------------------------------------
# This script simulates discrete-time survival data for insects exposed to
# four treatments over a 14-day period. Survival curves are plotted
# and statistically compared using the log-rank test and pairwise comparisons.
# -------------------------------------------------------------------------

# Clear Workspace
rm(list = ls())

# Load required packages
library(survival)     # For survival analysis functions
library(survminer)    # For visualizing survival curves
library(dplyr)        # For data manipulation

# Set seed for reproducibility
set.seed(1994)

# -------------------------------
# Define Experimental Design
# -------------------------------

# Basic parameters
n_insects <- 15  # number of insects per treatment
treatments <- c("Diet A", "Diet B", "Tree of Heaven", "Grapes")
observation_days <- c(0, 2, 4, 7, 9, 11, 14)  # discrete observation points

# --------------------------------------------
# Simulate Discrete-Time Survival Data
# --------------------------------------------

# Function to simulate death days and survival status
simulate_discrete_survival <- function(treatment, probs) {
  death_days <- sample(observation_days[-1], n_insects, replace = TRUE, prob = probs)
  status <- ifelse(death_days < 14, 1, 0)      # 1 = died, 0 = censored
  death_days[status == 0] <- 14                # Censor at final day
  data.frame(Treatment = treatment,
             Time = death_days,
             Status = status)
}

# Define survival gradients (lower early death = higher survival)
death_probs <- list(
  "Tree of Heaven" = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.44),   # High survival
  "Grapes"         = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.25),    # Intermediate
  "Diet A"         = c(0.1, 0.2, 0.25, 0.2, 0.15, 0.1),      # Lower survival
  "Diet B"         = c(0.2, 0.25, 0.25, 0.15, 0.1, 0.05)     # Lowest survival
)

# Generate combined dataset
data <- bind_rows(lapply(treatments, function(t) {
  simulate_discrete_survival(t, probs = death_probs[[t]])
}))

# Preview data structure
head(data)

# Ensure proper factor ordering for plotting
data$Treatment <- factor(data$Treatment,
                         levels = c("Tree of Heaven", "Grapes", "Diet A", "Diet B"))

# --------------------------------------------
# Fit and Visualize Survival Curves
# --------------------------------------------

# Create a survival object
surv_obj <- Surv(time = data$Time, event = data$Status)

# Fit Kaplan-Meier survival curves
fit <- survfit(surv_obj ~ Treatment, data = data)
?ggsurvplot
# Plot survival curves with customization
survival <- ggsurvplot(fit,
                       data = data,
                       pval = TRUE,
                       pval.method = TRUE,
                       palette = c("#1b7837","#762a83", "#fc8d59","#d73027"),
                       title = "Simulated Insect Survival by Diet",
                       xlab = "Days",
                       ylab = "Survival Probability",
                       legend = "bottom",
                       legend.title = "",
                       ggtheme = theme_bw() +
                         theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               legend.position = "bottom"))

# Display the plot
survival
ggsave("survival_plot.png",
       plot = survival$plot,
       width = 7,
       height = 5,
       dpi = 300)
# --------------------------------------------
# 5. Statistical Analysis
# --------------------------------------------

# Global log-rank test (are any groups different?)
surv_diff <- survdiff(surv_obj ~ Treatment, data = data)
print(surv_diff)

# --------------------------------------------
# 6. Pairwise Comparisons (Log-Rank Test)
# --------------------------------------------

# Perform pairwise log-rank tests with Bonferroni correction
pairwise_results <- pairwise_survdiff(
  Surv(Time, Status) ~ Treatment,
  data = data,
  p.adjust.method = "bonferroni"
)

# View pairwise comparison results
print(pairwise_results)
