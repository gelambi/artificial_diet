# Research Toward Development and Evaluation of Artificial Liquid Diets for Xenobiotic Delivery in *Lycorma delicatula* (Spotted Lanternfly)

This repository contains scripts that simulate and analyze data from artificial diet experiments with *L. delicatula*. The analyses focus on feeding preference, survival, and growth responses across life stages and dietary treatments.

## Contents

### `script1_preference.R`

Simulates feeding behavior, survival, and growth of 3rd instar, 4th instar, and adult *L. delicatula* under two dietary conditions: artificial diets (A vs. B) and ailanthone treatment (control vs. ailanthone). 

This script:
- Generates two datasets reflecting different diet types
- Fits generalized linear models using the `glmmTMB` package
- Conducts estimated marginal means and pairwise comparisons with `emmeans`

### `script2_survivalcurves.R`

Simulates individual-level survival data for insects exposed to four diets (Diet A, Diet B, Tree of Heaven, Grapes) over a 14-day period. 

This script:
- Uses the `survival` and `survminer` packages for modeling and visualization
- Generates synthetic survival data with right-censoring
- Fits Kaplan-Meier survival curves and visualizes them
- Performs log-rank tests and pairwise survival comparisons

---

