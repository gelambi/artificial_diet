# Development of an artificial liquid diet for allelochemical delivery in *Lycorma delicatula*

This repository contains data and scripts from artificial diet experiments with *L. delicatula*. The analyses focus on feeding preference and survival across life stages and dietary treatments. All the analyses were performed in R v. 4.4.2.

## Contents

### `script1_preference.R`

**Description:**\
Analyzes feeding behavior by quantifying diet consumption in third- and fourth-instar nymphs across artificial diet treatments (with and without ailanthone). The script processes raw diet data, calculates per-individual food intake, fits generalized linear models to test treatment effects, and generates figures.

**Data used:** `clean_data_liquitdiet.csv`

------------------------------------------------------------------------

### `script2_survival_curves.R`

**Description:**\
Analyzes survival of *L. delicatula* nymphs under artificial diet treatments and host plant conditions. The script processes survival data, corrects instar transitions using molting information, fits Kaplan–Meier survival curves, performs log-rank and pairwise comparisons, and generates survival curves and proportional survival bar plots across time.

**Data used:** `clean_data_liquitdiet.csv` and `survival_TOHandgrape.csv`

------------------------------------------------------------------------

### `script3_pilot_study.R`

**Description:**\
Analyzes mass change in response to ailanthone treatment and evaporation controls. The script tests differences between evaporation and feeding effects, and evaluates the interaction between ailanthone treatment and life stage on mass difference. It fits generalized linear models, performs post hoc comparisons, and generates figures summarizing treatment effects.

**Data used:** `clean_data_pilot.csv`
