# psoriasis-epidemiology-analysis

Childhood Infections and Risk of Psoriasis in Early Adulthood

This repository contains the statistical analysis code for the manuscript:

> **Infections during childhood as predisposing factors to develop psoriasis into early adulthood**  

## Overview

This analysis investigates whether infections during childhood are associated with an increased risk of developing psoriasis in early adulthood. The workflow includes:

- **Univariate logistic regression** to evaluate crude associations between each covariate and psoriasis risk.
- **Multivariable logistic regression** adjusting for relevant confounders.
- **Propensity score matching (PSM)** using nearest neighbor matching without replacement to balance confounders between exposed and unexposed groups.
- **Conditional logistic regression** for matched data.
- **Covariate balance assessment** using standardized mean differences (SMDs) before and after matching.

## Matching Details

- **Matching method:** Nearest neighbor (1:1) without replacement.
- **Caliper width:** None (no restriction applied).
- **Balance check:** SMD < 0.1 considered acceptable.

