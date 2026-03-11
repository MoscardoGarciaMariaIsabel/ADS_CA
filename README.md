# Roche ADS Programmer – Coding Assessment

**Candidate:** Maria Isabel Moscardo Garcia
**Position:** ADS Programmer  
**Date Submitted:** 11.03.2026

## Overview

This repository contains the complete submission for the Roche ADS Programmer coding assessment. The assessment covers the end-to-end clinical data programming workflow — from raw SDTM domain creation through ADaM dataset derivation, clinical table/figure production, and a bonus Generative AI component — using open-source tools.

All scripts are self-contained, execute error-free, and follow CDISC SDTM/ADaM conventions. Each question folder includes the source script, a `.txt` console log confirming clean execution, and the deliverable output(s).

---

## Repository Structure

```
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R
│   ├── 01_create_ds_domain_Log.txt
│   └── ds_domain.csv
│
├── question_2_adam/
│   ├── create_adsl.R
│   ├── create_adsl_Log.txt
│   └── adsl_q2.csv
│
├── question_3_tlg/
│   ├── 01_create_ae_summary_table.R
│   ├── 01_create_ae_summary_table_Log.txt
│   ├── ae_summary_table.html
│   ├── 02_create_visualizations.R
│   ├── 02_create_visualizations_Log.txt
│   ├── AE_Barplot.pdf
│   └── Top10_AE.pdf
│
├── question_4_agent/
│   ├── GenAI_clinical_data_assistant .ipynb
│   ├── Output_Q4.txt
│
└── README.md
```
