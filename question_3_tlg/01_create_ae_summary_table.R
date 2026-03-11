log_file <- file("01_create_ae_summary_table_Log.txt", open = "wt")
sink(log_file, type = "output")
sink(log_file, type = "message")

################# Q3: TLG - Adverse Events Reporting #################
#Goal: Create outputs for adverse events summary using the ADAE dataset and {gtsummary}. 
#Author: Maria Isabel Moscardo Garcia
#Date: 10/03/2026
#Description: This script creates tables and figures describing the adverse events data
################################################################################

library(dplyr)
library(pharmaverseadam)
library(gtsummary)
library(forcats)
library(ggplot2)

#1. Load datasets
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

#2. Generate summary table 
adae <- adae |>
  filter(
    # safety population
    SAFFL == "Y",
    # treatment-emergent adverse events 
    TRTEMFL == "Y") |>
  mutate(
    AETERM = forcats::fct_infreq(AETERM),  # sort terms by frequency
    AESOC  = forcats::fct_infreq(AESOC)    # sort SOCs by frequency
  )

tbl <- adae |>
  tbl_hierarchical(
    variables = c(AESOC,AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = list(
      # Mapping the variable names to the desired descriptive labels
      AESOC ~ "Primary System Organ Class",
      AETERM ~ "Reported Term for Adverse Event",
      "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
    )
  )
tbl

################################################################################
sink(type = "message")
sink(type = "output")