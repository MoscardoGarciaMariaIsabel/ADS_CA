log_file <- file("create_adsl_Log.txt", open = "wt")
sink(log_file, type = "output")
sink(log_file, type = "message")

################# Q2: ADaM ADSL Dataset Creation using {admiral} #################
#Goal: Create the ADSL using the input SDTM data, the {admiral} family of packages,and tidyverse tools
#Author: Maria Isabel Moscardo Garcia
#Date: 10/03/2026
#Description: This script creates the ADSL dataset with specific variables of interest (AGEGR9 & AGEGR9N, TRTSDTM & TRTSTMF, ITTFL, LSTAVLDT)
################################################################################

library(dplyr)
library(admiral)
library(pharmaverseraw)
library(pharmaversesdtm)

#1. Load datasets
vs <- pharmaversesdtm::vs
dm <- pharmaversesdtm::dm
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

vs <-convert_blanks_to_na(vs)
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)

#1. Use the DM domain as the basis of the ADSL
adsl <- dm %>%
  select(-DOMAIN)

#2. Derive the AGEGR9 and AGEGR9N
agegr9_lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr9_lookup
)

#3. Derive Start Date of treatment variables
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",     # Impute time only (hours/mins/secs)
    time_imputation = "00:00:00" # Impute missing time with 00:00:00
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "00:00:00"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    # Filter for valid doses and complete date parts
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & 
      nchar(EXSTDTC) >= 10 & !is.na(EXSTDTM),
    # Map to target ADSL variables with the specific logic for 'S' flag
    new_vars = exprs(
      TRTSDTM = EXSTDTM, 
      TRTSTMF = if_else(EXSTTMF == "S", NA_character_, EXSTTMF)
    ),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Last Exposure (TRTEDTM) - Required for LSTAVLDT item (4)
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & 
      nchar(EXENDTC) >= 10 & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

#4. Set ITTFL variable based on missing values in DM.ARM "Y" or not "N"
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(
      ITTFL = if_else(ARM != "Screen Failure", "Y", "N")
    )
  )

#5. Set LSTAVLDT variable to the max of Vitals complete, AE onset complete, dispoisiton complete, treatment complete
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      
      # (1) Last Vital Signs with valid result
      event(
        dataset_name = "vs",
        condition = nchar(VSDTC) >= 10 & (!is.na(VSSTRESN) | !is.na(VSSTRESC)),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(VSDTC))
      ),
      
      # (2) Last AE onset
      event(
        dataset_name = "ae",
        condition = !is.na(AESTDTC),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(AESTDTC))
      ),
      
      # (3) Last Disposition
      event(
        dataset_name = "ds",
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(DSSTDTC))
      ),
      
      # (4) Last Treatment
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTSDTM),
        set_values_to = exprs(LSTAVLDT = date(TRTSDTM))
      )
    ),
    source_datasets = list(
      vs = vs, 
      ae = ae, 
      ds = ds, 
      adsl = adsl
    ),
    order = exprs(LSTAVLDT),
    mode = "last",       # <-- takes the maximum date across all events
    new_vars = exprs(LSTAVLDT)
  )

print("Snapshot of Derived Variables for first 10 subjects:")
print(adsl)

print("SUCCESS: The program ran without errors and the ADSL dataset has been generated.")

################################################################################
sink(type = "message")
sink(type = "output")