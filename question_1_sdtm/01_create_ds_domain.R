log_file <- file("01_create_ds_domain_Log.txt", open = "wt")
sink(log_file, type = "output")
sink(log_file, type = "message")

################# Q1: SDTM DS Domain Creation using {sdtm.oak} #################
#Goal: Create an SDTM Disposition (DS) domain dataset from raw clinical trial data using the {sdtm.oak} package
#Author: Maria Isabel Moscardo Garcia
#Date: 10/03/2026
#Description: This script constructs a DS domain containing the variables: STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
################################################################################

library(dplyr)
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)

#1. Load datasets
ct_study <- read.csv('/cloud/project/data/sdtm_ct.csv')
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

#2. Create oak id variables that maps the raw records to their mapped SDTM counterparts throught the pipeline
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

#3. Map DS variables
ds <-
  # DSTERM (topic variable): if OTHERSP is not null use OTHERSP, else IT.DSTERM
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM"
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  
  # DSDECOD: if OTHERSP is not null use OTHERSP, else IT.DSDECOD
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  
  # DSCAT: OTHER EVENT if OTHERSP not null,
  #        PROTOCOL MILESTONE if IT.DSDECOD == "Randomized",
  #        DISPOSITION EVENT otherwise
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP) & IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP) & IT.DSDECOD != "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    id_vars = oak_id_vars()
  ) %>%
  
  # DSDTC: combine DSDTCOL (dd-mm-yyyy) and DSTMCOL (HH:MM) into ISO 8601
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("mm-dd-yyyy", "HH:MM"),
    raw_unk = c("UN", "UNK"),
    id_vars = oak_id_vars()
  ) %>%
  
  # DSSTDTC: date only from IT.DSSTDAT into ISO 8601
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "mm-dd-yyyy",
    raw_unk = c("UN", "UNK"),
    id_vars = oak_id_vars()
  ) %>%
  
  # VISIT and VISITNUM: mapped via controlled terminology from INSTANCE
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = ct_study,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = ct_study,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  
  # STUDYID: mapped directly from raw STUDY column
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_var = "STUDYID",
    id_vars = oak_id_vars()
  ) 

#4. Create SDTM derived variables 
ds <- ds %>%
  dplyr::mutate(
    DOMAIN = "DS",
    USUBJID = paste0("01", "-", ds_raw$PATNUM)
  ) %>%
  arrange(USUBJID, DSSTDTC) %>%
  # derive sequence number
  derive_seq(tgt_var = "DSSEQ",
             rec_vars= c("USUBJID", "DSSTDTC")) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  )%>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  )


print("Snapshot of Derived Variables for first 10 subjects:")
print(ds)

print("SUCCESS: The program ran without errors and the DS domain has been generated.")

################################################################################
sink(type = "message")
sink(type = "output")