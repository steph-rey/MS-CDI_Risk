#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-01-05
# @DateModified: 2021-01-05 at 12:50PM PT 
# @Description - This file reads in necessary data and identifies variables of interest for building a 
# multivariate logistic regression model to evaluate C diff risk among hospitalized patients. 
#-------------------------------------------------------------------------------

# Load required packages ----
#library(here)
library(readr)
library(tidyverse)
library(psych)
library(lubridate)
library(tableone)
library(kableExtra)



# Read in data (only first n=5000 rows) ----
dm <- read_delim("/Users/sreynolds2/Downloads/ucsf_data_pull_11.08.21/demographics and event table 11.08.21.csv", delim = "|", n_max = 5000)
lab <- read_delim("/Users/sreynolds2/Downloads/ucsf_data_pull_11.08.21/lab results 11.08.21.csv", delim = "|", n_max = 5000)
dx <- read_delim("/Users/sreynolds2/Downloads/ucsf_data_pull_11.08.21/covid diagnoses 11.08.21.csv", delim = "|", n_max = 5000)
med_admin <- read_delim("/Users/sreynolds2/Downloads/ucsf_data_pull_11.08.21/med admin table 11.08.21.csv", delim = "|", n_max = 5000)



# Clean `dm` data ----
dm <- dm %>% 
  select(deid_enc_id, age, sex, pat_race, ethnicity, smoking, BMI, covid_pos, readmit, HAI_pathogen, HAI_type, HAI_first_date, ED_dispo, present_source, admit_time) %>% 
  rename(ID = deid_enc_id, 
         race = pat_race) %>% 
  mutate_at(c("sex", "race", "ethnicity", "smoking", "covid_pos", "readmit", "ED_dispo", "present_source", "HAI_type"), factor) %>% 
  mutate_at(c("admit_time", "HAI_first_date"), ymd_hms) %>% 
  mutate_at("BMI", as.numeric)

# Create frequency table for `dm` ----
CreateCatTable(data = dm, vars = c("sex", "race", "ethnicity", "smoking", "covid_pos", "readmit", "ED_dispo", "present_source", "HAI_type"))



# Clean `lab` data ---- 
lab <- lab %>%
  select(deid_enc_id, COMMON_NAME, ORD_VALUE, Abnormal, order_time, RESULT_TIME, lab_concept) %>% 
  mutate_at(c("COMMON_NAME", "Abnormal", "lab_concept"), factor) %>% 
  mutate_at("ORD_VALUE", as.numeric)

# Create frequency table for `lab` ---- 
CreateCatTable(data = lab, vars = c("COMMON_NAME", "Abnormal", "lab_concept"))



# Clean `dx` data ---- 
dx <- dx %>% 
  select(deid_enc_id, dx_group, DX_NAME, present_on_admit, severity_of_dx, co_morbidity) %>% 
  mutate_at(c("dx_group", "DX_NAME", "present_on_admit", "severity_of_dx", "co_morbidity"), factor)

# Create frequency table for `dx` ---- 
CreateCatTable(data = dx, vars = c("dx_group", "DX_NAME", "present_on_admit", "severity_of_dx", "co_morbidity"))



# Clean `med_admin` table ----
med_admin <- med_admin %>% 
  select(deid_enc_id, name, thera_class, pharm_class, pharm_subclass, taken_time) %>% 
  rename(ID = deid_enc_id, 
         med_name = name) %>%
  mutate_at(c("med_name", "thera_class", "pharm_class", "pharm_subclass"), factor)

# Create frequency table for `med_admin` ---- 
CreateCatTable(data = med_admin, vars = c("med_name", "thera_class", "pharm_class", "pharm_subclass"))



# Save all data ---- 
write_csv(dm, file = "/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/dm.csv")
write_csv(dx, file = "/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/dx.csv")
write_csv(lab, file = "/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/lab.csv")
write_csv(med_admin, file = "/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/med_admin.csv")


######################################################################################################

# Identify variables of interest in datasets to include in logistic reg model ----
    # COMMON PARAMETERS / RISK FACTORS FOR C DIFF RISK ASSESSMENT MODEL 

# Age >= 65 --> dm$age

# Prior hospital admission in 30 days --> dm$readmit == "Yes"

# Admission from ED --> dm$ED_dispo == "Admit" OR dm$present_source == ??

# Diabetes --> lab$lab_concept == "diabetes" OR dx$dx_group == "DIABETES MELLITUS"

# Receipt of antibiotics in last 30 days --> med_admin$thera_class %in% c("ANTIBIOTICS", "ANTIFUNGALS", "ANTIINFECTIVES/MISCELLANEOUS", "ANTIPARASITICS", "ANTIVIRALS")
    # NOTE: Doesn't include outpatient abx, only those during hospitalization

# Receipt of proton pump inhibitor (PPI) --> med_admin$pharm_class == "PROTON-PUMP INHIBITORS"

# Receipt of gastric acid suppressants, laxatives, etc. --> med_admin$pharm_class %in% c("ANTACIDS", "LAXATIVES AND CATHARTICS", "LAXATIVES, LOCAL/RECTAL")

# levels(med_admin$thera_class)

# med_admin %>% filter(thera_class == "GASTROINTESTINAL") %>% select(pharm_class) %>% unique()


