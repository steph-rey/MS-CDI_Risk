library(ricu)
miiv
src <- "mimic_demo"
src

dict <- load_dictionary(src)
head(dict)


table(vapply(dict, `[[`, character(1L), "category"))


mimic_demo$admissions


import_src(src)
attach_src(x = src, assign_env = .GlobalEnv)


attach_src(x = "miiv", assign_env = .GlobalEnv)

subset(miiv)

str(miiv)

load_src(mimic_demo$admissions, subject_id > 44000, 
         cols = c("hadm_id", "admittime", "dischtime"))

explain_dictionary(dict)
