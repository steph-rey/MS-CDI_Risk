# Logistic Regression with performance package

# Install and load required packages ---- 
# install.packages("performance", dependencies = TRUE)
library(performance)
library(tidyverse)
library(dlookr)

# Import `df_CDI_risk_model.csv` and assign to `df` ----
df <- read_csv("/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/df_CDI_risk_model.csv")

# Create dataset with only relevant parameters (readmit, age, cdif, ppi, gas, abx) ---- 
df <- df %>% 
  select_if(is.numeric) %>% 
  select(-DBM)

# EDA with dlookr ---- 
dlookr::diagnose_paged_report(df)

# Build logistic regression models ----
  # m1 includes all parameters
  m1 <- glm(data = df, formula = HACDIF ~ readmit + age_gte_65 + PPI + GAS + ABX, family = binomial)
  summary(m1)
  model_performance(m1)
  
  # m2 includes all parameters except for GAS and READMIT
  m2 <- glm(data = df, formula = HACDIF ~ age_gte_65 + PPI + ABX, family = binomial)
  summary(m2)

# Compare performance between m1 and m2 ----
compare_performance(m1, m2, rank = T)
  
# Compare individual quality indicators: R2, ICC, AIC, BIC ----
  # R2
  r2(m1)
  r2(m2)
  
  # Intraclass correlation coefficient (ICC) - only used with random effects models
  # icc(m1)
  # icc(m2)
  
  # AIC
  AIC(m1)
  AIC(m2)
  
  # BIC
  BIC(m1)
  BIC(m2)

# Check model assumptions ----
  # m1
  check_model(m1)
  
  # m2
  check_model(m2)

# Check individual model assumptions ---- 
  # Check normality - not necessary for our log reg model, since response variable is binary
  # check_normality(m1)
  # check_normality(m2)
  
  # Check collinearity by variance inflation factor (VF)
  check_collinearity(m1)
  plot(check_collinearity(m1))
  
  # Check heteroscedasticity - not necessary for our model, since ... ??
  # check_heteroscedasticity(m1)
  # plot(check_heteroscedasticity(m1))
  
  # Check outliers
  check_outliers(m1)  # can add method='iqr'
  plot(check_outliers(m1))
  
  # Check singularity
    # If TRUE and there is singularity, best way to deal with it is simplify the model or reduce the number of parameters.
  check_singularity(m1)
  check_singularity(m2)
  
# End of Document 

  