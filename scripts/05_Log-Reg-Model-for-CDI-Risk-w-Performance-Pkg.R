# Logistic Regression with performance package

# Install and load required packages ---- 
# install.packages("performance", dependencies = TRUE)
library(performance)
library(tidyverse)
library(dlookr)
library(DescTools) # calculate Brier Score

# Import `df_CDI_risk_model.csv` and assign to `df` ----
df <- read_csv("/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/df_CDI_risk_model.csv")

# Create dataset with only relevant parameters (readmit, age, cdif, ppi, gas, abx) and convert to factor type ---- 
df <- df %>% 
  select(-c(ID, ED_dispo)) %>% 
  mutate_if(is.numeric, factor)

# Build model and summarize regression output ----
  # Full model includes all parameters except for `AMR` which is highly collineated with `ABX`
  m1 <- glm(data = df, formula = HACDIF ~ readmit + age_gte_65 + PPI + GAS + anyGAS + ABX, family = binomial)
  summary(m1)
  model_performance(m1)
  
  # m2 includes all parameters except for anyGAS (which is moderately correlated with PPI/GAS) 
  m2 <- glm(data = df, formula = HACDIF ~ readmit + age_gte_65 + PPI + GAS + LAX + ABX, family = binomial)
  summary(m2)
  model_performance(m2)

# Compare model performance ----
compare_performance(m1, m2, rank = T)

# Compare individual quality indicators: R2, ICC, AIC, BIC ----
  # R2
  r2(m1)
  r2(m2)
  
  # Intraclass correlation coefficient (ICC) - only used with random effects models
  # icc(m1)
  # icc(m2)
  
  # AIC
  cat("AIC:", round(AIC(m1),1))
  cat("AIC:", round(AIC(m2),1))
  
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
      # Moderate correlation between GAS and ABX (VIF>5)
  
  check_collinearity(m2)
      # No collinearity
  
  # Check heteroscedasticity - not necessary for our model, since ... ??
  # check_heteroscedasticity(m1)
  # plot(check_heteroscedasticity(m1))
  
  # Check outliers
  check_outliers(m1)  # can add method='iqr'
  # plot(check_outliers(m1))
  check_outliers(m2)
      # No outliers detected for m1 or m2
  
  # Check singularity
    # If TRUE and there is singularity, best way to deal with it is simplify the model or reduce the number of parameters.
  check_singularity(m1)
  check_singularity(m2)
      # No singularity for m1 or m2

  # Calculate and print Brier scores for m1-m4
  cat("Brier Score for M1:", round(BrierScore(m1), 4))
  cat("Brier Score for M2:", round(BrierScore(m2), 4))
