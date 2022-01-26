# Logistic Regression with tidymodels and vip packages

# Load required packages ---- 
library(tidyverse)
library(tidymodels)
library(vip) # to visualize feature importance 

# Import `df_CDI_risk_model.csv` and assign to `df` ----
df <- read_csv("/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/df_CDI_risk_model.csv")

# Create dataset with only relevant parameters (readmit, age, cdif, ppi, gas, abx) and convert to factor type ---- 
df <- df %>% 
  select_if(is.numeric) %>% 
  select(-DBM) %>%  
  mutate_if(is.numeric, factor)

# Summarize counts for dataset
summary(df)

# Fit the model ----
model_fit_glm <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(HACDIF ~ . , data = training(splits))

model_fit_glm

# Check model assumptions ----
check_model(model_fit_glm)

# Check model performance ---- 
model_performance(model_fit_glm)

# Feature importance ----
  # Visualize most important features 
  model_fit_glm %>% 
    vip(num_features = 5,
        geom = "point",
        aes = list(size = 3, color = '18bc9c')) + 
    theme_minimal(base_size = 15) + 
    labs(title = "Logistic Regression: Feature Importance")
    # Features with highest importance are: PPI, AGE, and ABX

  # Visualize top features
  df %>% ggplot(aes(HACDIF, age_gte_65, color = ABX)) +
    geom_jitter(alpha = 0.3) + 
    theme_minimal(base_size = 16) +
    scale_color_viridis_d(end = 0.4) + 
    labs(title = "Comparison of Predicted CDIF Diagnosis\nusing AGE and ABX as Key Predictors")

  # Visualize top features
  df %>% ggplot(aes(HACDIF, PPI, color = ABX)) +
    geom_jitter(alpha = 0.3) + 
    theme_minimal(base_size = 16) +
    scale_color_viridis_d(end = 0.4) + 
    labs(title = "Comparison of Predicted CDIF Diagnosis\nusing PPI and ABX as Key Predictors")
  
# Create train and test splits ----
  # Set seed to ensure reproducibility 
  set.seed(123)
  
  # Create initial splits 
  splits <- df %>% initial_split(prop = 0.8)
  
  # View splits - can also view the training and testing splits individually
  splits
  training(splits)
  testing(splits)

# Predict class and numeric probabilities on testing data ---- 
  # Predict class (whether or not pt predicted to have cdiff)
  prediction_class_test <- predict(model_fit_glm, new_data = testing(splits), type = 'class')
  prediction_class_test
  
  # Predict numeric probabilities for each class (Y/N cdiff)
  prediction_prob_test <- predict(model_fit_glm, new_data = testing(splits), type = 'prob')
  prediction_prob_test
  
  # Create table that binds predicted class and probs
  # Will be used to evaluate model in next step 
  results_tbl <- bind_cols(prediction_class_test, prediction_prob_test, testing(splits))
  results_tbl
  
# Evaluate model performance: AUC and ROC ----
  # Plot AUC 
  results_tbl %>% roc_auc(HACDIF, .pred_0)
  # AUC = 0.619
  
  # Visualize ROC 
  results_tbl %>% roc_curve(HACDIF, .pred_0) %>% 
    autoplot()

# End of Document
