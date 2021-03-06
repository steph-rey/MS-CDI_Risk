# Logistic Regression with tidymodels and vip packages
# Train CDI risk model on training split; validated with testing split 

# Load required packages ----
library(tidyverse)
library(tidymodels)
library(vip) # to visualize feature importance

# Import `df_CDI_risk_model.csv` and assign to `df` ----
df <- read_csv("/Users/sreynolds2/Documents/GitHub/MS-CDI_Risk/MS-CDI_Risk/data/clean/df_CDI_risk_model.csv")

# Create dataset with only relevant parameters and convert to factor type ---- 
df <- df %>% 
  select(-c(ID, ED_dispo)) %>% 
  mutate_if(is.numeric, factor)

# Create train and test splits ----
  # Set seed to ensure reproducibility 
  set.seed(123)
  
  # Create initial splits 
  splits <- df %>% initial_split(prop = 0.8)
  
  # View splits - can also view the training and testing splits individually
  splits
  training(splits)
  testing(splits)

# Fit the model ----
model_fit_glm <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(HACDIF ~ readmit + age_gte_65 + PPI + GAS + anyGAS + LAX + ABX, data = training(splits))

  # View summary of model
  model_fit_glm

  # Calculate R2 
  r2(model_fit_glm)

# Check model assumptions ----
check_model(model_fit_glm)
  
  check_collinearity(model_fit_glm)
  # No collinearity issues detected

# Check model performance ---- 
model_performance(model_fit_glm)

# Visualize feature importance ----
  # Visualize most important features 
  model_fit_glm %>% 
    vip(num_features = 5,
        geom = "point",
        aes = list(size = 3, color = '18bc9c')) + 
    theme_minimal(base_size = 15) + 
    labs(title = "Logistic Regression: Feature Importance")
    # Features with highest importance are: ABX, GAS, and PPI

  # Visualize top features
  df %>% ggplot(aes(HACDIF, ABX, color = GAS)) +
    geom_jitter(alpha = 0.2) + 
    theme_minimal(base_size = 16) +
    scale_color_viridis_d(end = 0.4) + 
    labs(title = "Comparison of Predicted CDIF Diagnosis\nusing ABX and GAS as Key Predictors")

  # Visualize top features
  df %>% ggplot(aes(HACDIF, ABX, color = PPI)) +
    geom_jitter(alpha = 0.3) + 
    theme_minimal(base_size = 16) +
    scale_color_viridis_d(end = 0.4) + 
    labs(title = "Comparison of Predicted CDIF Diagnosis\nusing ABX and PPI as Key Predictors")

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
  results_auc <- results_tbl %>% roc_auc(HACDIF, .pred_0) %>% summarize(.estimate)
  # AUC = 0.837
  
  # Visualize ROC 
  results_tbl %>% roc_curve(HACDIF, .pred_0) %>% 
    autoplot()
  
