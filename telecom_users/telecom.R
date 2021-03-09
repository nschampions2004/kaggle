library(tidyverse)
library(tidymodels)
library(DataExplorer)
library(skimr)
library(doParallel)

theme_set(theme_minimal())

data_folder <- 'data'
plots_folder <- 'plots'
models_folder <- 'models'
predictions_folder <- 'predictions'


telecom_data <- read_csv(file.path(data_folder, 'telecom_users.csv')) %>%
  select(-X1) %>%
  mutate(Churn = as.factor(Churn))


# skim(telecom_data)
# ── Data Summary ────────────────────────
# Values
# Name                       telecom_data
# Number of rows             5986
# Number of columns          21
# _______________________
# Column type frequency:
#   character                16
# numeric                  5
# ________________________
# Group variables            None
#
# ── Variable type: character ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable    n_missing complete_rate   min   max empty n_unique whitespace
# 1 customerID               0             1    10    10     0     5986          0
# 2 gender                   0             1     4     6     0        2          0
# 3 Partner                  0             1     2     3     0        2          0
# 4 Dependents               0             1     2     3     0        2          0
# 5 PhoneService             0             1     2     3     0        2          0
# 6 MultipleLines            0             1     2    16     0        3          0
# 7 InternetService          0             1     2    11     0        3          0
# 8 OnlineSecurity           0             1     2    19     0        3          0
# 9 OnlineBackup             0             1     2    19     0        3          0
# 10 DeviceProtection         0             1     2    19     0        3          0
# 11 TechSupport              0             1     2    19     0        3          0
# 12 StreamingTV              0             1     2    19     0        3          0
# 13 StreamingMovies          0             1     2    19     0        3          0
# 14 Contract                 0             1     8    14     0        3          0
# 15 PaperlessBilling         0             1     2     3     0        2          0
# 16 PaymentMethod            0             1    12    25     0        4          0
#
# ── Variable type: numeric ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable  n_missing complete_rate     mean       sd    p0   p25    p50    p75  p100 hist
# 1 SeniorCitizen          0         1        0.161    0.368   0     0      0      0      1  ▇▁▁▁▂
# 2 tenure                 0         1       32.5     24.5     0     9     29     56     72  ▇▃▃▃▆
# 3 MonthlyCharges         0         1       64.8     30.1    18.2  35.6   70.4   89.9  119. ▇▅▆▇▅
# 4 TotalCharges          10         0.998 2298.    2274.     18.8 404.  1412.  3847.  8685. ▇▂▂▂▁
# 5 Churn                  0         1        0.265    0.441   0     0      0      1      1  ▇▁▁▁▃


# EDA
# DataExplorer::create_report(telecom_data)


# I want to set up my prediction before trying different models and techniques
set.seed(69420)
telecom_split <- initial_split(data = telecom_data, prop = 0.8)

telecom_train <- training(telecom_split)
telecom_test <- testing(telecom_split)

# recipe set up
telecom_recipe <- recipe(formula = Churn ~ ., data =telecom_train) %>%
  update_role(customerID, new_role = "ID") %>%
  update_role(Churn, new_role = "outcome") %>%
  step_dummy(c(all_predictors(), -has_role("ID"), -all_numeric()), one_hot = T) %>%
  step_meanimpute(TotalCharges) %>%
  themis::step_upsample(Churn) %>%
  prep()

final_train <- bake(telecom_recipe, new_data = NULL)
final_test <- bake(telecom_recipe, telecom_test)

final_train %>% pull(Churn) %>% table

# correlation plot
DataExplorer::plot_correlation(final_train %>% bind_rows(final_test))

# dfine a simple logistic regression
telecom_lr <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

# this will have some features I want to tune
# for time, I'll accept some defaults
telecom_gbm <- boost_tree(
  mode= "classification",
  mtry = tune(),
  min_n = tune(),
  tree_depth = 4,
  learn_rate = 0.3,
  trees = 1000,
  loss_reduction = 0,
  sample_size = 1,
  stop_iter = 50
) %>%
  set_engine("xgboost")


telecom_lr_wf <- workflow() %>%
  add_formula(Churn ~ . - customerID) %>%
  add_model(telecom_lr)

telecom_gbm_wf <- workflow() %>%
  add_formula(Churn ~ . - customerID) %>%
  add_model(telecom_gbm)

set.seed(69420)
telecom_folds <- vfold_cv(data = final_train)

registerDoParallel(8)
lr_grid <- tune_grid(
    object= telecom_lr_wf,
    resamples = telecom_folds,
    control = control_grid(save_pred = T, verbose = T),
    metrics = metric_set(mn_log_loss, accuracy, precision, recall)
)

gbm_grid <- tune_grid(
  object= telecom_gbm_wf,
  resamples = telecom_folds,
  control = control_grid(save_pred = T, verbose = T),
  metrics = metric_set(mn_log_loss, accuracy, precision, recall)
)


# find the best model for log loss
lr_spec <- lr_grid %>%
  select_best(metric = "mn_log_loss")

gbm_spec <- gbm_grid %>%
  select_best(metric = "mn_log_loss")

# retrain a gbm with the best specs on the full training data
best_lr <- finalize_model(x = telecom_lr, lr_spec) %>%
  fit(data = final_train, formula = Churn ~ . - customerID)

best_gbm <- finalize_model(x = telecom_gbm, gbm_spec) %>%
  fit(data = final_train, formula = Churn ~ . - customerID)

# pull out the best model and it's average performance on the
# holdout sets
lr_grid %>%
  collect_metrics() %>%
  inner_join(lr_spec)

# so we have a top performing logistic regression model with:
# accuracy    binary     0.798 (we want to be better than this) ✓
# mn_log_loss binary     0.424 (we want this to go down)
# precision   binary     0.843 (we want to do better than this)
# recall      binary     0.888 (we want to be better than this) ✓

# Can we beat this base model?
# We can switch out the model for something maybe a bit more powerful?
# Maybe it would be best if we could look at our logistic regresison's
# features...

# so, how'd we do?
# accuracy    binary     0.805 (the increase in recall beat out our loss in precision)
# mn_log_loss binary     0.448 (wished this would have gone down)
# precision   binary     0.841 (we are getting slightly more FPs)
# recall      binary     0.906 (we are picking up more Churners)
gbm_grid %>%
  collect_metrics() %>%
  inner_join(gbm_spec)

# accuracy    binary     0.877 (this got fixed once I pushed learning rate up)
# mn_log_loss binary     0.327 (this got fixed once I pushed learning rate up)
# precision   binary     0.938 (this got fixed once I pushed learning rate up)
# recall      binary     0.808 (this got fixed once I pushed learning rate up)

# plotting roc curve
best_gbm %>%
  predict(final_test, type = "prob") %>%
  bind_cols(final_test) %>%
  roc_curve(truth = Churn, .pred_No) %>%
  autoplot() +
  geom_label(aes(label = ))


## Feature Importance for our Logistic Regression Model
best_lr %>%
  tidy() %>%
  filter(term != '(Intercept)') %>%
  mutate(term = fct_reorder(term, abs(estimate)),
         sign = ifelse(estimate < 0, "negative", "positive")) %>%
  ggplot(aes(x = term, estimate, color = sign, fill = sign)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Feature Importance of Initial LR Model",
         subtitle = "Negative = less likely to churn, Positive = more likely to Churn") +
    theme(plot.title.position = "plot",
          legend.position = "none")

# predictions need to be written out because I'm paranoid
best_lr %>%
  predict(final_test) %>%
  bind_cols(final_test) %>%
  write_csv(file.path(predictions_folder, 'lr_preds.csv'))

