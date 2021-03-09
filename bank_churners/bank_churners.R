library(tidyverse)
library(tidymodels)
library(doParallel)
library(DataExplorer)
library(skimr)
library(ggrepel)

theme_set(theme_minimal())

data_folder <- 'data'
plots_folder <- 'plots'
models_folder <- 'models'
results_folder <- 'results'

bank_data <- read_csv(file.path(data_folder, 'BankChurners.csv')) %>%
  select(-starts_with("Naive_Bayes"), -Avg_Open_To_Buy, -Total_Revolving_Bal)

skimr::skim(bank_data)
# ── Data Summary ────────────────────────
# Values
# Name                       bank_data
# Number of rows             10127
# Number of columns          21
# _______________________
# Column type frequency:
#   character                6
# numeric                  15
# ________________________
# Group variables            None
#
# ── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable   n_missing complete_rate   min   max empty n_unique whitespace
# 1 Attrition_Flag          0             1    17    17     0        2          0
# 2 Gender                  0             1     1     1     0        2          0
# 3 Education_Level         0             1     7    13     0        7          0
# 4 Marital_Status          0             1     6     8     0        4          0
# 5 Income_Category         0             1     7    14     0        6          0
# 6 Card_Category           0             1     4     8     0        4          0
#
# ── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable            n_missing complete_rate          mean           sd         p0           p25           p50           p75          p100 hist
# 1 CLIENTNUM                        0             1 739177606.    36903783.    708082083  713036770.    717926358     773143533     828343083     ▇▁▂▂▁
# 2 Customer_Age                     0             1        46.3          8.02         26         41            46            52            73     ▂▆▇▃▁
# 3 Dependent_count                  0             1         2.35         1.30          0          1             2             3             5     ▇▇▇▅▁
# 4 Months_on_book                   0             1        35.9          7.99         13         31            36            40            56     ▁▃▇▃▂
# 5 Total_Relationship_Count         0             1         3.81         1.55          1          3             4             5             6     ▇▇▆▆▆
# 6 Months_Inactive_12_mon           0             1         2.34         1.01          0          2             2             3             6     ▅▇▇▁▁
# 7 Contacts_Count_12_mon            0             1         2.46         1.11          0          2             2             3             6     ▅▇▇▃▁
# 8 Credit_Limit                     0             1      8632.        9089.         1438.      2555          4549         11068.        34516     ▇▂▁▁▁
# 9 Total_Revolving_Bal              0             1      1163.         815.            0        359          1276          1784          2517     ▇▅▇▇▅
# 10 Avg_Open_To_Buy                  0             1      7469.        9091.            3       1324.         3474          9859         34516     ▇▂▁▁▁
# 11 Total_Amt_Chng_Q4_Q1             0             1         0.760        0.219         0          0.631         0.736         0.859         3.40  ▅▇▁▁▁
# 12 Total_Trans_Amt                  0             1      4404.        3397.          510       2156.         3899          4741         18484     ▇▅▁▁▁
# 13 Total_Trans_Ct                   0             1        64.9         23.5          10         45            67            81           139     ▂▅▇▂▁
# 14 Total_Ct_Chng_Q4_Q1              0             1         0.712        0.238         0          0.582         0.702         0.818         3.71  ▇▆▁▁▁
# 15 Avg_Utilization_Ratio            0             1         0.275        0.276         0          0.023         0.176         0.503         0.999 ▇▂▂▂▁



# DataExplorer::create_report(bank_data)
DataExplorer::plot_correlation(bank_data)


bank_split <- rsample::initial_split(bank_data, prop = 0.8)


bank_training <- training(bank_split)
bank_testing <- testing(bank_split)


bank_recipe <- recipe(Credit_Limit ~ ., bank_training) %>%
  update_role(CLIENTNUM, new_role = "ID") %>%
  step_range(c(where(is.numeric), -Credit_Limit), min = 0, max = 1) %>% # min or max will be truncate to 0 or 1 if outside these bounds in testing data set
  step_dummy(c(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category), one_hot = T) %>%
  prep()

# finalize the training and testing data based on the recipe above
final_training <- bake(bank_recipe, new_data = bank_training)
final_testing <- bake(bank_recipe, new_data = bank_testing)


bank_lm <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

bank_rf <- rand_forest(
  min_n = tune(),
  mtry = tune(),
  trees = 1000,
  mode = "regression"
) %>%
  set_engine("ranger")


bank_xgb <- boost_tree(
  min_n = tune(),
  mtry = tune(),
  trees = 1000,
  tree_depth = tune(),
  learn_rate = 0.01,
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = 50,
  mode = "regression"
) %>%
  set_engine("xgboost") # , nthread = 11 THIS WORKS

bank_lm_wf <- workflow() %>%
  add_formula(formula = Credit_Limit ~ . - CLIENTNUM) %>%
  add_model(bank_lm)

bank_rf_wf <- workflow() %>%
  add_formula(formula = Credit_Limit ~ . - CLIENTNUM) %>%
  add_model(bank_rf)

bank_xgb_wf <- workflow() %>%
  add_formula(formula = Credit_Limit ~ . - CLIENTNUM) %>%
  add_model(bank_xgb)

# folds for Cross Validation
bank_folds <- vfold_cv(data = final_training, v = 10)
bank_folds_rf <- vfold_cv(data = final_training, v = 4)

lm_tune_grid <- tune_grid(object = bank_lm_wf,
  resamples = bank_folds,
  grid = 10,
  control = control_grid(save_pred = T, verbose = T))

doParallel::registerDoParallel(11)
rf_tune_grid <- tune_grid(object = bank_rf_wf,
  resamples = bank_folds_rf,
  grid = 8,
  control = control_grid(save_pred = T, verbose = T))

doParallel::registerDoParallel(11)
xgb_tune_grid <- tune_grid(object = bank_xgb_wf,
  resamples = bank_folds_rf,
  grid = 8,
  control = control_grid(save_pred = T, verbose = T))


lm_tune_grid %>%
  collect_metrics()

rf_tune_grid %>%
  collect_metrics()

best_lm_spec <- lm_tune_grid %>%
  select_best('rmse')

best_lm <- bank_lm %>%
  finalize_model(best_lm_spec) %>%
  fit(formula = Credit_Limit ~ . - CLIENTNUM,
      data = final_training)


best_rf_spec <- rf_tune_grid %>%
  select_best('rmse')

best_rf <- bank_rf %>%
  finalize_model(best_lm_spec) %>%
  fit(formula = Credit_Limit ~ . - CLIENTNUM,
      data = final_training)

best_xgb_spec <- xgb_tune_grid %>%
  select_best('rmse')

best_xgb <- bank_xgb %>%
  finalize_model(best_xgb_spec) %>%
  fit(formula = Credit_Limit ~ . - CLIENTNUM,
      data = final_training)


# so, linear model penalized down to two features

best_lm %>%
  tidy() %>%
  filter(term != '(Intercept)') %>%
  mutate(term = fct_reorder(as.factor(term), abs(estimate)),
         sign = ifelse(estimate >= 0, "positive", "negative"), nudge_y = 1000) %>%
  ggplot(aes(x = term, y = estimate, color = sign, fill = sign)) +
    #geom_bar(stat = 'identity') +
    geom_point() +
    geom_text_repel(aes(label = scales::comma(round(estimate, 0)))) +
    coord_flip() +
    labs(title = "Feature Importance Plot",
         y = "Credit Limit") +
    theme(plot.title.position = "plot",
          legend.position = "none")

# not _super_ penalized, but the penalization is forcing many terms to be 0
# see that mixture ~ 1.0 -> that means the best model penalized many params to be 0
  # and build the most out of Avg_Open_To_Buy and Total_Revolving_Bal
best_lm_spec %>%
  inner_join(lm_tune_grid %>% collect_metrics)

# rmse = 5666
# rsq = 0.611 -> Fixeds

best_rf_spec %>%
  inner_join(rf_tune_grid %>% collect_metrics)

# rmse = 3,897
# rsq = 0.818


best_xgb_spec %>%
  inner_join(xgb_tune_grid %>% collect_metrics)

# rmse = 385X
# rsq = 0.821




best_lm %>%
  predict(final_testing)

best_rf %>%
  predict(final_testing) %>%
  bind_cols(final_testing) %>%
  write_csv()


best_xgb %>%
  predict(final_testing) %>%
  bind_cols(final_testing) %>%
  write_csv




