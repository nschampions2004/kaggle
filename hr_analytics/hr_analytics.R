library(tidyverse)
library(tidymodels)
library(doParallel)
library(DataExplorer)
library(skimr)
library(themis)
library(ggrepel)


# defaults
theme_set(theme_minimal())


# files / folders
data_folder <- 'data'
results <- 'results'
aug_train_file <- 'aug_train.csv'
aug_test_file <- 'aug_test.csv'


missing_cols <- c("city", "gender",
"relevent_experience", "enrolled_university",
"education_level", "major_discipline",
"experience", "company_size",
"company_type", "last_new_job")


aug_data <- read_csv(file.path(data_folder, aug_train_file))


aug_data <- read_csv(file.path(data_folder, aug_train_file)) %>%
  mutate(across(dplyr::matches(missing_cols), tidyr::replace_na, "Missing"),
         target = as.factor(target))

aug_test <- read_csv(file.path(data_folder, aug_test_file)) %>%
  mutate(across(dplyr::matches(missing_cols), tidyr::replace_na, "Missing"))

skimr::skim(aug_data)

# ── Data Summary ────────────────────────
# Values
# Name                       aug_data
# Number of rows             19158
# Number of columns          14
# _______________________
# Column type frequency:
# character                10
# numeric                  4
# ________________________
# Group variables            None
#
# ── Variable type: character ────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable       n_missing complete_rate   min   max empty n_unique whitespace
# 1 city                        0         1         6     8     0      123          0
# 2 gender                   4508         0.765     4     6     0        3          0
# 3 relevent_experience         0         1        22    23     0        2          0
# 4 enrolled_university       386         0.980    13    16     0        3          0
# 5 education_level           460         0.976     3    14     0        5          0
# 6 major_discipline         2813         0.853     4    15     0        6          0
# 7 experience                 65         0.997     1     3     0       22          0
# 8 company_size             5938         0.690     3     9     0        8          0
# 9 company_type             6140         0.680     3    19     0        6          0
# 10 last_new_job              423         0.978     1     5     0        6          0
#
# ── Variable type: numeric ──────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable          n_missing complete_rate      mean       sd    p0     p25       p50      p75      p100 hist
# 1 enrollee_id                    0             1 16875.    9616.    1     8554.   16982.    25170.   33380     ▇▇▇▇▇
# 2 city_development_index         0             1     0.829    0.123 0.448    0.74     0.903     0.92     0.949 ▁▂▁▁▇
# 3 training_hours                 0             1    65.4     60.1   1       23       47        88      336     ▇▃▁▁▁
# 4 target                         0             1     0.249    0.433 0        0        0         0        1     ▇▁▁▁▂


# Features
#
# enrollee_id : Unique ID for candidate
#
# city: City code
#
# city_ development _index : Developement index of the city (scaled)
#
# gender: Gender of candidate
#
# relevent_experience: Relevant experience of candidate
#
# enrolled_university: Type of University course enrolled if any
#
# education_level: Education level of candidate
#
# major_discipline :Education major discipline of candidate
#
# experience: Candidate total experience in years
#
# company_size: No of employees in current employer's company
#
# company_type : Type of current employer
#
# lastnewjob: Difference in years between previous job and current job
#
# training_hours: training hours completed
#
# target: 0 – Not looking for job change, 1 – Looking for a job change

# Use this to do some Exploratory Data Analysis...
# DataExplorer::create_report(aug_data)


DataExplorer::plot_correlation(data = aug_data)



# Conclusions:
# 1. There's alot of cardinality in the categorrical columns
# 2. There's alot of "missing-ness"



# set up the pipeline from train to test...
## we've got an Id col
## we can roll up the high cardinality cols into an "other" field
set.seed(69420)
hr_recipe <- recipes::recipe(target ~ ., aug_data) %>%
  # update_role(enrollee_id, new_role = "id") %>%
  step_other(city, threshold = 0.05) %>%
  step_other(experience, threshold = 0.1) %>%
  step_rm(enrollee_id) %>%
  step_dummy(city, one_hot = T) %>%
  step_dummy(gender, one_hot = T) %>%
  step_dummy(relevent_experience, one_hot = T) %>%
  step_dummy(enrolled_university, one_hot = T) %>%
  step_dummy(education_level, one_hot = T) %>%
  step_dummy(major_discipline, one_hot = T) %>%
  step_dummy(experience, one_hot = T) %>%
  step_dummy(company_size, one_hot = T) %>%
  step_dummy(company_type, one_hot = T) %>%
  step_dummy(last_new_job, one_hot = T) %>%
  step_mutate(target = as.factor(target),
              city_development_index = as.numeric(city_development_index)) %>%
  themis::step_smote(target, over_ratio = 0.5) %>%
  prep()


# 1 city
# 2 gender
# 3 relevent_experience
# 4 enrolled_university
# 5 education_level
# 6 major_discipline
# 7 experience
# 8 company_size
# 9 company_type
# 10 last_new_job


hr_train <- bake(hr_recipe, aug_data)

hr_test <- bake(hr_recipe, aug_test)


# workflow
hr_base_lm <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")


hr_rf <- rand_forest(
  mode = "classification",
  mtry = tune(),
  min_n = tune(),
  trees = 1000 # maybe don't let this vary much
) %>%
  set_engine("ranger")


# I don't know what to do here
hr_base_wf <- workflow() %>%
  add_formula(target ~ .) %>%
  add_model(hr_base_lm)

hr_rf_wf <- workflow() %>%
  add_formula(target ~ .) %>%
  add_model(hr_rf)

# vfolds
hr_folds <- rsample::vfold_cv(data = hr_train, v = 5)

# tune grid
# MODELS GO BRRRRRRRRRRRRRRR
doParallel::registerDoParallel(10)
hr_grid <- tune_grid(hr_base_wf,
  resamples = hr_folds,
  control = control_resamples(save_pred = T, verbose = T),
  grid = 50,
  metrics = metric_set(mn_log_loss, precision, recall, accuracy))

hr_rf_grid <- tune_grid(hr_rf_wf,
  resamples = hr_folds,
  control = control_resamples(save_pred = T, verbose = T),
  grid = 10,
  metrics = metric_set(mn_log_loss, precision, recall, accuracy))


hr_grid %>%
  collect_metrics() %>%
  ggplot(aes(x = penalty, y = mixture, color = mean, fill = mean)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = round(mean, 2))) +
    facet_wrap(~ .metric) +
    labs(title = "Final Model Summary")


hr_rf_grid %>%
  collect_metrics() %>%
  ggplot(aes(x = min_n, y = trees, color = mean, fill = mean)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = round(mean, 2))) +
  facet_wrap(~ .metric) +
  labs(title = "Final Model Summary")

best_lm <- hr_grid %>%
  select_best(metric = "mn_log_loss")

best_rf <- hr_rf_grid %>%
  select_best(metric = "mn_log_loss")

final_lm <- finalize_model(x = hr_base_lm, best_lm) %>%
  fit(target ~ ., data = hr_train)

final_rf <- finalize_model(x = hr_rf, best_rf) %>%
  fit(target ~ ., data = hr_train)

tidy(final_lm) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(as.factor(term), abs(estimate)),
         sign = ifelse(estimate < 0, "negative", "positive")) %>%
  ggplot(aes(x = term, y = estimate, color = sign, fill = sign)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = 'Feature Importance Plot (Would be better if we scaled numeric features)',
         subtitle = 'Positive bars indicate a push towards someone switching jobs\nNegative bars indicate less likely to switch job',
         x = NULL,
         y = "Coefficient") +
    theme(plot.title.position = "plot",
          legend.position = "none")


tidy(final_rf) # ? will this work? Nope!

lm_test <- final_lm %>%
  predict(hr_test)

rf_test <- final_rf %>%
  predict(hr_test)

lm_preds <- hr_test %>%
  bind_cols(lm_test) %>%
  rename(predictions = .pred_class)

rf_preds <- hr_test %>%
  bind_cols(rf_test) %>%
  rename(predictions = .pred_class)


# output
saveRDS(lm_preds,
    file.path(results, 'lm_preds.rds'))
write_csv(lm_preds, file.path(results, 'lm_preds.csv'))


saveRDS(rf_preds,
        file.path(results, 'rf_preds.rds'))
write_csv(rf_preds, file.path(results, 'rf_preds.csv'))
