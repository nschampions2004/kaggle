library(doMC)
library(doFuture)

library(tidyverse)
library(tidymodels)
library(skimr)
library(DataExplorer)

theme_set(theme_minimal())

data_folder <- "data"

stroke_pred <- read_csv("data/healthcare-dataset-stroke-data.csv") %>%
  mutate(bmi = as.numeric(ifelse(bmi == "N/A", NA, bmi)),
         stroke = as.factor(stroke))


stroke_holdout <- stroke_pred %>%
  head(100)

stroke_pred <- stroke_pred[c(100:nrow(stroke_pred)), ]


stroke_pred %>% View()
# 1) id: unique identifier
# 2) gender: "Male", "Female" or "Other"
# 3) age: age of the patient
# 4) hypertension: 0 if the patient doesn't have hypertension, 1 if the patient has hypertension
# 5) heart_disease: 0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease
# 6) ever_married: "No" or "Yes"
# 7) work_type: "children", "Govt_jov", "Never_worked", "Private" or "Self-employed"
# 8) Residence_type: "Rural" or "Urban"
# 9) avg_glucose_level: average glucose level in blood
# 10) bmi: body mass index
# 11) smoking_status: "formerly smoked", "never smoked", "smokes" or "Unknown"*
# 12) stroke: 1 if the patient had a stroke or 0 if not
# *Note: "Unknown" in smoking_status means that the information is unavailable for this patient

DataExplorer::create_report(stroke_pred)

skimr::skim(stroke_pred)
# ── Data Summary ────────────────────────
# Values
# Name                       stroke_pred
# Number of rows             5110
# Number of columns          12
# _______________________
# Column type frequency:
#   character                5
# numeric                  7
# ________________________
# Group variables            None
#
# ── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable  n_missing complete_rate   min   max empty n_unique whitespace
# 1 gender                 0             1     4     6     0        3          0
# 2 ever_married           0             1     2     3     0        2          0
# 3 work_type              0             1     7    13     0        5          0
# 4 Residence_type         0             1     5     5     0        2          0
# 5 smoking_status         0             1     6    15     0        4          0
#
# ── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable     n_missing complete_rate       mean        sd    p0     p25     p50     p75    p100 hist
# 1 id                        0         1     36518.     21162.    67    17741.  36932   54682   72940   ▇▇▇▇▇
# 2 age                       0         1        43.2       22.6    0.08    25      45      61      82   ▅▆▇▇▆
# 3 hypertension              0         1         0.0975     0.297  0        0       0       0       1   ▇▁▁▁▁
# 4 heart_disease             0         1         0.0540     0.226  0        0       0       0       1   ▇▁▁▁▁
# 5 avg_glucose_level         0         1       106.        45.3   55.1     77.2    91.9   114.    272.  ▇▃▁▁▁
# 6 bmi                     201         0.961    28.9        7.85  10.3     23.5    28.1    33.1    97.6 ▇▇▁▁▁
# 7 stroke                    0         1         0.0487     0.215  0        0       0       0       1   ▇▁▁▁▁

# so, I want to predict whether or not someone who has a stroke
sum(as.numeric(as.character(stroke_pred$stroke))) / nrow(stroke_pred)

# quick prevalence check
#sum(as.numeric(as.character(stroke_train$stroke))) / nrow(stroke_train) #0.04940083
# sum(stroke_train$stroke) / nrow(stroke_train)

# preprocessing
## create



stroke_recipe <- recipe(stroke ~ ., stroke_pred) %>%
  update_role(id, new_role = "ID") %>%
  step_knnimpute(bmi)  %>%
  step_dummy(all_nominal(), -all_outcomes())

stroke_prep <- prep(stroke_recipe)

juiced <- juice(stroke_prep)

# ?? What about the testing data
# modeling

stroke_spec <- parsnip::logistic_reg(
  mode = "classification",
  penalty = tune(),
  mixture = tune()) %>%
  set_engine("glmnet")


stroke_wf <- workflow() %>%
  add_formula(stroke ~ .) %>%
  add_model(stroke_spec)

set.seed(8675309)
stroke_fold <- vfold_cv(juiced, v = 10)

registerDoFuture()
plan(multicore)
stroke_tune <-  tune_grid(stroke_wf,
  resamples = stroke_fold,
  control = control_resamples(save_pred = T, verbose = T),
  grid = 50,
  metrics = metric_set(mn_log_loss))


stroke_tune %>%
  collect_metrics() %>%
  ggplot(aes(x = mixture, y = mean, color = penalty, fill = penalty)) +
    geom_point() +
    facet_wrap(~ .metric, scales = "free_x")

# get the best model
best_model <- stroke_tune %>%
  select_best()

# mean log loss winner
stroke_tune %>%
  collect_metrics() %>%
  inner_join(best_model)

final_model <- finalize_model(stroke_spec, best_model)

final_wf <- workflow() %>%
  add_formula(stroke ~ .) %>%
  add_model(final_model)

final_wf %>%
  fit(juiced) %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = as.factor(term),
         abs_coeff = abs(estimate),
         term = fct_reorder(.f = term, .x = estimate, .fun = abs)) %>%
  ggplot(aes(x = term, estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "coefficient",
       y = "feature",
       title = "Feature Importance: What features influence the decision on whether someone has a stroke?",
       subtitle = "Read: don't get hypertension or heart disease") +
  theme(plot.title.position = "plot")



# feature importance plot
stroke_final_fit <- final_wf %>%
  fit(juiced)


stroke_final_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = as.factor(term),
         abs_coeff = abs(estimate),
         term = fct_reorder(.f = term, .x = estimate, .fun = abs)) %>%
  ggplot(aes(x = term, estimate)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "coefficient",
         y = "feature",
         title = "Feature Importance: What features influence the decision on whether someone has a stroke?",
         subtitle = "Read: don't get hypertension or heart disease") +
    theme(plot.title.position = "plot")

# holdout set preprocessing
# stroke_holdout <- stroke_pred %>%
#   head(100)

st_hold_prep <- prep(stroke_recipe, data = stroke_holdout)
st_hold_j <- juice(st_hold_prep)


stroke_final_fit %>%
  predict(st_hold_j, "prob")



