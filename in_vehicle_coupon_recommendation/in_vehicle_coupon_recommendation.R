library(tidyverse)
library(tidymodels)
library(ggrepel)
library(doParallel)
library(skimr)
library(DataExplorer)
library(patchwork)
library(ggridges)

# ggplot theme set
theme_set(theme_minimal())

# folders
data_folder <- 'data'
plots_folder <- 'plots'
models_folder <- 'models'
predictions_folder <- 'preds'

# missing_cols
missing_cols <- c("car", "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", "Restaurant20To50")

# data file
coupon_data <- read_csv(file.path(data_folder, 'in-vehicle-coupon-recommendation.csv'),
                        guess_max = 2000) %>%
  mutate(across(dplyr::matches(missing_cols), tidyr::replace_na, "Missing")) %>%
  janitor::clean_names("small_camel")





# coupon_data %>% View()
# destination: No Urgent Place, Home, Work
# passanger: Alone, Friend(s), Kid(s), Partner (who are the passengers in
  # the car)
# weather: Sunny, Rainy, Snowy
# temperature:55, 80, 30
# time: 2PM, 10AM, 6PM, 7AM, 10PM
# coupon: Restaurant(<$20), Coffee House, Carry out & Take away, Bar,
  # Restaurant($20-$50)
# expiration: 1d, 2h (the coupon expires in 1 day or in 2 hours)
# gender: Female, Male
# age: 21, 46, 26, 31, 41, 50plus, 36, below21
# maritalStatus: Unmarried partner, Single, Married partner, Divorced, Widowed
# hasChildren:1, 0
# education: Some college - no degree, Bachelors degree, Associates degree,
  # High School Graduate, Graduate degree (Masters or Doctorate),
  # Some High School
# occupation: Unemployed, Architecture & Engineering, Student,
  # Education&Training&Library, Healthcare Support,
  # Healthcare Practitioners & Technical, Sales & Related, Management,
  # Arts Design Entertainment Sports & Media, Computer & Mathematical,
  # Life Physical Social Science, Personal Care & Service,
  # Community & Social Services, Office & Administrative Support,
  # Construction & Extraction, Legal, Retired,
  # Installation Maintenance & Repair, Transportation & Material Moving,
  # Business & Financial, Protective Service,
  # Food Preparation & Serving Related, Production Occupations,
  # Building & Grounds Cleaning & Maintenance,
  # Farming Fishing & Forestry
# income: $37500 - $49999, $62500 - $74999,
  # $12500 - $24999, $75000 - $87499,
  # $50000 - $62499, $25000 - $37499,
  # $100000 or More, $87500 - $99999,
  # Less than $12500
# Bar: never, less1, 1~3, gt8, nan4~8
  # (feature meaning: how many times do you go to a bar every month?)
# CoffeeHouse: never, less1, 4~8, 1~3, gt8, nan
  # (feature meaning: how many times do you go to a coffeehouse every month?)
# CarryAway:n4~8, 1~3, gt8, less1, never
  # (feature meaning: how many times do you get take-away food every month?)
# RestaurantLessThan20: 4~8, 1~3, less1, gt8, never
  # (feature meaning: how many times do you go to a restaurant with an average
  # expense per person of less than $20 every month?)
# Restaurant20To50: 1~3, less1, never, gt8, 4~8, nan
  # (feature meaning: how many times do you go to a restaurant with average
  # expense per person of $20 - $50 every month?)
# toCouponGEQ15min:0,1 (feature meaning: driving distance to the restaurant/bar
  # for using the coupon is greater than 15 minutes)
# toCouponGEQ25min:0, 1
  # (feature meaning: driving distance to the restaurant/bar for using the
  # coupon is greater than 25 minutes)
# directionsame:0, 1
  # (feature meaning: whether the restaurant/bar is in the same direction as
  # your current destination)
# direction_opp:1, 0
  # (feature meaning: whether the restaurant/bar is in the same direction as
  # your current destination)
# Y:1, 0 (whether the coupon is accepted)


skimr::skim(coupon_data)
# ── Data Summary ────────────────────────
# Values
# Name                       Piped data
# Number of rows             12684
# Number of columns          26
# _______________________
# Column type frequency:
#   character                18
# numeric                  8
# ________________________
# Group variables            None
#
# ── Variable type: character ───────────────────────────────────────────────────────────────────────
# skim_variable        n_missing complete_rate   min   max empty n_unique whitespace
# 1 destination                  0             1     4    15     0        3          0
# 2 passanger                    0             1     5     9     0        4          0
# 3 weather                      0             1     5     5     0        3          0
# 4 time                         0             1     3     4     0        5          0
# 5 coupon                       0             1     3    21     0        5          0
# 6 expiration                   0             1     2     2     0        2          0
# 7 gender                       0             1     4     6     0        2          0
# 8 age                          0             1     2     7     0        8          0
# 9 maritalStatus                0             1     6    17     0        5          0
# 10 education                    0             1    16    38     0        6          0
# 11 occupation                   0             1     5    41     0       25          0
# 12 income                       0             1    15    16     0        9          0
# 13 car                          0             1     6    40     0        6          0
# 14 Bar                          0             1     3     7     0        6          0
# 15 CoffeeHouse                  0             1     3     7     0        6          0
# 16 CarryAway                    0             1     3     7     0        6          0
# 17 RestaurantLessThan20         0             1     3     7     0        6          0
# 18 Restaurant20To50             0             1     3     7     0        6          0
#
# ── Variable type: numeric ─────────────────────────────────────────────────────────────────────────
# skim_variable     n_missing complete_rate   mean     sd    p0   p25   p50   p75  p100 hist
# 1 temperature               0             1 63.3   19.2      30    55    80    80    80 ▃▁▅▁▇
# 2 has_children              0             1  0.414  0.493     0     0     0     1     1 ▇▁▁▁▆
# 3 toCoupon_GEQ5min          0             1  1      0         1     1     1     1     1 ▁▁▇▁▁
# 4 toCoupon_GEQ15min         0             1  0.561  0.496     0     0     1     1     1 ▆▁▁▁▇
# 5 toCoupon_GEQ25min         0             1  0.119  0.324     0     0     0     0     1 ▇▁▁▁▁
# 6 direction_same            0             1  0.215  0.411     0     0     0     0     1 ▇▁▁▁▂
# 7 direction_opp             0             1  0.785  0.411     0     1     1     1     1 ▂▁▁▁▇
# 8 Y                         0             1  0.568  0.495     0     0     1     1     1 ▆▁▁▁▇







coupon_recipe <- recipe(y ~ ., data = coupon_data) %>%
  step_other(occupation) %>%
  step_dummy(destination,
             passanger,
             weather,
             time,
             coupon,
             expiration,
             gender,
             age,
             maritalStatus,
             education,
             occupation,
             income,
             car,
             bar,
             coffeeHouse,
             carryAway,
             restaurantLessThan20,
             restaurant20To50, one_hot = T) %>%
  step_mutate(y = as.factor(y)) %>%
  prep()






# I was worried we would have TOOOOOO many features in occupation and age.
# Turns out occupation has too many features.  age I can live with.
# So, now we've got a "finalized" data set.  I'm thinking we should one-hot encode this.
#

final_coupon <- bake(coupon_recipe, NULL)
final_coupon %>% names()

final_matrix <- final_coupon %>% select(-y)
fmatrix <- data.matrix(final_matrix)
final_coupon_dmatrix <- xgboost::xgb.DMatrix(fmatrix, label = final_coupon$y)

coup_xgb <- xgboost::xgb.train(nrounds = 1000,
                               data = final_coupon_dmatrix)

xgb_preds <- predict(coup_xgb,
                     newdata = final_coupon_dmatrix,
                     predcontrib = T,
                     approxcontrib = T)

shaps <- as.data.frame(xgb_preds) %>%
  as_tibble() %>%
  select(-BIAS)

shap_values <- shaps %>%
  pivot_longer(names_to = "features", values_to = "shaps",
               cols = everything()) %>%
  mutate(features = as.factor(features),
         features = fct_reorder(features, shaps, .fun = median),
         abs_shap = abs(shaps))

shapper_plot_data <- shap_values %>%
  group_by(features) %>%
  summarize(median_shap = median(abs(shaps))) %>%
  ungroup() #%>%
  #top_n(20)

set.seed(69420)
shapper %>%
  inner_join(shapper_plot_data)%>%
  sample_n(100000) %>%
  ggplot(aes(x = features, y = shaps, group = features)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "SHAP Value")

set.seed(69420)
shap_values %>%
  inner_join(shapper_plot_data)%>%
  sample_n(100000) %>%
  ggplot(aes(x = shaps, y = features , group = features)) +
  ggridges::geom_density_ridges() +
  labs(title = "SHAP Value")


feat_matrix <- final_matrix %>%
  mutate(id = row_number()) %>%
  pivot_longer(names_to = "features",
               values_to = "feature_value",
               cols = -id)

# what happens if we include BIAS??? you, can't.
# it's got _all_ features inclusive

# I need a function to:
  # 1. clean the data mtrix 1:1 CHECK!
  # 2. clean the shaps 1:1 CHECK!
  # 3. a function to join data matrix and shaps 1:1
  # 4. a function to plot them 1:1

clean_feat_matrix <- function(df_before_dmatrix) {
  long_feat_df <- df_before_dmatrix %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "features",
                 values_to = "feature_value",
                 cols = -id) %>%
    filter(feature_value != 0) %>%
    separate(features,
             into = c("feature", "subfeature"),
             remove = FALSE)

  long_feat_df
}

clean_shaps_matrix <- function(xgb_preds) {
  shaps_df <- as.data.frame(xgb_preds) %>%
    as_tibble() %>%
    select(-BIAS) %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "features", values_to = "shaps",
                 cols = -id)
}

feat_df <- clean_feat_matrix(final_matrix)



shaps_to_graph <- shaps %>%
  inner_join(feat_matrix)

cols <- c('destination', 'passanger',
'weather','time',
'coupon','expiration',
'gender','age',
'maritalStatus','education',
'occupation','income',
'car','Bar',
'CoffeeHouse','CarryAway',
'RestaurantLessThan20','Restaurant20To50')




shapper <- function(x, df, sample_size = NULL) {

  if (!is.null(sample_size)) {
    set.seed(69420)
    data_to_plot <- df %>%
      sample_n(sample_size)
  }

  data_to_plot <- data_to_plot %>%
    filter(str_detect(string = features, pattern = x)) %>%
    tidyr::separate(col = features,
                    into = c('feature', 'subset'),
                    sep = '_') %>%
    mutate(subset = fct_reorder(subset, shaps, .fun = median))

  data_to_plot %>%
    ggplot(aes(x = shaps, y = subset, alpha = 0.7)) +
    ggridges::geom_density_ridges() +
    stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), linetype = 2) +
    labs(x = NULL,
         y = NULL,
         title = glue::glue("SHAP Values for {unique(data_to_plot$feature)[1]}"),
         subtitle = 'sorted by median SHAP value, within group median listed with dotted line') +
    facet_wrap(~feature_value) +
    theme(legend.position = "none",
          plot.title.position = "plot")
}

map("coupon", shapper, df = shaps_to_graph, sample_size = 100000)



# Gameplan:
# 1. Get a logistic regression model up
# 1. a. Create and move predictions
# 2. Look at the probabilities of predictions
# 3. Data viz it.

# initial
coupon_lr <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

coupon_wf <- workflow() %>%
  add_model(coupon_lr) %>%
  add_formula(Y ~ .)


set.seed(69420)
coupon_folds <- vfold_cv(data = final_coupon, v =4)

coupon_lr_grid <- tune_grid(
  coupon_wf,
  resamples = coupon_folds,
  control = control_grid(save_pred = T, verbose = T)
)

lr_acc <- coupon_lr_grid %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = penalty, y = mixture, fill = mean, color = mean)) +
    geom_point() +
    geom_text_repel(aes(label = round(mean, 3)), nudge_x = 0.2) +
    labs(title = "Results for cv tuning")

lr_roc_auc <- coupon_lr_grid %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(x = penalty, y = mixture, fill = mean, color = mean)) +
    geom_point() +
    geom_text_repel(aes(label = round(mean, 3)), nudge_x = 0.2)

lr_acc + lr_roc_auc

# So, conclusion here is MIXTURE DOESN'T MATTER!
# So, I hate MIXTURE = 1.
# I love ridge regression which is MIXTURE = 0, I'll probably move
# on with that.
# Don't select my features for me LASSO! I'm my own person!

coupon_lr_grid %>%
  select_best()

final_lr <- logistic_reg(
  penalty = 0.0000000192,
  mixture = 0.0
) %>%
  set_engine("glmnet")

lr_model <- final_lr %>%
  fit(Y ~ ., data = final_coupon)

set.seed(69420)
pretend_holdout <- final_coupon %>%
  sample_n(6000)

preds <- lr_model %>%
  predict(pretend_holdout, type = "prob") %>%
  bind_cols(pretend_holdout) %>%
  select(-.pred_0)

preds <- read_csv(file.path(predictions_folder, 'lr_preds.csv'))
# preds %>%
#   write_csv(file.path(predictions_folder, 'lr_preds.csv'))

# now it's time to see what our model has chosen with this data
# should we look at a high level overview of features?
# why not!
lr_model %>%
  tidy() %>%
  filter(term != '(Intercept)',
         abs(estimate) > 0.25 ) %>%
  mutate(sign = ifelse(estimate < 0, "neg", "pos"),
         term = fct_reorder(term, estimate, abs)) %>%
  ggplot(aes(x = term, y = estimate, fill = sign)) +
    geom_bar(stat = "identity") +
    # geom_text(aes(label = round(estimate, 3))) +
    coord_flip() +
    labs(title = "Feature Importance",
         subtitle = "Pink = less likely to accept the coupon \nGreen = more likely to accept the coupon",
         caption = "a more accurate graph would have all features scaled from 0 to 1, or on the same scale") +
    theme(plot.title.position = "plot",
          legend.position = "none")

# this is really hard to read! and I have glasses... let's trim this where
# the absolute value of the coefficient is at least 0.25

cols <- c("pos" = "green")

lr_model %>%
  tidy() %>%
  filter(term != '(Intercept)',
         estimate >= 0.25) %>%
  mutate(sign = ifelse(estimate > 0, "neg", "pos"),
         term = fct_reorder(term, estimate, abs)) %>%
  ggplot(aes(x = term, y = estimate, fill = sign)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = round(estimate, 3))) +
  coord_flip() +
  labs(title = "Feature Importance: Positive",
       subtitle = "Pink = less likely to accept the coupon \nGreen = more likely to accept the coupon",
       caption = "a more accurate graph would have all features scaled from 0 to 1, or on the same scale") +
  scale_fill_manual(values = "green") +
  theme(plot.title.position = "plot",
        legend.position = "none")
# To Summarize :
#   1) Carry out and take away coupons
#   2) Coupon price for restaurants are less than $20, to a coffee house,
#     or missing
#   3) education didn't graduate
#   4) No real intention of going anywhere in the car ride
#   5) weather's nice
#   6)


lr_model %>%
  tidy() %>%
  filter(term != '(Intercept)',
         estimate <= -0.25) %>%
  mutate(sign = ifelse(estimate > 0, "neg", "pos"),
         term = fct_reorder(term, estimate, abs)) %>%
  ggplot(aes(x = term, y = estimate, fill = sign)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = round(estimate, 3))) +
  coord_flip() +
  labs(title = "Feature Importance: Negative",
       subtitle = "Pink = less likely to accept the coupon \nGreen = more likely to accept the coupon",
       caption = "a more accurate graph would have all features scaled from 0 to 1, or on the same scale") +
  theme(plot.title.position = "plot",
        legend.position = "none")
# To Summarize :
#   1) Crossover, Scooter & Motorcycles, No Car or "Missing" says you're
#       less likely to accept the coupon
#   2) "high" income (>$62.5k) and lots of education means less likely to
#       accept
#   3) CoffeeHouse, if it's missing, you go there a _ton_, 1, or neve
#   4) Bar and Restaurant with $20 - $50 price point: Never, Missing,
#   5) Expiration is 2h less likely to take

# What about the predictions on our holdout set?
# How do their densities compare across our variables?

# How does education influence whether you accept a coupon?
preds %>%
  mutate(education = fct_reorder(education, .pred_1, .fun = function(x) quantile(x, 0.95))) %>%
  ggplot(aes(x = .pred_1, y = education)) +
    geom_density_ridges() +
    labs(title = "Scale goes from 0 to 1",
         subtitle = "closer to 1 -> more likely to accept coupon\ncloser to 0 -> less likely to accept coupon",
         caption = "density ridges sorted by 95th percentile of prediction from pen log reg model") +
    theme(plot.title.position = "plot")

density_plotter <- function(density_ridge_split) {
  density_ridge_split_name <- as.name(density_ridge_split)
  preds %>%
    mutate(density_ridge_split = fct_reorder(!!density_ridge_split_name, .pred_1, .fun = function(x) quantile(x, 0.95))) %>%
    ggplot(aes(x = .pred_1, y = density_ridge_split)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = "Scale goes from 0 to 1",
         subtitle = "closer to 1 -> more likely to accept coupon\ncloser to 0 -> less likely to accept coupon",
         caption = "density ridges sorted by 95th percentile of prediction from pen log reg model",
         y = density_ridge_split,
         x = "probability of accepting a coupon") +
    scale_x_continuous(labels = scales::percent)+
    theme(plot.title.position = "plot")
}

# density plotter go BRRRRRRRRRRR
density_plotter("education")

density_names_batch_1 <- preds %>%
  select(where(is.factor)) %>%
  select(1:9)

density_names_batch_2 <- preds %>%
  select(where(is.factor)) %>%
  select(10:19)

# map(names(density_names), density_plotter)
# my computer can't hang!

# Use map because for loops strike fear in all R users
map(names(density_names_batch_1), density_plotter)


map(names(density_names_batch_2), density_plotter)


# this was an interesting dive into what the logistic
# regression model is doing when it's predicting.



# could we do the same with SHAP values and GBMs?
coupon_gbm_spec <- boost_tree(
  mode = "classification",
  trees = 1000
) %>%
  set_engine("xgboost")

coupon_gbm <- coupon_gbm_spec %>%
  fit(y ~ ., final_coupon)


gbm_preds <- coupon_gbm %>%
  predict(pretend_holdout) %>%
  bind_cols(preds)


tidy_shaps <- coupon_gbm %>%
  predict(pretend_holdout,
          type = "raw",
          opts = list(predcontrib = T,
                      approxcontrib = F)) %>%
  as_tibble()

final_matrix <- final_coupon %>% select(-y)

feat_matrix <- final_matrix %>%
  mutate(id = row_number()) %>%
  pivot_longer(names_to = "features",
               values_to = "feature_value",
               cols = -id)

# what happens if we include BIAS??? you, can't.
# it's got _all_ features inclusive

# I need a function to:
# 1. clean the data mtrix 1:1 CHECK!
# 2. clean the shaps 1:1 CHECK!
# 3. a function to join data matrix and shaps 1:1 CHECK!
# 4. a function to plot them 1:1 CHECK!

clean_feat_matrix <- function(df_before_dmatrix, response_variable) {
  #' @param: df_before_dmatrix {tibble} the tibble before it's turned into a dmatrix
  #' @param: response_variable {string} the response variable to be predicted
  #'


  response_name <- as.name(response_variable)

  long_feat_df <- df_before_dmatrix %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "features",
                 values_to = "feature_value",
                 cols = -c(id, all_of(response_name))) %>%
    filter(feature_value != 0) %>%
    mutate(features = str_replace_all(features, pattern = "\\.+", replacement = '_')) %>%
    separate(features,
             into = c("feature", "subfeature"),
             remove = FALSE,
             extra = "merge")

  long_feat_df
}

clean_shaps_matrix <- function(xgb_preds) {
  #' @param: xgb_preds {tibble} the tibble after predicting SHAPs from tidy models shap
  #'

  shaps_df <- as.data.frame(xgb_preds) %>%
    as_tibble() %>%
    select(-BIAS) %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "subfeatures", values_to = "shaps",
                 cols = -id) %>%
    mutate(subfeatures = str_replace_all(subfeatures,
                                         pattern = "\\.+",
                                         replacement = '_')) %>%
    separate(subfeatures,
             into = c("feature", "subfeature"),
             remove = FALSE,
             extra = "merge") %>%
    group_by(id, feature) %>%
    summarize(shaps = sum(shaps), .groups = "drop")
}

combine_shaps_and_features <- function(shaps_frame, feat_df) {
  #' @param: shap_frame {tibble}: the tibble from `clean_shaps_matrix`
  #' @param: feat_df {tibble}: the tibble from `clean_feat_matrix`

  shaps_to_graph <- shaps_frame %>%
    inner_join(feat_df, by = c("id", "feature"))

  cat_feats <- shaps_to_graph %>%
    filter(!is.na(subfeature))

  num_feats <- shaps_to_graph %>%
    filter(is.na(subfeature))

  return_vec <- c()
  return_vec[['cats']] <- cat_feats
  return_vec[['numeric']] <- num_feats

  return_vec
}

shapper_cats <- function(x = NULL, df, sample_size = NULL) {
  #' @param: x {string}: the categorical variable to plot grouped densities from
  #' @param: df {tibble}: the categorical data frame output from `combine_shaps_and_features`
  #' @param: sample_size {int}: how many shap value to sample from the total
  #'

  if (!is.null(sample_size)) {
    set.seed(69420)
    df <- df %>%
      sample_n(sample_size)
  }

  if (!is.null(x)) {
    df <- df %>%
      filter(str_detect(string = feature, pattern = x)) %>%
      mutate(subfeature = fct_reorder(subfeature, shaps, .fun = median))

    var_for_title <- unique(df$feature)[1]
  } else {
    df <- df %>%
      mutate(subfeature = fct_reorder(subfeature, shaps, .fun = median))

    var_for_title <- "All Categorical Variables"
  }

  df %>%
    ggplot(aes(x = shaps, y = subfeature, alpha = 0.7)) +
    ggridges::geom_density_ridges() +
    stat_summary(fun = "median", geom = "errorbar", aes(xmax = ..x.., xmin = ..x..), linetype = 2) +
    labs(x = NULL,
         y = NULL,
         title = glue::glue("SHAP Values for {var_for_title}"),
         subtitle = 'sorted by median SHAP value, within group median listed with dotted line') +
    theme(legend.position = "none",
          plot.title.position = "plot")
}

shapper_numeric <- function(x = NULL, df, sample_size = NULL) {
  #' @param: x {string}: the numerical variable to plot densities from
  #' @param: df {tibble}: the numeric data frame output from `combine_shaps_and_features`
  #' @param: sample_size {int}: how many shap value to sample from the total
  #'


  if (!is.null(sample_size)) {
    set.seed(69420)
    df <- df %>%
      sample_n(sample_size)
  }

  if (!is.null(x)) {
    df <- df %>%
      filter(str_detect(string = feature, pattern = x))

    var_for_title <- unique(df$feature)[1]
  } else {
    var_for_title <- "All Numeric Variables"
  }

  df %>%
    ggplot(aes(x = feature_value, y = shaps)) +
    ggridges::geom_density_ridges() +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~feature) +
    labs(x = NULL,
         y = NULL,
         title = glue::glue("SHAP Values for {var_for_title}")) +
    theme(legend.position = "none",
          plot.title.position = "plot")
}

# set up the list of categorical variables you want to graph
cols <- c('destination', 'passanger',
          'weather','time',
          'coupon','expiration',
          'gender','age',
          'maritalStatus','education',
          'occupation','income',
          'car')

# data cleaning functions
feat_df <- clean_feat_matrix(pretend_holdout, "y")
shaps_frame <- clean_shaps_matrix(tidy_shaps)

shap_vector <- combine_shaps_and_features(shaps_frame, feat_df)

# plot the densities in their respective groups
list_of_cats <- map(.x = cols, .f = shapper_cats, df = shap_vector$cats)
patchwork::wrap_plots(list_of_cats, ncol = 3, nrow = 5)

# plot them altogether
shapper_cats(df = shap_vector$cats, sample_size = 100000)

# plot numeric factors
shapper_numeric(df = shap_vector$numeric)














