source("utils.R")

theme_set(theme_minimal())

library(tidyverse)
library(tidymodels)
library(skimr)
library(DataExplorer)


data_folder <- "data"
plots_folder <- "plots"
models_folder <- "models"

housing <- read_csv(file.path(data_folder, "austinHousingData.csv")) %>% 
  janitor::clean_names("small_camel") %>% 
  mutate_if(is.character, as.factor)
  
# histogram of prices
qplot(housing$latestPrice) +
  labs(title = "hist of latest price") +
  scale_x_log10(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title.position = "plot")


housing_splits <- initial_split(housing, prop = 4/5)
housing_train <- training(housing_splits)
housing_test <- testing(housing_splits)


housing_recipe <- recipe(latestPrice ~ ., data = housing_train) %>% 
  # remove non-numeric columns
  step_rm(latestPriceSource, 
          homeImage,
          latestSaledate,
          description,
          streetAddress) %>%
  step_dummy(homeType,
             city) %>% 
  prep()



clean_housing_train <- bake(housing_recipe, new_data = NULL)
clean_housing_test <- bake(housing_recipe, new_data = housing_test)

base_xgb <- boost_tree(mode = "regression") %>% 
  set_engine("xgboost")

base_fit <- base_xgb %>% 
  fit(latestPrice ~ ., data = clean_housing_train)

initial_preds <- base_fit %>% 
  predict(clean_housing_test) %>% 
  bind_cols(clean_housing_test) %>% 
  mutate(residuals = latestPrice - .pred)

rmse(initial_preds, truth = latestPrice, estimate = .pred)$.estimate
# 244,462.5 all numerics, no tuning
# 243,335.3 + homeType, no tuning
# 244,361.1 + homeType, city, no tuning


ggplot(initial_preds, aes(x = latestPrice, y = .pred)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "true",
       y = "predictions",
       title = "preds vs. true values",
       subtitle = "linear model for geom_smooth") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  coord_obs_pred() +
  theme(plot.title.position = "plot")

ggplot(initial_preds, aes(x = latestPrice, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "true values",
     y = "residuals",
     title = "residuals: an unsightly trend updwards",
     subtitle = "linear model for geom_smooth") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.title.position = "plot")

tidy_shaps <- base_fit %>% 
  predict(clean_housing_train,
          type = "raw",
          opts = list(predcontrib= T,
                      approxcontrib = F)) %>% 
  as_tibble()


feat_df <- clean_feat_matrix(clean_housing_train, NULL)
shaps_df <- clean_shaps_matrix(tidy_shaps)
shap_vec <- combine_shaps_and_features(shaps_df, feat_df)


shapper_numeric(df = shap_vec$numeric) +
  labs(title = "SHAP Values for All Numeric Variables",
       subtitle = "the numeric column is on the x-axis, the SHAP value on the y-axis") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

shapper_cats(df = shap_vec$cats)










