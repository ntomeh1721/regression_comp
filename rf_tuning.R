# Random Forest tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load("data/saved_stuff.rda")

# Loading Data 
train <- read_csv("data/train.csv")
train <- editing_data(train)


# updating recipe ----
  
rf_recipe <- recipe(money_made_inv ~ ., data = train) %>% 
  step_interact(~grade:sub_grade) %>% 
  step_dummy(all_nominal(), one_hot = T) %>%
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors())


# rf_recipe %>% 
#   prep() %>% 
#   bake(new_data = NULL)


# Define model ----
rf_model <- rand_forest(
  mtry = tune(), 
  min_n = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")


# set-up tuning grid ----

# update parameters 
rf_params <- parameters(rf_model)  %>% 
  update(mtry = mtry(range = c(3, 48)))

# define tuning grid
rf_grid <- grid_regular(rf_params, levels = 15) 

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe) 

# Tuning/fitting ----
rf_tune <- rf_workflow %>% 
  tune_grid(
    resample = loan_folds, 
    grid = rf_grid
  ) 

# Write out results & workflow
save(rf_tune, rf_workflow, file = "model_info/rf_tune.rda")
