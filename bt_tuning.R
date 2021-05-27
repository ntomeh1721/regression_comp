
# Boosted Tree Tuning  ---------------------------------------------------

## Boosted Tree is  like a random forest, but it learns from other trees 
## i.e. weights different trees

# load packages 
library(tidyverse)
library(tidymodels)

# set seed 
set.seed(2468)

# load required objects ----
load("data/saved_stuff.rda")


# Task 5: Defining the Model  ---------------------------------------------

bt_model <- boost_tree(
  mode = "regression", 
  mtry = tune(), 
  min_n = tune(), 
  learn_rate = tune() 
) %>% 
  set_engine("xgboost")

loan_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)


# Task 6: Building Out Tuning Grid  ---------------------------------------

## set-up tuning grid 

bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(3, 100)),
         learn_rate = learn_rate(range = c(-5, -0.2)))


# define regular grid 
bt_grid <- grid_regular(bt_params, levels = c(5, 10, 5))

# random forest workflow 
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(loan_recipe)


# Tuning/Fitting 
## have the model compete along the folds 
bt_tune <- bt_workflow %>% 
  tune_grid(resamples = loan_folds, 
            grid = bt_grid)

## Tuning is fitting


# Write out results and workflow 
save(bt_tune, bt_workflow, file = "model_info/bt_tune.rda") 

