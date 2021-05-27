
# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load("data/saved_stuff.rda")
load("model_info/mars_tune.rda")
load("model_info/rf_tune.rda")
load("model_info/bt_tune.rda")

train <- read_csv("data/train.csv")
train <- editing_data(train)

test <- read_csv("data/test.csv")
test <- editing_data(test)


# Assessments ---------------------------------------------------------

## Mars 
mars_best_five <- mars_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  head(5) %>% 
  select(num_terms, prod_degree)

## Boosted Trees 
bt_best <- bt_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  head(5) %>%
  select(mtry, min_n, learn_rate)

## Random Forest 
rf_best <- rf_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean) %>% 
  head(10)

# Fitting  -----------------------------------------------------------

### MARS 
mars_predicts <- function(i){ 
  
  mars_model <- mars(
    num_terms = mars_best_five[[i, 1]],
    prod_degree = mars_best_five[[i, 2]]
  ) %>% 
    set_mode("regression") %>% 
    set_engine("earth")
  
  mars_workflow <- workflow() %>% 
    add_model(mars_model) %>% 
    add_recipe(loan_recipe) 
  
  mars_workflow %>% 
    fit(data = train) %>% 
    predict(new_data = test) %>% 
    cbind(id = test$id) %>% 
    select("ID" = id, "Predicted" = .pred)
  
  
  }
 
for(i in 1:5){
  write.csv(
    mars_predicts(i),
    paste0("predictions/mars_prediction", i, ".csv"),
    row.names = F
  )
}


## Boosted Trees
bt_predicts <- function(i){ 
  
  bt_model <- boost_tree(
    mode = "regression", 
    mtry = bt_best[[i, 1]], 
    min_n = bt_best[[i, 2]], 
    learn_rate = bt_best[[i, 3]]
  ) %>% 
    set_engine("xgboost")
  
  bt_workflow <- workflow() %>% 
    add_model(bt_model) %>% 
    add_recipe(loan_recipe) 
  
  bt_workflow %>% 
    fit(data = train) %>% 
    predict(new_data = test) %>% 
    cbind(id = test$id) %>% 
    select("ID" = id, "Predicted" = .pred)
  
  
}

for(i in 1:5){
  write.csv(
    bt_predicts(i),
    paste0("predictions/bt_prediction", i, "moreparams.csv"),
    row.names = F
  )
}


### Random Forest 
## Boosted Trees
rf_predicts <- function(i){ 
  
  rf_model <- rand_forest(
    mtry =  rf_best [[i, 1]], 
    min_n = rf_best[[i, 2]]
  ) %>% 
    set_mode("regression") %>% 
    set_engine("ranger")
  

  rf_workflow <- workflow() %>% 
    add_model(rf_model) %>% 
    add_recipe(rf_recipe) 
  
  rf_workflow %>% 
    fit(data = train) %>% 
    predict(new_data = test) %>% 
    cbind(id = test$id) %>% 
    select("ID" = id, "Predicted" = .pred)
  
  
  
}

for(i in 1:5){
  write.csv(
    rf_predicts(i),
    paste0("predictions/rf_prediction", i, ".csv"),
    row.names = F
  )
}
