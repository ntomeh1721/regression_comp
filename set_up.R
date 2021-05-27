
# Set Up ------------------------------------------------------------------


# Loading Packages 
library(tidyverse)
library(tidymodels)
library(lubridate)


# Loading Data 
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

# Set Seed
set.seed(1014)

# Writing not in function 
`%notin%` <- negate(`%in%`)



# Exploring Data ----------------------------------------------------------

# missing variables - NONE
train %>%
  naniar::miss_var_summary()

# plotting the distribution of the outcome variable 
train %>% 
  ggplot(aes(money_made_inv)) + 
  geom_histogram()

# skimming the nominal/numeric breakdown of the data
skimr::skim_without_charts(train)

# looking at character vectors to see what can be made
# a numeric or factor variable
train %>% 
  keep(is.character)

###  emp_title variable 

# goal: to aggregate professions into ten groups

# there are 768 different jobs out of 5778 individuals 
train %>% 
  mutate(emp_title = emp_title %>% tolower()) %>% 
  select(emp_title) %>% 
  unique() %>% 
  nrow()

train %>% 
  mutate(emp_title = emp_title %>% tolower()) %>% 
  select(id) %>% 
  unique() %>% 
  nrow()


###  purpose variable

train %>% 
  count(purpose) %>% 
  arrange(desc(n))

# looking to see whether the less frequent purposes 
# are especially correlated with the outcome variable. 
# given that they're not, we can group the more infrequent
# variables together 
# the spreads of "car" and "small_business" are sufficiently different 
# to the infrequent variables so as to leave them out 
train %>% 
  mutate(purpose = factor(purpose, 
                          levels = train %>% 
                            count(purpose) %>% 
                            arrange(desc(n)) %>% 
                            pluck("purpose"))) %>% 
  ggplot(aes(x = purpose, y = money_made_inv)) + 
  geom_boxplot() 





# editing data function -------- 
# to quickly apply the same processes for training and testing data 
editing_data <- function(df){
  
  df %>% 
    mutate(
      
      ### emp_title 
      ### aggregating professions into top 10 main fields, including other
      
      # standardizing case 
      emp_title = emp_title %>% tolower(), 
      # aggregating by field
      emp_title = case_when(
        str_detect(emp_title, "teacher|principal|professor") ~ "education",
        str_detect(emp_title, "manager|supervisor") ~ "manager",
        str_detect(emp_title, "(nurs|^rn$|^lpn$|physician|respitory|anaesthesia|medi|^emt$|therap)") ~ "medicine", 
        str_detect(emp_title, "(admin|assist|account|business|^hr$|human resource|sales)") ~ "business_opps", 
        str_detect(emp_title, "(owner|ceo|director|president|executive|partner|chief)") ~ "executive", 
        str_detect(emp_title, "driver") ~ "driver", 
        str_detect(emp_title, "engin|^it$") ~ "engineer", 
        str_detect(emp_title, "consult|analyst") ~ "consulting_analytics", 
        str_detect(emp_title, "tech|foreman|machi|electrician|mech|operator") ~ "technical", 
        TRUE ~ "other"
      ),
      
      ### earliest_cr_line variable and last_credit_pull_d
      # standardizing the dates into a numeric variable: specifically, 
      # how many weeks prior to Jan 2021 the variable took place 
      
      ## earliest_cr_line
      
      # Getting the month and year of earliest_cr_line
      month_cr_line = str_remove(earliest_cr_line, "-[0-9]*") %>% match(month.abb), 
      year_cr_line = str_remove(earliest_cr_line, ".*-") %>% as.numeric,
      # Turning this into a date 
      date_cr_line = paste0(year_cr_line, "-", ifelse(month_cr_line < 10, "0", ""), month_cr_line, "-01") %>% as.Date(),
      # Finding the Weeks since earliest_cr_line as of Jan 2021. 
      earliest_cr_line = difftime(date_cr_line, "2021-01-01", units = "weeks") %>% 
        as.numeric() %>% abs(), 
      
      ##2 last_credit_pull_d
      
      month_pull_d = str_remove(last_credit_pull_d, "-[0-9]*") %>% match(month.abb), 
      year_pull_d  = str_remove(last_credit_pull_d, ".*-") %>% as.numeric,
      # Turning this into a date 
      date_pull_d  = paste0(year_pull_d, "-", ifelse(month_pull_d < 10, "0", ""), month_pull_d, "-01") %>% as.Date(),
      # Finding the Weeks since earliest_cr_line as of Jan 2021. 
      last_credit_pull_d = difftime(date_pull_d, "2021-01-01", units = "weeks") %>% 
        as.numeric() %>% abs(),
      
      
      ### emp_length 
      emp_length = ifelse(emp_length == "< 1 year", "0.5", str_extract(emp_length, "[0-9]*")) %>% 
        as.numeric(), 
      # single missing value, so make it zero 
      emp_length = ifelse(is.na(emp_length), 0, emp_length), 
      
      
      ### grade and sub_grade variables 
      grade = factor(grade) %>% as.numeric(), 
      sub_grade = substring(sub_grade, 2, 2) %>% as.numeric(), 
      
      ### purpose variable 
      purpose = ifelse(purpose %in% c("medical", "vacation", "moving", 
                                      "house", "renewable_energy", "wedding"), 
                       "other", purpose)
      
      
      ) %>% 
    # selecting out temporary variables 
    select(
      -month_cr_line, -year_cr_line, -date_cr_line,
      -month_pull_d, -year_pull_d, -date_pull_d
      )

  
}

train <- editing_data(train)
test <- editing_data(test)


# Splitting Data ----------------------------------------------------------

# cross validation
loan_folds <- vfold_cv(train, v = 5, repeats = 3, strata = money_made_inv) 

# Recipe ------------------------------------------------------------------

loan_recipe <- recipe(money_made_inv ~ ., data = train) %>% 
  step_interact(~grade:sub_grade) %>% 
  step_dummy(all_nominal()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# loan_recipe %>%
#   prep() %>% 
#   bake(new_data = NULL)


# Saving Out  -------------------------------------------------------------

save(loan_recipe, loan_folds, editing_data, file = "data/saved_stuff.rda")
