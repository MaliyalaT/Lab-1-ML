# Load tidyverse package
library(tidyverse)

# Load tidymodels package
library(tidymodels)

# Load janitor package
library(janitor)

# Read in the data
student_data <- read_csv("studentInfo.csv")

# Mutate variables
student_data <- student_data %>%
  mutate(pass_binary = ifelse(final_result == "Pass", 1, 0)) %>%
  mutate(pass_binary = as.factor(pass_binary))

student_data <- student_data %>%
  mutate(has_disability = as.factor(disability))

# Examine the data
student_data

# Feature engineering
student_data <- student_data %>%
  mutate(imd_band_factor = factor(imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))) %>%
  mutate(imd_band_numeric = as.integer(imd_band_factor))

# Split data
set.seed(20230712)
train_test_split <- initial_split(student_data, prop = 0.80)
data_train <- training(train_test_split)
data_test <- testing(train_test_split)

# Create a recipe
my_recipe <- recipe(pass_binary ~ has_disability + imd_band_factor, data = data_train)

# Specify the model
my_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Add model and recipe to workflow
my_workflow <- 
  workflow() %>% 
  add_model(my_model) %>% 
  add_recipe(my_recipe)

# Fit model
fitted_model <- fit(my_workflow, data = data_train)


test_split <- rsample::initial_split(data_test, prop = 0.8)

# Fit the model using the testing data
final_fit <- last_fit(my_workflow, split = test_split)


final_fit

# Collect predictions
final_fit %>%
  collect_predictions()

# prediction range
final_fit %>%
  collect_predictions() %>%
  select(.pred_class, pass_binary) %>%
  mutate(correct_prediction = .pred_class == pass_binary) %>%
  tabyl(correct_prediction)

# Wrap up and knit the document
