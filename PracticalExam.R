# Datacamp Practical Actual Exam 1

# TASK 1
# Use this cell to write your code for Task 1

setwd("/Users/torbenparker/Downloads")
library(tidyverse)

# Helper functions for counting/identifying:
print_nonmatch <- function(my_vector, target_categories) {
  nonmatch_strings <- my_vector[!my_vector %in% target_categories]
  intro_string <- str_c("Nonmatched strings in ", 
                        deparse(substitute(my_vector)), ":")
  print(intro_string)
  print(unique(nonmatch_strings))
  print(str_c("Number of nonmatched strings = ", 
              as.character(length(nonmatch_strings))))
}

count_na <- function(my_vector) {
  na_count <- sum(is.na(my_vector))
  print(str_c("Number of NAs in ", 
              deparse(substitute(my_vector)), 
              ": ", as.character(na_count)))
}

# Load and examine data
house_sales_pt1 <- read_csv("house_sales.csv")
str(house_sales_pt1)

# Checking non-conforming categories:
print_nonmatch(house_sales_pt1$city, 
               c("Silvertown", "Riverford", "Teasdale", "Poppleton"))
# There are 73 nonconforming strings, all "--", functionally missing values
# Counting NAs:
count_na(house_sales_pt1$city) 
# There are 0 correct NAs.

# Define object
missing_city <- 73
print(missing_city)

# TASK 2
# Use this cell to write your code for Task 2

# Loading data
house_sales_pt2 <- read_csv("house_sales.csv")
str(house_sales_pt2)
# 'city' is a chr. It should be a factor.
# 'sale_price' is a num. It should be an integer.
# 'sale_date' is a Date. This is correct.
# 'months_listed' is a num. This is correct.
# 'bedrooms' is a num. It should be an integer.
# 'house_type' is a chr. It should be an ordinal factor.
# 'area' is a chr. It should be a num.

# Examining 'area':
check_nonnumber <- function(my_vector) {
  nonnumber_strings <- str_subset(my_vector, "^[0123456789.]+$", negate = TRUE)
  num_nonnumber <- length(nonnumber_strings)
  if (num_nonnumber == 0) {
    print("Number of non-number rows = 0")  
  } else {
    print(str_c("Number of non-nunber rows = ", as.character(num_nonnumber)))
    print("Non-number strings:")
    print(nonnumber_strings)
  }
}
check_nonnumber(house_sales_pt2$area)
# It appears all incorrect values have " sq.m." at the end.

# Data cleaning
clean_data <- house_sales_pt2 %>%
# city: Replace 73x of the "--" with "Unknown", change to factor (from Task 1)
mutate(city = str_replace_all(city, "--", "Unknown"),
       city = as.factor(city),
# sale_price: Round to 0 decimal places, convert to integer
       sale_price = round(sale_price, digits = 0),
       sale_price = as.integer(sale_price),
# months_listed: replace nas with mean rounded to 1 decimal place
       months_listed = replace_na(months_listed,
                                  round(mean(months_listed, na.rm = TRUE),
                                        digits = 1)),
# bedrooms: Round to 0 decimal places, convert to integer
       bedrooms = round(bedrooms, digits = 0),
       bedrooms = as.integer(bedrooms),
# house_type: Edit abbreviations, convert to ordinal factor (Terr > Semi > Det)
# Order was chosen since detached units are likely to be the most expensive.
       house_type = str_replace_all(house_type,
                                    c("^Terr.$" = "Terraced", 
                                      "^Semi$" = "Semi-detached", 
                                      "^Det.$" = "Detached")),
       house_type = factor(house_type, ordered = TRUE,
                           levels = c("Terraced", 
                                      "Semi-detached", 
                                      "Detached")),
# area: Remove non-number characters (" sq.m."); convert to numeric; round
       area = str_remove(area, " sq.m."),
       area = as.numeric(area),
       area = round(area, digits = 1)
)

# Final structure check
str(clean_data)
head(clean_data)

# TASK 3
# Use this cell to write your code for Task 3

price_by_rooms <- clean_data %>%
  group_by(bedrooms) %>%
  summarize(avg_price = round(mean(sale_price), digits = 1), 
            var_price = round(var(sale_price), digits = 1))
head(price_by_rooms)

# TASK 4
# Use this cell to write your code for Task 4

training_data <- read_csv("train.csv")
test_data <- read_csv("validation.csv")

bedrooms_model <- lm(sale_price ~ bedrooms, data = training_data)
base_result <- test_data %>%
mutate(price = predict(bedrooms_model, newdata = test_data)) %>%
select(house_id, price)
head(base_result)

# TASK 5
# Use this cell to write your code for Task 5
library(randomForest)

# Conversion of training and test data to include ordinal factors
training_data_pt5 <- training_data %>%
mutate(house_type = factor(house_type, ordered = TRUE,
                           levels = c("Terraced", "Semi-detached", 
                                      "Detached"))) %>%
mutate(city = factor(city, ordered = FALSE,
                     levels = c("Silvertown", "Riverford", "Teasdale",
                                "Poppleton", "Unknown")))

test_data_pt5 <- test_data %>%
mutate(house_type = factor(house_type, ordered = TRUE,
                           levels = c("Terraced", "Semi-detached", 
                                      "Detached"))) %>%
mutate(city = factor(city, ordered = FALSE,
                     levels = c("Silvertown", "Riverford", "Teasdale", 
                                "Poppleton", "Unknown")))

# Initial model creation:
# Earlier drafts included a higher nodesize, interaction terms (*), 
# and an intercept. These were removed.
forest_model_beta <- randomForest(sale_price ~ city + bedrooms + house_type + 
                                    area + sale_date, 
                                  data = training_data_pt5, 
                                  ntree = 500, mtry = 2, nodesize = 5, 
                                  importance = TRUE)

# Plot %IncMSE and IncNodePurity of each variable
# Determines removal of city and sale_date 
varImpPlot(forest_model_beta)

# Plot changes in error based on ntree
# Error plateaus around ntree = 200
plot(forest_model_beta)

# Final model creation
forest_model_final <- randomForest(sale_price ~ bedrooms + house_type + area, 
                                   data = training_data_pt5, 
                                   ntree = 200, mtry = 2, nodesize = 5, 
                                   importance = TRUE)

# Prediction using model:
compare_result <- test_data_pt5 %>%
  mutate(price = predict(forest_model_final, newdata = test_data_pt5)) %>%
  select(house_id, price)
head(compare_result)