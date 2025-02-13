# Loading required libraries
library(tidyverse)

# Loading required dataset
churn.data <-
read.csv("C:/Users/Erhun/Documents/Data Analysis/Kaggle/Telco Customer Churn/Datasets and Model Object/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>%
  as_tibble()
churn.data

# Budget data by spliting data set into train and test

# Create random row samples
train.numerics <- sample(x = 1:dim(churn.data)[1], size = dim(churn.data)[1]*0.8)
train.numerics

# Taking out train samples
churn.train <-
churn.data[train.numerics,]
churn.train

# Taking out test samples
churn.test <-
churn.data[-train.numerics,]
churn.test

# Checking out data information
glimpse(churn.data)

# Checking out NA information
churn.data %>%
  is.na %>%
  sum

churn.data %>%
  filter(!complete.cases(.))

# Making function to check na values, empty values and column data type
count.stuff <-
function(data){

  # Redefine data to aviod confusion
  function.data = data

  # Declare need vectors for output columns
  empty.vector = c()
  na.vector = c()
  type.vector = c()

  # Create for loop to fill up declared vectors
  for(i in colnames(function.data)){

    # Taking NA counts and appending them to na.vector
    na.vector = c(
      na.vector,
      sum(is.na(function.data[[i]]))
    )

    # Taking empty cell counts and appending them to empty.vector
    empty.vector = c(
      empty.vector,
      nrow(function.data %>% filter(i == ""))
    )

    # Taking data type counts and appending them to type.vector
    type.vector = c(
      type.vector,
      class(function.data[[i]])
    )

  }

  # Create output dataset
  output = tibble(
    columns = colnames(function.data),
    class_type = type.vector,
    na_count = na.vector,
    empty_count = empty.vector
  )

  # Specify output for return
  return(output)

}

# Lets see the overall data composition of churn data
count.stuff(churn.data) %>%
  print(n = 22)

# Check normality of monthly charges data
churn.data %>%
  select_if(is.numeric) %>%
  ggplot(aes(MonthlyCharges)) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Monthly Charges",
    y = NULL
  )

# Seeing if the log would make the data normal
churn.data %>%
  select_if(is.numeric) %>%
  ggplot(aes(log(MonthlyCharges))) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Log of Monthly Charges",
    y = NULL
  )

# Check normality of total charges data
churn.data %>%
  select_if(is.numeric) %>%
  ggplot(aes(TotalCharges)) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Total Charges",
    y = NULL
  )

# Seeing if the log would make the data normal
churn.data %>%
  select_if(is.numeric) %>%
  ggplot(aes(log(TotalCharges))) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Log of Total Charges",
    y = NULL
  )
