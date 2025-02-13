# Loading required libraries
library(tidyverse)
library(broom)

# Recollect dataset and model
model <- read_rds("C:/Users/Erhun/Documents/Data Analysis/Kaggle/Telco Customer Churn/Datasets and Model Object/model_object.rds")
model %>%
  summary()

# Recollect dataframe
churn.data

# Check out most important variables in the logistic model. This gives us a first-hand view on what affects Churn the most
tidy(model$fit$fit) %>%
  filter(term != "(Intercept)") %>%
  arrange(-abs(estimate)) %>%
  print(n = 30)

# A model variable named Contat_Two.year seems to have the most effect on the logistic model by having an inverse relationship with the log odds of the occurance of Churn. The presence of it reduces the probability of Churn occuring
# Lets check it out
churn.data %>%
  count(Contract, Churn) %>%
  group_by(Contract) %>%
  mutate(prop = n/sum(n))
# These set of proportion makes sense. People on a two year contract are extremely likely to continue subscriptions.

# Next is the presence of fiber optics which seems to make customer churn more likely. Lets check it out
churn.data %>%
  count(InternetService, Churn) %>%
  group_by(InternetService) %>%
    mutate(prop = n/sum(n))
# Again, this proportions makes sense. 40% of customers on Fiber optics Churn compared to DSL and None with 19% and 7% respectively

# Total amount of charges payed to Telco seem to affect the probability of churning positively. Lets chect that out
churn.data %>%
  mutate(Churn = as.factor(if_else(Churn == "No", "Did not Churn", "Churned"))) %>%
  ggplot(aes(TotalCharges, Churn, fill = Churn)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = "Total Charges",
    y = NULL
  )
# While there is large overlaps, the means of the distribution of Total Charges for those that Churned is considerably lower than those who joined.

# Lets also review the effects of monthly charges
churn.data %>%
  mutate(Churn = as.factor(if_else(Churn == "No", "Did not Churn", "Churned"))) %>%
  ggplot(aes(MonthlyCharges, Churn, fill = Churn)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = "Total Charges",
    y = NULL
  )
# Its seems that those that churned tended to have pay more monthly charges than others
# However, the model seemed to have penalized this variable to have 0 effect on the outcome. Perhaps Total Charges carried similar information.

# Finally, cross check the effects of paperless billing
churn.data %>%
  count(PaperlessBilling, Churn) %>%
  group_by(PaperlessBilling) %>%
   mutate(prop = n/sum(n))
# Customers who are billed online tend to have higher proportion of churn than those who are not
























