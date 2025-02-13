# Simple Data Analysis on the Churn

This is mainly a practice iteration and would be updated with a ShinyApp for better appreciation.

## Project Objectives
The main aim of this project is to analyize and predict the amount of churning a Telecommunication company would experience.

## Data Source
The data used as the basis of this project is gotten from Kaggle and may be view [here](https://www.kaggle.com/datasets/blastchar/telco-customer-churn).

## Methodology
The methodology used in this project includes the following:
- Preprocessing and Cleaning
- Modeling
- Interpretation and Analysis

While quite un-orthodox, carrying out data modeling before analysis allowed the absolute weights from the logistic model to guide the search for the variables that actually distinguished between those who churned and those who did not. The findings includes the following:
- Customers who churn tends to be with any contract with the company, gaining their subscription on a one-on-one basis
- Customers on fiber optics connections tended to churn more quickly than others.
- Customers who are billed online tend to churn more easily

## Side Notes
While the methodology above proved useful in streamlining analysis, it did not capture all the essence of churning in this company. For instance, the Senior Citizen variable constant is set to zero in the model even though older people churn in proportions significantly higher than others.
Also, it isn't intuitive why the use of fiber optics increases the propability of a customer churning as opposed to none, as internet should be faster in them. Perhaps the company's service quality in that area is lacking and should be looked on more thoroughly.
