# customer-churn-analysis
This project analyzes the Telco Customer Churn dataset to predict churn, identify key factors affecting monthly charges and churn, and devise strategies for reducing churn and enhancing customer retention for a telecom company.

This project utilizes the Telco Customer Churn dataset to analyze and predict customer churn for a telecom company. With 7,043 observations and 33 variables covering customer demographics, contact details, and services subscribed, I aim to address three key research questions. Firstly, I seek to uncover factors influencing monthly charges, to build an accurate predictive model. Secondly, I aim to identify major factors contributing to customer churn. Finally, utilizing visualization techniques, I delve deeper into the identified variables to formulate effective strategies for reducing churn and enhancing customer retention. Through rigorous analysis and modeling, this project aims to provide actionable insights to help the telecom company improve customer satisfaction and loyalty.

## Getting Started

1. Clone the Repository: Use Git to clone the repository to your local machine:

`git clone https://github.com/YourUsername/customer-churn-analysis.git`

2. Open RStudio: Navigate to the project directory and open RStudio.
3. Install Dependencies: Ensure you have the required R packages installed. You may need packages like tidyverse, caret, ggplot2, and dplyr. Install them using:

`install.packages(c("janitor", "pacman", "dplyr", "ggplot2", "tidyr", "corrplot", "ISLR", "ROCR", "caret", "usmap", "scales", "cowplot", "glmnet"))`

4. Open the R Script: Open the customer_churn_analysis.R script in RStudio. This script contains the data analysis process, including data preprocessing, exploratory data analysis (EDA), feature engineering, model building, and evaluation.
5. Contribute (Optional): If you have suggestions for improvements or would like to contribute, feel free to open an issue or submit a pull request on GitHub.

## Data source 
<link>https://www.kaggle.com/datasets/yeanzc/telco-customer-churn-ibm-dataset </link>

## Aim of the Analysis 
1. What factors contribute to Monthly Charges, and can we predict them accurately?
2. Identify major factors responsible for customer churn
3. Further investigate the variables found in the above questions and suggest a strategy to reduce customer churn.

## Methodologies 
* Data Cleaning
* Exploratory data analysis for identifing factors affecting monthly charges.
* Deploying liner regression to predict monthly charge and finding optimal model.
* Exploratory data analysis for identifing factors contributing customer churn
* Creating Logistic model for predecting customer churn.
* Summarize findings and suggest strategy.

## key findings
Our comprehensive analysis has unveiled valuable insights into factors influencing churn, paving the way for targeted strategies to reduce customer attrition. Leveraging results from hypothesis tests and regression models, here are tailored strategies to mitigate churn: 

### Personalized Customer Engagement:  
* Insight: Customers with dependents exhibit lower churn rates.  
* Strategy: Develop personalized engagement initiatives, providing tailored services and offers to customers with dependents. Family-centric promotions and bundled services can enhance loyalty. 

### Loyalty Programs for Long-Term Customers:  
* Insight: Longer tenure is associated with reduced churn.  
* Strategy: Implement loyalty programs and exclusive benefits for customers with extended tenure. Acknowledge and reward their loyalty, fostering a sense of appreciation and commitment. 

### Enhance Communication for Senior Citizens:  
* Insight: Senior citizens are more likely to churn.
* Strategy:Improve communication channels with senior customers, ensuring clear and accessible information. Tailor marketing campaigns to address their unique needs and preferences.  

### Targeted Retention for Multiple Lines Users:  
* Insight: Customers with multiple lines are prone to churn.  
* Strategy: Implement targeted retention programs for customers with multiple lines, offering incentives, discounts, or exclusive services to enhance their satisfaction and reduce the likelihood of churn.  
 
### Continuous Monitoring and Feedback Loop:  
* Insight: Regularized logistic regression models provide stable and accurate predictions.
* Strategy: Establish a continuous monitoring system leveraging regularized models. Implement a feedback loop for real-time adjustments to customer engagement strategies based on evolving patterns and behaviours.  


By aligning strategies with these insights, we can proactively address the underlying causes of churn, enhance customer satisfaction, and fortify the company’s position in a competitive market.





