#FINAL PROJECT - Group: Dhruv, Dhruvi & Farrukh

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(janitor)
library(pacman)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(ISLR)
library(ROCR)
library(caret)
library(usmap)
library(scales)
library(cowplot)
library(glmnet)

churn_df <- read.csv("Telco_customer_churn.csv")
head(churn_df)

str(churn_df)
summary(churn_df)

# Check for missing values
sum(is.na(churn_df))
#Getting 11 missing values

#Pre-analysis of data.

# Check unique values for categorical variables
unique(churn_df$Internet.Service)
unique(churn_df$Contract)
unique(churn_df$state)

#conclusion: -> we can drop country from analysis as there is only one country i.e uited states
#there are 3 types of internet services available namely : "DSL","Fiber Optics","No"
#there are 3 types of contract available :-> "Month-to-month", "Two year", "One year" 

# Boxplot of numerical variables
boxplot(churn_df$Total.Charges, main = "Boxplot of Numerical Variables")
ggplot(data=churn_df,aes(x=Monthly.Charges))+
  geom_histogram()
#box plot suggest there is no visible outlier for total charges and average price lies around 1000.
#histogram of monthly price suggest the data is not normally distributed ,there is highest volume of data with price around 20 and then as monthly price gets higher data tends to follow normal distribuation.

# Remove rows with missing values
churn_df <- churn_df[complete.cases(churn_df), ]
churn_df <- clean_names(churn_df)
colnames(churn_df)

# 1. Convert "Yes" and "No" to 1 and 0 for Monthly Charges Prediction
monthly_charges_data <- churn_df %>%
  select(city,state,latitude,longitude,phone_service, monthly_charges, tenure_months, internet_service, contract, online_security, online_backup, device_protection, tech_support,multiple_lines,contract,)

monthly_charges_data <- monthly_charges_data %>%
  mutate(
    phone_service = as.numeric(phone_service == "Yes"),
    online_security = as.numeric(online_security == "Yes"),
    online_backup = as.numeric(online_backup == "Yes"),
    device_protection = as.numeric(device_protection == "Yes"),
    tech_support = as.numeric(tech_support == "Yes")
  )

monthly_charges_data$internet_service <- as.factor(monthly_charges_data$internet_service)
monthly_charges_data$contract <- as.factor(monthly_charges_data$contract)


# 2. Convert "Yes" and "No" to 1 and 0 for Customer Churn Prediction
churn_prediction_data <- churn_df %>%
  select(city,state,latitude,longitude,churn_value, gender, senior_citizen, partner, dependents, tenure_months, phone_service, multiple_lines, online_backup, device_protection)

churn_prediction_data <- churn_prediction_data %>%
  mutate(
    gender = as.numeric(gender == "Male"),  # Assuming Male is 1 and Female is 0
    senior_citizen = as.numeric(senior_citizen == "Yes"),
    partner = as.numeric(partner == "Yes"),
    dependents = as.numeric(dependents == "Yes"),
    phone_service = as.numeric(phone_service == "Yes"),
    multiple_lines = as.numeric(multiple_lines == "Yes"),
    online_backup = as.numeric(online_backup == "Yes"),
    device_protection = as.numeric(device_protection == "Yes")
  )

churn_prediction_data$gender <- as.factor(churn_prediction_data$gender)
churn_prediction_data$senior_citizen <- as.factor(churn_prediction_data$senior_citizen)
churn_prediction_data$partner <- as.factor(churn_prediction_data$partner)
churn_prediction_data$dependents <- as.factor(churn_prediction_data$dependents)
# Can add more for other columns

# Check updated summary statistics
summary(churn_prediction_data)

############################# QUESTION 1 EDA ####################################
# Univariate Analysis - Monthly Charges Distribution
ggplot(monthly_charges_data, aes(x = monthly_charges)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Monthly Charges Distribution", x = "Monthly Charges", y = "Frequency")+
  theme_bw()

# Bivariate Analysis - Monthly Charges vs. Tenure Months


# Categorical Variables - Internet Service Distribution
mean_price_for_internet_service=monthly_charges_data %>%
  group_by(internet_service) %>%
  summarise(avg_monthly_price=mean(monthly_charges))

ggplot(mean_price_for_internet_service, aes(x = internet_service,y=avg_monthly_price, fill = internet_service)) +
  geom_col() +
  labs(title = "Internet Service Distribution", x = "Internet Service", y = "Average charges for Internet service")
#there is a huge impact of type of internet service person using ,if plan is provided by fiber optics it is tend to be most expensive followed by DSL.However no internet plans are cheapest which make sens.

#does city  play role in monthly pricing ?
#need to plot data and explore.


#does tenure of plan affects price ?
unique(monthly_charges_data$tenure_months)
monthly_charges_data$tenure_months_bins<-cut(monthly_charges_data$tenure_months,
                                             breaks=c(0,12,24,48,100),
                                             labels=c('Less then one year', 'Less then two year','less-then 3 years','more then 3 years'))

monthly_charges_for_tenur<-monthly_charges_data %>%
  group_by(tenure_months_bins)%>%
  summarise(mean_charges=mean(monthly_charges))

ggplot(monthly_charges_for_tenur,aes(x=tenure_months_bins,y=mean_charges))+
  geom_col(fill="blue")+
  theme_bw()+
  labs(x="Tenure of plan", y="mean price for each Tenure",title="Mean price for Each tenure")
#Form graph it is evident as you opt for longer term you tend to pay more charge.

#analyzing contract type's influence on monthly price.
monthly_charges_for_contract<-monthly_charges_data %>%
  group_by(contract)%>%
  summarise(mean_charges=mean(monthly_charges))

ggplot(monthly_charges_for_contract,aes(x=contract,y=mean_charges))+
  geom_col(fill="blue")+
  theme_bw()+
  labs(x="Contract of plan", y="mean price for Contract",title="Mean price for contract")

#As the length of contract increases the monthly price decreases.

#simple encoding for categorical variables.
encoded_internet_services <- model.matrix(~ 0 + internet_service, data = monthly_charges_data)
encoded_contract<-model.matrix(~ 0 + contract, data = monthly_charges_data)
encoded_multiple_lines<-model.matrix(~ 0 + multiple_lines, data = monthly_charges_data)

monthly_charges_data<-cbind(monthly_charges_data,encoded_internet_services,encoded_contract,encoded_multiple_lines)
monthly_charges_data<-subset(monthly_charges_data,select=-c(internet_service,multiple_lines,contract,tenure_months_bins,latitude,longitude,city,state))


# Correlation Analysis
correlation_matrix <- cor(monthly_charges_data)
print(correlation_matrix)

#corrplot(correlation_matrix, method = "number", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(correlation_matrix)


# Linear Regression Model
linear_model <- lm(monthly_charges ~ ., data = monthly_charges_data)

# Summary of the Model
summary(linear_model)

#Advancing the model 
#####################################    Q-1    ##########################################

# Splitting data into training (70%) and test (30%)
set.seed(123)
split_index <- createDataPartition(monthly_charges_data$monthly_charges, p = 0.7, list = FALSE)
train_data_monthly_charges <- monthly_charges_data[split_index, ]
test_data_monthly_charges <- monthly_charges_data[-split_index, ]

#######################HYPOTHESIS TESTING FOR Q1 ###############################

# Create a one-way ANOVA
anova_result <- aov(monthly_charges ~ internet_serviceDSL, data = train_data_monthly_charges)

# Display ANOVA results
print(summary(anova_result))

# Choose significance level
alpha <- 0.05

# Check significance
if (summary(anova_result)[[1]]$"Pr(>F)"[1] < alpha) {
  cat("Reject the null hypothesis. Internet Service Type significantly affects monthly charges.\n")
} else {
  cat("Fail to reject the null hypothesis. Internet Service Type doesn't significantly affect monthly charges.\n")
}
################################################################################

clean_names(monthly_charges_data)

linear_model_trainData<-lm(monthly_charges~.,train_data_monthly_charges)
summary(linear_model_trainData)

# Residual plot
plot(linear_model_trainData, which = 1)

# QQ plot
plot(linear_model_trainData, which = 2)

# Predict monthly charges using the test set
predictions_monthlyCharges <- predict(linear_model_trainData,
                                      newdata = test_data_monthly_charges)

# Evaluate the model's performance (e.g., mean squared error)
mse <- mean((test_data_monthly_charges$monthly_charges - predictions_monthlyCharges)^2)
print(paste("Mean Squared Error: ", mse))

# Predicted vs. actual plot
plot(test_data_monthly_charges$monthly_charges, predictions_monthlyCharges,
     main = "Predicted vs. Actual Monthly Charges",
     xlab = "Actual Monthly Charges",
     ylab = "Predicted Monthly Charges")
abline(0, 1, col = "red", lty = 2)

######### PERFORMANCE METRICS FOR MODEL 1 #########




#MODEL 2
# Corrected model formula with high significant attributes.
linear_model_trainData <- lm(monthly_charges ~ tenure_months + internet_serviceDSL + `internet_serviceFiber optic`+ 
                               `contractMonth-to-month`  + 
                               online_security + online_backup + device_protection + tech_support + 
                               multiple_linesNo + `multiple_linesNo phone service` ,
                             data = train_data_monthly_charges)

# Summary of the Model
summary(linear_model_trainData)


# Predict monthly charges using the test set
predictions_monthlyCharges <- predict(linear_model_trainData,
                                      newdata = test_data_monthly_charges)

# Evaluate the model's performance (e.g., mean squared error)
mse <- mean((test_data_monthly_charges$monthly_charges - predictions_monthlyCharges)^2)
print(paste("Mean Squared Error: ", mse))

###############################################################################

#MODEL 3
############################### LASSO ##########################################
library(glmnet)

# Prepare the training data
train_data_monthly_charges <- as.data.frame(train_data_monthly_charges)  # Ensure data is in a data frame format

# Extract the response variable and predictors
response_variable <- train_data_monthly_charges$monthly_charges
predictors <- train_data_monthly_charges[, !colnames(train_data_monthly_charges)
                                         %in% c("monthly_charges")]

# Fit Lasso regression model
lasso_model <- cv.glmnet(as.matrix(predictors), response_variable, alpha = 1)
print(lasso_model)
plot(lasso_model)

# Make predictions on the test set
lasso_predictions <- predict(lasso_model, s = "lambda.min", newx = as.matrix(test_data_monthly_charges[, !colnames(test_data_monthly_charges) %in% c("monthly_charges")]))

# Evaluate the model
lasso_mse <- mean((lasso_predictions - test_data_monthly_charges$monthly_charges)^2)
print(paste("Lasso Mean Squared Error: ", lasso_mse))

#################################################################################

########################### Model 4 RIDGE #######################################

# Fit Ridge regression model
ridge_model <- cv.glmnet(as.matrix(predictors), response_variable, alpha = 0)
plot(ridge_model)

# Make predictions on the test set
ridge_predictions <- predict(ridge_model,
                             s = "lambda.min",
                             newx = as.matrix(test_data_monthly_charges[, !colnames(test_data_monthly_charges)
                                                                        %in% c("monthly_charges")]))

# Evaluate the model
ridge_mse <- mean((ridge_predictions - test_data_monthly_charges$monthly_charges)^2)
print(paste("Ridge Mean Squared Error: ", ridge_mse))

#summary with results 
#       Model                                 MSE 
#lm with all variables                 44.7363834316337
#lm with selected features             44.7309350372958
#Lasso regression model                44.7028731182513
#Ridge regression model                46.3520439247711
###############################################################################


############################# QUESTION 2 EDA ###################################

# Univariate Analysis - Churn Value Distribution
ggplot(churn_prediction_data, aes(x = factor(churn_value), fill = factor(churn_value))) +
  geom_bar() +
  labs(title = "Churn Value Distribution", x = "Churn Value", y = "Count")

# does gender is critical factor for churn ?
#male=1
churn_prediction_data_gender_ratio=churn_prediction_data %>% 
  group_by(gender,churn_value) %>%
  summarise(churn_count=n())
churn_prediction_data_gender_ratio

print("female churn rate : 26.95%")
print("male churn rate : 26.20%")
#there is no visible impact of gender on churn .

#does senior citizen is churning more ?
churn_prediction_data_senior_citizen_ratio=churn_prediction_data %>% 
  group_by(senior_citizen,churn_value) %>%
  summarise(churn_count=n())
churn_prediction_data_senior_citizen_ratio

#does dependent or having partner impact churn ?

#male senior citizen churn rate= ((476/(476+666))*100 )
print("senior citizen churn rate : 41.68%")
print("young citizen churn rate : 23.65%")
#senior citizen are more likely to churn.

churn_prediction_data$churn_value<-as.factor(churn_prediction_data$churn_value)
plot1<-ggplot(churn_prediction_data, aes(x=dependents,fill=churn_value))+
  geom_bar(position = 'fill')+
  theme_bw()
plot2<-ggplot(churn_prediction_data, aes(x=partner,fill=churn_value))+
  geom_bar(position = 'fill')+
  theme_bw()

churn_prediction_data$tenure_months_bins<-cut(churn_prediction_data$tenure_months,
                                              breaks=c(0,12,24,48,100),
                                              labels=c('Less then one year', 'Less then two year','less-then 3 years','more then 3 years'))

plot3<-ggplot(churn_prediction_data, aes(x=tenure_months_bins,fill=churn_value))+
  geom_bar(position = 'fill')+
  theme_bw()

plot4<-ggplot(churn_prediction_data, aes(x=device_protection,fill=churn_value))+
  geom_bar(position = 'fill')+
  theme_bw()

plot_grid(plot1,plot2,plot3,plot4)

#Conclusion
#People with dependent are very less likely to churn
#People with partner are less likely to churn
#As tenure increases the churn rate constantly decreases
#Offering device protection didn't effectively reduce churn

# Splitting data into training (70%) and test (30%)
set.seed(123)
split_index <- createDataPartition(churn_prediction_data$churn_value, p = 0.7, list = FALSE)
train_data_churn <- churn_prediction_data[split_index, ]
test_data_churn <- churn_prediction_data[-split_index, ]

##################### HYPOTHESIS TESTING FOR Q2 ################################

#Null Hypothesis(H0) : churn rate is not affected by gender. 
#Alternate Hypothesis(H1) : churn rate is affected by gender.

# Create a contingency table
contingency_table <- table(train_data_churn$gender, train_data_churn$churn_value)

# Perform Chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# Display results
cat("Chi-squared test statistic:", chi_squared_test$statistic, "\n")
cat("P-Value:", chi_squared_test$p.value, "\n")

# Choose significance level
alpha <- 0.05

# Check significance
if (chi_squared_test$p.value < alpha) {
  cat("Reject the null hypothesis.gender is a significent factor in churn rate.\n")
} else {
  cat("Fail to reject the null hypothesis. Gender dosen't affect the churn rate.\n")
}

############################# PHONE SERVICE TEST ############################### 

# Null Hypothesis(H0) : Churn rate is not affected by phone service.
# Alternate Hypothesis(H1) : Churn rate is affected by phone service.

# Create a contingency table
contingency_table_phone <- table(train_data_churn$phone_service, train_data_churn$churn_value)

# Perform Chi-squared test
chi_squared_test_phone <- chisq.test(contingency_table_phone)

# Display results
cat("Chi-squared test statistic:", chi_squared_test_phone$statistic, "\n")
cat("P-Value:", chi_squared_test_phone$p.value, "\n")

# Choose significance level
alpha <- 0.05

# Check significance
if (chi_squared_test_phone$p.value < alpha) {
  cat("Reject the null hypothesis. Phone service is a significant factor in churn rate.\n")
} else {
  cat("Fail to reject the null hypothesis. Phone service doesn't affect the churn rate.\n")
}

################################################################################

# Logistic Regression Model
logistic_model <- glm(churn_value ~ gender + senior_citizen + partner + dependents + tenure_months + 
                        phone_service + multiple_lines + online_backup + device_protection, 
                      data = train_data_churn, family = "binomial")
# Summary of the Model
summary(logistic_model)

#predicting output on test data set.
predictions <- predict(logistic_model,
                             newx = as.matrix(test_data_churn[, !colnames(test_data_churn)
                                                                        %in% c("churn_value")]))
predictions_boolean= as.factor(ifelse(predictions>= 0.5,1,0))

cm<-confusionMatrix(predictions_boolean,train_data_churn$churn_value)
model1_accuracy<-cm$overall[1]
print(model1_accuracy)

### METRICS ###

# Predictions (binary)
predictions <- ifelse(predict(logistic_model, newdata = test_data_churn, type = "response") >= 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(predictions, test_data_churn$churn_value)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
cat("Recall (Sensitivity):", recall, "\n")

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1-score:", f1_score, "\n")

#################################################################################
#Model 2

logistic_model_updated <- glm(churn_value ~ senior_citizen + partner + dependents +
                        tenure_months+ multiple_lines +device_protection+online_backup, 
                      data = train_data_churn, family = "binomial")
# Summary of the Model
summary(logistic_model_updated)

#predicting outputs
predictions <- predict(logistic_model_updated,
                       newx = as.matrix(test_data_churn[, !colnames(test_data_churn)
                                                        %in% c("churn_value")]))
predictions_boolean= as.factor(ifelse(predictions>= 0.5,1,0))

cm<-confusionMatrix(predictions_boolean,train_data_churn$churn_value)
model2_accuracy<-cm$overall[1]
model2_accuracy

#### METRICS
# Predictions (binary)
predictions_2 <- ifelse(predict(logistic_model_updated, newdata = test_data_churn, type = "response") >= 0.5, 1, 0)

# Confusion Matrix
conf_matrix_2 <- table(predictions_2, test_data_churn$churn_value)

# Accuracy
accuracy_2 <- sum(diag(conf_matrix_2)) / sum(conf_matrix_2)
cat("Accuracy:", accuracy_2, "\n")

# Precision
precision_2 <- conf_matrix_2[2, 2] / sum(conf_matrix_2[, 2])
cat("Precision:", precision_2, "\n")

# Recall (Sensitivity)
recall_2 <- conf_matrix_2[2, 2] / sum(conf_matrix_2[2, ])
cat("Recall (Sensitivity):", recall_2, "\n")

# F1-score
f1_score_2 <- 2 * (precision_2 * recall_2) / (precision_2 + recall_2)
cat("F1-score:", f1_score_2, "\n")

#################################################################################
#Model 3

churn_prediction_data<-subset(churn_prediction_data,select=-c(tenure_months_bins,city,state,latitude,longitude))
colnames(churn_prediction_data)

set.seed(123)
training.samples <- churn_prediction_data$churn_value %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- churn_prediction_data[training.samples, ]
test.data <- churn_prediction_data[-training.samples, ]

# Dumy code categorical predictor variables
x <- model.matrix(churn_value~., train.data)[,-1]
# Convert the outcome (class) to a numerical variable
y <- train.data$churn_value

set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

plot(cv.lasso)

# Make prediction on test data
x.test <- model.matrix(churn_value ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

#confusion matrix
cm<-confusionMatrix(as.factor(predicted.classes),test.data$churn_value)
model3_accuracy<-cm$overall[1]
model3_accuracy

#################################################################################
#model 4

set.seed(123) 
cv.ridge<- cv.glmnet(x, y, alpha = 0, family = "binomial")
model <- glmnet(x, y, alpha = 0, family = "binomial",
                lambda = cv.ridge$lambda.min)

plot(cv.ridge)

# Make prediction on test data
x.test <- model.matrix(churn_value ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

#confusion matrix
cm<-confusionMatrix(as.factor(predicted.classes),test.data$churn_value)
model4_accuracy<-cm$overall[1]
model4_accuracy

#Summary of results 

#       Model                          Accuracy 
#lm with all variables                0.7587327 
#lm with selected features            0.7575142
#Lasso regression model               0.7686833 
#Ridge regression model               0.7516014 

#############################################################

lasso_coefficients <- coef(cv.lasso, s = "lambda.min")
top_lasso_predictors <- rownames(lasso_coefficients)[order(abs(lasso_coefficients), decreasing = TRUE)]
print(top_lasso_predictors)

ridge_coefficients <- coef(cv.ridge, s = "lambda.min")
top_ridge_predictors <- rownames(ridge_coefficients)[order(abs(ridge_coefficients), decreasing = TRUE)]
print(top_ridge_predictors)
