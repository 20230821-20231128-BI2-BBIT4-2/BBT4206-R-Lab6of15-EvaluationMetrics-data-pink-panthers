# STEP 1. Install and Load the Required Packages ----
## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
# 1. Accuracy and Cohen's Kappa ----
## 1.a. Load the dataset ----
stock_ror_dataset <- read_csv(
  "data/transforms/dow_jones_index.csv",
  col_types = cols(
    stock = col_factor(
      levels = c(
        "AA",
        "AXP",
        "BA",
        "BAC",
        "CAT",
        "CSCO",
        "CVX",
        "DD",
        "DIS",
        "GE",
        "HD",
        "HPQ",
        "IBM",
        "INTC",
        "JNJ",
        "JPM",
        "KRFT",
        "KO",
        "MCD",
        "MMM",
        "MRK",
        "MSFT",
        "PFE",
        "PG",
        "T",
        "TRV",
        "UTX",
        "VZ",
        "WMT",
        "XOM"
      )
    ),
    date = col_date(format = "%m/%d/%Y")
  )
)
summary(stock_ror_dataset)

stock_no_na <- na.omit(stock_ror_dataset)

View(stock_no_na)


## 1.b. Determine the Baseline Accuracy ----
stock_freq <- stock_no_na$stock
cbind(frequency=
        table(stock_freq),
                 percentage = prop.table(table(stock_freq)) * 100)


## 1.c. Split the dataset ----

train_index <- createDataPartition(stock_no_na$stock,
                                   p = 0.75,
                                   list = FALSE)
stock_train <- stock_no_na[train_index, ]
stock_test <- stock_no_na[-train_index, ]

## 1.d. Train the Model ----


train_control <- trainControl(method = "cv", number = 5)

set.seed(7)
stock_model_rpart <-
  train(stock ~ ., data = stock_train, method = "rpart",
        metric = "Accuracy", trControl = train_control)

## 1.e. Display the Model's Performance ----
print(stock_model_rpart)

# 2. RMSE, R Squared, and MAE ----
## 2.a. Load the dataset ----
library(readr)
Customer_Churn <- read_csv("data/Customer Churn.csv")
View(Customer_Churn)

## 2.b. Split the dataset ----
set.seed(7)

train_index <- sample(1:dim(Customer_Churn)[1], 10) # nolint: seq_linter.
customer_train <- Customer_Churn[train_index, ]
customer_test <- Customer_Churn[-train_index, ]

## 2.c. Train the Model ----
train_control <- trainControl(method = "boot", number = 1000)

customer_model_lm <-
  train(Churn ~ ., data = customer_train,
        na.action = na.omit, method = "lm", metric = "RMSE",
        trControl = train_control)

## 2.d. Display the Model's Performance ----
print(customer_model_lm)


### Option 2: Compute the metric yourself using the test dataset ----
predictions <- predict(customer_model_lm, customer_test[, 1:14])

print(predictions)

#### RMSE ----
rmse <- sqrt(mean((customer_test$Churn - predictions)^2))
print(paste("RMSE =", rmse))

#### SSR ----
# SSR is the sum of squared residuals (the sum of squared differences
# between observed and predicted values)
ssr <- sum((customer_test$Churn - predictions)^2)
print(paste("SSR =", ssr))


#### SST ----
# SST is the total sum of squares (the sum of squared differences
# between observed values and their mean)
sst <- sum((customer_test$Churn - mean(customer_test$Churn))^2)
print(paste("SST =", sst))

#### R Squared ----
# We then use SSR and SST to compute the value of R squared
r_squared <- 1 - (ssr / sst)
print(paste("R Squared =", r_squared))

#### MAE ----
absolute_errors <- abs(predictions - customer_test$Churn)
mae <- mean(absolute_errors)
print(paste("MAE =", mae))

# 3. Area Under ROC Curve ----
## 3.a. Load the dataset ----
library(readr)
defaulter_dataset <-
  readr::read_csv(
    "data/default of credit card clients.csv",
    col_types = cols(
      SEX = col_factor(levels = c("1", "2")),
      EDUCATION = col_factor(levels = c("0", "1", "2", "3", "4", "5", "6")),
      MARRIAGE = col_factor(levels = c("0", "1", "2", "3")),
      `default payment next month` = col_factor(levels = c("1", "0")),
      `default payment next month` = col_factor(levels = c("1", "0"))
    ),
    skip = 1
  )


## 3.b. Determine the Baseline Accuracy ----
defaulter_freq <- defaulter_dataset$`default payment next month`
cbind(frequency=
        table(defaulter_freq),
      percentage = prop.table(table(defaulter_freq)) * 100)


## 3.c. Split the dataset ----
train_index <- createDataPartition(defaulter_dataset$`default payment next month`,
                                   p = 0.75,
                                   list = FALSE)
defaulter_train <- defaulter_dataset[train_index, ]

defaulter_test <- defaulter_dataset[-train_index, ]

## 3.d. Train the Model ----
# We apply the 10-fold cross validation resampling method
train_control <- trainControl(method = "cv", number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
set.seed(7)
defualter_model_knn <-
  train( `default payment next month` ~ ., data = defaulter_train, method = "knn",
        metric = "ROC", trControl = train_control)

print(defaulter_model_knn)

### Option 2: Compute the metric yourself using the test dataset ----
#### Sensitivity and Specificity ----
predictions <- predict(defaulter_model_knn, defaulter_test[, 1:25])

print(predictions)
confusion_matrix <-
  caret::confusionMatrix(predictions,
                         defaulter_test[, 1:25]$`default payment next month`)

print(confusion_matrix)

predictions <- predict(defaulter_model_knn, defaulter_test[, 1:25],
                       type = "prob")

roc_curve <- roc(defualter_test$`default payment next month`, predictions$neg)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for KNN Model", print.auc = TRUE,
     print.auc.x = 0.6, print.auc.y = 0.6, col = "blue", lwd = 2.5)

# 4. Logarithmic Loss (LogLoss) ----
## 4.a. Load the dataset ----
stock_ror_dataset <- read_csv(
  "data/transforms/dow_jones_index.csv",
  col_types = cols(
    stock = col_factor(
      levels = c(
        "AA",
        "AXP",
        "BA",
        "BAC",
        "CAT",
        "CSCO",
        "CVX",
        "DD",
        "DIS",
        "GE",
        "HD",
        "HPQ",
        "IBM",
        "INTC",
        "JNJ",
        "JPM",
        "KRFT",
        "KO",
        "MCD",
        "MMM",
        "MRK",
        "MSFT",
        "PFE",
        "PG",
        "T",
        "TRV",
        "UTX",
        "VZ",
        "WMT",
        "XOM"
      )
    ),
    date = col_date(format = "%m/%d/%Y")
  )
)
summary(stock_ror_dataset)

stock_no_na <- na.omit(stock_ror_dataset)

## 4.b. Train the Model ----
# We apply the 5-fold repeated cross validation resampling method
# with 3 repeats
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss)
set.seed(7)

stock_model_cart <- train(stock ~ ., data = stock_no_na, 
                          method = "rpart",
                         metric = "logLoss", trControl = train_control)
## 4.c. Display the Model's Performance ----
print(stock_model_cart)
