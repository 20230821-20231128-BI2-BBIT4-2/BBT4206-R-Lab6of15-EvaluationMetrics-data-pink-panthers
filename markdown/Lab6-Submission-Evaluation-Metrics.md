Business Intelligence Project
================

- [Business Intelligence Lab Submission
  Markdown](#business-intelligence-lab-submission-markdown)
- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Step 1: Install and Load Packages](#step-1-install-and-load-packages)
- [Step 2: Accuracy and Cohen’s
  Kappa](#sec-step-3-load-dataset-and-description)
  - [Load Dataset](#load-dataset)
  - [Determine the Baseline Accuracy](#determine-the-baseline-accuracy)
  - [Split the dataset](#split-the-dataset)
  - [Train the Model](#train-the-model)
  - [Display the Model’s Performance](#display-the-models-performance)
- [Step 2: RMSE, R Squared, and MAE](#step-2-rmse-r-squared-and-mae)
  - [Load the dataset](#load-the-dataset)
  - [Split the dataset](#split-the-dataset-1)
  - [Train the Model](#train-the-model-1)
  - [Test the Model](#test-the-model)
  - [Display the Model’s Performance](#display-the-models-performance-1)
  - [Option 2: Compute the metric yourself using the test
    dataset](#option-2-compute-the-metric-yourself-using-the-test-dataset)
  - [RMSE](#rmse)
  - [SSR](#ssr)
  - [SST](#sst)
  - [R Squared](#r-squared)
  - [MAE](#mae)
- [Step 3: Area Under ROC Curve](#step-3-area-under-roc-curve)
  - [Load the dataset](#load-the-dataset-1)
  - [Determine the Baseline
    Accuracy](#determine-the-baseline-accuracy-1)
  - [Split the dataset](#split-the-dataset-2)
  - [Train the Model](#train-the-model-2)
  - [Option 2: Compute the metric yourself using the test
    dataset](#option-2-compute-the-metric-yourself-using-the-test-dataset-1)
  - [Plot the ROC curve](#plot-the-roc-curve)
- [Step 4: Logarithmic Loss (LogLoss)](#step-4-logarithmic-loss-logloss)
  - [Load the dataset](#load-the-dataset-2)
  - [Train the Model](#train-the-model-3)
  - [Display the Model’s Performance](#display-the-models-performance-2)

# Business Intelligence Lab Submission Markdown

<Data Pink Panthers> \<23/10/23\>

# Student Details

<table style="width:99%;">
<colgroup>
<col style="width: 43%" />
<col style="width: 38%" />
<col style="width: 17%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Student ID Numbers and Names of Group Members</strong></td>
<td><p>| 1. 137315 - C - Yashvi Bhadania</p>
<p>| 2. 134668 - C - June Ndinda Mutiso</p>
<p>| 3. 135227 - C - Innocent Mbuvi</p>
<p>| 4. 134253 - C - Uzair Farooq</p>
<p>| 5. 135109 - C - Jackson Kaburu</p></td>
<td></td>
</tr>
<tr class="even">
<td></td>
<td><strong>GitHub Classroom Group Name</strong></td>
<td>Data Pink Panthers</td>
</tr>
<tr class="odd">
<td><strong>Course Code</strong></td>
<td>BBT4206</td>
<td></td>
</tr>
<tr class="even">
<td><strong>Course Name</strong></td>
<td>Business Intelligence II</td>
<td></td>
</tr>
<tr class="odd">
<td><strong>Program</strong></td>
<td>Bachelor of Business Information Technology</td>
<td></td>
</tr>
<tr class="even">
<td><strong>Semester Duration</strong></td>
<td>21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023</td>
<td></td>
</tr>
</tbody>
</table>

# Setup Chunk

**Note:** the following “*KnitR*” options have been set as the defaults
in this markdown:  
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy.opts = list(width.cutoff = 80), tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

``` r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    warning = FALSE,
    collapse = FALSE,
    tidy = TRUE
)
```

------------------------------------------------------------------------

**Note:** the following “*R Markdown*” options have been set as the
defaults in this markdown:

> output:
>
> github_document:  
> toc: yes  
> toc_depth: 4  
> fig_width: 6  
> fig_height: 4  
> df_print: default
>
> editor_options:  
> chunk_output_type: console

# Step 1: Install and Load Packages

We start by installing all the required packages

``` r
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
```

# Step 2: Accuracy and Cohen’s Kappa

### Load Dataset

``` r
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
```

### Determine the Baseline Accuracy

``` r
## 1.b. Determine the Baseline Accuracy ----
stock_freq <- stock_no_na$stock
cbind(frequency=
        table(stock_freq),
                 percentage = prop.table(table(stock_freq)) * 100)
```

### Split the dataset

``` r
train_index <- createDataPartition(stock_no_na$stock,
                                   p = 0.75,
                                   list = FALSE)
stock_train <- stock_no_na[train_index, ]
stock_test <- stock_no_na[-train_index, ]
```

### Train the Model

``` r
train_control <- trainControl(method = "cv", number = 5)

set.seed(7)
stock_model_rpart <-
  train(stock ~ ., data = stock_train, method = "rpart",
        metric = "Accuracy", trControl = train_control)
```

### Display the Model’s Performance

``` r
print(stock_model_rpart)
```

# Step 2: RMSE, R Squared, and MAE

### Load the dataset

``` r
library(readr)
Customer_Churn <- read_csv("data/Customer Churn.csv")
View(Customer_Churn)
```

### Split the dataset

``` r
set.seed(7)

train_index <- sample(1:dim(Customer_Churn)[1], 10) # nolint: seq_linter.
customer_train <- Customer_Churn[train_index, ]
customer_test <- Customer_Churn[-train_index, ]

  
```

### Train the Model

``` r
train_control <- trainControl(method = "boot", number = 1000)

customer_model_lm <-
  train(Churn ~ ., data = customer_train,
        na.action = na.omit, method = "lm", metric = "RMSE",
        trControl = train_control)
```

### Test the Model

``` r
#STEP 4: Testing the model
 predictions_lm <- predict(PimaIndiansDiabetes_model_lm,
                           PimaIndiansDiabetes_test[, 1:9])
```

### Display the Model’s Performance

``` r
print(customer_model_lm)
```

### Option 2: Compute the metric yourself using the test dataset

``` r
predictions <- predict(customer_model_lm, customer_test[, 1:14])

print(predictions)
```

### RMSE

``` r
rmse <- sqrt(mean((customer_test$Churn - predictions)^2))
print(paste("RMSE =", rmse))
```

### SSR

``` r
ssr <- sum((customer_test$Churn - predictions)^2)
print(paste("SSR =", ssr))
```

### SST

``` r
sst <- sum((customer_test$Churn - mean(customer_test$Churn))^2)
print(paste("SST =", sst))
```

### R Squared

``` r
r_squared <- 1 - (ssr / sst)
print(paste("R Squared =", r_squared))
```

### MAE

``` r
absolute_errors <- abs(predictions - customer_test$Churn)
mae <- mean(absolute_errors)
print(paste("MAE =", mae))
```

# Step 3: Area Under ROC Curve

### Load the dataset

``` r
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
```

### Determine the Baseline Accuracy

``` r
defaulter_freq <- defaulter_dataset$`default payment next month`
cbind(frequency=
        table(defaulter_freq),
      percentage = prop.table(table(defaulter_freq)) * 100)
  
```

### Split the dataset

``` r
train_index <- createDataPartition(defaulter_dataset$`default payment next month`,
                                   p = 0.75,
                                   list = FALSE)
defaulter_train <- defaulter_dataset[train_index, ]

defaulter_test <- defaulter_dataset[-train_index, ]
```

### Train the Model

``` r
train_control <- trainControl(method = "cv", number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
set.seed(7)
defualter_model_knn <-
  train( `default payment next month` ~ ., data = defaulter_train, method = "knn",
        metric = "ROC", trControl = train_control)

print(defaulter_model_knn)
```

### Option 2: Compute the metric yourself using the test dataset

``` r
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
```

### Plot the ROC curve

``` r
plot(roc_curve, main = "ROC Curve for KNN Model", print.auc = TRUE,
     print.auc.x = 0.6, print.auc.y = 0.6, col = "blue", lwd = 2.5)
```

# Step 4: Logarithmic Loss (LogLoss)

### Load the dataset

``` r
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
   
```

### Train the Model

``` r
# We apply the 5-fold repeated cross validation resampling method
# with 3 repeats
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss)
set.seed(7)

stock_model_cart <- train(stock ~ ., data = stock_no_na, 
                          method = "rpart",
                         metric = "logLoss", trControl = train_control)
```

### Display the Model’s Performance

``` r
print(stock_model_cart)
   
```
