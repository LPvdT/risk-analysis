# Dependencies
library(dplyr)
library(caret)
library(randomForest)
library(ROCR)
library(e1071)
library(kernlab)
library(rpart)
library(rpart.plot)

# Load in data transformed by descriptive process
credit.df <- read.csv("credit_dataset_transformed.csv", header = TRUE, sep = ",")
attach(credit.df)

## Function: factoring
to.factors <- function(df, variables) {
  
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  
  return(df)
}

## Function: normalize
scale.features <- function(df, variables) {
  
  for (variable in variables) {
    df[[variable]] <- scale(df[[variable]], center = TRUE, scale = TRUE)
  }
  
  return(df)
}

## Function: feature selection, using recursive feature elimination (and RF algorithm for model evaluation)
run.feature.selection <- function(num.iters = 20, feature.vars, class.var) {

  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv", verbose = FALSE, returnResamp = "all", number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var, sizes = variable.sizes, rfeControl = control)
  
  return(results.rfe)
}

## Function: visualise ROC curve
plot.roc.curve <- function(predictions, title.text){
  
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, main = title.text, cex.main = 1, cex.lab = 1, xaxs = "i", yaxs = "i")
  abline(0,1, col = "red")
  
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc, 2)
  
  legend(0.4, 0.4, legend = c(paste0("AUC: ", auc)), cex = 1, bty = "n", box.col = "white")
}

## Function: visualize Precision Recall curve
plot.pr.curve <- function(predictions, title.text){
  
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2, main = title.text, cex.main = 1, cex.lab = 1, xaxs = "i", yaxs = "i")
}

# Feature normalisation
numeric.vars <- c("credit.duration.months", "age", "credit.amount")

credit.df <- scale.features(credit.df, numeric.vars)

# Feature factorisation
categorical.vars <- c("credit.rating", "account.balance", "previous.credit.payment.status", "credit.purpose", "savings", "employment.duration", "installment.rate", "marital.status", "guarantor", "residence.duration", "current.assets", "other.credits", "apartment.type", "bank.credits", "occupation", "dependents", "telephone", "foreign.worker")

credit.df <- to.factors(credit.df, categorical.vars)

# Divide data: Train and Test in tuples by ratio 60:40
indices <- sample(1:nrow(credit.df), size = 0.6 * nrow(credit.df))
train.data <- credit.df[indices, ]
test.data <- credit.df[-indices, ]

# Run feature selections and print results
rfe.results <- run.feature.selection(feature.vars = train.data[, -1], class.var = train.data[, 1])

print(rfe.results)
varImp(rfe.results)

### Predictive: Logistic
test.feature.vars <- test.data[, -1]
test.class.var <- test.data[, 1]

formula.init <- as.formula("credit.rating ~ .") 

lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")
summary(lr.model)

lr.predictions <- predict(lr.model, test.data, type = "response") %>%
  round()

confusionMatrix(data = lr.predictions, reference = test.class.var, positive = "1")

# Logistic: feature selection
formula <- as.formula("credit.rating ~ .")

control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)

importance <- varImp(model, scale = FALSE)
plot(importance)

# Refine
formula.new <- as.formula("credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months")

lr.model.new <- glm(formula = formula.new, data = train.data, family = "binomial")

lr.predictions.new <- predict(lr.model.new, test.data, type = "response") %>%
  round()

confusionMatrix(data = lr.predictions.new, reference = test.class.var, positive = "1")

# Choose best fit: first model
lr.model.best <- lr.model

lr.prediction.values <- predict(lr.model.best, test.feature.vars, type = "response")
predictions <- prediction(lr.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "Logistic Regression ROC Curve")
plot.pr.curve(predictions, title.text = "Logistic Regression Precision/Recall Curve")

### Predictive: SVM
# Train
svm.model <- svm(formula = formula.init, data = train.data, kernel = "radial", cost = 100, gamma = 1)
summary(svm.model)

# Predict
svm.predictions <- predict(svm.model, test.feature.vars)
confusionMatrix(data = svm.predictions, reference = test.class.var, positive = "1")

# Feature selection SVM
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula.init, data = train.data, method = "svmRadial", trControl = control)

importance <- varImp(model, scale = FALSE)
plot(importance, cex.lab = 0.5)

# New SVM model based on feature selection
formula.new <- as.formula("credit.rating ~ account.balance + credit.duration.months + savings + previous.credit.payment.status + credit.amount")
svm.model.new <- svm(formula = formula.new, data = train.data, kernel = "radial", cost = 100, gamma = 1)

# Predict using new model: Accuracy down, specificity up, i.e. better model because poor credibility gets properly recognised.
svm.predictions.new <- predict(svm.model.new, test.feature.vars)
confusionMatrix(data = svm.predictions.new, reference = test.class.var, positive = "1")

# Model hyperparameter optimisation using a grid search algorithm
cost.weights <- c(0.1, 10, 100)
gamma.weights <- c(0.01, 0.25, 0.5, 1)

tuning.results <- tune(svm, formula.new, data = train.data, kernel = "radial", ranges = list(cost = cost.weights, gamma = gamma.weights))

print(tuning.results)
plot(tuning.results)

# Predict using best model: Better accuracy, specificity and sensitivity
svm.model.best <- tuning.results$best.model

svm.predictions.best <- predict(svm.model.best, test.feature.vars)
confusionMatrix(data = svm.predictions.best, reference = test.class.var, positive = "1")

# Plots
svm.predictions.best <- predict(svm.model.best, test.feature.vars, decision.values = T)
svm.prediction.values <- attributes(svm.predictions.best)$decision.values
predictions <- prediction(svm.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "SVM Optimised: ROC Curve")
plot.pr.curve(predictions, title.text = "SVM Optimised: Prediction/Recall Curve")

## Optimise based on AUROC
# Tranform data for handling
transformed.train <- train.data
transformed.test <- test.data

for (variable in categorical.vars) {
  
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}

transformed.train <- to.factors(df = transformed.train, variables = categorical.vars)
transformed.test <- to.factors(df = transformed.test, variables = categorical.vars)

transformed.test.feature.vars <- transformed.test[, -1]
transformed.test.class.var <- transformed.test[, 1]

# AUROC optimised model
grid <- expand.grid(C = c(1, 10, 100), sigma = c(0.01, 0.05, 0.1, 0.5, 1))

ctr <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

svm.roc.model <- train(formula.init, data = transformed.train, method = "svmRadial", trControl = ctr, tuneGrid = grid, metric = "ROC")

# Predictions: Again good accuracy, slightly decreased specificity but increase in sensitivity
predictions <- predict(svm.roc.model, transformed.test.feature.vars)
confusionMatrix(predictions, transformed.test.class.var, positive = "X1")

# Plots
svm.predictions <- predict(svm.roc.model, transformed.test.feature.vars, type = "prob")
svm.prediction.values <- svm.predictions[, 2]

predictions <- prediction(svm.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "SVM AUROC Optimised: ROC Curve")
plot.pr.curve(predictions, title.text = "SVM AUROC Optimised: Precision/Recall Curve")

### Predictive: Decision Trees
# Initial model
dr.control <- rpart.control(minsplit = 20, cp = 0.05)
dt.model <- rpart(formula = formula.init, method = "class", data = train.data, control = dr.control)

dt.predictions <- predict(dt.model, test.feature.vars, type = "class")
confusionMatrix(data = dt.predictions, reference = test.class.var, positive = "1")

# Feature selection
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula.init, data = train.data, method = "rpart", trControl = control)

importance <- varImp(model, scale = FALSE)
plot(importance)

# Feature selected model: Aggressive but classifies poor credit rating customers more efficiently
# Prior parameter used to weight: 0.7 to bad credit, 0.3 to good credit (adjust for density skewness)
formula.new <- as.formula("credit.rating ~ account.balance + savings + credit.amount + credit.duration.months + previous.credit.payment.status")

dt.model.new <- rpart(formula = formula.new, method = "class", data = train.data, control = dr.control, parms = list(prior = c(0.7, 0.3)))

dt.predictions.new <- predict(dt.model.new, test.feature.vars, type = "class")
confusionMatrix(data = dt.predictions.new, reference = test.class.var, positive = "1")

# Visualise best model
dt.model.best <- dt.model.new
print(dt.model.best)

par(mfrow = c(1, 1))

prp(dt.model.best, type = 1, extra = 3, varlen = 0, faclen = 0)

dt.predictions.best <- predict(dt.model.best, test.feature.vars, type = "prob")
dt.prediction.values <- dt.predictions.best[, 2]

predictions <- prediction(dt.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "Decision Tree: ROC Curve")
plot.pr.curve(predictions, title.text = "Decision Tree: Precision/Recall Curve")

### Predictive: Random Forest
rf.model <- randomForest(formula.init, data = train.data, importance = T, proximity = T)
print(rf.model)

rf.predictions <- predict(rf.model, test.feature.vars, type = "class")
confusionMatrix(data = rf.predictions, reference = test.class.var, positive = "1")

# Model with best features: Lower accuracy due to decrease in overfitting, better specificity, slightly lower sensitivity.
formula.new <- as.formula("credit.rating ~ account.balance + savings + credit.amount + credit.duration.months + previous.credit.payment.status")

rf.model.new <- randomForest(formula.new, data = train.data, importance = T, proximity = T)

rf.predictions.new <- predict(rf.model.new, test.feature.vars, type = "class")

confusionMatrix(data = rf.predictions.new, reference = test.class.var, positive = "1")

# Hyperparameter tuning using grid search
nodesize.vals <- c(2, 3, 4, 5)
ntree.vals <- c(200, 500, 1000, 2000)

tuning.results <- tune.randomForest(formula.new, data = train.data, mtry = 3, nodesize = nodesize.vals, ntree = ntree.vals)

print(tuning.results)

# Evaluate resulting best model: Very slight performance increase
rf.model.best <- tuning.results$best.model
rf.predictions.best <- predict(rf.model.best, test.feature.vars, type = "class")

confusionMatrix(data = rf.predictions.best, reference = test.class.var, positive = "1")

# Evaluation plots
rf.predictions.best <- predict(rf.model.best, test.feature.vars, type = "prob")
rf.prediction.values <- rf.predictions.best[, 2]

predictions <- prediction(rf.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "Random Forest: ROC Curve")
plot.pr.curve(predictions, title.text = "Random Forest: Precision/Recall Curve")

### Predictive: Neural Networks
# Uses earlier encoded data tranformations (i.e. 1 -> X01)
nn.model <- train(formula.init, data = transformed.train, method = "nnet")
print(nn.model)

# Decent accuracy with decent accuracy and specificity
nn.predictions <- predict(nn.model, transformed.test.feature.vars, type = "raw")
confusionMatrix(data = nn.predictions, reference = transformed.test.class.var, positive = "X1")

# Important feature selection
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula.init, data = transformed.train, method = "nnet", trControl = control)

importance <- varImp(model, scale = FALSE)
plot(importance)

# New model based on important features
formula.new <- as.formula("credit.rating ~ foreign.worker + previous.credit.payment.status + account.balance + employment.duration + guarantor + credit.purpose + savings")

nn.model.new <- train(formula.new, data = transformed.train, method = "nnet")

nn.predictions.new <- predict(nn.model.new, transformed.test.feature.vars, type = "raw")
confusionMatrix(data = nn.predictions.new, reference = transformed.test.class.var, positive = "X1")

# Visualisation of internal hyperparameter tuning
plot(nn.model.new, cex.lab = 0.5)

# Best model: Second model
nn.model.best <- nn.model.new

nn.predictions.best <- predict(nn.model.best, transformed.test.feature.vars, type = "prob")
nn.prediction.values <- nn.predictions.best[, 2]
predictions <- prediction(nn.prediction.values, test.class.var)

par(mfrow = c(1, 2))

plot.roc.curve(predictions, title.text = "Neural Network: ROC Curve")
plot.pr.curve(predictions, title.text = "Neural Network: Precision/Recall Curve")
