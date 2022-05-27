---
  title: "R Notebook"
output: html_notebook
---
  Required Libraries
```{r}
library(ISLR)
library(rpart)
library(caret)
library(cvms)
library(randomForest)
library(pROC)
library(gbm)
```


Splitting the data into training and testing data frames:
  ```{r}
set.seed(2021)
whole <- read.csv("life_expectancy_data.csv")
whole$Status = as.factor(whole$Status)

#removing all rows which contain one or more na value
whole = na.omit(whole)

index <- sample(1:nrow(whole), 1319)
who_train <- whole[index, ]
who_test <- whole[-index, ]
```

Use random forest to fit the training data:
  ```{r}
# Tuning The Random Forest Classifier
mtry = tuneRF(who_train[,-3], who_train[,3], ntreeTry=500, stepFactor=0.5, trace=F, plot=F)
best_mtry = mtry[mtry[,2] == min(mtry[,2]), 1]

# Random Forest Classifier
who.rf <- randomForest(Status ~ Country + Year + Life.expectancy + Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling, 
                       data = who_train,
                       mtry = 4
)
pred_who_rf <- predict(who.rf, newdata = who_test, type = 'prob')
pred_who_rf = as.data.frame(pred_who_rf)
```

How many inputs are used to splitting the tree at each node?
  ```{r}
who.rf$mtry
```

Evaluate the prediction performance through the testing set. 
Draw the ROC curve:
  ```{r}
g = roc(who_test$Status ~ pred_who_rf[, 1])
plot(g)
```
Calculate its AUC:
  ```{r}
g$auc
```
What is the misclassification rate?
  Data Manipulation
```{r}
pred_who_rf2 <- as.data.frame(ifelse(pred_who_rf$Developed > pred_who_rf$Developing, "Developed", "Developing"))
colnames(pred_who_rf2)[1] <- "Prediction"
rf_true_pred = cbind(who_test$Status, pred_who_rf2)
colnames(rf_true_pred)[1] <- "True"
```
Evaluating the classifier
```{r}
eval2 = evaluate(rf_true_pred,
                 target_col = "True",
                 prediction_col = "Prediction",
                 type = "binomial")
eval2
```
Misclassification Rate:
  ```{r}
rf_mis = 1-eval2$Accuracy
rf_mis
```

Confusion Matrix:
  ```{r}
conf_mat = eval2$'Confusion Matrix'[[1]]
conf_mat
plot_confusion_matrix(conf_mat)
```

Draw a variable importance plot.
```{r}
varImpPlot(who.rf,type = 2)
```
