library(caret)
library(tidyverse)

# Data Setup
data(iris)
dataset <- iris

# Data Validation
validation_index <- createDataPartition(dataset$Species, p = 0.8, list = FALSE)

validation <- dataset[-validation_index]
dataset <- dataset[validation_index,]

# Summarising the Data
dim(dataset)

sapply(dataset, class)

head(dataset)

levels(dataset$Species)

summary(dataset)

# Basic Plots
x <- dataset[,1:4]

y <- dataset[,5]

# Distribution of Attributes Box Plot
par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(x[i], main = names(iris)[i])
  }

plot(y)

# Multivariate plots

featurePlot(x=x, y=y, plot = 'ellipse')

featurePlot(x=x, y=y, plot = 'box')

# density plots for each attribute by class value

scales <- list(x=list(relation="free"), y=list(relation="free"))

featurePlot(x=x, y=y, plot="density", scales=scales)


# Algorithms

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Linear Discriminant Analysis

set.seed(7)
fit.lda <- train(
  Species~., 
  data=dataset, 
  method="lda", 
  metric=metric, 
  trControl=control
)

# Classification and Regression Trees

set.seed(7)
fit.cart <- train(
  Species~., 
  data=dataset, 
  method="rpart", 
  metric=metric, 
  trControl=control
)

# K-Nearest Neighbors

set.seed(7)
fit.knn <- train(
  Species~., 
  data=dataset, 
  method="knn", 
  metric=metric, 
  trControl=control
)

# Support Vector Machines

set.seed(7)
fit.svm <- train(
  Species~., 
  data=dataset, 
  method="svmRadial", 
  metric=metric, 
  trControl=control
)

# Random Forest

set.seed(7)
fit.rf <- train(
  Species~., 
  data=dataset, 
  method="rf", 
  metric=metric, 
  trControl=control
)

# Model Selection

results <- resamples(list(
   lda=fit.lda, 
   cart=fit.cart, 
   knn=fit.knn, 
   svm=fit.svm, 
   rf=fit.rf
))

summary(results)

dotplot(results)

print(fit.lda)

# Predictions

predictions <- predict(fit.lda, validation)

confusionMatrix(predictions, validation$Species)
