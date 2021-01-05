library(caret)
library(tidyverse)

# Data Setup
data(iris)
iris_data <- iris

# Data Validation
validation_index <- createDataPartition(iris_data$Species, p = 0.8, list = FALSE)

validation <- iris_data[-validation_index]
iris_data <- iris_data[validation_index,]

# Summarising the Data
dim(iris_data)

sapply(iris_data, class)

head(iris_data)

levels(iris_data$Species)

summary(iris_data)

# Basic Plots
x <- iris_data[,1:4]

y <- iris_data[,5]

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
  data=iris_data, 
  method="lda", 
  metric=metric, 
  trControl=control
)

# Classification and Regression Trees

set.seed(7)
fit.cart <- train(
  Species~., 
  data=iris_data, 
  method="rpart", 
  metric=metric, 
  trControl=control
)

# K-Nearest Neighbors

set.seed(7)
fit.knn <- train(
  Species~., 
  data=iris_data, 
  method="knn", 
  metric=metric, 
  trControl=control
)

# Support Vector Machines

set.seed(7)
fit.svm <- train(
  Species~., 
  data=iris_data, 
  method="svmRadial", 
  metric=metric, 
  trControl=control
)

# Random Forest

set.seed(7)
fit.rf <- train(
  Species~., 
  data=iris_data, 
  method="rf", 
  metric=metric, 
  trControl=control
)


