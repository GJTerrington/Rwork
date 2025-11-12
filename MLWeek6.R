library(tidyverse)
library(MASS)

data("iris") # Loads the iris dataset into a variable "iris"

iris[1:5,] # selects rows 1-5

iris <- iris[sample(1:nrow(iris)),] # shuffle thw rows of the dataframe in random order
iris[1:5,]

train_size = 0.7 # using 70% of the data for training

# this creates the training data frame
iris_train <- iris[1:(train_size*nrow(iris)),]

# this creates the testing data frame
iris_test <- iris[(nrow(iris_train)+1):nrow(iris),]

colours <- c('#1b9e77', '#d95f02', '#7570b3') # colours chosen to be visually ambiguous: see https://colorbrewer2.org

iris_train_colours <- colours[as.numeric(iris_train$Species)]

shapes <- c('o', '+', 'x')
iris_train_shapes <- shapes[as.numeric(iris_train$Species)]

# this creates a plot of species according to sepal width and sepal length
ggplot(
  data = iris_train,
  aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point(colour = iris_train_colours, shape = iris_train_shapes, size = 5)

binaryColours <- function(data, species) {
  tf_values <- data$Species == species
  colour_indices <- as.numeric(tf_values) + 1
  return(colours[colour_indices])
}

binaryShapes <- function(data, species) {
  tf_values <- data$Species == species
  shape_indices <- as.numeric(tf_values) + 1
  return(shapes[shape_indices])
}

# this makes virginica our species for binary classfication
binarySpecies = 'virginica'

# this creates a plot for binary classification of virginica
ggplot(
  data = iris_train,
  aes(x = Sepal.Length, y = Sepal.Width)
) + geom_point(
  colour = binaryColours(iris_train, binarySpecies),
  shape = binaryShapes(iris_train, binarySpecies),
  size = 5
)

# lets do the same but using petal length and width instead of sepal length/width
binarySpecies = 'virginica'
ggplot(
  data = iris_train,
  aes(x = Petal.Length, y = Petal.Width)
) + geom_point(
  colour = binaryColours(iris_train, binarySpecies),
  shape = binaryShapes(iris_train, binarySpecies),
  size = 5
)

# the same but with the other species, versicolor and setosa
binarySpecies = 'versicolor'
ggplot(
  data = iris_train,
  aes(x = Petal.Length, y = Petal.Width)
) + geom_point(
  colour = binaryColours(iris_train, binarySpecies),
  shape = binaryShapes(iris_train, binarySpecies),
  size = 5
)

binarySpecies = 'setosa'
ggplot(
  data = iris_train,
  aes(x = Petal.Length, y = Petal.Width)
) + geom_point(
  colour = binaryColours(iris_train, binarySpecies),
  shape = binaryShapes(iris_train, binarySpecies),
  size = 5
)


# this predicts whether a given sample is virginica or not
iris_train$binarySpecies <- iris_train$Species == 'virginica' # this will return either TRUE/FALSE

iris_train$binarySpecies <- iris_train$binarySpecies * 1 # convert from TRUE/FALSE to 1/0

iris_train[1:10,] # examine the data to make sure the binarySpecies label was set correctly

# now use binomial logistic regression for the classification (predicting yes/no variable)
# start by using petal length/width to predict the class
iris_binary_model <- glm(
  binarySpecies ~ Petal.Width + Petal.Length, # predicting the binarySpecies label using petal length/width
  family = binomial(link = 'logit'), # use a logistic regression
  data = iris_train
)

# to evaluate the model, we must calculate the continuous value on the logit curve using the predict method
binomial_probabilities <- predict(
  iris_binary_model,
  newdata = iris_test,
  type = 'response'
)

print(binomial_probabilities) # these are the estimated probabilities that each same is virginica

# now we need to convert these to discrete labels, which can be done by binarising at 0.5
binomial_predictions <- ifelse(
  binomial_probabilities > 0.5,
  1,
  0
)

print(binomial_predictions) # now we have discrete predictions, which we can compare to the model's discrete labels

# we will evaluate the model by accuracy
# first convert test set labels to binary
iris_test$binarySpecies <- iris_test$Species == 'virginica'
iris_test$binarySpecies <- iris_test$binarySpecies * 1 # convert from TRUE/FALSE to 1/0

# now calculate the error as the number of cases where we did not get the right label, and the accuracy as 1 minus that
binomial_classification_error <- mean(
  binomial_predictions != iris_test$binarySpecies
)

print(paste('Accuracy', 1 - binomial_classification_error))

# now moving onto decision tree

# load required packages

install.packages("caTools")
library(rpart)
library(caTools)
library(ggplot2)
library(dplyr)

# start building the tree using rpart
decision_tree_model <- rpart(
  binarySpecies ~ Petal.Width + Petal.Length,
  data = iris_train,
  method = "class"
)

# view the tree using plot and text
plot(decision_tree_model)
text(decision_tree_model)

# predicting the test dataset
decision_tree_predictions <- predict(
  decision_tree_model,
  newdata = iris_test,
  type = "class"
)

# evaluating the model
dt_classification_error <- mean(
  decision_tree_predictions != iris_test$binarySpecies
)

print(paste('Accuracy', 1 - dt_classification_error))

# to analyse this further, create a confusion matrix
confusion_matrix_dt <- table(iris_test$binarySpecies, decision_tree_predictions)
print(confusion_matrix_dt) # this prints a basic table showing how the predictions match the real values

# can create a visualtion with appropriate labels to make the matrix easier to understand
confusion_matrix_dt_df <- as.data.frame(confusion_matrix_dt)
colnames(confusion_matrix_dt_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_dt_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label")
+
  theme_minimal()

# multinomial logisitc regression
# can train a multinomial logistic regression using the multinom method of the nnet package
library(nnet)

iris_multinomial_model <- multinom(
  Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length, # predicting the multinomial Species label using all four features
  data = iris_train
)

mlr_predictions <- predict(
  iris_multinomial_model,
  newdata = iris_test,
  method = "class"
)

print(mlr_predictions)

# lets do a confusion matrix
confusion_matrix_mlr <- table(iris_test$Species, mlr_predictions)
print(confusion_matrix_mlr)

# to calculate accuracy, we can add all the diagonal elements (correctly predicted cases) divided by total observations
acc_mlr <- sum(diag(confusion_matrix_mlr)/sum(confusion_matrix_mlr))
print(paste('Accuracy', acc_mlr))

# create a ggplot visualisation of the confusion matrix
confusion_matrix_mlr_df <- as.data.frame(confusion_matrix_mlr)
colnames(confusion_matrix_mlr_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_mlr_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()

# decision tree for multi-class classification
decision_tree_multiclass_model <- rpart(
  Species ~ Petal.Width + Petal.Length,
  data = iris_train,
  method = "class"
)

plot(decision_tree_multiclass_model)
text(decision_tree_multiclass_model)

# again, use predict to create predictions
decision_tree_multiclass_predictions <- predict(
  decision_tree_multiclass_model,
  newdata = iris_test,
  type = "class"
  )

print(decision_tree_multiclass_predictions)

confusion_matrix_dt_mc <- table(iris_test$Species, decision_tree_multiclass_predictions)
print(confusion_matrix_dt_mc)

# to calculate accuracy, we can add all the diagonal elements (correctly predicted cases) divided by total observations
acc_dt_mc <- sum(diag(confusion_matrix_dt_mc)/sum(confusion_matrix_dt_mc))
print(paste('Accuracy', acc_dt_mc))

# create a ggplot visualisation of the confusion matrix
confusion_matrix_dtmc_df <- as.data.frame(confusion_matrix_dt_mc)
colnames(confusion_matrix_dtmc_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_dtmc_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()

# Random Forest
install.packages("randomForest")
library(randomForest)

# train model using randomForest
rf_model <- randomForest(
  Species ~ Petal.Width + Petal.Length,
  data = iris_train)

rf_model # Can see the mode, including number of trees, and the class error in the training data

# again, use predict to create predictions
rf_model_predictions <- predict(
  rf_model,
  newdata = iris_test
)

print(rf_model_predictions)

confusion_matrix_rf <- table(iris_test$Species, rf_model_predictions)
print(confusion_matrix_rf)

# to calculate accuracy, we can add all the diagonal elements (correctly predicted cases) divided by total observations
acc_rf <- sum(diag(confusion_matrix_rf)/sum(confusion_matrix_rf))
print(paste('Accuracy', acc_rf))

# create a ggplot visualisation of the confusion matrix
confusion_matrix_rf <- as.data.frame(confusion_matrix_rf)
colnames(confusion_matrix_rf) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_rf, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()


# K-Nearest Neighbours
library(class)

# as KNN works by measuring distance between instances, we must scale all the numerical features we want to use. can do this by using scale() and pass the columns that contain numerical features
# see distribution of variables prior to scaling
# notice how variables have different range of values
plot(density(iris_train$Sepal.Length)) # between 4-8
plot(density(iris_train$Sepal.Width)) # between 2-5
plot(density(iris_train$Petal.Length)) # between 0-8
plot(density(iris_train$Petal.Width)) # between 0-3

iris_train_scaled <- scale(iris_train[1:4])
iris_test_scaled <- scale(iris_test[1:4])

# see distribution of scaled variables
iris_train_scaled <- as.data.frame(iris_train_scaled)
plot(density(iris_train_scaled$Sepal.Length))
plot(density(iris_train_scaled$Sepal.Width))
plot(density(iris_train_scaled$Petal.Length))
plot(density(iris_train_scaled$Petal.Width))

# now that variables are scales, we can do predictions using KNN
knn_predictions <- knn(
  train = iris_train_scaled, # the scaled training data (numerical variables only and no labels)
  test = iris_test_scaled, # the scaled test data (numerical values only)
  cl = iris_train$Species, # the labels for the training data
  k = 5 # the value of k
)

# can evaluate the model using same metrics as before: confusion matrix and accuracy
confusion_matrix_knn <- table(iris_test$Species, knn_predictions)
confusion_matrix_knn

acc_knn_multiclass <- sum(diag(confusion_matrix_knn)/sum(confusion_matrix_knn))
print(paste('Accuracy', acc_knn_multiclass))




















