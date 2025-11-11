library(tidyverse)
library(MASS)
library(rgl)

data("hills") # Loads the hills dataset into a variable "hills"

nrow(hills) # checks the number of rows

hills_train <- hills[1:30,] # This sets rows 1-30 into a training dataset
hills_test <- hills[31:35,] # This sets rows 31-35 into a test dataset

hills_train[1:10,] # This returns the first 10 rows

summary(hills_train) # This returns the basic statistics of the training dataset

# This creates a scatterplot comparing dist to time. Sanity check to look visually at the relationship
ggplot(
  data = hills_train,
  aes(x = dist, y= time)
) + geom_point()

# This creates a scatterplot comparing climb to time
ggplot(
  data = hills_train,
  aes(x = climb, y= time)
) + geom_point()

# Use cor.test to run a quick correlation analysis between dist and time
cor.test(
  hills_train$dist,
  hills_train$time
)

# Use cor.test to run a quick correlation analysis between climb and time
cor.test(
  hills_train$climb,
  hills_train$time
)

# fit a model for using dist to predict time, and save the model object into a variable
mod_dist <- lm(
  formula = time~dist, # predicting time using dist
  data = hills_train
)

summary(mod_dist) # The summary() function provides lots of information about the model including the values of the intercept 𝑏𝑏 (-2.5138) and the slope 𝑎𝑎 (7.9264). The slope is the average increase in time for every one-unit increase in dist (negative would signal a decrease). So the model or equation the lm() function has produced (based on using Ordinary Least Squares to find the best fit line) is: time = −2.5138 + (7.9264 ∗ dist).

#  to see just the coefficients of the model (i.e. a and b) without the additional reporting from summary(), you can use the coef() function
coef(mod_dist)

# model for using climb to predict time
mod_climb <- lm(
  formula = time~climb, # predicting the time using climb
  data = hills_train
)

summary(mod_climb) # a higher r-squared = higher model quality. climb produces a lower one than dist

# draw the regression line on the graph of the data points. The geom_abline() function in ggplot2 defines a straight line using the slope and intercept, which we’ll extract from the model using the coef function.
coefs_dist <- coef(mod_dist)

ggplot(
  data = hills_train,
  aes(x = dist, y = time),
) +
  geom_point() +
  geom_abline(mapping = aes(
    slope = coefs_dist["dist"],
    intercept = coefs_dist["(Intercept)"]
  ), colour = 'red')

# This gives you an overall sense of how well the model fits the data: here we see it follows the trend line but leaves a lot of variance not accounted for. To see the remaining variance in more detail, we can graph the residuals of the model. Remember that these are the difference between the predicted value for a sample (based on the independent variables) and the actual value (of the dependent variable). We can first calculate the predicted values and the residuals for each input sample using the predict() and residuals() functions in R.

hills_resid <- hills_train # make a copy of the dataset, to leave the original untouched

hills_resid$predicted <- predict(mod_dist) # if data are not specified, uses the data the model was fit to

hills_resid$residuals <- residuals(mod_dist)

# show the data with predicted and residual values
hills_resid[1:10,]

# show the residuals for illustrative purposes with the same scatterplot of the data by graphing the predicted values and showing the residuals using the geom_segment() command
ggplot(
  data = hills_resid,
  aes(x = dist, y = time)
) +
  geom_point(size = 3) + # make the actual values show up more clearly
  geom_point(size = 2, aes(y = predicted), shape = 1) + # show the predicted values
  geom_segment(aes(xend = dist, yend = predicted), alpha = 0.9, colour = 'red') +
  geom_abline(mapping = aes(
    slope = coefs_dist["dist"],
    intercept = coefs_dist["(Intercept)"]
  ), colour = 'grey')

# can plot them much more directly using one of the diagnostic plots that R automatically generates for linear regression models. To do so, we’ll use the plot() function (which=1 indicates which of the diagnostic plots to show, here the residuals)
plot(
  mod_dist,
  which = 1
)
# Instead of using either dist or time directly, this plot graphs the predicted value for each point (i.e., climbing up the line) against its calculated residual. Now we can see very clearly that Bens of Jura and Knock Hill are very much outliers in these data

coefs_climb <- coef(mod_climb)

ggplot(
  data = hills_train,
  aes(x = climb, y = time),
) +
  geom_point() +
  geom_abline(mapping = aes(
    slope = coefs_climb["climb"],
    intercept = coefs_climb["(Intercept)"]
  ), colour = 'red')

# This gives you an overall sense of how well the model fits the data: here we see it follows the trend line but leaves a lot of variance not accounted for. To see the remaining variance in more detail, we can graph the residuals of the model. Remember that these are the difference between the predicted value for a sample (based on the independent variables) and the actual value (of the dependent variable). We can first calculate the predicted values and the residuals for each input sample using the predict() and residuals() functions in R.

hills_resid$predicted <- predict(mod_climb) # if data are not specified, uses the data the model was fit to

hills_resid$residuals <- residuals(mod_climb)

# show the data with predicted and residual values
hills_resid[1:10,]

# show the residuals for illustrative purposes with the same scatterplot of the data by graphing the predicted values and showing the residuals using the geom_segment() command
ggplot(
  data = hills_resid,
  aes(x = climb, y = time)
) +
  geom_point(size = 3) + # make the actual values show up more clearly
  geom_point(size = 2, aes(y = predicted), shape = 1) + # show the predicted values
  geom_segment(aes(xend = climb, yend = predicted), alpha = 0.9, colour = 'red') +
  geom_abline(mapping = aes(
    slope = coefs_climb["climb"],
    intercept = coefs_climb["(Intercept)"]
  ), colour = 'grey')

# can plot them much more directly using one of the diagnostic plots that R automatically generates for linear regression models. To do so, we’ll use the plot() function (which=1 indicates which of the diagnostic plots to show, here the residuals)
plot(
  mod_climb,
  which = 1
)


# Now that we’ve fitted our model, we want to use it to predict hill climb times for new hills (that is, hills we didn’t observe when fitting the model). To do so, we’ll use R’s predict() function, which we can pass new, unseen data to
predict(
  mod_dist,
  newdata = hills_test
)

# To get a sense of how reliable these predictions are we can include calculating the 95% confidence intervals around these predictions, using the interval argument
predict(
  mod_dist,
  newdata = hills_test,
  interval = 'confidence'
)

# Now, we actually know what the true values are for these points. That means we can see how well our model is actually doing on these new samples by calculating the residuals ourselves, as the difference between the predictions and the true values
hills_dist_test <- hills_test # make a copy of the test data to leave the original unaltered
hills_dist_test$predicted <- predict(mod_dist, newdata = hills_dist_test)
hills_dist_test$residual <- hills_dist_test$predicted - hills_dist_test$time
hills_dist_test

# Although the model doesn’t predict the correct value for these mountains, the predictions are all just about within the 95% confidence interval we calculated above, so the model fit is pretty strong! To see this a different way, we can graph the residuals like we did with the training dat
ggplot(
  data = hills_dist_test,
  aes(x = dist, y = time)
) +
  geom_point(size = 3) + # make the actual values show up more clearly
  geom_point(size = 2, aes(y = predicted), shape = 1) + # show the predicted values
  geom_segment(aes(xend = dist, yend = predicted), alpha = 0.9, colour = 'red') +
  geom_abline(mapping = aes(
    slope = coefs_dist["dist"],
    intercept = coefs_dist["(Intercept)"]
  ), colour = "grey")

# to get a single overall number we can use for comparing different models in terms of how well they predict the test data, we’ll calculate the sum of squared errors (SSE).
sse_dist <- sum(hills_dist_test$residual**2)
sse_dist


# Now that we’ve fitted our model, we want to use it to predict hill climb times for new hills (that is, hills we didn’t observe when fitting the model). To do so, we’ll use R’s predict() function, which we can pass new, unseen data to
predict(
  mod_climb,
  newdata = hills_test
)

# To get a sense of how reliable these predictions are we can include calculating the 95% confidence intervals around these predictions, using the interval argument
predict(
  mod_climb,
  newdata = hills_test,
  interval = 'confidence'
)

# Now, we actually know what the true values are for these points. That means we can see how well our model is actually doing on these new samples by calculating the residuals ourselves, as the difference between the predictions and the true values
hills_climb_test <- hills_test # make a copy of the test data to leave the original unaltered
hills_climb_test$predicted <- predict(mod_climb, newdata = hills_climb_test)
hills_climb_test$residual <- hills_climb_test$predicted - hills_climb_test$time
hills_climb_test

# Although the model doesn’t predict the correct value for these mountains, the predictions are all just about within the 95% confidence interval we calculated above, so the model fit is pretty strong! To see this a different way, we can graph the residuals like we did with the training dat
ggplot(
  data = hills_climb_test,
  aes(x = climb, y = time)
) +
  geom_point(size = 3) + # make the actual values show up more clearly
  geom_point(size = 2, aes(y = predicted), shape = 1) + # show the predicted values
  geom_segment(aes(xend = climb, yend = predicted), alpha = 0.9, colour = 'red') +
  geom_abline(mapping = aes(
    slope = coefs_climb["climb"],
    intercept = coefs_climb["(Intercept)"]
  ), colour = "grey")

# to get a single overall number we can use for comparing different models in terms of how well they predict the test data, we’ll calculate the sum of squared errors (SSE).
sse_climb <- sum(hills_climb_test$residual**2)
sse_climb

# multi-variate linear regression
# plot dist and climb together (ignoring time for now), to see if they look colinear
ggplot(
  data = hills_train,
  aes(x = dist, y = climb)
) + geom_point()

# There’s some relationship, but also a lot of scatter–that’s good! That means that adding them together can be helpful. Let’s run a quick correlation test to make sure
cor.test(hills_train$dist, hills_train$climb)
# A correlation of 0.523 says they’re definitely correlated, but they’re also definitely not perfect predictors of each other.

# Let’s now plot dist and climb together with time to get a sense for how they relate
library(rgl)
plot3d(
  x = hills_train$dist,
  y = hills_train$climb,
  z = hills_train$time
)

rglwidget() # this should open an interactive 3-d widget in the Viewer pane

#  now, let’s train the multivariate regression. We still use the lm() command, but now indicate multiple predictor variables:
mod_hills <- lm(
  formula = time~climb + dist, # predicting time using both climb AND dist
  data = hills_train
)

# Since we’re only using two predictor variables, we can still visualise the regression model, but now as a 2-D plane instead of a line
plot3d(
  x = hills_train$dist,
  y = hills_train$climb,
  z = hills_train$time,
  type = 's', size = 2, col = 'blue' # show the data points as big blue spheres for visibility
)
coefs <- coef(mod_hills)
planes3d(a = coefs["dist"], b = coefs["climb"], c = -1, d = coefs["(Intercept)"], col = 'grey') # this uses the model coefficients to plot the regression plane in 3-d space
rglwidget()

layout(matrix(1:3, ncol=3))
plot(mod_dist, which=1) # `dist`-based model
plot(mod_climb, which=1) # `climb`-based model
plot(mod_hills, which=1) # multivariate model
