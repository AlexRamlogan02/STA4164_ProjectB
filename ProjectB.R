##Project B Code
library(car)
library(MASS)
library(olsrr)
library(Metrics)

#headers have been changed from original state to be easier to code
df <- read.csv("StudentStressFactors.csv") 
attach(df)
dim(df)

summary(df)
pairs(df)

cor(df)

##model

full_model <- lm(stressLevel ~ ., data = df)
plot(full_model)
crPlots(full_model)

##All of the component + residual plots are approximately linear, and don't need any transformations


# Fit a new model with the transformed variable
full_model <- lm( stressLevel ~ sleepQuality + studyLoad + extracurriculars + I(log(sleepQuality * studyLoad)^2), data = df)
boxplot(sqrt_model)

crPlots(full_model)


#####Residual Analysis
plot(full_model)
par(mfrow = c(1, 1))
#look at leverage
leverageval <- (2 * 4)/520 
print(leverageval)
tail(sort(hatvalues(full_model))) 
leverage <- hatvalues(model)
violationLeverage <- which(leverage > leverageval)
print(length(violationLeverage)) #35 in violation!!!


#look at cook's distance
tail(sort(cooks.distance(full_model)), n = 10) # no violation

#look at jackknife residual -> n - k - 2
t <- qt(.025, 520 - 4 - 2, lower.tail = FALSE)
print(t)

jackKnife <- sort(studres(full_model)) 
violationjackKnife <- which(abs(jackKnife) > t)

print(violationjackKnife) ##5 in violation

#jackknife
head(sort(studres(full_model)), n=20) #Print 20 lowest values, none in violation
tail(sort(studres(full_model)), n=20) #Print 20 highest values, 5 in violation

#leverage
print(leverageval)
tail(sort(hatvalues(full_model)), n=) #Print 10 largest values, 6 in violation

# Shapiro-Wilk Test for Normality
shapiro.test(full_model$residuals) #violated -> check residuals

boxCox(full_model)

residual_plotting <- function(residuals){
  
  m <- mean(residuals)
  s <- sd(residuals)
  hist_data <- hist(residuals, breaks=24)
  x_values <- seq(min(residuals), max(residuals), length = 200)
  y_values <- dnorm(x_values, mean = mean(residuals), sd = sd(residuals)) 
  y_values <- y_values * diff(hist_data$mids[1:2]) * length(residuals) 
  lines(x_values, y_values, lwd = 2)
}
residual_plotting(full_model$residuals)


altered_model <- lm(I(sqrt(stressLevel)) ~ sleepQuality + studyLoad + extracurriculars + I(log(sleepQuality * studyLoad)^2), data = df) #none of the options look good
boxCox(altered_model)

residual_plotting(altered_model$residuals)

# Shapiro-Wilk Test for Normality
shapiro.test(altered_model$residuals) #still in violation, but we will move on anyways

full_model <- altered_model

#Co-linearity 
options(scipen = 20)
ols_coll_diag(full_model)

#variable 5 is in violation, so remove 
altered_model <- lm(I(sqrt(stressLevel)) ~ sleepQuality + studyLoad + extracurriculars, data = df)
ols_coll_diag(altered_model)

#VIF is ok, and CI is now ok!
full_model <- altered_model

#set seed
set.seed(1)
full <- df

#Observastion numbers
#Using 70% for training
train=sample(1:nrow(full),nrow(full)*0.70) 
test=(-train)

#testing and training split

train_data <- full[train, ]
test_data <- full[test, ]
print(train_data)
print(test_data)

# Selection Types
attach(train_data)

final_model <- full_model
model_all <- ols_step_all_possible(full_model)
head(model_all[order(model_all$adjr, decreasing = T),])
#plot(model_all)
head(model_all[order(model_all$aic, decreasing = F),])

#Backwards Selection with p-value as the criteria
model_backward <- ols_step_backward_p(final_model,prem = 0.1 ,details=TRUE)
print(model_backward)
plot(model_backward)

#using AIC
model_backward2 <- ols_step_backward_aic(full_model,details=TRUE)
print(model_backward2)


######### Forwards Selection
model_forward <- ols_step_forward_p(full_model, penter = 0.1, details=TRUE)
print(model_forward)

#Use ols_step_forward_aic to use AIC instead of p-values.
model_forward_AIC <- ols_step_forward_aic(full_model, details=TRUE)
print(model_forward_AIC)
plot(model_forward_AIC)


######### Piecewise Selection
model_step1 <- ols_step_both_p(full_model, pent = 0.1, prem = 0.3, details=T)
print(model_step1)

model_step2 <- ols_step_both_aic(full_model, details=T)
print(model_step2)


#check reliability and model fit
df2 <- train_data[,-c(2)]
df2
head(df2)
final_model <- lm(I(sqrt(stressLevel)) ~ studyLoad + sleepQuality, data = df2)

summary(final_model)

#y-hats
pred_train <- predict(final_model, train_data)
mse(train_data$stressLevel, pred_train)

pred_test <- predict(final_model, test_data)
mse(test_data$stressLevel, pred_test)
 
###train MSE > test MSE -> overfitting -> reduce training size
set.seed(123)
full <- df

#Observastion numbers
#Using 50% for training
train=sample(1:nrow(full),nrow(full)*0.50) 
test=(-train)

#testing and training split

train_data <- full[train, ]
test_data <- full[test, ]
print(train_data)
print(test_data)

# Selection Types
attach(train_data)

final_model <- full_model
model_all <- ols_step_all_possible(full_model)

#Backwards Selection with p-value as the criteria
model_backward <- ols_step_backward_p(final_model,prem = 0.1 ,details=TRUE)
print(model_backward)
plot(model_backward)

#using AIC
model_backward2 <- ols_step_backward_aic(full_model,details=TRUE)
print(model_backward2)


######### Forwards Selection
model_forward <- ols_step_forward_p(full_model, penter = 0.1, details=TRUE)
print(model_forward)

#Use ols_step_forward_aic to use AIC instead of p-values.
model_forward_AIC <- ols_step_forward_aic(full_model, details=TRUE)
print(model_forward_AIC)
plot(model_forward_AIC)


######### Piecewise Selection
model_step1 <- ols_step_both_p(full_model, pent = 0.1, prem = 0.3, details=T)
print(model_step1)

model_step2 <- ols_step_both_aic(full_model, details=T)
print(model_step2)


#check reliability and model fit
df2 <- train_data[,-c(2)]
df2
head(df2)
final_model <- lm(I(sqrt(stressLevel)) ~ studyLoad + sleepQuality, data = df2)

summary(final_model)

#y-hats
pred_train <- predict(final_model, train_data)
mse(train_data$stressLevel, pred_train)

pred_test <- predict(final_model, test_data)
mse(test_data$stressLevel, pred_test)