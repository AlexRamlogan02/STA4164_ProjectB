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

#####Residual Analysis
plot(full_model)

#look at leverage
tail(sort(hatvalues(full_model)), n=10) 

#look at cook's distance
tail(sort(cooks.distance(full_model)), n = 10) # no violation

#look at jackknife residual -> n - k - 2
t <- qt(.025, 520 - 5 - 2, lower.tail = FALSE)
print(t)

head(sort(studres(full_model)), n=20) #Print 20 lowest values, 7 in violation
tail(sort(studres(full_model)), n=20) #Print 20 highest values, 15 in violation

shapiro.test(full_model$residuals) #not violated -> normality is good


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

altered_model <- lm(I(stressLevel) ~ ., data = df) #none of the options look good

residual_plotting(altered_model$residuals)

#Co-linearity 

options(scipen = 20)
ols_coll_diag(full_model)

#VIF is ok!

#set seed
set.seed(123)
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

final_model <- lm(stressLevel ~ headaches + sleepQuality + studyLoad + extracurriculars + I(headaches * extracurriculars), data = train_data)
model_all <- ols_step_all_possible(final_model)

print(model_all)

#Backwards Selection with p-value as the criteria
model_backward <- ols_step_backward_p(final_model,prem = 0.1 ,details=TRUE)
print(model_backward)

#check reliability and model fit
df2 <- train_data[,-c(2)]
head(df2)

final_model2 <- lm(stressLevel ~ headaches + sleepQuality + studyLoad + extracurriculars + I(headaches * extracurriculars), data = df2)
summary(final_model)

#y-hats
pred_train <- predict(final_model2, train_data)
mse(train_data$stressLevel, pred_train)

pred_test <- predict(final_model2, test_data)
mse(test_data$stressLevel, pred_test)
 
###train MSE approximately equal test MSE