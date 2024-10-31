##Project B Check-in Code
library(car)
library(MASS)

#headers have been changed from original state to be easier to code
df <- read.csv("StudentStressFactors.csv") 
attach(df)
dim(df)

summary(df)
head(df)

pairs(df)

cor(df)

##model

full_model <- lm(stressLevel ~ ., data = df)
crPlots(full_model)

##All of the component + residual plots are approximately linear, and don't need any transformations


##assess assumptions
plot(full_model)

#######please look over here!! 

#look at leverage
tail(sort(hatvalues(full_model)), n=10) 

#look at cook's distance
tail(sort(cooks.distance(full_model)), n = 10) # no violation

#look at jackknife residual -> n - k - 2
t <- qt(.025, 520 - 5 -, lower.tail = FALSE)
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

