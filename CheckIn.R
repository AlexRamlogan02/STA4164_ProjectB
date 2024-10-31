##Project B Check-in Code
library(car)
library(MASS)
library(olsrr)

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
