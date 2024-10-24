##Project B Check-in Code

#headers have been changed from original state to be easier to code
df <- read.csv("StudentStressFactors.csv") 
attach(df)

head(df)

plot(sleepQuality, performance)

model = lm(stressLevel ~ I((sleepQuality)^-1))
plot(model)
