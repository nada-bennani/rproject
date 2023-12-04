#a
library(lme4)
library(ggplot2)
data(sleepstudy)
#b
str(sleepstudy)
head(sleepstudy)
#c
# Visualize the distribution of Reaction time
hist(sleepstudy$Reaction, main = "Distribution of Reaction Time", xlab = "Reaction Time")
#Scatterplot to visualize the relationship between Days and Reaction time
plot(sleepstudy$Days, sleepstudy$Reaction, main = "Scatterplot of Reaction Time vs Days", xlab = "Days", ylab = "Reaction Time") 
#descriptive_statistics
#a
describe(sleepstudy[, c("Days", "Reaction")])
summary(sleepstudy$Days)
#These statistics describe the distribution of the "Days" variable. 
#The median value of 4.5 suggests that the middle observation falls around the 4.5th day.
summary(sleepstudy$Reaction)
#These statistics describe the distribution of the "Reaction" variable, representing reaction times. The median reaction time is 288.7
#, and the mean reaction time is 298.5.
summary(sleepstudy$Subject)
#There are 18 unique subjects (308, 309, 310, 330, 331, ..., 372) in the dataset, each with 10 observations.
# Histogram of reaction times
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Reaction Times", x = "Reaction Time", y = "Frequency")
ggplot(sleepstudy, aes(x = Reaction, fill = as.factor(Days))) +
  geom_histogram(binwidth = 10, position = "dodge", alpha = 0.7, color = "black") +
  labs(title = "Grouped Histogram of Reaction Times by Days", x = "Reaction Time", y = "Frequency") +
  theme_minimal()
# Density plot for reaction times by days
ggplot(sleepstudy, aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Times by Days", x = "Reaction Time", y = "Density") +
  theme_minimal()
# Boxplot of reaction times by days
ggplot(sleepstudy, aes(x = as.factor(Days), y = Reaction)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Reaction Times Over Different Days", x = "Days", y = "Reaction Time") 
#3model
#In this case, you may want to model the relationship between reaction time 
#(Reaction) and days (Days) while considering the repeated measures for each 
#subject 
#A linear mixed-effects model (LMM) is a suitable approach for this type of data.
sleep_model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(sleep_model)
#interpretation 
#Intercept : The estimated baseline reaction time is 251.405 milliseconds. 
#This represents the expected reaction time when the number of days is zero.
#Days : The estimated change in reaction time per day is 10.467 milliseconds. 
#The positive coefficient suggests that, on average, reaction time increases 
#by approximately 10.467 milliseconds for each additional day.
#subject (intercept) :The estimated variance (612.10) represents the degree of this variability. 
#The standard deviation (24.741) indicates the typical amount of deviation of 
#individual subject.
#subject (days):The estimated variance (35.07) and standard deviation (5.922) 
#quantify the extent of this variability. 

# Extract residuals from the model
residuals <- resid(sleep_model)
hist(residuals)

# Check for normality of residuals
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
qqline(residuals)
#Points closely follow the diagonal line.so they are normally distributed.
plot(fitted(sleep_model), residuals, main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
