data = SleepStudy

## Data Visualization:

# 1.a. Comparative Boxplot of Alcoholic Drinks per Week by Class Standing

# Convert ClassYear to factor with labels
data$ClassYear = factor(data$ClassYear, levels = 1:4, labels = c("Freshman", "Sophomore", "Junior", "Senior"))

# Construct box plot
windows()
plot1 = boxplot(Drinks ~ ClassYear, data = data,
        main = "Alcoholic Drinks per Week by Class Standing",
        xlab = "Class Standing",
        ylab = "Number of Drinks per Week",
        col = c("lightblue", "lightgreen", "lightyellow", "lightpink"),
        border = "darkgray")
plot1$stats

# 1.b. Comparative Boxplot of Overall GPA by Gender

# Convert Gender to character (0=female, 1=male)
data$Gender = as.character(data$Gender)

# Construct box plot
windows()
plot2 = boxplot(GPA ~ Gender, data = data,
        main = "Overall GPA by Gender",
        xlab = "Gender",
        ylab = "GPA",
        col = c("lightpink", "lightblue"),
        names = c("Female = 0", "Male = 1"),
        border = "darkgray")
# plot2$stats

#############################################

## Hypothesis Testing:

# 2. Stress Level by Gender

# Subset the data by gender
MaleSleep=subset(data, data$Gender=="1") 
FemaleSleep=subset(data, data$Gender=="0")

alpha = 0.05

# 2.a. Testing the claim that the stress level for female and male students is different
# Perform t-test
t.test(FemaleSleep$StressScore, MaleSleep$StressScore, alternative= "two.sided", conf.level=1-alpha)

# 2.b. Testing the claim that female students, on average, sleep more on weekdays than their male counterparts
# Perform t-test
t.test(FemaleSleep$WeekdaySleep, MaleSleep$WeekdaySleep, alternative="greater", conf.level=1-alpha)

#############################################

### Simple Linear Regression:

# 3. Stress Score Predicted by Anxiety Score

# Construct scatter plot
windows()
plot(data$AnxietyScore, data$StressScore, main="Stress Score vs. Anxiety Score", xlab="Anxiety Score", ylab="Stress Score")

# Create model
model_slr1 = lm(StressScore ~ AnxietyScore, data=data)

# Add regression line
abline(model_slr1, col="lightpink", lwd=2)  # cor(data$AnxietyScore, data$StressScore) # 0.6980524

# Regression analysis
summary(model_slr1)

#############################################
# 4 Stress Score Predicted by Depression Score

# Construct scatter plot
windows()
plot(data$DepressionScore, data$StressScore, main="Stress Score vs. Depression Score", xlab="Depression Score", ylab="Stress Score")

# Create model
model_slr2 = lm(StressScore ~ DepressionScore, data=data)

# Add regression line
abline(model_slr2, col="lightpink", lwd=2) # cor(data$DepressionScore, data$StressScore)   # 0.6219619

# Regression analysis
summary(model_slr2)

#############################################

### Multiple Linear Regression:

# 5. Stress Score Predicted by Happiness, Weekday Sleep, and Weekend Sleep

# Create model
model_mlr1 = lm(StressScore ~ Happiness + WeekdaySleep + WeekendSleep, data = data)

# Regression analysis
summary(model_mlr1)

#############################################

# 6. Stress Score Predicted by Anxiety and Depression

# Create model
model_mlr2 = lm(StressScore ~ AnxietyScore + DepressionScore, data = data)

# Regression analysis
summary(model_mlr2)

#############################################

### Chi-Square for Categorical Variables:

# 7. Association between Gender and Alcohol Use

# Create contingency table
table1 = table(  
  "Gender" = ifelse(data$Gender == 0, "0=Female", "1=Male"),  
  "AlcoholUse" = data$AlcoholUse
)  
table1

# Perform chi-square test
chisq.test(table1)

#############################################

# 8. Association between Gender and All-nighters

# Create contingency table
table2 = table(  
  "Gender" = ifelse(data$Gender == 0, "0=Female", "1=Male"),  
  "AllNighter" = ifelse(data$AllNighter == 0, "0=No", "1=Yes")  
)  
table2

# Perform chi-square test
chisq.test(table2)

#############################################

### ANOVA:

# 9. GPA Differences by Class Standing

# Convert ClassYear to factor with labels
data$ClassYear = factor(data$ClassYear, levels = 1:4, labels = c("Freshman", "Sophomore", "Junior", "Senior"))

# Check Normality
# Check Numerical Variable
windows();
hist(data$GPA) 

# Check Homogeneity/ Equality of Variances

# Find std.
# tapply(vector, index, function)
sd_by_gpa = tapply(data$GPA, data$ClassYear, sd)
sd_by_gpa

# Standard Deviation Condition for Equality of Variances 
# Is one std dev >= three times another gp Std Dev? 

sds = c(0.3661250, 0.3736898, 0.4198895, 0.3639701)
vector <- sds
result <- FALSE

# Loop through each element in the vector
for (i in 1:length(vector)) {
  for (j in 1:length(vector)) {
    if (i != j && vector[i] > 2 * vector[j]) {
      result <- TRUE
      break
    }
  }
  if (result) break
}

# Print the result: We want to see FALSE
print(result)

# Perform ANOVA
one.way1 = aov(data$GPA ~ data$ClassYear)
summary(one.way1)

#If the ANOVA results are significant, then perform Tukey's HSD test for post-hoc analysis 
tukey_result = TukeyHSD(one.way1)
print(tukey_result)

table(data$ClassYear)

#############################################

# 10. GPA Differences by Alcohol Use

# Check Normality
# Check Numerical Variable
windows();
hist(data$GPA) 

# Check Homogeneity/ Equality of Variances

# Find std.
# tapply(vector, index, function)
sd_by_gpa = tapply(data$GPA, data$AlcoholUse, sd)
sd_by_gpa

# Standard Deviation Condition for Equality of Variances 
# Is one std dev >= three times another gp Std Dev? 

sds = c(0.4793122, 0.4367589, 0.3849260, 0.3888629)
vector <- sds
result <- FALSE

# Loop through each element in the vector
for (i in 1:length(vector)) {
  for (j in 1:length(vector)) {
    if (i != j && vector[i] > 2 * vector[j]) {
      result <- TRUE
      break
    }
  }
  if (result) break
}

# Print the result: We want to see FALSE
print(result)

# Perform ANOVA
one.way2 = aov(data$GPA ~ data$AlcoholUse)
summary(one.way2)

#If the ANOVA results are significant, then perform Tukey's HSD test for post-hoc analysis
tukey_result = TukeyHSD(one.way1)
print(tukey_result)

table(data$ClassYear)

#############################################

rm(SleepStudy)
