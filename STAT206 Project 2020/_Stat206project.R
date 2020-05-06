#Showing Normality of  Rating Distributions - relevant for sources of error 
hist(data1$MediumRoastRating, breaks =15, xlab = "Medium Roast Rating", main = "Histogram of Medium Roast Ratings", col = "ivory3")
hist(data1$DarkRoastRating, breaks =15,xlab = "Dark Roast Rating", main = "Histogram of Dark Roast Ratings", col = "ivory4")
hist(data1$LightRoastRating, breaks =15, xlab = "Light Roast Rating", main = "Histogram of Light Roast Ratings", col = "ivory")

# ---------------------------------------------------------------------------------------------------

# Question #1 - Do people prefer certain roasts over others?
# H0 = All people like the same coffee roast equally 
# HA = There is a difference in the means of the ratings between at least one pair of the roasts
# ANOVA with 2 blocked factors: Participant and the day of the week when the data was recorded

# Importing data and establishing factors
blockingdata <- read.csv("_stat206_projblockingdata.csv", as.is=T)
str(blockingdata)

blockingdata$Tester  <- as.factor(blockingdata$Tester)
blockingdata$DayofWeek <- as.factor(blockingdata$DayofWeek)
blockingdata$CoffeeType <- as.factor(blockingdata$CoffeeType)
blockingdata$Major <- as.factor(blockingdata$Major)
blockingdata$Gender <- as.factor(blockingdata$Gender)
blockingdata$Smoker <- as.factor(blockingdata$Smoker)
blockingdata$OtherCaf <- as.factor(blockingdata$OtherCaf)
blockingdata$SleptPrevFactor <- as.factor(blockingdata$SleptPrevFactor)
blockingdata$SleptAvgFactor <- as.factor(blockingdata$SleptAvgFactor)
str(blockingdata)

summary(blockingdata)

# Boxplot of Coffee types and their ratings
par(mfrow=c(1,1))
boxplot(Rating~CoffeeType, data=blockingdata, main="Boxplot of Coffee Ratings Between Roasts", xlab = "Roast Type", ylab= "Rating on Scale of -10 - 10", col = "ivory2")

# Anova with 2 blocked factors
aov1 <- aov(Rating ~ CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov1)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that all people like each roast of coffee equally

# Checking model assumptions 
# Normality
res1 <- residuals(aov1)
qqnorm(res1, main="QQ Plot")
qqline(res1, col = "red") 

# Independence
plot(res1, xlab="Residuals", ylab="Residual value", main="Residuals of the Two-way ANOVA")
abline(h=0) # structureless, no obvious patterns

# Constant variance - Bartlett's test 
bartlett.test(Rating ~ CoffeeType, data=blockingdata)
# p value > 0.05 --> we fail to reject the null hypothesis that variances are equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ CoffeeType, data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that all people like each roast of coffee equally

# Using both a parametric and non-parametric test, we can conclude that there is not suffiecient evidence to say that, in general, people have distinct coffee preferences
summary(blockingdata)
str(blockingdata)

# Tukey ( Only look at Coffee Type )
TukeyHSD(aov1)
 
# ---------------------------------------------------------------------------------------------------

# Question #2 - Is there a relationship between gender and coffee preference?
# H0 = Each gender likes the same coffee roast equally for all roasts
# HA = There is a difference in the means of the roast preferences between the genders for at least one coffee roast
# 2 way ANOVA with interaction: Rating (Integer -10 to 10) vs. Gender (Factor, 2 levels) and Coffe Type (Factor, 3 levels) and blocking for tester and day of week

# Importing data and establishing factors
data1 <- read.csv("_stat206projdata.csv", as.is=T)
str(data1)

data1$DayofWeek <- as.factor(data1$DayofWeek)
data1$FavCoffee <- as.factor(data1$FavCoffee)
data1$Major <- as.factor(data1$Major)
data1$Gender <- as.factor(data1$Gender)
data1$Smoker <- as.factor(data1$Smoker)
data1$OtherCaf <- as.factor(data1$OtherCaf)
data1$SleptPrevFactor <- as.factor(data1$SleptPrevFactor)
data1$SleptAvgFactor <- as.factor(data1$SleptAvgFactor)
str(data1)

# Boxplots of Coffee Ratings between Genders
par(mfrow=c(1,3))
boxplot(DarkRoastRating~Gender, data=data1, main = "Gender vs. Dark Rating", col = "ivory4")
boxplot(MediumRoastRating~Gender, data=data1, main = "Gender vs. Medium Rating", col = "ivory3")
boxplot(LightRoastRating~Gender, data=data1,main = "Gender vs. Light Rating", col = "ivory")

#Interaction plot
par(mfrow=c(1,1))
with(blockingdata, interaction.plot(Gender, CoffeeType, Rating))

# 2 way Anova with interaction
aov2 <- aov(Rating ~ Gender*CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov2)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each gender likes the same coffee roast equally across all roasts
#interaction not significant

# Checking model assumptions 
# Normality
res2 <- residuals(aov2)
qqnorm(res2, main="QQ Plot")
qqline(res2, col = "red") 

# Independence
plot(res2, xlab="Residuals", ylab="Residual value", main="Residuals of the Two-way ANOVA")
abline(h=0) # structureless, no obvious patterns

# Constant variance - Bartlett's test 
bartlett.test(Rating ~ interaction(Gender,CoffeeType), data=blockingdata)
# p value > 0.05 --> we fail to reject the null hypothesis that variances are equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ interaction(Gender,CoffeeType), data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each gender likes the same coffee roast equally across all roasts

# Using both a parametric and non-parametric test, we can conclude that there is not suffiecient evidence to say that one gender prefers a certain roast more than the other gender

# Tukey ( Only look at Gender and Gender:Coffee Type)
TukeyHSD(aov2)


# ---------------------------------------------------------------------------------------------------

# Question #3 - Is there a relationship between sleeping habits and coffee preference?
# H0 = People who slept greater than or equal to 7 hours before the test have the same coffee preferences as people who slept less than 7 hours
# HA = There is a difference in the means of coffee preferences between people who slept more than or equal to 7 hours before the test, and people who slept less than 7 hours before the test
# 2 way ANOVA with interaction: Rating (Integer -10 to 10) vs. Slept more/equal to or less than 7 hours last night  (Factor, 2 levels) and Coffee Type (Factor, 3 levels) and blocking for tester and day of week

# Boxplots of Coffee Ratings between Sleeping habits
par(mfrow=c(1,3))
boxplot(DarkRoastRating~SleptPrevFactor, data=data1, main = "Hours Slept Last Night vs. Dark Rating", col = "ivory4")
boxplot(MediumRoastRating~SleptPrevFactor, data=data1, main = "Hours Slept Last Night vs. Medium Rating", col = "ivory3")
boxplot(LightRoastRating~SleptPrevFactor, data=data1,main = "Hours Slept Last Night vs. Light Rating", col = "ivory")

#Interaction plot
par(mfrow=c(1,1))
with(blockingdata, interaction.plot(SleptPrevFactor, CoffeeType, Rating))

# 2 way Anova with interaction
aov3 <- aov(Rating ~ SleptPrevFactor*CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov3)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each gender likes the same coffee roast equally across all roasts

# Normality
res3 <- residuals(aov3)
qqnorm(res3, main="QQ Plot")
qqline(res3, col = "red") 

# Independence
plot(res3, xlab="Residuals", ylab="Residual value", main="Residuals of the Two-way ANOVA")
abline(h=0) # structureless, no obvious patterns

# Constant variance - Bartlett's test 
bartlett.test(Rating ~ interaction(SleptPrevFactor,CoffeeType), data=blockingdata)
# p value > 0.05 --> we fail to reject the null hypothesis that variances are equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ interaction(SleptPrevFactor,CoffeeType), data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each gender likes the same coffee roast equally across all roasts

# Using both a parametric and non-parametric test, we can conclude that there is not suffiecient evidence to say that coffee tastes relate to how many hours people slept the previous night

# Tukey ( Only look at Slept Prev Factor and Slept Prev Factor:Coffee Type)
TukeyHSD(aov3)

# ---------------------------------------------------------------------------------------------------

# Regression analysis - Coffee Ratings 
model1 <- lm(Rating ~ Age+AgeStartDrinkCoffee+DayofWeek+Smoker+Gender+CoffeeType+HrsSleptAvg+OtherCaf, data = blockingdata)
summary(model1)
# None of these variables have a significant relationship with coffee rating for any type

# Regression analysis - Average Sleep
model2 <- lm(HrsSleptAvg ~ Age+AgeStartDrinkCoffee+DayofWeek+CoffeeType+Smoker+Gender+OtherCaf, data = blockingdata)
summary(model2)
# People who started drinking coffee later in life tend to sleep more
# Smokers tend to get more sleep, but there is an unbalanced population, so this can be further tested with more smokers in the population
# People who consume other caffienated drinks besides coffee on a reglar basis tend to sleep less

# Regression analysis - Estimated Caffiene Consumption from Coffee per Week
model3 <- lm(EstWeekCoffeeCafCons ~ Age+AgeStartDrinkCoffee+CoffeeType+Smoker+Gender+OtherCaf+HrsSleptAvg, data = blockingdata)
summary(model3)
# People who started drinking coffee later in life consume less caffiene from coffee per week
# Smokers tend to drink more coffee, but there is an unbalanced population, so this can be further tested with more smokers in the population
