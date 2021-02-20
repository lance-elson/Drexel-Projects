# Statistical Analysis Applications Project | 03/2020 - 04/2020
# Author - Lance Elson

################################################################################
# Overview
# 65-participant study observing coffee taste preferences between light, medium,
# and dark roasts given to each participant in a random order. Team conducted 1
# test on Monday, and 1 test on Wednesday at the same location. Water is used as
# a pallete cleanser between taste tests. The objective is to find relationships
# between coffee preferences, demographic data, and habitual data for
# marketing applications.

################################################################################
# Error sources
# - Test was conducted next to a Starbucks in a college building. Our population,
#   therefore, was skewed towards coffee drinkers.
# - Population age was heavily skewed towards college students
# - 7 out of 65 participants are smokers
# - 20 possible rating integers

################################################################################
# Data collected:
#   Tester - Unique number assigned to each participant
#   DayofWeek - Which day (Mon, Wed) the test was conducted
#   DarkRoastRating - Roast rating on a scale of -10 (hated it) to 10 (loved it)
#   MediumRoastRating - Roast rating on a scale of -10 (hated it) to 10 (loved it)
#   LightRoastRating - Roast rating on a scale of -10 (hated it) to 10 (loved it)
#   FavCoffee - Calculated value observing which roast participant liked the most
#   Major - What college major someone is. Cleaned up for considtency.
#   Gender - Participant gender
#   Age - Participant age
#   AgeStartDrinkCoffee - Participant's age at which they started drinking coffee
#   Smoker - Whether participant smokes regularly
#   HrsSleptPrev - How many hours the participant slept before the day of the test
#   SleptAvgFactor - Whether participant slept more or less than mean slept hours
#   HrsSleptAvg - How many hours the participant usually sleeps per night
#   SleptAvgFactor - Whether participant sleeps more or less than mean sleeping hours
#   --- Used for calculation
#   DaysWeekDrinkCoffee - How many days a week participant drinks coffee
#   CoffeeflozPerDay - How much coffee a participant drinks on days they drink coffee
#   CaffieneIndex - How a person drinks their coffee on a scale of decaf (0) to 
#   extremely caffinated (10)
#   OtherCaf - Whether a person regularly drinks other caffinated beverages besides
#   coffee
#   EstWeekCoffeeCafCons - Calculated field using the above metrics determining
#   how much caffiene participant gets from coffee every week

################################################################################
# I. Importing data

# Coffee roasts separated by column
data <- read.csv("_stat206projdata.csv", as.is=T)
# Coffee roasts separated by row
blockingdata <- read.csv("_stat206projblockingdata.csv", as.is=T)

# Separating factors from numerical data
factorCols <- c("Tester", "DayofWeek", "Major", "Gender",
                "FavCoffee", "Smoker", "OtherCaf", "SleptPrevFactor",
                "SleptAvgFactor")
data[factorCols] <- lapply(data[factorCols], as.factor)

factorColsBlocking <- c("Tester", "DayofWeek", "CoffeeType", "Major", "Gender",
                        "FavCoffee", "Smoker", "OtherCaf", "SleptPrevFactor",
                        "SleptAvgFactor")
blockingdata[factorColsBlocking] <- lapply(blockingdata[factorColsBlocking], as.factor)

# Summary
summary(data)

################################################################################
# II. Visualizing data distributions
# Relevant for sources of error 

# Checking rating distributions normality
hist(data$MediumRoastRating, breaks = 15, xlab = "Medium Roast Rating",
     main = "Histogram of Medium Roast Ratings", col = "ivory3")
hist(data$DarkRoastRating, breaks = 15,xlab = "Dark Roast Rating",
     main = "Histogram of Dark Roast Ratings", col = "ivory4")
hist(data$LightRoastRating, breaks = 15, xlab = "Light Roast Rating",
     main = "Histogram of Light Roast Ratings", col = "ivory")
# Confirmed skew towards coffee drinkers. People generally like coffee.

# Numbers of favorite coffee roasts
plot(data$FavCoffee, xlab = "Favorite Roast Type", ylab = "Frequency",
     main = "Plot of Favorite Roast Frequencies",
     col = c("ivory4", "ivory", "ivory3"))

# Distribution of age
hist(data$Age, breaks = 20, xlab = "Participant Age",
     main = "Histogram of Age")
# Heavy skew towards college-age participants

# Distribution of age when people started drinking coffee
hist(data$AgeStartDrinkCoffee, breaks = 20, xlab = "Age",
     main = "Histogram of Age Participants Started Drinking Coffee")
# Relatively normal distribution with spike at 16

# Distributions of sleeping hours
par(mfrow=c(1,2))
hist(data$HrsSleptPrev, breaks = 10, xlab = "Hours",
main = "Hours Slept Last Night")
hist(data$HrsSleptAvg, breaks = 5, xlab = "Hours",
     main = "Avg. Hours Slept")
# Relatively normal distributions
par(mfrow=c(1,1))

################################################################################
# II. Analysis

# A. Linear Regression - Variables vs. How People Rate Roasts ------------------
model1 <- lm(Rating ~ Age+AgeStartDrinkCoffee+DayofWeek+Smoker+Gender+CoffeeType
             +HrsSleptAvg+OtherCaf, data = blockingdata)
summary(model1)
# - None of these variables have a significant relationship with roast rating

# B. Linear Regression - Variables vs. Average Sleep
model2 <- lm(HrsSleptAvg ~ Age+AgeStartDrinkCoffee+DayofWeek+CoffeeType+Smoker+
               Gender+OtherCaf, data = blockingdata)
summary(model2)
# - People who started drinking coffee later in life tend to sleep more
# - Smokers tend to get more sleep, but there are few smokers in the population,
#   so this can be further tested with more smokers
# - People who consume other caffeinated drinks besides coffee on a regular basis
#   tend to sleep less

# C. Linear Regression - Variables vs. Est. Weekly Caffiene Consumption from Coffee
model3 <- lm(EstWeekCoffeeCafCons ~ Age+AgeStartDrinkCoffee+CoffeeType+Smoker+
               Gender+OtherCaf+HrsSleptAvg, data = blockingdata)
summary(model3)
# - People who started drinking coffee later in life consume less caffeine from
#   coffee per week
# - Smokers tend to drink more coffee, but there are few smokers in the population,
#   so this can be further tested with more smokers

#------------------------------------------------------------------------------
# Question #1 - Do people prefer certain roasts over others? ------------------
#   H0 = All people like the same coffee roast equally 
#   HA = There is a difference in the means of the ratings between at least one
#   pair of the roasts
#   ANOVA with 2 blocked factors: Participant and the day of the week when the
#   data was recorded
#   Also Kruskal test, since abnormal roast rating distributions

# ANOVA with 2 blocked factors
aov1 <- aov(Rating ~ CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov1)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that all
# people like each roast of coffee equally

# Checking model assumptions 
# Normality
res1 <- residuals(aov1)
qqnorm(res1, main="QQ Plot")
qqline(res1, col = "red") 

# Independence
plot(res1, xlab="Residuals", ylab="Residual value", main="Residuals of the
     Two-way ANOVA")
abline(h=0) # structureless, no obvious patterns

# Constant variance - Bartlett's test 
bartlett.test(Rating ~ CoffeeType, data=blockingdata)
# p value > 0.05 --> we fail to reject the null hypothesis that variances are
# equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ CoffeeType, data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that all
# people like each roast of coffee equally

# Using both a parametric and non-parametric test, we can conclude that there is
# not suffiecient evidence to say that, in general, people have distinct coffee
# preferences
summary(blockingdata)
str(blockingdata)

# Tukey ( Only look at Coffee Type )
TukeyHSD(aov1)

# ------------------------------------------------------------------------------
# Question #2 - Is there a relationship between gender and coffee preference? --
#   H0 = Each gender likes the same coffee roast equally for all roasts
#   HA = There is a difference in the means of the roast preferences between the
#   genders for at least one coffee roast
#   2 way ANOVA with interaction: Rating (Integer -10 to 10) vs. Gender
#   (Factor, 2 levels) and Coffe Type (Factor, 3 levels) and blocking for tester
#   and day of week
#   Also Kruskal test, since abnormal roast rating distributions

# Boxplots of Coffee Ratings between Genders
par(mfrow=c(1,3))
boxplot(DarkRoastRating~Gender, data=data, main = "Gender vs. Dark Rating",
        col = "ivory4")
boxplot(MediumRoastRating~Gender, data=data, main = "Gender vs. Medium Rating",
        col = "ivory3")
boxplot(LightRoastRating~Gender, data=data,main = "Gender vs. Light Rating",
        col = "ivory")
par(mfrow=c(1,1))

#Interaction plot
with(blockingdata, interaction.plot(Gender, CoffeeType, Rating))

# 2 way Anova with interaction
aov2 <- aov(Rating ~ Gender*CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov2)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each
# gender likes the same coffee roast equally across all roasts
# Interaction not significant

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
# p value > 0.05 --> we fail to reject the null hypothesis that variances are
# equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ interaction(Gender,CoffeeType), data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that each
# gender likes the same coffee roast equally across all roasts

# Using both a parametric and non-parametric test, we can conclude that there is
# not suffiecient evidence to say that one gender prefers a certain roast more
# than another gender

# Tukey ( Only look at Gender and Gender:Coffee Type)
TukeyHSD(aov2)

# ------------------------------------------------------------------------------
# Question #3 - Is there a relationship between last-night sleeping habits and -
# coffee preference?
#   H0 = People who slept greater than or equal to 7 hours before the test have 
#   the same coffee preferences as people who slept less than 7 hours
#   HA = There is a difference in the means of coffee preferences between people
#   who slept more than or equal to 7 hours before the test, and people who
#   slept less than 7 hours before the test
#   2 way ANOVA with interaction: Rating (Integer -10 to 10) vs. Slept
#   more/equal to or less than 7 hours last night  (Factor, 2 levels) and
#   Coffee Type (Factor, 3 levels) and blocking for tester and day of week
#   Also Kruskal test, since abnormal roast rating distributions

# Boxplots of Coffee Ratings between Sleeping habits
par(mfrow=c(1,3))
boxplot(DarkRoastRating~SleptPrevFactor, data=data,
        main = "Hours Slept Last Night vs. Dark Rating", col = "ivory4")
boxplot(MediumRoastRating~SleptPrevFactor, data=data,
        main = "Hours Slept Last Night vs. Medium Rating", col = "ivory3")
boxplot(LightRoastRating~SleptPrevFactor, data=data,
        main = "Hours Slept Last Night vs. Light Rating", col = "ivory")

#Interaction plot
par(mfrow=c(1,1))
with(blockingdata, interaction.plot(SleptPrevFactor, CoffeeType, Rating))

# 2 way Anova with interaction
aov3 <- aov(Rating ~ SleptPrevFactor*CoffeeType+Tester+DayofWeek, data = blockingdata)
summary(aov3)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that people
# who sleep less than 7 hours have the same roast preferences than people who
# sleep more than 7 hours

# Normality
res3 <- residuals(aov3)
qqnorm(res3, main="QQ Plot")
qqline(res3, col = "red") 

# Independence
plot(res3, xlab="Residuals", ylab="Residual value", main="Residuals of the Two-way ANOVA")
abline(h=0) # structureless, no obvious patterns

# Constant variance - Bartlett's test 
bartlett.test(Rating ~ interaction(SleptPrevFactor,CoffeeType), data=blockingdata)
# p value > 0.05 --> we fail to reject the null hypothesis that variances are
# equal across samples 
# which is what we want --> Constant variance assumption holds

# Non parametric test
kruskal.test(Rating ~ interaction(SleptPrevFactor,CoffeeType), data = blockingdata)
# p value CoffeeType > 0.05 --> we fail to reject the null hypothesis that people
# who sleep less than 7 hours have the same roast preferences than people who
# sleep more than 7 hours

# Using both a parametric and non-parametric test, we can conclude that there is
# not suffiecient evidence to say that coffee tastes relate to how many hours
# people slept the previous night

# Tukey ( Only look at Slept Prev Factor and Slept Prev Factor:Coffee Type)
TukeyHSD(aov3)

# ------------------------------------------------------------------------------
################################################################################
