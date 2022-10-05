
install.packages("Rlof")

library(tidyverse)
library(caret) 
library(plyr)
library(dplyr) 
library(anomalize)
library(Hmisc)
library(DMwR)
library(dbscan)
library(Rlof)

data <- read.csv("C:/Users/hibal/Documents/cmps 276/project/final2.csv")
head(data)
summary(data)

data1 <- data[c(4,5,6,7,8,9,13:16,18:30,32:ncol(data))]
data1 <- data1[c(1:8,10:ncol(data1))]

summary(data1)

data1 = subset(data1, select = -c(X) )
names(data1) <- sub("X..", "", names(data1))

str(data1)

# Chceking the distribution of variables 
hist.data.frame(data1)
## Most of the variables follow a normal distribution
## a few exceptions following a negative exponential such as these variables: agg, HIV.Prevelance.Rate, and traffic volume per meter 
## the population variable seems constant with most values around the same value  

train <- data1[1:as.integer(0.7 * nrow(data1)),]
test <- data1[(as.integer(0.7 * nrow(data1))+1):nrow(data1),]
nrow(train)
nrow(test)

hist(data1$Population)
class(train)

summary(train)

#Checking for missing values in the entire dataframe
colSums(is.na(data1))
any(is.na(train))

#Checking for the total number of missing values in the entire dataframe

sum(is.na(train))


## since the variables containing Nans are of different distributions we will use the KNN imputer method to impute missing values 
train <- knnImputation(train)  # perform knn imputation.
anyNA(train)
test <- knnImputation(test)  # perform knn imputation.
anyNA(test)
## check for variance in each column and it seems to be very high  
sapply(train, var)
sapply(test, var)

## check for outliers and remove them 
outlier.scores <- lof(train, k=c(5:10))
outlier.scores
plot(density(outlier.scores))
thr = quantile(outlier.scores, .97)
out_index = which(outlier.scores >= thr)

nrow(train)
print(out_index)
train[out_index,]

plot(train, col="blue", type='p', pch=19)
points(x=out_index, y=train[out_index,], pch=19, col="red")


##
# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
n <- nrow(train)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(train), cex=.8, xlabs=labels)

##

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(train, pch=pch, col=col)



##
data1$location <- data$location

head(data1)
summary(data1)



cases2020 <- read.csv("C:/Users/hibal/Documents/cmps 276/project/casesdeaths/casesdeaths.csv")

cases2020 <- cases2020[c(4,5,6,7,8,10,12,13)]
cases2020$location <-paste(as.character(cases2020$state),as.character(cases2020$location_name))
head(cases2020)
cases2020 <- distinct(cases2020[c(3:9)])
cases2020$date <- as.Date(cases2020$date)
names(cases2020)[names(cases2020) == "date"] <- "Date.Local"
cases2020 <- cases2020[order(cases2020$location,cases2020$Date.Local),]
finalloc <- intersect(unique(cases2020$location), data$location)
data <- data[data$location %in% finalloc,]
summary(cases2020)

# new_cases and new_deaths have negative values
cases2020 <- cases2020[cases2020$location %in% finalloc,]
cases2020$new_cases[cases2020$new_cases < 0] <- NA
cases2020$new_deaths[cases2020$new_deaths < 0] <- NA
summary(cases2020)
newCases <- cases2020[1,]

for (i in finalloc)
{
  x <- cases2020[cases2020$location == i,]
  x[1,]$new_cases <- 0
  x[1,]$new_deaths <- 0
  for (j in 2:nrow(x))
  {
    if (x[j,]$cumulative_cases < x[j-1,]$cumulative_cases)
      {
      
      x[j,]$cumulative_cases <- x[j-1,]$cumulative_cases
      }
    if (x[j,]$cumulative_deaths < x[j-1,]$cumulative_deaths)
    {
      x[j,]$cumulative_deaths <- x[j-1,]$cumulative_deaths
      }
    if (is.na(x[j,]$new_cases)) { x[j,]$new_cases <- x[j,]$cumulative_cases - x[j-1,]$cumulative_cases }
    if (is.na(x[j,]$new_deaths)) { x[j,]$new_deaths <- x[j,]$cumulative_deaths - x[j-1,]$cumulative_deaths }
  }
  newCases <- rbind(newCases,x)
}

newCases <- newCases[2:nrow(newCases),]
newCases[newCases$location == "California Kern",]

summary(newCases)



print(counterC)
print(counterD)

covidInd <- data.frame(unique(cases2020$location))
covidInd$population <- 0
covidInd$indC  <- 0
covidInd$indD  <- 0
covidInd$daysC  <- 0
covidInd$daysD  <- 0
head(covidInd)
names(covidInd)[names(covidInd) == "unique.cases2020.location."] <- "location"
for (i in covidInd$location)
{
  x <- cases2020[cases2020$location == i,]
  # print(x)
  for (r in 1:nrow(x))
  {
    # print(x[r,])
    # # print(x[r,]$cumulative_cases != 0)
    # print(covidInd$daysC[covidInd$location == i])
    if (x[r,]$cumulative_cases != 0 && is.na(x[r,]$cumulative_cases)== FALSE)
    {
      covidInd$daysC[covidInd$location == i] <- covidInd$daysC[covidInd$location == i] +1
      # print(covidInd$daysC[covidInd$location == i])
      # print(x[r,]$cumulative_cases)
      # print(x[r,]$population)
      covidInd$indC[covidInd$location == i] <- covidInd$indC[covidInd$location == i] + (x[r,]$cumulative_cases/x[r,]$total_population)
    }
    if (x[r,]$cumulative_deaths != 0 && is.na(x[r,]$cumulative_deaths)== FALSE)
    {
      covidInd$daysD[covidInd$location == i] <- covidInd$daysD[covidInd$location == i] +1
      covidInd$indD[covidInd$location == i] <- covidInd$indD[covidInd$location == i] + (x[r,]$cumulative_deaths/x[r,]$total_population)
    }
  }
}
covidInd$casesInd <- covidInd$indC * 2000
covidInd$casesInd <- covidInd$casesInd /  covidInd$daysC
covidInd$deathsInd <- covidInd$indD * 20000 
covidInd$deathsInd <- covidInd$deathsInd / covidInd$daysD

write.csv(covidInd,"covidInd.csv", row.names = FALSE)

cases2020[cases2020$location == "Arizona Apache",]

head(cases2020)


full <- merge(data,covidInd)
head(full)

plot(full$mean, full$casesInd)
plot(full$sd, full$casesInd)
plot(full$agg, full$casesInd)
plot(full$Average.Daily.PM2.5, full$casesInd)

plot(full$mean, full$deathsInd)
plot(full$sd, full$deathsInd)
plot(full$agg, full$deathsInd)
plot(full$Average.Daily.PM2.5, full$deathsInd)
head(data)



finalloc <- intersect(unique(cases2020$location), data$location)

data <- data[data$location %in% finalloc,]
cases2020 <- cases2020[cases2020$location %in% finalloc,]

summary(data[3:7])
# no negatives or too big values in Life Expectancy, 5 NAs
# Number of deaths among residents under age 75 per 100,000 population (age-adjusted): no negatives in mortality, but we have an 84000 maximum, 6 NAs
# remove age adj death rate
# Number of deaths among children under age 18 per 100,000 population.
# remove infant mortality
# Percentage of adults reporting 14 or more days of poor physical health per month.
# Percentage of adults reporting 14 or more days of poor mental health per month.
# Percentage of adults aged 20 and above with diagnosed diabetes.
# remove HIV
# Percentage of population who lack adequate access to food.
# Percentage of population who are low-income and do not live close to a grocery store.
# Percentage of adults who report fewer than 7 hours of sleep on average.
# Percentage of adults under age 65 without health insurance.
# Ratio of population to primary care providers other than physicians.
# The income where half of households in a county earn more and half of households earn less.
# Number of deaths due to suicide per 100,000 population.
# Average traffic volume per meter of major roadways in the county.
# Resident population.
# Percentage of population below 18 years of age.
# Percentage of population ages 65 and older.

data2 <- read.csv()





