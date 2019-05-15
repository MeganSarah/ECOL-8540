##Megan Beaudry 
#version 5-13-19
#Megan.Beaudry@uga.edu
##script for Scientific programming module in R IDEAS course
##comments: Original script was written on 5-13-19

##Step 1: PACKAGES TO LOAD
#the only time you dont want to load all packages at once is if they
###have competing functions
library(ggplot2)
library(lubridate)


##Step 2: DECLARE FUNCTIONS

##Step 3: LOAD DATA
##created WNV working directory and load the data
setwd('C:/Users/msb75462/Desktop/wnv')

##getwd() to check the working directory
getwd()

#create an R dataform by typing data then read.csv
wnv<-read.csv('wnv.csv')

##Step 4: ANALYSIS
##make a histogram using ggplot for the total number of cases in each state in each year
##Here is a line by line explanation of the function used below
##ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  #data defines the object in R
  #aes defines the asethigs, these are the x and y variables
   #geom_histogram(stat = "identity", position = "stack") +
    ##identify just means go and grab the data that is in the dataframe correctly
    ##position = stack is adding the multiple years for counting for a state, so you get the total
    ##position if you want bars on top of each other or next to each other. Dodge is the other option
      #facet_wrap(~wnv$tate) +
      #This makes the data so we have multiple panels by stat
          ##labs(y = "Total Number of Cases", x = "Year")
          ##These are the total number of cases
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$tate) +
  labs(y = "Total Number of Cases", x = "Year")

##pretty but wrong version
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  geom_histogram(stat = "identity", position = "stack", aes(fill = wnv$tate)) +
  coord_flip() +
  labs(y = "Total Number of Cases", x = "Year")

##now make a histogram for the loga of the number of cases. Version #1. 
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = log(wnv$Total))) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$tate) +
  labs(y = "Total Number of Cases", x = "Year")

##now make the same graph a slighly different way
#I will do this by creating a function for log
logTotal <- log(wnv$Total)
logTotal
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = logTotal)) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$tate) +
  labs(y = "Total Number of Cases", x = "Year")

##Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state
##in each year. Plot a histogram of the calcated CFRs.
##CFR=fatalities/total
CFR=(wnv$Fatal/wnv$Total)

ggplot(data = wnv, aes(x = reorder(wnv$Year, CFR), y = CFR)) +
  geom_histogram(stat = 'identity', position = "stack") +
  facet_wrap(~wnv$tate) +
  labs(y = 'Raw Case Fatality Rate', x = 'Year')

#Use arithmetic operators, logical operators, and the function sum to verify that the
##variable Total is simply the sum of the number of febrile cases, neuroinvasive cases, and other
##cases.
sum(wnv$Fever+wnv$EncephMen+wnv$Other)==sum(wnv$Total)

##Now lets look and see what is false
wnv$Total != (wnv$Fever+wnv$EncephMen+wnv$Other)

##Use modular arithmetic to provide an annual case count for each state rounded
##(down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated
##with this calculate, then add the errors to obtain the total error.
##this gives you the remainder after you divide by 12
##the two %% gives you the remainder by whatever number is after that
wnv$rounded<-wnv$Total-wnv$Total%%12

##now we think we need to calculate the standard error
##here i created a new colulm in the first line
wnv$rounded2<-wnv$Total-wnv$rounded
sum(wnv$rounded2/sum(wnv$Total))


#write a function to calc mean
mean <- mean(wnv$Fever/wnv$Total)
mean

#write a fucntion to calc SD
stanDev <- sd(wnv$Fever/wnv$Total)
stanDev

#calc the average severe disease rate in california, Colorado, and NY
#first we need to pull out a subset of the data
calwnv<- wnv[ which(wnv$tate==c('California')),]
COwnv<- wnv[ which(wnv$tate==c('Colorado')),]
NYwnv<- wnv[ which(wnv$tate==c('New York')),]
detach(wnv)
##Now calc the mean of severe disease per state that was pulled out
mean((calwnv$Fever+calwnv$EncephMen)/calwnv$Total)
mean((COwnv$Fever+COwnv$EncephMen)/COwnv$Total)
mean((NYwnv$Fever+NYwnv$EncephMen)/NYwnv$Total)

##use ggplot to show the neuroivanse disease rate for these stats as a bar graph with error bars to show the SD
mean((calwnv$Fever)/calwnv$Total)
mean((COwnv$Fever)/COwnv$Total)
mean((NYwnv$Fever)/NYwnv$Total)

sd(calwnv$Fever/calwnv$Total)
sd(COwnv$Fever/COwnv$Total)
sd(NYwnv$Fever/NYwnv$Total)

ndr <- function(tate='Colorado', years=1999:2007){
  ##we are using the fucntion to call out colorado in those years, the state and the years are the argument in our function
x <- wnv[wnv$tate %in% tate & wnv$Year %in% years,]
##%in% means within the list of years
y <- data.frame(tate= x$tate, ndr = x$EncephMen/ x$Total)
m <- aggregate(y$ndr, by=list(y$tate), FUN = mean)
##aggregate subsets the data by whatever variables you qualify it with 
se <- aggregate(y$ndr, by=list(y$tate), FUN = function(x) sd(x)/sqrt(length(x)) )
out <- merge(m, se, by = 'Group.1')
names(out) <- c('tate', 'mean.ndr', 'se.ndr')
return(out)
}
disease <- ndr(tate=c('California', 'Colorado', 'New York'))

ggplot(disease, aes(x=tate, y=mean.ndr, fill=tate)) +
  geom_bar(stat = "identity")
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr)) +
    labs(x="tate", y='Neurodegenerative disease rate', title='Neurodegenerative disease rate, 1999-2007 (mean +/- se)')
  