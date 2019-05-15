#Megan Beaudry
##Megan.Beaudry@uga.edu
##5-14-19 Linear Modelng
##comments: File started on 5-14-19 in class






library(GGally)
library(magrittr)
data(cars)
cars %>% ggpairs(column=c('speed','distance'))

library(dplyr)
cars %<>% 
  mutate (log10speed=log10(speed))

##reproducibiliy and subsampling - subsample or randomize to make a repodruciable work flow
x<-tribble(rnorm(10)) %>% print

#reproductiitlty and subsampling-a random selection
x %>% sample_n(5)

##set seed is a starting number for number generation
set.seed(123)
x %>% sample_n(5)
##set seed will gave the same random numbers when you set the seed with the same random code 

#linear modeling
library(ggplot2)
ggplot(cars)+geom_point(aes(speed,dist)) +
  geom_smooth(aes(speed,dist),method="lm")
#geompoint is the style which is a scatterplot
##geomsmooth is the method
##linear regression and gives the line of best fit

##linear modeling in R - fitting a linear modeling where he is looking at the relationship between speed and distance
summary(lm(speed-dist,data=cars))

##to create lists - this allows you to mix up data types
y<-list(3.14,"eggs",lm(speed~dist,data=cars)) %>$ print

##coorelation test
corr.test(car$speed, car$dist, method = "spearman")

##modelr - a wayt to store the models with the data frame, store modeling infomation with data, group and nest data for analysis, 
#####un-nest for visualization








###############EXERCISE############################################

##load libraries
library(tidyverse)
library(magrittr)
library(GGally)
library(ggplot2)



## TAKS1
##import the data 
setwd('C:/Users/msb75462/Desktop/Wrangling')
LymeDisease<-read_csv('lymedata.csv')

##taks2
##GGally work
ggpairs(LymeDisease,columns=c("prcp","avtemp","size","cases"))


###Task 3: Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size
###and cases supplied when you recreate the ggpairs plot. Why do we add 1 to the numberof cases?
##Here i am making a new column with the function mutate. I then name the column in the first part
##of the paranthese, and then write what the column will be made of in the second part of the parantheses
LymeDisease %<>% mutate(log10size=log10(size))
LymeDisease %<>% mutate(log10cases=log10(cases+1))


#####Task 4: Using set.seed(222) for reproducibility, create a new data frame to be a random sample 
######(n=100 rows) of the full data frame and plot precipitation 
####(x-axis) vs average temperature (y-axis).
set.seed(222)
sublyme <-LymeDisease %>% sample_n(100)

##Task 4 and 5 making the plot and smooth line
library(ggplot2)
myplot <-ggplot (data=sublyme) +
  geom_point(aes(x = prcp, y = avtemp)) + geom_smooth(aes(x = prcp, y = avtemp), method = 'lm')
myplot

###Task 6: Create a linear model (lm) object with a call like myModel <- lm(y ~ x, data = myData) for the
##subsetted data, where y=avtemp and x=prcp. In addition, view the summary with a call along the lines of
###summary(myModel)

myModel <- lm(avtemp ~ prcp, data=sublyme)
myModel
summary(myModel)

###Task 7: What is the slope of the line you plotted in Task 5, and is the 
###slope significantly different from 0 (p<0.05)?
###3.190341e-06 this was brought forward with the command below
##with the above command I saw 0.00672
summary(myModel)$coefficients[2,4]

##The modelr package
##Task 8: Write a single line of code to generate a ggplot of 
###total population size by year.
task8plot <- LymeDisease %>% group_by(year) %>% summarize(TotalPop = sum(size)) %>% ggplot(.) +geom_point(aes (year, TotalPop))
task8plot

###Task 9: Create a data frame 
##called "by_state" from the main data frame, that groups by state, and inspect it.
by_state <- LymeDisease %>% group_by(state)

##Task 10: Next, update this new data frame so that it is nested (simply pass 
###it to nest). Again, inspect thedata frame by typing its name in the console
###so see how things changed.
#we start with a data frame and we push it over to the right, then
##by pushing it right to left we overwrite what was there. Then call it by state.
##the extra arrow stores it
by_state %<>% nest
by_state

###Task 11: Display the Georgia data in the console window.
by_state

###Task 12: Write a function that takes a data frame as its argument and returns a 
###linear model object that predicts size by year.
##we
lingrowthmodel <- function(df){
Task12 <- lm(size ~ year, data=df)
return(Task12)
}

##to use the argument you created above
##you can pick the data set in the bracket
lingrowthmodel(sublyme)

##Task 13: Add a column to the by_state dataframe, where each row (state) has its own model object.
##using a function in PURRR called map which will connect the dataframe to a function \
##in this case it is our linear model function
##here we will apply it to all the states in the US or in our data frame
## :: was calling it from the particular package. Proceeding it with purrr it grabs the right function
library(purrr)
by_state %<>% mutate(model = purrr::map(data, lingrowthmodel))

##Task 14: Run these commands and inspect "resids". 
##What is the structure of "resids"?
library(purrr)
library(modelr)
purrr::map2
by_state %<>% mutate(resids = purrr::map2(data, model, add_residuals))

###Task 15: Write a function that accepts an object of the type in the resids list,
###and returns a sum of the absolute values, i.e. 
##ignoring sign: abs(3)+abs(-2)=5. Use the function to add a column called 
###totalResid to by_state that provides the total size of residuals summed over counties and years.
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = purrr::map(resids,sum_resids))
                     
##Task 16: Write a function that accepts a linear model and returns the slope 
##(model M has slope
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model,get_slope))

##Task 17: Plot the growth rate (slope value) for all states.
##
un_nest <- by_state %<>% unnest
