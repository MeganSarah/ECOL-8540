##Ecol 8540 R Workshop

#we want to set the working directory which can be done by setwd('~/./mers)
##where the . is path to the file
##this can also be done by Alternatively, you can navigate by using the Session drop down menu and selecting Set
##Working Directory.)
setwd('C:/Users/msb75462/Desktop/mers')

##getwd() to check the working directory
getwd()

#create an R dataform by tping data then read.csv
mers<-read.csv('cases.csv')

#we can inspect the data using the command head, then the name of the folder
head(mers)

##$ means the head of the colummn, this command will tell us what kind of data we are looking at
class(mers$onset)
##this told us it was a factor

##now we are creating new variables using the data class, the number in this command tells us what row we are going too
mers$hospitalized[890]<-c('2015-02-20')


#this next command is for deleting a row, the comma may mean all the colums
mers<-mers[-471,]

##we may need to install a package. To install a package to go the right hand corner of your screen.
##click the package tab and then install. Find the package you want to install
##can also use command install.pakcage("lubridate)
#NOW we can load the package with the following command
library(lubridate)
##this red message is not an error that says "attaching package"

#there is a variable called onset, the format that it is is not ideal
#we are using a function from the package lubridate, called ymd that puts it in the form we want
#we are naming it onset2 so we get a new table
mers$hospitalized2<-ymd(mers$hospitalized)

#antoher rename
mers$onset2<-ymd(mers$onset)

class(mers$onset2)

##now we are using a command to find the a simple numerical value for the days elapzed since the start of illness
##this code can be used to find the earliest date of onset
day0<-min(na.omit(mers$onset2))


#create a new empidemic day for each one
#as.numeric sees if the object is interpretable as numbers
mers$epi.day<-as.numeric(mers$onset2-day0)

##install gggplot
##now load ggplot
library(ggplot2)

##to make an edmic curve
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/ME
       RS-Cases/blob/gh-pages/data/cases.csv")

#we are now modifying the plot to show  to use aesthetic fill
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##we can change the "fill" which is a column header, on how to sort our data. I will try gender.
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=gender)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#another way to modify the barplot is to change the coordinates by adding "coord_flip" or "cord_polar" to the plot
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_polar()

##UNIVARIATE PLOT
##now we calculate the infectious period and plot a histogram
#this is to calculate the raw infectiou period
mers$infectious.period <- mers$hospitalized2-mers$onset2

##these data are class "difftime"
class(mers$infectious.period)
##now convert to days
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

#now time to make the histogram
ggplot(data=mers) +geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#now we want to calculate a new infectious period as this is a nonsocimal infection
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) + geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
      caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##Now time for a density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', caption ="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
##we can also create an area plot
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', caption ="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#BIVARIATE PLOT
## a form of a bivariate plot is a scatter plot, which can be develped using plot (X variable, Y variable)
##mers is the name of the object we made in R
#this plot will tell us the length of how long people are infected by when they were infected
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##now we want to make a seperate graph that gets rid of the countries with a smoothed lne
##so i added the geom_smooth
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_smooth()+
  geom_point() +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##now ploet infectious.period2 against time but addd a seperate smooth fit for each country
##by adding the mapping, color, and method in the geom_smooth we get the line for each country.
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_smooth(mapping = aes(color=country), method= "loess")+
  geom_point() +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


##FACEING
##this is a method of adding multipanel plots
##we can use facet_wrap or facet_grid
##Now we are excludig countries that did not show a lot of data
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
  title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##now we are seperating it by country and by male and femal
ggplot(data=subset(mers, gender %in% c('M', 'F') &
                     country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')),
       mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


##still determining how to calc case fatalitiy rate
##have installed coarsedata
