#Megan Beaudry
##Megan.Beaudry@uga.edu
##5-14-19 Data Wrangling
##comments: File started on 5-14-19 in class

##Step one: Packages to load
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
##load the data
setwd('C:/Users/msb75462/Desktop/Wrangling')
prism<-read_csv('climate.csv')
pop<-read_csv('pop.csv')
ld<-read_csv('lyme.csv')

pop %<>% select(fips,starts_with("pop2"))
#this got rid of threea of three of columns
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
##this got rid of all the data that was stratified by year
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
##this added a column with just the year without the word pop in front of it
pop %<>% mutate(year=as.integer(year))
##this made everything an integer??
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
##this got rid of the zeros 
pop %<>% mutate(fips=as.integer(fips))

##task4
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld %<>% mutate(year=str_replace_all(str_year, "Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)

fips.builder<-function(st,ct){
  if(str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))
ld %<>% select(-c(STCODE,CTYCODE,str_year))

lyme.climate<-inner_join(ld,prism,by=c('year','fips'))
lyme.climate.pop<-inner_join(lyme.climate,pop,by=c('fips','year'))

#how many cases of lyme per yearl
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

##The average number of cases in each state - averaged across county and
##year
lymeAvgState <- lyme.climate.pop %>% ungroup %>% group_by(state) %>% summarize(avgLyme=mean(cases)) %>% print


#What was the worst year? Which three states have been most impacted on average?
#worst year is 2009
#3 states are Connecticut, Massachusetts, Delware

####Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same
save(lyme.climate.pop, file='LymeData.Rda')
write_csv(lyme.climate.pop, path='lymeData.csv')


##task 9: 

#get the map
county_map <- map_data("county")
state_map <- map_data("state")

##prepare the data for plot. First group bioogial by fips
ag.fips <- group_by(lyme.climate.pop,fips)

##add al lth ecases tat occur ina county over time
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
##add in the state and county names to each fits to match to geographical data
ld.16y<-left_join(select(lyme.climate.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
##rename state and county headers, and manipuate entries to match to geographical data
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
#add column where cases are expressed as log10
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
#combine geographical and lyme data
map.ld.16y<-inner_join(county_map,ld.16y)
#plot
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))
