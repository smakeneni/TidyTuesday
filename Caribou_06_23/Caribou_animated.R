#Tidy Tuesday 06/23/2020 - Spandana Makeneni
library(lubridate)
library(gganimate)
library(tidyverse)


#Reading input files
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

#examining individuals
summary(individuals)

#Theres a lot of missing values
#Lets explore locations

summary(locations)

#This seems more complete
#There seems to be a time gap between 1992 to 2001 
#Lets see if we can plot the migration patterns of the caribou in Summer and Winter between 2001-2016

#Lets add date and year columns to locations

locations <- locations %>% mutate(date= date(timestamp)) %>% mutate(year=year(timestamp)) 

migration_by_season <- locations %>% group_by(year) %>% arrange(year) %>% 
filter(year>=2001) %>% ggplot(aes(x=longitude,y=latitude)) +geom_point(aes(fill=study_site))+ 
    transition_time(date)+labs(title="Caribou migration patterns in British Columbia between 2001-2016",
    subtitle="Year:{frame_time}",fill="Location",x="Longitude",y="Latitude")+shadow_wake(wake_length = 0.1,alpha=FALSE)
  
  #shadow_mark(past=TRUE,future=FALSE,alpha=0.1)

animate(migration_by_season,renderer=gifski_renderer("Caribou_migration.gif",width=3000,height=6000))
