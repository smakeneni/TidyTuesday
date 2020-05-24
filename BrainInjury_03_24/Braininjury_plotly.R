#Loading libraries
library(tidyverse)
library(lazyeval)
library(plotly)

#Reading files
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv',header=TRUE)
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv',header=TRUE)

#Gathering total injuries by mechanism per year, spreading the data to wide format, changing column names
#Spread the data to work with plotly
total_injuries <- tbi_year %>% filter(!is.na(number_est)) %>% group_by(year,injury_mechanism) %>% summarize(total=sum(number_est))
total_injuries <- spread(total_injuries,injury_mechanism,total)
colnames(total_injuries) <- c("year","Assault","Selfharm","MotorVehicle_Crashes","Nomechanism","Other_unintentionalinjury","Unintentional_falls","Unintentionallystruck")
total_injuries <- as.data.frame(total_injuries)

#Gathering total deaths by mechanism per year
total_deaths <- tbi_year %>% filter(!is.na(number_est)) %>% filter(type=="Deaths") %>% group_by(year,type,injury_mechanism) %>% summarize(total=sum(number_est))
total_deaths <- spread(total_deaths,injury_mechanism,total)
colnames(total_deaths) <- c("year","type","Assault","Selfharm","MotorVehicle_Crashes","Nomechanism", "Other_unintentionalinjury","Unintentional_falls","Unintentionallystruck")
total_deaths <- as.data.frame(total_deaths)

#function to add a column frame for use with plotly anaimations
accumulate_by <- function(dat, var) {
  var <- f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  bind_rows(dats)
}

#adding frame column to the total injuries data frame
total_injuries <- total_injuries %>% accumulate_by(~year)
total_deaths <- total_deaths %>% accumulate_by(~year)

#plotting injuries by mechanism
injuries_plot <- plot_ly(total_injuries,x=~year,y=~Assault,frame=~frame,type="scatter",mode="plines",name="Assault") %>% 
  add_trace(y=~Selfharm,type="scatter",mode="plines",name="Selfharm") %>% 
  add_trace(y=~MotorVehicle_Crashes,type="scatter",mode="plines",name="Motor Vehicle Crashes") %>% 
  add_trace(y=~Nomechanism,type="scatter",mode="plines",name="No mechanism") %>% 
  add_trace(y=~Other_unintentionalinjury,type="scatter",mode="plines",name="Other unintentinal injury") %>% add_trace(y=~Unintentional_falls,type="scatter",mode="plines",name="Unintentional falls") %>% add_trace(y=~Unintentionallystruck,type="scatter",mode="plines",name="Unintentionally struck")

injuries_plot <- injuries_plot %>% animation_slider(currentvalue=list(prefix="Year "))
injuries_plot <- injuries_plot %>% layout(title="Total Brain Injuries by mechanism") %>% 
  layout(yaxis=list(title="")) %>% layout(xaxis=list(title=""))
injuries_plot

#plotting deaths by mechanism
deaths_plot <- plot_ly(total_deaths,x=~year,y=~Assault,frame=~frame,type="scatter",mode="plines",name="Assault") %>%
  add_trace(y=~Selfharm,type="scatter",mode="plines",name="Selfharm") %>% 
  add_trace(y=~MotorVehicle_Crashes,type="scatter",mode="plines",name="Motor Vehicle Crashes") %>% 
  add_trace(y=~Nomechanism,type="scatter",mode="plines",name="No mechanism") %>% 
  add_trace(y=~Other_unintentionalinjury,type="scatter",mode="plines",name="Other unintentinal injury") %>% 
  add_trace(y=~Unintentional_falls,type="scatter",mode="plines",name="Unintentional falls") %>% 
  add_trace(y=~Unintentionallystruck,type="scatter",mode="plines",name="Unintentionally struck")

deaths_plot <- deaths_plot %>% animation_slider(currentvalue=list(prefix="Year "))
deaths_plot <- deaths_plot %>% layout(title= "Total deaths from brain injuries by mechanism") %>% 
  layout(yaxis=list(title="")) %>% layout(xaxis=list(title=""))
deaths_plot
