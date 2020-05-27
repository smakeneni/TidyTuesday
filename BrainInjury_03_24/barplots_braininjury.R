library(tidyverse)
library(lazyeval)
library(plotly)
library(shiny)

#Reading files
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')

#Changing mechanism names to shorter names
tbi_year <- tbi_year %>% filter(!is.na(number_est)) %>% 
  mutate(injury_mechanism=ifelse(injury_mechanism=="Unintentionally struck by or against an object","Unintentionally struck",
                                 ifelse(injury_mechanism=="Other unintentional injury, mechanism unspecified","Other unintentional",
                                        ifelse(injury_mechanism=="Other or no mechanism specified","Not specified",injury_mechanism))))

#changing type of visit names to shorter names 
tbi_year <- tbi_year %>% mutate(type=ifelse(type=="Emergency Department Visit","EmergencyDeptVisit",type))

#aggregating by year, mechanism, and type of visit
totals <- tbi_year %>% group_by(year,injury_mechanism,type) %>% 
  summarize(total=sum(number_est))

#converting to wide format for plotly 
totals<- spread(totals,type,total)

#Creating a basic bar plot with custom colors
fig <- plot_ly(totals,x=~injury_mechanism,y=~EmergencyDeptVisit,frame=~year,type="bar",name="ERvisits",marker=list(color='rgba(55,128,191,0.7)'),texttemplate='%{y:.2s}',textposition="outside")%>% add_trace(y=~Hospitalizations,name="Hospitalizations",marker=list(color='rgba(233,212,96,0.7)'),texttemplate='%{y:.2s}',textposition="outside") %>% add_trace(y=~Deaths,name="Deaths",marker=list(color='rgba(219,64,82,0.7)'),texttemplate='%{y:.2s}',textposition="outside")

# Adding background color, changing yaxis to log scale, and removing axis titles
fig <- fig %>% layout(yaxis = list(type="log",title = ''), barmode = 'group', xaxis=list(title=""),paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(245,246,249,1)')%>% animation_slider(currentvalue=list(active=1,prefix="",font=list(color='rgba(0,0,0,0)')))
