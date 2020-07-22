#Loading libraries
library(tidyverse)
library(ozmaps)
library(sf)
library(ggthemr)
library(gganimate)

ggthemr("dust")
#Loading data
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

#after initial exploration, I want to explore how many animals were euthanized during these years

#plotting euthanized animals by year 
Euthanized_animals <- animal_outcomes %>% filter(outcome=="Euthanized")
plot_1 <- ggplot(Euthanized_animals,aes(x=factor(year),y=Total,group=animal_type,color=animal_type))+geom_line()+geom_point()+
  ggtitle("Euthanized animals in Australia between 1999-2018")+xlab("Year")+ylab("")+ transition_reveal(year)+labs(color="Animal")+
  theme(axis.text.x = element_text(face="bold",angle=40),axis.text.y=element_text(face="bold"))+theme(legend.position = "bottom")

#saving the plot as a gif
animate(plot_1,width=1000,height=1000,res=150,renderer=gifski_renderer("plot_1.gif"))

#looks like the number of euthanized cats and dogs have been decreasing consistently while there seems to an increase in widlife thats being euthanized 
#I am going to try to plot if a specific region is contributing to this

#converting wide to long format 
Euthanized_animals <- gather(Euthanized_animals,NAME,subtotal,ACT:WA)

#Using ozmaps to plot a map with euthanzied animals by territory
oz_states <- ozmaps::ozmap_states
oz_states <- oz_states %>% filter(NAME!="Other Territories") %>% mutate(NAME=ifelse(NAME=="New South Wales","NSW",
                                        ifelse(NAME=="Victoria","VIC",
                                        ifelse(NAME=="Queensland","QLD",
                                        ifelse(NAME=="South Australia","SA",
                                        ifelse(NAME=="Western Australia","WA",
                                        ifelse(NAME=="Tasmania","TAS",
                                        ifelse(NAME=="Northern Territory","NT","ACT"))))))))

#adding latitude and longitude 
Euthanized_wildlife <- Euthanized_animals %>% filter(animal_type=="Wildlife")
Euthanized_wildlife <- Euthanized_wildlife %>% mutate(latitude=ifelse(NAME=="NSW",-33,
                                  ifelse(NAME=="VIC",-36,
                                  ifelse(NAME=="QLD",-21,
                                  ifelse(NAME=="SA",-31,
                                  ifelse(NAME=="WA",-24,
                                  ifelse(NAME=="TAS",-43,
                                  ifelse(NAME=="NT",-18,-35))))))))
Euthanized_wildlife <- Euthanized_wildlife %>% mutate(longitude=ifelse(NAME=="NSW",147,
                                  ifelse(NAME=="VIC",144,
                                  ifelse(NAME=="QLD",143,
                                  ifelse(NAME=="SA",137,
                                  ifelse(NAME=="WA",122,
                                  ifelse(NAME=="TAS",146,
                                  ifelse(NAME=="NT",134,148)))))))) 

#second plot
plot_2 <- ggplot(oz_states)+geom_sf()+geom_point(data=Euthanized_wildlife,mapping=aes(x=longitude,y=latitude,size=subtotal),color="purple",alpha=0.5)+
  coord_sf(xlim=c(105.55,167.99),ylim=c(-48.63,-9.22))+geom_sf_text(aes(label=NAME,color="black"),show.legend = FALSE)+
  scale_size_continuous(range=c(0,12),breaks=c(3000,6000,9000,12000,15000))+labs(size="Total Euthanized wildlife")+theme(legend.position="bottom")+
  theme(plot.title=element_text(size=16,hjust=0.5,face="bold",colour="black",vjust = 0.0))

#animating
animated_plot_2 <- plot_2+transition_states(year,transition_length = 4,state_length = 1)+labs(title="RSPCA outcomes(Euthanized wildlife) by Year: {closest_state}")

#saving the plot as a gif
animate(animated_plot_2,width=1000,height=1000,res=150,renderer=gifski_renderer("euthanized_wildlife.gif"))


