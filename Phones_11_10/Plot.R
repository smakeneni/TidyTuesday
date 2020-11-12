library(tidyverse)

#reading data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

#combining the data sets and calculating mean values
land_mobile <- inner_join(landline, mobile, by = c('entity', 'year'), suffix = c('_landline', '_mobile'))
mean_values <- land_mobile %>% group_by(continent_landline,year) %>% summarise_at(vars(mobile_subs,landline_subs),funs(mean(., na.rm=TRUE)))

#lollipop plot to calculate the change in average no. of landline and mobile subscriptions by continent
plot1 <- ggplot(mean_values)+geom_segment(aes(x=year,xend=year,y=landline_subs,yend=mobile_subs),color="gray")+
  geom_point(aes(x=year,y=landline_subs,color="Landline"),size=3)+geom_point(aes(x=year,y=mobile_subs,color="Mobile"),size=3)+
  theme_classic()+xlab("")+ylab("Average number of subscriptions per 100 people")+ facet_wrap(~continent_land)+
  scale_color_manual(values=c(rgb(0.2,0.7,0.1,0.5),rgb(0.7,0.2,0.1,0.5)))+theme(legend.title=element_blank())+
  theme(strip.text = element_text(face="bold"))+theme(strip.background = element_rect(fill="wheat2"))+
  labs(title= "Landline vs. mobile subscriptions between 1990-2017", caption = "Data source: OurWorldInData.org\n Dataviz:@smakeneni15")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),plot.caption = element_text(face="bold.italic"))

ggsave("phonesubscriptions.png",plot1,dpi=300)
