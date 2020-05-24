library(tidyverse)
library(ggplot2)
library(gganimate)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

grosses_top10 <- grosses %>% filter(year(week_ending) >=2010 & year(week_ending)<=2019) %>% mutate(year= year(week_ending)) %>% 
          group_by(year,show) %>% summarize(total=sum(weekly_gross)) %>% arrange(desc(total)) %>% ungroup() %>% group_by(year) %>%
          slice(1:10) %>% mutate(rank=rank(total),total_rel=total/total[rank==1],total_label=paste0(" ",round(total/1e7))) %>% ungroup()
  

#static_plot
top10_plot <- ggplot(grosses_top10,aes(rank,group=show,fill=factor(show),color=factor(show)))+
  geom_tile(aes(y=total/2,height=total,width=0.9),alpha=0.8,color=NA)+
  geom_text(aes(y=0,label=paste(show," ")),vjust=0.2,hjust=1)+geom_text(aes(y=total,label=total_label,hjust=0))+
  coord_flip(clip="off",expand=FALSE)+scale_y_continuous(labels=scales::comma)+guides(color=FALSE,fill=FALSE)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y = element_blank(),axis.ticks =element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",panel.background = element_blank(),
        panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        plot.title=element_text(size=25,hjust=0.5,face="bold",colour="black",vjust = 0.0),
        plot.subtitle=element_text(size=18,hjust=0.5,face="italic",color="grey"),
        plot.caption=element_text(size=12,hjust=0.5,face="italic-bold",color="black"),
        plot.background = element_blank(),
        plot.margin = margin(2,2,2,6,"cm"))

animated_plot <- top10_plot+ transition_states(year,transition_length = 4,state_length = 1.5)+ view_follow(fixed_x=TRUE)+labs(title="Top 10 broadway gross incomes: {closest_state}",caption = "Total gross in Millions USD")

animate(animated_plot,200,fps=20,width=1200,height=1000,renderer=gifski_renderer("anim.gif"))
