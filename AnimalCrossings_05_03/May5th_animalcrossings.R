#Loading libraries
library(tidyverse)
library(ggthemes)
library(ggpubr)

#Loading data
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')


#Impact of gender on species type
#categorizing species into 3 categories and adding a column 
wild <- c("gorilla","alligator","bull","koala","kangaroo","wolf","elephant","deer","tiger","cub","bear","hippo","lion","monkey","rhino")
domestic <- c("squirrel","pig","mouse","cat","horse","hamster","sheep","dog","goat","rabbit","cow")
birds <- c("bird","eagle","penguin","chicken","duck","ostrich")
other <- c("anteater","frog","octopus")
villagers <- villagers %>% mutate(species_type=ifelse(species %in% wild,"wild",
                                      ifelse(species %in% domestic,"domestic",
                                      ifelse(species %in% birds,"birds","other"))))
#Plot for gender vs species type
plot1 <- villagers %>% group_by(gender,species_type) %>% summarize(total=n()) %>% arrange(desc(total)) %>% 
  ggplot(aes(x=reorder(species_type,total),y=total,fill=gender))+geom_bar(stat="identity",position=position_dodge(),color="slategray")+
  scale_fill_brewer(palette="YlGn")+xlab("")+ylab("Total")+geom_text(aes(label=total),vjust=1.5,position=position_dodge(0.9))+
  theme_economist(base_size=10)


#Impact of gender on personality
plot2 <- villagers %>% group_by(gender,personality) %>% summarize(total=n()) %>% arrange(desc(total)) %>% 
  ggplot(aes(x=reorder(personality,total),y=total))+geom_bar(stat="identity",aes(fill=gender),color="slategray")+
  facet_wrap(~gender,scales="free_x")+scale_fill_brewer(palette="YlGn")+xlab("")+ylab("Total")+
  geom_text(aes(label=total),vjust=1.5,position=position_dodge(0.9))+theme_economist(base_size=10)+
  theme(axis.text.x = element_text(angle=60))

#Saving the plot 
plot3 <- ggarrange(plot1,plot2,nrow=2,ncol=1,common.legend = TRUE,legend="bottom")
plot3 <- annotate_figure(plot3,top=text_grob("Species and Personality types by gender",face="bold",size=14))
ggsave(filename="Gendereffect.png",plot=plot3,device="png",width=6,height=8,units=,dpi="retina")
