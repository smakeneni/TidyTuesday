#loading libraries
library(ggthemes)
library(tidyverse)

#reading files
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')
covers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/covers.csv')
comic_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/comic_bechdel.csv')

#exploring the data frame 
summary(characters)

#Changing two character names to include "=" so that I can split the column into tow columns
characters$character <- ifelse(characters$character=="Professor X","Professor X = Charles",
                        ifelse(characters$character=="Moira MacTaggert (scientist helper)","Scientist = Moira MacTaggert",characters$character))

#Splitting the character column to two columns Xname and name
characters <- characters %>% separate(character,c("Xname","name"),"=")

#All female characters
Female <- c("Marvel Girl/Phoenix ","Storm ","Ariel/Sprite/Shadowcat ","Phoenix(2) ","Rogue ","Mystique ","Binary/Ms Marvel ",
            "Dazzler ","Psylocke ","Scientist ","Jubilee ")

#Adding a new gender column
characters <- characters %>% mutate(gender=ifelse(Xname%in%Female,"Female","Male"))

#plotting tears by gender
crying_by_gender <- characters %>% group_by(Xname,gender) %>% summarize(total=sum(visible_tears_number_of_panels)) %>% 
  filter(total>0) %>% arrange(desc(total)) %>% 
  ggplot(aes(x=reorder(Xname,total),y=total,color=gender,label=total))+
  geom_segment(aes(x=reorder(Xname,total),xend=reorder(Xname,total),y=0,yend=total),color="black")+
  geom_point(size=3)+geom_text(nudge_y=1.0)+coord_flip()+facet_wrap(~gender,ncol=1,scale="free_y")+
  labs(x=NULL,y=NULL,caption="Viz:@smakeneni",title="No. of instances X-MEN characters depicted crying")+
  scale_color_manual(values=c("darkred","dodgerblue3"))+theme_clean()+theme(plot.background = element_rect(fill="grey"))+
  theme(legend.position = "none")+
  theme(text=element_text(family="Georgia",color="black",size=14),strip.background = element_rect(fill="lightgrey"))+
  theme(plot.title=element_text(color="gold",hjust=1.0,size=16),plot.caption = element_text(size=8))

#saving the plot
ggsave("Xmen.png",crying_by_gender,device="png",dpi=320)
 