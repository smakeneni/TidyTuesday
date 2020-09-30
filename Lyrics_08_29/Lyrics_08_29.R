library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggpubr)
library(ggthemr)

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

beyonce_tidy_lyrics <- beyonce_lyrics %>% unnest_tokens(word,line) %>% anti_join(stop_words,by=c("word"="word"))
taylor_tidy_lyrics <- taylor_swift_lyrics %>% unnest_tokens(word,Lyrics) %>% anti_join(stop_words,by=c("word"="word"))

#word cloud plot 
beyonce_tidy_lyrics %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort=TRUE) %>% acast(word~sentiment,value.var="n",fill=0) %>% 
            comparison.cloud(colors=c("red","darkgreen"),max.words=100,scale=c(3.5,0.5))

taylor_swift_tidy_lyrics %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort=TRUE) %>% acast(word~sentiment,value.var="n",fill=0) %>% 
  comparison.cloud(colors=c("red","green"),max.words=100)

#bar plot showing positibe and negative words 
beyonce_plot <- beyonce_tidy_lyrics %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort=TRUE) %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
            mutate(word=reorder(word,n)) %>% ggplot(aes(word,n,fill=sentiment))+ geom_col(show.legend = FALSE)+ facet_wrap(~sentiment,scales="free_y")+
            coord_flip()+labs(y=NULL,x=NULL)

taylor_plot <- taylor_tidy_lyrics %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort=TRUE) %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
  mutate(word=reorder(word,n)) %>% ggplot(aes(word,n,fill=sentiment))+ geom_col(show.legend = FALSE)+ facet_wrap(~sentiment,scales="free_y")+
  coord_flip()+labs(y="Contribution to sentiment",x=NULL)

#saving the plot
plot3 <- ggarrange(beyonce_plot,taylor_plot,nrow=2,ncol=1,common.legend = TRUE,legend="bottom",labels=c("Beyonce","Taylor swift"),font.label=list(size=11,face="bold.italic",color="tan4",family="serif"))
plot3 <- annotate_figure(plot3,top=text_grob("Top 10 positive and negative words used in Beyonce and Taylor Swift songs",face="bold",size=14,color="tan4",family="serif"),
                         bottom = text_grob("Data source: Rosie Baillie and Dr. Sara Stoudt\n Dataviz: @smakeneni", hjust = 1, x = 0.97, face = "italic", size = 10,color="tan4"))
ggsave(filename="Lyrics.png",plot=plot3,device="png",width=8,height=8,units=,dpi="retina")


