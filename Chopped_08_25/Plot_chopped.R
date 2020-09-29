#Loading libraries
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(gridExtra)
library(grid)

#Reading data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

#Plot1 - word cloud plots for most popular ingredients by course
appetizers <- chopped %>% select(appetizer) %>% unnest_tokens(word,appetizer,token=stringr::str_split,pattern=",") %>% count(word,sort=TRUE)
appetizer_plot <- appetizers %>% filter(n>=5) %>% wordcloud2(size=0.5,gridSize=2.0,color="random-light",backgroundColor="grey")

entrees <- chopped %>% select(entree) %>% unnest_tokens(word,entree,token=stringr::str_split,pattern=",") %>% count(word,sort=TRUE)
entree_plot <- entrees %>% filter(n>=5) %>% wordcloud2(size=0.5,gridSize=2.0,color="random-light",backgroundColor="grey")

desserts <- chopped %>% select(dessert) %>% unnest_tokens(word,dessert,token=stringr::str_split,pattern=",") %>% count(word,sort=TRUE)
dessert_plot <- desserts %>% filter(n>=5) %>% wordcloud2(size=0.5,gridSize=2.0,color="random-light",backgroundColor="grey")

#save png files and then read them to make another plot 
#There is probably a more efficient way to do this but this works for now.

saveWidget(appetizer_plot,"image_1.html",selfcontained = F)
webshot("image_1.html","image_1.png", delay =5, vwidth = 800, vheight=500)

saveWidget(entree_plot,"image_2.html",selfcontained = F)
webshot("image_2.html","image_2.png", delay =5, vwidth =800, vheight=500)

saveWidget(dessert_plot,"image_3.html",selfcontained = F)
webshot("image_3.html","image_3.png", delay =5, vwidth =800, vheight=500)

rl = lapply(sprintf("image_%i.png", 1:3), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
png(width=8,height=3,units="cm",res=600,bg="lightgrey");
print(gridExtra::grid.arrange(grobs=gl,ncol=3))
#grid.text(label="CHOPPED: Most common ingredients used by course",x=0.5,y=0.9,gp=gpar(cex=0.5,fontface="italic",col="blue"))
grid.text(label="Appetizer",x=0.2,y=0.85,gp=gpar(cex=0.4,fontface="EUC",fontfamily="HersheySerif",fontface="cyrillic",col="purple"))
grid.text(label="Entree",x=0.5,y=0.85,gp=gpar(cex=0.4,fontface="EUC",fontfamily="HersheySerif",col="purple"))
grid.text(label="Dessert",x=0.85,y=0.85,gp=gpar(cex=0.4,fontface="EUC",fontfamily="HersheySerif",col="purple"))
grid.text(label="Size of the word represents the frequency",x=0.85,y=0.05,gp=gpar(cex=0.3,fontface="italic",col="black"))
dev.off()

#Plot 2 - rating across all seasons 

chopped %>% filter(!is.na(episode_rating)) %>% filter(season<=40) %>% 
  ggplot(aes(x=factor(season),y=episode_rating))+geom_point()+
  stat_summary(fun=mean,color="red",geom="line",aes(group=1))+scale_y_continuous(limits=c(5,10))
