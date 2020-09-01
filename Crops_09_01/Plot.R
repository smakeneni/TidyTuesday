library(tidyverse)
library(hrbrthemes)
library(viridis)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

#converting data to long format 
USA_long_crops <- key_crop_yields %>% filter(Code=="USA") %>% gather("croptype","cropquantity",4:14) %>% mutate(croptype = str_remove_all(croptype, " \\(tonnes per hectare\\)"))
USA_long_crops$Year <- as.integer(USA_long_crops$Year)
USA_long_crops <- na.omit(USA_long_crops)

#stack area plot
USA_plot <- ggplot(USA_long_crops,aes(x=Year,y=cropquantity,fill=croptype))+geom_area(colour="black",alpha=0.6,size=0.5)+
  scale_fill_viridis(discrete = T,name="crop type")+theme_ipsum()+ggtitle("Crop Yields in USA between 1961 and 2018")+
  scale_x_continuous(breaks=seq(1960,2020,5))+
  theme(axis.text.x=element_text(face="bold",angle=45),axis.text.y = element_text(face="bold"),axis.title.x = element_text(face="bold",size=15),
        axis.title.y=element_text(face="bold",size=15))+
  theme(plot.title = element_text(hjust=0.5))

#saving the plot
ggsave(plot=USA_plot,filename="USA_plot.png",device="png",dpi=600)



