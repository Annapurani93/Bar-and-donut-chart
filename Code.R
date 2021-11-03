library(tidyverse)
library(readxl)
library(ggtext)

read_excel("100 Most Spoken Languages.xlsx")->data
str_replace_all(data$`Total Speakers (in million)`,"M","")->data$`Total Speakers (in million)`
str_replace_all(data$`Native Speakers (in million)`,"M","")->data$`Native Speakers (in million)`
str_replace_all(data$`Native Speakers (in million)`,"NA","0")->data$`Native Speakers (in million)`

data$Rank<-as.numeric(data$Rank)
data$`Total Speakers (in million)`<-parse_number(data$`Total Speakers (in million)`)
data$`Native Speakers (in million)`<-parse_number(data$`Native Speakers (in million)`)

colnames(data)<-c("Rank","Language","Total","Native","Origin")

data%>%
  rowwise()%>%
  mutate(NonNative=Total-Native)%>%
  select(Language, Total, Native, NonNative)%>%
  arrange(desc(Total))->newspeakers

newspeakers%>%
  rowwise()%>%
  mutate(NativeP=round((Native/Total)*100))%>%
  mutate(NonNativeP=round((NonNative/Total)*100))%>%
  arrange(desc(Total))->share

share[-c(2,3,4)]->share1
share1%>%
  head(10)->share1

share%>%
  head(10)->share2

#column chart of the top 10 languages based on total speakers

ggplot(share2,aes(x=Total,y=reorder(Language,Total,decreasing=TRUE),label=paste(Total,"million")))+
         geom_col(fill="#71b7ec",colour="white")+
  geom_text(colour="black",hjust=1.2,fontface="bold")+
  theme(axis.text.y=element_text(color="white", size=12, hjust=.5, face = "bold"),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "white", face="bold"),
        plot.subtitle=element_markdown(size=12, colour="white",margin=margin(b=15)))+
  labs(title=str_wrap("ENGLISH, FRENCH AND STANDARD ARABIC ARE AMONG THE TOP 10 MOST-SPOKEN LANGUAGES IN THE WORLD...",50),
       subtitle=str_wrap("The top 10 most-spoken languages in the world based on the number of people who speak them",50))->chart1

library(reshape2)
melt(share1,id.vars = "Language",measure.vars = c("NativeP","NonNativeP"),value.name = "value") ->share3      


share3%>%
  mutate(hsize=3)->share3
hsize=3

share3%>%
  mutate(Language=fct_relevel(Language,levels="English","Mandarin Chinese","Hindi",
                              "Spanish","French","Standard Arabic","Bengali",
                              "Russian","Portuguese","Indonesian"))->share3

#donut chart of the share of native and non-native speakers
ggplot(share3,aes(x=hsize,y=value,fill=variable))+
  geom_col(colour="white",alpha=1)+
  scale_fill_manual(values=c("#00b7b7","#ea8282"),labels=c("Native Speakers","Non-native Speakers"))+
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))+
  facet_wrap(~Language,nrow=2)+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="white",face="bold",size=12),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face = "bold",size=14),
        plot.title.position = "plot",
        plot.title=element_text(size=16, colour = "white", face="bold"),
        plot.subtitle=element_text(size=12, colour="white",margin=margin(b=15)),
        plot.caption=element_text(hjust=1, size=9, colour="white", margin=margin(t=15)))+
  labs(title=str_wrap("...BUT A GREATER SHARE OF PEOPLE WHO SPEAK THESE LANGUAGES ARE NON-NATIVE SPEAKERS",50),
       subtitle=str_wrap("Share of the top 10 most-spoken languages as native and non-native speakers",50),
       caption = "Data: @DiversityinData | Design: @annapurani93")->chart3


chart1+chart3->chart2
ggsave("chart2.png",chart2,width = 21,height=10,dpi=600)


