#Load Libraries


library(tidyverse)
library(tidytext)
library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(readxl)
library(textdata)
library(ggplot2)
library(scales)
library(ggrepel)
library(wordcloud2)
library(htmlwidgets)
library(webshot)


##Stacked bar chart###
######################SKILL LEVEL###########  
Pre_Post_Skill<-read_excel("Data/Pre and Post/Pre_Post_Stacked.xlsx", sheet=4)
ggplot(Pre_Post_Skill)+
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Stage, y = P, group = Level), size = 5, position = position_stack(vjust = 0.5, reverse=TRUE))+
  coord_flip()+
  theme_classic()+
  theme(strip.text = element_text(size = 20), strip.background=element_rect(size=1))+
  facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Level"),palette = "GnBu")+
  theme(axis.text.y = element_text(colour = "black", size = 20),
        axis.title.y=element_text(size=20,face="bold"),
        axis.text.x = element_text(colour = "black", size = 20),
        axis.title.x=element_text(size=20,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 24, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("")+
  theme(legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18))
ggsave(
  filename = "GGEE_23_Summer_Pre_Post_Stacked_Skill_0326.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Pre Post Survey/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

####Stacked######
####################Enjoyment########################
Pre_Post_Enjoy<-read_excel("Data/Pre and Post/Pre_Post_Stacked.xlsx", sheet=3)
ggplot(Pre_Post_Enjoy)+
  geom_bar(aes(x = Question, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Question, y = P, group = Level), size = 5, position = position_stack(vjust = 0.5, reverse=TRUE))+
  coord_flip()+
  scale_x_discrete(label=label_wrap(25))+
  theme_classic()+
  theme(strip.text = element_text(size = 18), strip.background=element_rect(size=1))+
  #facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Level"),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 20),
        axis.title.x=element_text(size=20,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 20),
        axis.title.y=element_text(size=20,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("")+
  theme(legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18))
ggsave(
  filename = "GGEE_23_Summer_Pre_Post_Stacked_Experience_0326.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Pre Post Survey/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

