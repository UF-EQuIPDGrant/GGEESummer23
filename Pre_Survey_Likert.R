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
install.packages("webshot")
webshot::install_phantomjs()


####################################################################################################################


Pre_Exp <- read_excel("Data/GGEE_23_Pre_Survey_Likert.xlsx", sheet = 5)
Pre_Enj <- read_excel("Data/GGEE_23_Pre_Survey_Likert.xlsx", sheet = 6)

## STACK PRE - EXPERIENCE


aes(label = paste0(value*100,"%"))

ggplot(Pre_Exp, aes(x=Question, y = n, fill = Level, label = round(p, digits=0),"%"))+
  geom_bar(stat="identity", colour='black', size=.5)+
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Prior Experience in Programming")+
  scale_fill_brewer(guide = guide_legend("% of Rating", reverse = TRUE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_x_discrete(labels = label_wrap(25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210), n.breaks=10)+
  ylab("Number of Students")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))

ggsave(
  filename = "GGEE_23_Summer_Pre_Exp_Stacked.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



## STACK PRE - ENJOY

ggplot(Pre_Enj, aes(x=Question, y = n, fill = Level, label = round(p, digits=0),"%"))+
  geom_bar(stat="identity", colour='black', size=.5)+
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Prior Enjoyment of Programming")+
  scale_fill_brewer(guide = guide_legend("% of Rating", reverse = TRUE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_x_discrete(labels = label_wrap(25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210), n.breaks=10)+
  ylab("Number of Students")+
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))

ggsave(
  filename = "GGEE_23_Summer_Pre_Enj_Stacked.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)
 














