---
  #title: Average End of Day Over Time
  #author: Krista Dulany Chisholm, Olivia Lancaster
  #date: February 19, 2024
  ---

#####Load Libraries

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


#Brewers color pallet: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html


###STUDENT FEELING###

EoD_Feeling <- read_excel("Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet =1)
#View(EoD_Feeling)


ggplot(EoD_Feeling, aes(x=Stage, y=Mean, shape = Question)) + 
  geom_point(stat="identity", size=3, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .3))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Feeling")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")
  



  
  
  
###STUDENT IDENTITY###
EoD_Identity <- read_excel("Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet =2)
#View(EoD_Identity)


ggplot(EoD_Identity, aes(x=Stage, y=Mean, shape = Question)) + 
  geom_point(stat="identity", size=3, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .3))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Identity")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

