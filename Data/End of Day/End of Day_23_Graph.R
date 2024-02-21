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

EoD_Feeling <- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet =1)
#View(EoD_Feeling)


ggplot(EoD_Feeling, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Feeling")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")
  

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Feelings.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

  
#theme(legend.position = c(.7, .3))
  
###STUDENT IDENTITY###
EoD_Identity <- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 2)
#View(EoD_Identity)


ggplot(EoD_Identity, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Feeling")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Identity.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


##################################
##################################
##################################
##Broken Further

###FELT###
EoD_Felt<- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 3)
#View(EoD_Identity)


ggplot(EoD_Felt, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .2))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Feeling")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Felt.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

###IDENTITY 2###
EoD_Ident<- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 4)
#View(EoD_Identity)


ggplot(EoD_Ident, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .2))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Identity")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Ident.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

###FIND###
EoD_Find<- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 5)
#View(EoD_Identity)


ggplot(EoD_Find, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .2))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Findings")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Find.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



###Enjoy###
EoD_Enjoy<- read_excel("GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 6)
#View(EoD_Identity)


ggplot(EoD_Enjoy, aes(x=Stage, y=Mean)) + 
  geom_point(stat="identity", size=4, aes(fill=Question, color = Question))+ 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color = Question), width=.1)+
  theme_classic()+
  theme(legend.position = c(.7, .2))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("Average End of Day Student Enjoyment")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6), n.breaks=6)+
  scale_color_brewer(palette = "Set1")+
  xlab("Activity")+
  ylab("Average Rating (1-5)")

ggsave(
  filename = "GGEE_23_Summer_EndofDay_Enjoy.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)
