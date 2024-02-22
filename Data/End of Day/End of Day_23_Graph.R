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

#####STACKED BAR 100%##############

####################################################################################################################
###CODING EXPERIENCE AND ENJOYMENT###
####################################################################################################################

EoD_FELT <- read_excel("Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 8)


## STACK PRE - FELT


#https://stackoverflow.com/questions/18774632/how-to-produce-stacked-bars-within-grouped-barchart-in-r

aes(label = paste0(value*100,"%"))

ggplot(EoD_FELT, aes(x=Question, y = P, fill = Level, label = round(P, digits=0),"%"))+
  geom_bar(stat="identity", colour='black', size=.5)+facet_grid(~ Stage)
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Student's End of Day Feelings")+
  scale_fill_brewer(guide = guide_legend("% of Rating", reverse = TRUE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_x_discrete(labels = label_wrap(25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210), n.breaks=10)+
  ylab("Percent of Students")+
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







##clustered bar chart
ggplot(EoD_FELT) +
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = "stack",
           stat = "identity") +
  facet_wrap(~ Question)+
  ylab("Percent of Students")+
  xlab("Question")+
  coord_flip()+
  ggtitle("Student's End of Day Feelings")+
  scale_fill_brewer(guide = guide_legend("Rating", reverse = FALSE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "bold"))+
  scale_x_discrete(labels = label_wrap(25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))
  

























######SCATTERPLOT##########
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

