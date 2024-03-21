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
library(stringr)
library(ggrepel)
#Brewers color pallet: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

#####STACKED BAR 100%##############

####################################################################################################################
###FEELING ###
####################################################################################################################

EoD_FELT <- read_excel("~/Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 8)

##clustered bar chart

ggplot(EoD_FELT)+
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Stage, y = P, group = Level), size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  scale_x_discrete(limits=c('Technical Design Challenge', 'Micro:bit Pet','Programming Basics' ), labels = label_wrap(25))+
  coord_flip()+
  theme_classic()+
  theme(strip.text = element_text(size = 14), strip.background=element_rect(size=1))+
  facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Rating", reverse = FALSE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("Camp Activity")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


ggsave(
  filename = "GGEE_23_Summer_EoD_Stacked_Clustered_0304.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/oliviarandell/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#oliviarandell
#kristadulany
#kristachisholm
#strip.background = element_blank()


####################################################################################################################
###Identity Graph###
####################################################################################################################

EoD_Identity <- read_excel("~/Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 9)

##clustered bar chart

ggplot(EoD_Identity)+
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Stage, y = P, group = Level), size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  scale_x_discrete(limits=c('Technical Design Challenge', 'Micro:bit Pet','Programming Basics' ), labels = label_wrap(25))+
  coord_flip()+
  theme_classic()+
  theme(strip.text = element_text(size = 14), strip.background=element_rect(size=1))+
  facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Rating", reverse = FALSE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("Camp Activity")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


ggsave(
  filename = "GGEE_23_Summer_EoD_Stacked_Clustered_0304_Identity.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/oliviarandell/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#oliviarandell
#kristadulany
#kristachisholm
#strip.background = element_blank()


####################################################################################################################
###Find Graph###
###################################################################################################################
EoD_Find <- read_excel("~/Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 10)

ggplot(EoD_Find)+
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Stage, y = P, group = Level), size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  scale_x_discrete(limits=c('Technical Design Challenge', 'Micro:bit Pet','Programming Basics' ), labels = label_wrap(25))+
  coord_flip()+
  theme_classic()+
  theme(strip.text = element_text(size = 14), strip.background=element_rect(size=1))+
  facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Rating", reverse = FALSE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("Camp Activity")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


ggsave(
  filename = "GGEE_23_Summer_EoD_Stacked_Clustered_0304_Find.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/oliviarandell/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#oliviarandell
#kristadulany
#kristachisholm
#strip.background = element_blank()

####################################################################################################################
###Enjoy Graph###
###################################################################################################################
EoD_Enjoy <- read_excel("~/Documents/GitHub/GGEESummer23/Data/End of Day/End of Day_23_Graph.xlsx", sheet = 11)

ggplot(EoD_Enjoy)+
  geom_bar(aes(x = Stage, y = P, fill = Level),
           position = position_stack(reverse="True"),
           stat = "identity",
           color = "black")+
  #geom_text(aes(label = paste0(round(P, digits=0),"%"),x=Stage, y=P),size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  geom_text(aes(label = round(P, digits=0), x=Stage, y = P, group = Level), size = 3, position = position_stack(vjust = 0.5, reverse=TRUE))+
  scale_x_discrete(limits=c('Technical Design Challenge', 'Micro:bit Pet','Programming Basics' ), labels = label_wrap(25))+
  coord_flip()+
  theme_classic()+
  theme(strip.text = element_text(size = 14), strip.background=element_rect(size=1))+
  facet_wrap(~ Question, ncol=1, scales = 'free_y', labeller= labeller(group= label_wrap_gen(width= 25)))+
  scale_fill_brewer(guide = guide_legend("Rating", reverse = FALSE),palette = "GnBu")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks=3)+
  ylab("Percent of Students")+
  xlab("Camp Activity")+
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


ggsave(
  filename = "GGEE_23_Summer_EoD_Stacked_Clustered_0304_Enjoy.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/oliviarandell/Documents/GitHub/GGEESummer23/Graphs/End of Day/",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#oliviarandell
#kristadulany
#kristachisholm
#strip.background = element_blank()
