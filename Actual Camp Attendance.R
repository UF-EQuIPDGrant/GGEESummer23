#Actual Attendance
#Krista D Chisholm
#09/28/2023



library(tidyverse)
library(tidytext)
library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(readxl)
library(textdata)
library(ggplot2)
library(ggrepel)
library(scales)


GGEE_23_Camps <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 1)
#View(GGEE_23_Camps)
GGEE_23_Camps_Intro <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 2)
GGEE_23_Camps_Adv <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 3)
  
  
All_Students <- sum(GGEE_23_Camps$n)
Intro_Students <- sum(GGEE_23_Camps_Intro$n)
Adv_Students <- sum(GGEE_23_Camps_Adv$n)


###Pie All###

Camps_all <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 4)

df_all <- Camps_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(Camps_all, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Purples")+
  ggtitle("Distribution of Students by District for All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_all,
                   aes(y = pos, label = paste0(round(n/All_Students*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "Graphs/All Camps",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



###Pie Intro###

Camps_intro <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 5)

df_intro <- Camps_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(Camps_intro, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Purples")+
  ggtitle("Distribution of Students by District for Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_intro,
                   aes(y = pos, label = paste0(round(n/Intro_Students*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "Graphs/All Camps",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###Pie Adv###

Camps_adv <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 6)

df_adv <- Camps_adv%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(Camps_adv, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Purples")+
  ggtitle("Distribution of Students by District for Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_adv,
                   aes(y = pos, label = paste0(round(n/Adv_Students*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "Graphs/All Camps",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

