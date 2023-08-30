---
#title: Demographics Analysis
#author: Krista Dulany Chisholm
#date: August 30, 2023
---


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

GGEE_23_Pre <- read_excel("Data/GGEE_23_PreSurvey.xlsx", sheet = 1)
View(GGEE_23_Pre)

##Age Distribution
Age<- GGEE_23_Pre |>
  count(Age)

ggplot(Age, aes(x=Age, y = n))+
  geom_bar(position= position_dodge(),stat="identity",
           colour='black', size=.3) +
  theme_classic() +
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer 2023 Age Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 90), n.breaks=10)+
  xlab(" ")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Age.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs",
  scale = 1,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)
