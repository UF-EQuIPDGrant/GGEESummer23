---
#title: Demographics Analysis
#author: Krista Dulany Chisholm
#date: August 30, 2023
---

install.packages("ggrepel")
  
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

GGEE_23_Pre0 <- read_excel("Data/GGEE_23_PreSurvey.xlsx", sheet = 1)
GGEE_23_Pre <-filter(GGEE_23_Pre0, Finished == "True")
#View(GGEE_23_Pre)

##Age Distribution
age <-  select(GGEE_23_Pre, Age, Program)
age2 <- age|>
  count(Age, Program)
age3 <- na.omit(age2)

age_n <- 6+39+1+66+7+55+3+41+5+1
age_n

#AGE BOTH Programs
ggplot(age3, aes(x=Age, y = n, fill= Program))+
  geom_bar(position= position_dodge(),stat="identity",
           colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(.15, .7))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer 2023 Age Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75), n.breaks=20)+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  xlab("Student Age (Years)")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Age_Both.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

#AGE INTRO Programs
age_intro <- filter(age3, Program == "Introductory - 1st Year with GGEE")

age_intro_n <- 6+39+66+55+41+5+1
age_intro_n

ggplot(age_intro, aes(x=Age, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#9ECAE2", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Introductory Summer Program 2023 Age Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75), n.breaks=20)+
  xlab("Student Age (Years)")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Age_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#AGE ADVANCED Programs
age_adv <- filter(age3, Program == "Advanced - 2nd Year with GGEE")

age_adv_n <- 1+7+3
age_adv_n

ggplot(age_adv, aes(x=Age, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#DDEBF7", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Advanced Summer Program 2023 Age Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), n.breaks=10)+
  xlab("Student Age (Years)")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Age_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#---------------------------------------------------------------------
##District Participation in Research

##District Both

district <-  select(GGEE_23_Pre, District, Program)
district2 <- district|>
  count(District, Program)
district3 <- na.omit(district2)

district_n <- 10+14+8+36+36+35+80+4+6
district_n

ggplot(district3, aes(x=District, y = n, fill= Program))+
  geom_bar(position= position_dodge(),stat="identity",
           colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(.15, .7))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer Program 2023 District Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85), n.breaks=20)+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  xlab("District")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 District_Both.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



##DISTRICT INTRO

district_intro <- filter(district3, Program == "Introductory - 1st Year with GGEE")

district_intro_n <- 10+14+36+36+35+80+6
district_intro_n

ggplot(district_intro, aes(x=District, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#9ECAE2", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Introductory Summer Program 2023 District Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85), n.breaks=20)+
  xlab("District")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 District_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


##DISTRICT ADV

district_adv <- filter(district3, Program == "Advanced - 2nd Year with GGEE")

district_adv_n <- 8+4
district_adv_n

ggplot(district_adv, aes(x=District, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#DDEBF7", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Advanced Summer Program 2023 District Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), n.breaks=10)+
  xlab("District")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 District_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#---------------------------------------------------------------------
##Grade Level in Research

##GRADE LEVEL BOTH

grade <-  select(GGEE_23_Pre, Grade, Program)
grade2 <- grade|>
  count(Grade, Program)
grade3 <- na.omit(grade2)

grade_n <- 1+5+37+2+66+4+62+5+46
grade_n

ggplot(grade3, aes(x=Grade, y = n, fill= Program))+
  geom_bar(position= position_dodge(),stat="identity",
           colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(.15, .7))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer Programs 2023 Grade Level")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75), n.breaks=20)+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  scale_x_discrete(name ="Grade Level", 
                   limits=c("6th Grade","7th Grade","8th Grade","9th Grade","10th Grade"))+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Grade_Both.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


##GRADE INTRO

grade_intro <- filter(grade3, Program == "Introductory - 1st Year with GGEE")

grade_intro_n <- 5+37+66+62+46
grade_intro_n

ggplot(grade_intro, aes(x=Grade, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#9ECAE2", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Introductory Summer Programs 2023 Grade Level")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75), n.breaks=20)+
  scale_x_discrete(name ="Grade Level", 
                   limits=c("6th Grade","7th Grade","8th Grade","9th Grade","10th Grade"))+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Grade_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

##GRADE ADVANCED

grade_adv <- filter(grade3, Program == "Advanced - 2nd Year with GGEE")

grade_adv_n <- 1+2+4+5
grade_adv_n

ggplot(grade_adv, aes(x=Grade, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#DDEBF7", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Advanced Summer Programs 2023 Grade Level")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), n.breaks=10)+
  scale_x_discrete(name ="Grade Level", 
                   limits=c("6th Grade","7th Grade","8th Grade","9th Grade","10th Grade"))+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Grade_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



#---------------------------------------------------------------------
##Gender in Research

##GENDER BOTH

gender <-  select(GGEE_23_Pre, Gender, Program)
gender2 <- gender|>
  count(Gender, Program)
gender3 <- na.omit(gender2)

gender_n <- 4+48+7+156+1+6+5
gender_n

ggplot(gender3, aes(x=Gender, y = n, fill= Program))+
  geom_bar(position= position_dodge(),stat="identity",
           colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(.15, .7))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer Programs 2023 Genders")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160), n.breaks=20)+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  xlab("Gender")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Gender_Both.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


##GENDER INTRO

gender_intro <- filter(gender3, Program == "Introductory - 1st Year with GGEE")

gender_intro_n <- 48+156+6+5
gender_intro_n

ggplot(gender_intro, aes(x=Gender, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#9ECAE2", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Introductory Summer Programs 2023 Genders")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 170), n.breaks=20)+
  xlab("Gender")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Gender_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


##GENDER ADV

gender_adv <- filter(gender3, Program == "Advanced - 2nd Year with GGEE")

gender_adv_n <- 4+7+1
gender_adv_n

ggplot(gender_adv, aes(x=Gender, y = n))+
  geom_bar(position= position_dodge(),stat="identity", fill= "#DDEBF7", colour='black', size=.3) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.8))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Advanced Summer Programs 2023 Genders")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), n.breaks=10)+
  xlab("Gender")+
  ylab("Number of Students")

ggsave(
  filename = "GGEE Summer 2023 Gender_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



##Gender OLD

#gender <- GGEE_23_Pre |>
#  count(Gender)

#ggplot(gender, aes(x=Gender, y = n))+
#  geom_bar(position= position_dodge(),stat="identity",
#           colour='black', size=.5) +
#  theme_classic() +
#  theme(axis.text.x = element_text(colour = "black"))+
#  theme(axis.text.y = element_text(colour = "black"))+
#  ggtitle("GGEE Summer 2023 Gender Distribution")+
#  theme(plot.title = element_text(hjust = 0.5))+
#  scale_y_continuous(expand = c(0, 0), limits = c(0, 200), n.breaks=10)+
#  scale_fill_brewer("Blues")+
#  xlab("Gender")+
#  ylab("Number of Students")


#ggsave(
#  filename = "GGEE Summer 2023 Gender.png",
#  plot = last_plot(),
#  device = "png",
#  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
#  scale = 2,
#  width = 6,
#  height = 4,
#  units = c("in"),
#  dpi = 300,
#  limitsize = TRUE,
#  bg = NULL)




#Gender PIE Chart - BOTH

gender_pie <- gender|>
  count(Gender)|>
  na.omit(gender)

gender_pie_n <- 4+48+7+156+1+6+5

df2 <- gender_pie%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_pie, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  ggtitle("GGEE Summer 2023 Gender Distribution")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(n/gender_pie_n*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5))
  

ggsave(
  filename = "GGEE Summer 2023 Gender_PIE_Both.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



#Gender PIE Chart - INTRO
gender_pie0 <- filter(gender, Program == "Introductory - 1st Year with GGEE")
gender_pie_intro <- gender_pie0|>
  count(Gender)|>
  na.omit(gender_pie0)

gender_pie_intro_n <- 48+156+6+5

df_in <- gender_pie_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_pie_intro, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  ggtitle("GGEE Introductory Summer 2023 Gender Distribution")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df_in,
                   aes(y = pos, label = paste0(round(n/gender_pie_intro_n*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5))

ggsave(
  filename = "GGEE Summer 2023 Gender_PIE_Intro.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



#Gender PIE Chart - ADV
gender_pie1 <- filter(gender, Program == "Advanced - 2nd Year with GGEE")
gender_pie_adv <- gender_pie1|>
  count(Gender)|>
  na.omit(gender_pie1)

gender_pie_adv_n <-  4+7+1

df_adv <- gender_pie_adv%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_pie_adv, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = TRUE),palette = "Blues")+
  ggtitle("GGEE Advanced Summer 2023 Gender Distribution")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df_adv,
                   aes(y = pos, label = paste0(round(n/gender_pie_adv_n*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5))

ggsave(
  filename = "GGEE Summer 2023 Gender_PIE_Adv.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Demographics",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

