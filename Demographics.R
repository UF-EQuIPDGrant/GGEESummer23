---
#title: Demographics Analysis
#author: Krista Dulany Chisholm
#date: September 27, 2023
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

#################################################################################
##Age Distribution
#################################################################################

###############
####AGE PIE####
###############
#age <-  select(GGEE_23_Pre, Age, Program)
age <- select(GGEE_23_Pre, Age, Program)|>
        count(Age, Program)|>
        na.omit(age)

###AGE ALL###

age_all <- select(GGEE_23_Pre, Age, Program)|>
  count(Age)|>
  na.omit(age)


age_n <- 1+6+3+6+34+60+49+39+5+1
age_n

df_age_all <- age_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(age_all, aes(x="", y=n, fill=Age)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Age in All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_age_all,
                   aes(y = pos, label = paste0(round(n/age_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Age_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###AGE INTRO###

age_intro <- filter(age, Program == "Introductory - 1st Year with GGEE")

age_intro_n <- 6+34+60+49+39+5+1
age_intro_n

df_age_intro <- age_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(age_intro, aes(x="", y=n, fill=Age)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Age in Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_age_intro,
                   aes(y = pos, label = paste0(round(n/age_intro_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Age_INTRO.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###AGE ADVANCED###

age_adv <- filter(age, Program == "Advanced - 2nd Year with GGEE")

age_adv_n <- 1+6+3
age_adv_n

df_age_adv <- age_adv%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(age_adv, aes(x="", y=n, fill=Age)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Age in Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_age_adv,
                   aes(y = pos, label = paste0(round(n/age_adv_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Age_ADV.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)






#################################
##BAR PLOTS AGE##################
#################################

age <-  select(GGEE_23_Pre, Age, Program)
age2 <- age|>
  count(Age, Program)
age3 <- na.omit(age2)

age_n <- 1+6+3+6+34+60+49+39+5+1
age_n

#BARPLOT AGE BOTH Programs
#ggplot(age3, aes(x=Age, y = n, fill= Program))+
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
  filename = "GGEE_23_Summer_Age_Both.png",
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

#BAR PLOT AGE INTRO Programs
age_intro <- filter(age3, Program == "Introductory - 1st Year with GGEE")

age_intro_n <- 6+34+60+49+39+5+1
age_intro_n

#ggplot(age_intro, aes(x=Age, y = n))+
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
  filename = "GGEE_23_Summer_Age_Intro.png",
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


#BAR PLOT AGE ADVANCED Programs
age_adv <- filter(age3, Program == "Advanced - 2nd Year with GGEE")

age_adv_n <- 1+6+3
age_adv_n

#ggplot(age_adv, aes(x=Age, y = n))+
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
  filename = "GGEE_23_Summer_Age_Adv.png",
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






#################################################################################
##District Participation in Research##
#################################################################################

district <- select(GGEE_23_Pre, District, Program)|>
            count(District, Program) |>
            na.omit(district)

district_n <- 6+4+10+11+35+35+31+66+6
district_n

###DISTRICT ALL###

district_all <- select(GGEE_23_Pre, District, Program)|>
  count(District)|>
  na.omit(age)


district_n_all <- 10+11+41+35+31+66+10
district_n_all

df_district_all <- district_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(district_all, aes(x="", y=n, fill=District)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District for All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_district_all,
                   aes(y = pos, label = paste0(round(n/district_n_all*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))


ggsave(
  filename = "GGEE_23_Summer_Pie_District_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###DISTRICT INTRO###

district_intro <- filter(district, Program == "Introductory - 1st Year with GGEE")

district_intro_n <- 10+11+35+35+31+66+6
district_intro_n

df_district_intro <- district_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

ggplot(district_intro, aes(x="", y=n, fill=District)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District in Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_district_intro,
                   aes(y = pos, label = paste0(round(n/district_intro_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))


ggsave(
  filename = "GGEE_23_Summer_Pie_District_INTRO.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



###DISTRICT ADVANCED###

district_adv <- filter(district, Program == "Advanced - 2nd Year with GGEE")

district_adv_n <- 6+4
district_adv_n

df_district_adv <- district_adv %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

ggplot(district_adv, aes(x="", y=n, fill=District)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District in Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_district_adv,
                   aes(y = pos, label = paste0(round(n/district_adv_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))


ggsave(
  filename = "GGEE_23_Summer_Pie_District_ADV.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



#################################
##BAR PLOTS DISTRICT ##################
#################################

##BAR PLOT District Both

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
  filename = "GGEE_23_Summer_District_Both.png",
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



##BAR PLOT DISTRICT INTRO

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
  filename = "GGEE_23_Summer_District_Intro.png",
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


##BAR PLOT DISTRICT ADV

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
  filename = "GGEE_23_Summer_District_Adv.png",
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






#################################################################################
##Grade Level in Research##
#################################################################################

#######################
####GRADE LEVEL PIE####
#######################

grade <- select(GGEE_23_Pre, Grade, Program)|>
  count(Grade, Program)|>
  na.omit(grade)

graden <- 2+4+4+5+32+60+53+44 

###GRADE LEVEL ALL###

grade_all <- select(GGEE_23_Pre, Grade, Program)|>
  count(Grade)|>
  na.omit(grade)

grade_n <- 5+32+62+57+48
grade_n

df_grade_all <- grade_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(grade_all, aes(x="", y=n, fill=Grade)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Grade Levels in All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_grade_all,
                   aes(y = pos, label = paste0(round(n/grade_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Grade_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###GRADE LEVEL INTRO###

grade_intro <- filter(grade, Program == "Introductory - 1st Year with GGEE")

grade_intro_n <- 5+32+60+53+44 
grade_intro_n

df_grade_intro <- grade_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(grade_intro, aes(x="", y=n, fill=Grade)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Grade Levels in Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_grade_intro,
                   aes(y = pos, label = paste0(round(n/grade_intro_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Grade_INTRO.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###GRADE LEVEL ADVANCED###

grade_adv <- filter(grade, Program == "Advanced - 2nd Year with GGEE")

grade_adv_n <- 2+4+4
grade_adv_n

df_grade_adv <- grade_adv %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(grade_adv, aes(x="", y=n, fill=Grade)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Grade Levels in Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_grade_adv,
                   aes(y = pos, label = paste0(round(n/grade_adv_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(.9, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Grade_ADV.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)




###########################################
##BAR PLOTS GRADE LEVEL  ##################
###########################################

##BAR PLOT GRADE LEVEL BOTH

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
  filename = "GGEE_23_Summer_Grade_Both.png",
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


##BAR PLOT GRADE INTRO

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
  filename = "GGEE_23_Summer_Grade_Intro.png",
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

##BAR PLOT GRADE ADVANCED

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
  filename = "GGEE_23_Summer_Grade_Adv.png",
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






#################################################################################
##RACE AND ETHNICITY in Research
#################################################################################

#DOUBLE CHECK NUMBERS
raceraw <-  select(GGEE_23_Pre, Race, Program)
raceraw_Count <- count(raceraw, Race, Program)

#write_xlsx(raceraw_Count,("/Users/kristachisholm/Documents/GitHub/GGEESummer23/Data/race_raw_count.xlsx"))

raceraw_Count2 <- count(raceraw, Race)


##BAR PLOT RACE ETH ALL

GGEE_23_Race_Eth_ALL <- read_excel("Data/GGEE_23_Race_Eth_1.xlsx", sheet = 3)

ggplot(GGEE_23_Race_Eth_ALL, aes(x=Race, y = n, fill = Secondary, label = round(n, digits=0),'%'))+
  geom_bar(stat="identity", colour='black', size=.5)+
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Distribution of Race and Ethnicity in All Programs")+
  scale_fill_brewer(guide = guide_legend("# Secondary Race/Ethnicity", reverse = FALSE),palette = "Blues")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95), n.breaks=20)+
  ylab("Number of Students")+
  theme(legend.position = c(.8, .2),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))



ggsave(
  filename = "GGEE_23_Summer_Pie_Race_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



##BAR PLOT RACE ETH INTRO

GGEE_23_Race_Eth_IN <- read_excel("Data/GGEE_23_Race_Eth_1.xlsx", sheet = 1)

ggplot(GGEE_23_Race_Eth_IN, aes(x=Race, y = n, fill = Secondary, label = round(n, digits=0),'%'))+
  geom_bar(stat="identity", colour='black', size=.5)+
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Distribution of Race and Ethnicity in Introductory Programs")+
  scale_fill_brewer(guide = guide_legend("# Secondary Race/Ethnicity", reverse = FALSE),palette = "Blues")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95), n.breaks=20)+
  ylab("Number of Students")+
  theme(legend.position = c(.8, .2),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))



ggsave(
  filename = "GGEE_23_Summer_Pie_Race_INTRO.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



##BAR PLOT RACE ETH ADV

GGEE_23_Race_Eth_ADV <- read_excel("Data/GGEE_23_Race_Eth_1.xlsx", sheet = 2)

race_n <- 6+19+4+42+4+26+21+82
race_n


ggplot(GGEE_23_Race_Eth_ADV, aes(x=Race, y = n, fill = Secondary, label = round(n, digits=0),'%'))+
  geom_bar(stat="identity", colour='black', size=.5)+
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  theme_classic()+
  coord_flip()+
  ggtitle("Distribution of Race and Ethnicity in Advanced Programs")+
  scale_fill_brewer(guide = guide_legend("# Secondary Race/Ethnicity", reverse = FALSE),palette = "Blues")+
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.title.x=element_text(size=14,face="bold"))+
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.title.y=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5), n.breaks=4)+
  ylab("Number of Students")+
  theme(legend.position = c(.8, .2),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))

ggsave(
  filename = "GGEE_23_Summer_Pie_Race_ADV.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)







#################################################################################
##Gender in Research
#################################################################################

##################
####GENDER PIE####
##################

gender <- select(GGEE_23_Pre, Gender, Program)|>
  count(Gender, Program)|>
  na.omit(gender)

gendern <- 4+5+1+45+140+6+3

###GENDER ALL###

gender_all <- select(GGEE_23_Pre, Gender, Program)|>
  count(Gender)|>
  na.omit(gender)

gender_n <- 49+145+6+4
gender_n

df_gender_all <- gender_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_all, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Genders in All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_gender_all,
                   aes(y = pos, label = paste0(round(n/gender_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Gender_ALL.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###GENDER INTRO###

gender_intro <- filter(gender, Program == "Introductory - 1st Year with GGEE")

gender_intro_n <- 5+32+60+53+44 
gender_intro_n

df_gender_intro <- gender_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_intro, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Genders in Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_gender_intro,
                   aes(y = pos, label = paste0(round(n/gender_intro_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Gender_INTRO.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


###GENDER ADVANCED###

gender_adv <- filter(gender, Program == "Advanced - 2nd Year with GGEE")

gender_adv_n <- 4+5+1
gender_adv_n

df_gender_adv <- gender_adv %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


ggplot(gender_adv, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Distribution of Genders in Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, size = 20, face = "bold"))+
  geom_label_repel(data = df_gender_adv,
                   aes(y = pos, label = paste0(round(n/gender_adv_n*100, digits=1), "%")),
                   size = 10, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1, .5),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))



ggsave(
  filename = "GGEE_23_Summer_Pie_Gender_ADV.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Demographics_092723",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)




###########################################
##BAR PLOTS GENDER  #######################
###########################################

##BAR PLOT GENDER BOTH

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
  filename = "GGEE_23_Summer_Gender_Both.png",
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


##BAR PLOT GENDER INTRO

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
  filename = "GGEE_23_Summer_Gender_Intro.png",
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


##BAR PLOT GENDER ADV

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
  filename = "GGEE_23_Summer_Gender_Adv.png",
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


##OLD Gender Pie Chart Both
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
  filename = "GGEE_23_Summer_Gender_PIE_Both.png",
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



#OLD Gender PIE Chart - INTRO
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
  filename = "GGEE_23_Summer_Gender_PIE_Intro.png",
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



#OLD Gender PIE Chart - ADV
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
  filename = "GGEE_23_Summer_Gender_PIE_Adv.png",
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

