
  #title: Pre Survey Analysis
  #author: Krista Dulany Chisholm
  #date: September 27, 2023

install.packages("tidyverse")

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
library(writexl)
library(wordcloud2)
library(webshot)
library(htmlwidgets) 
webshot::install_phantomjs()

GGEE_23_PreSurvey <- read_excel("Data/GGEE_23_PreSurvey.xlsx")
#View(GGEE_23_PreSurvey)

Experience<- select(GGEE_23_PreSurvey, ResponseID, ExperienceType)
Experience_omit <- na.omit(Experience)
#write_xlsx(Experience_omit,"/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/ExperienceSheet1.xlsx")

ExperienceSheet1_ <- read_excel("Graphs/Pre_Survey Responses/ExperienceSheet1_.xlsx")

#remove words
remove_words <-data.frame("word"= c("coded", "program", "6th", "5th", "3rd", "4th", "7th", "0","1","10","	
1st", "27","null", "NA", "na", "coding", "ive", "that's", "code", "grade", "2", "	
2015", "2021", "2022", "30", "3rd", "50", "6", "9", "90", "experience", "learned", "called", "candy", "chapman", "forgot", "floor", "lake", "taught", "parents", "taking", "stuff", "simple", "east", "hart"))

Experience_tidy<-unnest_tokens(ExperienceSheet1_, word, ExperienceType)
Experience_omit2 <- na.omit(Experience_tidy)

#write_xlsx(Experience_omit2,"/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/ExperienceSheet.xlsx")


Experience_remove<- anti_join(Experience_omit2,remove_words) #remove repeat words

Experience_clean<-anti_join(Experience_remove, stop_words)

Experience_counts <- count(Experience_clean, word, sort = TRUE)

Experience_Fin <- filter(Experience_counts,n>1)

write_xlsx(Experience_Fin,"/Users/kristachisholm/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/ExperienceSheet_Fin.xlsx")


Experience_Cloud <- read_excel("Graphs/Pre_Survey Responses/ExperienceSheet_Fin.xlsx")


hw <- wordcloud2(Experience_Cloud)


library(RColorBrewer)
color_range_number <- length(unique(Experience_Cloud$word))
color <- colorRampPalette(brewer.pal(9,"Blues")[3:7])(color_range_number)[factor(Experience_Cloud$word)]

hw = wordcloud2(Experience_Cloud, color=color, size=2)

hw



##Export Word Cloud

saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 1200, delay =10)

