
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


GGEE_23_PreSurvey <- read_excel("Documents/GitHub/GGEESummer23/Data/GGEE_23_PreSurvey.xlsx")
#View(GGEE_23_PreSurvey)

Experience<- select(GGEE_23_PreSurvey, ResponseID, ExperienceType)
Experience_omit <- na.omit(Experience)

#remove words
remove_words <-data.frame("word"= c("coded", "program", "6th", "5th", "3rd", "4th", "7th", "0","1","10","	
1st", "27","null", "NA", "na", "coding", "ive", "that's", "code", "grade", "2", "	
2015", "2021", "2022", "30", "3rd", "50", "6", "9", "90", "experience", "learned", "called", "candy", "chapman", "forgot", "floor", "lake", "taught", "parents", "taking", "stuff", "simple", "east", "hart"))

Experience_tidy<-unnest_tokens(Experience, word, ExperienceType)

Experience_remove<- anti_join(Experience_omit,remove_words) #remove repeat words

Experience_clean<-anti_join(Experience_remove, stop_words)

Experience_counts <- count(Experience_clean, word, sort = TRUE)

library(wordcloud2)
wordcloud2(Experience_counts)

