---
  #title: Pre Survey Analysis
  #author: Krista Dulany Chisholm
  #date: August 31, 2023
  ---

#Load Libraries
  
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
library(wordcloud2)
library(htmlwidgets)
library(webshot)
install.packages("webshot")
webshot::install_phantomjs()


#################################################################################
####WORD CLOUD - PREVIOUS EXPERIENCE####
#################################################################################


Q1Sres <- read_excel("Data/GGEE_23_PreSurvey.xlsx", sheet = 1)
Q1SresSel<- select(Q1Sres, ResponseID, ExperienceType)
Q1Omit <- na.omit(Q1SresSel)
Omit_1 <- as_tibble(Q1Omit)


##Make correct spelling function
#library(hunspell)
#library(purrr)

correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output) }

#no remove words
#Q1Srestidy<-unnest_tokens(Q1Omit, word, Experience_Type)
#Q1Sresclean<-anti_join(Q1Srestidy, stop_words)
#Q1Sres_counts <- count(Q1Sresclean, word, sort = TRUE)
#wordcloud2(Q1Sres_counts)


######################################################################################
#remove words
remove_words <-data.frame("word"= c("coded", "program", "6th", "5th", "3rd", "4th", "7th", "0","1","10","	
1st", "27","null", "NA", "na", "coding", "ive", "that's", "code", "grade", "2", "	
2015", "2021", "2022", "30", "3rd", "50", "6", "9", "90", "experience", "learned", "called", "candy", "chapman", "forgot", "floor", "lake", "taught", "parents", "taking", "stuff", "simple", "east", "hart"))

Q1Srestidy<-unnest_tokens(Omit, word, Experience_Type, to_lower = FALSE)

Q1Sres_remove<- anti_join(Q1Srestidy,remove_words) #remove repeat words

Q1Sresclean<-anti_join(Q1Sres_remove, stop_words)
words <- filter(Q1Sresclean, "word" )


#Q1Sres_counts <- words %>%
#  rename(original = words) %>%
#  group_by(original) %>%
#  summarise(count = n()) %>%
#  ungroup() %>% # so we can mutate word
#  mutate(suggestion = correct_spelling(original)) %>%
#  filter(suggestion != original)


Q1Sres_counts <- count(Q1Sresclean, word, sort = TRUE)

Q1results <- filter(Q1Sres_counts, n > 1)


library(RColorBrewer)
color_range_number <- length(unique(Q1results$word))
color <- colorRampPalette(brewer.pal(9,"Blues")[3:7])(color_range_number)[factor(Q1results$word)]

hw <-wordcloud2(Q1results, color=color, size=2)

hw
##Export Word Cloud

#hw <- wordcloud2(Q1results,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 1200, delay =10)




Q1results %>%
  filter(n > 5) %>% # keep rows with word counts greater than 500
  mutate(word = reorder(word, n)) %>% #reorder the word variable by n and replace with new variable called word
  ggplot(aes(n, word)) + # create a plot with n on x axis and word on y axis
  geom_col()+ # make it a bar plot
  geom_bar(position= position_dodge(),stat="identity", fill= "#9ECAE2", colour='black', size=.3) +
  theme_classic()+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  ggtitle("GGEE Summer 2023 Student Prior Experience")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30), n.breaks=10)+
  xlab("Count")+
  ylab("Words")

ggsave(
  filename = "GGEE_23_Summer_Prior_Experience_Graph.png",
  plot = last_plot(),
  device = "png",
  path = "/Users/kristadulany/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses",
  scale = 2,
  width = 6,
  height = 4,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)



##############################################################################

##SENTIMENT ANALYSIS - PREVIOUS EXPERIENCE

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
loughran <- get_sentiments("loughran")

sentiment_afinn <- inner_join(Q1Sresclean, afinn, by = "word")
sentiment_bing <- inner_join(Q1Sresclean, bing, by = "word")
sentiment_nrc <- inner_join(Q1Sresclean, nrc, by = "word")
sentiment_loughran <- inner_join(Q1Sresclean, loughran, by = "word")

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_afinn <- count(sentiment_afinn, value, sort = TRUE)
summary_nrc <- count(sentiment_nrc, sentiment, sort = TRUE)
summary_loughran <- count(sentiment_loughran, sentiment, sort = TRUE)

summary_bing2 <- sentiment_bing %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)

summary_afinn %>%
  ggplot(aes(x="", y=n, fill=value)) +
  geom_bar(width = .6, stat = "identity") +
  labs(title = "Sentiment Values of Teacher Interviews",
       subtitle = "") +
  coord_polar(theta = "y") +
  theme_void()

summary_afinn2 <- sentiment_afinn %>% 
  group_by(Student_Code) %>% 
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive")) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "AFINN")

summary_bing2 <- sentiment_bing %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "bing")

summary_nrc2 <- sentiment_nrc %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "nrc") 

summary_loughran2 <- sentiment_loughran %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "loughran") 

summary_sentiment <- bind_rows(summary_afinn2,
                               summary_bing2,
                               summary_nrc2,
                               summary_loughran2) %>%
  arrange(method, Student_Code) %>%
  relocate(method)

summary_sentiment

total_counts <- summary_sentiment %>%
  group_by(Student_Code) %>%
  summarise(total = sum(n))

sentiment_counts <- left_join(summary_sentiment, total_counts)

sentiment_counts

sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

sentiment_percents

sentiment_percents %>%
  ggplot(aes(x = Student_Code, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  coord_flip() +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "Describe your experiences in completing the camp activities",
       x = "Student Codes", 
       y = "Percentage of Words")

summary_nrc2 %>%
  ggplot(aes(x = n, y = sentiment, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "Describe your experiences in completing the camp activities",
       x = "Number of Words", 
       y = "Sentiment")


summary_bing2 %>%
  ggplot(aes(x = n, y = sentiment, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "Describe your experiences in completing the camp activities",
       x = "Number of Words", 
       y = "Sentiment")

summary_afinn %>%
  ggplot(aes(x = value, y = n, fill=value)) +
  geom_bar(width = .8, stat = "identity") +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "Describe your experiences in completing the camp activities",
       x = "Sentiment Value", 
       y = "Number of Words")

