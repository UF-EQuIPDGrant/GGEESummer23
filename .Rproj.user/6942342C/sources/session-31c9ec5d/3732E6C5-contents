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

Q1Sres <- read_excel("Data/Student Interviews_Dulany.xlsx", sheet=1)
Q1SresSel<- select(Q1Sres, Student_Code, Quote)

remove_words <-data.frame("word"= c("lot", "um", "it's","it", "uh", "ive", "that's"))

Q1Srestidy<-unnest_tokens(Q1SresSel, word, Quote)

Q1Sres_remove<- anti_join(Q1Srestidy,remove_words) #remove repeat words

Q1Sresclean<-anti_join(Q1Sres_remove, stop_words)

Q1Sres_counts <- count(Q1Sresclean, word, sort = TRUE)

library(wordcloud2)
wordcloud2(Q1Sres_counts)

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

