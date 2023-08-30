library(tidyverse)
library(tidytext)
library(dplyr)
library(readr)
library(tidyr)
library(rtweet)
library(writexl)
library(readxl)
library(tidytext)
library(textdata)
library(ggplot2)
library(textdata)
library(scales)

teacherresponses <- read_excel("~/Documents/GitHub/EQuIPD Data/teacher interviews1.xlsx")
teacherresclean<- select(teacherresponses, Code, Quote)
teacherstidy<-unnest_tokens(teacherresclean, word, Quote)
teacherclean<-anti_join(teacherstidy, stop_words)
teacher_counts <- count(teacherclean, word, sort = TRUE)
library(wordcloud2)
wordcloud2(teacher_counts)

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
loughran <- get_sentiments("loughran")

sentiment_afinn <- inner_join(teacherclean, afinn, by = "word")
sentiment_bing <- inner_join(teacherclean, bing, by = "word")
sentiment_nrc <- inner_join(teacherclean, nrc, by = "word")
sentiment_loughran <- inner_join(teacherclean, loughran, by = "word")

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_afinn <- count(sentiment_afinn, value, sort = TRUE)
summary_nrc <- count(sentiment_nrc, sentiment, sort = TRUE)
summary_loughran <- count(sentiment_loughran, sentiment, sort = TRUE)

summary_bing <- sentiment_bing %>% 
  group_by(Code) %>% 
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
  group_by(Code) %>% 
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive")) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "AFINN")

summary_bing2 <- sentiment_bing %>% 
  group_by(Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "bing")

summary_nrc2 <- sentiment_nrc %>% 
  filter(Code %in% c("positive", "negative")) %>%
  group_by(Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "nrc") 

summary_loughran2 <- sentiment_loughran %>% 
  filter(Code %in% c("positive", "negative")) %>%
  group_by(Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "loughran") 

summary_sentiment <- bind_rows(summary_afinn2,
                               summary_bing2,
                               summary_nrc2,
                               summary_loughran2) %>%
  arrange(method, Code) %>%
  relocate(method)

summary_sentiment

total_counts <- summary_sentiment %>%
  group_by(Code) %>%
  summarise(total = sum(n))

sentiment_counts <- left_join(summary_sentiment, total_counts)

sentiment_counts

sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

sentiment_percents

sentiment_percents %>%
  ggplot(aes(x = Code, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  coord_flip() +
  labs(title = "Sentiment of Teacher Responses", 
       subtitle = "Goldberg Gator Engineering Explorers",
       x = "Teacher Codes", 
       y = "Percentage of Words")
