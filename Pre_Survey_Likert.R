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


####################################################################################################################


install.packages("HH")
install.packages("lattice")

library(psych)
library(likert)
library(HH)
library(ggplot2)


pre_survey <- read_excel("Data/GGEE_23_PreSurvey.xlsx", sheet = 1)

graph1<-likert(Item~., pre_survey, ReferenceZero=3, ylab = "Statement", xlab = "Percentage", main = list("End of Day: Student Sentiments", x=unit(.62, "npc")), auto.key = list(columns = 2, reverse.rows = T))

graph1

##png("/Users/kristadulany/Documents/GitHub/GGEE/Data/Graph1.png",
# height=720, width=1080)
##graph1
##dev.off()

###MAKE DATA FRAME BY HAND
Item <- c("I felt confident when completeing today's camp activites", "I enjoyed completing today's camp activities", "I find today's camp activties difficult")

Strongly_Disagree <-c(1.42, 1.42, 23.49)
Somewhat_Disagree <- c(0.71, 1.42, 22.42)
Neither <- c(4.96, 3.90, 22.06)
Somewhat_Agree <- c(27.66, 23.05, 25.98)
Strongly_Agree <- c(65.25, 70.21, 6.05)

df <- data.frame(Item, Strongly_Disagree, Somewhat_Disagree, Neither, Somewhat_Agree, Strongly_Agree)
view(df)

plot(likert(summary = df), plot.percent.neutral=FALSE, legend.position="right")

