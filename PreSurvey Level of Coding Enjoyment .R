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
library(psych)
library(likert)
library(HH)
library(ggplot2)
install.packages("webshot")
webshot::install_phantomjs()

library(readxl)
GGEE_23_Pre_Survey_Likert <- read_excel("Data/GGEE_23_Pre_Survey_Likert.xlsx", sheet=3)
View(GGEE_23_Pre_Survey_Likert)

graph1<-likert(Item~., GGEE_23_Pre_Survey_Likert, ReferenceZero=3, 
               ylab = "Statement", 
               xlab = "Percentage", 
               main = list("Pre-Survey Level of Coding Enjoyment", 
                           x=unit(.62, "npc")), 
               borders = list(),
               col=c("#2271B5", "#6BAED6", "#EFF3FF", "#9ECAE2"),
               auto.key = list(columns = 2,reverse.rows = T,
                               reverse.rows = T,
                               rect= list(col=c("#2271B5", "#6BAED6", "#EFF3FF", "#9ECAE2"),
                                          border = "black")))

graph1

reso <- 1200
length <- 3.25*reso/72

png("", units="in",res=reso,height=length,width=length)
graph1
dev.off()



pdf("/Users/oliviarandell/Documents/GitHub/GGEESummer23/Graphs/Pre_Survey Responses/GGEE_23_Likert_PreSurvey_Enjoyment.pdf", width = 4, height = 4)
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
graph1
dev.off()
