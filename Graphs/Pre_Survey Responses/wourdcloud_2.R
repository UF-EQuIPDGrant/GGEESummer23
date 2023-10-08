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



Experience_Cloud <- read_excel("Graphs/Pre_Survey Responses/ExperienceSheet_Fin.xlsx")


test <- wordcloud2(Experience_Cloud)


library(RColorBrewer)
color_range_number <- length(unique(Experience_Cloud$word))
color <- colorRampPalette(brewer.pal(9,"Blues")[3:7])(color_range_number)[factor(Experience_Cloud$word)]

cloud <- wordcloud2(Experience_Cloud, color=color, size=2)
cloud

##Export Word Cloud

saveWidget(cloud,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 1200, delay =10)
