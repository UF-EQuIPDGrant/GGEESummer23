##Registration locations


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

GGEE_23_Reg_Locations <- read_excel("Data/GGEE_23_Registration Locations.xlsx")


Locations <- select(GGEE_23_Reg_Locations, District, Program)|>
  count(District)

write_xlsx(Locations,"/Users/kristachisholm/Documents/GitHub/GGEESummer23/Data/GGEE_23_Locations_2.xlsx")


GGEE_23_Locations_2 <- read_excel("Data/GGEE_23_Locations_2.xlsx")


