RETIRED SCRIPTS FROM REPORT
  
  
  
####GRAPHS IN R MARKDOWN ``` 

````{r District, echo=FALSE, }
GGEE_23_Camps <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 1)
#View(GGEE_23_Camps)
GGEE_23_Camps_Intro <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 2)
GGEE_23_Camps_Adv <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 3)


All_Students <- sum(GGEE_23_Camps$n)
Intro_Students <- sum(GGEE_23_Camps_Intro$n)
Adv_Students <- sum(GGEE_23_Camps_Adv$n)

#District All
Camps_all <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 4)

df_all <- Camps_all%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


District_all <- ggplot(Camps_all, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District for All Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df_all,
                   aes(y = pos, label = paste0(round(n/All_Students*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5))


Camps_intro <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 5)

df_intro <- Camps_intro%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


District_intro <- ggplot(Camps_intro, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District in Introductory Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df_intro,
                   aes(y = pos, label = paste0(round(n/Intro_Students*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5))


Camps_adv <- read_excel("Data/GGEE_23_Camps.xlsx", sheet = 6)

df_adv <- Camps_adv%>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


District_Adv <- ggplot(Camps_adv, aes(x="", y=n, fill=Dist)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(guide = guide_legend(reverse = FALSE),palette = "Blues")+
  ggtitle("Students Per District in Advanced Programs")+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5))+
  geom_label_repel(data = df_adv,
                   aes(y = pos, label = paste0(round(n/Adv_Students*100, digits=1), "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE)+
  theme(legend.position = c(1.12, .5))

```

<center>
  ````{r District_all, echo=FALSE, }
District_all
```

</center>
  
  <center>
  ```{r District_in_out, echo=FALSE, fig.show = "hold", out.width = "50%"}

District_intro
District_Adv
```


</center>