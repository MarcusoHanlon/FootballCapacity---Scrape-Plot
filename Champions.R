library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)
library(magrittr)
library(tidyr)
library("reshape2")

# - - - - -
# GET DATA
# - - - - -

url <- "https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_finals"
df <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[4]') %>%
  html_table() %>%
  data.frame()

#---
#Clean
#---

names(df)[1] <- "Club"
names(df)[3] <- "Runner_Up"

df$Percentage <- ((df$Winners) / (df$Runner_Up + df$Winners)) * 100

dfc <- df[1:20,] %>%
	transform(.,Club = reorder(Club, Winners))%>%
      gather(., "Type", "value", c(2,3))

dfc <- dfc %>% mutate(
  Club = reorder(Club, value),
  Percentage = sprintf("%.0f%%", Percentage)
)

View(dfc)
ggp <- ggplot(data = dfc, aes(x = Club)) + 
  geom_bar(
      aes(y = value, alpha = Type), 
      stat="identity", 
      position="stack"
) + 
  geom_text(colour = "white", y = 0.5, aes(label = Percentage)) + 
  coord_flip() +
  scale_alpha_manual(values=c(1, .3)) +
  theme(axis.text.x  = element_text(size=14),
  axis.text.y  = element_text(hjust=1, size=12)) +
  ylab("CL Trophies (and Runner Up)") +
  xlab("Club") + 
  ggtitle("UEFA Champions League Record")

ggp

getwd()
setwd("C:/Users/marcus.ohanlon/Documents/GitHub/FootballCapacity---Scrape-Plot")

ggsave("CLFinal.png", ggp, width=14, height=10, units="in")
