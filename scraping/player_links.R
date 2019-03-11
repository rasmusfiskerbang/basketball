# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(rvest)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Letter urls
urls <- str_c("https://www.basketball-reference.com/players/",letters) %>% 
  str_subset("x$", negate = TRUE)

scrape_player_links <- function(url){
  read_html(url) %>% html_nodes("table") %>% 
  html_nodes("a") %>% html_attr("href") %>% 
  str_subset("players") %>% 
  str_c("https://www.basketball-reference.com",.)
}

player_links <- map(urls, scrape_player_links)
player_links <- unlist(player_links)

save(player_links, file = "player_links.RData")








