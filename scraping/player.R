# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(rvest)
library(janitor)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading player links
load("player_links.RData")

# Alternative strsplit
strsplit <- function(x,split,type = "remove",perl = FALSE,...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

# Defining split words for player info
split_words <- c("Pronunciation","Twitter","Position","Shoots","Born","College",
                 "High School","Draft","NBA Debut","Hall of Fame") %>% 
  str_c(., ":") %>% str_c(collapse = "|")

# Function to scrape information
scrape_player_profile <- function(url){
  player_page <- try(read_html(url))
  if (any(class(player_page) == "try-error")){
    Sys.sleep(10)
    player_page <- try(read_html(url))
  }
  top_info <- player_page %>% html_nodes("#meta") %>% html_text() %>% 
    str_split("\n\n\n\n|▪") %>% unlist() %>% 
    str_remove_all("\n|\t") %>% str_squish() %>% 
    strsplit(split_words, type = "before") %>% 
    unlist() %>% .[-length(.)]
  
  # Fixing name
  top_info[1] <- str_c("Name: ", top_info[1])
  
  # Getting height and weight
  top_info[str_detect(top_info, "[0-9]{3}cm")] <- top_info %>% 
    str_subset("[0-9]{3}cm") %>% 
    str_extract("\\([^()]+\\)") %>% str_remove_all("\\(|\\)") %>% 
    str_c("Physics: ",.)
  
  # Removing nicknames
  top_info <- top_info %>% str_subset("^\\(.+\\)", negate = TRUE)
  
  col_names <- top_info %>% str_split(":") %>% map(~ .[1]) %>% unlist()
  top_info <- top_info %>% str_split(":") %>% map(~ .[2]) %>% unlist()
  
  top_info <- matrix(top_info, ncol = length(top_info)) %>% as.data.frame() %>% 
    setNames(col_names) %>% clean_names()
  
  # Getting totals table
  player_page %>% html_table() %>% .[[1]] %>% cbind(top_info, .)  
}

players <- list()
tictoc::tic()
for (i in 1:length(player_links)) {
  players[[i]] <- scrape_player_profile(player_links[i])
  print(i/length(player_links))
}
tictoc::toc()

save(players, file = "players_raw.RData")


