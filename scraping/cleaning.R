# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(lubridate)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading raw data
load("players_raw.RData")

# Making all characters
players <- modify(players, function(x) x %>% mutate_all(as.character))

# Making data frame
players <- bind_rows(players)

# Removing unecessary rows
players <- players %>% filter(str_detect(Season, "[0-9]{4}\\-[0-9]{2}"))

# Fixing physics
players <- players %>% separate(physics, c("height","weight"), sep = ", ") %>% 
  mutate(height = parse_number(height), weight = parse_number(weight))

# Separating into birthday and birth location
players <- players %>% separate(born, c("birthday","birth_location"), sep = " in ")

# Removing duplicate names
players <- players %>% mutate(name = str_trim(name)) %>% 
  mutate(name = map(name, function(x) x %>% str_split(" ") %>% unlist() %>% 
                                        .[1:2] %>% str_c(collapse = " ")))

# Fixing dates
players %>% mutate()






