# Tidy Tuesday 2025 Code
# Week 2: posit::conf talks


# setup 

install.packages("tidyverse")
install.packages("forcats")
install.packages("devtools")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("webshot")
library(tidyverse)
library(forcats)
library(dplyr)
library(tidytext)
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2) 

# import data 

conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')
conf2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

# clean variable names and join data

conf2023_clean <- conf2023 %>%
      select(speaker_name, session_type, session_title, session_abstract) %>%
      rename(track = session_type, talk_title = session_title, description = session_abstract) %>%
      mutate(year = 2023)

conf2024_clean <- conf2024 %>%
      select(speaker_name, track, talk_title, description) %>%
      mutate(year = 2024)

# recode track for 2024

conf2024_standard <- conf2024_clean %>%
      mutate(track = fct_collapse(track,
          "keynote" = "Keynote",
          "lightning" = "Lightning Talks",
          "regular" = unique(track)[!unique(track) %in% c("Keynote", "Lightning Talks")]))

posit_23_24 <- bind_rows(conf2023_clean, conf2024_standard)

# remove duplicates

posit_23_24_clean <- posit_23_24 %>%
      distinct(track, talk_title, description, year)

posit_23_24_clean

# data viz - convert to word count
# for 2023 conference

tidy_text_data_23 <- posit_23_24_clean %>%
      filter(year == 2023) %>% 
      unnest_tokens(word, description)

tidy_text_posit_23 <- tidy_text_data_23 %>%
  anti_join(stop_words)

word_count_23 <- tidy_text_posit_23 %>%
  count(word, sort = TRUE)

tidy_text_posit_23 %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# for 2024 conference

tidy_text_data_24 <- posit_23_24_clean %>%
  filter(year == 2024) %>% 
  unnest_tokens(word, description)

tidy_text_posit_24 <- tidy_text_data_24 %>%
  anti_join(stop_words)

word_count_24 <- tidy_text_posit_24 %>%
  count(word, sort = TRUE)

tidy_text_posit_24 %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

word_count_clean_23 <- word_count_23 %>%
  filter(n >= 5) %>%
  rename(freq = n)

word_count_clean_24 <- word_count_24 %>%
  filter(n >= 5) %>%
  rename(freq = n)

# data viz and final output

letterCloud(data = word_count_clean_23, word = "Posit23", color="white", backgroundColor="black")
letterCloud(data = word_count_clean_24, word = "Posit24", color="white" , backgroundColor="black")

