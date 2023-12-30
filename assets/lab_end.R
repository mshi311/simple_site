#https://www.chicagotribune.com/sports/bears/ct-cb-chicago-bears-brad-biggs-10-thoughts-20220110-ikzhm3kjinbbnkkecec4hvcarq-story.html
#https://www.chicagotribune.com/sports/bears/ct-cb-chicago-bears-brad-biggs-10-thoughts-20211221-rxz7gfm7djdtjio62ku5oplzh4-story.html
#https://www.chicagotribune.com/sports/bears/ct-cb-chicago-bears-10-thoughts-los-angeles-rams-week-1-20210913-3hh2f2emqzgqhhqjg2lsoa2plm-story.html
install.packages("textdata")
install.packages("tidytext")
library(tidyverse)
library(tidytext)

library(textdata)

setwd("/Volumes/GoogleDrive-115002132805773023506/.shortcut-targets-by-id/1_z_w4gIH2mbwIFb4uUwgRk8rvcJZud8a/Teaching/DAP II - Levy/Lecture Notes Data Skills 2 R/Lecture 9 - nlp 1")

s1 <- read_file("bears_10_thoughts_sep21.txt")
s15 <- read_file("bears_10_thoughts_dec21.txt")
s18 <- read_file("bears_10_thoughts_jan22.txt")
text_df1 <- tibble(text = s1)
text_df15 <- tibble(text = s15)
text_df18 <- tibble(text = s18)
word_tokens_df1 <- unnest_tokens(text_df1, word_tokens,  text, token = "words")
word_tokens_df15 <- unnest_tokens(text_df15, word_tokens,  text, token = "words")
word_tokens_df18 <- unnest_tokens(text_df18, word_tokens,  text, token = "words")
word_tokens_df1$season <- "After Week 1"
word_tokens_df15$season <- "After Week 15"
word_tokens_df18$season <- "After Week 18"
df <- rbind(word_tokens_df1, word_tokens_df15, word_tokens_df18)

#or load data all together: 
# unnest_tokens(tibble(text = c(s1, s15, s18),
#                      doc = c("After Week 1", "After Week 15", "After Week 18")),
#               word_tokens,
#               text,
#               token = "words")


df <- df %>%
  left_join(get_sentiments("nrc"), by = c("word_tokens" = "word")) %>%
  rename(nrc = sentiment) %>%
  anti_join(stop_words, by = c("word_tokens" = "word"))

ggplot(data = filter(df, !is.na(nrc))) +
  geom_histogram(aes(nrc, fill = season), position = "dodge", stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "NRC Sentiment: 10 Thoughts About the Bears",
       x = element_blank(), y = "Count of words in NRC group") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal() +
  theme(legend.title = element_blank())
