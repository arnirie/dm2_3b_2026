#text mining, sentiment/emotion analysis

library(tidyverse)
library(tidytext)
library(lubridate)
library(tm)
library(wordcloud)

#dir & file
setwd('/Users/arnirie/Downloads')
filename <- 'psu_lgu_feedback_simulated.csv'

feedback_raw <- read_csv(filename, show_col_types = FALSE) |>
  mutate(
    date = mdy(date)
  )
feedback_raw

stop_filipino <- data.frame(
  word = c('ang', 'mga', 'ng', 'at', 'po', 'opo', 'sa', 'rin', 'ba', 'lang', 'na', 'ito', 'doon', 'dito', 'ako', 'ikaw'),
  lexicon = 'filipino'
)

#clean
clean_comments <-feedback_raw |> select(comment_id, comment, language, location) |>
  unnest_tokens(word, comment) |>
  mutate(word = str_to_lower(word)) |>
  mutate(word = str_replace_all(word, '[^a-z]', '')) |> 
  filter(word != '', nchar(word) >= 2) |>
  anti_join(stop_words, by = 'word') |>
  anti_join(stop_filipino, by = 'word')
clean_comments

#word count
word_counts <- clean_comments |> count(word, sort = TRUE) |>
  filter(n > 5)

#word cloud
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  random.order = FALSE,
  scale = c(3,0.5),
  max.words = 50,
  colors = brewer.pal(5, "Dark2")
)

#word association
gutom_context <- clean_comments |> filter(word == 'gutom') |>
  inner_join(clean_comments, by = 'comment_id') |>
  count(word.y, sort = TRUE) |>
  filter(word.y != 'gutom')
gutom_context


#sentiment analysis
sentiment_bing <- get_sentiments('bing')
sentiment_results <- feedback_raw |> select(comment_id, comment) |>
  unnest_tokens(word, comment) |>
  inner_join(sentiment_bing, by = 'word' ) |>
  count(comment_id, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    net_sentiment = positive - negative,
    sentiment_category = case_when(
      net_sentiment > 0 ~ 'positive',
      net_sentiment < 0 ~ 'negative',
      TRUE ~ 'neutral'
    )
  )

sentiment_summary <- sentiment_results |> count(sentiment_category, sort = TRUE)
sentiment_summary

#emotion
emotion_results <- get_nrc_sentiment(feedback_raw$comment) |>
  mutate(
    comment_id = feedback_raw$comment_id
  ) |>
  pivot_longer(cols = -comment_id, names_to = 'emotion', values_to = 'score') |>
  group_by(emotion) |>
  summarise(total_score = sum(score)) |>
  arrange(desc(total_score))
emotion_results

#plot