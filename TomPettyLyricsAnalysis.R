#If never installed packages, run through the codes below:
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("tidytext")
#install.packages("textdata")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("formattable")
#install.packages("tidyr")
#install.packages("widyr")
#install.packages("ggrepel")
#install.packages("circlize")
#install.packages("yarrr")
#install.packages("radarchart")
#install.packages("igraph")
#install.packages("ggraph")

#load libraries needed and import the data (change data file directory if needed)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidytext)
library(textdata)
library(knitr)
library(kableExtra)
library(formattable)
library(tidyr)
library(widyr)
library(ggrepel)
library(circlize)
library(yarrr)
library(radarchart)
library(igraph)
library(ggraph)
tp <- read.csv("tompettylyrics.csv", stringsAsFactors = FALSE)

#Data Preparation
fix.contractions <- function(doc) 
{doc <- gsub("won't", "will not", doc)
doc <- gsub("can't", "can not", doc)
doc <- gsub("n't", " not", doc)
doc <- gsub("'ll", " will", doc)
doc <- gsub("'re", " are", doc)
doc <- gsub("'ve", " have", doc)
doc <- gsub("'m", " am", doc)
doc <- gsub("'d", " would", doc)
doc <- gsub("'s", "", doc)
return(doc)}

#Brief view
tp$Lyrics <- sapply(tp$Lyrics, fix.contractions)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
tp$Lyrics <- sapply(tp$Lyrics, removeSpecialChars)
tp$Lyrics <- sapply(tp$Lyrics, tolower)
summary(tp)

my_colors <- c("#FFBE4F", "#6BD2DB", "#0EA7B5", "#0C457D", "#E8702A")
theme_lyrics <- function()
{theme(plot.title = element_text(hjust = 0.5),
       axis.text.x = element_blank(),
       axis.ticks = element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.position = "none")}

#Filter the lyrics
undesirable_words <- c("yeah", "the", "baby", "and", "you","out", "this","boy", 
                       "but", "into", "gonna","for","not", "with","this")
tp_words_filtered <- tp %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words) %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 2)

#Count the words
full_word_count <- tp %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Title, Album) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#Table of 10 songs with most words
full_word_count[1:9,] %>%
  ungroup(num_words, Album) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(Album = color_tile("lightpink","lightpink")(Album)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Word Count for Each Song") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)

#Word count distribution
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = Album )) +
  ylab("Song Count") +
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())


#Most frequent words
tp_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") +
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words in Tom Petty Solo Song Lyrics") +
  coord_flip()

#Most frequent words (grouped by Albums)
popular_words <- tp_words_filtered %>%
  group_by(Album) %>%
  count(word, Album, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Album,n) %>%
  mutate(row = row_number())
popular_words %>%
  ggplot(aes(row, n, fill = Album)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Popular Words by Albums") +
  theme_lyrics() +
  facet_wrap(~Album, scales = "free") +
  scale_x_continuous( 
    breaks = popular_words$row,
    labels = popular_words$word) +
  coord_flip()

#Word length distribution
tp_word_lengths <- tp %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Title,Album) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word))
tp_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length),
         binwidth = 10) +
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 2),
                 show.legend = FALSE) +
  xlab("Word Length") +
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

## Lexical Diversity Plot 
lex_diversity_per_year <- tp %>% 
  unnest_tokens(word, Lyrics) %>% 
  group_by(Title,ï..Year) %>% 
  summarise(lex_diversity = n_distinct(word)) %>% 
  arrange(desc(lex_diversity)) 
diversity_plot <- lex_diversity_per_year %>% 
  ggplot(aes(ï..Year, lex_diversity)) + 
  geom_point(color = my_colors[3], 
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") + 
  geom_smooth(aes(x = ï..Year, y = lex_diversity), se = FALSE, 
              color = "blue", lwd = 2) + 
  ggtitle("Lexical Diversity") + 
  xlab("") + 
  ylab("") + 
  scale_color_manual(values = my_colors) + 
  theme_classic() + 
  theme_lyrics() 
diversity_plot 

## Lexical Density Plot 
lex_density_per_year <- tp %>% 
  unnest_tokens(word, Lyrics) %>% 
  group_by(Title,ï..Year) %>% 
  summarise(lex_density = n_distinct(word)/n()) %>% 
  arrange(desc(lex_density)) 
density_plot <- lex_density_per_year %>% 
  ggplot(aes(ï..Year, lex_density)) + 
  geom_point(color = my_colors[4], 
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", 
              se = FALSE, 
              method = "lm") + 
  geom_smooth(aes(x = ï..Year, y = lex_density), 
              se = FALSE, 
              color = "blue", 
              lwd = 2) + 
  ggtitle("Lexical Density") + 
  xlab("") + 
  ylab("") + 
  scale_color_manual(values = my_colors) + 
  theme_classic() + 
  theme_lyrics() 
density_plot 

## Important Words using TF-IDF by Album 
tfidf_words_album <- tp %>% 
  unnest_tokens(word, Lyrics) %>% 
  distinct() %>% 
  filter(!word %in% undesirable_words) %>% 
  filter(nchar(word) > 3) %>% 
  count(Album, word, sort = TRUE) %>% 
  ungroup() %>% 
  bind_tf_idf(word, Album, n) %>% 
  arrange(desc(tf_idf)) 
top_tfidf_words_album <- tfidf_words_album %>% 
  group_by(Album) %>% 
  slice(seq_len(8)) %>% 
  ungroup() %>% 
  arrange(Album, tf_idf) %>% 
  mutate(row = row_number()) 
top_tfidf_words_album %>% 
  ggplot(aes(x = row, tf_idf, fill = Album)) + 
  geom_col(show.legend = NULL) + 
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Album") + 
  theme_lyrics() + 
  facet_wrap(~Album, 
             ncol = 3, nrow = 2, 
             scales = "free") + 
  scale_x_continuous(  # this handles replacement of row 
    breaks = top_tfidf_words_album$row, # notice need to reuse data frame 
    labels = top_tfidf_words_album$word) + 
  coord_flip() 

#Polarity
tp_bing <- tp_words_filtered %>%
  inner_join(get_sentiments("bing"))
tp_nrc <- tp_words_filtered %>%
  inner_join(get_sentiments("nrc"))
tp_nrc_sub <- tp_words_filtered %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))
tp_polarity_chart <- tp_bing %>%
  count(sentiment, Album) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)
#Polarity by album
plot1 <- tp_polarity_chart %>%
  ggplot( aes(Album, polarity, fill = Album)) +
  geom_col() +
  scale_fill_manual(values = my_colors[3:5]) +
  geom_hline(yintercept = 0, color = "red") +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity By Album")
#Percent positive by album
plot2 <- tp_polarity_chart %>%
  ggplot( aes(Album, percent_positive, fill = Album)) +
  geom_col() +
  scale_fill_manual(values = c(my_colors[3:5])) +
  geom_hline(yintercept = 0, color = "red") +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive By Album")
grid.arrange(plot1, plot2, ncol = 2)

#some interesting individual songs nrc sentiment
#A fast paced song but is negative with little words variability
tp_nrc %>%
  filter(Title %in% "The Apartment Song") %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = NULL) +
  theme_minimal() + theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("'The Apartment Song' NRC Sentiment") +
  coord_flip()
tp_words_filtered %>%
  filter(Title %in% 'The Apartment Song') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + 
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  xlab(NULL) + ylab(NULL) +
  ggtitle("'The Apartment Song' Sentiment Words") +
  coord_flip()

#Bee is categorized under anger and fear
tp_nrc %>%
  filter(Title %in% "Honey Bee") %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = NULL) +
  theme_minimal() + theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("'Honey Bee' NRC Sentiment") +
  coord_flip()
tp_words_filtered %>%
  filter(Title %in% 'Honey Bee') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + 
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  xlab(NULL) + ylab(NULL) +
  ggtitle("'Honey Bee' Sentiment Words") +
  coord_flip()

#Trust but negative at the same time?
tp_nrc %>%
  filter(Title %in% "Turn This Car Around") %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = NULL) +
  theme_minimal() + theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("'Turn This Car Around' NRC Sentiment") +
  coord_flip()
tp_words_filtered %>%
  filter(Title %in% 'Turn This Car Around') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + 
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  xlab(NULL) + ylab(NULL) +
  ggtitle("'Turn This Car Around' Sentiment Words") +
  coord_flip()

#horse is categorized under trust (compare with bee) and mother involves sadness
tp_nrc %>%
  filter(Title %in% "Ankle Deep") %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = NULL) +
  theme_minimal() + theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("'Ankle Deep' NRC Sentiment") +
  coord_flip()
tp_words_filtered %>%
  filter(Title %in% 'Ankle Deep') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + 
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  xlab(NULL) + ylab(NULL) +
  ggtitle("'Ankle Deep' Sentiment Words") +
  coord_flip()