library(tidyverse)
library(RedditExtractoR)
library(wordcloud2)
library(tidytext)
library(webshot)
library(htmlwidgets)

post_urls <- find_thread_urls(keywords = 'Game Thread: Green Bay Packers vs',
                              subreddit = 'GreenBayPackers', period = "year")

game_thread_urls <-
    post_urls %>%
    select(date = date_utc, title, url) %>% #rename date_utc to date
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% # format date column
    filter(date > "2021-09-11") %>% # Only select games from 2021 season
    filter(str_detect(title,  regex("\\[Week.*[(\\d+)]"))) %>% # Select threads with [Week 1-18] in name
    filter(!str_detect(title,  regex("Pre"))) %>% # Remove pre-game threads
    filter(!str_detect(title,  regex("Post"))) %>% # Remove post-game threads
    pull(url)

game_thread_comments <- get_thread_content(game_thread_urls)[[2]]


game_thread_comments <- 
    game_thread_comments %>%
    select(url, comment)

comments_words <-
    game_thread_comments %>%
    unnest_tokens(word, comment) %>% # Create data frame where each row is a word
    anti_join(stop_words) %>% # Remove stop words
    filter(!(word %in% c("game", "à"))) %>% # Remove the word "game" and also weird character
    filter(!str_detect(word,  regex("(\\d+)"))) # Remove numbers 

### Create data frame where each row is a word and the number of times it appears
### And then select top 200 most commonly used words
comments_words_freq <-
    comments_words %>%
    count(word, name = "freq") %>%
    slice_max(freq, n = 200)

color_palette <- c("#203731", "#FFB612")
wc <-wordcloud2(comments_words_freq, 
                color=rep_len(c(color_palette), nrow(comments_words_freq)),
                size = .75)
saveWidget(wc, "tmp.html", selfcontained = F)
webshot("tmp.html", "packers_wordcloud.png", delay = 5, vwidth = 1000, vheight = 1000)
