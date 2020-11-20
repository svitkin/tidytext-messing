rm(list = ls())
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(gutenbergr)
library(tidyr)
library(scales)
library(ggplot2)
data("stop_words")

# Emily Dickinson Poem
text <- c(
  "Because I could not stop for Death -",
  "He kindly stopped for me -",
  "The Carriage held but just Ourselves -",
  "and Immortality"
)

text_df <- tibble(line = 1:4, text = text)

text_df %>% 
  unnest_tokens(word, text)

# Jane Austen
original_books <- 
  austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                            ignore_case = TRUE
    )))
  ) %>%
  ungroup()

tidy_books <- 
  original_books %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

# Gutenberg Books

# Let's get some Sherlock Holmes
sherlock <-
  gutenberg_metadata %>% 
  filter(author == "Doyle, Arthur Conan", 
         has_text, 
         language == "en", 
         gutenberg_bookshelf == "Detective Fiction") %>% 
  distinct(title, .keep_all = TRUE) %>% 
  pull(gutenberg_id) %>% 
  gutenberg_download()

# Let's get some Agatha Christie
christie <-
  gutenberg_metadata %>% 
  filter(author == "Christie, Agatha", 
         has_text, 
         language == "en", 
         gutenberg_bookshelf == "Detective Fiction") %>% 
  distinct(title, .keep_all = TRUE) %>% 
  pull(gutenberg_id) %>% 
  gutenberg_download()

# Clean both datasets up
tidy_sherlock <-
  sherlock %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

tidy_christie <-
  christie %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

# Combine both datasets and group by author
tidy_detective_frequency <-
  bind_rows(tidy_sherlock %>% mutate(author = "Doyle"),
            tidy_christie %>% mutate(author = "Christie")) %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) 

# Compare the frequency of words in each
ggplot(tidy_detective_frequency, aes(Christie, Doyle)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(low = "darkslategray4", high = "gray75") +
  theme_bw()