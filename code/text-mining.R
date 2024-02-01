# tuohmas, 19 Sept, 2023
#
# Script for text mining corporate blog posts by major social media platforms

# Clean the environment
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               tidytext, # for tokenizing text
               textstem) # for lemmatizing tokens

pacman::p_load(tidyverse,
               quanteda)



# Read search terms
misinfo_search <- readLines("data/misinfo_search_terms_lemmatized.txt")
reliable_search <- readLines("data/reliable_info_search_terms_lemmatized.txt")
covid_search <- readLines("data/covid_search_terms_lemmatized.txt")

misinfo_search
reliable_search
covid_search

# Read blog document data
document_data <- read_csv("data/platformblogs.csv") %>%
  mutate(company = str_to_title(company)) %>%
  mutate(doc_id = 1:nrow(.)) %>% # Add identifier
  relocate(doc_id, 1)

# Glimpse data
glimpse(document_data)

keep_cols <- c("doc_id", "company", "title", "text")

# Preprocess the corpus
document_tokens <- document_data %>% select(keep_cols) %>%

  # Tokenize
  # quanteda::tokens(text, what = "word",
  #                  remove_punct = TRUE,
  #                  remove_symbols = TRUE)
  unnest_tokens(word, text) %>%

  # Lemmatize tokens using hunspell (language: en_US)
  mutate(lemma = lemmatize_words(word),

         # remove numbers and punctuation
         lemma = gsub("[[:digit:]]|[[:punct:]]", NA, lemma)) %>%

  # Remove empty rows after gsub
  filter(!is.na(lemma))

# Glimpse and View document data
glimpse(document_tokens)
View(document_tokens)

# Dataframe for lemmas and term frequencies per document
document_words <- document_tokens %>%
  count(title, lemma, sort = TRUE, name = "count") %>%

  # Match lemma to corresponding search term group
  mutate(group = case_when(lemma %in% covid_search ~ "Covid",
                           lemma %in% misinfo_search ~ "Misinformation",
                           lemma %in% reliable_search ~ "Reliable")) %>%
  arrange(desc(group))

# Glimpse and View document search terms data
glimpse(document_words)
View(document_words)

# ADD drop stop words
stopwords <- stopwords(language = "en")
stopwords <- stopwords(language = "en", source = "hunspell") # Try another engine

?stopwords()

anti_join()

# Dataframe for search term frequencies
search_documents <- document_words %>%
  filter(!is.na(group)) %>%   # Drop NA group
  group_by(title, group) %>%  # Group data
  summarise(search_term_frequency = sum(count)) %>% # Add search term freq's
  spread(group, search_term_frequency) # Long to wide format, keep NA

# Glimpse and View data
glimpse(search_documents)
View(search_documents)

# Calculate tf-idf statistic
document_words <- document_words %>%
  bind_tf_idf(lemma, title, count)

# Extract keywords per document
keyword_data <- document_words %>%
  group_by(title) %>%

  # Slice maximum of 5 kw's by maximum tf-idf value
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%

  # Aggregate top keywords per document
  aggregate(lemma ~ title, ., function(x) paste(x, collapse = ", ")) %>%

  # Rename
  rename("top_keywords" = "lemma")

# Glimpse data
glimpse(keyword_data)

# Merge datasets
master_data <- document_data %>%
  group_by(title) %>%
  slice(1L) %>%
  left_join(search_documents, ., by = "title") %>%
  select(company, date, url, title, Covid, Misinformation, Reliable) %>%
  left_join(keyword_data, by = "title") %>%
  arrange(match(company, c("Twitter", "Meta", "Youtube")), date)

# Glimpse master dataset
glimpse(master_data)
View(master_data)

# Add included field in platform blog data

