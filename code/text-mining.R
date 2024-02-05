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
               quanteda,
               tm,
               tidytext) # for expanded stopwords lexicon

# Error in UseMethod("pivot_longer") :
# no applicable method for 'pivot_longer' applied to an object of class "tokens"

# Read blog document data, add unique identifier to documents
document_data <- read_csv("data/platformblogs.csv") %>%

  mutate(company = str_to_title(company),
         doc_id = 1:nrow(.)) %>% # Add identifier

  relocate(doc_id, 1) # Relocate identifier to index 1

# Clean the endings of text column
document_data <- document_data %>%
  mutate(text = gsub("\\. NA$", "", text)) %>%

  # remove hyphens: fact-check -> factcheck, covid-19 -> covid19
  mutate(text = gsub("-+", "", text)) %>%

  # remove em dashes
  mutate(text = gsub("—", " ", text)) %>%

  # Remove diacritics
  mutate(text = stringi::stri_trans_general(text, "Latin-ASCII")) %>%

  # trim whitespaces
  mutate(text = trimws(text, "both"))

# Glimpse data
glimpse(document_data)

# Lemmatize tokens using lemma table from lexicon::hash_lemmas$token
token_table <- lexicon::hash_lemmas$token
lemma_table <- lexicon::hash_lemmas$lemma

document_tokens <- document_data %>% select(doc_id, text) %>%
  # select(all_of(keep_cols)) %>%

  quanteda::corpus() %>% # Create a corpus object

  # Tokenize: remove punctuation, numbers and symbols
  quanteda::tokens(what = "word",
                   remove_punct = TRUE,
                   remove_numbers = TRUE,
                   remove_symbols = TRUE,
                   remove_url = TRUE) %>%

  # Lemmatize tokens
  tokens_replace(pattern = token_table,
                 replacement = lemma_table, case_insensitive = TRUE) %>%

  # Convert to dataframe with counts via creating a document-frequency matrix
  dfm() %>% convert(to = "data.frame") %>%

  # Pivot longer
  pivot_longer(cols = c(!doc_id), names_to = "tokens", values_to = "freq") %>%

  # Filter out empty frequencies
  filter(freq > 0) %>%

  # Stringify doc_id
  mutate(doc_id = as.numeric(doc_id))

# Explore tokens and remove stopwords and special words
summary <- document_tokens %>% group_by(tokens) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(desc(total_freq))

View(summary)

# Additional cleaning to tokens
document_tokens <- document_tokens %>%

  # Remove missed punctuation
  filter(!grepl("@|,|:|—|\\.", tokens)) %>%

  # Remove hashtags
  filter(!grepl("^#.", tokens)) %>%

  # Remove all possessives, part 1
  mutate(tokens = gsub("’", "'", tokens)) %>%

  # Remove all possessives, part 2
  mutate(tokens = gsub("'s$", "", tokens)) %>%

  # Merge "fact-check" related words not found in lemma tables
  mutate(tokens = gsub("factcheck.+", "factcheck", tokens))

# Remove stopwords
# Additional stopwords: platform company and product names
additional_stopwords <- c("youtube", "facebook", "instagram", "messenger",
                          "whatsapp", "meta", "twitter", "google")

# Stopwords from SMART, onix and snowball lexicons, plus added
stopwords <- tidytext::stop_words %>%
  add_row(word = additional_stopwords, lexicon = "added")

# Remove stopwords
cleaned_document_tokens <- document_tokens %>%
  anti_join(stopwords, by = c("tokens" = "word"))

# Remove non-latin tokens and numbers
cleaned_document_tokens <- cleaned_document_tokens %>%
  filter(!grepl("[^\u0001-\u007F]+|<U\\+\\w+>", tokens)) %>%
  mutate(tokens = gsub("[0-9]+", "", tokens)) %>%
  mutate(tokens = trimws(tokens, "both")) %>%
  drop_na()

# glimpse summarized, cleaned tokens
summary <- cleaned_document_tokens %>% group_by(tokens) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(desc(total_freq)) %>%
  View()

### IDENTIFY KEYWORDS ##########################################################

# Read search term files
misinfo_search <- read_lines("data/misinfo_search_terms_lemmatized.txt")
reliable_search <- read_lines("data/reliable_info_search_terms_lemmatized.txt")
covid_search <- read_lines("data/covid_search_terms_lemmatized.txt")

print(misinfo_search)
print(reliable_search)
print(covid_search)

# Join tokens to document data
keep_cols <- c("doc_id", "company", "date", "title")

# Dataframe for lemmas and term frequencies per document
document_keywords <- document_data %>% select(any_of(keep_cols)) %>%

  # Join datasets
  left_join(cleaned_document_tokens, by = "doc_id") %>%

  # Count keywords per document
  count(title, tokens, sort = TRUE, name = "count") %>%

  # Match lemma to corresponding search term group
  mutate(group = case_when(tokens %in% covid_search ~ "Covid",
                           tokens %in% misinfo_search ~ "Misinformation",
                           tokens %in% reliable_search ~ "Reliable")) %>%
  arrange(desc(group))

# Glimpse and View document search terms data
glimpse(document_keywords)
View(document_keywords)

# Dataframe for search term frequencies
search_documents <- document_keywords %>%
  filter(!is.na(group)) %>%   # Drop NA group
  group_by(title, group) %>%  # Group data
  summarise(search_term_frequency = sum(count)) %>% # Add search term freq's
  spread(group, search_term_frequency) # Long to wide format, keep NA

# Glimpse and View data
glimpse(search_documents)
View(search_documents)

### Analysis: TF-IDF statistics ################################################

# Calculate tf-idf statistic
document_keywords <- document_keywords %>%
  bind_tf_idf(tokens, title, count)

# Extract keywords per document
keyword_data <- document_keywords %>%
  group_by(title) %>%

  # Slice maximum of 5 kw's by maximum tf-idf value
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%

  # Aggregate top keywords per document
  aggregate(tokens ~ title, ., function(x) paste(x, collapse = ", ")) %>%

  # Rename
  rename("top_keywords" = "tokens")

# Glimpse data
glimpse(keyword_data)
View(keyword_data)

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
