# tuohmas, 5 Feb, 2024
#
# Topic modeling Platform corporation blog data

# Clean the environment
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               tidytext,
               textstem,
               topicmodels,
               tm)

### TOKENIZE BLOG DATA #########################################################

# Read blog document data
document_data <- read_csv("data/platformblogs.csv") %>%
  mutate(company = str_to_title(company)) %>%
  mutate(doc_id = 1:nrow(.)) %>% # Add identifier
  relocate(doc_id, 1)

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

# Optional: make your own dictionary from the data
# lemma_dictionary <-
#   make_lemma_dictionary(document_data$text,
#                         engine = "lexicon",
#                         lang = "en")
#
# glimpse(lemma_dictionary)

# Preprocess the corpus
document_tokens <- document_data %>% select(doc_id, text) %>%

  # Tokenize by words (1-grams)
  tidytext::unnest_tokens(word, text) %>%

  # Lemmatize tokens: add a column for lemmas to compare (engine: lexicon)
  # Tip: to compare lemma dictionaries (hunspell, koRpus and lexicon),
  # and create lemmas from text, use textstem::make_lemma_dictionary
  mutate(lemma = textstem::lemmatize_words(word)) %>%

  # Remove missed punctuation
  filter(!grepl("@|,|:|—|\\.", lemma)) %>%

  # Remove hashtags
  filter(!grepl("^#.", lemma)) %>%

  # Remove all possessives, part 1
  mutate(lemma = gsub("’", "'", lemma)) %>%

  # Remove all possessives, part 2
  mutate(lemma = gsub("'s$", "", lemma)) %>%

  # Merge "fact-check" related words not found in lemma tables
  mutate(lemma = gsub("factcheck.+", "factcheck", lemma))

# Remove stopwords
# Additional stopwords: platform company and product names
additional_stopwords <- c("youtube", "facebook", "instagram", "messenger",
                          "whatsapp", "meta", "twitter", "google")

# Stopwords from SMART, onix and snowball lexicons, plus added
stopwords <- tidytext::stop_words %>%
  add_row(word = additional_stopwords, lexicon = "added")

# Remove stopwords
cleaned_document_tokens <- document_tokens %>%
  anti_join(stopwords, by = c("lemma" = "word"))

# Remove non-latin tokens and numbers
cleaned_document_tokens <- cleaned_document_tokens %>%
  filter(!grepl("[^\u0001-\u007F]+|<U\\+\\w+>", lemma)) %>%
  mutate(lemma = gsub("[0-9]+", "", lemma)) %>%
  mutate(lemma = trimws(lemma, "both")) %>%
  mutate(lemma = ifelse(nchar(lemma) == 1 & lemma != "i", NA, lemma))

# write as csv
cleaned_document_tokens %>%
  left_join(document_data[,c("doc_id","company","title","date", "category")],
            by = "doc_id") %>%
  write_csv("data/platform_blogs_tokens.csv")

################################################################################

# read blogs tokens data
blog_tokens <- read_csv("data/platform_blogs_tokens.csv")

# Glimpse dataset
glimpse(blog_tokens)

# Filter to cover only Meta
blog_tokens <- blog_tokens %>% filter(company == "Meta")

# Data contains n documents:
blog_tokens$title %>% unique() %>% length()

# Data spans time frame of x days:
span <- interval(min(blog_tokens$date), max(blog_tokens$date))
duration_days <- as.duration(span) %>% as.numeric("days")

# Cast pivot long into a sparse Document-term matrix
dtm_blogs <- blog_tokens %>%
  group_by(doc_id, lemma) %>% summarise(count = n()) %>%
  cast_dtm(doc_id, lemma, count)

# Find best fitting model by Log-likelihood; apply a function that goes
# though k values from 2 through 100
# data("AssociatedPress")

# AssociatedPress[21:31,] %>% tidy()

# best_model_loglike_df <- read_csv("data/lda_log_like_blogs.csv")
# data <- read_csv("data/lda_log_like_blogs.csv")

# Set seed to reproduce results
set.seed(987654321)

# Calculate optimal k values by measuring
k_range <- 2:100

# Initialize an empty data frame
best_model_metrics <- data.frame(topics = as.numeric(),
                                    LL = as.numeric(),
                                    perplexity = as.numeric())

# Iterate though k_range
for (k in k_range) {

  model <- LDA(dtm_blogs, k)

  LL <- logLik(model) %>% as.numeric()
  perplexity <- perplexity(model, newdata = dtm_blogs, estimate_theta = F)

  best_model_metrics <-
    rbind(best_model_metrics, data.frame(k, LL, perplexity))

   # Glimpse topic LL pairs
   glimpse(best_model_metrics)

   # Write as csv file
   write_csv(best_model_metrics, "data/lda_loglike_metablog.csv")
}

# DISCARD
# png(file = "plots/lda_log_like_blogs.png",
#     width = 1200, height = 1200)

data <- data %>%
  arrange(topics)

ggplot(data, aes(x = topics, y = LL)) +
  xlab("Number of topics") + ylab("Log-likelihood of the model") +
  geom_line() +
  theme_bw()

dev.off()

best_model <- lapply(seq(2, 100, by = 1),
                     function(k) { LDA(dtm_blogs, k) } )

best_model_loglike <-
  lapply(best_model, logLik) %>% as.matrix() %>% as.data.frame()

best_model_loglike_df <-
  data.frame(topics = c(2:10), LL = as.numeric(as.matrix(best_model_loglike)))

best_model_loglike_df %>% write_csv("data/lda_log_like_blogs.csv")

png(file = "plots/lda_log_like_blogs.png",
    width = 1200, height = 1200)

ggplot(best_model_loglike_df, aes(x = topics, y = LL)) +
  xlab("Number of topics") + ylab("Log-likelihood of the model") +
  geom_line() +
  theme_bw()

dev.off()

  # opts(axis.title.x = theme_text(vjust = -0.25, size = 14)) +
  # opts(axis.title.y = theme_text(size = 14, angle = 90))
  #


# Fit model; Set a seed so that the output of the model is predictable
blogs_lda <- LDA(dtm, k = 25, control = list(seed = 1234))
blogs_lda

# Explore topics
blog_topics <- tidy(blogs_lda, matrix = "beta")
blog_topics

# Visualize topic distributions
blog_top_terms <- blog_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%      # 15 words per topic
  ungroup() %>%
  arrange(topic, -beta)

blog_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Validation
?logLik()

################################################################################

# Tokenize with quanteda
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
  quanteda::tokens_replace(pattern = lexicon::hash_lemmas$token,
                 replacement = lexicon::hash_lemmas$lemma,
                 case_insensitive = TRUE)





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

