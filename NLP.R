library(readtext)
library(quanteda)
library(lexicon)
library(seededlda)

#Load the data
DIR <- getwd()
comments <- readtext(paste(DIR, '/comments_2009_volkswagen.txt', sep=''))

#Create the corpus
comments_corpus <- corpus(comments)

#Tokenize
tokens <- tokens(comments_corpus,
                 remove_punct = TRUE,
                 remove_numbers = TRUE)

#Delete stopwords
tokens_nostop <- tokens_select(tokens, pattern = stopwords('en'), selection = 'remove')

#Lemmatize words
tokens_lemmatize <- tokens_replace(tokens_nostop, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)

#Create feature matrix
df_comments <-  dfm(tokens_lemmatize) %>%
                dfm_trim(min_termfreq = 0.6, termfreq_type = 'quantile')

#Extract topics
#LDA
topics_comments <- textmodel_lda(df_comments, k = 5)

terms(topics_comments, 10)

#Search for some words that are useless for our analysis
kw <- kwic(tokens_lemmatize, pattern = 'car')
#743 appearance
kw <- kwic(tokens_lemmatize, pattern = 'vw')
#202 appearance
kw <- kwic(tokens_lemmatize, pattern = 'passat')
#62 appearance
kw <- kwic(tokens_lemmatize, pattern = 'tiguan')
#79 appearance
kw <- kwic(tokens_lemmatize, pattern = 'routan')
#47 appearance

useless_tokens = list('car', 'vw', 'passat', 'tiguan', 'routan')

#Delete useless tokens
tokens_lemmatize_cleaned = tokens_select(tokens_lemmatize, pattern =c(useless_tokens) ,selection = 'remove')

#Re-train the lda model
df_comments <-  dfm(tokens_lemmatize_cleaned) %>%
  dfm_trim(min_termfreq = 0.7, termfreq_type = 'quantile')

topics_comments <- textmodel_lda(df_comments, k = 5)

terms(topics_comments, 10)

#Different nº of clusters
topics_comments <- textmodel_lda(df_comments, k = 3)
terms(topics_comments, 10)

topics_comments <- textmodel_lda(df_comments, k = 7)
terms(topics_comments, 10)
