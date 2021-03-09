setwd("~/R_Cap_Stone/final/en_US/Final_release/quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.corpora")
install.packages("topicmodels")
install.packages("newsmap")
install.packages("remotes")
remotes::install_github("koheiw/Newsmap")
library("tidytext")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stopwords)
library(tm)
library(xfun)
library(textrecipes)
library(tokenizers)
library(qdap)
library(quanteda)
library(readtext)
library("gsl")
library("topicmodels")
library("caret")
library("newsmap")
library("tidyr")
library("data.table")

filenames <- dir(pattern = "[.]txt$")

set.seed(123)

#blogs
con <- file(filenames[1], "rb")
blogs <-  readLines(con, -1L, encoding="UTF-8", skipNul=TRUE) # to read the totality of the file
close(con)

# news
con <- file(filenames[2], "rb")
news <-  readLines(con, -1L, encoding="UTF-8", skipNul=TRUE) # to read the totality of the file
close(con)

# twitter
con <- file(filenames[3], "rb")
twitter <-  readLines(con, -1L, encoding="UTF-8", skipNul=TRUE) # to read the totality of the file
close(con)

sampleBlogs <-    blogs[sample(1:length(blogs), round(0.01*length(blogs)))] 
sampleNews  <-    news[sample(1:length(news), round(0.01*length(news)))]
sampleTwitter <-  twitter[sample(1:length(twitter), round(0.01*length(twitter)))]

rm(blogs, news, twitter)

sampleBlogs <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", sampleBlogs)  # remove single letters
sampleBlogs <- gsub(pattern = "[<->]", replace = " ", sampleBlogs)  # removal < and >
#sampleBlogs <- gsub(pattern = REGEX, replace = " ", sampleBlogs)  # for emoticons, symbols and transport symbol
sampleBlogs <-  str_replace_all(sampleBlogs, "[[:punct:]]", "")  # removal of special characters
sampleBlogs <- replace_symbol(sampleBlogs)

sampleNews <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", sampleNews)     # remove single letters
sampleNews <- gsub(pattern = "[<->]", replace = " ", sampleNews)  # removal < and >
sampleNews <-  str_replace_all(sampleNews, "[[:punct:]]", "")  # removal of special characters
sampleNews <- replace_symbol(sampleNews)

sampleTwitter <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", sampleTwitter)   # remove single letters
sampleTwitter <- gsub(pattern = "[<->]", replace = " ", sampleTwitter)  # removal < and >
sampleTwitter <-  str_replace_all(sampleTwitter, "[[:punct:]]", "")  # removal of special characters
sampleTwitter <- replace_symbol(sampleTwitter)

blogs <- data.table(sampleBlogs)
news <- data.table(sampleNews)
twitter <- data.table(sampleTwitter)

names(blogs) = names(news) = names(twitter) = "text"

document <- rbind(blogs, news, twitter)  # a data.frame of 42696 elements

# corpus
doc.corpus <- corpus(document)

"Corpus consisting of 1 document"
"The bruschetta however missed the mark Instead of manageab..."

# Cleaning and creating tokens

# This is the process of cleaning the text with the following actions:  
# - replace_symbol ($ becomes dollar)  
# - removePonctuation (like , . ? are skipped)  
# - bracketX (remove texts within brackets)  
# - replace_contraction (shouldn't becomes should not)  
# - replace_abreviation (Sr becomes senior)  
# - removeNumbers  (suppress as it gives valuable insight)
# - stripWhitespace (strip extra white space)
# - tolower (change to lower case)

# tokens
doc.tokens <- tokens(doc.corpus)
doc.tokens <- tokens(doc.tokens, remove_punct = TRUE)
#doc.tokens <- tokens_select(doc.tokens, stopwords('english'),selection='remove')
#doc.tokens <- tokens_wordstem(doc.tokens)    # unsure we want this
doc.tokens <- tokens_tolower(doc.tokens)

# n-grams
token_4 <- tokens_ngrams(doc.tokens, n=4L, skip = 0L, concatenator = " ")  # 53 MB  -> 87 MB
token_3 <- tokens_ngrams(doc.tokens, n=3L, skip = 0L, concatenator = " ")  # 54 MB  -> 77 MB
token_2 <- tokens_ngrams(doc.tokens, n=2L, skip = 0L, concatenator = " ")  # 45 MB  -> 50 MB

# dfm

# for the 1-gram
my_dfm <- dfm(doc.tokens)
top <- topfeatures(my_dfm, 15)
barplot(top, main= "Repartition of top words" , ylab = "Number of repetitions", las=2)

# for the 2-grams
my_dfm2 <- dfm(token_2)
top2 <- topfeatures(my_dfm2, 15)
barplot(top2, main= "Repartition of top words" , ylab = "Number of repetitions", las=2)

# for the 3-grams
my_dfm3 <- dfm(token_3)
top3 <- topfeatures(my_dfm3, 15)
barplot(top3, main= "Repartition of top words" , ylab = "Number of repetitions", las=2)

# for the 4-grams
my_dfm4 <- dfm(token_4)
top4 <- topfeatures(my_dfm4, 15)
par(mar = c( 12, 3, 3, 3))
barplot(top4, main= "Repartition of top words" , ylab = "Number of repetitions", las=2)


# Unknown categories: Unsupervised machine learning - 
# Latent Dirichlet Allocation (LDA)
# LDA: Latent Dirichlet Allocation
# belong to topic modeling.

mydfm <- dfm(doc.tokens,
             tolower = TRUE,
             stem = TRUE,
             remove = stopwords("english"))

mydfm.un.trim <-
  dfm_trim(
    mydfm,
    min_docfreq = 0.01,
    #min_docfreq = 0.075,
    # min 7.5%
    max_docfreq = 0.90,
    # max 90%
    docfreq_type = "prop"
  ) 

topic.count <- 20 # Assigns the number of topics for the model

# Convert the trimmed DFM to a topicmodels object
dfm2topicmodels <- convert(mydfm.un.trim, to = "topicmodels")

lda.model <- LDA(dfm2topicmodels, topic.count)

lda.model
# A LDA_VEM topic model with 20 topics.

as.data.frame(terms(lda.model, 6))

# probability of a term in a topic
lda_topics_beta <- tidy(lda.model, matrix = "beta")
head(lda_topics_beta)

# probability of a topic in a document
lda_topics_gamma <- tidy(lda.model, matrix = "gamma")
head(lda_topics_gamma)

lda.similarity <- as.data.frame(lda.model@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# how tppics are connected
par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

# Documents in which topic are particularly strong
head(data.frame(Topic = topics(lda.model)),10)

# find the 10 terms that are most common within each topic
# nice display with ggplot
ap_top_terms <- lda_topics_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# A prediction in term of topics (20 topics) of a sentence on which we apply the lda.model

text <- blogs[201]

my_text <- dfm(tokens(corpus(text)),tolower = TRUE, stem = TRUE, remove = stopwords("english"))

# reponse à une sollicitation
resp <- posterior(lda.model, my_text)
# Le text (modifié en my) est destiné à etre le plus proche d'un des topics sans dire le mot n+1 !