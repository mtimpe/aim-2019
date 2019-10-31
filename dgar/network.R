library(tidyverse)
library(tidytext)
library(readtext)
library(lubridate)
library(reshape2)
library(igraph)

## Set your directory
mydir <- "/disk2a/dgarolini/eron_EY/aim-2019/dgar/"

source(str_glue("{mydir}/../support.R"))
setwd(mydir)

adjl_from_corpus <- function(corpus){
  
  # extraction
  email_from <- corpus %>% select(From) %>% unlist()
  email_from_u <- corpus %>% select(From) %>% map(unique) %>% unlist()
  email_to <- select(corpus, To)
  
  # selecting the one that are also sender
  email_to <- sapply(email_to[[1]], function(x) {
    uu <- x[x %in% email_from_u]
    if(length(uu)==0) uu <- NA
    uu
  })
  na_scuper <- sapply(email_to, function(x) all(is.na(x)))
  email_to <- email_to[!na_scuper]
  
  # filtering the emails
  etry <- tibble(email_from=email_from[!na_scuper], email_to=email_to) %>% unnest(cols = c(email_to)) %>% as.matrix()
  
  # removing duplicates
  entry_mask <- sapply(1:nrow(etry), function(x) any(duplicated(etry[x,])))
  etry <- etry[!entry_mask,]
  
  # returning adj list
  invisible(etry)
}

text_filter_tm <- function(bodies){
  corpus <- Corpus(VectorSource(bodies[[1]]))
  
  # Convert the corpus to lowercase
  corpus <- tm_map(corpus, tolower)
  # inspect(corpus[[1]])
  # corpus <- tm_map(corpus, PlainTextDocument) # it is removing everything here
  # Remove punctuation from the corpus
  corpus <- tm_map(corpus, removePunctuation)
  # Remove all English-language stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Remove some more words
  corpus <-
    tm_map(
      corpus,
      removeWords,
      c(
        "c",
        "just",
        "will",
        "thanks",
        "please",
        "can",
        "let",
        "said",
        "say",
        "per"
      )
    )
  # Stem document
  corpus <- tm_map(corpus, stemDocument) # This function extracts the stems of each of the given words in the vector.
  # remove numbers
  corpus <- tm_map(corpus, removeNumbers) 
  invisible(corpus)
}

# NETWORK ----------------------------------------------
corpus <- readRDS(str_glue("{mydir}../mail_corpus.RData"))

# name identifiers
who <- corpus %>% select(who) %>% map(unique) %>% unlist()
find_candidate_replicates(who)

# email identifiers - from
email_from_u <- corpus %>% select(From) %>% map(unique) %>% unlist()
sim_email <- find_candidate_replicates(email_from_u, plot_tile_pl = F)
grep(email_from_u, pattern = "enron", value = T, invert = T) # all the emails are internals (sender)

# email identifiers - to
email_to <- corpus %>% select(To) %>% map(unique)
# sim_email <- find_candidate_replicates(email_to, plot_tile_pl = F)
# grep(email_to, pattern = "enron", value = T, invert = T) # all the emails are internals (sender)

email_from <- corpus %>% select(From) %>% unlist()
email_to <- corpus %>% select(To)
email_to <- sapply(email_to[[1]], function(x) {
  uu <- x[x %in% email_from_u]
  if(length(uu)==0) uu <- NA
  uu
})
na_scuper <- sapply(email_to, function(x) all(is.na(x)))
email_to <- email_to[!na_scuper]

# find the email with the max number of receiver
the_max <- which.max(sapply(email_to, length))
max(sapply(email_to, length))
# email_to2 <- lapply(email_to, function(x) c(x, rep(NA, the_max - length(x))))
# email_to[the_max]

cat(corpus %>% slice(the_max) %>% select(corpus) %>% pull() %>% unlist(), sep = "\n")
# %>% mutate_at( "To", .funs = map, ~ str_extract(.x, pattern = paste(who, sep = "|")))
# etry2 <- etry %>% distinct(email_from, email_to)
# Define network

etry <- adjl_from_corpus(corpus)
eee <- as_tibble(etry) %>% group_by(email_from, email_to) %>% summarise(Freq=n())
to_div <- eee %>% group_by(email_from) %>% summarise(Freq2 = n()) %>% pull(Freq2)
eee %>% group_by(email_from) %>%

g <- graph_from_edgelist(as.matrix(eee), directed=T )
coms <- clusters(g)
coms <- spinglass.community(g)
coms <- cluster_optimal(g)

# a <- tibble(a=c(1,1,2), b =c(1,2,3)) 
# a[sapply(1:nrow(a), duplicated)]

# Plot network
plot(coms, g, 
     edge.arrow.size = 0.3,
     vertex.label=NA, 
     layout=layout.fruchterman.reingold,
     vertex.size=3,
     main="Enron e-mail network global"
)
E(g)

# WORDCLOUD ================
library(SnowballC)
library(wordcloud)
library(tm)
corpus_gen <- readRDS(str_glue("{mydir}../mail_corpus.RData"))
corpus_gen <- corpus_gen %>% arrange(DateUTC)
trials <- corpus_gen %>% select(DateUTC) %>% unlist() 
trials[1] > ymd("2000/10/1") 
as_datetime(trials[1])
as_datetime(trials[length(trials)])

c1_pre <- filter(corpus_gen, DateUTC > ymd("2001/1/1"), DateUTC < ymd("2001/6/30"))
c2_centra <- filter(corpus_gen, DateUTC > ymd("2001/7/1"), DateUTC < ymd("2002/1/23"))
c2_crisis <- filter(corpus_gen, DateUTC > ymd("2001/10/15"), DateUTC < ymd("2001/11/8"))
c2_post <- filter(corpus_gen, DateUTC > ymd("2002/1/24"), DateUTC < ymd("2002/12/23"))

# corpus
# inspect(corpus[[1]])
corpus1 <- text_filter_tm(select(c1_pre, corpus))
subject1 <- text_filter_tm(select(c1_pre, Subject))
corpus2 <- text_filter_tm(select(c2_centra, corpus))
subject2 <- text_filter_tm(select(c2_centra, Subject))
corpus2b <- text_filter_tm(select(c2_crisis, corpus))
subject2b <- text_filter_tm(select(c2_crisis, Subject))
corpus3 <- text_filter_tm(select(c2_post, corpus))
subject3 <- text_filter_tm(select(c2_post, Subject))

plot_wc(corpus1)
plot_wc(corpus2)
plot_wc(corpus2b)
plot_wc(corpus3)

plot_wc(subject1)
plot_wc(subject2)
plot_wc(subject2b)
plot_wc(subject3)

plot_wc <- function(which_corpus, max.words = 200){
  # Build a document-term matrix out of the corpus
  frequencies <- DocumentTermMatrix(which_corpus)
  # remove Sparse Term
  sparse <- removeSparseTerms(frequencies, 0.993)
  # Convert the document-term matrix to a data frame called sparseBodiesDF
  sparseBodiesDF <- as.data.frame(as.matrix(sparse))
  # Building wordcloud
  par(bg = "gray10")
  pal <- brewer.pal(7, "Dark2")
  wordcloud(
    colnames(sparseBodiesDF),
    colSums(sparseBodiesDF),
    scale = c(4.5, 0.5), 
    random.order = F,
    max.words = max.words,
    colors = pal, 
    fixed.asp = T
  )
}
  
