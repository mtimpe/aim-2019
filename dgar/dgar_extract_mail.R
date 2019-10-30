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

## ######## List directory
## List all the directories to be analyzed

dirs <- list.dirs("../../enron_mail_20150507/", full.names = T, recursive = T) %>%
  str_subset("/sent$")

## ######## Extract metadata
## Read mails and extract metadata (takes some minutes/ to be optimized)

meta <- map_dfr(c.c(dirs)[1:3], function(x) {
  cat(x, "\n")
  readtext(x, verbosity = 0)
}, .id = "directory") %>%
  as_tibble() %>%
  mutate_at("directory", ~ str_remove(., ".*maildir/")) %>% # removing the maildir from directory columns
  separate("directory", into = c("who", "directory"), sep = "/", extra = "merge") #%>% # splitting the the directory columns into who and directory and merging

# splits the text character element in newlines (list of charachters) # the tilde allows to reference the mapping into str_split 
meta <- meta %>% 
  mutate_at(.vars = "text", .funs = map, ~ str_split(.x, "\n")[[1]])
meta %>% 
  mutate(features = map(text, extract_meta)) 
meta %>%
  unnest(features) 

cat(chr <- what_text <- select(meta, text)[[1]][[325]], sep = "\n")
# interesting ones: 325 - Re:
select(meta, text)[[1]][[1]]
extract_meta(what_text)
identify_forward(what_text)

# what is mutate_at and mutate
(trial <- as.tibble(mtcars))
starwars %>% mutate(mass/mean(mass, na.rm = T)) %>% pull()
trial %>% mutate(seq=map(cyl, seq))  # first is the name, map(cyl, seq) applies seq to each cyl and finally adds the column 
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
starwars %>% mutate_at(c("height", "mass"), scale2, na.rm = TRUE)

## ######## Extract body
## Extract actual email body; remove empty mails (just forwarded); fix date format 

corpus <- meta %>%
  mutate(features = map(text, extract_body)) %>%
  unnest(features) %>%
  filter(map_lgl(body, ~ str_squish(.x) %>% str_detect("[:alpha:]") %>% any())) %>% ## Remove empty bodies 
  select(-text) %>%
  separate(Date, into = c("Weekday", "DateLocal", "Timezone"), sep = "(, | -)") %>%
  mutate_at("Timezone", ~ str_extract(., "(?<=\\()[:alpha:]+(?=\\))")) %>%
  mutate_at("DateLocal", dmy_hms) %>%
  mutate(DateUTC = map2(DateLocal, Timezone, function(x, y) {
    tz(x) <- y
    return(x)
  })) %>%
  unnest(DateUTC) %>%
  mutate_at(c("To", "Cc", "Bcc"), ~ map(., function(x) {
    x <- str_split(x, pattern = "( |,)+")[[1]] %>%
      str_subset("@")
    grep("(<|>)", x, value = T, invert= T)
  }))
saveRDS(corpus, str_glue("{mydir}mail_corpus.RData"))



## Unnest all of the lists and save to .csv
corpus <- readRDS(str_glue("{mydir}mail_corpus.RData")) %>%
  mutate_at(c("To", "Cc", "Bcc"), map, function(x) {
    if(lt(x) == 0) return(NA)
    str_c(x, collapse = ", ")
  }) %>%
  mutate_at(c("corpus"), map, ~ str_c(.x, collapse = "\n") %>% str_squish()) %>%
  unnest(To, Cc, Bcc, corpus)
readr::write_csv(corpus, str_glue("{mydir}mail_corpus.csv"), col_names = T)



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
the_max <- which.max(sapply(email_to, length))
max(sapply(email_to, length))
# email_to2 <- lapply(email_to, function(x) c(x, rep(NA, the_max - length(x))))
# email_to[the_max]
cat(corpus %>% slice(the_max) %>% select(corpus) %>% pull() %>% unlist(), sep = "\n")
# %>% mutate_at( "To", .funs = map, ~ str_extract(.x, pattern = paste(who, sep = "|")))
etry <- tibble(email_from=email_from[!na_scuper], email_to=email_to) %>% unnest(cols = c(email_to)) %>% as.matrix()
entry_mask <- sapply(1:nrow(etry), function(x) any(duplicated(etry[x,])))
etry <- etry[!entry_mask,]
# etry2 <- etry %>% distinct(email_from, email_to)
# Define network
g <- graph_from_edgelist(as.matrix(etry), directed=T)
clusters <- clusters(g)
giant <- clusterg() ## using the biggest component as an example, you can use the others here.
communities = giant.community_spinglass()
coms <- spinglass.community(g)

a <- tibble(a=c(1,1,2), b =c(1,2,3)) 
a[sapply(1:nrow(a), duplicated)]

# Plot network
plot(coms, g, 
     vertex.label=NA, 
     layout=layout.fruchterman.reingold,
     vertex.size=3,
     main="Enron e-mail network global"
)

# WORDCLOUD ================
library(SnowballC)
library(wordcloud)
library(tm)
corpus <- readRDS(str_glue("{mydir}../mail_corpus.RData"))

bodies <- corpus %>% select(corpus)
corpus <- Corpus(VectorSource(bodies[[1]]))
# Convert the corpus to lowercase
corpus <- tm_map(corpus, tolower)
inspect(corpus[[1]])
# corpus <- tm_map(corpus, PlainTextDocument) # it is removing everything here
# Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[[1]])
# Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus
inspect(corpus[[1]])

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

corpus
inspect(corpus[[1]])

# Stem document
corpus <- tm_map(corpus, stemDocument) # This function extracts the stems of each of the given words in the vector.
corpus <- tm_map(corpus, removeNumbers) # This function extracts the stems of each of the given words in the vector.
# Build a document-term matrix out of the corpus
frequencies <- DocumentTermMatrix(corpus)
# remove Sparse Term
sparse <- removeSparseTerms(frequencies, 0.991)
# Convert the document-term matrix to a data frame called sparseBodiesDF
sparseBodiesDF <- as.data.frame(as.matrix(sparse))
# Building wordcloud
par(bg = "gray27")
pal <- brewer.pal(7, "Dark2")
wordcloud(
  colnames(sparseBodiesDF),
  colSums(sparseBodiesDF),
  scale = c(2.5, 0.25),
  max.words = 150,
  colors = pal
)
