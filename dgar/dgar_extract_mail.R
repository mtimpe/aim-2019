library(tidyverse)
library(tidytext)
library(readtext)
library(lubridate)
library(reshape2)

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
who <- corpus %>% select(who) %>% map(unique); who <- who[[1]]
find_candidate_replicates(who)

# email identifiers - from
email_from <- corpus %>% select(From) %>% map(unique); email_from <- email_from[[1]]
sim_email <- find_candidate_replicates(email_from, plot_tile_pl = F)
grep(email_from, pattern = "enron", value = T, invert = T) # all the emails are internals (sender)

# email identifiers - to
email_to <- corpus %>% select(To) %>% map(unique)
sim_email <- find_candidate_replicates(email_to, plot_tile_pl = F)
grep(email_to, pattern = "enron", value = T, invert = T) # all the emails are internals (sender)

# Define network
g <- graph_from_edgelist(as.matrix(inboxes), directed=F)
coms <- spinglass.community(g)

# Plot network
par(mar=c(0,0,2,0))
plot(coms, g, 
     vertex.label=NA, 
     layout=layout.fruchterman.reingold,
     vertex.size=3,
     main="Enron e-mail network snapshot"
)

# WORDCLOUD 
corpus <- Corpus(VectorSource(bodies))
# Convert the corpus to lowercase
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
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
corpus <- tm_map(corpus, stemDocument)
# Build a document-term matrix out of the corpus
frequencies <- DocumentTermMatrix(corpus)
# remove Sparse Term
sparse <- removeSparseTerms(frequencies, 0.99)
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
