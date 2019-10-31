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

