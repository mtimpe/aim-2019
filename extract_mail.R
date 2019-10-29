library(tidyverse)
library(tidytext)
library(readtext)
library(lubridate)

## Set your directory

mydir <- "~/Projects/AIM2019/"

source(str_glue("{mydir}support.R"))
setwd(mydir)

## ########
## List all the directories to be analyzed

dirs <- list.dirs("./maildir", full.names = T, recursive = T) %>%
    str_subset("/sent$")


## ########
## Read mails and extract metadata (takes some minutes/ to be optimized)

meta <- map_dfr(c.c(dirs), function(x) {
    cat(x, "\n")
    readtext(x, verbosity = 0)
}, .id = "directory") %>%
    as_tibble() %>%
    mutate_at("directory", ~ str_remove(., ".*maildir/")) %>%
    separate("directory", into = c("who", "directory"), sep = "/", extra = "merge") %>%
    mutate_at("text", map, ~ str_split(.x, "\n")[[1]]) %>%
    mutate(features = map(text, extract_meta)) %>%
    unnest(features) 

## ########
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
 


