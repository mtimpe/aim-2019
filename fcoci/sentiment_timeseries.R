library(tidyverse)
library(tidytext)
library(readtext)
library(lubridate)
library(Rtsne)
library(cleanNLP)
library(data.table)
## Set your directory

mydir <- "~/Projects/AIM2019/"

source(str_glue("{mydir}support.R"))
source(str_glue("~/Projects/Buzsaki/tpc_tools.R"))
setwd(mydir)

## Match with lexicon and arrangement in scores per category 
token_spacy <- readRDS(str_glue("{mydir}spacy_sentiment.RData"))

tok <- token_spacy %>%
    mutate(person = str_split_fixed(doc_id, "/", n = 2)[,1]) %>%
    mutate(DayPart = DateLocal) %>%
    mutate_at("DayPart", ~ case_when(between(hour(.), 5, 7) ~ "EarlyMorning",
                                     between(hour(.), 8, 12) ~ "Morning",
                                     between(hour(.), 13, 17) ~ "Afternoon",
                                     between(hour(.), 18, 21) ~ "Evening",
                                     between(hour(.), 22, 23) ~ "Night",
                                     between(hour(.), 0, 4) ~ "Night")) %>%
    mutate(Quarter = lubridate::quarter(DateLocal, with_year = T)) %>%
    mutate(score_norm2 = score/body_length) %>%
    filter(body_length < 150) %>%
    filter(between(Quarter, 1999.2, 2001.3))
    
    
useless <- tok %>%
    filter(str_detect(sentiment, "fear")) %>%
    group_by(person) %>%
    mutate(score_person = rep(mean(score_norm), n())) %>%
    ungroup() %>%
    filter(score_person > quantile(score_person, 0.25)) %>%
    pull(person) %>%
    unique()

## nrow(tok)
## tok <- filter(tok, !person %in% useless)
## nrow(tok)


df <- tok %>%
    group_by(person, sentiment, Quarter) %>%
    summarize(q99 = quantile(score_norm2, 0.99),
              q95 = quantile(score_norm2, 0.95),
              mean = mean(score_norm2)) %>%
    ungroup()

df.mean <- df %>%
    group_by(Quarter, sentiment) %>%
    summarize_at(c("q99", "q95", "mean"), quantile, 0.95) %>%
    ungroup() %>%
    mutate(QuarterQuant = Quarter) %>%
    mutate_at("QuarterQuant", function(x) {
        yr <- floor(x)
        qr <- (x - yr - 0.1)*2.5
        return(yr + qr)
    }) %>%
    gather(key = "stat", value = "mean", mean, q99, q95) %>%
    mutate_at("sentiment", ~ str_remove(., "sentiment_"))

df.tot <- tok %>%
    group_by(sentiment, Quarter) %>%
    summarize(q99 = quantile(score_norm2, 0.99),
              q95 = quantile(score_norm2, 0.95),
              mean = mean(score_norm2)) %>%
    ungroup() %>%
    mutate(QuarterQuant = Quarter) %>%
    mutate_at("QuarterQuant", function(x) {
        yr <- floor(x)
        qr <- (x - yr - 0.1)*2.5
        return(yr + qr)
    }) %>%
    gather(key = "stat", value = "mean", mean, q99, q95) %>%
    mutate_at("sentiment", ~ str_remove(., "sentiment_"))




## PLOTS 

pl <- df %>%
    gather(key = "stat", value = "value", mean, q99, q95) %>%
    filter(stat == "q95") %>%
    mutate(QuarterQuant = Quarter) %>%
    mutate_at("QuarterQuant", function(x) {
        yr <- floor(x)
        qr <- (x - yr - 0.1)*2.5
        return(yr + qr)
    }) %>%
    mutate_at("sentiment", ~ str_remove(., "sentiment_")) %>%
    filter(!sentiment %in% c("positive", "negative", "anticipation", "sadness")) %>%
    mutate_at("sentiment",factor, levels = c("anger", "joy",
                                             "fear", "surprise",
                                             "disgust", "trust")) %>%
    ggplot(aes(x = QuarterQuant)) +
    geom_line(aes(group = person, y = value), alpha = 0.8, color = "grey")  +
    ## geom_line(data = df.mean, aes(y = mean, color = sentiment), alpha = 0.5, size = 1.2)  +
    geom_line(data = filter(df.tot,
                            stat == "q95",
                            !sentiment %in% c("positive", "negative", "anticipation", "sadness")),
              aes(y = mean, color = sentiment),
              alpha = 0.8, size = 1.2)  +
    facet_wrap(sentiment ~ ., scales = "free", ncol = 3) +
    theme_linedraw() +
    xlab("Year") +
    theme(strip.text = element_text(size = 13),
          legend.position = "none")
ggsave(str_glue("{mydir}timeseries_all.png"), pl, height = 6, width = 8)
## ggsave(str_glue("{mydir}timeseries_all_nopeople.png"), pl, height = 6, width = 8)



## #################################################
## ############### OTHER STUFF

walk(c("mean", "q95", "q99"), function(stt) {
    ## df.tot <- df.tot %>%
    ##     filter(stat == stt)
    ## df.mean <- df.mean %>%
    ##     filter(stat == stt)
    ## pl <- df %>%
    ##     gather(key = "stat", value = "value", mean, q99, q95) %>%
    ##     filter(stat == stt) %>%
    ##     mutate(QuarterQuant = Quarter) %>%
    ##     mutate_at("QuarterQuant", function(x) {
    ##         yr <- floor(x)
    ##         qr <- (x - yr - 0.1)*2.5
    ##         return(yr + qr)
    ##     }) %>%
    ##     mutate_at("sentiment", ~ str_remove(., "sentiment_")) %>%
    ##     ggplot(aes(x = QuarterQuant)) +
    ##     geom_line(aes(group = person, y = value), alpha = 0.8, color = "grey")  +
    ##     geom_line(data = df.mean, aes(y = mean, color = sentiment), alpha = 0.5, size = 1.2)  +
    ##     geom_line(data = df.tot, aes(y = mean, color = sentiment),
    ##               alpha = 0.8, size = 1.2, linetype = 2)  +
    ##     facet_wrap(sentiment ~ ., scales = "free", ncol = 1)
    
    pl <- bind_rows(list(tot = df.tot, mean = df.mean), .id = "how") %>%
        filter(stat == stt) %>%
        ggplot(aes(x = QuarterQuant)) +
        geom_line(aes(y = mean, color = sentiment), alpha = 0.5, size = 1.2)  +
        facet_wrap(sentiment ~ how, scales = "free", ncol = 2)
    ggsave(str_glue("{mydir}timeseries_{stt}.png"), pl, height = 15, width = 6)
})    

pl <- bind_rows(list(tot = df.tot, mean = df.mean), .id = "how") %>%
    unite(col = "how", how, stat, sep = "_") %>%
    ggplot(aes(x = QuarterQuant)) +
    geom_line(aes(y = mean, color = sentiment), alpha = 0.5, size = 1.2)  +
    facet_wrap(sentiment ~ how, scales = "free", ncol = 6) +
    theme(axis.text.x = element_blank())




df %>%
    gather(key = "stat", value = "value", mean, q99, q95) %>%
    filter(stat == "q95") %>%
    mutate(QuarterQuant = Quarter) %>%
    mutate_at("QuarterQuant", function(x) {
        yr <- floor(x)
        qr <- (x - yr - 0.1)*2.5
        return(yr + qr)
    }) %>%
    mutate_at("sentiment", ~ str_remove(., "sentiment_")) %>%
    filter(!sentiment %in% c("positive", "negative")) %>%
    mutate_at("sentiment",factor, levels = c("anger", "fear", "sadness", "disgust",
                                             "joy", "surprise", "trust", "anticipation")) %>%
    ggplot(aes(x = QuarterQuant)) +
    ## geom_line(aes(group = person, y = value), alpha = 0.8, color = "grey")  +
    ## geom_line(data = df.mean, aes(y = mean, color = sentiment), alpha = 0.5, size = 1.2)  +
    geom_line(data = filter(df.tot,
                            stat == "q95",
                            !sentiment %in% c("positive", "negative")),
              aes(y = mean, color = sentiment),
              alpha = 0.8, size = 1.2)

