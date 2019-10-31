library(tidyverse)
library(tidytext)
library(readtext)
library(lubridate)
library(Rtsne)

## Set your directory

mydir <- "/disk2a/dgarolini/eron_EY/aim-2019/dgar/"

source(str_glue("{mydir}/../support.R"))
setwd(mydir)

## Lexicon with sentiment categories
lexicon <- get_sentiments("nrc")

corpus <- readRDS(str_glue("{mydir}../mail_corpus.RData"))

details <- select(corpus, -corpus)

## Tokenize in words
vocab <- corpus %>%
    select("Message-ID", corpus) %>%
    mutate(corpus = map(corpus, str_c, collapse= "")) %>%
    unnest(corpus) %>%
    unnest_tokens(word, corpus, token = "words", to_lower = T, strip_numeric = T) %>%
    filter(!str_detect(word, "(\\.|:)")) %>%
    filter(str_length(word) > 2) %>%
    filter(!str_detect(word, "[0-9]")) 


## Match with lexicon and arrangement in scores per category
sent <- vocab %>%
    inner_join(lexicon, by = "word") %>%
    mutate_at("sentiment", factor) %>%
    group_by(`Message-ID`, sentiment, .drop = F) %>%
    summarise(score = n()) %>%
    ungroup() %>%
    left_join(details, by = "Message-ID") %>% ## Mails that do not have category are dropped
    mutate_at("sentiment", ~ paste0("sentiment_", .)) %>%
    spread(sentiment, score)


## t-SNE

data <- select(sent, starts_with("sentiment"))
tsne <- Rtsne(as.matrix(data),
             dims = 2,
             perplexity = 30,
             theta = 0.5,
             max_iter = 1000,
             momentum = 0.5,
             final_momentum = 0.8,
             eta = 200,
             exaggeration_factor = 12,
             pca = FALSE,
             check_duplicates = FALSE,
             verbose = TRUE,
             ## stop_lying_iter = ifelse(is.null(Y_init), 250L, 0L),
             ## mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L),
             
)

df_tsne <- as_tibble(tsne$Y) %>% set_names(c("dim1", "dim2")) %>%
    bind_cols(sent)
saveRDS(splice(df = df_tsne, tsne[-5]), str_glue("{mydir}tsne_1.RData"))


## Plot per category

df_tsne <- readRDS(str_glue("{mydir}tsne_1.RData"))$df
df_all <- df_tsne %>%
    gather(key = "sentiment", value = "score", starts_with("sentiment")) %>%
    mutate_at("sentiment", ~ str_remove(., "sentiment_")) 

pal <- RColorBrewer::brewer.pal(9, "YlGnBu")[c(4,9)]
pl <- df_all %>%
    filter(score != 0) %>%
    ggplot(aes(x = dim1, y = dim2)) +
    geom_point(data = df_all, color = "grey85", shape = 20, size = 0.01) +
    geom_point(aes(color = score), shape = 20, size = 0.03) +
    theme_classic() +
    scale_color_gradient(low = pal[1], high = pal[2]) +
    ## guides(color = guide_colorbar(override.aes = list(size = 6))) +
    facet_wrap(sentiment ~ ., nrow = 2) +
    theme(strip.text = element_text(size = 10))

ggsave(str_glue("{mydir}tsne.png"), pl, width = 24, height = 10)








