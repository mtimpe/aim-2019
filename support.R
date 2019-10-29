lt <- function(x) {length(x)}

c.c <- function(...) {
    ## Creates a named vector whose names are the same elements; for map() use
    ## It produces the same results with map() as sapply(..., simplify=F)
    x <- c(...)
    setNames(x, x)
}


identify_meta <- function(chr) {
    ## Identify boundaries of the metadata

    first <- str_which(chr, "Message-ID:")[1]
    last <- str_which(chr, "X-FileName:")[1]
    if(length(first) == 0 | length(last) == 0) {
        browser()
    }
    return(c(first:last))
}

identify_forward <- function(chr) {
    ## Identify beginning of Forwarded/Previous Messages

    sep1 <- str_detect(chr, "---") & str_detect(chr, "(riginal|orward)")
    sep2 <- str_detect(chr, "From:")
    sep3 <- str_detect(chr, "To:")
    sep4 <- str_detect(chr, "<([:alpha:]|\\.)+@")
    sep5 <- str_detect(chr, "[0-9]{2}/[0-9]{2}/[0-9]{2,4} [0-9]{2}:[0-9]{2}")
    sep6 <- str_detect(chr, "^> ")
    
    stack <- which(sep1 | sep2 | sep3 | sep4 | sep5 | sep6)[1] ## Remove also the line before; there should not be anything above
    return(stack)
}

extract_meta <- function(chr) {
    ## Extract metadata and organize them in data.frame

    chr <- chr[identify_meta(chr)]
    chr <- str_c(chr, collapse = "\n")
    feat <- str_extract_all(chr, "\n([:alpha:]|-)+:")[[1]]
    sep <- paste0("(Message-ID:|", str_c(feat, collapse = "|"), ")")
    text <- str_split(str_c(chr, collapse = "\n"), sep)[[1]][-1] %>% str_squish()
    tibble(feat = c("Message-ID", feat), text = text) %>%
        mutate_at("feat", ~ str_replace_all(., "(\n|:)", "")) %>%
        mutate_at("feat", ~ factor(., levels = unique(.))) %>%
        spread(feat, text)
}


extract_body <- function(chr) {
    ## Extract the mail body by stripping metadata and previous messages

    mm <- identify_meta(chr)

    ## Strip Metadata
    chr <- chr[-mm]

    ## Identify Forwarded/Previous Messages
    kk <- identify_forward(chr)

    if(is.na(kk)) {
        body <- chr
        stack <- F
    } else {
        # Strip Previous Messages
        body <- chr[c(1:(kk-1))]
        stack <- T
    }
    tibble(body = list(body), stack = stack)
}
