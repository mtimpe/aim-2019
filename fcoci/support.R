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
    ## Identify beginning of Forwarded/Previous Messages - corrected for capital letters (?i) (?-i)

    sep1 <- str_detect(chr, "---") & str_detect(chr, "(?i)(riginal|orward)(?-i)") # forwarded email | original email
    sep2 <- str_detect(chr, "(?i)From(?-i):") 
    sep3 <- str_detect(chr, "(?i)To(?-i):")
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


# DGAROl ===============

tile_plot_mat <- function(mat, label = NULL, titleit = NULL, annot = NULL, vlines = NULL, diag = T, col = "Oranges", col_ann = "Blues", return_plot = F, plot = T){
  mat <- as.matrix(mat)
  dimnames(mat) <- NULL
  if(!is.null(annot)) {
    annot <- annot[1:nrow(mat)]
    df_seg <- data.frame("xs" = 1:nrow(mat), "xsend" = 1:nrow(mat), "ys" = rep(-1, nrow(mat)), "ysend" = rep(0, nrow(mat)), "scol_res" = .factorOrnot(annot))
    if(nrow(mat) == ncol(mat)) {
      df_seg <- data.frame("xs" = 1:nrow(mat), "xsend" = 1:nrow(mat), "ys" = rep(-1, nrow(mat)), "ysend" = rep(0, nrow(mat)), "scol_res" = .factorOrnot(sort(annot)))
      mat <- mat[order(annot), order(annot)]
    }
  }
  if(!diag) diag(mat) <- NA
  melted_mat1 <- reshape2::melt(data = as.matrix(mat), value.name = "lab") 
  gg_tile <- ggplot(data = melted_mat1, aes(x = Var1, y = Var2, fill = lab)) + theme_minimal() + theme(panel.grid = element_blank()) + geom_tile() + 
    scale_fill_distiller(palette = col) + xlab("") + ylab("") + scale_y_continuous(breaks = scales::pretty_breaks()) + scale_x_continuous(breaks = scales::pretty_breaks())
  if(!is.null(vlines)){
    gg_tile <- gg_tile + geom_vline(xintercept = vlines, size = 0.5, col = "black")
  }
  if(is.null(label)){
    gg_tile <- gg_tile + theme(legend.position = "none")
  }else{
    gg_tile <- gg_tile + labs(fill = label)
  }
  if(!is.null(annot)){
    gg_tile <- gg_tile + theme(legend.position = "none")
    cl_pl_res <- ggplot() + geom_rect(data = df_seg, mapping = aes(xmin = xs, ymin = ys, xmax = xsend + 1, ymax = ysend, fill = scol_res)) + theme_minimal() + 
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), 
            legend.position = "none", plot.margin = unit(c(0,0,-0.25,0), "cm"), panel.grid = element_blank()) + .palettit(.ltuni(df_seg$scol_res) < 8, col_ann, type = "fill") 
    if(!is.null(titleit)) cl_pl_res <- cl_pl_res + ggtitle(titleit) 
    gg_tile <- cowplot::plot_grid(cl_pl_res, gg_tile, align = "v", ncol = 1, rel_heights = c(0.2, 1)) 
  }else{
    if(!is.null(titleit)) gg_tile <- gg_tile + ggtitle(titleit) 
  }
  if(plot) print(gg_tile)
  if(return_plot) invisible(gg_tile)
}

# the distance is a generalized Levenshtein (edit) distance, giving the minimal possibly weighted number of insertions, deletions and substitutions needed to transform one string into another.
# It is at most the length of the longer string. -> Normalization
find_candidate_replicates <- function(who, plot_tile_pl = T){
  dist_mat <- adist(who, who)
  lev_sim_names <- sapply(1:nrow(dist_mat), function(y) dist_mat[y,] / sapply(who, function(x) max(nchar(x), nchar(who[y]))))
  diag(lev_sim_names) <- NA
  if(plot_tile_pl) hist(lev_sim_names)
  if(plot_tile_pl) tile_plot_mat(lev_sim_names, label = "Char Dist")
  cat("Quartiles:", quantile(lev_sim_names, probs = seq(0, 1, length.out = 5), na.rm = T), "\n")
  cat("Value range:", range(lev_sim_names, na.rm = T), "\n")
  colnames(lev_sim_names) <- who
  lev_sim_names[upper.tri(lev_sim_names, diag = T)] <- NA
  melted_lev_sim <- melt(lev_sim_names)
  out_mat <- melted_lev_sim[order(melted_lev_sim$value, decreasing = F, na.last = T),]
  print(head(out_mat))
  invisible(out_mat)
}
