suppressPackageStartupMessages({
  library(ggplot2)
  library(RColorBrewer)
  library(dplyr)
  library(dichromat)
  library(assertthat)
  library(fossil)
})

args = commandArgs(trailingOnly = TRUE)

# args
kt = args[1]
# kt = "g0"

colour_palatte = c('#297AB1', '#57B5ED', '#71AB7F', '#FBBE4B', 
                   "#FF9438", "#8980D4", "#ED8F57",
                   '#BFD7E8', '#BCE1F8', '#C6DDCC', 
                   '#FDE5B7', '#FFD4AF', "#D0CCEE", 
                   "#F8D2BC", "#AB4E68", "#17C3B2")


clusters = read.csv('results/kinbank_wclusters.csv')
clusters$label_ = clusters[,kt]
clusters = clusters[!is.na(clusters$label_),]

embeddings = read.csv(
  paste0("results/hdbscan/", kt, ".csv")
)

embeddings = subset(embeddings, select = -c(X.1, label_))

x = assert_that(nrow(embeddings) == nrow(clusters))

clusters = dplyr::left_join(clusters, embeddings, by = c("Glottocode_ID" = "Glottocode"))

# get dplace colours
dplace = read.csv('dplace-data/datasets/EA/data.csv') %>%
  dplyr::filter(var_id == "EA027")

dplace.socs = read.csv('dplace-data/datasets/EA/societies.csv')
dplace = dplyr::left_join(dplace, dplace.socs, by = c("soc_id"= "id"))

# merge
d2 = dplyr::left_join(clusters, dplace, by = c("Glottocode" = "glottocode"))

n.cols = length(unique(d2$label_)) - 1 # ignore outliers
if(n.cols <= length(colour_palatte)){
  g.cols = colour_palatte[1:n.cols]
} else {
  extra = n.cols - length(colour_palatte)
  g.cols = c(colour_palatte, colorRampPalette(colour_palatte)(extra))
}

cols_df = data.frame(cols = g.cols,
                     label_ = LETTERS[1:n.cols])

cols_df = rbind(cols_df, c(alpha("grey", 0.5), "Outlier"))

d2 = dplyr::left_join(d2, cols_df, by = "label_")

# ea kin type colours
d2$cols.ea = brewer.pal(8, "Set1")[d2$code]
d2$cols.ea = ifelse(is.na(d2$cols.ea), alpha("grey", 0.5), d2$cols.ea)

## centered labels
d_c = d2 %>% 
  dplyr::select(X, Y, label_) %>%
  dplyr::filter(label_ != "Outlier") %>%
  dplyr::group_by(label_) %>% 
  dplyr::summarize(x = mean(X, na.rm=TRUE), y = mean(Y, na.rm=TRUE))

pdf(paste0('results/umap/', kt,'_ggplot.pdf'), width = 4, height = 4)
ggplot(data = d2, aes(x = X, y = Y, label = label_)) + 
  geom_point(col = d2$cols, size = 2) + 
  geom_text(data = d_c, aes(x = x, y = y, label = label_)) + 
  xlab('') + ylab('') + 
  theme_minimal() + 
  theme(legend.position = 'right', axis.text.x=element_blank(), 
        axis.text.y=element_blank())
trash = dev.off()

## Rand index
d2$g0_numbers = recode(d2$g0,
      A = 1, B = 1, C = 2, D = 1, E = 1, 
      F = 1, G = 3, H = 4, I = 4, J = 1, 
      K = 3, L = 1, M = 1, N = 1, O = 3)

d2$code_adj = recode(d2$code,
                     "1" = 3, "2" = 2, "3" = 1, 
                     "4" = 4, "5" = 3, "6" = 3, "7" = 2)
d2$code_label = recode(d2$code,
                     "1" = "Bifurcate Merging", "2" = "Bifurcate Collateral", "3" = "Lineal", 
                     "4" = "Generational", "5" = "Bifurcate Merging", 
                     "6" = "Bifurcate Merging", "7" = "Bifurcate Collateral",
                     "8" = "Outlier")
d2$code_label = ifelse(is.na(d2$code_label), "Outlier", d2$code_label)

d2$cols.adj.ea = brewer.pal(8, "Set1")[d2$code_adj]
d2$cols.adj.ea = ifelse(d2$code_adj < 0, alpha("grey", 0.5), d2$cols.adj.ea)

# 1 is Lineal
# 2 is Descriptive or bifurcate collateral
# 3 is Bifurcate merging
# 4 is Generational

rand_df = d2 %>% 
  select(code_adj, g0_numbers) %>% 
  na.omit()


ari = adj.rand.index(rand_df$code, rand_df$g0_numbers)

cols_df = d2 %>% select(code_adj, cols.adj.ea) %>% distinct

cat("The adjusted Rand index is", round(ari, 2), "determied from", nrow(rand_df), "langauges\n")

if(kt == "g0"){
  pdf(paste0('results/umap/', kt,'_ggplot_eacolours.pdf'),  width = 6, height = 4)
  ggplot(data = d2, aes(x = X, y = Y, label = label_, col = code_label)) + 
    geom_point(size = 2) + 
    geom_text(data = d_c, aes(x = x, y = y, label = label_), col = "black") + 
    xlab('') + ylab('') + 
    theme_minimal() + 
    scale_color_manual(values = c(
      "Lineal" = "#E41A1C", "Bifurcate Collateral" = "#377EB8", 
      "Bifurcate Merging" = "#4DAF4A", "Generational" = "#984EA3", "Outlier" = alpha('grey', 0.5)
    )) + 
    theme(legend.position = 'right', axis.text.x=element_blank(), 
          axis.text.y=element_blank(), 
          legend.title = element_blank(),
          text = element_text(size=8)) 
  trash = dev.off()
}
