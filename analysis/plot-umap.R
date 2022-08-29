library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(dichromat)


args = commandArgs(trailingOnly = TRUE)

# args
kt = args[1]
kt = "siblings"

colour_palatte = c('#297AB1', '#57B5ED', '#71AB7F', '#FBBE4B', 
                   "#FF9438", "#8980D4", "#ED8F57",
                   '#BFD7E8', '#BCE1F8', '#C6DDCC', 
                   '#FDE5B7', '#FFD4AF', "#D0CCEE", 
                   "#F8D2BC", "#AB4E68", "#17C3B2")


d = read.csv(paste0('../../kinspace-systems/code/results/umap/', kt, '_embeddings.csv'), header = FALSE, stringsAsFactors = FALSE)
clusters = read.csv(paste0('../../kinspace-systems/code/results/hdbscan/', kt, '.csv'))
d2 = cbind(d, clusters)
# d2$label_factor = factor(d2$label_)
d2$label_factor = ifelse(d2$label_ >= 0, LETTERS[d2$label_ + 1], "Outlier")

# get dplace colours
dplace = read.csv('~/OneDrive - University of Bristol/projects/dplace-data/datasets/EA/data.csv') %>%
  dplyr::filter(var_id == "EA027")

dplace.socs = read.csv('~/OneDrive - University of Bristol/projects/dplace-data/datasets/EA/societies.csv')
dplace = dplyr::left_join(dplace, dplace.socs, by = c("soc_id"= "id"))

# merge
d2 = dplyr::left_join(d2, dplace, by = c("Glottocode" = "glottocode"))

n.cols = length(unique(d2$label_))
if(n.cols < length(colour_palatte)){
  g.cols = colour_palatte[1:n.cols]
} else {
  extra = n.cols - length(colour_palatte)
  g.cols = c(colour_palatte, colorRampPalette(colour_palatte)(extra + 1))
}

#plot(1:17, pch = 19, col = g.cols)
d2$cols = g.cols[d2$label_ + 2]
d2$cols = ifelse(d2$label_ == -1, alpha("grey", 0.5), d2$cols)


# ea kin type colours
d2$cols.ea = brewer.pal(8, "Set1")[d2$code]
d2$cols.ea = ifelse(is.na(d2$cols.ea), alpha("grey", 0.5), d2$cols.ea)

# label order by frequency
label_frequency = sort(table(d2$label_[d2$label_ > -1]), decreasing = TRUE)
labels = data.frame(old_labels = names(label_frequency),
                    new_labels = 1:length(label_frequency))


# re-organise labels so 1 is the most frequent cluster
d2$label_ = match(d2$label_, table = labels$old_labels, nomatch = -1)

## centered labels
d_c = d2 %>% 
  dplyr::select(V1, V2, label_) %>%
  dplyr::filter(label_ > -1) %>%
  dplyr::group_by(label_) %>% 
  dplyr::summarize(x = mean(V1, na.rm=TRUE), y = mean(V2, na.rm=TRUE))

# replace numbers with letters
d_c$label_ = LETTERS[d_c$label_]

d2 %>% 
  select(label_, cols) %>% 
  distinct() %>% 
  filter(label_>-1)%>% 
  mutate(new_label = LETTERS[label_]) %>% 
  View()


pdf(paste0('results/umap/', kt,'_ggplot.pdf'), width = 4, height = 4)
ggplot(data = d2, aes(x = V1, y = V2, label = label_factor)) + 
  geom_point(col = d2$cols, size = 2) + 
  geom_text(data = d_c, aes(x = x, y = y, label = label_)) + 
  xlab('') + ylab('') + 
  theme_minimal() + 
  theme(legend.position = 'right', axis.text.x=element_blank(), 
        axis.text.y=element_blank())
dev.off()

## code for making alternative colours graphs in g0
# for biological theory paper

# broad colours
# broad.cols = data.frame(label_ = 0:15, 
#            type = c("Cross-Parallel",
#                     "Cross-Parallel",
#                     "Hawaiian",
#                     "Hawaiian",
#                     "Cross-Parallel",
#                     "Eskimo",
#                     "Eskimo",
#                     "Eskimo",
#                     "Eskimo",
#                     "Hawaiian",
#                     "Eskimo",
#                     "Eskimo",
#                     "Eskimo",
#                     "Eskimo",
#                     "Cross-Parallel",
#                     "Sudanese"
#            ))
#d2 = dplyr::left_join(d2, broad.cols, "label_")
#d2$cols.bt = brewer.pal(8, "Set1")[d2$type]

# ggplot(d2, aes(x = V1, y = V2, group = code)) + 
#   geom_point(col = d2$cols.ea, size = 3) + 
#   xlab('') + ylab('')
# 
# ggplot(d2, aes(x = V1, y = V2, group = type)) + 
#   geom_point(col = d2$cols.bt, size = 3) + 
#   xlab('') + ylab('')