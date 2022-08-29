library(purrr)
library(stringr)
#library(igraph)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)


source('processing/helper.R')

## read data
hdbscan_files = list.files('results/hdbscan/', full.names = TRUE, pattern = "*.csv")

clusters = lapply(hdbscan_files, read.csv)
clusters = purrr::map2(clusters, hdbscan_files,
                       function(x, y) {
                         new_label = basename(y) %>% 
                           tools::file_path_sans_ext(.)
                         x = x %>%
                           select(Glottocode, label_)
                         names(x)[names(x) == "label_"] = new_label
                         x
                       })

# join all datasets
clusters_m = clusters %>% 
  purrr::reduce(left_join, by = "Glottocode")

## ------------- Network graph ----------------- ## 

## network is going to show G0 <-> G1 <-> G2
## then a seperate bi-partite graph for Nibs <-> Sibs

# get network info data
ntwrk = clusters_m %>% 
  dplyr::select(g0, g1, g2) %>% 
  dplyr::filter(g0 > -1 & g1 > -1 & g2 > -1) 

ntwrk = Map(paste, names(ntwrk), ntwrk, sep = ':') %>% 
  do.call(cbind, .) %>% as.data.frame()

ntwrk_l = data.table::rbindlist(list(ntwrk[,2:1], ntwrk[,3:2]), use.names = FALSE)
colnames(ntwrk_l) = c("from", "to")

ntwrk_tlly = ntwrk_l %>% 
  group_by(from, to) %>% 
  tally(sort = TRUE)
ntwrk_tllym = as.matrix(ntwrk_tlly[,1:2])

conditional_links = cond_prob_BgvA(a = ntwrk_l$from, b = ntwrk_l$to)

cl = conditional_links %>% 
  dplyr::filter(str_detect(unqe_a, "g1") & str_detect(unqe_b, "g0"))
  
cl$cond_prob2 = ifelse(cl$count < 2, 0, cl$cond_prob) 

# Heatmap 
ggplot(cl, aes(unqe_a, unqe_b, fill= count)) + 
  geom_tile() + 
  geom_text(aes(label = round(cond_prob, 2))) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) 

cl.wide = pivot_wider(cl, id_cols = unqe_a, names_from = unqe_b, values_from = cond_prob)
cl.widecount = pivot_wider(cl, id_cols = unqe_a, names_from = unqe_b, values_from = count)
idx = which(cl.widecount < 5, arr.ind = TRUE)

cl.matrix = as.matrix(cl.wide[,2:ncol(cl.wide)])
rownames(cl.matrix) = stringr::str_remove(cl.wide$unqe_a, "g0:")
colnames(cl.matrix) = stringr::str_remove(colnames(cl.matrix), "g1:")
cl.matrix = cl.matrix[order(as.numeric(rownames(cl.matrix))), 
                      order(as.numeric(colnames(cl.matrix)))]


cl.matrix[!idx]

hmg0g1 = heatmap(cl.matrix, Colv = NA, Rowv = NA, keep.dendro = FALSE, xlab = "G1", ylab = "G0")

cl12 = conditional_links %>% 
  dplyr::filter(str_detect(unqe_a, "g2") & str_detect(unqe_b, "g1"))

cl.wide = pivot_wider(cl12, id_cols = unqe_a, names_from = unqe_b, values_from = cond_prob)
cl.matrix = as.matrix(cl.wide[,2:ncol(cl.wide)])
rownames(cl.matrix) = stringr::str_remove(cl.wide$unqe_a, "g1:")
colnames(cl.matrix) = stringr::str_remove(colnames(cl.matrix), "g2:")
cl.matrix = cl.matrix[order(as.numeric(rownames(cl.matrix))), 
                      order(as.numeric(colnames(cl.matrix)))]

hmg1g2 = heatmap(cl.matrix, Colv = NA, Rowv = NA, keep.dendro = FALSE, xlab = "G2", ylab = "G1")
# # directed weights
# w = tapply(ntwrk_tlly$to, ntwrk_tlly$from, function(x) table(x) / length(x))
# w2 = map2(names(w), w, function(x, y) data.frame(from = x, to = names(y), weight = y))
# weights = do.call(rbind, w2) %>% 
#   dplyr::select(from, to, weight.Freq)

# ntwrk_tlly = left_join(ntwrk_tlly, weights, by = c("from", "to"))

# g = graph_from_edgelist(ntwrk_tllym, directed = F)
g = graph_from_edgelist(as.matrix(conditional_links[,1:2]), directed = F)

layers = ifelse(str_detect(V(g)$name, "g0"), 3, 
                ifelse(str_detect(V(g)$name, "g1"), 2, 1)) 
# l = layout_with_sugiyama(g, ceiling(match(V(g)$name, ntwrk_tllym)/nrow(ntwrk_tllym)), vgap = 0.5)
l = layout_with_sugiyama(g, layers = layers, vgap = 0.5, hgap = NULL)

# # equal spacing
# unq_layers = unique(l$layout[,2])
# for(i in unq_layers){
#   idx <- which(l$layout[,2] == i)  # find "club"-vertices
#   r = range(l$layout[,1])
#   n_values = length(idx)
#   b = seq(r[1], r[2], length=n_values)
#   l$layout[idx,1] <- b
# }

## rescale 
unq_layers = unique(l$layout[,2])
widths = c(1, 0.8, 0.6)
for(i in 1:length(unq_layers)){
  idx <- which(l$layout[,2] == unq_layers[i])  # find "club"-vertices
  r = range(l$layout[,1]) * widths[i]
  l$layout[idx,1] <- rescale(l$layout[idx,1], to = r, from = range(l$layout[,1]))  
}

# cuts <- l$layout[idx, 1]         # find x-coords of vertices
# n_g2 = length(unique(ntwrk$g2))
# cut(cuts, n_g2)                # cut into 3 intervals
# l$layout[idx,1] <- c(1.493405e-308,3.83e-308, 3.77e-308, 3.54e-308, 3.72e-308, 5.904158e-308) 

# ---------- Edge details ------------ # 
#faded = scales::rescale(ntwrk_tlly$n, to = c(0.01, 1), from = range(ntwrk_tlly$n))
#faded = ntwrk_tlly$weight.Freq
# E(g)$color = alpha("red", alpha = faded)
E(g)$color = alpha("red", alpha = conditional_links$cond_prob)

# ---------- Vertex details ------------ # 
V(g)$size = 10
V(g)$vertex.color = "#000000"

#pdf('results/bipartite/bipartite_g0g1g2.pdf')
plot(g, 
     layout=l$layout,
     vertex.label.cex = 0.5, 
     vertex.color = "#FFF7EC", 
     vertex.label.color = "#000000"
     )
dev.off()

## ------------- Sibs and Nibs Network graph ----------------- ## 

# get network info data
# ntwrk_ns = clusters_m %>% 
#   dplyr::select(niblings, siblings) %>% 
#   dplyr::filter(niblings > -1 & siblings > -1) 
# 
# ntwrk_ns = Map(paste, names(ntwrk_ns), ntwrk_ns, sep = ':') %>% 
#   do.call(cbind, .) %>% as.data.frame()
# 
# colnames(ntwrk_ns) = c("from", "to")
# 
# ntwrk_tllysn = ntwrk_ns %>% 
#   group_by(from, to) %>% 
#   tally(sort = TRUE)
# ntwrk_tllymsn = as.matrix(ntwrk_tllysn[,1:2])
# 
# g = graph_from_edgelist(ntwrk_tllymsn, directed = F)
# l = layout_with_sugiyama(g, ceiling(match(V(g)$name, ntwrk_tllymsn)/nrow(ntwrk_tllymsn)))
# 
# idx <- which(l$layout[,2] == 1)  # find "club"-vertices
# cuts <- l$layout[idx, 1]         # find x-coords of vertices
# n_g2 = length(unique(ntwrk$g2))
# cut(cuts, n_g2)                # cut into 3 intervals
# layout$layout[idx,1] <- c(6,7.5,9) 
# 
# # ---------- Edge details ------------ # 
# faded = scales::rescale(ntwrk_tllysn$n, to = c(0.01, 1), from = range(ntwrk_tllysn$n))
# E(g)$color <- alpha("red", alpha = faded)
# 
# # ---------- Vertex details ------------ # 
# new_labels = str_remove(V(g)$name, 'iblings')
# V(g)$name <- new_labels
# 
# pdf('results/bipartite/bipartite_siblingnibling.pdf')
# plot(g, 
#      layout=-l$layout,
#      vertex.size = 10,
#      vertex.label.cex = 0.5, 
#      vertex.color = "#FFF7EC", 
#      vertex.label.color = "#000000", 
#      vertex.label.family = "sans",
#      vgap = 0.25)
# dev.off()
