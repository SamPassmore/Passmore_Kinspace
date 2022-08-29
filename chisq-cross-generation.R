## network strength links

library(purrr)
library(stringr)
library(igraph)
library(tidyr)
library(dplyr)
library(stats)
library(rcompanion)
library(nnet)


# -- Functions -- #
cramer_estV = function(a, b){
  cV = rcompanion::cramerV(a, b)

  B = 4000
  dum = vector (mode = "numeric", length = B)
  for(i in 1:B){
    tb = table(df.g1g2$g1, df.g1g2$g2)
    rtot = rowSums(tb)
    ctot = colSums(tb)
    dum[i] <- rcompanion::cramerV(r2dtable(1, rtot, ctot)[[1]])
  } 
  #MC.p.value <- length(dum[dum > g1g2V])/B
  #MC.p.value
  dum
}


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

# --- G1 to G2 relationship -- # 

# get network info data
df.g1g2 = clusters_m %>% 
  dplyr::select(g1, g2) %>% 
  dplyr::filter(g1 > -1 & g2 > -1) 

g1g2V_est = data.frame(CV = cramer_estV(df.g1g2$g1, df.g1g2$g2))
rcompanion::groupwiseMean(CV~1, 
                          data   = g1g2V_est, 
                          conf   = 0.95,
                          digits = 3)

summary(lm(as.character(clusters_m$g1) ~ as.character(clusters_m$g2)))
summary(lm(as.character(clusters_m$g0) ~ as.character(clusters_m$g1)))
summary(lm(as.character(clusters_m$g1) ~ as.character(clusters_m$g2)))

summary(multinom(lm(as.character(clusters_m$g1) ~ as.character(clusters_m$g2))))

pdfCluster::adj.rand.index(df.g1g2$g1, df.g1g2$g2)


# -- G0 to G1 relationship -- #

df.g0g1 = clusters_m %>% 
  dplyr::select(g0, g1) %>% 
  dplyr::filter(g0 > -1 & g1 > -1) 

g0g1V_est = data.frame(CV = cramer_estV(df.g0g1$g0, df.g0g1$g1))
rcompanion::groupwiseMean(CV~1, 
                          data   = g0g1V_est, 
                          conf   = 0.95,
                          digits = 3)

pdfCluster::adj.rand.index(df.g0g1$g0, df.g0g1$g1)
# -- Siblings to Niblings relationship -- #

df.sibnib = clusters_m %>% 
  dplyr::select(siblings, niblings) %>% 
  dplyr::filter(siblings > -1 & niblings > -1) 

snV_est = data.frame(CV = cramer_estV(df.sibnib$siblings, df.sibnib$niblings))
rcompanion::groupwiseMean(CV~1, 
                          data   = snV_est, 
                          conf   = 0.95,
                          digits = 3)

pdfCluster::adj.rand.index(df.sibnib$siblings, df.sibnib$niblings)



# t.g1g2 = table(ntwrk$g1, ntwrk$g2)
# rs.g1g2 = rowSums(t.g1g2)
# cs.g1g2 = colSums(t.g1g2)
# n.g1g2 = sum(rs.g1g2)
# expected <- outer(rs.g1g2, cs.g1g2, "*") / n.g1g2
# maxSqResid <- function(x) max((x - expected) ^ 2 / expected)
# sim_tables = sapply(r2dtable(100, rs.g1g2, cs.g1g2), maxSqResid)
# sum(sim_tables >= maxSqResid(t.g1g2)) / 100
# 
# # Relationship between two generations
# rcompanion::cramerV(ntwrk$g1, ntwrk$g2)
# DescTools::Lambda(ntwrk$g1, ntwrk$g2, direction = "column") # G1 predicts G2
# DescTools::Lambda(ntwrk$g1, ntwrk$g2, direction = "row") # G2 predicts G1
# 
# # conditional probability
# cond_prob_BgvA = function(a, b){
#   if(length(a) != length(b))
#     stop("x and y must be the same length")
# 
#   unqe_a = unique(a)
#   unqe_b = unique(b)
# 
#   cond_prob = expand_grid(unqe_a, unqe_b)
#   cond_prob$cond_prob = NA
#   for(i in 1:length(unqe_a)){
#     for(j in 1:length(unqe_b)){
#       p_a = sum(a == i) / length(a)
#       p_anb = sum(a == i & b == j) / length(a)
#       cond_prob[cond_prob$unqe_a == i & cond_prob$unqe_b == j,"cond_prob"] = p_anb / p_a
#     }
#   }
#   cond_prob
# }
# # 
# test = cond_prob_BgvA(df.g0g1$g1, df.g0g1$g0)
# 
# # g0 (rows) & g1 (columns)
# total = table(ntwrk$g0, ntwrk$g1)
# # total
# total = round(prop.table(total), 2)
# idx = which(total == max(total), arr.ind = 2)
# idx
# total[idx]
# # g1 in g0
# round(prop.table(total, margin = 1), 2)[idx]
# # g0 in g1
# round(prop.table(total, margin = 2), 2)[idx]
# 
# 
# 
#       