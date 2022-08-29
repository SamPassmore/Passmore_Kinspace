# conditional probabilities 

# -- Libraries -- # 
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)

# -- Functions -- #
source('processing/helper.R')


# -- Get data -- #
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


# -- G1 given G2 -- #
df.g1g2 = clusters_m %>% 
  select(g1, g2) %>% 
  filter(g1 > -1 & g2 > -1) %>% 
  drop_na()


g1g2 = cond_prob_BgvA(a = df.g1g2$g2, b = df.g1g2$g1)
(g1g2 %>% filter(count >= 10))
g1g2 %>% filter(count > 0) %>% nrow()
nrow(g1g2)

ggplot(g1g2, aes(unqe_a, unqe_b, fill= cond_prob)) + 
  geom_tile() + 
  scale_fill_gradient(high = "#ED5C4D", low = "#F4E9DA") +
  theme_minimal()

# -- G0 given G1 -- # 
df.g0g1 = clusters_m %>% 
  select(g0, g1) %>% 
  filter(g0 > -1 & g1 > -1) %>% 
  drop_na()
g0g1 = cond_prob_BgvA(a = df.g0g1$g1, b = df.g0g1$g0)
tail(g0g1 %>% filter(count > 9))
g0g1 %>% filter(count > 0) %>% nrow() # observed pairings
nrow(g0g1) # possible pairings

# -- Niblings given Siblings -- #
df.ns = clusters_m %>% 
  select(niblings, siblings) %>% 
  filter(niblings > -1 & siblings > -1) %>% 
  drop_na()
ns = cond_prob_BgvA(a = df.ns$siblings, b = df.ns$niblings)
tail(ns %>% filter(count > 10))
ns %>% filter(count > 0) %>% nrow() # observed pairings
nrow(ns) # possible pairings
