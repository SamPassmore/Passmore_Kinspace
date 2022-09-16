# /usr/bin/Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggtext)
})

# Summary statistics
clusters = read.csv('results/kinbank_wclusters.csv')

summary_files = list.files('results/global/data/', full.names = TRUE)

subsets = lapply(summary_files, read.csv)
names(subsets) = basename(summary_files) %>% 
  tools::file_path_sans_ext()
subsets = bind_rows(subsets, .id = "type")

# Average Silhouette scores by type
subsets %>% 
  group_by(type) %>% 
  summarise(
    mean(silhouette, na.rm = TRUE),
    mean(diversity, na.rm = TRUE),
    range(diversity, na.rm = TRUE)
  )

# rankings
subsets %>% 
  group_by(type) %>% 
  mutate() 
  

subsets %>%
  dplyr::filter(type == "g0") 

clusters %>% 
  dplyr::filter(g0 == "A") %>% 
  count(Family) 

subsets = 
  subsets %>% 
  group_by(type) %>% 
  mutate(total = sum(frequency),
         prop = frequency / total,
         diversity.rank = dense_rank(desc(diversity)),
         frequency.rank = dense_rank(desc(frequency)),
         id = n():1) %>% 
  arrange(type, prop)

table(subsets$frequency.rank, 
      subsets$diversity.rank)

subsets$type = recode(subsets$type,
                      g0 = "G^0",
                      g1 = "G^1",
                      g2 = "G^2",
                      niblings = "Niblings",
                      siblings = "Siblings")

subsets$type = factor(subsets$type,
                      levels = c("Niblings",
                                 "Siblings",
                                 "G^0",
                                 "G^1",
                                 "G^2"))

p1 = 
  ggplot(subsets, aes(size = diversity, y = prop, x = id)) +
  geom_point(aes(col = type, fill = type, size = diversity), colour = "black",
             shape = 21) + 
  xlab("") +
  ylab("Proportion of total") +
  ylim(c(0, 0.6)) + 
  theme_minimal() +
  facet_grid(~type, scales = "free_x", space = "free") + 
  coord_cartesian(clip = 'off') + 
  scale_x_continuous(breaks = NULL) + 
  guides(fill = "none") +
  theme(legend.title = element_blank(), legend.direction = "horizontal",
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.box.just = "right",
        legend.text = element_markdown())


p1
ggsave(filename = "results/figure_5.jpeg", plot = p1, bg = 'white',
       width = 190, height = 100, units = "mm")

p2 = 
  ggplot(subsets, aes(size = diversity, y = prop, x = id)) +
  geom_point(aes(col = type, fill = type, size = diversity), colour = "black",
             shape = 21) + 
  xlab("") +
  ylab("Proportion of total") +
  ylim(c(0, 1.0)) + 
  theme_minimal() +
  facet_grid(~type, scales = "free_x", space = "free") + 
  coord_cartesian(clip = 'off') + 
  scale_x_continuous(breaks = NULL) + 
  guides(fill = "none") +
  theme(legend.title = element_blank(), legend.direction = "horizontal",
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.box.just = "right",
        legend.text = element_markdown())


p2
ggsave(filename = "results/figure_S5.jpeg", plot = p2, bg = 'white')


# Sibling type diversity by region
subsets %>% 
  dplyr::filter(type == "siblings") %>% 
  slice_min(diversity, n = 2) # the least diverse type is L

# Where is sibling type L found?
clusters %>% 
  dplyr::filter(siblings == "L") %>% 
  count(Macroarea) 

clusters %>% 
  dplyr::filter(siblings == "L") %>% 
  count(Family) 

clusters %>% 
  dplyr::filter(siblings == "A") %>% 
  count(Family) 


subsets %>% 
  dplyr::filter(type == "g0") 

# Where is cousin A found
clusters %>% 
  dplyr::filter(g0 == "A") %>% 
  count(Macroarea) 

clusters %>% 
  dplyr::filter(g0 == "A") %>% 
  count(Family) 

## Average diversity
subsets %>% 
  group_by(type) %>% 
  summarise(
    mean(silhouette, na.rm = TRUE),
    mean(diversity, na.rm = TRUE),
    range(diversity, na.rm = TRUE)
  )
  

## Unique types
library(plyr)
matrix_files = list.files('data/matrix/', ".csv", full.names = TRUE)
matrices = lapply(matrix_files, read.csv)
names(matrices) = basename(matrix_files)

distinct_mat = lapply(matrices, function(m) {
  ddply(m[,-ncol(m)], colnames(m)[-ncol(m)], nrow) 
})
names(distinct_mat) = basename(matrix_files)
unlist(lapply(distinct_mat, nrow)) / unlist(lapply(matrices, nrow))

## Mean cluster distances
library(vegan)

g0 = read.csv('data/matrix/g0_matrix.csv')

dd = vegdist(g0[,-ncol(g0)], method = "jaccard") %>% 
  as.matrix()
dimnames(dd) = list(g0$Glottocode,g0$Glottocode) 

# mean overall distance
mean_dist = mean(dd[lower.tri(dd)])
mean_dist

# Within cluster dist
cs = unique(clusters$g0[!is.na(clusters$g0)])
m_dist = c()
for(i in seq_along(cs)){
  idx = clusters$Glottocode_ID[clusters$g0 == cs[i] & !is.na(clusters$g0)]
  
  idxx = match(idx, colnames(dd))
  ss = dd[idxx,idxx]
  m_dist[i] = mean(ss[lower.tri(ss)])
}
names(m_dist) = cs
sort(m_dist)

# percent less than average
1 - (sort(m_dist) / mean_dist)

# comparable crossness
idx = (g0$mFBeSmeB == 1 & 
         g0$mMBeSmeB == 0 & 
         g0$mFZeDmeZ == 0 & 
         g0$meZfMZeD == 1)
g0$Glottocode[idx]
