# cluster summary statistics
library(dplyr)
library(ggplot2)
library(tidyr)

## G+1 cluster stats
glottolog = glottolog = read.csv('https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv', stringsAsFactors = FALSE)

cluster = read.csv('results/hdbscan/g1.csv', stringsAsFactors = FALSE) %>% 
  dplyr::left_join(., glottolog, "Glottocode")

# how many language families total in g1
lf = cluster %>% 
  group_by(label_, Family_ID) %>% 
  dplyr::summarise(count = n()) 

tidyr::spread(lf[,c("label_", "Family_ID", "count")], 
               key = "label_", value = "count", fill = 0) %>%
  dim() # 69 LF, 14 clusters


cluster %>% 
  filter(label_ == 10 & Family_ID != "" & !is.na(Family_ID)) %>%
  pull(Family_ID) %>% 
  unique()
# 21 LF for cluster 10

cluster %>% 
  filter(label_ == 2 & Family_ID != "" & !is.na(Family_ID)) %>%
  pull(Family_ID) %>% 
  unique()

# nerlove figure
nr_freq = read.csv('data/nerlove-frequency.csv')
nr_freq$kinbank = nr_freq$kinbank.cluster
nr_long = pivot_longer(nr_freq[,c("type", "nerlove", "kinbank")], cols = c("nerlove", "kinbank"))
nr_long$type = factor(nr_long$type, levels = c(1:12, "other"))
nr_long = nr_long %>% 
  group_by(name) %>% 
  mutate(n = sum(value)) %>%
  mutate(percent = value / n)

ggplot(nr_long, aes(y = percent, x = type, fill = name)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  ylab("Percent of sample") +
  xlab("N&R Type") +
  theme_minimal() +
  scale_fill_manual(values = c("#FBBE4B", "#ED5C4D"), labels = c("Kinbank", "N&R")) +
  theme(legend.position = c(0.9, 0.95), legend.title = element_blank(),
        text = element_text(size=15)) 
  
# -- G0 -- # 
# silhouette closer cluster
g0s = read.csv('results/hdbscan/silhouette/g0_all.csv')

g0s %>% 
  filter(cluster %in% c(5, 6, 7, 11)) %>% 
  group_by(cluster) %>% 
  count(neighbor)

# -- G1 --#
# silhouette cluster
g1s = read.csv('results/hdbscan/silhouette/g1_all.csv')
g1s %>% 
  filter(cluster == 1) %>% 
  group_by(cluster) %>% 
  count(neighbor)


# -- niblings -- #
nibs = read.csv('results/hdbscan/silhouette/niblings_all.csv')
nibs %>% 
  filter(cluster %in% c(0, 5)) %>% 
  group_by(cluster) %>% 
  count(neighbor)
