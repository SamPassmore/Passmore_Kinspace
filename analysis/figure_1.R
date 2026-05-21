# Figure 1

# sibling POC
source('processing/helper.R')
library(pdfCluster)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)

to_letters = function(x){
  tt = sort(table(x), decreasing = TRUE)
  tt_df = data.frame(tt)
  
  outliers = tt_df[tt_df$x == "-1",]
  
  df_ss = tt_df[tt_df$x != "-1",]
  df_ss$letter_labels = LETTERS[1:nrow(df_ss)]
  
  outliers$letter_labels = "Outlier"
  
  out = rbind(df_ss, outliers)
  out$x = as.numeric(as.character(out$x))
  out$letter_labels
}

# make structural vectors for exemplary types
nr_freq = read.csv('data/nerlove-frequency.csv') %>% 
  select(type, nerlove)

nr_types = read.csv('data/nerlove-typology.csv', stringsAsFactors = FALSE) %>% 
  t()

colnames(nr_types) = nr_types[1,]
nr_types = nr_types[-1,]

nr_matrix = apply(nr_types, 1, get_vector, method = "binary") %>% t() %>% as.data.frame()

# determine which languages perfectly make exemplary types
sibling_matrix = read.csv('data/matrix/siblings_matrix.csv', stringsAsFactors = FALSE)

types = vector(length = nrow(sibling_matrix))
for(i in 1:nrow(sibling_matrix)){
  row = subset(sibling_matrix[i,], select = -Glottocode)
  match = apply(nr_matrix, 1, function(x) all(x == row))
  type = names(match)[match]
  if(sum(match) == 0){
    type = "none"
  }
  types[i] = type
}

nr_types = data.frame(Glottocode = sibling_matrix$Glottocode, nr_type = types)

# hdbscan results
cluster = read.csv('results/hdbscan/siblings_hamming.csv', stringsAsFactors = FALSE)

cluster_joined = dplyr::left_join(cluster, nr_types, by = "Glottocode")

# total rand index
rand_df = cluster_joined %>% 
  filter(nr_type != "none")
pdfCluster::adj.rand.index(rand_df$label_, rand_df$nr_type)

# rand index with reclassified reversals & derivatives
## here I merge the two reversed / derivative clusters into one (as per N&R)
## and reclassify reversals in the N&R typology
## cluster 5 is reverse of cluster 3
cluster_joined$label_adj = ifelse(cluster_joined$label_ == 5, 3, cluster_joined$label_)
cluster_joined$nr_adjust = ifelse(cluster_joined$nr_type == 'none' & cluster_joined$label_ == 5, 
                                  'type.3', as.character(cluster_joined$nr_type))
## 7 is a derivative of 8
cluster_joined$label_adj = ifelse(cluster_joined$label_ == 7, 8, cluster_joined$label_adj)
cluster_joined$nr_adjust = ifelse(cluster_joined$nr_type == 'none' & cluster_joined$label_ == 7, 
                                  'type.12', as.character(cluster_joined$nr_adjust))

## 9 is the reverse of 6
cluster_joined$label_adj = ifelse(cluster_joined$label_ == 9, 6, cluster_joined$label_adj)
cluster_joined$nr_adjust = ifelse(cluster_joined$nr_type == 'none' & cluster_joined$label_ == 9, 
                                  'type.5', as.character(cluster_joined$nr_adjust))


# table(cluster_joined$label_adj, cluster_joined$nr_adjust)
pdfCluster::adj.rand.index(cluster_joined$label_adj, cluster_joined$nr_adjust)

# rand index of exact matches
idx = cluster_joined$nr_type != 'none'
pdfCluster::adj.rand.index(cluster_joined$label_[idx], cluster_joined$nr_type[idx])

nr_long = cluster_joined %>% 
  group_by(nr_type) %>% 
  filter(nr_type != "none") %>% 
  summarise(kinbank = n()) %>% 
  mutate(type = gsub("type.", "", nr_type),
         type = ifelse(type == "none", "other", type)) %>% 
  left_join(., nr_freq, "type") %>%
  select(type, nerlove, kinbank) %>% 
  pivot_longer(., cols = c("nerlove", "kinbank")) %>% 
  group_by(name) %>% 
  mutate(percent = value / sum(value, na.rm = T),
         type = factor(type, levels = c(1:12, "other"))) %>% 
  arrange(desc(value)) 

kb = nr_long %>% filter(name == "kinbank") %>% arrange(desc(value)) %>% mutate(letter_label = LETTERS[1:n()]) %>%
  ungroup() %>% dplyr::select(type, letter_label)

nr_long = left_join(nr_long, kb) %>% 
  mutate(plot_label = paste(letter_label, " (", type, ")", sep = ))

p1 = ggplot(nr_long, aes(y = percent, x = plot_label, fill = name)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  ylab("Percent of sample") +
  xlab("Cluster (N&R Type)") +
  theme_minimal() +
  scale_fill_manual(values = c("#FBBE4B", "#ED5C4D"), labels = c("Kinbank", "N&R")) +
  theme(legend.position = c(0.9, 0.95), legend.title = element_blank(),
        text = element_text(size=20)) 

ggsave(plot = p1, filename = "results/umap/sibling_frequency.pdf")

# load("results/umap/siblings_ggplot.rdata")
# 
# p3 = pp + p1
# 
# pdf("results/figure_1.pdf", width = 8, height = 4)
# p3
# dev.off()
# 
# 
