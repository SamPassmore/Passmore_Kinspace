# sibling POC
source('processing/helper.R')
library(pdfCluster)
library(dplyr)

# make structural vectors for exemplary types
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
cluster = read.csv('results/hdbscan/siblings.csv', stringsAsFactors = FALSE)

cluster_joined = dplyr::left_join(cluster, nr_types, by = "Glottocode")

(table(cluster_joined$label_, cluster_joined$nr_type))

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


table(cluster_joined$label_adj, cluster_joined$nr_adjust)
pdfCluster::adj.rand.index(cluster_joined$label_adj, cluster_joined$nr_adjust)

# rand index of exact matches
idx = cluster_joined$nr_type != 'none'
pdfCluster::adj.rand.index(cluster_joined$label_[idx], cluster_joined$nr_type[idx])

## qualitative review of languages
terms = read.csv('data/terms/siblings_terms.csv', stringsAsFactors = FALSE)
terms_j = dplyr::left_join(terms, cluster_joined, "Glottocode")

test = terms_j %>% 
       dplyr::filter(label_ == 2 & nr_type != 'type.6')

View(test)

apply(test[,1:8], 1, function(x) length(unique(x)))
      