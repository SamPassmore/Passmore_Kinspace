# test of diversity measures
# Tabula homepage
# https://tabula.archaeo.science/reference/heterogeneity-index.html

library(igraph)
library(ggdag)
library(readxl)
library(dplyr)
library(tabula)

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 1

glottolog = read.csv('raw/glottolog/cldf/languages.csv', stringsAsFactors = FALSE)

cluster = read.csv('results/hdbscan/g2.csv', stringsAsFactors = FALSE) %>% 
  dplyr::left_join(., glottolog, "Glottocode")

cluster_subset = cluster %>% 
  filter(label_ > -1) %>% 
  filter(!is.na(Family_ID)) %>% 
  filter(Family_ID != "")

# get counts
cluster_count = cluster_subset %>% 
  group_by(label_, Family_ID) %>% 
  summarise(count = n())

# make community matrix
community_matrix = tidyr::spread(cluster_count[,c("label_", "Family_ID", "count")], 
                                 key = "Family_ID", value = "count", fill = 0) # NAs are 0

diversity_matrix = as.matrix(community_matrix[,!names(community_matrix) %in% "Family_ID"])

dm = CountMatrix(data = diversity_matrix, nrow=nrow(diversity_matrix))

d_measures = tabula::diversity(dm) %>% 
  lapply(., rescale) %>% 
  do.call(c, .) %>% 
  as.data.frame()

d_measures$measure = rownames(d_measures)
colnames(d_measures) = c("value", "measure")
d_measures$type = stringr::str_extract(d_measures$measure, "[0-9]")
d_measures$measure = stringr::str_remove_all(d_measures$measure, "[0-9]")

ggplot(d_measures, aes(y = value, x = type, group = measure, col = measure)) + 
  geom_line()
