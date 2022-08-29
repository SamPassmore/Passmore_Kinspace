## This script is only used to visualise the terms for each cluster and determine the structure they are picking up on

library(dplyr)

cluster = read.csv('results/hdbscan/g1.csv', stringsAsFactors = FALSE)
terms = read.csv('data/terms/g1_terms.csv', stringsAsFactors = FALSE)
structural_vectors = read.csv('data/matrix/g1_matrix.csv', stringsAsFactors = FALSE)

terms = dplyr::left_join(terms, cluster, "Glottocode")

table(terms$label_)

nrow(structural_vectorss)
nrow(unique(structural_vectors[,-ncol(structural_vectors)]))

sum(apply(structural_vectors[,-ncol(structural_vectors)], 1, function(x) all(x == 1)))

# view terms
terms %>% 
  filter(label_ == 0  & cluster_prob == 1) %>% 
#  dplyr::select(feZ, meZ, feB, meB, Glottocode, label_, cluster_prob) %>% 
  View()

idx =  terms$label_ == 9

View(structural_vectors[idx,])

terms[which(terms$fF != terms$fFeB & terms$fFeB == terms$fFeZ & terms$fFeB == terms$fMeZ),] %>% View()

# concatentate
test = apply(structural_vectors[idx,-ncol(structural_vectors)], 1, paste0, collapse = "-")

dplace = read.csv('~/OneDrive - University of Bristol/Projects_Git/dplace-data/csv/glottolog.csv')      

dplace_family = dplyr::left_join(terms, dplace, by = c("Glottocode" = "language_id"))

dplace_family %>% 
  group_by(family_name, label_) %>% 
  filter(label_ == 2) %>%
  summarise(n())

dplace_family %>% 
  dplyr::filter(label_ == 7 & cluster_prob == 1) %>% 
  dplyr::select(family_name, Glottocode) %>% 
  dplyr::distinct()
