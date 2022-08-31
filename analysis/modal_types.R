# modal type
library(plyr)

source('processing/helper.R')

kinspace_results = read.csv('results/kinbank_wclusters.csv')

langs = 
  kinspace_results %>% 
  filter(siblings == "A") %>% 
  pull(Glottocode)

matrix = read.csv('data/matrix/siblings_matrix.csv')
terms = read.csv('data/terms/sibling_terms.csv')

idx = matrix$Glottocode %in% langs
types = subset(matrix, select = -c(Glottocode), subset = idx)


# modal type
freq = ddply(types, colnames(types), nrow) %>% 
  arrange(desc(V1))

top_type = freq %>% 
  slice_max(V1) %>% 
  select(-V1) %>% 
  unlist()

revert_vector(top_type, nmes = colnames(terms)[-ncol(terms)])
