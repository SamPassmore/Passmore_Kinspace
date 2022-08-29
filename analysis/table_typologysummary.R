# --- Typology summary --- #

library(dplyr)
library(boot)

files = list.files('data/matrix/', full.names = TRUE)

matrices = lapply(files, read.csv)

n = lapply(matrices, nrow) %>% unlist()
unique_types = lapply(matrices, function(x) nrow(unique(as.matrix(x[,-ncol(x)])))) %>% unlist()
type_byN = unique_types / n

unq_types = function(x, i){
  nrow(unique(as.matrix(x[i,-ncol(x)])))
}