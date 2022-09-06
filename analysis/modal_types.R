# modal type
library(plyr)
library(dplyr)
library(stringr)
source('processing/helper.R')

kinspace_results = read.csv('results/kinbank_wclusters.csv')

cluster_sets = c("g0", "g1", "g2", "niblings", "siblings")

modal_types = vector(mode = "list", length = length(cluster_sets))
names(modal_types) = cluster_sets
for(c in cluster_sets){
  
  clusters = kinspace_results[,c]
  cluster_types = sort(unique(clusters[!is.na(clusters)]))
  
  matrix = read.csv(
    paste0("data/matrix/", c,"_matrix.csv")
  )
  terms = read.csv(
    paste0('data/terms/', c, '_terms.csv')
  )
  
  kin_categories = colnames(terms)[-ncol(terms)]
  
  nmes_idx = sapply(
    kin_categories, function(t)
      any(str_detect(names(matrix), t) == TRUE)
  )
  used_kin_categories = kin_categories[nmes_idx]
  
  count = c()
  df = matrix(NA, ncol = length(used_kin_categories) + 2, nrow = length(cluster_types))
  
  for(i in seq_along(cluster_types)){
    c_t = cluster_types[i]
    langs = 
      kinspace_results %>% 
      filter(get(c) == c_t) %>% 
      pull(Glottocode)  
    
    idx = matrix$Glottocode %in% langs
    types = subset(matrix, select = -c(Glottocode), subset = idx)
    
    # modal type
    freq = ddply(types, colnames(types), nrow) %>% 
      arrange(desc(V1))
    
    count = max(freq$V1)
    
    top_type = freq %>% 
      slice_max(V1, n = 1) %>% 
      select(-V1) %>% 
      slice_sample(n = 1) %>% 
      unlist()
    
    reverted = revert_vector(top_type, nmes = used_kin_categories)
    
    df[i,] = 
      c(c_t,
        count,
        unlist(reverted)
      )
    colnames(df) = c("Cluster", "Count", used_kin_categories)
    
  }
  modal_types[[c]] = df
}

filenames = paste0("results/modaltype_",
                   names(modal_types),
                   ".csv")
purrr::map2(modal_types, filenames, function(x, y) write.csv(x, y, row.names = FALSE))


