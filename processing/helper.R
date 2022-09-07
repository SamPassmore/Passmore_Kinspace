node_values = function(edges){
  edges$to = stringr::str_extract(edges$to, "[0-9]+")
  edges$from = stringr::str_extract(edges$from, "[0-9]+")
  edges
}

## Term comparison function
get_dist = function(x, y, max_char, method = "osa"){
  if(method == "binary"){
    ifelse(x == y, 1, 0)
  } else {
    stringdist::stringdist(x, y, method = method) / max_char 
  }
}

get_vector = function(x, method){
  max_char = sapply(x, nchar) %>% max(.)
  ## compare all values to each other, then take the lower trianble of the matrix
  ## i.e. we don't need every comparison twice.
  m = outer(x, x, get_dist, method = method, max_char = max_char) %>% 
    .[lower.tri(.)] %>% 
    c(.)
  
  new_colnames = outer(names(x), names(x), paste0)
  names(m) = new_colnames[lower.tri(new_colnames)]
  m
}

# --- Functions --- #
# # conditional probability
cond_prob_BgvA = function(a, b){
  require(tidyr)
  if(length(a) != length(b))
    stop("x and y must be the same length")
  
  if(any(is.na(a) | is.na(b))){
    stop("remove na's")
  }
  
  unqe_a = unique(a)
  unqe_b = unique(b)
  
  cond_prob = expand_grid(unqe_a, unqe_b)
  cond_prob$cond_prob = rep(NA, nrow(cond_prob)) %>% as.numeric()
  cond_prob$count = rep(NA, nrow(cond_prob)) %>% as.numeric()
  
  tble = table(a, b)
  
  for(i in 1:nrow(cond_prob)){
    p_a = sum(a == c(cond_prob[i,1])) / length(a)
    p_anb = sum(a == c(cond_prob[i,1]) & b == c(cond_prob[i,2])) / length(a)
    cond_prob[i,"cond_prob"] = p_anb / p_a
    
    row = which(rownames(tble) == as.character(cond_prob[i,1]))
    column = which(colnames(tble) == as.character(cond_prob[i,2]))
    
    cond_prob[i,"count"] = tble[row,column]
  }
  cond_prob[order(cond_prob$cond_prob),]
}

# function test
# a = c("a", "b", "c")
# b = c(1, 2, 3)
# cond_prob_BgvA(a, b)

revert_vector = function(v, nmes){
  require(igraph)
  
  n = length(v)
  dims = 0.5 * (sqrt((8 * n) + 1) + 1)
  mat = matrix(NA, ncol = dims, nrow = dims)
  mat[lower.tri(mat)] = v
  mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
  diag(mat) = 1
  
  dd = graph_from_adjacency_matrix(mat, mode = "undirected") %>% 
    clusters() %>% {.$membership}
  names(dd) = nmes
  dd
}

get_diversity = function(x, i){
  # get subset
  # i in the rows is where the bootstrapping takes place
  x = x[i,]
  
  # get counts
  cluster_count = x %>% 
    group_by(label_, Family_ID) %>% 
    dplyr::summarise(count = n(), .groups = "drop_last")
  
  # make community matrix
  community_matrix = tidyr::spread(cluster_count[,c("label_", "Family_ID", "count")], 
                                   key = "label_", value = "count", fill = 0) # NAs are 0
  
  diversity_matrix = as.matrix(community_matrix[,!names(community_matrix) %in% "Family_ID"])
  rownames(diversity_matrix) = community_matrix$Family_ID
  dm = vegan::diversity(diversity_matrix, MARGIN = 2, index = "simpson") # diversity measure
  
  # make output table
  types = unique(x$label_)
  out = matrix(NA, nrow = length(types))
  rownames(out) = sort(types)
  
  out[names(dm),] = dm
  out
}

