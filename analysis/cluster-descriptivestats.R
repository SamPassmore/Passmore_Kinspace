

jaccard_dist <- function(v1, v2) {
  
  a <- sum(v1 == 1 & v2 == 1)
  b <- sum(v1 == 1 & v2 == 0)
  c <- sum(v1 == 0 & v2 == 1)
  
  jacc <- a / (a + b + c)
  return(1 -jacc)
}

type = "g1"

cluster = read.csv(
  paste0('results/hdbscan/', type , '.csv'), 
  stringsAsFactors = FALSE)

terms = read.csv(
  paste0('data/terms/', gsub("s$", "", type), '_terms.csv'), 
                 stringsAsFactors = FALSE)

structural_vectors = read.csv(
  paste0('data/matrix/', type, '_matrix.csv'), 
  stringsAsFactors = FALSE)

## descriptive statistics 
n_lang = nrow(terms) # how many languages
n_types = nrow( # how many unique organisations
  unique(structural_vectors[,-ncol(structural_vectors)])
)
n_classified = sum(cluster$label_ > -1) # how many languages are classified total

## diversity in unclassified types
unclassified_gc = cluster$Glottocode[cluster$label_ == -1]

uc_types = structural_vectors[structural_vectors$Glottocode %in% unclassified_gc ,-ncol(structural_vectors)]
n_uctypes = nrow(unique(uc_types))
## space diversity nrow(structural_vectors)

# distance between all vectors
# all_distances = combn(1:nrow(structural_vectors), 2, 
#              function(x){
#                jaccard_dist(structural_vectors[x[1],-ncol(structural_vectors)], 
#                             structural_vectors[x[2],-ncol(structural_vectors)])
#              }, simplify = TRUE)
# 
# # into a matrix
# dist_mat = matrix(NA, ncol = nrow(structural_vectors), 
#                   nrow = nrow(structural_vectors))
# dist_mat[lower.tri(dist_mat)] = all_distances
# 
# mean_distances = mean(all_distances, na.rm = TRUE)
# sd_distances = sd(all_distances, na.rm = TRUE)

#plot(density(all_distances, na.rm = TRUE))

# output
# descriptions = c("# languages", "# types", "# classified", "mean distance", "sd distance")
# values = c(n_lang, n_types, n_classified, mean_distances, sd_distances)

descriptions = c("# languages", "# types", "# classified", "# uc types")
values = c(n_lang, n_types, n_classified, n_uctypes)


out = data.frame(descriptions = descriptions, values = values)

write.csv(out, 
          paste0('results/type-descriptions/', type, '_description.csv')
)