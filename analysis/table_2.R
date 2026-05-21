## table summary 

# N Languages
structure_files = list.files("data/matrix/", full.names = TRUE, pattern = "*.csv")
structures = lapply(structure_files, read.csv)
names(structures) = basename(structure_files)
n_languages = lapply(structures, function(x) length(unique(x$Glottocode)))

names_reorder = c("siblings_matrix.csv", "g0_matrix.csv", "g1_matrix.csv", "g2_matrix.csv", "niblings_matrix.csv")

n_languages = n_languages[names_reorder]

# N observed structures
n_structures = lapply(structures, function(x) {
  tmp = apply(x[, 1:(ncol(x) - 1)], 1, function(y) {
    paste0(y, collapse = "")
  })
  length(unique(tmp))
})
names(n_structures) = basename(structure_files)
n_structures = n_structures[names_reorder]

# N kin types
n_kintypes = c(8, 40, 20, 4, 20)
names(n_kintypes) = c("siblings", "g0", "g1", "g2", "g-1")

# N clusters
cluster_files = list.files("results/hdbscan/", full.names = TRUE, pattern = "*_jaccard.csv")
clusters = lapply(cluster_files, read.csv)
names(clusters) = basename(cluster_files)
n_clusters = lapply(clusters, function(x) length(unique(x$label_[x$label_ != -1])))
names_reorder_cluster = c("siblings_jaccard.csv", "g0_jaccard.csv", "g1_jaccard.csv", "g2_jaccard.csv", "niblings_jaccard.csv")
n_clusters = n_clusters[names_reorder_cluster]

# N possible structures 
n_possible = c("4,140", "1.57 x 1,035", "5.17 x 1,013", 15, "5.17 x 1,013")
names(n_possible) = c("siblings", "g0", "g1", "g2", "g-1")

table_2 = cbind(kinset =  c("siblings", "g0", "g1", "g2", "g-1"),
                n_languages = n_languages,
                n_kintypes = n_kintypes,
                n_observed = n_structures,
                n_clusters = n_clusters,
                n_possible = n_possible)

write.csv(table_2, file = "results/table_2.csv")
