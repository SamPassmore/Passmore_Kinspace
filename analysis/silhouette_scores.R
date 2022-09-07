# silhouettes

suppressPackageStartupMessages({
  library(dplyr)
  library(cluster)
  library(proxy)
})


kintype = c("g0", "g1", "g2", "siblings", "niblings")

for(k in kintype){
  clusters = read.csv(paste0('results/hdbscan/', k,'.csv')) %>% 
    filter(label_ != -1)
  
  structural_vectors = read.csv(paste0('data/matrix/', k,'_matrix.csv')) %>% 
    filter(Glottocode %in% clusters$Glottocode)
  
  nrow(structural_vectors) == nrow(clusters)
  
  # sort in order
  cluster_order = order(clusters$label_)
  labels = clusters$label_[cluster_order]
  structural_vectors = structural_vectors[cluster_order,]
  
  dist = proxy::dist(structural_vectors[,-ncol(structural_vectors)], by_rows = TRUE, method = "Jaccard")
  
  ss = silhouette(labels, dist)
  
  cluster_coef = tapply(ss[,3], ss[,1], median)
  cluster_coef = data.frame(cluster = names(cluster_coef), value = cluster_coef)
  
  write.csv(cluster_coef, paste0('results/hdbscan/silhouette/', k, '.csv'), row.names = FALSE)
  write.csv(ss, paste0('results/hdbscan/silhouette/', k, '_all.csv'), row.names = FALSE)
  
  pdf(paste0('results/hdbscan/silhouette/', k, '.pdf'))
    plot(ss, col = as.factor(labels))
  dev.off()
}

