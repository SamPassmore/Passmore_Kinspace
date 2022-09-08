# silhouettes

suppressPackageStartupMessages({
  library(dplyr)
  library(cluster)
  library(proxy)
  library(assertthat)
})


kintype = c("g0", "g1", "g2", "siblings", "niblings")

clusters_all = read.csv('results/kinbank_wclusters.csv')

for(k in kintype){
  cat("Analysing:", k, "\n")
  clusters = clusters_all[!is.na(clusters_all[,k]),]
  clusters$label_ = clusters[,k]
  
  structural_vectors = read.csv(paste0('data/matrix/', k,'_matrix.csv'))
  x = assert_that(nrow(structural_vectors) == nrow(clusters))
  
  # order is the same
  clusters = clusters[order(clusters$Glottocode_ID),]
  structural_vectors = structural_vectors[order(structural_vectors$Glottocode),]
  
  x = assert_that(all(clusters$Glottocode_ID == structural_vectors$Glottocode))
  
  # remove outliers since we don't care about their silhouette score
  # idx = clusters$label_ != "Outlier"
  # clusters = clusters[idx,]
  # structural_vectors = structural_vectors[idx,]
  
  ## Silhouette requires clusters are integer codes
  labels_df = data.frame(label_ = sort(unique(clusters$label_)))
  labels_df$integers = 1:nrow(labels_df)
  
  clusters = left_join(clusters, labels_df, by = "label_")
  
  # sort in order
  cluster_order = order(clusters$integers)
  clusters = clusters[cluster_order,]
  labels = clusters$integers
  structural_vectors = structural_vectors[cluster_order,]
  
  x = assert_that(all(clusters$Glottocode_ID == structural_vectors$Glottocode))
  
  dist = proxy::dist(structural_vectors[,-ncol(structural_vectors)], by_rows = TRUE, method = "Jaccard")
  
  ss = silhouette(labels, dist)
  ss_df = as.data.frame(ss)
  
  ss_df = left_join(ss_df, 
                 labels_df, 
                 by = c("cluster" = "integers")) %>% 
    left_join(., labels_df, by = c("neighbor" = "integers"), suffix = c("", ".neighbor"))
  
  cluster_coef = tapply(ss_df$sil_width, ss_df$label_, median)
  cluster_coef = data.frame(cluster = names(cluster_coef), value = round(cluster_coef, 2))
  
  write.csv(cluster_coef, paste0('results/global/silhouette/', k, '.csv'), row.names = FALSE)
  write.csv(ss_df, paste0('results/global/silhouette/', k, '_all.csv'), row.names = FALSE)
  
  pdf(paste0('results/global/silhouette/', k, '.pdf'))
    plot(ss, col = as.factor(labels))
  dev.off()
}

