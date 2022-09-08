## Cluster formatting

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(assertthat)
})

to_letters = function(df){
  x = df$label_
  tt = sort(table(x), decreasing = TRUE)
  tt_df = data.frame(tt)
  
  outliers = tt_df[tt_df$x == "-1",]
  
  df_ss = tt_df[tt_df$x != "-1",]
  df_ss$letter_labels = LETTERS[1:nrow(df_ss)]
  
  outliers$letter_labels = "Outlier"
  
  out = rbind(df_ss, outliers)
  out$x = as.numeric(as.character(out$x))
  dplyr::left_join(df, out, c("label_" = "x"))
}

cluster_files = list.files('results/hdbscan/', full.names = TRUE, recursive = FALSE, pattern = "*.csv")
clusters = lapply(cluster_files, read.csv)
names(clusters) = basename(cluster_files) %>% 
  tools::file_path_sans_ext()

# letter codes by frequency
clusters = lapply(clusters, to_letters)

clusters = bind_rows(clusters, .id = "subset")
clusters_wide = pivot_wider(data = clusters[,c("subset", "Glottocode", "letter_labels")], 
                            names_from =  subset, 
                            values_from = letter_labels
                            )

clustersprob_wide = pivot_wider(data = clusters[,c("subset", "Glottocode", "cluster_prob")], 
                            names_from =  subset, 
                            values_from = cluster_prob
)
colnames(clustersprob_wide)[2:ncol(clustersprob_wide)] = paste0(
  colnames(clustersprob_wide)[2:ncol(clustersprob_wide)], ".clusterprobability"
)

clustersoutlier_wide = pivot_wider(data = clusters[,c("subset", "Glottocode", "outlier_prob")], 
                                names_from =  subset, 
                                values_from = outlier_prob
)
colnames(clustersoutlier_wide)[2:ncol(clustersoutlier_wide)] = paste0(
  colnames(clustersoutlier_wide)[2:ncol(clustersoutlier_wide)], ".outlierprobability"
)


clusters_wide = cbind(clusters_wide, 
                      clustersprob_wide[,2:ncol(clustersprob_wide)],
                      clustersoutlier_wide[,2:ncol(clustersoutlier_wide)])

x = assert_that(n_distinct(clusters_wide$Glottocode) == nrow(clusters_wide))

languages = read.csv('kinbank/kinbank/cldf/languages.csv') 

# We used letter suffixes when languages were recorded from different sources or did not have
# distinct glottocodes
languages$Glottocode_ID = stringr::str_extract(languages$ID,
                                               pattern = "[a-z]{4}[0-9]{4}[a-m]?$")

languages = languages %>% 
  group_by(Glottocode_ID) %>% 
  slice_head(n = 1)

x = assert_that(n_distinct(languages$Glottocode_ID) == nrow(languages))

x = assert_that(all(clusters_wide$Glottocode %in% languages$Glottocode_ID))

language_clusters = left_join(languages, clusters_wide, by = c("Glottocode_ID" = "Glottocode"))

write.csv(language_clusters, 'results/kinbank_wclusters.csv', row.names = FALSE)
