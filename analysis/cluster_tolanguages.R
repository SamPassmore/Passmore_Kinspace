## Cluster formatting
library(dplyr)
library(tidyr)

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

cluster_files = list.files('results/hdbscan/', full.names = TRUE)
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

languages = read.csv('kinbank/kinbank/cldf/languages.csv')

language_clusters = left_join(languages, clusters_wide, by = "Glottocode")


write.csv(language_clusters, 'results/kinbank_wclusters.csv')
