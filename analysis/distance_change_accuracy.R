# Cluster comparison
library(mclust)

f_jaccard = list.files("results/hdbscan/", pattern = "jaccard.csv", full.names = TRUE)
f_hamming = list.files("results/hdbscan/", pattern = "hamming.csv", full.names = TRUE)

jaccard = lapply(f_jaccard, read.csv)
hamming = lapply(f_hamming, read.csv)

names(jaccard) = tools::file_path_sans_ext(basename(f_jaccard))
names(hamming) = tools::file_path_sans_ext(basename(f_hamming))

adjusted_rand = c()
for(i in 1:length(jaccard)){
  adjusted_rand[i] = mclust::adjustedRandIndex(jaccard[[i]]$label_,
                            hamming[[i]]$label_)  
}
names(adjusted_rand) = names(jaccard)
