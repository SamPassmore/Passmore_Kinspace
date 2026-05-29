# /usr/bin/Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggdag)
  library(ggplot2)
  library(igraph)
  library(vegan)
  library(tidyr)
  library(boot)
  library(Rmisc)
})

seed = 567987

## functions
source('processing/helper.R')

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 1

args = commandArgs(trailingOnly = TRUE)

# args
type = args[1]

cat("Analysing:", type, "\n")

glottolog = read.csv('https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv', stringsAsFactors = FALSE)

descriptions = suppressMessages(
  read_xlsx("data/typology.xlsx", sheet = type, .name_repair = "universal")
)

cluster = read.csv('results/kinbank_wclusters.csv') %>% 
  dplyr::left_join(., glottolog, "Glottocode")

cluster = cluster[!is.na(cluster[,paste0(type, "_hamming")]),]
cluster$label_ = cluster[,paste0(type, "_hamming")]

cluster_subset = cluster %>% 
  filter(label_ != "Outlier") %>% 
  filter(!is.na(Family_ID)) %>% 
  filter(Family_ID != "")

cluster_subset$label_ = factor(cluster_subset$label_)

# Calculate diversity
sr = get_diversity(cluster_subset, TRUE)
simpson_raw = data.frame(label_ = as.factor(rownames(sr)), diversity = round(sr, 2))

# Calculate the frequency of each type
frequency_raw = as.data.frame(table(cluster$label_))
colnames(frequency_raw) = c("label_", "frequency")

# Build summary output
out = dplyr::left_join(simpson_raw, frequency_raw, by = "label_")

out$type = type

write.csv(out[,c("label_", "frequency", "diversity", "type")], 
          file = 
            paste0('results/global/data/',
                   type,
                   '.csv'),
          row.names = FALSE)
