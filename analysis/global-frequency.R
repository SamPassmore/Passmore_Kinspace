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

cluster = cluster[!is.na(cluster[,type]),]
cluster$label_ = cluster[,type]

## weighted network
edges = read_xlsx('data/edges.xlsx', sheet = type, na = "NA")
# remove edges that were deemed to not exist
edges = edges[!is.na(edges$rule),]

# rename nodes with given letters
edges$to = stringr::str_extract(edges$to, "^[A-Z]{1}")
edges$from = stringr::str_extract(edges$from, "^[A-Z]{1}")

# Print nodes that do not have any edges
unconnected_edges_idx = !cluster$label_ %in% c(edges$to, edges$from)
unconnected_edges = unique(cluster$label_[unconnected_edges_idx])

if(length(unconnected_edges) > 0){
  cat("Unconnected Nodes:\n")
  string = paste(
    descriptions$Node.Name[descriptions$Cluster %in% unconnected_edges], collapse = " \n"
  )
  cat(paste0(string, "\n"))
}

edge_weights = read.csv('data/weights/change.csv', stringsAsFactors = FALSE) %>%
  left_join(edges, ., "change")
edge_weights = read.csv('data/weights/level.csv', stringsAsFactors = FALSE) %>%
  left_join(edge_weights, ., "level")
edge_weights = read.csv('data/weights/type.csv', stringsAsFactors = FALSE) %>%
  left_join(edge_weights, ., "type")

edge_weights$weight = edge_weights$type.value * edge_weights$level.value * edge_weights$change.value

edges = data.frame(to = edge_weights$to, from = edge_weights$from, weight = edge_weights$weight)

# find isolated nodes
no_outs = edges[is.na(edges$to),]
isolated = no_outs[!(no_outs$from %in% edges$to),'from']

# create igraph format of DAG
simple_igraph = graph.data.frame(edges)
simple_igraph = add_vertices(simple_igraph, length(isolated), name=isolated)

# Determine the centrality of types in the network
# centrality in the total network
centrality = igraph::degree(simple_igraph, mode = "total")
centrality_df = data.frame(label_ = names(centrality), connections = (centrality))

centrality_strength = igraph::strength(simple_igraph, mode = "total")
centrality_strength = data.frame(label_ = names(centrality_strength), strength = (centrality_strength))
centrality_df = left_join(centrality_df, centrality_strength, by = "label_")

cluster_subset = cluster %>% 
  filter(label_ > -1) %>% 
  filter(!is.na(Family_ID)) %>% 
  filter(Family_ID != "")

cluster_subset$label_ = factor(cluster_subset$label_)

# save raw data
centrality_raw = data.frame(label_ = as.factor(names(centrality)), centrality = centrality,
                            strength = centrality_df$strength)

# Calculate diversity
sr = get_diversity(cluster_subset, TRUE)
simpson_raw = data.frame(label_ = as.factor(rownames(sr)), diversity = round(sr, 2))

# Calculate the frequency of each type
frequency_raw = as.data.frame(table(cluster$label_))
colnames(frequency_raw) = c("label_", "frequency")

# Build summary output
out = dplyr::left_join(simpson_raw, frequency_raw, by = "label_") %>% 
  dplyr::left_join(., centrality_raw, by = "label_")

out$type = type

# Merge silhouette scores
silhouette = read.csv(
  paste0("results/global/silhouette/",
         type,
         ".csv")
)
silhouette$silhouette = silhouette$value

  
out = left_join(out, silhouette, by = c("label_" = "cluster"))

write.csv(out[,c("label_", "frequency", "diversity", "centrality", "strength", "silhouette", "type")], 
          file = 
            paste0('results/global/data/',
                   type,
                   '.csv'),
          row.names = FALSE)
