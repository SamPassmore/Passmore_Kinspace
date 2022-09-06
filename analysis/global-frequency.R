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
type = "niblings"

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

cat("Unconnected Nodes:")
cat(paste(
  descriptions$Node.Name[descriptions$Cluster %in% unconnected_edges], collapse = " \n"
))

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
#simple_igraph = igraph::graph_from_edgelist(as.matrix(edges))
simple_igraph = add_vertices(simple_igraph, length(isolated), name=isolated)

# Determine the centrality of types in the network
# centrality in the total network
centrality = igraph::degree(simple_igraph, mode = "total")
#centrality_eigen = igraph::eigen_centrality(simple_igraph)$vector
#centrality_pr = igraph::page.rank(simple_igraph)$vector
#centrality_close = igraph::closeness(simple_igraph)
centrality_df = data.frame(label_ = names(centrality), connections = (centrality))

centrality_strength = igraph::strength(simple_igraph, mode = "total")
centrality_strength = data.frame(label_ = names(centrality_strength), strength = (centrality_strength))
centrality_df = left_join(centrality_df, centrality_strength, by = "label_")

cluster_subset = cluster %>% 
  filter(label_ > -1) %>% 
  filter(!is.na(Family_ID)) %>% 
  filter(Family_ID != "")

cluster_subset$label_ = factor(cluster_subset$label_)

# bootstrapped frequency of types
# --- bootstrapped diversity for variance NOT USED CURRENTlY -- # 
get_frequency = function(x, i){
  tble = table(x$label_[i])
  
  types = unique(x$label_)
  out = matrix(NA, nrow = length(types))
  rownames(out) = sort(types)
  
  out[names(tble),] = tble
  out
  }
#boot_frequency = boot(cluster_subset, get_frequency, R = 1000)
#boot_frequency$t = apply(boot_frequency$t, 1, rescale) %>% t()

# bootstrapped diversity of types
get_diversity = function(x, i){
  # get subset
  # i in the rows is where the bootstrapping takes place
  x = x[i,]
  
  # get counts
  cluster_count = x %>% 
    group_by(label_, Family_ID) %>% 
    dplyr::summarise(count = n())
  
  # make community matrix
  community_matrix = tidyr::spread(cluster_count[,c("label_", "Family_ID", "count")], 
                     key = "label_", value = "count", fill = 0) # NAs are 0
  
  diversity_matrix = as.matrix(community_matrix[,!names(community_matrix) %in% "Family_ID"])
  rownames(diversity_matrix) = community_matrix$Family_ID
  #dm = vegan::diversity(diversity_matrix, MARGIN = 2, index = "invsimpson") # diversity measure
  dm = vegan::diversity(diversity_matrix, MARGIN = 2, index = "simpson") # diversity measure
  dm
  
  # make output table
  types = unique(x$label_)
  out = matrix(NA, nrow = length(types))
  rownames(out) = sort(types)
  
  out[names(dm),] = dm
  out
}
#boot_diversity = boot(cluster_subset, get_diversity, R = 1000)
#boot_diversity$t = apply(boot_diversity$t, 1, rescale) %>% t()

#dim(boot_frequency$t)
# colnames(boot_frequency$t) = 0:(ncol(boot_frequency$t)-1)
# #dim(boot_diversity$t)
# colnames(boot_diversity$t) = 0:(ncol(boot_diversity$t)-1)
# 
# bf_df = gather(
#   as.data.frame(boot_frequency$t)
#   )
# colnames(bf_df) = c("label_", "freq")
# bd_df = gather(
#   as.data.frame(boot_diversity$t)
# )
# colnames(bd_df) = c("label_", "freq")

# plot_df = rbind(centrality_df, bf_df, bd_df)
# plot_df$from = c(rep("Network", (ncol(boot_diversity$t))), rep(c("Frequency", "Simpson"), each = nrow(bf_df)))
# 
# plot_df2 = summarySE(plot_df, measurevar="freq", groupvars=c("label_","from"))
# 
# new_labels$Label = as.factor(new_labels$Label)
# #new_labels$Coded.Description = factor(new_labels$Coded.Description, levels = unique(new_labels$Coded.Description)[order(centrality_pr)])
# plot_df2 = left_join(plot_df2, new_labels, by=c('label_' = 'Label'))
# 
# pdf(
#   paste0('results/global/graphs/', type, '.pdf')
# )
# ggplot(plot_df2, aes(x = Coded.Description, y = freq)) +   # group = id is important!
#   geom_line(size = 1, aes(color = from, group = from)) +
#   geom_errorbar(aes(ymin=freq-ci, ymax=freq+ci, color = from), width=.2) +
#   geom_point(aes(color = from)) +
#   theme(axis.text.x = element_text(angle = 75, hjust = 1),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank(),
#         legend.title = element_blank(),
#         legend.position = c(0.1, 0.85),
#         legend.background = element_rect(size=0.25, linetype="solid",
#                                          colour ="black")) +
#   xlab("") + ylab("Scaled Importance")
# 
# ggplot(plot_df2, aes(x = Coded.Description, y = freq)) +   # group = id is important!
#   geom_line(size = 1, aes(color = from, group = from)) +
#   geom_errorbar(aes(ymin=freq-ci, ymax=freq+ci, color = from), width=.2) +
#   geom_point(aes(color = from)) +
#   theme(axis.text.x = element_text(angle = 75, hjust = 1),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank(),
#         legend.title = element_blank(),
#         legend.position = c(0.1, 0.85),
#         legend.background = element_rect(size=0.25, linetype="solid",
#                                          colour ="black")) +
#   xlab("") + ylab("Scaled Importance")
# dev.off()

# save raw data
centrality_raw = data.frame(label_ = as.factor(names(centrality)), centrality = centrality,
                            strength = centrality_df$strength)

sr = get_diversity(cluster_subset, TRUE)
simpson_raw = data.frame(label_ = as.factor(rownames(sr)), diversity = round(sr, 2))

frequency_raw = as.data.frame(table(cluster$label_))
colnames(frequency_raw) = c("label_", "frequency")

out = dplyr::left_join(simpson_raw, frequency_raw, by = "label_") %>% 
  dplyr::left_join(., centrality_raw, by = "label_")

out$type = type

write.csv(out[,c("label_", "frequency", "diversity", "centrality", "strength", "type")], 
          file = 
            paste0('results/global/data/',
                   type,
                   '.csv'))
