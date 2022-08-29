# /usr/bin/Rscript

suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(ggdag))
suppressMessages(library(ggplot2))
suppressMessages(library(igraph))
suppressMessages(library(vegan))
suppressMessages(library(tidyr))
suppressMessages(library(boot))
suppressMessages(library(Rmisc))

seed = 567987

## functions
source('processing/helper.R')

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 1

args = commandArgs(trailingOnly = TRUE)

# args
type = args[1]

dg = suppressMessages(read_xlsx(
  paste0('results/type-descriptions/', type, '.xlsx'), 
         sheet = "dag", col_names = FALSE))

glottolog = read.csv('raw/glottolog-cldf/cldf/languages.csv', stringsAsFactors = FALSE)

cluster = read.csv(
  paste0('results/hdbscan/', type, '.csv'), 
  stringsAsFactors = FALSE) %>% 
  dplyr::left_join(., glottolog, "Glottocode")

new_labels = suppressMessages(
  read_xlsx(
  paste0('results/type-descriptions/', type, '.xlsx'), sheet = 1, .name_repair = "universal") %>% 
  select(Label, Coded.Description) %>% 
  filter(Label > -1)
)


# read in DAG
# dag = tidy_dagitty(unlist(dg[2]))
# edges = data.frame(from = dag$data$name, to = dag$data$to, stringsAsFactors = FALSE)

## weighted network
edges = read_xlsx(
  paste0('results/global/networks/vertices/', type, '.xlsx')
)

edge_weights = read.csv('results/global/networks/edge_weights/change.csv', stringsAsFactors = FALSE) %>%
  left_join(edges, "change")
edge_weights = read.csv('results/global/networks/edge_weights/level.csv', stringsAsFactors = FALSE) %>%
  left_join(edge_weights, "level")
edge_weights = read.csv('results/global/networks/edge_weights/type.csv', stringsAsFactors = FALSE) %>%
  left_join(edge_weights, "type")

edge_weights$weight = edge_weights$type.value * edge_weights$level.value * edge_weights$change.value


# save vertices
# write.csv(edges, 
#           paste0('results/global/networks/vertices/', type, ".csv")
#           )

edges = data.frame(to = edge_weights$to, from = edge_weights$from, weight = edge_weights$weight)
edges = node_values(edges)
# find isolated nodes
no_outs = edges[is.na(edges$to),]
isolated = no_outs[!(no_outs$from %in% edges$to),'from']
# connected nodes
edges = edges[!is.na(edges$to),]
# create igraph format of DAG
simple_igraph = graph.data.frame(edges)
#simple_igraph = igraph::graph_from_edgelist(as.matrix(edges))
simple_igraph = add_vertices(simple_igraph, length(isolated), name=isolated)
# pdf(
#   paste0('results/global/networks/', type, '.pdf')
# )
# plot(simple_igraph) # check it was read correctly
# dev.off()


# Determine the centrality of types in the network
# centrality in the total network
centrality_degree = igraph::degree(simple_igraph, mode = "total")
#centrality_eigen = igraph::eigen_centrality(simple_igraph)$vector
#centrality_pr = igraph::page.rank(simple_igraph)$vector
#centrality_close = igraph::closeness(simple_igraph)
centrality = centrality_degree
centrality_df = data.frame(label_ = names(centrality), freq = (centrality))

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
simpson_raw = data.frame(label_ = as.factor(rownames(sr)), diversity = sr)

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
