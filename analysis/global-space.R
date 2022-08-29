# /usr/bin/Rscript

# this script creates a distance matrix for the networks
# and a distance matri based on jaccard distance in space

# then calculates mantel tests between these two matrices. 

#suppressPackageStartupMessages(library(ggdag))
#suppressPackageStartupMessages(library(dagitty))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

source("processing/helper.R")

## functions
jaccard_dist <- function(v1, v2) {
  
  a <- sum(v1 == 1 & v2 == 1)
  b <- sum(v1 == 1 & v2 == 0)
  c <- sum(v1 == 0 & v2 == 1)
  
  jacc <- a / (a + b + c)
  return(1 -jacc)
}

## test 
# jaccard_dist(c(1,0,0), c(1,0,0)) # 0
# jaccard_dist(c(0,0,0), c(1,1,1))  # 1
# jaccard_dist(c(1,0,0), c(1,0,1)) # .5 
# jaccard_dist(c(0,1,0), c(1,1,1)) # .6

# network distance vs space distance

# args
args = commandArgs(trailingOnly = TRUE)
type = args[1]

edge_list = read_xlsx(
  paste0('results/global/networks/vertices/', type, '.xlsx')
)

edge_weights = read.csv('results/global/networks/edge_weights/change.csv', stringsAsFactors = FALSE) %>%
  inner_join(edge_list, "change")
edge_weights = read.csv('results/global/networks/edge_weights/level.csv', stringsAsFactors = FALSE) %>%
  inner_join(edge_weights, "level")
edge_weights = read.csv('results/global/networks/edge_weights/type.csv', stringsAsFactors = FALSE) %>%
  inner_join(edge_weights, "type")

edge_weights$weight = edge_weights$type.value * edge_weights$level.value * edge_weights$change.value

# g2 stuffs this up because there are no relational rule changes


# dag = tidy_dagitty(unlist(dg[2]))
# edges = data.frame(from = dag$data$name, to = dag$data$to, stringsAsFactors = FALSE)
# edges = node_values(edges)
# # find isolated nodes
# no_outs = edges[is.na(edges$to),]
# isolated = no_outs[!(no_outs$from %in% edges$to),'from']
# # connected nodes
# edges = edges[!is.na(edges$to),]
# create igraph format of DAG
#simple_igraph = igraph::graph_from_edgelist(as.matrix(edges))
#simple_igraph = add_vertices(simple_igraph, length(isolated), name=isolated)
simple_igraph = igraph::graph_from_edgelist(as.matrix(edge_weights[,c("from", "to")]))

# -- Add weights -- #
simple_igraph = set_edge_attr(simple_igraph, "weight", value = edge_weights$weight)

## create network distance matrix based on cluster types
network_distance = distances(simple_igraph) # distance matrix
network_edges = reshape2::melt(network_distance) # edge list
# make nodes number only to match clusters
network_edges$Var1 = str_extract(network_edges$Var1, "-?[0-9]+") %>% as.numeric()
network_edges$Var2 = str_extract(network_edges$Var2, "-?[0-9]+") %>% as.numeric()



# get language clusters 
cluster = read.csv(
  paste0('results/hdbscan/', type, '.csv'), 
  stringsAsFactors = FALSE) %>% 
  filter(label_ > -1) # remove unclustered languages

# network distance
langnetwork_distance = matrix(NA, ncol = nrow(cluster), nrow = nrow(cluster))
dimnames(langnetwork_distance) = list(cluster$Glottocode, cluster$Glottocode)

# jaccard distance
kinspace_matrix = read.csv(
  paste0('data/matrix/', type, '_matrix.csv'),
  stringsAsFactors = FALSE)

kinspace_matrix = kinspace_matrix[kinspace_matrix$Glottocode %in% cluster$Glottocode,]
km = kinspace_matrix[,-ncol(kinspace_matrix)]
kinspace_distance = matrix(NA, ncol = nrow(kinspace_matrix), nrow = nrow(kinspace_matrix))
dimnames(kinspace_distance) = list(kinspace_matrix$Glottocode, kinspace_matrix$Glottocode)

dim(kinspace_distance) == dim(langnetwork_distance)

## this is very slow
for(i in 1:nrow(langnetwork_distance)){
  for(j in 1:ncol(langnetwork_distance)){
  
    # network distance
    r_cluster = cluster$label_[i]
    c_cluster = cluster$label_[j]
    idx = which(network_edges$Var1==r_cluster&network_edges$Var2==c_cluster)
    dist = network_edges[idx,"value"]
    langnetwork_distance[i,j] = dist
    
    # kinspace distance
    ## if all terms are different from all other terms in both vectors, the distance is zero
    if(all(km[i,] == 0 & km[j,] == 0)){
      jd = 0
    }
    else{
      jd = jaccard_dist(km[i,], km[j,])  
    }

    kinspace_distance[i,j] = jd
  }
}

mantel_results = vegan::mantel(kinspace_distance, langnetwork_distance)

mantel_out = data.frame(statistic = mantel_results$statistic,
           significance = mantel_results$signif)

write.csv(mantel_out,
          paste0('results/global/mantel/', type, '.csv'))