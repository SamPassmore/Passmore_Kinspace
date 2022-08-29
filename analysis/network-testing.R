## testing for network based changes in kinship terms

library(network)
library(dplyr)
library(sna)
library(igraph)

start = c("a", "a", "a", "b", "b", "c", "c", "d", "d")
end = c("b", "c", "d", "a", "d", "a", "d", "b", "c")
# changes to are more likely than changes from
# i.e. gaining words is more likely than loosing words
# here if the letter is later in the alphabet that is a gained word
w1 = c(2, 2, 2, 1, 2, 1, 2, 1, 1)
# changes from are more likely than changes to
# i.e. losing words is more likely than gaining
w2 = c(1, 1, 1, 2, 1, 2, 1, 2, 2)

nodes = data.frame(id = 1:4, label = unique(c(to, from)), stringsAsFactors = FALSE)
edges = data.frame(start, end, w1, w2, stringsAsFactors = FALSE)

edges = edges %>% 
  left_join(nodes, by = c("start" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("end" = "label")) %>% 
  rename(to = id) 

edges_w = select(edges, from, to, w1, w2)
edges = select(edges, from, to)

simple_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist")
simple_igraph = graph_from_edgelist(as.matrix(edges))

simple_igraph = set_edge_attr(simple_igraph, "weight", value = edges_w$w1)

plot(simple_network, vertex.cex = 3, displaylabels = TRUE)
plot(simple_igraph, vertex.cex = 3)

sna::degree(simple_network, gmode = 'digraph')
igraph::degree(simple_igraph, mode = "total")
igraph::degree(simple_igraph, mode = "in")
igraph::degree(simple_igraph, mode = "out")
igraph::strength(simple_igraph, mode = "in")
