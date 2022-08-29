# networks
# the dag network is organised so that a direct link indicates a gain of a term
# this means that degree for in terms indicates the weight for gaining terms
# and the degree for out indicates the weight for the loss of terms

library(ggdag)
library(igraph)
library(bayestraitr)

siblings_dag = 'dag {
1 -> 2
1 -> 3
2 -> 10
2 -> 11
3 -> 4
3 -> 8
3 -> 9
4 -> 9
5 -> 9
6 -> 9
7 -> 9
}

'

siblings = tidy_dagitty(siblings_dag)

edges = data.frame(to = siblings$data$name, from = siblings$data$to, stringsAsFactors = FALSE)
edges = edges[!is.na(edges$from),]
# nodes = data.frame(label = unique(c(siblings$data$name, siblings$data$to)), stringsAsFactors = FALSE)
# nodes$id = 1:nrow(nodes)

# edges = edges %>% 
#   left_join(nodes, by = c("start" = "label")) %>% 
#   rename(from = id)
# 
# edges <- edges %>% 
#   left_join(nodes, by = c("end" = "label")) %>% 
#   rename(to = id) 
# 
# edges = select(edges, to, from)

simple_igraph = graph_from_edgelist(as.matrix(edges))
plot(simple_igraph)

# centrality in the total network
igraph::degree(simple_igraph, mode = "total")
# gain of terms
igraph::degree(simple_igraph, mode = "in")
# loss of terms
igraph::degree(simple_igraph, mode = "out")
# strength indicates weighted degree (currently there are no weights on this network)
igraph::strength(simple_igraph, mode = "in")

# Bantu siblings
sib_df = read.csv('code/data/bayestraits-input/sibling_bt.btdata', sep = "\t", header = FALSE)
rownames(sib_df) = sib_df$V1
tree = read.nexus('code/data/bayestraits-input/sibling_bt.bttrees')
sib_df$V2 = factor(sib_df$V2, levels =  (c("-", 1:11)))
sib_df = sib_df[(tree[[1]]$tip.label),]

plot(tree[[1]], cex = 0.5)
tiplabels(pch=19, col = sib_df$V2)

# subset dag to bantu
parameters = unique(sib_df$V2)

edges_bt = edges %>% 
  filter(edges$to %in% parameters & edges$from %in% parameters)

bt_igraph = graph_from_edgelist(as.matrix(edges_bt))
plot(bt_igraph)

igraph::degree(bt_igraph, mode = "total")
table(sib_df$V2)

ggplot(sib_df, aes(V2)) + geom_bar()

new_transitions = paste(edges_bt$from, "->", edges_bt$to)
new_dag = paste("dag {", paste(new_transitions, collapse = "\n"), "}")

model = bt_addmodel(new_dag)
