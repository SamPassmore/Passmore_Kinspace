# nice graphs

library(igraph)
library(RColorBrewer)
library(stringr)
library(readxl)
library(ggdag)
library(rgexf) 
library(tidyr)

source('processing/saveasgephi.R')


colour_palatte = c('#297AB1', '#57B5ED', '#71AB7F', '#FBBE4B', 
                   "#FF9438", "#8980D4", "#ED8F57",
                   '#BFD7E8', '#BCE1F8', '#C6DDCC', 
                   '#FDE5B7', '#FFD4AF', "#D0CCEE", 
                   "#F8D2BC", "#AB4E68", "#17C3B2")


#### Functions ####
make_letters = function(x){
  y = vector(length = length(x))
  for(i in seq_along(x)){
    if(x[i] == 0)
      y[i] = 0
    if(x[i] > 0)
      y[i] = LETTERS[x[i]]
    if(x[i] < 0)
      y[i] = letters[abs(x[i])]
  }
  y
}

type = "niblings"
# type = "siblings"

edge_list = read_xlsx(
  paste0('results/global/networks/vertices/', type, '.xlsx'),
  na = "NA"
)

# nodes and numbers
hdbscan = read.csv(
  paste0('results/hdbscan/', type,'.csv'))
col_order = names(sort(table(hdbscan$label_), decreasing = FALSE)) %>% as.numeric()

nodes = unique(c(edge_list$from, edge_list$to)) 
n.cols = length(nodes)
if(n.cols < length(colour_palatte)){
  g.cols = colour_palatte[1:n.cols]
} else {
  extra = n.cols - length(colour_palatte)
  g.cols = c(colour_palatte, colorRampPalette(colour_palatte)(extra)) # + 1?
}



#idx = which(col_order <= -1)
#nodes = R.utils::insert(nodes, ats = idx, NA)
# nodes = nodes[!is.na(nodes)]

# node_colours = data.frame(nodes = nodes, 
#                           colours = g.cols[col_order + 2])

node_colours = data.frame(nodes = nodes, 
                          colours = g.cols)

# get numbers from edges
edge_list$from = str_extract(edge_list$from, '-?[0-9]+\\s?') %>% str_trim()
edge_list$to   = str_extract(edge_list$to, '-?[0-9]+\\s?') %>% str_trim()

# remove numbers from edges
# edge_list$from = str_remove(edge_list$from, '[0-9]+\\s?')
# edge_list$to = str_remove(edge_list$to, '[0-9]+\\s?')

# remove edges that go nowhere
edge_list = edge_list[!is.na(edge_list$to),]

# change edges to letters
label_frequency = sort(table(hdbscan$label_[hdbscan$label_ > -1]), decreasing = TRUE)
labels = data.frame(old_labels = names(label_frequency),
                    new_labels = 1:length(label_frequency))


edge_list = dplyr::left_join(edge_list, labels, by = c("from" = "old_labels"))
edge_list$from[edge_list$from >= 0] = as.character(edge_list$new_labels[edge_list$from >= 0])

edge_list = dplyr::left_join(edge_list, labels, by = c("to" = "old_labels"), suffix = c("", ".to"))
edge_list$to[edge_list$to >= 0] = as.character(edge_list$new_labels.to[edge_list$to >= 0])

edge_list$from = as.numeric(edge_list$from)
edge_list$to = as.numeric(edge_list$to)

# edge_list to letters 
edge_list$from = make_letters(edge_list$from)
edge_list$to   = make_letters(edge_list$to)

# --- Create graph --- #
# create igraph from edge list
simple_igraph = igraph::graph_from_edgelist(as.matrix(edge_list[,c("from", "to")]))

# -- Layout -- #
l = layout_with_graphopt(simple_igraph, charge = 1)

# -- Node details -- #
n = length(V(simple_igraph))
nodes = get.vertex.attribute(simple_igraph)
V(simple_igraph)$color = colorRampPalette(brewer.pal(8, "Set2"))(n)
V(simple_igraph)$label.color = node_colours$colours[!is.na(node_colours$nodes)]

# -- Edge details -- #
# make edge labels
edge_labels = edge_list$rule_label
names(edge_labels) = paste0(edge_list$from, "|", edge_list$to)
edge_labels = edge_labels[attr(E(simple_igraph), "vnames")]
E(simple_igraph)$edge.color = "gray80"

plot.igraph(simple_igraph, 
     arrow.mode = '-',
     vertex.label.cex=1.1,
     vertex.font =2 ,
     layout = layout_with_gem,
     vertex.shape="none",
     edge.label = edge_labels,
     edge.label.font = 3, 
     edge.label.cex = 0.8,
     edge.arrow.mode = 0
)

# dynamically change layout
tkplot(simple_igraph, 
        arrow.mode = '-',
        vertex.frame.color="black", 
        vertex.label.color="black", 
        vertex.label.cex=1.1,
        vertex.font =2 ,
        layout = layout_with_gem,
        vertex.shape="none",
        edge.label = edge_labels,
        edge.label.font = 3, 
        edge.label.cex = 1,
        edge.arrow.mode = 0)
