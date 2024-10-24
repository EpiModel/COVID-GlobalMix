library(sna)
library(EpiModel)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(intergraph)

# Loading target stats and nodal attributes
node_attribute_target_stats<-
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", 0.001, ".Rds"))

N = node_attribute_target_stats$attr$rural %>% nrow()

nw <- network_initialize(N)


# Assign edges to the network item
## A vector of heads and tails of each edge 
head_vec = node_attribute_target_stats$node_hh_assign$edgelist$rural$head.node.ids
tail_vec = node_attribute_target_stats$node_hh_assign$edgelist$rural$tail.node.ids


## Assign the heads and tails to the network item by node id
### note: node.id serve as a global node id which are unique across the whole network
# the edges by head and tail are added using the global node ids - while household ids are not intentionally added, the edges of the same households are inherently grouped together
# hence, the edge adding is independent of household ids and solely depends on the node ids
nw <- network::add.edges(
  nw,
  head_vec, tail_vec) 


# Assign household id to each node
### note: although we assign hh.id attribute here, the adding of edge to nodes is completely independent from hh.id, detailed explanation is in below
nw <- set_vertex_attribute(nw, "hh.id", node_attribute_target_stats$attr$rural$hh.ids)
plot(nw)

# Plot by household id
hh_ids <- node_attribute_target_stats$attr$rural$hh.ids
vertex_colors <- rainbow(length(unique(hh_ids)))[as.numeric(factor(hh_ids))]
network::plot.network(nw, vertex.col = vertex_colors)

# Fancier plotting using igraph
igraph_net <- asIgraph(nw)  # Convert nw to an igraph object

# Extract node attributes (node ID and household ID)
# Plot the network using ggraph and ggplot2
home_layer_fig <- 
ggraph(igraph_net, layout = 'fr') +       # "fr" pulls connected nodes together
  geom_edge_link(alpha = 0.7) +          
  geom_node_point(
    aes(
    color = 
      factor(node_attribute_target_stats$attr$rural$hh.ids)
    ), 
    size = 1,
    show.legend = FALSE)
home_layer_fig 

# Validating network statistics 
##md
table(
nw %v% "hh.id") %>% mean

## degrange
degree(igraph_net) %>% table() %>% barplot()


