library(sna)
library(EpiModel)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(intergraph)


# read target stats
# Loading data
## target statistics
# node_attribute_target_stats <- 
#   readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", 0.001, ".Rds"))
# 
# node_attribute_target_stats_pt1<- 
#   readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", 0.1, ".Rds"))
# 
# ## check household size
# ### 0.1% target pop
# node_attribute_target_stats$node_hh_assign_validation$rural
# ### 10% target pop 
# node_attribute_target_stats_pt1$node_hh_assign_validation$rural



# N = node_attribute_target_stats$attr$rural %>% nrow()
N = length(node_hh_assign_rural$assignments$node.ids)

nw <- network_initialize(N)

# although we assign hh.id attribute here, the adding of edge is completelu independent from hh.id
nw_hh_bound <- set_vertex_attribute(nw, "hh.id", node_hh_assign_rural$assignments$hh.id)

# network w/ houshold boundary
nw_hh_bound <- network::add.edges(
  nw,
  #nw_hh_bound, # node.id serve as a global node id which are unique across the whole network
  # these edges by head and tail are added using the global node ids - while household ids are not intentionally added, the edges of the same households are inherently grouped together
  # hence, the edge creation is independent of household ids but solely depend on the node ids
  node_hh_assign_rural$edgelist$head.node.ids, node_hh_assign_rural$edgelist$tail.node.ids) 

plot(nw_hh_bound)

# Convert the network object to an igraph object using intergraph
igraph_net <- asIgraph(nw_hh_bound)  # Convert nw_hh_bound to an igraph object

# Extract node attributes (node ID and household ID)
# Plot the network using ggraph and ggplot2
ggraph(igraph_net, layout = 'fr') +       # Use a force-directed layout (or choose another)
  geom_edge_link(alpha = 0.8) +           # Plot the edges
  geom_node_point(
    aes(
    color = 
      factor(node_hh_assign_rural$assignments$hh.ids)
    ), 
    size = 5,
    show.legend = FALSE) +   # Color nodes by household
  geom_node_text(aes(label = 
                       paste("nd", node_hh_assign_rural$assignments$node.ids, 
                             "_@HH", node_hh_assign_rural$assignments$hh.ids))
                 ,
                 vjust = 1.5, size = 1.5) + # Add node and household labels
  scale_color_discrete(name = "Household") +  # Legend for household color
  theme_void() +                          # Minimalistic theme for network graphs
  ggtitle("Edges and nodes in the rural home layer")

# Validating network statistics 
##md
table(
nw_hh_bound %v% "hh.id") %>% mean

## degrange

degree(igraph_net) %>% hist()


