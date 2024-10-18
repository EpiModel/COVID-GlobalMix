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

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")

# N = node_attribute_target_stats$attr$rural %>% nrow()
N = length(node_hh_assign_rural$assignments$hh)


mean_deg = 2*sum(node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home)/N

edge_stat = (mean_deg/2)*N


nw <- network_initialize(N)
#nw_hh_bound <- set_vertex_attribute(nw, "hh_id", node_attribute_target_stats$attr$rural$hh)
nw_hh_bound <- set_vertex_attribute(nw, "hh_id", node_hh_assign_rural$assignments$hh)

# dissolution model
coef.diss <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = 1e+12)




# network w/o household boundary
g <- 
netest(
  nw = nw,
  formation = ~edges ,
  target.stats =  c(edge_stat),
  coef.diss = coef.diss)$fit
g <- 
  simulate(g)
plot(g)
mean(get_degree(g))
table(get_degree(g))
table(component.dist(g, connected = "weak")$csize)
components(g, connected = "weak")

table(component.dist(g, connected = "weak")$membership)

# network w/ houshold boundary
## this may be caused by the hh_id is too restrictive
### Sam's script

help(package = "network")
expand.grid(which(nw_hh_bound %v% "hh_id" == 1))
g_test<-network.initialize(3) 
add.edge(g_test, 1,2)
as.edgelist(g_test)

expand.grid(g_test)

### adaptation
edgelist_df <- 
node_hh_assign_rural$edgelist

# Retrieve the household IDs from the network for all nodes
hh_ids <- get.vertex.attribute(nw_hh_bound, "hh_id")


# Get the unique household IDs in the edgelist_df
unique_hh_all <- sort(unique(hh_ids)) # unique hh_ids of all nodes 
unique_hh_w_edges <- unique(edgelist_df$hh)  # unique hh_ids of all edges, 3 household ids aren't here given the nodes live alone

# Loop through each unique household
for(hh in unique_hh_w_edges
    ) {
  # Subset the rows in edgelist_df for the current household
  hh_subset <- edgelist_df[hh == edgelist_df$hh, ]
  
  # Loop through each row in the subset for the current household
  for(i in 1:nrow(hh_subset)) {
    # Get the .head and .tail node IDs directly
    head_node <- hh_subset$.head[i]
    tail_node <- hh_subset$.tail[i]
    
    # Add the edge between these nodes in the network
    nw_hh_bound <- add.edges(nw_hh_bound, head_node, tail_node)
  }
}

# verify whether the edge list belong to hh ==3
nodes_in_hh_4 <- node_hh_assign_rural$assignments$ids[which(hh_ids == 4)] # node id whose hh_id==3
edgelist <- as.edgelist(nw_hh_bound)

data.frame(edgelist)[valid_edges,]

# Check if all edges correspond to nodes within household hh == 3;  validate this more...
valid_edges <- apply(edgelist, 1, function(edge) {
  all(edge %in% nodes_in_hh_4)
})

# Show valid edges
edgelist[valid_edges, ]

# plot added edges
## color
colors <- rainbow(length(unique_hh_all))
color_map <- setNames(colors, unique_hh_all)
vertex_colors <- color_map[as.character(hh_ids)] 
# label nodes
vertex_labels <- 
  paste0(
    "nd_", node_hh_assign_rural$assignments$ids, 
    "@HH", hh_ids
  )



# Convert the network object to an igraph object using intergraph
igraph_net <- asIgraph(nw_hh_bound)  # Convert nw_hh_bound to an igraph object

# Extract node attributes (node ID and household ID)
node_df <- data.frame(
  id = V(igraph_net)$name,                # Node IDs
  hh_id = get.vertex.attribute(nw_hh_bound, "hh_id")  # Household IDs
)

# Plot the network using ggraph and ggplot2
ggraph(igraph_net, layout = 'fr') +       # Use a force-directed layout (or choose another)
  geom_edge_link(alpha = 0.8) +           # Plot the edges
  geom_node_point(
    aes(
    color = 
      factor(node_hh_assign_rural$assignments$hh)
    ), 
    size = 5,
    show.legend = FALSE) +   # Color nodes by household
  geom_node_text(aes(label = 
                       paste("nd", node_hh_assign_rural$assignments$ids, 
                             "_@HH", node_hh_assign_rural$assignments$hh))
                 ,
                 vjust = 1.5, size = 1.5) + # Add node and household labels
  scale_color_discrete(name = "Household") +  # Legend for household color
  theme_void() +                          # Minimalistic theme for network graphs
  ggtitle("Network with Abbreviated Node and Household Labels")




