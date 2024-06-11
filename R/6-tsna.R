# Note: the purpose of this script is to conduct temporal social network analysis
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Rural"/"Urban"/
# est_apch = "sto_apoxy"/"mcmle"/
# layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
# percent_target_pop = 0.1/0.4/1


# Packages
suppressMessages(library(tsna))
suppressMessages(library(EpiModel))
suppressMessages(library(doParallel))
suppressMessages(library(fs))

# Inputs
network <- Sys.getenv("network")
est_apch <- Sys.getenv("est_apch")
percent_target_pop <- Sys.getenv("percent_target_pop")

# Loading data
sim <- readRDS(paste0("data/netsim_outputs/sim_", "__", network,"__", est_apch, ".Rds"))

if (layer == "Home") {
  sim <- sim[["Home"]]
} else if (layer == "School") {
  sim <- sim[["School"]]
} else if (layer == "Work") {
  sim <- sim[["Work"]]
} else if (layer == "Nonhome"){
  sim <- sim[["Nonhome"]]
} else if (layer == "All") {
  sim_home <- sim[["Home"]]
  sim_school <- sim[["School"]]
  sim_work <- sim[["Work"]]
  sim_nonhome <- sim[["Nonhome"]]
  
  
  sim_all <- sim_home
  # Exporting the edge dynamics of layers other than home as dataframes
  ## Note: "onset" and "terminus" mean times that an edge starts and ends
  ## "tail" and "head" meaning the tail and head of the edge, by node/vertex's identifier
  sim_school_df <- as.data.frame.networkDynamic(sim_school)
  sim_work_df <- as.data.frame.networkDynamic(sim_work)
  sim_nonhome_df <- as.data.frame.networkDynamic(sim_nonhome)

  # For each node, add edges of the School, Work, and Nonhome layers to the Home layer, and then return it as a networkDynamic item containing edges at all layers for each node
  sim_all <- add.edges.active(sim_all, tail = sim_school_df[["tail"]], head = sim_school_df[["head"]],
                              onset = sim_school_df[["onset"]], terminus = sim_school_df[["terminus"]]
                              )
  
  sim_all <- add.edges.active(sim_all, tail = sim_work_df[["tail"]], head = sim_work_df[["head"]],
                              onset = sim_work_df[["onset"]], terminus = sim_work_df[["terminus"]]
                              )
  
  sim_all <- add.edges.active(sim_all, tail = sim_nonhome_df[["tail"]], head = sim_nonhome_df[["head"]],
                              onset = sim_nonhome_df[["onset"]], terminus = sim_nonhome_df[["terminus"]]
  )
  
  sim <- sim_all
}

sim 



get_all_frp <- 
  function(  ## Question: go through the script w/ Adrien to check Billy's understanding
    net, ## networkDynamic item
    from = 1, ## the starting time point of the FRP search
    to ## the ending time point of the FRP
    ) {
  ## number of time intervals in the network; Question: How this was defined? 
  last_obs <- length(net$gal$net.obs.period$observations)
  
  ## If the FRP's ending time point is more than the number of time interval of the network, we calculate the FRP until the network's ending time interval
  ## If the FRP's ending time point is less than the number of time interval of the network, we calculate the FRP until the specified FRP's ending time point
  to <- if (to > last_obs) last_obs else to 
  ## Defining number to time steps for the FRP. i.e., number of FRP's time intervals + 1
  n_steps <- to - from + 1 
  ## Number of nodes in the network
  n_nodes <- net$gal$n
  
  # nolint start
  ## Defining empty items (this can be removed)
  onset <- 
    terminus <- 
    head <- 
    tail <- NULL
  
  ## Save the starting (onset), ending (terminus), node ID of the tail and head to a data frame
  df_net <- dplyr::select(
    as.data.frame(net),
    onset, terminus, head, tail
  )
  
  # nolint end
  # the initial FRP contains only the vertex itself
  ## Create a list, each element is for a node
  frp_cur <- as.list(seq_len(n_nodes))
  ## Create a large matrix, whose number of column = FRP's time steps + 1 (for FRP's identifier [1,2,...]) and number of row = total number of nodes
  frp_parts <- matrix(list(numeric(0)), ncol = n_steps + 1, nrow = n_nodes)
  
  ## Assign the FRP's identifier to the first column of the matrix
  frp_parts[, 1] <- frp_cur
  
  ## Present progress of the FRP calculation
  p <- progressr::progressor(n_steps)
  
  ## For each time step, the following is iterated
  for (t in seq_len(n_steps)) {
    ## Progress tracking
    p()
    ## Defining the current time step of interest for the FRP
    cur_step <- t + from - 1
    
    # creation of a `connection` list of vectors
    # for each vertex 1:n_nodes we get a vector of the vertices it connects to
    ## Define an empty list of vector, with each element for a node
    connected <- vector(mode = "list", length = n_nodes)
    # nolint start
    ## Filter out the edges whose onset time is earlier and ending time is later than the FRP's time step - the edges that are active.
    el_t <- dplyr::filter(df_net, onset <= cur_step, terminus > cur_step)
    ## For the filtered edges, retrieve the head and trail information
    el_t <- dplyr::select(el_t, head, tail)
    # nolint end
    ## For each edge from "el_t", assign the node ID of the other side of the edge to the corresponding side in the "connected" list. 
    ## The output of this for loop is the "connected" list containing the nodes that an initial node (in each row) sequentially connected to for a single time step "t"
    for (i in seq_len(nrow(el_t))) {
      e_head <- el_t$head[i]
      e_tail <- el_t$tail[i]
      connected[[e_head]] <- c(connected[[e_head]], e_tail)
      connected[[e_tail]] <- c(connected[[e_tail]], e_head)
    }
    
    # PERF: bottleneck is here
    # frp_v is the current frp for vertex v at timestep t - 1
    # we add to it all the nodes that have edges at timestep t with any of the
    # nodes in the FRP
    # the while loop is to include the nodes that are connected to the FRP through
    # a node added this step
    frp_new <- lapply( ## For each element (i.e., frp_cur[i,j]) in frp_cur, apply the function - frp_cur is read as frp_v in the function
      frp_cur, ## FRP at time t
      function(frp_v) { # a current FRP for a given node
        only_new <- numeric(0)
        new <- frp_v
        ## When edges exist at time t AND ??? Question: what does "length(frp_v) < n_nodes" mean? - if this is F, the calculation should stop
        ## length(new) > 0 - 
        while (length(new) > 0 & length(frp_v) < n_nodes) {
          new <- unlist(connected[new]) # new is the current FRP, see who's connect to "new"
          new <- setdiff(new, frp_v) # find the difference between the new nodes and the node in frp_v
          frp_v <- c(frp_v, new) # frp_v growth at each iteration.
          only_new <- c(only_new, new)
        }
        only_new
      }
    )
    
    ## For each node, combine the FRP at t-1 (frp_cur) and new path at t, which is the FRP at t
    frp_cur <- Map(c, frp_cur, frp_new)
    frp_parts[, t + 1] <- frp_new
  }
  
  return(frp_parts) # the additional nodes rather than all nodes.
}

 
progressr::with_progress(
{test <-get_all_frp(net = sim, to =2)}  
)
  
# Get FRP from node 10.
unlist(test[10, seq_len(2)])  # node 10 has contact with at t == 1

# Get the length of the FRPs for each node at each timestep
## Interpretation of the below result: 
## For primary contact the node had contact with most other nodes in the network, and the number of contact is homogeneous (11756). 
## For secondary contact, Except for 3 nodes without any contact, the number of secondary contact is 21.
frp_parts_length <- apply(test, c(1, 2), function(x) length(x[[1]])) 

table(rowSums(frp_parts_length))

frp_lengths <- t(apply(frp_parts_length , 1,cumsum)) 

## Question should the from_ts be 1 instead of 100 in the example?


 
      


# Outputting estimation result for the network 
file.name <- paste0(
  "data/frp_outputs/frp_",
   network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)


frp_r <- saveRDS(file.name)

