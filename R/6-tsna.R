# Note: the purpose of this script is to conduct temporal social network analysis
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Rural"/"Urban"/
# est_apch = "sto_apoxy"/"mcmle"/
# layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
# percent_target_pop = 0.1/0.4/1

# Packages
rm(list = ls())
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



tp <- tsna::tPath(nd=sim, # networkDynamic object to be search for the FRP
                  v = 1, # integer identifier of node/vertex as starting point of searching the FRP
                  start = 1, # time at which to beginning searching
                  end = 365, # time to end searching
                  direction = "fwd" # searching forward in time and along edge directions
                  ) # this is for calculating the FRP of single node using tPath

tp[["tdist"]] %>% summary() # the earliest temporal distance is 0/Inf
tp[["previous"]] # previous vertx long the FRP
tp[["gsteps"]] %>% hist() # For urban network, the most common step to vertex 1 is ~40
 
plotPaths(sim,
          tp,
          label.cex=0.1)

get_all_frp <- function(net, from = 1, to) {
  last_obs <- length(net$gal$net.obs.period$observations)
  to <- if (to > last_obs) last_obs else to
  n_steps <- to - from + 1
  n_nodes <- net$gal$n
  
  # nolint start
  onset <- terminus <- head <- tail <- NULL
  df_net <- dplyr::select(
    as.data.frame(net),
    onset, terminus, head, tail
  )
  # nolint end
  # the initial FRP contains only the vertex itself
  frp_cur <- as.list(seq_len(n_nodes))
  frp_parts <- matrix(list(numeric(0)), ncol = n_steps + 1, nrow = n_nodes)
  frp_parts[, 1] <- frp_cur
  
  p <- progressr::progressor(n_steps)
  for (t in seq_len(n_steps)) {
    p()
    cur_step <- t + from - 1
    
    # creation of a `connection` list of vectors
    # for each vertex 1:n_nodes we get a vector of the vertices it connects to
    connected <- vector(mode = "list", length = n_nodes)
    # nolint start
    el_t <- dplyr::filter(df_net, onset <= cur_step, terminus > cur_step)
    el_t <- dplyr::select(el_t, head, tail)
    # nolint end
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
    frp_new <- lapply(
      frp_cur,
      function(frp_v) {
        only_new <- numeric(0)
        new <- frp_v
        while (length(new) > 0 & length(frp_v) < n_nodes) {
          new <- unlist(connected[new])
          new <- setdiff(new, frp_v)
          frp_v <- c(frp_v, new)
          only_new <- c(only_new, new)
        }
        only_new
      }
    )
    
    frp_cur <- Map(c, frp_cur, frp_new)
    frp_parts[, t + 1] <- frp_new
  }
  
  return(frp_parts)
}

 
progressr::with_progress(
{test <-get_all_frp(net = sim, to =3)}  
)
  
# Get FRP from node 10.
unlist(test[10, seq_len(2)])  # node at 10 has contact with, 1st time step

# Get the length of the FRPs for each node at each timestep
frp_parts_length <- apply(test, c(1, 2), function(x) length(x[[1]]))

frp_lengths <- t(apply(frp_parts_length , 1,cumsum)) # 20240531 reach here. The next step is to get into the function.
# 
 
      


# Outputting estimation result for the network 
file.name <- paste0(
  "data/frp_outputs/frp_",
   network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)


frp_r <- saveRDS(file.name)

