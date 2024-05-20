# Note: the purpose of this script is to conduct temporal social network analysis
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# layer = "Home"/"School"/"Work"/"Nonhome"/"all", where "all" means all 4 layers

suppressMessages(library("tsna"))
suppressMessages(library("EpiModel"))
suppressMessages(library("doParallel"))

network <- Sys.getenv("NETWORK")

file.name_in <- paste0("data/netsim_outputs/sim_", "__", network,"__", est_apch, ".Rds")

sim <- readRDS(file.name_in)

layer <- Sys.getenv("LAYER")
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
                  )

tp[["tdist"]] %>% summary() # the earliest temporal distance is 0/Inf
tp[["previous"]] # previous vertx long the FRP
tp[["gsteps"]] %>% hist() # For urban network, the most common step to vertex 1 is ~40
 
plotPaths(sim,
          tp,
          label.cex=0.1)



## The following is Emili's script for calculating FRP of the whole population
# simset <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")
#                      )
# 
# batchSize <- 25
# v <- ((batchSize*simset) - (batchSize - 1)):(batchSize*simset)
# 
# int <- as.numeric(Sys.getenv("INT"))
# ts <- seq(1, 260, int)
# 
# f <- function(layer, v, ts) {
#   m <- array(NA, dim = c(length(ts), length(v), 5))
#   for (jj in 1:length(v)) {
#     for (ii in 1:length(ts)) {
#       tp <- tsna::tPath(sim, v = v[jj], start = 1, end = ts[ii], direction = "fwd")
#       # forward reachable path
#       m[ii, jj, 1] <- sum(tp$tdist < Inf)
#       # median temporal distance
#       m[ii, jj, 2] <- median(tp$tdist[tp$tdist < Inf])
#       # median geodesic steps
#       m[ii, jj, 3] <- median(tp$gsteps[tp$gsteps < Inf])
#       # cross-sectional degree
#       m[ii, jj, 4] <- EpiModel::get_degree(network.collapse(sim, at = ts[ii]))[v[jj]]
#       # cumulative degree
#       m[ii, jj, 5] <- EpiModel::get_degree(network.collapse(sim, onset = 1, terminus = ts[[ii]]))[v[jj]]
#       # betweenness centrality
#       # m[ii, jj, 6] <- sna::betweenness(network.collapse(sim, at = ts[ii]), nodes = v[jj])
#     }
#   }
#   return(m)
# }
# 
# registerDoParallel(parallel::detectCores())
# out <- foreach(vv = 1:length(v)) %dopar% {
#   f(sim, v[vv], ts)
# }
# df <- do.call("cbind", out)
# 
# fn <- paste(network, layer, int, stringr::str_pad(simset, 3, pad = "0"), "rda", sep = ".")
# save(df, file = paste0("data/", fn))