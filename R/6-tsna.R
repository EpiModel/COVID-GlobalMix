# Note: the purpose of this script is to calculate the forward-reachable path (FRP) under different scenarios. 
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Rural"/"Urban"
# layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
# percent_target_pop = 0.1/0.4/1


# Packages
suppressMessages(library(tsna))
suppressMessages(library(EpiModel))
suppressMessages(library(doParallel))
suppressMessages(library(fs))
suppressMessages(library(progressr))
suppressMessages(library(dplyr))

# Loading data
## file name of the outputted files
file.name_out <- paste0(
  "data/frp_outputs/frp_length_",
  layer, "__", network,"__", percent_target_pop, ".Rds"
)

source("R/reachable.R")

layers <- c("Home", "School", "Work", "Nonhome")

# Loading edgelist
 if (layer %in%  layers) {
  
  el_cuml <- readRDS(  paste0("data/netsim_outputs/el_cuml__", layer, "__", 
                              network,"__",  percent_target_pop, ".Rds")
                     )
} else if (layer == "All") {

  # reading in individual layers
  file.name_in <- 
    paste0("data/netsim_outputs/el_cuml__", layers, "__", 
           network,"__",  percent_target_pop, ".Rds")
  el_cuml_home <- readRDS(file.name_in[1])
  el_cuml_school <- readRDS(file.name_in[2])
  el_cuml_work <- readRDS(file.name_in[3])
  el_cuml_nonhome <- readRDS(file.name_in[4])
  
  # combining edgelists of different layers
  el_all <- el_cuml <- list()
  
  for (i in 1:100) {
    el_all[[i]] <- dplyr::bind_rows(el_cuml_home[[i]], el_cuml_school[[i]], el_cuml_work[[i]], el_cuml_nonhome[[i]])
    # deduplicating edges
    el_cuml[[i]] <- dedup_cumulative_edgelist(el = el_all[[i]])
  }
  
  

}



# Calculating FRP length for each node and time step for all simulation iterations
frp_lengths <- list()

for (i in 1:100) {
frp_lengths[[i]] <- 
progressr::with_progress(
  get_forward_reachable(
  el_cuml[[i]], 
  from_step=1, 
  to_step=365,
  nodes = NULL # the number of nodes with edges whose FRPs are calculated, the default setting is NULL, which means FRPs for all nodes are calculated
  )$lengths #outputting the FRP length dataframe
)
}

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Outputting FRP result
saveRDS(frp_lengths, file = file.name_out)

