# Note: the purpose of this script is to calculate the forward-reachable path (FRP) under different scenarios. 
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Rural"/"Urban"
# est_apch = "sto_apoxy"/"mcmle"
# layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
# percent_target_pop = 0.1/0.4/1
# nodes - the number of nodes with edges whose FRPs are calculated, the default setting is NULL, that FRPs for all nodes are calculated


# Packages
suppressMessages(library(tsna))
suppressMessages(library(EpiModel))
suppressMessages(library(doParallel))
suppressMessages(library(fs))
suppressMessages(library(progressr))


# Load functions
source("R/reachable.R")


# Loading data



# file name of the outputted files

file.name_out <- paste0(
  "data/frp_outputs/frp_length_",
  layer, "__", network,"__", percent_target_pop, "__", paste0(as.character(nodes)), ".Rds"
)


# Loading edgelist
if (layer == "Home") {
  # manually convert to cumulative edgelist
  el_cuml <- 
    readRDS(
      paste0(
        "data/netest_outputs/deterministic_",
        layer, "__", network,"__",  percent_target_pop, ".Rds"
      )
    ) %>% data.frame() %>%  rename(tail=.tail, head=.head ) %>% select(head, tail) %>% mutate(start=1, stop=2)
  
} else if (layer %in%  c("School", "Work", "Nonhome")) {
  el_cuml <- readRDS(  paste0("data/netsim_outputs/el_cuml__", layer, "__", 
                              network,"__", est_apch,"__", percent_target_pop, ".Rds")
                     )
} else if (layer == "All") {
  # home
  el_cuml_home <- 
    readRDS(
      paste0(
        "data/netest_outputs/deterministic_",
        layer, "__", network,"__",  percent_target_pop, ".Rds"
      )
    ) %>% data.frame() %>%  rename(tail=.tail, head=.head ) %>% select(head, tail) %>% mutate(start=1, stop=2)
  # school, work, nonhome
  file.name_in <- 
    paste0("data/netsim_outputs/el_cuml__", c("School", "Work", "Nonhome"), "__", 
           network,"__", est_apch,"__", percent_target_pop, ".Rds")
  el_cuml_school <- readRDS(file.name_in[1])
  el_cuml_work <- readRDS(file.name_in[2])
  el_cuml_nonhome <- readRDS(file.name_in[3])
  
  # combining edgelists of different layers
  el_all <- dplyr::bind_rows(el_cuml_home, el_cuml_school, el_cuml_work, el_cuml_nonhome)
  
  # deduplicating edges
  el_cuml <- dedup_cumulative_edgelist(el = el_all)
}


# the identifiers of nodes whose FRPs would be calculated
if (!is.null(nodes)
    ) {
node_set <- unique(c(el_cuml$head, el_cuml$tail))
nodes <- sample(node_set, nodes)
}


# Calculating FRP length for each node and time step
frp_lengths <- 
progressr::with_progress(
  get_forward_reachable(
  el_cuml, 
  from_step=1, 
  to_step=365,
  nodes = nodes
  )
)


# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Outputting FRP result
saveRDS(frp_lengths, file = file.name_out)

