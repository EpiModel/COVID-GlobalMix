# Note: the purpose of this script is to calculate the forward-reachable path
# (FRP) under different scenarios.
# The following are arguments to be passed from the workflow to the HPC job,
# so not defined in this file:
#   network = "Rural"/"Urban"
#   layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, which: "ALL" = 4 layers
#   percent_target_pop = 0.1/0.4/1
#   n_reps = 100, number of simulation to run
#   n_cores, number of HPC core to use

# Packages
library(tsna)
library(EpiModel)
library(fs)
library(dplyr)

source("R/reachable.R")

network <- "Rural"
est_apch <- "mcmle"
layer <- "All"
percent_target_pop <- 1

layers <- c("Home", "School", "Work", "Nonhome")

# Loading data
## file name of the outputted files
file.name_out <- paste0(
  "data/frp_outputs/frp_length_",
  layer, "__",
  network,"__",
  percent_target_pop, ".Rds"
)

i <- 1

els_path <- paste0("data/netsim_outputs/outputs_by_reps/netsim_outputs__", i, ".rds")
els <- readRDS(els_path)

el_cuml <- dedup_cumulative_edgelist(dplyr::bind_rows(els))

from_step = 1
to_step = 365
nodes = NULL
dense_optim = "auto"

el_cuml <- el_cuml |>
  dplyr::mutate(stop = ifelse(is.na(stop), Inf, stop)) |> # current edges never ends
  dplyr::filter(start <= to_step, stop >= from_step) |> # remove edges before and after analysis period
  dplyr::mutate(start = ifelse(start < from_step, from_step, start)) |> # set older edges to start at beginning of analysis
  dplyr::select(start, stop, head, tail)

orig_indexes <- sort(unique(c(el_cuml$head, el_cuml$tail)))
el_cuml$head <- match(el_cuml$head, orig_indexes)
el_cuml$tail <- match(el_cuml$tail, orig_indexes)

if (is.null(nodes)) {
  nodes <- seq_along(orig_indexes)
} else {
  nodes <- match(nodes, orig_indexes)
  nodes <- nodes[!is.na(nodes)]
}

# Only consider steps where new edges occur
change_times <- sort(unique(el_cuml$start))
n_steps <- to_step - from_step + 1

reach_lengths <- matrix(0, ncol = n_steps + 1, nrow = length(orig_indexes))
reach_lengths[, 1] <- 1
for (cur_step in change_times[1:200]) {
  print(paste0("cur_step: ", cur_step))
  el_cur <- dplyr::filter(el_cuml, start <= cur_step, stop >= 1)

  start <- Sys.time()
  adj_list <- get_adj_list(el_cur, length(orig_indexes))
  subnets <- get_all_subnets(adj_list)
  print(Sys.time() - start)

  start <- Sys.time()
  subs <- get_sub_list(el_cur, length(orig_indexes))
  print(Sys.time() - start)

  reach_lengths[, cur_step + 1] <- subnets$lengths[subnets$ids]
}



el_cuml_sav <- el_cuml
# considere persistent edges as single nodes
# udpate edgelist accordingly
# remove duplicate
# calc FRP
# expand FRP
el_cuml <- el_cuml_sav
el_cur <- dplyr::filter(el_cuml, start == from_step, stop >= to_step)
adj_list <- get_adj_list(el_cur, length(orig_indexes))
subnets <- get_all_subnets(adj_list)
multi_subnets <- which(subnets$lengths > 1)
multi_sublens <- subnets$lengths[multi_subnets]

# remove these edges from the cuml.
# They are not relevant anymore as these subnet are considered a single node
el_cuml <- dplyr::filter(el_cuml, !(start == from_step & stop >= to_step))
# replace nodes by there subnet counterpart
el_cuml$head <- subnets$ids[el_cuml$head]
el_cuml$tail <- subnets$ids[el_cuml$tail]

el_cuml <- dedup_cumulative_edgelist(el_cuml)

# run classic FRP then de-dedup


reach_lengths <- t(apply(reach_lengths, 1, cumsum))
reach_cur <- lapply(reach_cur, \(x) orig_indexes[x])

names(reach_cur) <- paste0("node_", orig_indexes[nodes])
rownames(reach_lengths) <- names(reach_cur)
colnames(reach_lengths) <- paste0("step_", (from_step - 1):to_step)



get_subnet_adj_list <- function(adj_list) {
  for (i in seq_along(adj_list)) {
    # if first elt connected to before himself, already in a subnet
    if (length(adj_list[[i]]) == 0 || adj_list[[i]][1] < i)
      next
    # get current subnet of i
    subnet <- get_connected_nodes(adj_list, i)
    adj_list[[i]] <- subnet
    # all members of subnet point to i
    adj_list[subnet] <- i
  }
  adj_list
}

get_connected_nodes <- function(adj_list, nodes) {
  n_nodes <- length(adj_list)
  new_connections <- numeric(0)
  subnet <- nodes
  while (length(nodes) > 0 && length(subnet) < n_nodes) {
    nodes <- unlist(adj_list[nodes])
    nodes <- setdiff(nodes, subnet)
    subnet <- c(subnet, nodes)
    new_connections <- c(new_connections, nodes)
  }
  new_connections
}

get_subnet <- function(adj_list, nodes) {
  n_nodes <- length(adj_list)
  subnet <- nodes
  while (length(nodes) > 0 && length(subnet) < n_nodes) {
    nodes <- unlist(adj_list[nodes])
    nodes <- setdiff(nodes, subnet)
    subnet <- c(subnet, nodes)
  }
  subnet
}

get_all_subnets <- function(adj_list) {
  subnets_ids <- rep(0, length(adj_list))
  subnets_lengths <- c()
  current_subnet <- 1
  for (i in seq_along(subnets_ids)) {
    if (subnets_ids[i] > 0) next
    new_subnet <- get_subnet(adj_list, i)
    subnets_lengths <- c(subnets_lengths, length(new_subnet))
    subnets_ids[new_subnet] <- current_subnet
    current_subnet <- current_subnet + 1
  }
  list(
    ids = subnets_ids,
    lengths = subnets_lengths
  )
}
