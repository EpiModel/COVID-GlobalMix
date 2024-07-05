library(dplyr)
source("./forward_reacheable_path.R")

# el_lists <- list(
#   home = readRDS("el_cuml__home.rds"),
#   work = readRDS("el_cuml__work.rds"),
#   school = readRDS("el_cuml__school.rds"),
#   non_home = readRDS("el_cuml__non.rds")
# )

el_cuml <- readRDS("el_cuml__non.rds")
# el_cuml <- readRDS("./el_cuml_tom.rds")

n_nodes <- max(el_cuml$head, el_cuml$tail)
nodes <- sample(n_nodes, 1e2)

options("browser" = "firefox")
profvis::profvis({
  progressr::with_progress(
    x <- get_all_frp( el_cuml, 1, 52, nodes)
  )
})

system.time({
    x <- get_all_frp( el_cuml, 1, 52, nodes)
})

# start <- Sys.time()
# progressr::with_progress(
#  frps_old <- old_get_frp_lengths(
#    el_cuml, 1, 52,
#    nodes
#  )
# )
# print(Sys.time() - start)
#
# start <- Sys.time()
# progressr::with_progress(
#  frps_len <- get_frp_lengths(
#    el_cuml, 1, 52,
#    nodes
#  )
# )
# print(Sys.time() - start)
