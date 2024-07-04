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
nodes <- sample(n_nodes, 1e3)

# start <- Sys.time()
# progressr::with_progress(
#  frps <- get_all_frp(
#    el_cuml, 1, 260,
#    nodes
#  )
# )
# print(Sys.time() - start)

start <- Sys.time()
progressr::with_progress(
 frps_blk <- get_all_frp_block(
   el_cuml, 1, 26,
   nodes
 )
)
print(Sys.time() - start)




start <- Sys.time()
progressr::with_progress(
 frp_lengths_sub <- get_frp_lengths_sub(
   el_cuml, 1, 260,
   nodes
 )
)
frp_lengths_sub
print(Sys.time() - start)

for (i in seq_along(frp_lengths)) {
  if (frp_lengths[i] != frp_lengths_sub[i]) {
    print(i)
  }
}

cur_el

new_get_subnets_el <- function(connected, el) {
  subnets <- list()
  con_el <- get_connected(el, length(connected))
  j <- 1
  for (i in seq_along(con_el)) {
    if (length(con_el[[i]]) != 0) {
      subnets[[j]] <- get_subnet(connected, con_el[[i]])
      con_el[ subnets[[j]] ] <- list(NULL)
      j <- j + 1
    }
  }
  subnets
}

# get the network components involving some edges
get_subnets_el <- function(connected, el) {
  subnets <- list()
  i <- 1
  while (nrow(el) > 0) {
    subnets[[i]] <- get_subnet(connected, el$head[[1]]) # by def: head <-> tail
    # TODO: speed up with a `connected` approach?
    el <- dplyr::filter(el, !head %in% subnets[[i]])
    i <- i + 1
  }
  subnets
}

connected <- get_connected(full_el, n_nodes)
microbenchmark::microbenchmark(
  new = new_get_subnets_el(connected, cur_el),
  old = get_subnets_el(connected, cur_el)
)
