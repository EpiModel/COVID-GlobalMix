library(tsna)
source("./forward_reacheable_path.R")
# nd <- readRDS("nd_non.Rds")
# el_cuml <- readRDS("el_cuml__non.rds")

# nd <- readRDS("nd_school.Rds")
# el_cuml <- readRDS("el_cuml__school.rds")

# nd <- readRDS("nd_home.Rds")
# el_cuml <- readRDS("el_cuml__home.rds")

nd <- readRDS("nd_work.Rds")
el_cuml <- readRDS("el_cuml__work.rds")

nnodes <- max(el_cuml$head, el_cuml$tail)

from_step <- 1
to_step <- 52
nodes <- sample(nnodes, 100)
el_tp <- get_all_frp(el_cuml, from_step, to_step, nodes)

# ------------------------------------------------------------------------------

for (i in seq_along(el_tp)) {
  tp <- tsna::tPath(
    nd,
    v = nodes[[i]],
    start = from_step, end = to_step + 1,
    direction = "fwd"
  )
  nd_tp <- which(tp$tdist < Inf)
  if(!setequal(el_tp[[i]], nd_tp)) {
    print(i)
    stop("Missmatch on node: ", names(nodes)[[i]])
  }
}

nodes <- sample(nnodes, 100)
el_tp <- get_bkw_frp(el_cuml, from_step, to_step, nodes)

for (i in seq_along(el_tp)) {
  tp <- tsna::tPath(
    nd,
    v = nodes[[i]],
    start = from_step, end = to_step + 1,
  direction = "bkwd",
  type = "latest.depart"
  )
  nd_tp <- which(tp$tdist < Inf)
  if(!setequal(el_tp[[i]], nd_tp)) {
    print(i)
    stop("Missmatch on node: ", names(nodes)[[i]])
  }
}

nodes <- sample(nnodes, 5)
microbenchmark::microbenchmark(
  frp = get_all_frp(el_cuml, from_step, to_step, nodes),
  tPath = {
    for (i in seq_along(nodes)) {
      node = nodes[[i]]
      tsna::tPath(nd, v = node, start = from_step, end = to_step + 1, direction = "fwd")
    }
  }
)


# FRP example ------------------------------------------------------------------

# load a network dynamic object
nd <- readRDS("nd_obj.Rds")
el_cuml <- as_cumulative_edgelist(nd) # convert it to a cumulative edgelist

# sample 100 node indexes
nnodes <- max(el_cuml$head, el_cuml$tail)
nodes <- sample(nnodes, 100)


# `get_all_frp` uses steps [from_step, to_step] inclusive
el_frp <- get_all_frp(el_cuml, 1, 52, nodes)

# check if the results are consistent with `tsna::tPath`
for (i in seq_along(el_tp)) {
  t_frp <- tsna::tPath(nd, v = nodes[[i]],
                       start = 1, end = 52 + 1, # tPath works from [start, end) right exclusive
                       direction = "fwd")
  t_frp_set <- which(t_frp$tdist < Inf)
  if(!setequal(el_frp[[i]], t_frp_set))
    stop("Missmatch on node: ", names(nodes)[[i]])
}

