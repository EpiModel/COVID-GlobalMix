library(tsna)
library(progressr)
source("./forward_reacheable_path.R")

nd <- readRDS("./nd_school.Rds")
el_cuml <- as_cumulative_edgelist(nd)

node_set <- unique(c(el_cuml$head, el_cuml$tail))
nodes <- sample(node_set, 1e2)

system.time({
  progressr::with_progress(
    el_fwd <- get_forward_reachable(el_cuml, 1, 52, nodes, "auto")
  )
})

progressr::with_progress({
  nodes <- strsplit(names(el_fwd$reached), "_")
  p <- progressr::progressor(length(el_fwd$reached))
  for (i in seq_along(el_fwd$reached)) {
    p()
    node <- as.integer(nodes[[i]][2])
    t_fwd <- tsna::tPath(
      nd, v = node,
      start = 1, end = 52 + 1, # tPath works from [start, end) right exclusive
      direction = "fwd"
    )

    t_fwd_set <- which(t_fwd$tdist < Inf)
    if(!setequal(el_fwd$reached[[i]], t_fwd_set))
      stop("Missmatch on node: ", node)
  }
}
)
