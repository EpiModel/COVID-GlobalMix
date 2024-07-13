library(dplyr)
source("./forward_reacheable_path.R")

# el_lists <- list(
#   home = readRDS("el_cuml__home.rds"),
#   work = readRDS("el_cuml__work.rds"),
#   school = readRDS("el_cuml__school.rds"),
#   non_home = readRDS("el_cuml__non.rds")
# )

# el_cuml <- readRDS("el_cuml__school.rds")
# el_cuml <- readRDS("el_cuml__home.rds")
# el_cuml <- readRDS("el_cuml__non.rds")
el_cuml <- readRDS("el_cuml__work.rds")
# el_cuml <- readRDS("./el_cuml_tom.rds")

node_set <- unique(c(el_cuml$head, el_cuml$tail))
nodes <- sample(node_set, 1e3)

# from_step = 1
# to_step = 52

system.time({
  progressr::with_progress(
    x <- get_forward_reachable(el_cuml, 1, 52, nodes, TRUE)
  )
})
system.time({
  progressr::with_progress(
    old <- get_forward_reachable(el_cuml, 1, 52, nodes)
  )
})

for (nme in names(x$reached)) {
  if (!setequal(x$reached[[nme]], old$reached[[nme]])) print(nme)
}

for (nme in setdiff(names(old$reached), names(x$reached))) {
  if (length(old$reached[[nme]]) > 1) print(nme)
}




el_cuml$head <- el_cuml$head + 100L
el_cuml$tail <- el_cuml$tail + 100L

system.time({
  progressr::with_progress(
    x <- get_forward_reachable(el_cuml, 1, 52)
  )
})

system.time({
  progressr::with_progress(
    old <- get_forward_reachable_old(el_cuml, 1, 52)
  )
})



options("browser" = "firefox")
profvis::profvis({
  progressr::with_progress(
    x <- get_forward_reachable_old(el_cuml, 1, 52, nodes)
  )
})

microbenchmark::microbenchmark(
  step = get_forward_reachable_steps(el_cuml, 1, 52, nodes),
  reach = get_forward_reachable_steps(el_cuml, 1, 52, nodes),
  times = 4
)

microbenchmark::microbenchmark(
  raw = get_adj_list(el_cuml, n_nodes),
  old = get_subnet_adj_list(get_adj_list(el_cuml, n_nodes)),
  new = new_get_subnet_adj_list(el_cuml, n_nodes),
  times = 10
)

n_nodes = max(c(el_cuml$head, el_cuml$tail))
adj_list = get_adj_list(el_cuml, n_nodes)
sub_list = get_subnet_adj_list(adj_list)
new_sub = new_get_subnet_adj_list(el_cuml, n_nodes)


for (i in 11555:length(new_sub)) {
  if (!setequal(sub_list[[i]], new_sub[[i]])) {
    print(sub_list[[i]])
    print(new_sub[[i]])
    break
  }
}

# this works - so no info is lost on new_sub
ns2 <- get_subnet_adj_list(new_sub)
for (i in seq_along(ns2)) {
  if (!setequal(sub_list[[i]], ns2[[i]])) print(i)
}
