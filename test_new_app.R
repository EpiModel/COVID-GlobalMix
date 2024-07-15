library(dplyr)
source("./forward_reacheable_path.R")

# el_cuml <- readRDS("el_cuml__school.rds")
el_cuml <- readRDS("el_cuml__home.rds")
# el_cuml <- readRDS("el_cuml__non.rds")
# el_cuml <- readRDS("el_cuml__work.rds")
# el_cuml <- readRDS("./el_cuml_tom.rds")

node_set <- unique(c(el_cuml$head, el_cuml$tail))
nodes <- sample(node_set, 1e3)

# from_step = 1
# to_step = 52

system.time({
  progressr::with_progress(
    x <- get_forward_reachable(el_cuml, 1, 52, nodes, "yes")
  )
})
system.time({
  progressr::with_progress(
    old <- get_forward_reachable(el_cuml, 1, 52, nodes, "auto")
  )
})
system.time({
  progressr::with_progress(
    old <- get_forward_reachable(el_cuml, 1, 52, nodes, "no")
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

n_nodes <- max(c(el_cuml$head, el_cuml$tail))
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

for (i in 1:length(new_sub)) {
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

n_nodes <- 16
el_cuml <- tibble(
  tail = c(1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 10, 13, 13),
  head = c(5, 6, 14, 7, 10, 7, 8, 11, 9, 12, 14, 15, 16)
)
el <- el_cuml

adj_list = get_adj_list(el_cuml, n_nodes)
sub_list = get_subnet_adj_list(adj_list)
new_sub = new_get_subnet_adj_list(el_cuml, n_nodes)

adj_list

print(paste(low[i], high[i])); print(paste(low_state, high_state))
p_list(adj_list)
p_list(sub_list)
p_list(new_sub)

p_list <- function(ll) {
  for (j in seq_along(ll))
    writeLines(paste0(j, ": ", paste0(ll[[j]], collapse = ", ")))
}

n_nodes <- max(c(el_cuml$head, el_cuml$tail))
for (t in 1:52) {
  el_cur <- dplyr::filter(el_cuml, start <= t, stop >= t)
  adj_list <- get_adj_list(el_cur, n_nodes) |>
    get_subnet_adj_list()
  comp_list <- new_get_subnet_adj_list(el_cur, n_nodes)
  for (i in seq_along(adj_list)) {
    if (!setequal(adj_list[[i]], comp_list[[i]])) {
      print(paste0("step: ", t, " - node: ", i))
      break
    }
  }
}

# el_cuml <- readRDS("el_cuml__school.rds")
el_cuml <- readRDS("el_cuml__home.rds")
# el_cuml <- readRDS("el_cuml__non.rds")
# el_cuml <- readRDS("el_cuml__work.rds")
# el_cuml <- readRDS("./el_cuml_tom.rds")
n_nodes <- max(c(el_cuml$head, el_cuml$tail))
t <- 2
el_cur <- dplyr::filter(el_cuml, start <= t, stop >= t)
microbenchmark::microbenchmark(
  raw = get_adj_list(el_cur, n_nodes),
  old = get_subnet_adj_list(get_adj_list(el_cur, n_nodes)),
  new = new_get_subnet_adj_list(el_cur, n_nodes),
  times = 10
)

options("browser" = "firefox")
profvis::profvis({
  new_sub <- new_get_subnet_adj_list(el_cur, n_nodes)
})
new_sub <- new_get_subnet_adj_list(el_cur, n_nodes)
vapply(new_sub, length, 0) |> table()

new_sub <- get_subnet_adj_list(get_adj_list(el_cur, n_nodes))
vapply(new_sub, length, 0) |> table()
