source("./forward_reacheable_path.R")

get_state <- function(adj_list, node) {
  if (length(adj_list[[node]]) == 0) {
    return("empty")
  } else if (adj_list[[node]][1] > node) {
    return("subnet")
  } else {
    return("pointer")
  }
}

follow_pointer <- function(adj_list, node) {
  adj_list[[node]][1]
}
# assume edges are unique
# assume edges are tail < head
ord_new_get_subnet_adj_list <- function(el, n_nodes) {
  ordering <- order(el$tail, el$head)
  low <- el$tail[ordering]
  high <- el$head[ordering]

  adj_list <- vector(mode = "list", length = n_nodes)
  i <- 1
  while (i <= nrow(el)) {
    low_state <- get_state(adj_list, low[i])
    high_state <- get_state(adj_list, high[i])

    if (low_state == "pointer") {
      low[i] <- follow_pointer(adj_list, low[i])
      low_state <- "subnet"
    }

    if (high_state == "pointer") {
      high[i] <- follow_pointer(adj_list, high[i])
      high_state <- "subnet"
    }

    if (high[i] == low[i]) {
      i <- i + 1
      next
    } else if (high[i] < low[i]) {
      tmp <- high[i]
      high[i] <- low[i]
      low[i] <- tmp
      tmp <- high_state
      high_state <- low_state
      low_state <- tmp
    }

    if (low_state == "empty" && high_state == "empty") {
      adj_list[[low[i]]] <- high[i]
      adj_list[[high[i]]] <- low[i]
    } else if (low_state == "empty" && high_state == "subnet") {
      high_sub <- adj_list[[high[i]]]
      adj_list[[low[i]]] <- c(high[i], high_sub)
      adj_list[[high[i]]] <- low[i]
      adj_list[high_sub] <- low[i]
    } else if (low_state == "subnet" && high_state == "empty") {
      adj_list[[low[i]]] <- c(adj_list[[low[i]]], high[i])
      adj_list[[high[i]]] <- low[i]
    } else if (low_state == "subnet" && high_state == "subnet") {
      low_sub <- adj_list[[low[i]]]
      high_sub <- adj_list[[high[i]]]
      adj_list[[low[i]]] <- c(high_sub, high[i], low_sub)
      adj_list[[high[i]]] <- low[i]
      adj_list[high_sub] <- low[i]
    }

    i <- i + 1
  }

  adj_list
}

# assume edges are unique
# assume edges are tail < head
new_get_subnet_adj_list <- function(el, n_nodes) {
  ordering <- order(el$tail, el$head)
  low <- el$tail[ordering]
  high <- el$head[ordering]
  #
  # low <- el$tail
  # high <- el$head

  subnets <- vector(mode = "list", length = n_nodes)
  # states-> 0: empty, -1: subnet, n: pointer to n
  states <- numeric(n_nodes)

  i <- 1
  while (i <= nrow(el)) {
    low_state <- states[low[i]]
    high_state <- states[high[i]]

    if (low_state > 0) { # pointer
      low[i] <- states[low[i]]
      low_state <- -1
    }

    if (high_state > 0) {
      high[i] <- states[high[i]]
      high_state <- -1
    }

    if (high[i] == low[i]) {
      i <- i + 1
      next
    } else if (high[i] < low[i]) {
      tmp <- high[i]
      high[i] <- low[i]
      low[i] <- tmp
      tmp <- high_state
      high_state <- low_state
      low_state <- tmp
    }

    if (low_state == 0 && high_state == 0) { # both empty
      subnets[[low[i]]] <- high[i]
      states[low[i]] <- -1      # low is subnet
      states[high[i]] <- low[i] # hight points to low
    } else if (low_state == 0 && high_state == -1) { # empty, subnet
      new_sub <- c(subnets[[high[i]]], high[i])
      subnets[[low[i]]] <- new_sub
      subnets[[high[i]]] <- list(NULL)
      states[new_sub] <- low[i]
      states[low[i]] <- -1
    } else if (low_state == -1 && high_state == 0) { # subnet empty
      subnets[[low[i]]] <- c(subnets[[low[i]]], high[i])
      states[high[i]] <- low[i]
    } else if (low_state == -1 && high_state == -1) { # both subnet
      low_sub <- subnets[[low[i]]]
      high_sub <- c(subnets[[high[i]]], high[i])
      subnets[[low[i]]] <- c(low_sub, high_sub)
      states[high_sub] <- low[i]
    }

    i <- i + 1
  }

  for (i in seq_along(states)) { # put the links back into the subnets
    if (states[i] > 0) {
      subnets[[i]] <- states[i]
    }
  }

  subnets
}

library(dplyr)
# el_cuml <- readRDS("el_cuml__school.rds")
# el_cuml <- readRDS("el_cuml__home.rds")
# el_cuml <- readRDS("el_cuml__non.rds")
el_cuml <- readRDS("el_cuml__work.rds")
# el_cuml <- readRDS("./el_cuml_tom.rds")
nrow(el_cuml)

n_nodes <- max(c(el_cuml$head, el_cuml$tail))
t <- 2
el_cur <- dplyr::filter(el_cuml, start <= t, stop >= t)
microbenchmark::microbenchmark(
  old = get_subnet_adj_list(get_adj_list(el_cur, n_nodes)),
  ord = ord_new_get_subnet_adj_list(el_cur, n_nodes),
  new = new_get_subnet_adj_list(el_cur, n_nodes),
  times = 10
)

new_sub <- new_get_subnet_adj_list(el_cur, n_nodes)
vapply(new_sub, length, 0) |> table()

n_nodes <- max(c(el_cuml$head, el_cuml$tail))
for (t in 1:52) {
  el_cur <- dplyr::filter(el_cuml, start <= t, stop >= t)
  adj_list <- new_get_subnet_adj_list(el_cur, n_nodes)
  comp_list <- ord_new_get_subnet_adj_list(el_cur, n_nodes)
  for (i in seq_along(adj_list)) {
    if (!setequal(adj_list[[i]], comp_list[[i]])) {
      print(paste0("step: ", t, " - node: ", i))
      break
    }
  }
}

n_nodes <- 4e4
x <- numeric(n_nodes)
y <- vector(mode = "list", length = n_nodes)
ns <- sample(n_nodes, 10000)
microbenchmark::microbenchmark(
  x[ns] <- 12,
  y[ns] <- 12
)
