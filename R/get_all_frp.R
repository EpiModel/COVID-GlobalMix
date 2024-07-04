

get_all_frp <- function(net, from_step=1, to_step) {

  # Function to convert a network dynamic object into a cumulative edgelist
  netdyn2el_cuml <- function(net) {
    as.data.frame(net) |>
      dplyr::select(start = onset, stop = terminus, head, tail) |>
      dplyr::mutate(stop = stop - 1)
  }

  el_cuml <- netdyn2el_cuml(net = net)

  n_steps <- to_step - from_step + 1
  n_nodes <- max(c(el_cuml$head, el_cuml$tail))

  # nolint start
  df_net <- el_cuml |>
    dplyr::mutate(stop = ifelse(is.na(stop), Inf, stop)) |> # current edges never ends
    dplyr::filter(start <= to_step, stop >= from_step) |> # remove edges before and after analysis period
    dplyr::select(start, stop, head, tail)

  # nolint end
  # the initial FRP contains only the vertex itself
  frp_cur <- as.list(seq_len(n_nodes))
  frp_parts <- matrix(list(numeric(0)), ncol = n_steps + 1, nrow = n_nodes)
  frp_parts[, 1] <- frp_cur

  p <- progressr::progressor(n_steps)
  for (t in seq_len(n_steps)) {
    p()
    cur_step <- t + from_step - 1

    # creation of a `connection` list of vectors
    # for each vertex 1:n_nodes we get a vector of the vertices it connects to
    connected <- vector(mode = "list", length = n_nodes)
    # nolint start
    # IN EL_CUML: duration is [start, stop] (inclusive)
    # all current edges are needed (not only start). This is because we use the
    # connection to each node to calc the FRP to the other ones. Only start
    # would be enough for single FRP though
    el_t <- dplyr::filter(df_net, start <= cur_step, stop >= cur_step)
    # nolint end
    for (i in seq_len(nrow(el_t))) {
      e_head <- el_t$head[i]
      e_tail <- el_t$tail[i]
      connected[[e_head]] <- c(connected[[e_head]], e_tail)
      connected[[e_tail]] <- c(connected[[e_tail]], e_head)
    }

    # PERF: bottleneck is here
    # frp_v is the current frp for vertex v at timestep t - 1
    # we add to it all the nodes that have edges at timestep t with any of the
    # nodes in the FRP
    # the while loop is to include the nodes that are connected to the FRP through
    # a node added this step

    # use hashmaps? rust?
    frp_new <- lapply(
      frp_cur,
      function(frp_v) {
        only_new <- numeric(0)
        new <- frp_v
        while (length(new) > 0 & length(frp_v) < n_nodes) {
          new <- unlist(connected[new])
          new <- setdiff(new, frp_v)
          frp_v <- c(frp_v, new)
          only_new <- c(only_new, new)
        }
        only_new
      }
    )

    # could speed up here with "true arrays" (growable, pre-alloc)
    frp_cur <- Map(c, frp_cur, frp_new)
    frp_parts[, t + 1] <- frp_new
  }
  return(frp_parts)
}

