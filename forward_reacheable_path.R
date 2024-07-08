#' Convert a network dynamic object into a cumulative edgelist
netdyn2el_cuml <- function(net) {
  as.data.frame(net) |>
    dplyr::select(start = onset, stop = terminus, head, tail) |>
    dplyr::mutate(stop = stop - 1)
}

#' Convert an object to a `cumulative_edgelist`
#'
#' @return A `cumulative_edgelist` object, a `data.frame` with at least the
#' following columns: `head`, `tail`, `start`, `stop`.
#'
#' @detail
#' The edges are active from time `start` to time `stop` included. If stop is
#' `NA`, the edge was not disolved in the simulation that generated the list.
as_cumulative_edgelist <- function(x) {
  UseMethod("as_cumulative_edgelist")
}

as_cumulative_edgelist.networkDynamic <- function(x) {
  d <- as.data.frame(x)
  d <- d[c("head", "tail", "onset", "terminus")]
  names(d) <- c("head", "tail", "start", "stop")
  d$stop <- d$stop - 1
  class(d) <- c("cumulative_edgelist", class(d))
  return(d)
}

#' Deduplicate a cumulative edgelist by combining overlapping edges
#'
#' @param el A cumulative edgelist with potentially overlapping edges
#'
#' @return A cumulative edgelist with no overlapping edges
dedup_cumulative_edgelist <- function(el) {
  el_n <- el |>
    dplyr::group_by(head, tail) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup()

  e_unique <- el_n |>
    dplyr::filter(n == 1) |>
    dplyr::select(-n)

  e_dup <- el_n |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    dplyr::arrange(head, tail, start, stop)

  e_dedup <- e_dup |>
    dplyr::group_by(head, tail) |>
    dplyr::mutate(
      lstart = dplyr::lag(start),
      lstop = dplyr::lag(stop),
      overlap = !is.na(lstop) & !is.na(lstart) & start <= lstop,
      stop = ifelse(overlap, max(stop, lstop, na.rm = TRUE), stop),
      start = ifelse(overlap, min(start, lstart, na.rm = TRUE), start)
    ) |>
    dplyr::select(-c(lstart, lstop, overlap)) |>
    dplyr::ungroup() |>
    unique()

  dplyr::bind_rows(e_unique, e_dedup)
}

#' @title Calculate the Forward Reachable Path over a Time Series
#'
#' @description This function calculates the Forward Reachable Path (FRP) of all
#'              the nodes in a network over a time series. It is much faster
#'              than iterating \code{tsna::tPath} over all nodes.
#'
#' @param el_cuml a cumulative edgelist object. That is a data.frame with at
#'   least columns: head, tail, start and stop. Start and stop are inclusive.
#' @param from_step the beginning of the time period.
#' @param to_step the end of the time period.
#' @param nodes the subset of nodes to calculate the FRP for. (default = NULL,
#'        all nodes)
#'
#' @return
#' A list of FRP for each of the nodes of interest
#'
#' @section Time and Memory Use:
#' This function may be used to efficiently calculate all FRPs over many time
#' steps. For more limited calculations, see \code{tsna::tPath}. This function
#' takes 3 to 20 minutes on a network of 1e4 nodes over 260 time steps.
#'
#' @section Displaying Progress:
#' This function is using the
#' \href{https://progressr.futureverse.org/articles/progressr-intro.html}{progressr package}
#' to display its progression. Use
#' \code{progressr::with_progress({frp_parts <- get_all_frp(net, from = 1, to = 260)})}
#' to display the progress bar. Or see the
#' \href{https://progressr.futureverse.org/articles/progressr-intro.html}{progressr package}
#' for more information and customization.
#'
#' @section Number of Nodes:
#' This codes does not know the total number of node on the network and assumes
#' that the highest ID recorded correspond to the last node.
#' We can therefore arrive to a situation where there is elements in the output
#' than node in the network if the last N nodes (by ID) are never connected.
#' And therefore are not recorded in the cumulative edgelist.
#' So the FRP for the nodes not present in the output is always 1 (themselves).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate all the FRPs from step 100 to 260
#' from_ts <- 100
#' to_ts <- 260
#'
#' frps <- get_all_frp(el_cuml, from_step = from_ts, to_stop = to_ts)
#'
#' # testing the results against tPath
#' n_max <- 500
#' n <- 0
#' while(n < n_max) {
#'   v_int <- sample(n_nodes, 1)
#'   ts <- sample(n_steps, 1)
#'
#'   # get the FRP using tPath
#'   tp <- tsna::tPath(net, v = v_int,
#'                     start = from_ts, end = from_ts + ts,
#'                     direction = "fwd")
#'   frp_tp <- which(tp$tdist < Inf)
#'
#'   # get the FRP using this function
#'   frp_my <- frps[[v_int]]
#'
#'   if (!setequal(frp_tp, frp_my))
#'     stop("missmatch in node: ", v_int, "; for ts = ", ts)
#'   n <- n + 1
#'   print(n)
#' }
#'
#' }
get_all_frp <- function(el_cuml, from_step, to_step, nodes = NULL) {
  n_nodes <- max(c(el_cuml$head, el_cuml$tail))
  if (is.null(nodes))
    nodes <- seq_len(n_nodes)

  # the initial FRP contains only the vertex itself
  frp_cur <- as.list(nodes)
  names(frp_cur) <- paste0("node_", nodes)

  # Prepare the cumulative edgelist:
  # - set the `stop` time to `Inf` instead of NA
  # - remove the edges that don't exist in the analysis period
  # - set the oldest edges `start` to `to_step`
  #     (QoL to calcuate the `change_times`)

  # nolint start
  el_cuml <- el_cuml |>
    dplyr::mutate(stop = ifelse(is.na(stop), Inf, stop)) |> # current edges never ends
    dplyr::filter(start <= to_step, stop >= from_step) |> # remove edges before and after analysis period
    dplyr::mutate(start = ifelse(start < from_step, from_step, start)) |> # set older edges to start at beginning of analysis
    dplyr::select(start, stop, head, tail)
  # nolint end

  # Only consider steps where new edges occur
  change_times <- sort(unique(el_cuml$start))
  p <- progressr::progressor(length(change_times))

  for (cur_step in change_times) {
    p()

    # nolint start
    #
    # IN EL_CUML: duration is [start, stop] (inclusive)
    # all current edges are needed (not only start). This is because getting
    # connected to a node A means that we are also indirectly connected to its
    # connections, even the ones that started earlier.
    #
    el_cur <- dplyr::filter(el_cuml, start <= cur_step, stop >= cur_step)
    # nolint end

    # PERF: bottleneck is here
    # frp_v is the current frp for vertex v at timestep t - 1
    # we add to it all the nodes that have edges at timestep t with any of the
    # nodes in the FRP
    # the while loop is to include the nodes that are connected to the FRP
    # through a node added this step

    # get subnet works with adjacency list
    adj_list <- get_adj_list(el_cur, n_nodes)
    # at time T, the FRP(T) of a node is the subnet connected to the FRP(T-1)
    frp_cur <- lapply(frp_cur, get_connected_subnet, adj_list = adj_list)
  }
  return(frp_cur)
}

get_frp_lengths <- function(el_cuml, from_step, to_step, nodes = NULL) {
  frps <- get_all_frp(el_cuml, from_step, to_step, nodes)
  vapply(frps, length, numeric(1))
}

#' Returns all the node connected directly or indirectly to a set of nodes
#'
#' @param adj_list The network represented as an adjacency list
#' @param nodes A set of nodes
#'
#' @return A vector of nodes indexes that are connected together with the ones
#'         provided in the `nodes` argument
get_connected_subnet <- function(adj_list, nodes) {
  new <- nodes
  subnet <- nodes
  n_nodes <- length(adj_list)
  while (length(new) > 0 && length(subnet) < n_nodes) {
    new <- unlist(adj_list[new])
    new <- setdiff(new, subnet)
    subnet <- c(subnet, new)
  }
  subnet
}

#' Returns an adjacency list from an edge list
#'
#' @param el An edge list as a data.frame with columns `head` and `tail`
#' @param n_nodes The size number of node in the network
#'
#' @return An adjacency list for the network
#'
#' @detail
#' The adjacency list is a `list` of length `n_nodes`. The entry for each node
#' is a integer vector containing the index of all the nodes connected to it.
#' This layout makes it directly subsetable in O(1) at the expanse of memory
#' usage.
#' To get all connections to the nodes 10 and 15 : `unlist(adj_list[c(10, 15)]`
get_adj_list <- function(el, n_nodes) {
  head <- el$head
  tail <- el$tail

  adj_list <- vector(mode = "list", length = n_nodes)
  for (i in seq_len(nrow(el))) {
    e_head <- head[i]
    e_tail <- tail[i]
    adj_list[[e_head]] <- c(adj_list[[e_head]], e_tail)
    adj_list[[e_tail]] <- c(adj_list[[e_tail]], e_head)
  }
  adj_list
}

get_bkw_frp <- function(el_cuml, from_step, to_step, nodes = NULL) {
  el_cuml$stop <- ifelse(is.na(el_cuml$stop), Inf, el_cuml$stop)
  tmp <- el_cuml$start
  el_cuml$start <- - el_cuml$stop
  el_cuml$stop <- - tmp

  get_all_frp(el_cuml, -to_step, -from_step, nodes)
}
