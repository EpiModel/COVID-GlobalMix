# Note: the purpose of this script is to conduct temporal social network analysis


suppressMessages(library("tsna"))
suppressMessages(library("EpiModel"))
suppressMessages(library("doParallel"))

network <- Sys.getenv("NETWORK")

file.name_in <- paste0("data/netsim_outputs/sim_", "__", network,"__", est_apch, ".Rds")

sim <- readRDS("data/netsim_outputs/sim___Rural__sto_apoxy.Rds")

layer <- Sys.getenv("LAYER")
if (layer == "Home") {
  sim <- sim[["Home"]]
} else if (layer == "School") {
  sim <- sim[["School"]]
} else if (layer == "Work") {
  sim <- sim[["Work"]]
} else if (layer == "Nonhome"){
  sim <- sim[["Nonhome"]]
} else if (layer == "all") {
  sim_home <- sim[["Home"]]
  sim_school <- sim[["School"]]
  sim_work <- sim[["Work"]]
  sim_nonhome <- sim[["Nonhome"]]
  
  # check why the following is done
  sim_all <- sim_home
  sim_school_df <- as.data.frame(sim_school)
  sim_work_df <- as.data.frame(sim_work)
  sim_nonhome_df <- as.data.frame(sim_nonhome)

  
  sim_all <- add.edges.active(sim_all, tail = sim_school_df[["tail"]], head = sim_school_df[["head"]],
                              onset = sim_school_df[["onset"]], terminus = sim_school_df[["terminus"]]
                              )
  
  sim_all <- add.edges.active(sim_all, tail = sim_work_df[["tail"]], head = sim_work_df[["head"]],
                              onset = sim_work_df[["onset"]], terminus = sim_work_df[["terminus"]]
                              )
  
  sim_all <- add.edges.active(sim_all, tail = sim_nonhome_df[["tail"]], head = sim_nonhome_df[["head"]],
                              onset = sim_nonhome_df[["onset"]], terminus = sim_nonhome_df[["terminus"]]
  )
  
  sim <- sim_all
}

simset <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

batchSize <- 25
v <- ((batchSize*simset) - (batchSize - 1)):(batchSize*simset)

int <- as.numeric(Sys.getenv("INT"))
ts <- seq(1, 260, int)

f <- function(layer, v, ts) {
  m <- array(NA, dim = c(length(ts), length(v), 5))
  for (jj in 1:length(v)) {
    for (ii in 1:length(ts)) {
      tp <- tsna::tPath(sim, v = v[jj], start = 1, end = ts[ii], direction = "fwd")
      # forward reachable path
      m[ii, jj, 1] <- sum(tp$tdist < Inf)
      # median temporal distance
      m[ii, jj, 2] <- median(tp$tdist[tp$tdist < Inf])
      # median geodesic steps
      m[ii, jj, 3] <- median(tp$gsteps[tp$gsteps < Inf])
      # cross-sectional degree
      m[ii, jj, 4] <- EpiModel::get_degree(network.collapse(sim, at = ts[ii]))[v[jj]]
      # cumulative degree
      m[ii, jj, 5] <- EpiModel::get_degree(network.collapse(sim, onset = 1, terminus = ts[[ii]]))[v[jj]]
      # betweenness centrality
      # m[ii, jj, 6] <- sna::betweenness(network.collapse(sim, at = ts[ii]), nodes = v[jj])
    }
  }
  return(m)
}

registerDoParallel(parallel::detectCores())
out <- foreach(vv = 1:length(v)) %dopar% {
  f(sim, v[vv], ts)
}
df <- do.call("cbind", out)

fn <- paste(network, layer, int, stringr::str_pad(simset, 3, pad = "0"), "rda", sep = ".")
save(df, file = paste0("data/", fn))