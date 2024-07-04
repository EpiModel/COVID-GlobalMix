# Note: the purpose of this script is to conduct temporal social network analysis. This file contains the note taken from the meeting with Adrien on 20240606
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Rural"/"Urban"/
# est_apch = "sto_apoxy"/"mcmle"/
# layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
# percent_target_pop = 0.1/0.4/1


# Packages
suppressMessages(library(tsna))
suppressMessages(library(EpiModel))
suppressMessages(library(doParallel))
suppressMessages(library(fs))
suppressMessages(library(progressr))

network <- c("Rural", "Urban")[1]
est_apch <- c("sto_apoxy", "mcmle")[2]
layer <- c("All", "Home", "School", "Work", "Nonhome")[2]
percent_target_pop <- 0.4

# Loading data
sim <- readRDS(paste0(
  "data/netsim_outputs/sim_", network, "__", est_apch, "__",
  percent_target_pop, ".Rds"))

library(dplyr)
source("./forward_reacheable_path.R")
el_cuml <- netdyn2el_cuml(sim$Nonhome)
saveRDS(el_cuml, "el_cuml__non")

el_lists <- list(
  home = readRDS("el_cuml__home.rds"),
  work = readRDS("el_cuml__work.rds"),
  school = readRDS("el_cuml__school.rds"),
  non_home = readRDS("el_cuml__non.rds")
)

el_all <- dplyr::bind_rows(el_lists)

start <- Sys.time()
progressr::with_progress(
 frp_lengths <- get_frp_lengths(
   el_lists$non_home, 1, 365,
   nodes = sample(47e3, 1e2)
 )
)
print(Sys.time() - start)
frp_lengths

start <- Sys.time()
progressr::with_progress(
  frp_parts <- get_all_frp(el_lists$work, 1, 365, nodes = sample(4e4, 1e2))
)
print(Sys.time() - start)


el_cuml <- dedup_el_cuml(el_all)
