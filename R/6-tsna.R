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


# Loading data
sim <- readRDS(paste0("data/netsim_outputs/sim_", 
                      network,"__", est_apch,"__", percent_target_pop, ".Rds"))

if (layer == "Home") {
  sim <- sim[["Home"]]
} else if (layer == "School") {
  sim <- sim[["School"]]
} else if (layer == "Work") {
  sim <- sim[["Work"]]
} else if (layer == "Nonhome"){
  sim <- sim[["Nonhome"]]
} else if (layer == "All") {
  sim_home <- sim[["Home"]]
  sim_school <- sim[["School"]]
  sim_work <- sim[["Work"]]
  sim_nonhome <- sim[["Nonhome"]]
  
  
  sim_all <- sim_home
  # Exporting the edge dynamics of layers other than home as dataframes
  ## Note: "onset" and "terminus" mean times that an edge starts and ends
  ## "tail" and "head" meaning the tail and head of the edge, by node/vertex's identifier
  sim_school_df <- as.data.frame.networkDynamic(sim_school)
  sim_work_df <- as.data.frame.networkDynamic(sim_work)
  sim_nonhome_df <- as.data.frame.networkDynamic(sim_nonhome)

  # For each node, add edges of the School, Work, and Nonhome layers to the Home layer, and then return it as a networkDynamic item containing edges at all layers for each node
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



source("R/get_all_frp.R")

 
with_progress(
{frp <-get_all_frp(net = sim, to =365)}  
)

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Outputting FRP result
file.name <- paste0(
  "data/frp_outputs/frp_",
  network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

saveRDS(frp, file = file.name)

