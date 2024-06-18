# Note: the purpose of this script is to diagnose networks
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = 0.1/0.4/1


# Packages
suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))
suppressMessages(library(fs))


# Loading data
file.name_in <- 
    paste0("data/netest_outputs/netest_", 
           layer, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
           )
  
est  <- 
  readRDS(file.name_in) 

# Diagnosing layers 
source("R/layers_dx.R")

dx <- 
layers_dx(est_nw = est, 
          layer = layer
          )

# Outputting netdx result of the single layer
file.name_out <- 
    paste0("data/netdx_outputs/dx_", layer, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
           )

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netdx_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

saveRDS(dx,  file = file.name_out)







