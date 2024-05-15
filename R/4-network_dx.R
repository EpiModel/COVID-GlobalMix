# Note: the purpose of this script is to diagnose networks
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"

library("dplyr")
library("EpiModel")


file.name_in <- 
    paste0("data/netest_outputs/netest_", layer, "__", network,"__", est_apch, ".Rds")
  
est  <- 
  readRDS(file.name_in) 

# Diagnosing layers 
source("R/layers_dx.R")

dx <- 
layers_dx(est_nw = est, 
          layer = layer
          )


# Outputting netdx items of the 8 layers
file.name_out <- 
    paste0("data/netdx_outputs/dx_", layer, "__", network,"__", est_apch, ".Rds")

saveRDS(dx,  file = file.name_out)







