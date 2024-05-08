# Note: the purpose of this script is to diagnose networks

library("dplyr")
library("EpiModel")


file.name_in <- 
    paste0("data/netest_outputs/netest_8_layers_", layer, "__", network,"__", est_apch, ".Rds")
  
est  <- 
  readRDS(file.name_in) 

# Diagnosing layers 
source("R/layers_dx.R")

dx <- 
layers_dx(est_nws = est, 
          nw = network, # can be "Rural" / "Urban"
          layer = layer
          )


# Outputting netdx items of the 8 layers
file.name_out <- 
    paste0("data/netdx_outputs/dx_", layer, "__", network,"__", est_apch, ".Rds")

saveRDS(dx,  file = file.name_out)







