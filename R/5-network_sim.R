

rm(list = ls())

library(EpiModel)

# Load function for dynamic network simulation
source("R/sim_network.R") 

# Load Data 
est <- readRDS("data/models/netest_7_layers.RData")

## Dynamic network simulation
out <- sim_network(est = est$Rural, nsteps = 3); names(out) <- c("Home", "School", "Work", "Nonhome")

attri_tarstats$attr$rural

# fns <- strsplit(fn, "[.]")[[1]]
# fn.new <- paste(fns[1], "NetSim", fns[3], "rda", sep = ".")
saveRDS(out, file = "data/models/netsim_rural.RData")
