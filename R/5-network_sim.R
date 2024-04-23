

library(dplyr)
library(EpiModel)

# Load function for dynamic network simulation
source("R/sim_network.R") 

# Load Data 
layers <- c("Home", "School", "Work", "Nonhome")
networks <- c("Rural", "Urban")
est_apch <- "sto_apoxy"

file.name <- 
  c(
    paste0("data/netest_outputs/netest_8_layers_", layers, "__", networks[1],"__", est_apch, ".Rds"),
    paste0("data/netest_outputs/netest_8_layers_", layers, "__", networks[2],"__", est_apch, ".Rds")
  )

ests <- list()

ests$Urban$Home <- 
  readRDS(file.name[1]) 
ests$Urban$School <- 
  readRDS(file.name[2]) 
ests$Urban$Work <- 
  readRDS(file.name[3]) 
ests$Urban$Nonhome <- 
  readRDS(file.name[4]) 
ests$Rural$Home <-  
  readRDS(file.name[5]) 
ests$Rural$School <-
  readRDS(file.name[6]) 
ests$Rural$Work <- 
  readRDS(file.name[7]) 
ests$Rural$Nonhome <- 
  readRDS(file.name[8]) 

# nodal attribute for the sin
initial_attr <- readRDS("data/network_params/node_attribute_target_stats__0.1.Rds")
initial_attr$attr # contact status is binary


## Dynamic network simulation
ests$Urban$Home
out <- sim_network(est = ests$Rural , nsteps = 1); names(out) <- c("Home", "School", "Work", "Nonhome")

out[[1]] %>% summary()


# fns <- strsplit(fn, "[.]")[[1]]
# fn.new <- paste(fns[1], "NetSim", fns[3], "rda", sep = ".")
saveRDS(out, file = "data/models/netsim_rural.RData")
