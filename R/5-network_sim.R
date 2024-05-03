# Note: the purpose of this script is to simulate networks

library(dplyr)
library(EpiModel)

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

# nodal attribute
initial_attr <- readRDS("data/network_params/node_attribute_target_stats__0.1.Rds")
initial_attr$attr 


# Dynamic network simulation
## Load function for dynamic network simulation
source("R/sim_network.R") 
## Simulate rural network
rural_nw <- sim_network(est = ests$Rural , nsteps = 3)
urban_nw <- sim_network(est = ests$Urban , nsteps = 3)


file.name <- 
    paste0("data/netsim_outputs/sim_", "__", networks,"__", est_apch, ".Rds")



saveRDS(rural_nw, file = file.name[1])
saveRDS(urban_nw, file = file.name[2])
