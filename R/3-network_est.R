# Note: the purpose of this script is to estimate networks
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"

library(dplyr)
library(EpiModel)
library(tibble)

# Loading data
## target statistics
node_attribute_target_stats <- readRDS("data/network_stats_attributes/node_attribute_target_stats__0.1.Rds")

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")

############## Define items which will be read by netest  ##############
source("R/model_inputs.R")
model_input_items <- 
model_inputs(attri_tarstats = node_attribute_target_stats, dissolution = netstats$dissolution)


############## Model estimation  ##############
# Define control argument, "sto_apoxy" is for stochastic approximation, "mcmle" is for MCMLE
control.args <-  
  list(
    sto_apoxy=
      control.ergm(
        # The following setting is copied from - https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
        main.method = "Stochastic-Approximation", 
        MCMLE.maxit = 500,
        SAN.maxit = 3,
        SAN.nsteps.times = 4,
        MCMC.samplesize = 1e4,
        MCMC.interval = 5e3,
        parallel = 1
      ),
    mcmle=
      control.ergm(
        main.method = "MCMLE",
        MCMLE.maxit = 500,
        MCMC.samplesize = 1e4,
        MCMC.interval = 5e3,
        parallel = 1
      )
  )

# Sourcing function to estimate model
source("R/est_nws.R")

est <- est_nws(
  control.arg = control.args[[est_apch]],
  layer = layer,
  site = network,
  model_input_items = model_input_items
)

# outputting estimation result of the 8 layers 
file.name <- paste0(
  "data/netest_outputs/netest_8_layers_",
  layer, "__", network,"__", est_apch, ".Rds"
)

saveRDS(est,  file = file.name)



