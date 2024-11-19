# Note: the purpose of this script is to estimate networks
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = 0.1/0.4/1

# Packages
suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))
suppressMessages(library(tibble))
suppressMessages(library(fs))

# helper functions
source("R/netest_helper_functions.R")


# Loading data
## target statistics
node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")


############## Recode low degree at school layer to 0  ##############
# For both the urban and rural school layers, we recode those degree <1 to 0 to make netest viable
# For urban school layer the low values were <0.01, this threshold is used for the re-coding - this can make netest run, but netdx shows poor fit
node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School[
  node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School <1] <- 0

# For rural school layer the low values were <10, this threshold is used for the re-coding 
node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School[
  node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School <1] <- 0


############## Define items which will be read by netest  ##############
model_input_items <- 
model_inputs(node_attribute_target_stats = node_attribute_target_stats, dissolution = netstats$dissolution, layer = layer)

if(layer %in% c("School", "Work", "Nonhome")){
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
        MCMC.samplesize = 5e5,
        MCMC.interval = 25000,
        parallel = 10
      )
  )

# Use function to estimate model
est <- est_nws(
  control.arg = control.args[[est_apch]],
  layer = layer,
  site = network,
  model_input_items = model_input_items
)

# Outputting estimation result the single layer 
file.name <- paste0(
  "data/netest_outputs/netest_",
  layer, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
)
} else{ # Home layer
  est <- model_input_items$initiate_nw[[network]]$nw_h
  
  file.name <- paste0(
    "data/netest_outputs/deterministic_",
    layer, "__", network,"__",  percent_target_pop, ".Rds"
  )
  
}

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netest_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

saveRDS(est,  file = file.name)



