library(EpiModel); library(dplyr);library(tibble)

# setting up the environment for rural school layer 
layers = c("Home", "School", "Work", "Nonhome"); layer=layers[2]
networks = c("Rural", "Urban"); network=networks[1]
est_apch = "sto_apoxy" # or "mcmle" #  
percent_target_pop = 0.1


# helper functions
source("R/netest_helper_functions.R")

# Inputs - note: this shold be unmuted when running the script through sbatch. Currently, we use slurmworkflow to submit jobs of this script to the HPC.
# layer <- Sys.getenv("layer")
# network <- Sys.getenv("network")
# est_apch <- Sys.getenv("est_apch")
# percent_target_pop <- Sys.getenv("percent_target_pop")

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
        MCMC.samplesize = 5e5,
        MCMC.interval = 25000,
        parallel = 10
      )
  )

# Sourcing function to estimate model
est <- est_nws(
  control.arg = control.args[[est_apch]],
  layer = layer,
  site = network,
  model_input_items = model_input_items
)

############## Model diagnostic  ##############
# Diagnosing layers 
source("R/layers_dx.R")

dx <- 
  layers_dx(est_nw = est
  )

plot(dx)

# check item put into netest
model_input_items$formula_tarstats$Rural$School

node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School
node_attribute_target_stats$targetstats_x.layer$rural

node_attribute_target_stats$degrange$rural$School %>% data.frame()


