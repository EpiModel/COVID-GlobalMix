# Note: the purpose of this script is to estimate networks
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = 0.1/0.4/1

# Restart R
.rs.restartR()

# Packages
suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))
suppressMessages(library(tibble))
suppressMessages(library(fs))

# Inputs
# layer <- Sys.getenv("layer")
# network <- Sys.getenv("network")
# est_apch <- Sys.getenv("est_apch")
# percent_target_pop <- Sys.getenv("percent_target_pop")

layer <- "School"
network <- "Rural"
est_apch <- "mcmle"
percent_target_pop <- 0.4

# Loading data
## target statistics
node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")

############## Recode low degree at school layer to 0  ##############
# For urban school layer the low values were <0.01, this threshold is used for the re-coding - this can make netest run, but netdx shows poor fit
node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School[
  node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School <0.01] <- 0

# For rural school layer the low values were <10, this threshold is used for the re-coding 
node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School[
  node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School <10] <- 0

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
       # SAN.maxit = 20, SAN.nsteps.times = 20,
        parallel = 1
      )
  )

control.arg = control.args[[est_apch]]
site = network
model_input_items = model_input_items
    
if(site=="Rural"){ 
  nw_attributes <- model_input_items$initiate_nw$Rural # network attributes of all layers
  model_inputs <- model_input_items$formula_tarstats$Rural # network statistics and formation model formula of all layers
} else if (site =="Urban"){
  nw_attributes <- model_input_items$initiate_nw$Urban
  model_inputs <- model_input_items$formula_tarstats$Urban
} else{}


# define nodal attribute for the model
if(
  layer %in% c("Home", "Nonhome")
){
  nw_attributes_layer = nw_attributes$nw
} else if (
  layer == "School"
){
  nw_attributes_layer = nw_attributes$nw_s
} else if (
  layer == "Work"
){
  nw_attributes_layer = nw_attributes$nw_w
} else {}


tstat <- model_inputs[[layer]]$tstat

formation = ~edges + nodemix("age.grp", levels2 = -1) + nodefactor("deg.x_layer", levels = -1)

# model fitting for each layer
est_layer <- netest(nw = nw_attributes_layer,
                    formation = formation, 
                    target.stats = tstat, 
                    coef.diss = model_inputs[[layer]]$diss,
                    set.control.ergm =  
                      control.ergm(
                        main.method = "MCMLE",
                        MCMLE.maxit = 500,
                        MCMC.samplesize = 5e5,
                        MCMC.interval = 25000,
                        # SAN.maxit = 20, SAN.nsteps.times = 20,
                        parallel = 10
                    )
                  )


dx <- netdx(est_layer, nsims = 20, ncores = 10, nsteps = 1000, 
            nwstats.formula = ~edges + nodemix("age.grp", levels2 = NULL) + nodefactor("deg.x_layer", levels = NULL),
            set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 200000,
                                                             MCMC.interval = 25000),
            set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000))

saveRDS(est_layer, file = "RuralSchool_netest.rds")
saveRDS(dx, file = "RuralSchool_netdx.rds")



# Outputting estimation result the single layer 
file.name <- paste0(
  "data/netest_outputs/netest_",
  layer, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netest_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

saveRDS(est,  file = file.name)



