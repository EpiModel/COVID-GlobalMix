
library(dplyr)
library(EpiModel)
library(tibble)

# Loading data
## target statistics
node_attribute_target_stats <- readRDS("data/network_params/node_attribute_target_stats__0.1.Rds")

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_params/network_params.Rds")




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

# Estimate model, based on stochastic approximation / MCMLE
layers <- c("Home", "School", "Work", "Nonhome")
networks <- c("Rural", "Urban")

est_apch <- "sto_apoxy" # "sto_apoxy" or "mcmle"

## Rural
est_h_r <- 
  est_nws(control.arg=control.args[[est_apch]], layer = layers[1], site = networks[1], 
          model_input_items = model_input_items)
est_s_r <- 
  est_nws(control.arg=control.args[[est_apch]], layer = layers[2], site = networks[1],
          model_input_items = model_input_items)
est_w_r <- 
  est_nws(control.arg=control.args[[est_apch]], layer = layers[3], site = networks[1],
          model_input_items = model_input_items)
est_nh_r <- 
  est_nws(control.arg=control.args[[est_apch]], layer = layers[4], site = networks[1],
          model_input_items = model_input_items)

## Urban
est_h_u <-
  est_nws(control.arg=control.args[[est_apch]], layer = layers[1], site = networks[2],
          model_input_items = model_input_items)
est_s_u <-
  est_nws(control.arg=control.args[[est_apch]], layer = layers[2], site = networks[2], 
          model_input_items = model_input_items)
est_w_u <-
  est_nws(control.arg=control.args[[est_apch]], layer = layers[3], site = networks[2],
          model_input_items = model_input_items)
est_nh_u <-
  est_nws(control.arg=control.args[[est_apch]], layer = layers[4], site = networks[2],
          model_input_items = model_input_items)

# outputting estimation result of the 8 layers 
## see if I can add a vector
# 20240419 reach here, save thing it folders in eight items
file.name <- paste0("data/models/netest_8_layers_stocha_apoxy", layers, "__", networks,"__", est_apch, ".Rds")
saveRDS(est_eight_layers_sa,  file = file.name)


### outputting model formula and target statistics
saveRDS(model_input_items$formula_tarstats, file = "data/models/formulas.targetstats.Rds")



