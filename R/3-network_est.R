rm(list = ls())

library("tidyverse")
library("EpiModel")

# Loading data
## target statistics
node_attribute_target_stats <- readRDS("data/network_params/node_attribute_target_stats.RData")

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_params/network_params.RData")

source("R/model_inputs.R")

model_input_items <- 
model_inputs(attri_tarstats = node_attribute_target_stats, dissolution = netstats$dissolution)

############## Model fitting and simulation  ##############
# Estimating model stochastic approximation and MCMLE
## Define control argument, "sto_apoxy" is for stochastic approximation, "mcmle" is for MCMLE
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

## Souring function to estimate models of the 8 layers, using either stochastic approximation or MCMLE
source("R/est_nws.R")

## Use function to estimate model, based on stochastic approximation (sa)
est_eight_layers_sa <- 
est_nws(control.arg=control.args$sto_apoxy, all_layers = T, model_input_items = model_input_items)


## Use function to estimate model for each layer, using MCMLE
### Note: initially, I set the netest to est the model of the 8 layers by one under MCMLE, as under stochastic approximation
### this can be done successfully. However, the program froze at iteration of 32 for urban school when using loop
### So I estimate the model by site using MCMLE

### Rural
est_eight_layers_h_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Home", site = "Rural", 
          model_input_items = model_input_items)
est_eight_layers_s_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "School", site = "Rural",
          model_input_items = model_input_items)
est_eight_layers_w_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Work", site = "Rural",
          model_input_items = model_input_items)
est_eight_layers_nh_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Nonhome", site = "Rural",
          model_input_items = model_input_items)

### Urban
est_eight_layers_h_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Home", site = "Urban",
          model_input_items = model_input_items)
est_eight_layers_s_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "School", site = "Urban", 
          model_input_items = model_input_items)
est_eight_layers_w_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Work", site = "Urban",
          model_input_items = model_input_items)
est_eight_layers_nh_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Nonhome", site = "Urban",
          model_input_items = model_input_items)



## Outputting things in lists
### Estimated model
nws_r <- list(est_eight_layers_h_r,
              est_eight_layers_s_r,
              est_eight_layers_w_r,
              est_eight_layers_nh_r )
names(nws_r) <- layers

nws_u <- list(est_eight_layers_h_u,
              #est_eight_layers_s_u,
              est_eight_layers_w_u,
              est_eight_layers_nh_u)
names(nws_u) <- layers[-2]

nws <- list(nws_r, nws_u)
names(nws) <- c("Rural", "Urban")

### outputting the 8 layers of stochastic approximation
saveRDS(est_eight_layers_sa,  file = "./data/models/netest_8_layers_stocha_apoxy.RData")


### outputting model formula and target statistics
saveRDS(model_input_items$formula_tarstats, file = "./data/models/formulas.targetstats.RData")



