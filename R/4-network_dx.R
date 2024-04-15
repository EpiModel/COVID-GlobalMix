
library("tidyverse")
library("EpiModel")
library("ggpubr")
library("knitr")
library("svglite")
library("kableExtra")

# reading estimated models and model's formulas
est_mcmle_2_nws  <- 
  readRDS("./data/models/netest_7_layers.RData")

model_inputs  <- 
  readRDS("./data/models/model_inputs.RData")

layers <- c("Home", "School", "Work", "Nonhome") 
layers_u <- layers[-2] # remove school layer for the urban network



# simulating layers 
sim_layers_r <- sim_layers_u <- list()
## rural layers
for (i in 1:length(layers)) {
  print(i)
  if(i %in% c(1:3) # T-ERGM for home, school, work
     ){
  sim_layers_r[[i]] <-
    netdx(est_mcmle_2_nws[["Rural"]][[layers[i]]],
          nsims = 30,
          ncores = 10,
          nsteps = 1000,
          nwstats.formula = model_inputs$Rural[[layers[i]]]$frmn_fm,
          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
          set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
          dynamic = TRUE,
          skip.dissolution = FALSE
          #keep.tedgelist = TRUE
    )
  }else{ # ERGM for nonhome
    sim_layers_r[[i]] <- 
      netdx(est_mcmle_2_nws[["Rural"]][[layers[i]]],
            nsims = 30,
            ncores = 10,
            nsteps = 1000,
            nwstats.formula = model_inputs$Rural[[layers[i]]]$frmn_fm,
            set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
            set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
            dynamic = FALSE,
            skip.dissolution = FALSE
            #keep.tedgelist = TRUE
      )
  }
  
  
}
names(sim_layers_r) <- layers

## urban layers
for (i in 1:3) {
  print(i)
  if(i %in% c(1,2) # T-ERGM for home and work
     ){
  sim_layers_u[[i]] <-
    netdx(est_mcmle_2_nws[["Rural"]][[layers_u[i]]],
          nsims = 30,
          ncores = 10,
          nsteps = 1000,
          nwstats.formula = model_inputs$Rural[[layers_u[i]]]$frmn_fm,
          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
          set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
          dynamic = TRUE,
          skip.dissolution = FALSE
          #keep.tedgelist = TRUE
    )
  }else{ # ERGM for nonhome
    sim_layers_u[[i]] <-
      netdx(est_mcmle_2_nws[["Rural"]][[layers_u[i]]],
            nsims = 30,
            ncores = 10,
            nsteps = 1000,
            nwstats.formula = model_inputs$Rural[[layers_u[i]]]$frmn_fm,
            set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
            set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
            dynamic = FALSE,
            skip.dissolution = FALSE
            #keep.tedgelist = TRUE
      )
    
  }
 
}
names(sim_layers_u) <- layers_u

# Assessing the simulated layers
## rural
sim_layers_r$Home
sim_layers_r$School
sim_layers_r$Work
sim_layers_r$Nonhome

## urban
sim_layers_u$Home
sim_layers_u$Work
sim_layers_u$Nonhome

# Outputting netsim items
sim_layers_all <- list(sim_layers_r, sim_layers_u); names(sim_layers_all) <- c("Rural", "Urban")
saveRDS(sim_layers_all, file = "data/models/netdx_7_layers.RData")
