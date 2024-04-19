
library("tidyverse")
library("EpiModel")

# reading estimated models and model's formulas
est_2_nws  <- 
  readRDS("./data/models/netest_8_layers_stocha_apoxy.RData")

formulas.targetstats  <- 
  readRDS("./data/models/formulas.targetstats.RData")

# Diagnosing layers 
layers_dx <- function(est_eight_layers, formulas.targetstats, nw){
  layers <- c("Home", "School", "Work", "Nonhome") 
  dx_layers  <- list()
for (i in 1:length(layers)) {
  print(i)
  if(i %in% c(1:3) # T-ERGM for home, school, work
     ){
  dx_layers[[i]] <-
    netdx(est_eight_layers[[nw]][[layers[i]]],
          nsims = 30,
          ncores = 10,
          nsteps = 1000,
          nwstats.formula = formulas.targetstats[[nw]][[layers[i]]]$frmn_fm,
          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
          set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
          dynamic = TRUE,
          skip.dissolution = FALSE
          #keep.tedgelist = TRUE
    )
  }else{ # ERGM for nonhome
    dx_layers[[i]] <- 
      netdx(est_eight_layers[[nw]][[layers[i]]],
            nsims = 30,
            ncores = 10,
            nsteps = 1000,
            nwstats.formula = formulas.targetstats[[nw]][[layers[i]]]$frmn_fm,
            set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
            set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
            dynamic = FALSE,
            skip.dissolution = FALSE
            #keep.tedgelist = TRUE
      )
  }
  
  
}
names(sim_layers_r) <- layers

sim_layers_r
}

layers_dx_r <- 
layers_dx(est_eight_layers = est_2_nws, 
          formulas.targetstats = formulas.targetstats, 
          nw = "Rural" # can be "Rural" / "Urban"
          )


# Outputting netsim items
# sim_layers_all <- list(sim_layers_r, sim_layers_u); names(sim_layers_all) <- c("Rural", "Urban")
# saveRDS(sim_layers_all, file = "data/models/netdx_7_layers.RData")

# 
sim_layers_all <- 
readRDS("data/models/netdx_7_layers.RData")

# Assessing the simulated layers
## rural
sim_layers_all$Rural$Home
sim_layers_all$Rural$School %>% plot()
sim_layers_all$Rural$Work
sim_layers_all$Rural$Nonhome

## urban
sim_layers_all$Urban$Home
sim_layers_all$Urban$Work
sim_layers_all$Urban$Nonhome


