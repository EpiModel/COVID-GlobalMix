

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library(EpiModel)

## Inputs ##
# CITY <- "A"
# 
# city <- Sys.getenv("CITY")
site = "Urban"

if (site == "Urban") {
  nw_name <- "Urban"
} else {
  nw_name <- "Rural"
}

## Load Data ##
### GM load network item
# fn <- paste("data/artnet.NetEst", gsub(" ", "", nw_name), "rda", sep = ".")
# est <- readRDS(file = fn)

attri_tarstats <- readRDS("data/network_params/network_targetstats.RData") 
est <- readRDS("data/models/netest_7_layers.RData")

## Dynamic network sim



output$nw <- set_vertex_attribute(nw, attrname = "age.grp",
                                  value= as.character(attri_tarstats$attr[[network]]$target_age_grp )
                                  )


nw[[1]]

sim_network <- function(est, nsteps = 52*5) {
  
  # Init network sim
  nw <- list()
  for (i in 1:4) {
    x <- est$Rural[[i]]
    nw[[i]] <- simulate(x$fit, basis = x$fit$newnetwork,
                        control = control.simulate.ergm(MCMC.burnin = 2e5)
                        )
  } # nw contains simulations for the 4 layers
  
  # Dynamic time loop
  for (at in 1:nsteps) {
    # Main #
    deg_dist_casl <- as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at)
                                ) # skip over this since this is another layer
    nw[[1]] <- set.vertex.attribute(nw[[1]], attrname = "age.grp" , 
                                    value = as.character(attri_tarstats$attr[[network]]$target_age_grp ) #. this should be the contact status at home layer i==1
                                    )
    nw[[1]] <- suppressWarnings(simulate(nw[[1]],
                                         formation = est[[1]]$formation,
                                         dissolution = est[[1]]$coef.diss$dissolution,
                                         coef.form = est[[1]]$coef.form,
                                         coef.diss = est[[1]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    deg_dist_main <- as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at))
    nw[[2]] <- set.vertex.attribute(nw[[2]], attrname = "deg.main", value = deg_dist_main)
    
    # Casual #
    nw[[2]] <- suppressWarnings(simulate(nw[[2]],
                                         formation = est[[2]]$formation,
                                         dissolution = est[[2]]$coef.diss$dissolution,
                                         coef.form = est[[2]]$coef.form,
                                         coef.diss = est[[2]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    deg_dist_main <- as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at))
    deg_dist_casl <- as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at))
    deg_dist_tot <- pmin(deg_dist_main + deg_dist_casl, 3)
    nw[[3]] <- set.vertex.attribute(nw[[3]], attrname = "deg.tot", value = deg_dist_tot)
    
    # One-Off #
    nw[[3]] <- suppressWarnings(simulate(nw[[3]],
                                         formation = est[[3]]$formation,
                                         dissolution = est[[3]]$coef.diss$dissolution,
                                         coef.form = est[[3]]$coef.form,
                                         coef.diss = est[[3]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    cat("\n Step ", at, "/", nsteps)
  }
  
  return(nw)
}
out <- sim_network(est, nsteps = 260)

fns <- strsplit(fn, "[.]")[[1]]
fn.new <- paste(fns[1], "NetSim", fns[3], "rda", sep = ".")
saveRDS(out, file = fn.new)