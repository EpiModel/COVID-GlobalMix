layers_dx <- 
  function(est_nw, layer){
    
    if(layer %in% c("Home", "School", "Work") # T-ERGM for home, school, work
    ){
      dx <-
        netdx(est_nw,
              nsims =  30,
              ncores = 10,
              nsteps = 1000,
              nwstats.formula = est_nw$formation,
              set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 200000,
                                                               MCMC.interval = 25000),
              set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000),
              dynamic = TRUE,
              skip.dissolution = FALSE
        )
    }else{ # ERGM for nonhome
      dx <- 
        netdx(est_nw,
              nsims =  1000,
              ncores = 10,
    
              nwstats.formula = est_nw$formation,
              set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
              set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
              dynamic = FALSE,
              skip.dissolution = FALSE
              #keep.tedgelist = TRUE
        )
    }
    
    dx
  }