layers_dx <- 
  function(est_nw){
    
     # T-ERGM for school, work, and nonhome
      dx <-
        netdx(est_nw,
              nsims =  30,
              ncores = 10,
              nsteps = 1000,
              nwstats.formula = est_nw$formation,
              set.control.ergm = control.simulate.formula.ergm(MCMC.burnin =  200000, # can bump up to 1000000
                                                               MCMC.interval = 25000), # can bump up to 50000 
              set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000 # can bump up to 100000 
                                                                 ),
              dynamic = T,
              skip.dissolution = FALSE
        )
    
    dx
  }

