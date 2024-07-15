layers_dx <- 
  function(est_nw){
    
     # T-ERGM for home, school, work, and nonhome
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
    
    dx
  }