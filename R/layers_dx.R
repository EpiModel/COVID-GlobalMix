layers_dx <- 
  function(est_nw){
    
     # T-ERGM for home, school, work, and nonhome
      dx <-
        netdx(est_nw,
              nsims =  30,
              ncores = 10,
              nsteps = 1000,
              nwstats.formula = est_nw$formation,
              set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 1000000, # 2) bumping up from 200000
                                                               MCMC.interval = 50000), # 2) bumping up from 25000
              set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 100000 # 1) bumping up from 50000
                                                                 ),
              dynamic = F,
              skip.dissolution = FALSE
        )
    
    dx
  }
