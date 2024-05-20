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
              set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
              set.control.tergm = control.simulate.formula.tergm(MCMC.maxchanges = 1e7),
              dynamic = TRUE,
              skip.dissolution = FALSE
              #keep.tedgelist = TRUE
        )
    }else{ # ERGM for nonhome
      dx <- 
        netdx(est_nw,
              nsims =  30,
              ncores = 10,
              nsteps = 1000,
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