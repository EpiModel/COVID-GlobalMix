sim_network <- function(
    est,
    nsteps 
) {
  
  # Init network sim
  nw <- list()
  for (i in 1:4) {
    x <- est[[i]]
    nw[[i]] <- simulate(x$fit, basis = x$fit$newnetwork,
                        control = control.simulate.ergm(MCMC.burnin = 2e5)
    )
  } # nw contains simulations for the 4 layers
  
  # Dynamic time loop
  for (at in 1:nsteps) {
    # Home #
    
    nw[[1]]
  
    
    ## use the momemtary degree of the interacting layer as the nodal attribute as the layer of interest
    nw[[1]] <- set.vertex.attribute(nw[[1]], attrname = "contact_attribute_Nonhome" , 
                                    value =deg_dist_nh # this is contact status at nonhome layer 
    )
    
    ## simulate network with the attribute of the interacting and the coefficient of the layer of interest
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
    
    ## calculate momentary degree of the layer of interest from the simulated time step
    deg_dist_h <- as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at))
    
    ## set the momentary degree of the primary layer of interest as the nodal attribute of the interacting layer
    nw[[4]] <- set.vertex.attribute(nw[[4]], attrname = "contact_attribute_Home", value = deg_dist_h)
    
    
    # Nonhome #
    nw[[4]] <- suppressWarnings(simulate(nw[[4]],
                                         formation = est[[4]]$formation,
                                         dissolution = est[[4]]$coef.diss$dissolution,
                                         coef.form = est[[4]]$coef.form,
                                         coef.diss = est[[4]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    deg_dist_h <- as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at))
    deg_dist_nh <- as.numeric(summary(nw[[4]] ~ sociality(base = 0), at = at))
    
    #deg_dist_tot <- pmin(deg_dist_h + deg_dist_s, 3) # parallel minima of 3 and the total of edges of each node
    
    
    # School #
    ## the momentary degree (number of edges) each node has at work
    deg_dist_w <- as.numeric(summary(nw[[3]] ~ sociality(base = 0), at = at)
    )
    nw[[2]] <- set.vertex.attribute(nw[[2]], attrname = "contact_attribute_Work" , 
                                    value =deg_dist_w # this is contact status at nonhome layer 
    )
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
    
    deg_dist_s <- as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at))
    nw[[3]] <- set.vertex.attribute(nw[[3]], attrname = "contact_attribute_School", value = deg_dist_s)
    
    
    # Work #
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
    
    deg_dist_s <- as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at))
    deg_dist_w <- as.numeric(summary(nw[[3]] ~ sociality(base = 0), at = at))
    
    
    
    
    
    cat("\n Step ", at, "/", nsteps)
  }
  
  return(nw)
}