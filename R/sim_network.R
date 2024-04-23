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
    
    # update age at each time step
    ## get age of the current time step
    age <- 
      as.numeric(get_vertex_attribute(nw[[1]], attrname = "age") 
      )
    ## update age
    ### update continuous age
    age<- age+1/365
    ### update categorical age
    age.grp <- case_when( age>0 & age<10 ~ "0-9y",
                          age>= 10 & age<=20 ~ "10-19y",
                          age>= 20 & age<=30 ~ "20-29y",
                          age>= 30 & age<=40 ~ "30-39y",
                          age>= 40 & age<=60 ~ "40-59y",
                          age>= 60  ~ "60+y",
    )
    ## reassign age to the nodal attribute of the 4 layers
    for (i in 1:4) {
      nw[[i]] <- set.vertex.attribute(nw[[i]], attrname = "age" , 
                                      value =age)
      nw[[i]] <- set.vertex.attribute(nw[[i]], attrname = "age.grp", 
                                      value =age.grp)
    }
    
    # update momemtary degree of each layer
    ## for home & nonhome, we use the momentary degree of themselves as the nodal attribute 
    ### Home
    #### momentary degree (number of edges) of each node at home
    deg_node_h <- 
      as.numeric(summary(nw[[1]] ~ sociality(base = 0), at = at)
      )
    #### assign the degree to the own layer
    nw[[1]] <- set.vertex.attribute(nw[[1]], attrname = "contact_attribute_Home" , 
                                    value =deg_node_h  
    )
    #### simulate network 
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
    
    ### Nonhome
    #### momentary degree (number of edges) of each node at nonhome
    deg_node_nh <- 
      as.numeric(summary(nw[[4]] ~ sociality(base = 0), at = at)
      )
    #### assign the degree to the own layer
    nw[[4]] <- set.vertex.attribute(nw[[4]], attrname = "contact_attribute_Nonhome", 
                                    value = deg_node_nh
                                    )
    #### simulate network
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
    
    ## for School & Work, we use the momentary degree of the interacting layer as the nodal attribute for the main layer 
    ### School 
    #### momentary degree (number of edges) of each node at work
    deg_node_w <- 
      as.numeric(summary(nw[[3]] ~ sociality(base = 0), at = at)
      )
    #### assign the degree at work to school
    nw[[2]] <- set.vertex.attribute(nw[[2]], attrname = "contact_attribute_Work" , 
                                    value =deg_node_w # this is contact status at nonhome layer 
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
    
    ### Work
    #### momentary degree (number of edges) of each node at school
    deg_node_s <- 
      as.numeric(summary(nw[[2]] ~ sociality(base = 0), at = at)
      )
    #### assign the degree at school to work
    nw[[3]] <- set.vertex.attribute(nw[[3]], attrname = "contact_attribute_School", value = deg_node_s)
    
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