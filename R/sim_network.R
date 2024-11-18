sim_network <- function(
    est,
    nsteps 
) {
  
  # Init network sim
  nw <- list()
  for (i in 1:3) {
    x <- est[[i]]
    nw[[i]] <- simulate(x$fit, basis = x$fit$newnetwork,
                        control = control.simulate.ergm(MCMC.burnin = 2e5)
    )
  } # nw contains simulations for the 4 layers
  
  names(nw) <- c("School", "Work", "Nonhome")
  
  # Dynamic time loop
  for (at in 1:nsteps) {
    
    # update age at each time step
    ## get age of the current time step
    age <- 
      get_vertex_attribute(nw[[1]], attrname = "age") 
      
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
    
    ## reassign continuous and categorical age to the nodal attribute of the 4 layers
    for (i in 1:3) {
      nw[[i]] <- set_vertex_attribute(nw[[i]], attrname = "age" , 
                                      value =age)
      nw[[i]] <- set_vertex_attribute(nw[[i]], attrname = "age.grp", 
                                      value =age.grp)
    }
    
    # Simulate network for Nonhome layer
    nw[["Nonhome"]] <- suppressWarnings(simulate(nw[["Nonhome"]],
                                         formation = est[["Nonhome"]]$formation,
                                         dissolution = est[["Nonhome"]]$coef.diss$dissolution,
                                         coef.form = est[["Nonhome"]]$coef.form,
                                         coef.diss = est[["Nonhome"]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    # Note: for the following School & Work, we use the binary contact status (0-no contact [momentary degree =0], 1-have contact [momentary degree >0]) of the interacting layer as the nodal attribute for the layer of interest 
    
    # Simulate network for School layer 
    ## momentary degree (number of edges) of each node at work
    deg_node_w <-  get_degree(nw[["Work"]])
      # as.numeric(summary(nw[["Work"]] ~ sociality(base = 0), at = at)
      # )
    
    ## dichotomize momentary degree (number of edges) of each node at work into contact status
    deg_bi_node_w <- 
      ifelse(deg_node_w>0, yes=1, no=0)

    ## assign the contact status at work to school
    nw[["School"]] <- set_vertex_attribute(nw[["School"]], attrname = "deg.x_layer" , 
                                    value =deg_bi_node_w # this is contact status at nonhome layer 
    )
    
    ## simulate
    nw[["School"]] <- suppressWarnings(simulate(nw[["School"]],
                                         formation = est[["School"]]$formation,
                                         dissolution = est[["School"]]$coef.diss$dissolution,
                                         coef.form = est[["School"]]$coef.form,
                                         coef.diss = est[["School"]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    # Simulate network for Work layer
    ## momentary degree (number of edges) of each node at school
    deg_node_s <- get_degree(nw[["School"]])
  
    
    ## dichotomize momentary degree (number of edges) of each node at school into contact status
    deg_bi_node_s <- 
      ifelse(deg_node_s>0, yes=1, no=0)
    
    ## assign the contact status at school to work
    nw[["Work"]] <- set_vertex_attribute(nw[["Work"]], attrname = "deg.x_layer", 
                                    value = deg_bi_node_s)
    
    ## simulate
    nw[["Work"]] <- suppressWarnings(simulate(nw[["Work"]],
                                         formation = est[["Work"]]$formation,
                                         dissolution = est[["Work"]]$coef.diss$dissolution,
                                         coef.form = est[["Work"]]$coef.form,
                                         coef.diss = est[["Work"]]$coef.diss$coef.crude,
                                         time.start = at,
                                         time.slices = 1,
                                         time.offset = 0,
                                         monitor = "all",
                                         output = "networkDynamic"))
    
    
    cat("\n Step ", at, "/", nsteps)
  }
  
  nw
}
