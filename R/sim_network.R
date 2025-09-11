# function to generate network-specific a household identifier attribute
node_hh_assign <- 
  function(
    #observed proportions of households with children, adults, and elderly, respectively. 
    prop.hh.with.child,
    prop.hh.with.adult,
    prop.hh.with.elderly,
    #observed proportions of children with adults. 
    prop.children.with.adult, 
    #observed frequency of household only with children 
    freq.hh.child.only,
    # mean degree calculated from the age mixing matrix of edge count
    mean.deg,
    # age group of each node in the modeled population
    age.grp # 3-category age groups consist of "0-19y", 20-59y", and "60-100y"
  ){
    
    # note the age category of "60-100y" is also labeled as "60p" in the following script
    
    # total number of nodes
    n = length(age.grp)
    
    # Set number of households based on household size
    ## household size
    hh.size <- mean.deg +1 
    ## number of households
    n.hh <- round(n / hh.size) 
    ## node's ID
    ids <- 1:n # it's fine to generate node id here as the its in order with age.grp
    ## household ID
    hh.ids <- 1:n.hh
    
    # Create empty data frames to track hh assignment, the mem.u19, mem.20t59, and mem.60p are logic variables indicating whether there is >=1 hh member belong to that age group in a household
    hh.by.age <- data.frame(hh.ids = hh.ids, mem.u19 = NA, mem.20t59 = NA, mem.60p = NA)
    # Create data frame of nodal attribute, hh is to record the houshold id a node is assigned
    persons.by.hh <- data.frame(ids = ids, age.grp = age.grp, hh = NA) 
    
    
    # Determine which households will have a member under 19 
    hh.u19 <- sample(x = hh.ids, size = round(prop.hh.with.child * n.hh)) # randomly select a set of household id, equals to the number of hh having kids<19, to consider them to have children under 19
    hh.by.age$mem.u19 = ifelse(hh.by.age$hh.ids %in% hh.u19, TRUE, FALSE) # have the selection result in the dataframe (df) tracking household assignment
    
    # Assign children under 19 to the selected households
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "0-19y")[1:length(hh.u19)], ]$hh <- hh.u19 # in the nodal attribute df, we assign the household ids of hh.u19 to nodes under 19y
    # there may still be children left who haven't been assigned to a household because there are more children than the number of households with children (hh.u19) - given a household can have >=2 kids. 
    # the following assignment distributes the remaining children by sampling from the existing households (hh.u19),
    children.hh.assign <- sample(x = hh.u19, size = length(which(age.grp == "0-19y")) - length(hh.u19), replace = TRUE)  # select a subset of household ids of 0-19y for the rest of kids
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "0-19y")[(length(hh.u19) + 1):length(which(age.grp == "0-19y"))], ]$hh <- children.hh.assign # assign the household ids to the rest of nodes in 0-19y
    
    # Determine which hh with children will have at least one 19 - 59 member
    num.children.wo.adult <- round((1-prop.children.with.adult) * (sum(age.grp == "0-19y"))) # number of households only with children
    num.children <- persons.by.hh[persons.by.hh$age.grp == "0-19y", ] %>% group_by(hh) %>% summarize(num = n()) # number of 0-19y children per household
    ## this optimization is to select households ids have children but without adults
    hh.select <- gbp1d_solver_dpp(p = num.children$num, # count of children in each hh as weight for the optimization
                                  w = num.children$num,
                                  c = num.children.wo.adult) # constraint is the number of children live in households without an adult
    hh.wo.adult <- num.children$hh[as.logical(hh.select$k)]
    hh.with.adult <- setdiff(num.children$hh, hh.wo.adult) # hh w/ adults
    hh.by.age$mem.20t59 = ifelse(hh.by.age$hh.ids %in% hh.with.adult, TRUE, NA) 
    
    # Determine which hh without children will have at least one 20 - 59 member
    num.hh.add.adult <- round(n.hh * prop.hh.with.adult - sum(hh.by.age$mem.20t59 == TRUE, na.rm = TRUE) # those mem.20t59 == TRUE are households with children and with adult, so the output is the # of households without children but with adult
    )
    hh.add.adult <- sample(x = hh.by.age[is.na(hh.by.age$mem.20t59) & hh.by.age$mem.u19 == FALSE, ]$hh.ids, # for households haven't been characterized and without children, randomly select # of households without children but with adult
                           size = num.hh.add.adult)
    hh.by.age[hh.by.age$hh.ids %in% hh.add.adult, ]$mem.20t59 <- TRUE # record the select households in the dataframe tracking hh assignment
    hh.by.age[is.na(hh.by.age$mem.20t59), ]$mem.20t59 <- FALSE # for the rest of households, we consider them to not have adult (20-59)
    hh.20t59 <- hh.by.age[hh.by.age$mem.20t59 == TRUE, ]$hh.ids # retrieve all household id with adults (20-59) 
    
    # Assign houshold ids with adults to the nodal attribute data frame
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[1:length(hh.20t59)], ]$hh <- hh.20t59 
    adults.hh.assign <- sample(x = hh.20t59, size = length(which(age.grp == "20-59y")) - length(hh.20t59), replace = TRUE) # this assignment distributes the remaining household id with adults by sampling from the existing households - each household can have 2 adults
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[(length(hh.20t59) + 1):length(which(age.grp == "20-59y"))], ]$hh <- adults.hh.assign # assign the remaining household id with adults to the nodal attribute data
    
    # Determine which hh will have a 60+ member
    hh.must.elderly <- hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$hh.ids # we consider households don't have adult as those must have elderly
    hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$mem.60p <- TRUE # same as the logic above, we consider households don't have adult as those must have elderly
    num.hh.add.elderly <- round(n.hh * prop.hh.with.elderly - length(hh.must.elderly)) # the number of households >=1 elderly - the number of household without adults = number of households needs to have elderly 
    hh.add.elderly <- sample(x = hh.by.age[is.na(hh.by.age$mem.60p), ]$hh.ids, size = num.hh.add.elderly)
    hh.by.age[hh.by.age$hh.ids %in% hh.add.elderly, ]$mem.60p <- TRUE
    hh.by.age[is.na(hh.by.age$mem.60p), ]$mem.60p <- FALSE
    hh.60p <- hh.by.age[hh.by.age$mem.60p == TRUE, ]$hh.ids
    
    # Assign elderly to selected households
    ## select rows in the nodal attribute data whose nodes is 60-100y and in a set of rows equal to the number of households having 60-100y
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[1:length(hh.60p)], ]$hh <- hh.60p
    ## for the rest of nodes of the 3rd age group, we randomly select housholds ID with 60-100y to them
    elderly.hh.assign <- sample(x = hh.60p, size = length(which(age.grp == "60+y")) - length(hh.60p), replace = TRUE)
    ## assign elderly.hh.assign to the rest of nodes 60+
    persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[(length(hh.60p) + 1):length(which(age.grp == "60+y"))], ]$hh <- elderly.hh.assign 
    
    # Save results
    output$assignments <- output$validation<- output <- list()
    output$assignments <- persons.by.hh %>% rename(node.ids=ids, hh.ids=hh)
    
    # Check Household Assignment ----------------------------------------------
    # Rules 1 - 3: Proportions of households with at least one child/adult/elderly person 
    hh.check1 <- persons.by.hh %>% group_by(age.grp) %>% summarize(num.hh = n_distinct(hh))
    hh.check1$pct.hh.simulated <- round(hh.check1$num.hh / length(unique(persons.by.hh$hh)), 3) 
    
    sim_v_obs_props <- 
      hh.check1 %>% 
      select(age.grp, pct.hh.simulated) %>% 
      mutate(prop_type = 
               recode(age.grp,
                      "0-19y" = "prop_hh_w_child",
                      "20-59y" = "prop_hh_w_adult",
                      "60+y" = "prop_hh_w_elderly",
               )
      ) %>% select(prop_type, pct.hh.simulated)
    
    ## percentage of children who live with an adult in the 19-59 age range 
    hh.check4 <- persons.by.hh %>% group_by(hh, age.grp) %>% summarize(num.person = n())
    hh.check4 <- spread(hh.check4, key = age.grp, value = num.person)
    hh.check4 <- hh.check4[!is.na(hh.check4$`0-19y`) & !is.na(hh.check4$`20-59y`), ]
    children.in.hh.w.adult <- round(
      sum(hh.check4$`0-19y`) / length(which(age.grp == "0-19y")), 3
    )
    
    sim_v_obs_dta <- 
      sim_v_obs_props %>% 
      rbind(., c("prop_child_w_adult", children.in.hh.w.adult)) %>%  #simulated data
      cbind(., pct.hh.observed = c(prop.hh.with.child, prop.hh.with.adult,prop.hh.with.elderly,prop.children.with.adult) #observed data
      )
    
    # rename variable to save the rest of validation results
    sim_v_obs_dta <- 
      sim_v_obs_dta %>% rename(data_type = prop_type, simulated = pct.hh.simulated, observed = pct.hh.observed)
    
    # Rule 4: Average household size 
    hh.check2 <- persons.by.hh %>% group_by(hh) %>% summarize(num.person = n())
    mean.hh.check2 <- round(mean(hh.check2$num.person), 2)
    
    sim_v_obs_dta <- 
      sim_v_obs_dta %>% rbind(c("hh_size", 
                                mean.hh.check2, 
                                round(hh.size,2)
      )
      )
    
    
    # 5. Every household with a child must also have at least one adult for the simulated data
    hh.check3 <- persons.by.hh %>% group_by(hh) %>% summarize(min.age.grp = min(age.grp), max.age.grp = max(age.grp))
    orphans_simulated <- nrow(hh.check3[hh.check3$min.age.grp == "0-19y" & hh.check3$max.age.grp == "0-19y", ]) # the number of households having only children
    
    orphans_simulated
    
    sim_v_obs_dta <- 
      sim_v_obs_dta %>% rbind(c("freq_hh_child_only", 
                                orphans_simulated, freq.hh.child.only
                                
      )
      )
    
    # Save household edge list
    hhPairs <- merge(persons.by.hh, persons.by.hh, by = "hh") # getting all combinations of nodes that belong to the same household - a cartesian product of node ids within each houshold
    hhPairs <- subset(hhPairs, (ids.x < ids.y)) # remove duplicate pairs
    hhPairs <- hhPairs %>% select(hh, ids.x, ids.y) %>% rename(hh.ids=hh, head.node.ids = ids.x, tail.node.ids = ids.y)
    
    output$edgelist <- hhPairs
    
    
    # save validation data to "output"
    output$validation <- sim_v_obs_dta
    
    output
  }


# function to simulate networks for the school, work, and nonhome layers
sim_network <- function(
    est, # netest items of the school, work, and nonhome layers
    nsteps # number of time steps (days) to simulate for the school, work, and nonhome layers
) {
  
  # Init network sim
  nw <- list()
  for (i in 1:3) {
    x <- est[[i]]
    nw[[i]] <- simulate(x$fit, basis = x$fit$newnetwork,
                        control = control.simulate.ergm(MCMC.burnin = 2e5)
    )
  } # nw contains simulations for the 3 layers
  
  names(nw) <- c("School", "Work", "Nonhome")
  
  # Dynamic time loop
  for (at in 1:nsteps) {
    
    # update age at each time step
    ## get age of the current time step
    age <- 
      get_vertex_attribute(nw[["School"]], attrname = "age") 
      
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
    
    ## reassign continuous and categorical age to the nodal attribute of the 3 layers
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







