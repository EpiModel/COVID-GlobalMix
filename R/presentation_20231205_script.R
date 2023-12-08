### Item 1

target_stats_form_base <- 
  function(form_stat, 
           target_age_dist,
           n_node
  ){
    
    # Number of edges per age group, node-level, for nodefactor
    form_stat$nf.age.grp <- 
      form_stat$nf.age.grp %>% 
      left_join(
        target_age_dist %>% mutate(n_pop.age.grp=round(prop*n_node)) %>% #  number of nodes in each age group = relative distribution of age group of the target population * total network nodes 
          rename(participant_age=target_age_grp) %>% select(participant_age, n_pop.age.grp), 
        by = "participant_age"
      ) %>% mutate(nf.ag = single_day_nf_md*n_pop.age.grp # number of edges in each age group = md of each age group * number of node of each age group
      ) 
    
    
    # Total Edges, edge-level, for edge
    form_stat$edge <- 
      form_stat$edge %>% 
      mutate(edges=(single_day_md*n_node) /2 
      ) %>% # the reason /2 is used here is because this is a edge-level statistic, this way didn't adjust for the age distribution of the target population.
      left_join( 
        form_stat$nf.age.grp %>% group_by(contact_location) %>% summarize(edges_adj_age=sum(nf.ag)/2 # total number of edges adjusting for population age distribution, the reason 2 is in the denominator is the because this is an edge-level statistics
        ),
        by = "contact_location"
      ) #%>% select(-edges) # given we decided to go with the total number edges adjust for age distribution of target population, we exclude this variable  
    
    
    
    
    #  Number of matched edges in the same age group, edge-level, for node match
    ## nodematch(diff=T)
    form_stat$nm.age.grp <- 
      form_stat$nm.age.grp %>% 
      left_join(form_stat$nf.age.grp %>% select(participant_age, contact_location, nf.ag), by = c("participant_age", "contact_location")  # number of nodes in age group
      ) %>% 
      mutate(
        nm.ag= (nf.ag/2)*single_day_nm_md # adapted from ARTnet: (number of nodes in each age group /2) * prop of matched nodes, the reason 2 is here is because this is an edge-level statistic
      )
    
    
    ## nodematch(diff=F)
    form_stat$nm.age.grp.sum <- 
      form_stat$nm.age.grp %>% group_by( contact_location) %>% summarize(nm_age.grp.sum = sum(nm.ag)
      )
    
    form_stat
  }

formation_stats$rural$edge %>% select(contact_location, edges, edges_adj_age)%>%
  rename(Layer=1, Edges = 2, `Age-adjusted edges`=3) %>% 
  kbl(caption = "Target statistics for edge in rural network") %>%
  kable_classic(full_width = F, html_font = "Cambria")

formation_stats$urban$edge %>% select(contact_location, edges, edges_adj_age)%>%
  rename(Layer=1, Edges = 2, `Age-adjusted edges`=3) %>% 
  kbl(caption = "Target statistics for edge in urban network") %>%
  kable_classic(full_width = F, html_font = "Cambria")

### Item 2

target_stats_form_base <- 
  function(form_stat, 
           target_age_dist,
           n_node
  ){
    
    # Number of edges per age group, node-level, for nodefactor
    form_stat$nf.age.grp <- 
      form_stat$nf.age.grp %>% 
      left_join(
        target_age_dist %>% mutate(n_pop.age.grp=round(prop*n_node)) %>% #  number of nodes in each age group = relative distribution of age group of the target population * total network nodes 
          rename(participant_age=target_age_grp) %>% select(participant_age, n_pop.age.grp), 
        by = "participant_age"
      ) %>% mutate(nf.ag = single_day_nf_md*n_pop.age.grp # number of edges in each age group = md of each age group * number of node of each age group
      ) 
    
    
    # Total Edges, edge-level, for edge
    form_stat$edge <- 
      form_stat$edge %>% 
      mutate(edges=(single_day_md*n_node) /2 
      ) %>% # the reason /2 is used here is because this is a edge-level statistic, this way didn't adjust for the age distribution of the target population.
      left_join( 
        form_stat$nf.age.grp %>% group_by(contact_location) %>% summarize(edges_adj_age=sum(nf.ag)/2 # total number of edges adjusting for population age distribution, the reason 2 is in the denominator is the because this is an edge-level statistics
        ),
        by = "contact_location"
      )# %>% select(-edges) # given we decided to go with the total number edges adjust for age distribution of target population, we exclude this variable  
    
    
    
    
    #  Number of matched edges in the same age group, edge-level, for node match
    ## nodematch(diff=T)
    form_stat$nm.age.grp <- 
      form_stat$nm.age.grp %>% 
      left_join(form_stat$nf.age.grp %>% select(participant_age, contact_location, nf.ag), by = c("participant_age", "contact_location")  # number of nodes in age group
      ) %>% 
      mutate(
        nm.ag= (nf.ag/2)*single_day_nm_md # adapted from ARTnet: (number of nodes in each age group /2) * prop of matched nodes, the reason 2 is here is because this is an edge-level statistic
      ) %>% 
    
    left_join(
      target_age_dist %>%
        rename(participant_age=target_age_grp) %>% select(participant_age, prop), # joining relative distribution of target population
      by = "participant_age"
    ) %>% left_join(form_stat$edge %>% select(contact_location, edges_adj_age), by = "contact_location" # number of edges in each layer
    ) %>% mutate(nm.ag.frp= edges_adj_age*prop*single_day_nm_md)
    
    
    ## nodematch(diff=F)
    form_stat$nm.age.grp.sum <- 
      form_stat$nm.age.grp %>% group_by( contact_location) %>% summarize(nm_age.grp.sum = sum(nm.ag)
      )
    
    form_stat
  }

formation_stats$rural$nm.age.grp %>% select(participant_age, contact_location, edges_adj_age, prop, nf.ag, nm.ag, nm.ag.frp) %>%
  mutate(
    edge_frp =edges_adj_age*prop , 
    edge_artnet = nf.ag/2,
    across(where(is.numeric), round, 0)) %>% 
  select(participant_age, contact_location, edge_artnet,  edge_frp,nm.ag, nm.ag.frp ) %>% 
  rename(`Age grp.`=1, Layers = 2, `# of edges, app. 1`=3, `# of edges, app. 2`=4, `# of matched edges, app. 1`=5, `# of matched edges, app. 2`=6) %>% 
  kbl(caption = "Num. of (matched) edges in the rural network by two approaches") %>%
  kable_classic(full_width = F, html_font = "Cambria")
