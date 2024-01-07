# Characterization of target statistics and model parameterization
lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)

# Total number of nodes in each network
n_node=1e4

# load network parameters
netstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_stats.RData")


# Categories in key variabes
target_age_grp <- netstats$formation$formation_stats_rural$edge_node_factor_match_rural$nf.age.grp$participant_age %>% unique()%>% factor() # the six age group
layers <-  netstats$formation$formation_stats_rural$edge_node_factor_match_rural$edge$contact_location %>% unique()

# target population numbers in urban & rural networks 
target_age_distribut <- data.frame(target_age_grp=rep(target_age_grp,2),
                                   total_pop=c(c(199+750+933,1017+958,1196+1195,1309+1272,1215+1088+1067+876,777+626+499+321+437), # number of population in the rural network from DSS
                                               c(309+1144+1458,1474+1814,1805+1731,1740+1669,1471+1395+1206+975,891+572+412+236+245)  # number of population in the urban network from DSS
                                              ),
                                   network=rep(c("rural", "urban"), each = length(target_age_grp))
)%>% 
  group_by(network) %>% # proportion (relative frequency) of target population in each age group by network
  mutate(prop=total_pop/sum(total_pop)) %>% ungroup() %>% 
  mutate(tar_pop = round(prop*n_node)) # number of node at each age group

target_age_distribut %>% mutate(tar_pop = prop*1e4)%>% pull(tar_pop) %>% sum()


# Function generating nodal's age and age group based on distribution of target population
node.age.grp <- 
  function(target_age_dist, # age distribution of target population
           site, # study site - urban / rural
           n # total number of node in a network 
  ){
    ## number of nodes by age.grp in a network
    target_age_dist_site<- 
      target_age_dist %>% filter(network == site) 
    
    ## assign numeric code to each age group in order
    age.grp.df <- 
      target_age_dist_site %>% rownames_to_column() %>% 
      rename(age.grp.num=rowname) 
    
    ## generate individual nodes labeled by age group 
    age.grp.num <- age.grp.df %>% 
      slice(rep(1:n(), times= tar_pop)
      ) %>% 
      pull(age.grp.num) 
    
    ## For each age group, generating numeric age for each node based on the range of each age group from a uniform distribution
    ### lower and upper boundry of age groups
    min_age <- c(0,10,20,30,40,60); max_age <- c(9,19,29,39,59,100) # upper and lower ranges of each age group
    
    age <- c()
    for (i in 1:6) {
      age <-
        c(age,
          runif(n= target_age_dist_site$tar_pop[i],# the number of node to simulate in each group equals to the number of target population in each group
                min=min_age[i], max=max_age[i]
          ) # age range in each age group
        )
    }
    
    data.frame(age.grp.num, age) %>% 
      left_join(age.grp.df %>% select(age.grp.num, target_age_grp), by = "age.grp.num")
  }

node.age.grp.rural <- 
  node.age.grp(target_age_dist=target_age_distribut, site= "rural", n=n_node) # rural network
node.age.grp.urban <- 
  node.age.grp(target_age_dist=target_age_distribut, site= "urban", n=n_node) # urban network


# Visualizing age distribution of study population
# ggarrange(
#   target_age_distribut %>% ggplot(aes(x=target_age_grp, y=total_pop))+geom_bar(stat = "identity")+facet_wrap(~network)+
#     geom_text(aes(label=total_pop), vjust=-0.3, size=3.5)+
#     theme_classic()+ylab("Frequency")+xlab("Age group"),
#   target_age_distribut %>% ggplot(aes(x=target_age_grp, y=prop))+geom_bar(stat = "identity")+facet_wrap(~network)+
#     geom_text(aes(label=round(prop,2)), vjust=-0.3, size=3.5)+
#     theme_classic()+ylab("Relative frequency (obs.)")+xlab("Age group"),
#   
#   rbind(
#     node.age.grp.rural %>% group_by(target_age_grp) %>% summarize(mean_age= mean(age), prop=n()/9999) %>% mutate(network = "rural"),
#     node.age.grp.urban %>% group_by(target_age_grp) %>% summarize(mean_age= mean(age), prop=n()/9999) %>% mutate(network = "urban")
#   ) %>% ggplot(aes(x=target_age_grp, y=prop))+geom_bar(stat = "identity")+facet_wrap(~network)+
#     geom_text(aes(label=round(prop,2)), vjust=-0.3, size=3.5)+
#     theme_classic()+ylab("Relative frequency (sim.)")+xlab("Age group"),
#   
#   nrow = 3
# ) 

# Function generating nodal attribute of contact status at each layer
node.layer.contact <- function(deg.age.layer.dist_2days, target_age_dist, node.age.group){
  
  deg.layer.prop <- 
    deg.age.layer.dist_2days %>% 
    filter(contact_status == 1
    ) %>% select(-contact_status) %>% pivot_longer(!layer, names_to = "age.grp", values_to = "gt_0_prop") 
  
  
  deg.layer.prop <- 
    left_join(deg.layer.prop, 
              target_age_dist %>% 
                rename(age.grp = target_age_grp) %>% select(-c(total_pop, prop)) %>% 
                mutate(tar_pop = round(tar_pop)), # rounding the number of nodes to integer
              by = c("age.grp") 
              
    )
  
  layer_attribute_single_layer   <- data.frame() # create dataframe to store intermediate results
  
  ## generate nodal attribute of contact  each layer
  for (i in 1:length(layers)
  ) {
    for (j in 1:length(target_age_grp)) {
      deg.prop_single_layer_age_grp <-  deg.layer.prop %>% filter(layer == layers[i] & age.grp == target_age_grp[j])
      
      n_pop_single <- deg.prop_single_layer_age_grp %>% pull(tar_pop) # number of nodes to generate for this single scenario
      
      contact_attribute <- # attribute of contact in a single age group and layer
        rbinom(n =  n_pop_single, 
               size = 1,#  for bernoulli trial
               prob = deg.prop_single_layer_age_grp %>% pull(gt_0_prop)
        ) 
      
      layer_attribute_single_layer <- 
        rbind(layer_attribute_single_layer,
              data.frame( target_age_grp=rep(target_age_grp[j], length = n_pop_single),
                          contact_attribute
              )
        )
      
    }
    colnames(layer_attribute_single_layer)[2]= paste0("contact_attribute_", layers[i])
    
    if(sum(as.numeric( ! layer_attribute_single_layer$target_age_grp == node.age.group$target_age_grp))>0
    ) {
      print("warning: the age groups do not match ")
    }
    
    node.age.group <- cbind(node.age.group, layer_attribute_single_layer %>% select(2))  # save the nodal status of contact for i 
    
    layer_attribute_single_layer <- data.frame() # before moving to the next iteration of i + 1, we remove the data from iteration of i
  }
  
  node.age.group
}

node.age.grp.rural <- 
node.layer.contact(deg.age.layer.dist_2days = netstats$formation$formation_stats_rural$layer_assoc_rural$deg.age.layer.dist_2days, 
                   target_age_dist = target_age_distribut %>% filter( network == "rural"), 
                   node.age.group = node.age.grp.rural) # rural network

node.age.grp.urban <- 
  node.layer.contact(deg.age.layer.dist_2days = netstats$formation$formation_stats_urban$layer_assoc_urban$deg.age.layer.dist_2days, 
                     target_age_dist = target_age_distribut %>% filter( network == "urban"), 
                     node.age.group = node.age.grp.urban) # urban network




############## Set up vertex attribute ##############
# Initiate nodes
nw_rural <- nw_urban <- network_initialize(n_node)



# Nodes w/ age groups, each layer has the same age attribution
nw_rural <- set_vertex_attribute(nw_rural, "age.grp",
                                 node.age.grp.rural %>% pull(age.grp.num)
)


nw_rural_h <- set_vertex_attribute(nw_rural, attrname = "work",
                                   value = rep(0:1, each = n_node/2) 
) # check how ARTnet did this

# nw_urban <- set_vertex_attribute(nw_urban, "age",
#                                  node.age.grp.urban %>% pull(age_site)
#                                  )


############## Target statistics ##############
# Baseline target statistics
## function to calculate formation target stats for ~ edge + nodefactor(age.grp) + nodematch(age.grp)
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
      ) %>% select(-edges) # given we decided to go with the total number edges adjust for age distribution of target population, we exclude this variable  
    
    
    
    
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

formation_stats$rural <- 
  target_stats_form_base(form_stat = formation_stats$rural, 
                         target_age_dist = target_age_distribut %>% filter(network == "rural"),
                         n_node = n
  )


formation_stats$urban <- 
  target_stats_form_base(form_stat = formation_stats$urban, 
                         target_age_dist = target_age_distribut %>% filter(network == "urban"),
                         n_node = n
  )

# Evaluating target stats
## nf
formation_stats$rural$nf.age.grp %>% select(participant_age, contact_location, nf.ag ) %>% pivot_wider(names_from = participant_age, values_from = nf.ag)
formation_stats$urban$nf.age.grp %>% select(participant_age, contact_location, nf.ag ) %>% pivot_wider(names_from = participant_age, values_from = nf.ag)

## nm
formation_stats$rural$nm.age.grp %>% select(participant_age, contact_location, nm.ag ) %>% pivot_wider(names_from = participant_age, values_from = nm.ag) # NA mean the edge wasn't observed
formation_stats$urban$nm.age.grp %>% select(participant_age, contact_location, nm.ag ) %>% pivot_wider(names_from = participant_age, values_from = nm.ag)

#  Number of matched edges in different age groups, edge-level, for node mix
## Function for calculating target statistics of nodemix - talk to Sam about this 11/10 meeting.
target_stat_nmix <- 
  function(
    layer, # network layer 
    mix_matrix, 
    nf.age.grp # 
    
  ){
    mix_matrix[[layer]] %>% 
      rownames_to_column(., var = "participant_age") %>% 
      mutate(
        `10-19y` = (nf.age.grp  %>% filter(contact_location == layer) %>% select(participant_age,  nf.ag) %>% filter(participant_age == "10-19y") %>% pull(nf.ag)/2 # check with Sam, should the proportion here be the average proportion of the two age groups or the age group of the egocentric node
        )*`10-19y`,
        `20-29y` = (nf.age.grp  %>% filter(contact_location == layer) %>% select(participant_age,  nf.ag) %>% filter(participant_age == "20-29y") %>% pull(nf.ag)/2 
        )*`20-29y`,
        `30-39y` = (nf.age.grp  %>% filter(contact_location == layer) %>% select(participant_age,  nf.ag) %>% filter(participant_age == "30-39y") %>% pull(nf.ag)/2 
        )*`30-39y`,
        `40-59y` = (nf.age.grp  %>% filter(contact_location == layer) %>% select(participant_age,  nf.ag) %>% filter(participant_age == "40-59y") %>% pull(nf.ag)/2 
        )*`40-59y`,
        `60+y` = (nf.age.grp  %>% filter(contact_location == layer) %>% select(participant_age,  nf.ag) %>% filter(participant_age == "60+y") %>% pull(nf.ag)/2 
        )*`60+y`
        #20-29y     30-39y     40-59y       60+y
        # nm.ag= (nf.ag/2)*single_day_nm_md # adapted from ARTnet: (number of nodes in each age group /2) * prop of matched nodes
      ) 
    
  }
## Home
target_stat_nmix(
  layer = "Home",
  mix_matrix = formation_stats$rural$nodemix,
  nf.age.grp = formation_stats$rural$nf.age.grp
)

formation_stats$rural$nodemix$Home

## School
target_stat_nmix(
  layer = "School",
  mix_matrix = formation_stats$rural$nodemix,
  nf.age.grp = formation_stats$rural$nf.age.grp
)
## Work
target_stat_nmix(
  layer = "Work",
  mix_matrix = formation_stats$rural$nodemix,
  nf.age.grp = formation_stats$rural$nf.age.grp
)

## Nonhome
target_stat_nmix(
  layer = "Nonhome",
  mix_matrix = formation_stats$rural$nodemix,
  nf.age.grp = formation_stats$rural$nf.age.grp
)

## nodematch(diff=F)
formation_stats$rural$nm.age.grp.sum <- 
  formation_stats$rural$nm.age.grp %>% group_by( contact_location) %>% summarize(nm_age.grp.sum = sum(nm.ag)
  )



# Effect from another layer
# insert result here






# ## Model arguments set up
# By default, we exclude the 1st age group (0-10 years old)

# Formulation model
## model for home and nonhome layers, rural
formation.edge_age.nf.nm <- 
  ~edges + nodefactor("age.grp" , levels = -1) + nodematch("age.grp" , levels = -1, diff=T)


# Target statistics of formation
## home, rural
tstats.edge_age.nf.nm.h.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "Home" ) %>% pull(edges_adj_age), # edge
                                   (formation_stats$rural$nf.age.grp %>% filter(contact_location == "Home" ) %>% pull(nf.ag))[-1],  # nodefactor
                                   (formation_stats$rural$nm.age.grp %>% filter(contact_location == "Home" ) %>% pull(nm.ag))[-1]  # nodematch
)
## nonhome, rural
tstats.edge_age.nf.nm.nonh.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "Nonhome" ) %>% pull(edges_adj_age), # edge
                                      (formation_stats$rural$nf.age.grp %>% filter(contact_location == "Nonhome" ) %>% pull(nf.ag))[-1],  # nodefactor
                                      (formation_stats$rural$nm.age.grp %>% filter(contact_location == "Nonhome" ) %>% pull(nm.ag))[-1]  # nodematch
)


# Dissolution model
diss.h <- dissolution_coefs(dissolution = ~offset(edges), duration = 1e6) # very large number, edge doesn't dissolve
diss.nh <- dissolution_coefs(dissolution = ~offset(edges), duration = 1) # non-persistent (turnover every day)

diss.school.rural <- dissolution_coefs(dissolution = ~offset(edges), 
                                       duration = dissolution_stats %>% filter(study_site=="Rural" & contact_location=="School") %>% pull(know_contact_duration_avg)
) 
diss.work.rural <- dissolution_coefs(dissolution = ~offset(edges), 
                                     duration = dissolution_stats %>% filter(study_site=="Rural" & contact_location=="Work") %>% pull(know_contact_duration_avg)
) 
diss.school.urban <- dissolution_coefs(dissolution = ~offset(edges), 
                                       duration = dissolution_stats %>% filter(study_site=="Urban" & contact_location=="School") %>% pull(know_contact_duration_avg)
) 
diss.work.urban <- dissolution_coefs(dissolution = ~offset(edges), 
                                     duration = dissolution_stats %>% filter(study_site=="Urban" & contact_location=="Work") %>% pull(know_contact_duration_avg)
) 









## Model fitting and simulation
## Rural network
### Home

# Fully saturated model with ~ edge + nodefactor + nodematch
est.h.edges.nf.nm <- netest(nw_rural, 
                            formation = formation.edge_age.nf.nm, 
                            target.stats = tstats.edge_age.nf.nm.h.rural, 
                            coef.diss =  diss.h,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.h.edges.nf.nm <- netdx(est.h.edges.nf.nm, nsims = 20, ncores = 8, 
                          nsteps = 1000, dynamic = T,
                          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                          nwstats.formula = formation.edge_age.nf.nm, 
                          keep.tedgelist = TRUE
)

dx.h.edges.nf.nm


### Nonhome

# Fully saturated model with ~ edge + nodefactor + nodematch
est.nh.edges.nf.nm <- netest(nw_rural, 
                             formation = formation.edge_age.nf.nm, 
                             target.stats = tstats.edge_age.nf.nm.nonh.rural, 
                             coef.diss =  diss.nh,
                             set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.nh.edges.nf.nm <- netdx(est.nh.edges.nf.nm, nsims = 1000, ncores = 8,  # incease n sim
                           #nsteps = 1000, 
                           dynamic = F, # this is an ERGM rather than TERGM
                           set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                           nwstats.formula = formation.edge_age.nf.nm, 
                           keep.tedgelist = TRUE
) 

dx.nh.edges.nf.nm



### School

## school, rural
### target stats for edge + nodefactor
tstats.edge_age.nf.s.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "School" ) %>% pull(edges_adj_age), # edge
                                c( (formation_stats$rural$nf.age.grp %>% filter(contact_location == "School" ) %>% pull(nf.ag))[-c(1,5,6)],0,0) # excluding the 1st category, considering it as the default reference, passing zero to the fifth and sixth categories given the low target stat
)

### target stats for edge + nodefactor + nodematch
tstats.edge.nf_nm.s.rural <- 
  c(tstats.edge_age.nf.s.rural, 
    c((formation_stats$rural$nm.age.grp %>% filter(contact_location == "School" ) %>% pull(nm.ag)
    )[-c(1,3,4,5)],0,0,0,0) # the value of the last three observed groups of 3,4,5 are <20, we pass 0 to them and the 6th grp, which wasn't observed
  )# excluding the 1st category, considering it as the default reference, passing zero to the 2nd category given low target stat


# model with data of Edge + nodefactor(age.grp) 
est.s.edges.nf <- netest(nw_rural, 
                         formation = ~edges + nodefactor("age.grp" , levels = -1
                         ) , 
                         target.stats = tstats.edge_age.nf.s.rural, 
                         coef.diss =  diss.school.rural,
                         set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.s.edges.nf <- netdx(est.s.edges.nf, nsims = 20, ncores = 8,
                       nsteps = 1000,
                       dynamic = T,
                       set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                       nwstats.formula = ~edges + nodefactor("age.grp" , levels = -1
                       ),
                       keep.tedgelist = TRUE
)
dx.s.edges.nf

### Finding out which level for nodematch should be adjusted
#### simulation adding the nodematch term to decide what categories to include
dx.s.edges.nf.nm_sim <- netdx(est.s.edges.nf, nsims = 20, ncores = 8, 
                              nsteps = 1000, 
                              dynamic = T, 
                              set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                              nwstats.formula = formation.edge_age.nf.nm, 
                              keep.tedgelist = TRUE
)



dx.s.edges.nf.nm_sim$stats.table.formation  %>% 
  mutate(Target = tstats.edge.nf_nm.s.rural,
         `Pct Diff` = 100*(`Sim Mean`-tstats.edge.nf_nm.s.rural)/tstats.edge.nf_nm.s.rural ) 


# model with data of Edge + nodefactor(age.grp) + nodematch(age.grp)
est.s.edges.nf.nm <- netest(nw_rural, 
                            formation = formation.edge_age.nf.nm , 
                            target.stats = tstats.edge.nf_nm.s.rural, 
                            coef.diss =  diss.work.rural,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)
# talk to Sam, this model cannot be run

# dx.s.edges.nf.nm <- netdx(est.s.edges.nf.nm, nsims = 20, ncores = 8, 
#                    nsteps = 1000, 
#                    dynamic = T, 
#                set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                nwstats.formula = formation.edge_age.nf.nm, 
#                keep.tedgelist = TRUE
#                )
# 
# dx.s.edges.nf.nm 
## Note: the bias for grp.3 seems still a little big, despite being < 5%



### Work

## Work, rural


### target stats for edge + nodefactor 
tstats.edge_age.nf.w.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "Work" ) %>% pull(edges_adj_age), # edge
                                c(0, 0,(formation_stats$rural$nf.age.grp %>% filter(contact_location == "Work" ) %>% pull(nf.ag))[-c(1,2)])[-3] # passing zero to the 1st and 2nd category given low target stat, excluding the 3rd category, considering it as the default reference, 
)

### target stats for edge + nodefactor + nodematch
tstats.edge.nf_nm.work.rural <- 
  c(tstats.edge_age.nf.w.rural, 
    c(0,0, (formation_stats$rural$nm.age.grp %>% filter(contact_location == "Work" ) %>% pull(nm.ag)
    )[-c(1,2)])[-3]
  )# excluding the 1st category, considering it as the default reference, passing zero to the 2nd category given low target stat

# model with data of Edge + nodefactor(age.grp) 
est.w.edges.nf <- netest(nw_rural, 
                         formation = ~edges + nodefactor("age.grp" , levels = -3) , 
                         target.stats = tstats.edge_age.nf.w.rural, 
                         coef.diss =  diss.work.rural,
                         set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.w.edges.nf<- netdx(est.w.edges.nf, nsims = 20, ncores = 8, 
                      nsteps = 1000, 
                      dynamic = T, 
                      set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                      nwstats.formula = ~edges + nodefactor("age.grp" , levels = -3), 
                      keep.tedgelist = TRUE
)

dx.w.edges.nf 

# simulation adding the nodematch term to decide what categories to include
dx.w.edges.nf.nm_sim <- netdx(est.w.edges.nf, nsims = 20, ncores = 8, 
                              nsteps = 1000, 
                              dynamic = T, 
                              set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                              nwstats.formula =  ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
                                                                                                         levels = -3, diff = T), 
                              keep.tedgelist = TRUE
)

dx.w.edges.nf.nm_sim 



dx.w.edges.nf.nm_sim$stats.table.formation  %>% 
  mutate(Target = tstats.edge.nf_nm.work.rural,
         `Pct Diff` = 100*(`Sim Mean`-tstats.edge.nf_nm.work.rural)/tstats.edge.nf_nm.work.rural ) 
# Note: none of the simulated catories is less than 5 percent indicating all of them may need to be adjusted. But, the value of the 6th group is relative low, we pass 0 to it for the below run.


# model with data of Edge + nodefactor(age.grp) + nodematch(age.grp)
est.w.edges.nf.nm <- netest(nw_rural, 
                            formation =  ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
                                                                                                 levels = -3, diff = T), 
                            target.stats = tstats.edge.nf_nm.work.rural,  #c(tstats.edge.nf_nm.work.rural[-length(tstats.edge.nf_nm.work.rural)],0), 
                            coef.diss =  diss.work.rural,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.w.edges.nf.nm <- netdx(est.w.edges.nf.nm, nsims = 20, ncores = 8, 
                          nsteps = 1000, 
                          dynamic = T, 
                          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                          nwstats.formula = ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
                                                                                                    levels = -3, diff = T), 
                          keep.tedgelist = TRUE
) #  the nodematch term for group 6 has bias > 5%. Since it is low, we can exclude it moving forward? 

dx.w.edges.nf.nm 
## Note: the bias for grp.3 seems still a little big, despite being < 5%


formation_stats$rural$nf.age.grp %>% filter(contact_location == "Work" )


### target stats for edge + nodefactor + nodematch
tstats.edge.nf_nm.s.rural <- 
  c(tstats.edge_age.nf.s.rural, 
    c((formation_stats$rural$nm.age.grp %>% filter(contact_location == "School" ) %>% pull(nm.ag)
    )[-c(1,3,4,5)],0,0,0,0) # the value of the last three observed groups of 3,4,5 are <20, we pass 0 to them and the 6th grp, which wasn't observed
  )



# 20231121 things to do
# (2) run model with baseline target states for age.grp 
# (3) 











