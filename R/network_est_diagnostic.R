lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)

# Loading data
## target statistics
attri_tarstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_targetstats.RData")


##### zeroing out nf ==0

## summary statistics, provides duration of contacts
netstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_params.RData")

############## Set up vertex attribute ##############
# Total number of nodes in each network - difference caused by rounding
n_node_rural = attri_tarstats$attr$rural %>% nrow() # compared to 117,808
n_node_urban= attri_tarstats$attr$urban %>% nrow() # compared to 257,977


# Initiate nodes
nw_rural <- network_initialize(n_node_rural)
nw_urban <- network_initialize(n_node_urban)

# Nodes w/ age groups, each layer of a network has the same age attribution
nw_rural <- set_vertex_attribute(nw_rural, attrname = "age.grp",
                                value= as.character(attri_tarstats$attr$rural$target_age_grp )
                                )

## Adding nodal attribute (contact at school) of contact for the x-layer effect of work-layer predicted effect on school
nw_rural_s <- set_vertex_attribute(nw_rural, attrname = "deg.work", 
                                   value = as.character(attri_tarstats$attr$rural$contact_attribute_School
                                                        ) 
)

## Have a look at nodal attribute
nw_rural_s %v% "age.grp"; nw_rural_s %v% "deg.work"


############## Set up target statistics  ##############
# Note: we treat the 1st age group (0-10 years old) as reference group

# Target statistics of nodemix at school, rural
## Write a function to pull target statistics from list and organize them in lexicographical order for model fitting
nmix_tar_lex <- 
function(edge_ct_mx){
  matrix <- edge_ct_mx %>% as.matrix()
  target_nmix_vec <- c(matrix[,1][1] %>% as.numeric(), 
                       matrix[,2][1:2] %>% as.numeric(),
                       matrix[,3][1:3] %>% as.numeric(),
                       matrix[,4][1:4] %>% as.numeric(),
                       matrix[,5][1:5] %>% as.numeric(),
                       matrix[,6][1:6] %>% as.numeric()
  ) # target stat of nodemix in lexicographic order
  target_nmix_vec
}

## Target statistics of nodemix at school, rural
target_nmix_vec_r_s <- nmix_tar_lex(edge_ct_mx = 
                                      attri_tarstats$targetstats_age.grp$formation_stats_rural$symmetric_mix_matrix$School
                                    )

# formation model formulas
## model for school basedline covariates + x-layer effect from work
r_s_frmn_fm <- 
  ~edges + nodefactor("age.grp", levels = -1) + 
  nodemix("age.grp", levels2 = c(1, 3, 6, 10, 15,21)) + # lexicographic order of matched edges in nodemix  
  nodefactor("deg.work", levels = -1) # the category w/o contact at work layer is treated as reference group

# Formation model target statistics 
## rural, school 
### target statistics by the ergm.ego approach
tstat.r_s_w_ergm.ego <- c(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge %>% 
                        filter(contact_location == "School" ) %>% 
                        pull(edges.ergm.ego), # edge
                      
                       (attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp %>% filter(contact_location == "School") %>% pull(nf.ag.ergm.ego))[-1],  # nodefactor, excluding 1st age group, which is the reference group
                      
                      target_nmix_vec_r_s[c(1, 3, 6, 10, 15,21)],  # matched edges from nodemix
                      
                      attri_tarstats$targetstats_x.layer$rural %>% filter(association == "s_by_w") %>% pull(nf_other_layer_1) # ties at school layer when there's contact at work layer
)

### target statistics by ARTnet approach
tstat.r_s_w_artnet <- c(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge %>% 
                           filter(contact_location == "School" ) %>% 
                           pull(edges.artnet), # edge
                         
                         (attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp %>% filter(contact_location == "School") %>% pull(nf.ag.artnet))[-1],  # nodefactor, excluding 1st age group, which is the reference group
                         
                         target_nmix_vec_r_s[c(1, 3, 6, 10, 15,21)],  # matched edges from nodemix
                         
                         attri_tarstats$targetstats_x.layer$rural %>% filter(association == "s_by_w") %>% pull(nf_other_layer_1) # ties at school layer when there's contact at work layer
)

# Dissolution model statistics
diss.r.s <-  dissolution_coefs(dissolution = ~offset(edges), 
                               duration =
                                 netstats$dissolution %>% filter(study_site == "Rural" & contact_location == "School") %>% pull(know_contact_duration
                                                                                                                                )
                               )


# Model fitting and simulation
## Rural network
### School
#### Estimating using stochastic approximation and simulate using static network - ergm.ego target statistics
##### "Stochastic-Approximation"
est.r.s <- 
  netest(nw_rural_s, 
         formation = r_s_frmn_fm, 
         target.stats = tstat.r_s_w_ergm.ego, 
         coef.diss =  diss.r.s,
         set.control.ergm = 
           control.ergm(
             main.method = "Stochastic-Approximation", # adapted from https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
             MCMLE.maxit = 500,
             SAN.maxit = 3,
             SAN.nsteps.times = 4,
             MCMC.samplesize = 1e4,
             MCMC.interval = 5e3,
             parallel = 1
           )
         )

##### MCMLE 
est.r.s.mcmle <- 
  netest(nw_rural_s, 
         formation = r_s_frmn_fm, 
         target.stats = tstat.r_s_w_ergm.ego, 
         coef.diss =  diss.r.s,
         edapprox = T
         
  ) # interpretation: MCMLE running failed

#### Based on the estimates, simulating network - ergm.ego target statistics
r.s.sim <- 
  netdx(est.r.s,  
        nsims = 10, 
        ncores = 1, 
        nsteps = 500, 
        nwstats.formula = r_s_frmn_fm, 
        #set.control.tergm = control.simulate.formula.tergm(MCMC.burnin = 2e5),
        dynamic = T,
        skip.dissolution = T
        #keep.tedgelist = TRUE
) # encounter cryptic error 

##### Interpretation - The error is likely caused by the sum of nmatch is larger than edge for target statistics using the ergm.ego approach. 


#### Estimating using stochastic approximation and simulate using static network - ARTnet target statistics
##### "Stochastic-Approximation"
est.r.s_artnet <- 
  netest(nw_rural_s, 
         formation = r_s_frmn_fm, 
         target.stats = tstat.r_s_w_artnet, 
         coef.diss =  diss.r.s,
         set.control.ergm = 
           control.ergm(
             main.method = "Stochastic-Approximation", # adapted from https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
             MCMLE.maxit = 500,
             SAN.maxit = 3,
             SAN.nsteps.times = 4,
             MCMC.samplesize = 1e4,
             MCMC.interval = 5e3,
             parallel = 1
           )
  )

##### MCMLE 
est.r.s_artnet_mcmle <- 
  netest(nw_rural_s, 
         formation = r_s_frmn_fm, 
         target.stats = tstat.r_s_w_artnet, 
         coef.diss =  diss.r.s,
         edapprox = T
  ) # MCMLE can be run but take long time


#### Based on the estimates, simulating network - artnet target statistics
r.s_artnet_sim <- 
  netdx(est.r.s_artnet ,  
        nsims = 50, 
        ncores = 1, 
        nsteps = 500, 
        nwstats.formula = r_s_frmn_fm, 
        #set.control.tergm = control.simulate.formula.tergm(MCMC.burnin = 2e5),
        dynamic = T,
        skip.dissolution = T
        #keep.tedgelist = TRUE
  ) 
r.s_artnet_sim 
##### Interpretation - with 






