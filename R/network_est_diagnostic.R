
source("R/network_targetstats.R")

library("tidyverse")
library("EpiModel")
library("ggpubr")
library("knitr")
library("svglite")
library("kableExtra")

# Loading data
## target statistics
attri_tarstats <- readRDS("data/network_params/network_targetstats.RData")

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_params/network_params.RData")

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
nw_urban <- set_vertex_attribute(nw_urban, attrname = "age.grp",
                                 value= as.character(attri_tarstats$attr$urban$target_age_grp )
)

## Adding nodal attribute (contact at school) of contact for the x-layer effect of work-layer predicted effect on school
nw_rural_s <- set_vertex_attribute(nw_rural, attrname = "deg.work",
                                   value = as.character(attri_tarstats$attr$rural$contact_attribute_School
                                                        )
)


############## Set up target statistics  ##############
# Note: we treat the 1st age group (0-10 years old) as reference group

# Target statistics of nodemix 
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
  
  data.frame(target_nmix_vec) %>% rownames_to_column(var= "lexi_order") %>% # create a column indicating the lexicographic order
    mutate(lexi_order = as.numeric(as.character(lexi_order))) %>% 
    mutate(mx_loc = c("1_1",
                      paste0(c(1:2), "_2"),
                      paste0(c(1:3), "_3"),
                      paste0(c(1:4), "_4"),
                      paste0(c(1:5), "_5"),
                      paste0(c(1:6), "_6"))
           )
}

## Target statistics of nodemix at each layer, rural
target_nmix_vec_rural <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix, nmix_tar_lex)
target_nmix_vec_urban <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix, nmix_tar_lex)

target_nmix_vec_rural

# model formulas and corresponding target statistics.
## We are interested in fitting different formation model to the school and work layer

# Defining a formula to try different target statistics
### layers of interest for both networks: home, school
### models of interest in form_model
#### 1) base:   ~edges + nodefactor("age.grp", levels = -1) +nodematch("age.grp", diff=T)
#### 2) nmix_saturate:  ~edges+ nodemix("age.grp", levels2 = -1)
#### 3) nmix_saturate_xlayer:  ~edges+ nodemix("age.grp", levels2 = -1)+nodefactor("deg.work", levels =-1)
#### 4) nmix_some_edge: ~edges+ some edge types with large count
#### 5) "edge_x_layer": ~edges + nodefactor("other.layer.deg", levels = -1)
### For models include nodemix, nth_large_ct is to specify the number of largest edge count terms to include, indexed in lexicographic order
formula_tarstats <- 
  function(layer, 
           form_model,
           site,
           nth_large_ct  
  ){
    
    if (site == "Rural"){
      form_stats <- attri_tarstats$targetstats_age.grp$formation_stats_rural
      target_nmix_vec <- target_nmix_vec_rural
      x_layer <- attri_tarstats$targetstats_x.layer$rural
      
    } else{
      form_stats <- attri_tarstats$targetstats_age.grp$formation_stats_urban
      target_nmix_vec <- target_nmix_vec_urban
      x_layer <- attri_tarstats$targetstats_x.layer$urban
    }
    
    
    if (layer == "School"){
      x_layer <-  x_layer %>% filter(association =="s_by_w")
      
    } else if (layer == "Work"){
      x_layer <-  x_layer %>% filter(association =="w_by_s")
      
    } else .
    
    
    ## formation model and its target statistics
    if(form_model == "base"){ 
      ### w/ node factor and differential nodematch
      frmn_fm <- 
        ~edges + 
        nodefactor("age.grp", levels = -1) +
        nodematch("age.grp", diff=T)
      #nodefactor("deg.work", levels = -1) # the category w/o contact at work layer is treated as reference group
      
      tstat <- c(form_stats$edge %>% 
                   filter(contact_location == layer ) %>% 
                   pull(edges), # edge
                 
                 (form_stats$nf.age.grp %>% filter(contact_location == layer) %>% pull(nf.ag))[-1],  # nodefactor, excluding 1st age group, which is the reference group
                 
                 
                 target_nmix_vec[[layer]]$target_nmix_vec[c(1, 3, 6, 10, 15,21)]  # matched edges from nodemix
                 
      )
      
    } else if (form_model == "nmix_saturate"){
      ### fully saturate model w/o node factor
      frmn_fm <- 
        ~edges+ 
        nodemix("age.grp", levels2 = -1)# + # lexicographic order in nodemix, the 1st value excluded as reference group
      
      tstat <- c(target_nmix_vec[[layer]]$target_nmix_vec %>% sum() %>% round(), # edge
                 target_nmix_vec[[layer]]$target_nmix_vec[-1]  #  edges counts from nodemix
      )
      
    } else if (form_model == "nmix_saturate_xlayer"){
      ### fully saturate model w/o node factor
      frmn_fm <- 
        ~edges+ 
        nodemix("age.grp", levels2 = -1) + #  lexicographic order in nodemix, the 1st value excluded as reference group
        nodefactor("deg.work", levels =-1)
      
      tstat <- c(target_nmix_vec[[layer]]$target_nmix_vec %>% sum() %>% round(), # edge
                 target_nmix_vec[[layer]]$target_nmix_vec[-1],  #  edges counts from nodemix
                 x_layer %>% pull(nf_other_layer_1)
                )
      
    }
    else if (form_model == "nmix_some_edge"){
      
      
      lex_order_large_ct <- 
        target_nmix_vec[[layer]] %>% arrange(desc(target_nmix_vec)) %>% # arrange by edge count from big to small
        slice(1:nth_large_ct) %>% # take the first nth biggest edge counts
        arrange(lexi_order) %>% pull(lexi_order)
      
      lex_order_large_ct_vec  <- paste0(lex_order_large_ct, collapse =",")
      
      frmn_fm <- 
        paste0(
          "~edges +",
          "nodemix(\"age.grp\", levels2 = c( ",  lex_order_large_ct_vec, "))"
        )
      frmn_fm <- as.formula(frmn_fm)
      
      tstat <- c(form_stats$edge %>% 
                   filter(contact_location == layer ) %>% 
                   pull(edges), # edge
                 target_nmix_vec[[layer]]$target_nmix_vec[lex_order_large_ct]  #  edges counts from nodemix
      )
    } else if (form_model == "edge_x_layer"){
  
      frmn_fm <- 
        ~edges+ 
        nodefactor("deg.work", levels =-1)
      
      tstat <- c(target_nmix_vec[[layer]]$target_nmix_vec %>% sum() %>% round(), # edge
                 x_layer %>% pull(nf_other_layer_1)
      )
      
    }
    
    # layer-based dissolution model statistics
    diss <-  # still need to add the duration for the nonhome layer
      if(layer == "Home"){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration =1e6)
      }else if (layer == layer){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration = netstats$dissolution %>% filter(study_site ==site & contact_location == layer) %>% pull(know_contact_duration)
        )
      }
    
    output <- list() # use a list to store things
    
    output$frmn_fm <- frmn_fm; output$tstat <- tstat; output$diss <- diss
    
    output
  }

model_inputs <-
  formula_tarstats(
    layer = "School",
    form_model = "nmix_saturate_xlayer",
    site ="Rural",
    nth_large_ct  =
  )


model_inputs <-
  formula_tarstats(
    layer = "School",
    form_model = "nmix_saturate_xlayer",
    site ="Rural",
    nth_large_ct  =
  )

model_inputs
attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% round() 


# SJ Work Starts Here -------------------------------------------------------------------------

## SJ: edges should be the sum of this matrix
model_inputs$tstat[1] <- attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% round() %>% sum()

attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% round()
attri_tarstats$targetstats_x.layer$rural %>% filter(association == "s_by_w")

# Model fitting and simulation
## network to be used
site="Rural"
if(site=="Rural"){
  nw=nw_rural_s
}  else if (site == "Urban") {
  nw=nw_urban
} else .

#### Estimating using  schochastic approximation and MCMLE and simulate using static network - ergm.ego target statistics
##### Stochastic approximation
est <- 
  netest(nw, 
         formation = model_inputs$frmn_fm, 
         target.stats = model_inputs$tstat, 
         coef.diss = model_inputs$diss,
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

## SJ: runs with fixed edges calculation
summary(est)


model_inputs$tstat
model_inputs2 <- model_inputs$tstat
model_inputs2[length(model_inputs2)] <- 0

##### MCMLE 
est.mcmle <- 
  netest(nw, 
         formation = model_inputs$frmn_fm, 
         target.stats = model_inputs$tstat, 
         coef.diss = model_inputs$diss,
         edapprox = TRUE,
         set.control.ergm = control.ergm(
           main.method = "MCMLE",
           MCMLE.maxit = 500,
           MCMC.samplesize = 1e4,
           MCMC.interval = 5e3,
           parallel = 1
         )
         
  ) # interpretation: MCMLE running failed

## SJ: runs with fixed edges calculation, 
##  also added control.ergm settings to help convergence; 
##   converged in 6 iterations (about 30 seconds)
summary(est.mcmle)
# SJ: coefficients are pretty close across the two methods


#### Based on the estimates, simulating network - ergm.ego target statistics
sim <- 
  netdx(est.mcmle,  
        nsims = 30, 
        ncores = 10, 
        nsteps = 1000, 
        nwstats.formula = ~edges + nodemix("age.grp", levels2 = -1) + nodefactor("deg.work", levels = NULL), 
        set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
        dynamic = TRUE,
        skip.dissolution = FALSE
        #keep.tedgelist = TRUE
) # encounter cryptic error 

## SJ: added the two set.control arguments to improve the overall fit. Good diagnostics (<0.5% of targets)
##   Often need to bump up the nsteps for long-duration edges like this model

sim

par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(sim, plots.joined = FALSE)

mcmc.diagnostics(est.mcmle$fit)


## SJ: Experiment with fully saturated nodemix model

# Check ordering of nodemix values/target stats
sim1 <-   netdx(est.mcmle,  
                nsims = 1, 
                ncores = 1,
                nsteps = 100,
                nwstats.formula = model_inputs$frmn_fm, 
                set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
                dynamic = TRUE,
                keep.tnetwork = TRUE
)
nw100 <- get_network(sim1, collapse = TRUE, at = 100)
nw100

summary(nw100 ~ edges + nodemix("age.grp", levels2 = NULL))

nmix.mat <- as.numeric(as.matrix(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% round()))
nmix.vec <- nmix.mat[c(1, 7:8, 13:15, 19:22, 25:29, 31:36)]

est.mcmle.full <- 
  netest(nw, 
         formation = ~edges + nodemix("age.grp", levels2 = -1), 
         target.stats = c(sum(nmix.mat), nmix.vec[-1]), 
         coef.diss = model_inputs$diss,
         edapprox = TRUE,
         set.control.ergm = control.ergm(
           main.method = "MCMLE",
           MCMLE.maxit = 500,
           MCMC.samplesize = 1e4,
           MCMC.interval = 5e3,
           parallel = 1
         )
         
  )
summary(est.mcmle.full)

# No problems with convergence

sim <- 
  netdx(est.mcmle.full,  
        nsims = 20, 
        ncores = 5, 
        nsteps = 1000, 
        # nwstats.formula = model_inputs$frmn_fm, 
        set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
        dynamic = TRUE,
        skip.dissolution = FALSE
        #keep.tedgelist = TRUE
  )

print(sim)
plot(sim)

# Target stats look great


