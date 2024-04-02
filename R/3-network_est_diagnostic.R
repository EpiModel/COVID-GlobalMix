


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

# Rural
## Adding age attribute
nw_rural <- set_vertex_attribute(nw_rural, attrname = "age.grp",
                                value= as.character(attri_tarstats$attr$rural$target_age_grp )
                                )
## Adding the x-layer effect
## For school 
nw_rural_s <- set_vertex_attribute(nw_rural, attrname = "deg.x_layer", 
                                   value = as.character(attri_tarstats$attr$rural$contact_attribute_School
                                   )
)
## For work
nw_rural_w <- set_vertex_attribute(nw_rural, attrname = "deg.x_layer",
                                   value = as.character(attri_tarstats$attr$rural$contact_attribute_Work
                                   )
)

# Urban
## Adding age attribute
nw_urban <- set_vertex_attribute(nw_urban, attrname = "age.grp",
                                 value= as.character(attri_tarstats$attr$urban$target_age_grp )
)
## Adding the x-layer effect
## For school 
nw_urban_s <- set_vertex_attribute(nw_urban, attrname = "deg.x_layer", 
                                   value = as.character(attri_tarstats$attr$urban$contact_attribute_School
                                   )
)
## For work
nw_urban_w <- set_vertex_attribute(nw_urban, attrname = "deg.x_layer",
                                   value = as.character(attri_tarstats$attr$urban$contact_attribute_Work
                                   )
)



############## Set up target statistics  ##############
# Note: we treat the 1st age group (0-10 years old) as reference group for nodemix

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


# model formulas and corresponding target statistics.
## We are interested in fitting different formation model to the school and work layer

# Defining a formula to try different target statistics
### layers of interest for both networks: home, school
### models of interest in form_model
#### 1) nmix_saturate:  ~edges+ nodemix("age.grp", levels2 = -1)
#### 2) nmix_saturate_xlayer:  ~edges+ nodemix("age.grp", levels2 = -1)+nodefactor("deg.work", levels =-1)

### nth_large_ct is to specify the edge types with the largest edge count to include, indexed in lexicographic order
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
    
    
    if (layer == "School"){ # if the main layer of interest is school
      x_layer <-  x_layer %>% filter(association =="s_by_w")
      
    } else if (layer == "Work"){ # if the main layer of interest is work
      x_layer <-  x_layer %>% filter(association =="w_by_s")
      
    } else {}
    
    
   ## formation model and its target statistics
   ### specify the lexicographic location of the first non-zero edge
    target_nmix_vec_layer <-  target_nmix_vec[[layer]]
    fst_gt0_edge <- which(target_nmix_vec_layer$target_nmix_vec != 0)[1]
    
 if (form_model == "nmix_saturate"){
   ### Fully saturate model for age mixing without x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
    frmn_fm <- 
     paste0(
       "~edges +",
       "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ")"
     )
   frmn_fm <- as.formula(frmn_fm)
   
   tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
              target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge]  #  edges counts from nodemix, excluding the first non-zero edge
              
   )
      
    } else if (form_model == "nmix_saturate_xlayer"){
      
    
      ### Fully saturate model for age mixing with x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
      frmn_fm <- 
        paste0(
          "~edges +",
          "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ")+",
          "nodefactor(\"deg.x_layer\", levels =-1)"
        )
      frmn_fm <- as.formula(frmn_fm)
       
      tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
                 target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge],  #  edges counts from nodemix, excluding the first non-zero edge
                 x_layer %>% pull(nf_other_layer_1) # x-layer effect
                )
    
    } 
    
    # layer-based dissolution model statistics
    diss <-  # still need to add the duration for the nonhome layer
      if(layer == "Home"){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration =1e6)
        
      } else if (layer == "Nonhome"){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration =1)
        
      } else if (layer %in% c("School", "Work")  
                 ){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration = netstats$dissolution %>% filter(study_site ==site & contact_location == layer) %>% pull(know_contact_duration)
        )
      } else {}
    
    output <- list() # use a list to store things
    
    output$frmn_fm <- frmn_fm; output$tstat <- tstat; output$diss <- diss
    
    output
  }

model_inputs_rural <- model_inputs_urban <- list()

layers <- c("Home", "School", "Work", "Nonhome") # layers that we'll define the model formulas and corresponding target statistics
form_model_types <- c("nmix_saturate", "nmix_saturate_xlayer", "nmix_saturate_xlayer", "nmix_saturate")

for (i in 1:4) {
  model_inputs_rural[[i]] <- 
    formula_tarstats(
      layer = layers[i],
      form_model = form_model_types[i],
      site ="Rural" 
    )
  
  model_inputs_urban[[i]] <- 
    formula_tarstats(
      layer = layers[i],
      form_model = form_model_types[i],
      site ="Urban"
    )
  
}
names(model_inputs_rural) <- names(model_inputs_urban) <- layers

## 20230402 start from here to fit the models target stats 

# Model fitting and simulation
## network to be used
site="Rural"
if(site=="Rural"){
  nw=nw_rural; nw_s=nw_rural_s; nw_w=nw_rural_w
}  else if (site == "Urban") {
  nw=nw_urban
} else .


### Estimating using stochastic approximation and MCMLE 
#### Stochastic approximation

est <- 
  netest(nw, # Home
         formation = model_inputs_rural$Home$frmn_fm, 
         target.stats = model_inputs_rural$Home$tstat, 
         coef.diss = model_inputs_rural$Home$diss,
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

est <- 
  netest(nw, # Nonhome
         formation = model_inputs_rural$Nonhome$frmn_fm, 
         target.stats = model_inputs_rural$Nonhome$tstat, 
         coef.diss = model_inputs_rural$Nonhome$diss,
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

est <- 
  netest(nw_s, # School
         formation = model_inputs_rural$School$frmn_fm, 
         target.stats = model_inputs_rural$School$tstat, 
         coef.diss = model_inputs_rural$School$diss,
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

est <- 
  netest(nw_w, # Work
         formation = model_inputs_rural$Work$frmn_fm, 
         target.stats = model_inputs_rural$Work$tstat, 
         coef.diss = model_inputs_rural$Work$diss,
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
  ) # fail to run the problem is deg.school rather than deg work should be assigned as nodal attribute

# scrutinize target statistics as the model cannot run - we found its arranged in correct order

model_inputs_rural$Work$tstat

(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum() 
  )==  model_inputs_rural$Work_some_edges_x_layer$tstat[1]

## Notes:
### attempt 1 - Since model doesn't run w/ x-layer effect, we run the following a model with saturated nodemix terms for age, but the running failed
### attempt 2 - We include the largest nodemix term, netest and netsim run successfully
### attempt 3 - We extract the first ten largest edges, which are also all those that are non-zero. This make me realize that the error of the model in the
### saturated nodemix model is likely caused by the lack of degree of freedom, despite "level2 = -1" was used, so we choose another edge as reference group.
model_inputs_rural$Work_some_edges_x_layer <- 
formula_tarstats(
  layer = "Work",
  form_model = "nmix_saturate_xlayer",
  site ="Rural"
)

model_inputs_rural$Work_some_edges_x_layer$tstat

##### stochastic approximation
est <- 
  netest(nw_w, # Work_some_edges_x_layer, w/o x-layer effect
         formation = model_inputs_rural$Work_some_edges_x_layer$frmn_fm, 
         target.stats = model_inputs_rural$Work_some_edges_x_layer$tstat, 
         coef.diss = model_inputs_rural$Work_some_edges_x_layer$diss,
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
est.mcmle <- 
  netest(nw_w, 
         formation = model_inputs_rural$Work_some_edges_x_layer$frmn_fm, 
         target.stats = model_inputs_rural$Work_some_edges_x_layer$tstat, 
         coef.diss = model_inputs_rural$Work_some_edges_x_layer$diss,
         edapprox = TRUE,
         set.control.ergm = control.ergm(
           main.method = "MCMLE",
           MCMLE.maxit = 500,
           MCMC.samplesize = 1e4,
           MCMC.interval = 5e3,
           parallel = 1
         )
         
  ) 

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
        nwstats.formula = model_inputs_rural$Work_some_edges_x_layer$frmn_fm,
        set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
        dynamic = TRUE,
        skip.dissolution = FALSE
        #keep.tedgelist = TRUE
)
# 
# ## SJ: added the two set.control arguments to improve the overall fit. Good diagnostics (<0.5% of targets)
# ##   Often need to bump up the nsteps for long-duration edges like this model
# 
# sim
# 
par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(sim, plots.joined = FALSE)

mcmc.diagnostics(est.mcmle$fit)
# 
# 
# ## SJ: Experiment with fully saturated nodemix model
# 
# # Check ordering of nodemix values/target stats
# sim1 <-   netdx(est.mcmle,  
#                 nsims = 1, 
#                 ncores = 1,
#                 nsteps = 100,
#                 nwstats.formula = model_inputs$frmn_fm, 
#                 set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
#                 dynamic = TRUE,
#                 keep.tnetwork = TRUE
# )
# nw100 <- get_network(sim1, collapse = TRUE, at = 100)
# nw100
# 
# summary(nw100 ~ edges + nodemix("age.grp", levels2 = NULL))
# 
# nmix.mat <- as.numeric(as.matrix(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% round()))
# nmix.vec <- nmix.mat[c(1, 7:8, 13:15, 19:22, 25:29, 31:36)]
# 
# est.mcmle.full <- 
#   netest(nw, 
#          formation = ~edges + nodemix("age.grp", levels2 = -1), 
#          target.stats = c(sum(nmix.mat), nmix.vec[-1]), 
#          coef.diss = model_inputs$diss,
#          edapprox = TRUE,
#          set.control.ergm = control.ergm(
#            main.method = "MCMLE",
#            MCMLE.maxit = 500,
#            MCMC.samplesize = 1e4,
#            MCMC.interval = 5e3,
#            parallel = 1
#          )
#          
#   )
# summary(est.mcmle.full)
# 
# # No problems with convergence
# 
# sim <- 
#   netdx(est.mcmle.full,  
#         nsims = 20, 
#         ncores = 5, 
#         nsteps = 1000, 
#         # nwstats.formula = model_inputs$frmn_fm, 
#         set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
#         set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 3e5),
#         dynamic = TRUE,
#         skip.dissolution = FALSE
#         #keep.tedgelist = TRUE
#   )
# 
# print(sim)
# plot(sim)
# 
# # Target stats look great
# 
# 
