
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

############## Set up vertex attributes ##############
# function to initiate a network and assign nodal attribute of age group and layer-specific contact status to that network
initiate_nw <- 
function(attri_tarstats, network){

# Create a list to store things for output
output <- list()  
  
# Total number of nodes in each network - difference caused by rounding
n_node = attri_tarstats$attr[[network]] %>% nrow() 

# Initiate nodes
nw <- network_initialize(n_node)

# Adding age attribute to the nodes
output$nw <- set_vertex_attribute(nw, attrname = "age.grp",
                                value= as.character(attri_tarstats$attr[[network]]$target_age_grp )
                                )

# Adding the nodal contact status of the conditioned layer for the conditioning x-layer effect
## For school as the conditioned layer
output$nw_s <- set_vertex_attribute(output$nw, attrname = "deg.x_layer", 
                                   value = as.character(attri_tarstats$attr[[network]]$contact_attribute_School
                                   )
)

## For work as the conditioned layer
output$nw_w <- set_vertex_attribute(output$nw, attrname = "deg.x_layer",
                                   value = as.character(attri_tarstats$attr[[network]]$contact_attribute_Work
                                   )
)

output

}

# run the function to set up the nodal attributes 
nw_rural <- initiate_nw(attri_tarstats = attri_tarstats, network = "rural")

nw_urban <- initiate_nw(attri_tarstats = attri_tarstats, network = "urban")


############## Arrange target statistics for age mixing in lexicographic order  ##############
# Note: we treat the 1st non-zero age group as reference group for nodemix -
# for the home, nonhome, and school layers, it's the edge of "0-9y-0-9y"
# for the work layer, it's the edge of "20-29y-20-29y"

# Target statistics for age mixing
## Function to arrange edge counts from mixing matrix to lexicographical order for model fitting
nmix_tar_lex <- 
function(edge_ct_mx){
  
  # convert matrix from list into a matrix item
  matrix <- edge_ct_mx %>% as.matrix()
  
  # arrange the upper triangular matrix into a vector containing edges in lexicographic order
  target_nmix_vec <- c(matrix[,1][1] %>% as.numeric(), 
                       matrix[,2][1:2] %>% as.numeric(),
                       matrix[,3][1:3] %>% as.numeric(),
                       matrix[,4][1:4] %>% as.numeric(),
                       matrix[,5][1:5] %>% as.numeric(),
                       matrix[,6][1:6] %>% as.numeric()
  ) 
  
  # create a column "lexi_order" indicating the lexicographic order
  data.frame(target_nmix_vec) %>% rownames_to_column(var= "lexi_order") %>% 
    # the lexicographic order is labeled in numbers
    mutate(lexi_order = as.numeric(as.character(lexi_order))) %>% 
    # "mx_loc" is a variable indicating the corresponding matrix location of the edges reordered into the dataframe
    # the left and right sides of "._." indicate the row and column indices of a upper triangular matrix
    mutate(mx_loc = c("1_1", 
                      paste0(c(1:2), "_2"),
                      paste0(c(1:3), "_3"),
                      paste0(c(1:4), "_4"),
                      paste0(c(1:5), "_5"),
                      paste0(c(1:6), "_6"))
           )
}

## Apply the function to each layer
### Rural
target_nmix_vec_rural <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix, nmix_tar_lex)

### Urban
target_nmix_vec_urban <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix, nmix_tar_lex)

############## Set up model formulas and network statistics  ##############

# Defining a function to try different target statistics
## Note the function reads the list of target statistics from the global environment. The following explains the meaning of each argument 
### layer - 1 of the 4 layers: "Home", "School", "Work", "Nonhome"
### site - 1 of the 2 sites: "Rural", "Urban"
### form_model - 1 of the following 2 formation models of interest 
#### 1) nmix_saturate:  ~edges+ nodemix("age.grp", levels2 = -1)
#### 2) nmix_saturate_xlayer:  ~edges+ nodemix("age.grp", levels2 = -1)+nodefactor("deg.work", levels =-1)

formula_tarstats <- 
  function(layer, 
           site,
           form_model 
  ){
    
    if (site == "Rural"){ # 
      target_nmix_vec <- target_nmix_vec_rural
      x_layer <- attri_tarstats$targetstats_x.layer$rural
      
    } else{
      target_nmix_vec <- target_nmix_vec_urban
      x_layer <- attri_tarstats$targetstats_x.layer$urban
    }
    
    
    if (layer == "School"){ # if the main layer of interest is school
      x_layer <-  x_layer %>% filter(association =="s_by_w")
      
    } else if (layer == "Work"){ # if the main layer of interest is work
      x_layer <-  x_layer %>% filter(association =="w_by_s")
      
    } else {}
    
    
   # Formation model and its target statistics
   ## Extract the lexicographically ordered edge count of a layer from the list  
   target_nmix_vec_layer <-  target_nmix_vec[[layer]]
    
   ## Specify the lexicographic location of the first non-zero edge
   fst_gt0_edge <- which(target_nmix_vec_layer$target_nmix_vec != 0)[1]
    
   ## Define the formula of formation model
 if (form_model == "nmix_saturate"){
   ### Fully saturate model for age mixing without x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
    frmn_fm <- 
     paste0(
       "~edges +",
       "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ")"
     )
   frmn_fm <- as.formula(frmn_fm)
   
   ### Target statistics correspond to the formation model
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
       
      ### Target statistics correspond to the formation model
      tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
                 target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge],  #  edges counts from nodemix, excluding the first non-zero edge
                 x_layer %>% pull(nf_other_layer_1) # x-layer effect
                )
    } 
    
    # layer-based dissolution model statistics
    diss <- 
      if(layer == "Home"){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration =1e6)
        
      } else if (layer == "Nonhome"){
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration =1)
        
      } else if (layer %in% c("School", "Work")  
                 ){ 
        # For school and work, we used the durations calculated from the query
        dissolution_coefs(dissolution = ~offset(edges), 
                          duration = netstats$dissolution %>% filter(study_site ==site & contact_location == layer) %>% pull(know_contact_duration)
        )
      } else {}
    
    output <- list() # use a list to store things
    
    output$frmn_fm <- frmn_fm; output$tstat <- tstat; output$diss <- diss
    
    output
  }

# Use the function to specify models and network statistics
## Create a list to store things
model_inputs_rural <- model_inputs_urban <- list()

## Argument specifications for each layer 
layers <- c("Home", "School", "Work", "Nonhome") 
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


############## Model fitting and simulation  ##############
# Estimating model stochastic approximation and MCMLE
## Define control argument, "sto_apoxy" is for stochastic approximation, "mcmle" is for MCMLE
control.args <-  
  list(
    sto_apoxy=
      control.ergm(
        # The following setting is copied from - https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
        main.method = "Stochastic-Approximation", 
        MCMLE.maxit = 500,
        SAN.maxit = 3,
        SAN.nsteps.times = 4,
        MCMC.samplesize = 1e4,
        MCMC.interval = 5e3,
        parallel = 1
      ),
    mcmle=
      control.ergm(
        main.method = "MCMLE",
        MCMLE.maxit = 500,
        MCMC.samplesize = 1e4,
        MCMC.interval = 5e3,
        parallel = 1
      )
  )

## Define function to estimate models of the 8 layers, using either stochastic approximation or MCMLE
est_nws <- function(control.arg, all_layers, layer, site){
  if(all_layers ==T){

    layers <- c("Home", "School", "Work", "Nonhome") 
    ## Create lists to store results of the 8 layers, one for stochastic approximation, one for MCMLE
    est_layer <- 
      list(Rural = c(), Urban = c()
      )
    
for (i in 1:2) { # i=1 is for rural network, i=2 is for urban network
 
 if(i==1){ 
   nw_attributes <- nw_rural # network attributes of all layers
   model_inputs <- model_inputs_rural # network statistics and formation model formula of all layers

 } else {
   nw_attributes <- nw_urban
   model_inputs <- model_inputs_urban
 }

for (j in 1:length(layers) # j of 1, 2, 3, 4 corresponds to home, school, work, nonhome
     ) {
  # define nodal attribute for the model
  if(
    layers[j] %in% c("Home", "Nonhome")
  ){
    nw_attributes_layer = nw_attributes$nw
  } else if (
    layers[j] == "School"
  ){
    nw_attributes_layer = nw_attributes$nw_s
  } else if (
    layers[j] == "Work"
  ){
    nw_attributes_layer = nw_attributes$nw_w
  } else .
  
  # model fitting for each layer
  est_layer[[i]][[j]] <- 
    netest(nw= nw_attributes_layer,
           formation = model_inputs[[layers[j]]]$frmn_fm, 
           target.stats = model_inputs[[layers[j]]]$tstat, 
           coef.diss = model_inputs[[layers[j]]]$diss,
           set.control.ergm = control.arg 
           
    )
  
  print(paste0("i=", i, " j=", j)
        )
}
}
  

#### Assign layer names to each network
names(est_layer$Rural) <- names(est_layer$Urban) <- layers
  
est_layer

  } else {
    
      if(site=="Rural"){ 
        nw_attributes <- nw_rural # network attributes of all layers
        model_inputs <- model_inputs_rural # network statistics and formation model formula of all layers
        
      } else if (site =="Urban"){
        nw_attributes <- nw_urban
        model_inputs <- model_inputs_urban
      } else{}
      
    
        # define nodal attribute for the model
        if(
          layer %in% c("Home", "Nonhome")
        ){
          nw_attributes_layer = nw_attributes$nw
        } else if (
          layer == "School"
        ){
          nw_attributes_layer = nw_attributes$nw_s
        } else if (
          layer == "Work"
        ){
          nw_attributes_layer = nw_attributes$nw_w
        } else {}
        
        # model fitting for each layer
        est_layer <- 
          netest(nw= nw_attributes_layer,
                 formation = model_inputs[[layer]]$frmn_fm, 
                 target.stats = model_inputs[[layer]]$tstat, 
                 coef.diss = model_inputs[[layer]]$diss,
                 set.control.ergm = control.arg 
                 
          )
    
    est_layer
  }
}

## Use function to estimate model, based on stochastic approximation (sa)
est_eight_layers_sa <- 
est_nws(control.arg=control.args$sto_apoxy, all_layers = T)

est_eight_layers_sa$Rural
est_eight_layers_sa$Urban

## Use function to estimate model for each layer, using MCMLE
### Note: initially, I set the netest to est the model of the 8 layers by one under MCMLE, as under stochastic approximation
### this can be done successfully. However, the program froze at iteration of 32 for urban school when using loop
### So I estimate the model by site using MCMLE

### Rural
est_eight_layers_h_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Home", site = "Rural")


est_eight_layers_s_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "School", site = "Rural")

est_eight_layers_w_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Work", site = "Rural")

est_eight_layers_nh_r <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Nonhome", site = "Rural")

### Urban
est_eight_layers_h_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Home", site = "Urban")


est_eight_layers_s_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "School", site = "Urban")
#### NoteL: I checked the network statistics and they seems to be organized appropriately


est_eight_layers_w_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Work", site = "Urban")


est_eight_layers_nh_u <- 
  est_nws(control.arg=control.args$mcmle, all_layers = F, layer = "Nonhome", site = "Urban")



## Outputting things in lists
### Estimated model
nws_r <- list(est_eight_layers_h_r,
              est_eight_layers_s_r,
              est_eight_layers_w_r,
              est_eight_layers_nh_r )
names(nws_r) <- layers

nws_u <- list(est_eight_layers_h_u,
              #est_eight_layers_s_u,
              est_eight_layers_w_u,
              est_eight_layers_nh_u)
names(nws_u) <- layers[-2]

nws <- list(nws_r, nws_u)
names(nws) <- c("Rural", "Urban")

### outputting the 7 layers
saveRDS(nws,  file = "./data/models/netest_7_layers.RData")

# ### outputting the yrban school layers
# names(est_eight_layers_s_u) <- layers[2]
# saveRDS(est_eight_layers_s_u,  file = "./data/models/netest_7_layers.RData")

### Model inputs
model_inputs <- 
list(model_inputs_rural, model_inputs_urban); names(model_inputs) <- c("Rural", "Urban")

saveRDS(model_inputs,  
        file = "./data/models/model_inputs.RData")


