
# sub-function to initiate a network and assign nodal attribute of age group and layer-specific contact status to that network
initiate_nw <- 
  function(attri_tarstats, network){
    
    # Create a list to store things for output
    output <- list()  
    
    # Total number of nodes in each network
    n_node = attri_tarstats$attr[[network]] %>% nrow() 
    
    # Initiate nodes
    nw <- network_initialize(n_node)
    
    # Adding age attribute to the nodes
    ## Age group
    nw <- set_vertex_attribute(nw, attrname = "age.grp",
                                      value= as.character(attri_tarstats$attr[[network]]$node.age.grp )
    )
    
    ## Continuous age
    nw <- set_vertex_attribute(nw, attrname = "age",
                                      value= attri_tarstats$attr[[network]]$node.age
    )
    
    # Adding household id to the home layer
    output$nw_h <- set_vertex_attribute(nw, attrname = "hh_id", 
                                        value = attri_tarstats$attr[[network]]$household_id)

    
    # Adding the nodal contact status of the conditioned layer for the conditioning x-layer effect
    ## For school as the conditioned layer
    output$nw_s <- set_vertex_attribute(nw, attrname = "deg.x_layer", 
                                        value = attri_tarstats$attr[[network]]$contact_attribute_Work
    )
    
    ## For work as the conditioned layer
    output$nw_w <- set_vertex_attribute(nw, attrname = "deg.x_layer",
                                        value = attri_tarstats$attr[[network]]$contact_attribute_School
    )
    
    # The non-home layer only contains the age attributes
    output$nw_nh <- nw
    
    output
    
  }

# sub-function to arrange edge counts from mixing matrix to lexicographical order for model fitting
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

# sub-function to define layer-specific target statistics and model formula
## Note the function reads the list of target statistics from the environment. The following explains the meaning of each argument 
### layer - 1 of the 4 layers: "Home", "School", "Work", "Nonhome"
### site - 1 of the 2 sites: "Rural", "Urban"
### form_model - 1 of the following 3 formation models of interest. Note: fst_gt0_edge is the 1st lexicographic location whose target statistic isn't 0
#### 1) nmix_saturate_nmatch_hh_id: ~edges+ nodemix("age.grp", levels2 = -fst_gt0_edge)+ nodematch("hh_id")
#### 2) nmix_saturate:  ~edges+ nodemix("age.grp", levels2 = -fst_gt0_edge)
#### 3) nmix_saturate_xlayer:  ~edges+ nodemix("age.grp", levels2 = -fst_gt0_edge)+nodefactor("nmix_saturate_xlayer", levels =-1)
formula_tarstats <- 
  function(layer, 
           form_model,
           target_nmix_vec_layer,
           x_layer,
           degrange,
           dissolution_value
  ){
    
    if (layer == "School"){ # if the main layer of interest is school
      x_layer <-  x_layer %>% filter(association =="s_by_w")
      
    } else if (layer == "Work"){ # if the main layer of interest is work
      x_layer <-  x_layer %>% filter(association =="w_by_s")
      
    } else {}
    
    
    # Formation model and its target statistics

    
    ## Specify the lexicographic location of the first non-zero edge
    fst_gt0_edge <- which(target_nmix_vec_layer$target_nmix_vec != 0)[1]
    
    ## Define the formula of formation model
    if (form_model == "nmix_saturate_nmatch_hh_id"){
      ### Fully saturate model for age mixing without x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
      frmn_fm <- 
        paste0(
          "~edges +",
          "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ")",
          "+ nodematch(\"hh_id\") + degree(0:2) + degrange(from=4)"
        )
      frmn_fm <- as.formula(frmn_fm)
      
      ### Target statistics correspond to the formation model
      tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
                 target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge],  #  edges counts from nodemix, excluding the first non-zero edge
                 target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge (i.e., 100% of edges are in the assortative mixing for hh_id)
                 c(degrange$N_nodes_age)[-4] # number of nodes in each age group
      )
      
    } else if (form_model == "nmix_saturate"){
      ### Fully saturate model for age mixing without x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
      frmn_fm <- 
        paste0(
          "~edges +",
          "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ") + degree(0:2) + degrange(from=4)"
        )
      frmn_fm <- as.formula(frmn_fm)
      
      ### Target statistics correspond to the formation model
      tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
                 target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge],  #  edges counts from nodemix, excluding the first non-zero edge
                 c(degrange$N_nodes_age)[-4] # number of nodes in each age group
                 )
      
    } else if (form_model == "nmix_saturate_xlayer"){
      ### Fully saturate model for age mixing with x-layer effect. For age mixing, we use the 1st non-zero lexicographic term as the reference group
      frmn_fm <- 
        paste0(
          "~edges +",
          "nodemix(\"age.grp\", levels2 =  -",   fst_gt0_edge, ")+",
          "nodefactor(\"deg.x_layer\", levels =-1) + degree(0:2) + degrange(from=4)"
        )
      frmn_fm <- as.formula(frmn_fm)
      
      ### Target statistics correspond to the formation model
      tstat <- c(target_nmix_vec_layer$target_nmix_vec %>% sum(), # total edge
                 target_nmix_vec_layer$target_nmix_vec[- fst_gt0_edge],  #  edges counts from nodemix, excluding the first non-zero edge
                 x_layer %>% pull(nf_other_layer_1), # x-layer effect
                 c(degrange$N_nodes_age)[-4] # number of nodes in each age group
      )
    } 
    
    # layer-based dissolution model statistics
    diss <- 
      dissolution_coefs(dissolution = ~offset(edges), 
                        duration = dissolution_value 
      )
    
    output <- list() # use a list to store things
    
    output$frmn_fm <- frmn_fm; output$tstat <- tstat; output$diss <- diss
    
    output
    
  }

# top-level function reading nodal attributes, target statistics, the dissolution statistics of the Urban and Rural networks
model_inputs <- function(attri_tarstats, dissolution){
  
  ############## Set up vertex attributes ##############
  # run the function to set up the nodal attributes 
  nw_rural <- initiate_nw(attri_tarstats = attri_tarstats, network = "rural")
  
  nw_urban <- initiate_nw(attri_tarstats = attri_tarstats, network = "urban")
  
  
  ############## Arrange target statistics for age mixing in lexicographic order  ##############
  # Note: we treat the 1st non-zero age group as reference group for nodemix -
  # for the home, nonhome, and school layers, it's the edge of "0-9y-0-9y"
  # for the work layer, it's the edge of "20-29y-20-29y"
  
  # Target statistics for age mixing
  ## Apply the function to each layer
  ### Rural
  target_nmix_vec_rural <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix, nmix_tar_lex)
  
  ### Urban
  target_nmix_vec_urban <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix, nmix_tar_lex)
  
  ############## Set up model formulas and network statistics  ##############
  # Use the function to specify models and network statistics
  ## Create a list to store things
  formula_tarstats_rural <- formula_tarstats_urban <- list()
  
  ## Argument specifications for each layer 
  layers <- c("Home", "School", "Work", "Nonhome") 
  form_model_types <- c("nmix_saturate_nmatch_hh_id", "nmix_saturate_xlayer", "nmix_saturate_xlayer", "nmix_saturate")
  
  for (i in 1:4) {
    formula_tarstats_rural[[i]] <- 
      formula_tarstats(
        layer = layers[i],
        form_model = form_model_types[i],
        target_nmix_vec_layer=target_nmix_vec_rural[[layers[i]]],
        x_layer = attri_tarstats$targetstats_x.layer$rural,
        degrange = attri_tarstats$degrange$rural[[layers[i]]],
        dissolution_value = dissolution  %>% filter(study_site =="Rural") %>% filter(contact_location == layers[i]) %>% pull(know_contact_duration)
      )
    
    formula_tarstats_urban[[i]] <- 
      formula_tarstats(
        layer = layers[i],
        form_model = form_model_types[i],
        target_nmix_vec_layer = target_nmix_vec_urban[[layers[i]]],
        x_layer = attri_tarstats$targetstats_x.layer$urban,
        degrange = attri_tarstats$degrange$urban[[layers[i]]],
        dissolution_value = dissolution  %>% filter(study_site =="Urban") %>% filter(contact_location == layers[i]) %>% pull(know_contact_duration)
      )
    
  }
  names(formula_tarstats_rural) <- names(formula_tarstats_urban) <- layers
  
  # Output all things 
  output <- list()
  output$initiate_nw$Rural <- nw_rural; output$initiate_nw$Urban <- nw_urban
  output$formula_tarstats$Rural <- formula_tarstats_rural; output$formula_tarstats$Urban <- formula_tarstats_urban
  
  output
  
}


## Define function to estimate models of the 8 layers, using either stochastic approximation or MCMLE
est_nws <- 
  function(control.arg, layer, site, model_input_items){
    
    if(site=="Rural"){ 
      nw_attributes <- model_input_items$initiate_nw$Rural # network attributes of all layers
      model_inputs <- model_input_items$formula_tarstats$Rural # network statistics and formation model formula of all layers
    } else if (site =="Urban"){
      nw_attributes <- model_input_items$initiate_nw$Urban
      model_inputs <- model_input_items$formula_tarstats$Urban
    } else{}
    
    
    # define nodal attribute for the model
    if(
      layer == "Home"
    ){
      nw_attributes_layer = nw_attributes$nw_h
    } else if (
      layer == "School"
    ){
      nw_attributes_layer = nw_attributes$nw_s
    } else if (
      layer == "Work"
    ){
      nw_attributes_layer = nw_attributes$nw_w
    } else if (
      layer == "Nonhome"
    ){
      nw_attributes_layer = nw_attributes$nw_nh
    }else {}
    
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
