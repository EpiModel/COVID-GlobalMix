model_inputs <- function(attri_tarstats, dissolution){
  
  ############## Set up vertex attributes ##############
  # function to initiate a network and assign nodal attribute of age group and layer-specific contact status to that network
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
      output$nw <- set_vertex_attribute(nw, attrname = "age.grp",
                                        value= as.character(attri_tarstats$attr[[network]]$node.age.grp )
      )
      
      ## Continuous age
      output$nw <- set_vertex_attribute(output$nw, attrname = "age",
                                        value= as.character(attri_tarstats$attr[[network]]$node.age )
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
                            duration = dissolution %>% filter(study_site ==site & contact_location == layer) %>% pull(know_contact_duration)
          )
        } else {}
      
      output <- list() # use a list to store things
      
      output$frmn_fm <- frmn_fm; output$tstat <- tstat; output$diss <- diss
      
      output
      
    }
  
  # Use the function to specify models and network statistics
  ## Create a list to store things
  formula_tarstats_rural <- formula_tarstats_urban <- list()
  
  ## Argument specifications for each layer 
  layers <- c("Home", "School", "Work", "Nonhome") 
  form_model_types <- c("nmix_saturate", "nmix_saturate_xlayer", "nmix_saturate_xlayer", "nmix_saturate")
  
  for (i in 1:4) {
    formula_tarstats_rural[[i]] <- 
      formula_tarstats(
        layer = layers[i],
        form_model = form_model_types[i],
        site ="Rural" 
      )
    
    formula_tarstats_urban[[i]] <- 
      formula_tarstats(
        layer = layers[i],
        form_model = form_model_types[i],
        site ="Urban"
      )
    
  }
  names(formula_tarstats_rural) <- names(formula_tarstats_urban) <- layers
  
  # Output all things 
  output <- list()
  output$initiate_nw$rural <- nw_rural; output$initiate_nw$urban <- nw_urban
  output$formula_tarstats$rural <- formula_tarstats_rural; output$formula_tarstats$urban <- formula_tarstats_urban
  
  output
  
}