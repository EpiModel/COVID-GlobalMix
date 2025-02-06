# function generating age and age group for each node based on distribution of target population
node.age.grp <- 
  function(target_age_dist_site # number of target population by age group of a network (urban/rual)
  ){
    
    ## assign numeric code to each age group in order
    age.grp.df <- 
      target_age_dist_site %>% rownames_to_column() %>% 
      rename(age.grp.num=rowname) 
    
    ## generate individual nodes labeled by age group 
    age.grp.num <- age.grp.df %>% 
      slice(rep(1:n(), times= tar_pop) # 1:6 correspond to the 6 age groups from young to old 
      ) %>% 
      pull(age.grp.num) 
    
    ## For each age group, generating numeric age for each node based on the range of each age group from a uniform distribution
    ### lower and upper boundry of age groups
    min_age <- c(0,10,20,30,40,60); max_age <- c(9,19,29,39,59,100) # upper and lower ranges of each age group
    
    set.seed(20240417)
    
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
      left_join(age.grp.df %>% select(age.grp.num, target_age_grp), by = "age.grp.num") # merging the categorical age group with the corresponding numerically coded age group
  }


# function generating nodal attribute of contact status at each layer
node.layer.contact <- function(deg.age.layer.dist_2day, target_age_dist){
  
  ## Extracting proportion of having any contact
  deg.layer.prop <- # deg.layer.prop stores the proportion w/ any contact at each age group
    deg.age.layer.dist_2day %>% 
    filter(contact_status == 1 # filter out the proportion of having any contact
    ) %>% select(-contact_status) %>% pivot_longer(!layer, names_to = "age.grp", values_to = "gt_0_prop") # converting to long format to facilitate data manipulation
  
  
  layer_attribute_single_layer   <-  data.frame() # create dataframe to store intermediate results
  layer_attribute_layers <- list() # create list to store nodal status of contact for a whole layer
  
  ## generate nodal attribute of contact  at each layer (i), at each age group (j)
  ### explanation of the below code: for a layer, we generate nodel status of contact for each age group 
  ### using the proportion of contact and the total number of nodes in a layer. We replicate this for all age group and all layers
  for (i in 1:length(layers)
  ) {
    for (j in 1:length(target_age_grp)) {
      deg.prop_single_layer_age_grp <-  deg.layer.prop %>% filter(layer == layers[i] & age.grp == target_age_grp[j]) %>% pull(gt_0_prop) # retrieving the proportion of having any contact in a age group of a layer in a network
      
      n_pop_single <- target_age_dist %>% filter(target_age_grp == target_age_grp[j]) %>% pull(tar_pop) # number of nodes to generate - number of people at this at group in the modeled population
      
      contact_attribute <- # attribute of contact in a single age group and layer
        rbinom(n =  n_pop_single, 
               size = 1,#  for Bernoulli trial
               prob = deg.prop_single_layer_age_grp 
        ) 
      
      ## combining the simulated nodal status in different age groups
      layer_attribute_single_layer <- 
        rbind(layer_attribute_single_layer,
              data.frame( target_age_grp=rep(target_age_grp[j], length = n_pop_single),
                          contact_attribute
              )
        )
      
    }
    colnames(layer_attribute_single_layer)[2]= paste0("contact_attribute_", layers[i])
    
    layer_attribute_layers[[i]]<- layer_attribute_single_layer 
    
    layer_attribute_single_layer <- data.frame() # before moving to the next iteration of i + 1, we remove the data from iteration of i
    
    
    ## Note: previously, we had some logic to see whether the simulated age group of node in this function is the same as those by node.age.grp. Given the two function generate nodal statuses
    ## based on the same target population. The logic must be TRUE, so, we remove the logic.
    
    
  }
  
  ## Combining results from all layers into a dataframe 
  layer_attribute_layers <- 
    cbind(
      layer_attribute_layers[[1]],
      contact_attribute_School=layer_attribute_layers[[2]]$contact_attribute_School,
      contact_attribute_Work=layer_attribute_layers[[3]]$contact_attribute_Work,
      contact_attribute_Nonhome=layer_attribute_layers[[4]]$contact_attribute_Nonhome
    )
  
  layer_attribute_layers
  
}

# function to calculate formation target stats related to age
target_stats_age <- 
  function(form_stat, # individual-level summary statistics 
           target_age_dist # total number of nodes of a network
  ){
    
    # Note form_stat[[1]] and form_stat[[2]] respectively are the edge_node_factor_match and mix_prop
    
    # Number of nodes between edges per age group, node-level, for nodefactor, by the approach used in ARTnet
    form_stat[[1]]$nf.age.grp <- 
      form_stat[[1]]$nf.age.grp %>% 
      left_join(
        target_age_dist %>% 
          rename(participant_age=target_age_grp) , 
        by = "participant_age"
      ) %>% mutate(nf.ag.ego = two_day_nf_md*tar_pop # number of edges in each age group = md of each age group * number of node of each age group
      ) 
    
    
    
    # Total Edges, edge-level, for edge, by the approach used in ARTnet
    form_stat[[1]]$edge <-
      form_stat[[1]]$edge %>%
      mutate(edges=two_day_md/2*sum(target_age_dist$tar_pop) # the crude total number of edges = overall MD/2 * total population across all age groups in a network
      ) %>% # the reason /2 is used here is because this is a edge-level statistic, this way didn't adjust for the age distribution of the target population.
      left_join(
        form_stat[[1]]$nf.age.grp %>% group_by(contact_location) %>%
          summarize(edges.artnet=sum(nf.ag.ego)/2 # total number of edges adjusting for population age distribution, the reason 2 is in the denominator is the because this is an edge-level statistics
          ),
        by = "contact_location"
      ) %>% select(-edges) # given we decided to go with the total number edges adjust for age distribution of target population, we exclude this variable
    
    
    #  Number of matched edges in the same age group, edge-level, for node match, by the approach used in ARTnet
    ## nodematch(diff=T)
    form_stat[[1]]$nm.age.grp <- 
      form_stat[[1]]$nm.age.grp %>% 
      left_join(form_stat[[1]]$nf.age.grp %>% select(participant_age, contact_location, nf.ag.ego), by = c("participant_age", "contact_location")  # number of nodes in age group
      ) %>% 
      mutate(
        nm.ag.artnet= (nf.ag.ego/2)*two_day_nm_md # adapted from ARTnet: number of match edge in each age group = (number of nodes in each age group /2) * prop of matched nodes, the reason 2 is here is because this is an edge-level statistic
      )
    
    
    ## nodematch(diff=F)
    form_stat[[1]]$nm.age.grp.sum <- 
      form_stat[[1]]$nm.age.grp %>% group_by( contact_location) %>% summarize(nm.ag.sum.artnet = sum(nm.ag.artnet)
      )
    
    
    #  Number of edges of a specific age-mixing pattern, edge-level, for node mix - this also forms a matrix for edge count
    
    ## Define a function for the calculation of total number of edges for a age mixing pattern
    mix_edge_num <- 
      function(grp.a, grp.b, asymmetric_mix_matrix., nf.ag_layer.){ 
        # grp.a and grp.b are the two contacting age groups , asymmetric_mix_matrix. is the mixing matrix with bidirectional mixing proportion (i.e., mixing between grp a and grp b and grp b and grp a),
        # nf.ag_layer. is the total number of nodes in an age group of a contact layer 
        # grp 1, 2, 3, 4, 5, 6 respectively correspond to the youngest to oldest age groups
        sum(
          asymmetric_mix_matrix.[grp.a,grp.b]* # grp.a as egocentric node, grp.b as contact
            nf.ag_layer. %>% filter(participant_age ==target_age_grp[grp.a]) %>% pull(nf.ag.ego)/2,# the reason /2 is used here is because this is a edge-level statistic
          asymmetric_mix_matrix.[grp.b,grp.a]* # grp.b as egocentric node, grp.a as contact
            nf.ag_layer. %>% filter(participant_age ==target_age_grp[grp.b]) %>% pull(nf.ag.ego)/2,
          na.rm = T #  for an egocentric age group that do not have contact with another age group (i.e., mixing proportion == NA), we consider the corresponding number of edges ==0
        )
      }
    
    
    ## Calculate nodemix target statistics for each layer i - outputing an upper triangular matrix for each layer i
    edge_ct_matrix <- list() # create a list to store target stats for all layers
    for (i in 1:length(layers)
    ) {
      
      nf.ag_layer <- # getting the target statistics for nodefactor(age.grp)
        form_stat[[1]]$nf.age.grp %>% 
        select(participant_age, contact_location, nf.ag.ego) %>% 
        filter(contact_location == layers[i])
      
      asymmetric_mix_matrix <- # getting the asymmetric mixing matrix
        form_stat[[2]][[layers[i]]][1][[1]] # "[1]" is to retrieve the glm-based proportions, [[1]] is to extract data frame from list 
      
      
      ## Calculating number of non-diagonal mixing edges of each mixing pattern - non-assortative mixing
      edge_ct_matrix_i <- matrix(NA, 6, 6) %>% data.frame()# create a symmetric matrix to store result
      rownames(edge_ct_matrix_i) <- colnames(edge_ct_matrix_i) <- target_age_grp
      
      
      ### non-assortative mixing of 0-9y (grp 1) w/the other age.grps
      for (j in 2:6) {
        edge_ct_matrix_i[1,j] <- 
          mix_edge_num(grp.a =1, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 10-19y (grp 2) w/the other age.grps
      for (j in 3:6) {
        edge_ct_matrix_i[2,j] <- 
          mix_edge_num(grp.a =2, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 20-29y (grp 3) w/the other age.grps
      for (j in 4:6) {
        edge_ct_matrix_i[3,j] <- 
          mix_edge_num(grp.a =3, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 30-39y (grp 4) w/the other age.grps
      for (j in 5:6) {
        edge_ct_matrix_i[4,j] <- 
          mix_edge_num(grp.a =4, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 50-59y (grp 5) w/the other age.grps
      edge_ct_matrix_i[5,6] <- 
        mix_edge_num(grp.a =5, grp.b = 6, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      
      
      ## Calculating number of diagonal mixing edges of each mixing pattern - assortative mixing
      for (j in 1:6) {
        edge_ct_matrix_i[j,j] <-
          nf.ag_layer %>% filter(participant_age ==target_age_grp[j]) %>% pull(nf.ag.ego)/2* # the reason /2 is used here is because this is a edge-level statistic
          asymmetric_mix_matrix[j,j]
      }
      
      
      edge_ct_matrix[[i]] <- edge_ct_matrix_i
      
    }
    
    ## Assigning layer names to layers
    names(edge_ct_matrix) <- layers
    
    ## For all layers, recoding NA to 0 in the matrix of edge count
    edge_ct_matrix$Home[is.na(edge_ct_matrix$Home)] <- 0  #  home
    edge_ct_matrix$School[is.na(edge_ct_matrix$School)] <- 0  #  school
    edge_ct_matrix$Work[is.na(edge_ct_matrix$Work)] <- 0  #  work
    edge_ct_matrix$Nonhome[is.na(edge_ct_matrix$Nonhome)] <- 0  #  nonhome
    
    
    ## For school and work layers, zeroing out ties in the upper triangular matrix of some age groups
    ### For school of both networks, zeroing out ties of 60+y column to 0
    edge_ct_matrix$School[,6] <- 0
    ### For work of both networks, zeroing out ties of <= 19y rows to 0
    edge_ct_matrix$Work[c(1,2), ] <- 0
    
    # Number of nodes between edges of each age group, node-level, for nodefactor, by the approach used in ergm.ego
    nmix_tar_edge_sum_h <-nmix_tar_edge_sum_s <- nmix_tar_edge_sum_w <-nmix_tar_edge_sum_nh <-c()  # number of edges of each age group calculated from nodemix
    
    for (i in 1:6) { # "i" in this loop corresponds to 0-9y    10-19y    20-29y    30-39y    40-59y     60+y
      
      # Calculation using target stats based on the target population
      nmix_tar_edge_sum_h[i] <-  
        sum(edge_ct_matrix$Home[i,]) + 
        sum(edge_ct_matrix$Home[,i]) - 
        edge_ct_matrix$Home[i,i]
      
      nmix_tar_edge_sum_s[i] <-  
        sum(edge_ct_matrix$School[i,]) + 
        sum(edge_ct_matrix$School[,i]) - 
        edge_ct_matrix$School[i,i]
      
      nmix_tar_edge_sum_w[i] <-  
        sum(edge_ct_matrix$Work[i,]) + 
        sum(edge_ct_matrix$Work[,i]) - 
        edge_ct_matrix$Work[i,i]
      
      nmix_tar_edge_sum_nh[i] <-  
        sum(edge_ct_matrix$Nonhome[i,]) + 
        sum(edge_ct_matrix$Nonhome[,i]) - 
        edge_ct_matrix$Nonhome[i,i]
      
      
    }  
    
    ## Saving the results to the data frame of mean degree
    form_stat[[1]]$nf.age.grp <- 
      form_stat[[1]]$nf.age.grp %>%
      mutate(nf.ag = c(nmix_tar_edge_sum_h, nmix_tar_edge_sum_s, nmix_tar_edge_sum_w, nmix_tar_edge_sum_nh)
      )
    
    
    # Number of edges in the whole layer, edge-level, edge, by the approach used in ergm.ego
    form_stat[[1]]$edge <-
      form_stat[[1]]$edge  %>% 
      left_join(
        form_stat[[1]]$nf.age.grp %>% group_by(contact_location) %>%
          summarize(edges=sum(nf.ag)/2 # total number of edges by the approach from ergm.ego, the reason 2 is in the denominator is the because this is an edge-level statistics
          ),
        by = "contact_location"
      ) 
    
    
    # Saving things in list
    form_stat[[1]]$edge_ct_matrix <- edge_ct_matrix # saving the edge-count matrices of the four layers
    
    
    form_stat[[1]] # output the edge-count matrices with the target statistics for nodefactor and edge
  }


# function re-weighting degree distribution, the outputs are the post-stratification weight - "wi", weighted degree of each study participant,  weight proportion of each degree type (k) - "adjusted_prop"
post_stratification <- 
  function(prop_parti,# age of each study participant
           modeled_pop_dta,# age of modeled population
           degree_2d){
    
    outputs  <-  list()
    
    # Compare age distribution of study and modeled populations
    
    ## modeled population age distribution
    prop_node <- modeled_pop_dta$node.age.grp %>% table %>% prop.table 
    
    
    # Age distribution reweighting
    ## post-stratification weight - higher weight assigned to the undersampled older population, lower weight assigned to the oversampled younger population
    wai <- (prop_node/prop_parti) %>% as.data.frame() %>% rename(participant_age=1,weight=2)
    
    outputs$w_ai <- wai
    
    # Proportion of a specific degree type among the study population, age-stratified
    deg_age.grp <- table(degree_2d$contact_location, degree_2d$n_contacts, degree_2d$participant_age)
    
    ## Initialize an empty list to store the proportion dataframes
    deg_age.grp_proportion_unw<- deg_age.grp_proportion_w <-  list()
    target_age_grp <- c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")
    
    ## for each age group, calculate the proportions
    for (i in 1:6) {
      
      # Use prop.table with margin = 1 to calculate row-wise proportions
      proportion_matrix <- prop.table(as.matrix(deg_age.grp[,,i]), margin = 1)
      
      # Sum the columns with degree type >= 4 and create a new column for them
      merged_column <- rowSums(proportion_matrix[, 5:ncol(proportion_matrix)])
      
      # Subset to keep only columns for degrees < 4 and add the merged column
      ## unweighted
      proportion_matrix <- cbind(proportion_matrix[, 1:4], ">=4" = merged_column)
      
      ## weighted
      proportion_matrix_w <-
        proportion_matrix*
        (wai %>% filter(participant_age == target_age_grp[i]) %>% pull(weight)
         )
      
      # Store the modified matrix as a dataframe in the list
      ## unweighted
      deg_age.grp_proportion_unw[[target_age_grp[i]]] <- 
        as.data.frame(t(proportion_matrix))
      
      ## weighted
      deg_age.grp_proportion_w[[target_age_grp[i]]] <- 
        as.data.frame(t(proportion_matrix_w))
      
    }
    
    # summing over all age group to get the overall proportion
    deg_proportion_unw <-  Reduce("+", deg_age.grp_proportion_unw)
    deg_proportion_w <- Reduce("+", deg_age.grp_proportion_w)
    
    # normalize to [0,1]
    deg_proportion_unw_rescale <-  prop.table(as.matrix(deg_proportion_unw), margin = 2) 
    deg_proportion_w_rescale <-  prop.table(as.matrix(deg_proportion_w), margin = 2) 
    

    outputs$age_specific$ deg_age.grp_proportion_unw <- deg_age.grp_proportion_unw; outputs$age_specific$ deg_age.grp_proportion_w <- deg_age.grp_proportion_w
    outputs$deg_proportion$deg_proportion_unw <-  deg_proportion_unw; outputs$deg_proportion$deg_proportion_w <-  deg_proportion_w;
    outputs$deg_proportion_rescale$deg_proportion_unw_rescale <- deg_proportion_unw_rescale ;  outputs$deg_proportion_rescale$deg_proportion_w_rescale <- deg_proportion_w_rescale 
    
    
    outputs
  }


# Function to calculate proportion of having contact by age group and layer using the signle-day weighted degree
# 
# contact_prop_age_layer <- 
#   function(degree_2d, wai){
#     
#     # Calculate weighted degree
#     weighted_degree <- 
#     degree_2d %>% left_join(., wai, by = "participant_age") %>% 
#       mutate(weighted_deg = n_contacts*weight) # note that although the weight is included here, the proportion without adjust for it is the same
#     
#     # Summarizing the data to calculate proportion of having any contact by contact_location and participant_age
#     contact_summary <- weighted_degree %>%
#       mutate(contact_status = ifelse(weighted_deg > 0, 1, 0)) %>%
#       group_by(contact_location, participant_age, contact_status) %>%
#       summarize(count = n()) %>% ungroup %>% 
#       group_by(contact_location, participant_age) %>%
#       mutate(proportion = count / sum(count)) %>%
#       ungroup()
#     
#     
#     wt_deg.age.layer.dist_2days <- contact_summary %>% select(-count) %>% 
#       pivot_wider(names_from = c(participant_age), values_from = proportion) %>% 
#       select(`0-9y`, `10-19y`, `20-29y`, `30-39y`, `40-59y`,  `60+y`, contact_status, contact_location) %>%
#       mutate(across(everything(), ~ replace_na(., 0))) %>% as.data.frame() %>% rename(layer = contact_location )
#     
#     wt_deg.age.layer.dist_2days 
#   }


# function characterizing pre-requisites for the cross layer effect, using post-stratification weighted degree 
assoc_btw_layers <- function(degree_2d, wai){
  
  # Calculate weighted degree
  weighted_degree <- 
    degree_2d %>% left_join(., wai, by = "participant_age") %>% 
    mutate(weighted_deg = n_contacts*weight) # note that although the weight is included here, the proportion without adjust for it is the same
  
  # Note: things that are outputted from the function - single-day regression coefficients of the associations (coefficient_summary_1day), single-day predicted mean degrees conditioning on the other layer (mean_deg_1day),
  # single-day proportions of having contacts (deg.layer.dist_1day), and the raw data in wide-format (data_wide) for the regressions 
  
  ## Getting individual-level contact count at different locations by row through converting long format data to wide format 
  contact_count_wide  <- # single-day contact count in wide format
    weighted_degree %>% data.frame()%>% 
    select(-c(weight, n_contacts)) %>% 
    tidyr::pivot_wider(names_from = "contact_location", values_from = "weighted_deg"
    ) %>%  # 624 rows for rural and 624 rows for urban, each row for a participant and the degrees at different layers
    as.data.frame()
  
  
  output_model <- output <-   list()
  
  # Create dichotomous degree category for each layer
  contact_count_wide <- 
    contact_count_wide %>% mutate(
      
      Home_cat = case_when(Home==0 ~ 0,
                           Home >0 ~ 1),
      Home_cat = factor( Home_cat),
      
      School_cat = case_when(School==0 ~ 0,
                             School >0 ~ 1),
      School_cat = factor( School_cat),
      
      Work_cat = case_when(Work==0 ~ 0,
                           Work >0 ~ 1),
      Work_cat = factor( Work_cat),
      
      Nonhome_cat = case_when(Nonhome==0 ~ 0,
                              Nonhome >0 ~ 1),
      Nonhome_cat = factor( Nonhome_cat)
    )
  
  ############### Characterizing single-day coefficients of the effect of other layers ###############
  # Effects of other layers on home 
  
  output_model$h_s <- 
    contact_count_wide %>% 
    lm(Home~ School_cat, data=.)
  
  output_model$h_w <- 
    contact_count_wide %>% 
    lm(Home~ Work_cat, data=.)
  
  output_model$h_nh <- 
    contact_count_wide %>% 
    lm(Home~ Nonhome_cat, data=.) 
  
  # Effects of other layers on School
  output_model$s_h <- 
    contact_count_wide %>% 
    lm(School~ Home_cat, data=.)
  
  output_model$s_w <- 
    contact_count_wide %>% 
    lm(School~ Work_cat , data=.)
  
  output_model$s_nh <- 
    contact_count_wide %>% 
    lm(School~ Nonhome_cat , data=.)
  
  # Effects of other layers on work 
  output_model$w_h <- 
    contact_count_wide %>% 
    lm(Work~ Home_cat, data=.)
  
  output_model$w_s <- 
    contact_count_wide %>% 
    lm(Work~ School_cat , data=.)
  
  output_model$w_nh <- 
    contact_count_wide %>% 
    lm(Work~ Nonhome_cat , data=.)
  
  
  # Effects of other layers on Nonhome
  output_model$nh_h <- 
    contact_count_wide %>% 
    lm(Nonhome~ Home_cat, data=.)
  
  output_model$nh_s <- 
    contact_count_wide %>% 
    lm(Nonhome~ School_cat , data=.)
  
  output_model$nh_w <- 
    contact_count_wide %>% 
    lm(Nonhome~ Work_cat , data=.)
  
  
  ############### Summarizing the two-day coefficients on a table ###############
  tb_slope <- 
    rbind(
      summary(output_model$h_s)$coefficients, summary(output_model$h_w)$coefficients,  summary(output_model$h_nh)$coefficients, 
      summary(output_model$s_h)$coefficients,  summary(output_model$s_w)$coefficients, summary(output_model$s_nh)$coefficients,  
      summary(output_model$w_h)$coefficients  ,  summary(output_model$w_s)$coefficients  ,  summary(output_model$w_nh)$coefficients,
      summary(output_model$nh_h)$coefficients  ,  summary(output_model$nh_s)$coefficients  , summary(output_model$nh_w)$coefficients 
    )[ c(c(1:12)*2), ] # outputting the slope coefficients located at the even rows
  
  
  rownames(tb_slope) <- 
    paste0(
      rep(
        c("h_", "s_", "w_", "nh_"),
        each=3
      ), 
      rownames(tb_slope)
    )
  
  
  ############### Characterizing 2-day predicted degree of the modeled layer by other layers ###############
  # function to characterize the network statistics based on coefficients of Poisson regression
  effect_oth_layers <- 
    function(
    layer_assoc
    ){
      
      out <- 
        c(# 2-day mean degree of the outcome (conditioned) layer when the predictive (conditioning) layer didn't have contact (labeled as "other_layer.0")
          layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*0, 
          # 2-day mean degree of the outcome layer when the predictive layer have any contact (labeled as "other_layer.1")
          layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*1
        )
      
      names(out) <- paste0( "other_layer", c("=0", "=1")) 
      
      out
      
      
    }
  
  
  # Network statistics of the other layers at 2-day scale 
  nf.deg.oth_layers <- 
    rbind(
      
      ### mean degrees of other layers on home ###
      effect_oth_layers(
        layer_assoc = output_model$h_s # school
      ),
      effect_oth_layers(
        layer_assoc = output_model$h_w # work 
      ),
      effect_oth_layers(
        layer_assoc = output_model$h_nh # nonhome
      ),
      
      ### mean degrees of other layers on school ###
      effect_oth_layers(
        layer_assoc = output_model$s_h # home
      ),
      effect_oth_layers(
        layer_assoc = output_model$s_w # work 
      ),
      effect_oth_layers(
        layer_assoc = output_model$s_nh # nonhome
      ),
      
      ### mean degrees of other layers on work ###
      effect_oth_layers(
        layer_assoc = output_model$w_h # home
      ),
      effect_oth_layers(
        layer_assoc = output_model$w_s # school 
      ),
      effect_oth_layers(
        layer_assoc = output_model$w_nh # nonhome
      ),
      
      ### mean degrees of other layers on nonhome ###
      effect_oth_layers(
        layer_assoc = output_model$nh_h # home
      ),
      effect_oth_layers(
        layer_assoc = output_model$nh_s # school 
      ),
      effect_oth_layers(
        layer_assoc = output_model$nh_w # work 
      )
      
    ) %>% data.frame()%>% 
    mutate(
      association = c(
        paste0("h", "_by_", c("s", "w", "nh")),
        paste0("s", "_by_", c("h", "w", "nh")),
        paste0("w", "_by_", c("h", "s", "nh")),
        paste0("nh", "_by_", c("h", "s", "w"))
      )
      
    )%>% 
    
    # Comparing the predicted mean degs with the unadjusted mean degs of the modeled layer at the single-day scale
    cbind(
      raw_md_modeled_layer = 
        rep(
          apply(contact_count_wide %>% select(Home, School, Work, Nonhome), MARGIN = 2, mean) %>% as.numeric(), 
          each=3
          
        )
    )
  
  
  
  ############### Characterizing the proportion of having and not having contact for each layer, and the raw mean degree ###############
  # Overall proportion, w/o stratifying for age
  deg.layer.dist_2day <- 
    rbind( # proportion
      prop.table(table(contact_count_wide$Home_cat)),
      prop.table(table(contact_count_wide$School_cat)),
      prop.table(table(contact_count_wide$Work_cat)),
      prop.table(table(contact_count_wide$Nonhome_cat))
    ) %>% data.frame() %>% 
    rename(prop_0=1, prop_1=2) %>% 
    mutate(layer = c("Home", "School", "Work", "Nonhome"))
  
  # age-stratified proportion, for generating nodal attribute
  deg.age.layer.dist_2day <- 
    rbind(
      # Home
      prop.table(
        table(contact_count_wide$participant_age, contact_count_wide$Home_cat) %>% t(), # transposing to age by column and contact status by row
        margin = 2 # calculating marginal proportion by column
      ),
      # School
      prop.table(
        table(contact_count_wide$participant_age, contact_count_wide$School_cat) %>% t(), # transposing to age by column and contact status by row
        margin = 2 # calculating marginal proportion by column
      ), 
      # Work
      prop.table(
        table(contact_count_wide$participant_age, contact_count_wide$Work_cat) %>% t(), # transposing to age by column and contact status by row
        margin = 2 # calculating marginal proportion by column
      ), 
      # Nonhome
      prop.table(
        table(contact_count_wide$participant_age, contact_count_wide$Nonhome_cat) %>% t(), # transposing to age by column and contact status by row
        margin = 2 # calculating marginal proportion by column
      ) 
    ) %>% as.data.frame() %>% 
    mutate(
      contact_status = rep(c(0,1), 4),
      layer = rep(c("Home", "School", "Work", "Nonhome"), each =2)
    ); row.names(deg.age.layer.dist_2day) <- NULL
  
  ############### Outputting ###############
  output$coefficient_summary_2day <- tb_slope; output$mean_deg_2day <-  nf.deg.oth_layers;
  output$deg.layer.dist_2day <- deg.layer.dist_2day; output$deg.age.layer.dist_2day <- deg.age.layer.dist_2day
  output$data_wide <- contact_count_wide
  
  output
  
} 


# function characterizing target statistics for the cross-layer effect
target_stats_x_layer <- 
  function(
    x_layer_items,
    N # number of population a whole network
  ){
    
    # Load things needed for characterizing the cross-layer target statistics
    prop_contact <- x_layer_items$deg.layer.dist_2day # proportion of having any contact at a 2-day scale
    cond_mean_deg <- x_layer_items$mean_deg_2day # conditioned mean 2-day degree
    coefficient <- # Linear regression coefficient for the cross-layer effects
      x_layer_items$coefficient_summary_2day %>% data.frame() %>% 
      tibble::rownames_to_column(var="association") %>% rename(coefficient=2, p_value =5) %>% 
      select(association, coefficient, p_value)
    
    
    # Characterize number of node w/o (_0) and w/ (_1) contact at each layer
    ## Home
    N_h_0=N*prop_contact %>% filter(layer=="Home") %>% pull(prop_0) # number of nodes in the Home layer as conditioning layer w/o contact
    N_h_1=N*prop_contact %>% filter(layer=="Home") %>% pull(prop_1) # number of nodes in the Home layer as conditioning layer w contact
    
    ## School
    N_s_0=N*prop_contact %>% filter(layer=="School") %>% pull(prop_0) # number of nodes in the School layer as conditioning layer w/o contact
    N_s_1=N*prop_contact %>% filter(layer=="School") %>% pull(prop_1) # number of nodes in the School layer as conditioning layer w contact
    
    ## Work
    N_w_0=N*prop_contact %>% filter(layer=="Work") %>% pull(prop_0) # number of nodes in the Work layer as conditioning layer w/o contact
    N_w_1=N*prop_contact %>% filter(layer=="Work") %>% pull(prop_1) # number of nodes in the Work layer as conditioning layer w contact
    
    ## Nonhome
    N_nh_0=N*prop_contact %>% filter(layer=="Nonhome") %>% pull(prop_0) # number of nodes in the Nonhome layer as conditioning layer w/o contact
    N_nh_1=N*prop_contact %>% filter(layer=="Nonhome") %>% pull(prop_1) # number of nodes in the Nonhome layer as conditioning layer w contact
    
    # Characterize conditioned node-level edge count - total number of edges of nodes of a layer, conditioned by the other layer 
    cond_mean_deg <- 
      cond_mean_deg %>% 
      mutate(
        nf_other_layer_0 = # target stats for without contact at the conditioning layer (the "other_layer")
          case_when(
            association %in% c("s_by_h", "w_by_h", "nh_by_h") ~ other_layer.0*N_h_0, # Home as the conditioning layer
            association %in% c("h_by_s", "w_by_s", "nh_by_s") ~ other_layer.0*N_s_0, # School as the conditioning layer
            association %in% c("h_by_w", "s_by_w", "nh_by_w") ~ other_layer.0*N_w_0, # Work as the conditioning layer
            association %in% c("h_by_nh", "s_by_nh", "w_by_nh") ~ other_layer.0*N_nh_0, # Nonhome as the conditioning layer
          ),
        nf_other_layer_1 = # target stats for having contact at the conditioning layer
          case_when(
            association %in% c("s_by_h", "w_by_h", "nh_by_h") ~ other_layer.1*N_h_1, # Home as the conditioning layer
            association %in% c("h_by_s", "w_by_s", "nh_by_s") ~ other_layer.1*N_s_1, # School as the conditioning layer
            association %in% c("h_by_w", "s_by_w", "nh_by_w") ~ other_layer.1*N_w_1, # Work as the conditioning layer
            association %in% c("h_by_nh", "s_by_nh", "w_by_nh") ~ other_layer.1*N_nh_1, # Nonhome as the conditioning layer
          )
      ) %>% 
      select(association, nf_other_layer_0, nf_other_layer_1, other_layer.0, other_layer.1)
    
    # Merge the conditioned node-level edge count w/ corresponding P values of the regressions 
    output <- 
    cond_mean_deg %>% 
      mutate(
        coeffi = coefficient %>% pull(coefficient),
        p_value = coefficient %>% pull(p_value)
      ) %>% 
      rename(md_other_layer_0 =other_layer.0, md_other_layer_1 = other_layer.1 ) # renaming to names that are more understandable
    
    output
    
  }


# function to calculate target statistics for degree and degrange terms
deg_target_stats <- 
  function(N_nodes, deg_dist){
  
  # target statistics
  data.frame(deg_dist*N_nodes) %>% rownames_to_column(var= "deg_range_5cat")
  
  
}


# function running all steps
node_attrib_target_pop <- 
  function(netstats, pct_target_pop){
    
    # Total number of rural and urban target populations
    tar_pop_size_rural = 117808
    tar_pop_size_urban = 257977
    
    # Define the total number of nodes of modeled population to be a percentage of the target population 
    n_node_rural=round(tar_pop_size_rural*pct_target_pop); n_node_urban=round(tar_pop_size_urban*pct_target_pop) 
    
    # Target population numbers in urban & rural networks 
    target_age_distribut <- data.frame(target_age_grp=rep(target_age_grp,2),
                                       dss_pop_age_grp=c(c(199+750+933,1017+958,1196+1195,1309+1272,1215+1088+1067+876,777+626+499+321+437), # number of population in the rural area from DSS
                                                         c(309+1144+1458,1474+1814,1805+1731,1740+1669,1471+1395+1206+975,891+572+412+236+245)  # number of population in the urban area from DSS
                                       ),
                                       total_pop = rep(c(n_node_rural, n_node_urban), each = length(target_age_grp)),   # Total population in the rural and urban area
                                       network=rep(c("rural", "urban"), each = length(target_age_grp))  
    ) %>% 
      group_by(network) %>% # proportion (relative frequency) of target population in each age group by network
      mutate(
        dss_pop = sum(dss_pop_age_grp), # total number of nodes in that network
        prop=dss_pop_age_grp/dss_pop) %>% ungroup() %>% # prop is the relative frequency of age group from the DSS data
      mutate(
        tar_pop = round(total_pop*prop) # we round the target population to integer to as the "n" argument, "number of observation", in runif/rbinom is integer 
      ) %>%  # number of node at each age group of the modeled population
      select(target_age_grp, network, tar_pop ) # variable needed for below analysis
    
    

    
    ################# Generating nodal attributes of age and layer-specific contact status #################
    # Generating age and age group for each node based on distribution of target population
    node.age.grp.rural <- 
      node.age.grp(target_age_dist_site=target_age_distribut %>% filter(network == "rural") %>% select(target_age_grp, tar_pop)
                   
      ) # rural network
    node.age.grp.urban <- 
      node.age.grp(target_age_dist_site=target_age_distribut %>% filter(network == "urban") %>% select(target_age_grp, tar_pop)
                   
      ) # urban network
  
    
    # Calculate post-stratification proportion for each degree type
    ## rural
    dists_r_2d <- 
      post_stratification(
        prop_parti=netstats$formation$formation_stats_rural$degree_related$prop_parti, # age distribution of study participants
        modeled_pop_dta = node.age.grp.rural %>% rename(node.age.grp = target_age_grp) %>% select(node.age.grp), # age groups of nodes for deriving age distributino of modeled population
        degree_2d= netstats$formation$formation_stats_rural$degree_related$contact_count_2d %>% data.frame() # raw contact counts at 2-day scale
      )
    
    
    ## urban
    dists_u_2d <- 
      post_stratification(
        prop_parti=netstats$formation$formation_stats_urban$degree_related$prop_parti,
        modeled_pop_dta = node.age.grp.urban %>% rename(node.age.grp = target_age_grp) %>% select(node.age.grp), 
        degree_2d= netstats$formation$formation_stats_urban$degree_related$contact_count_2d %>% data.frame() # raw contact counts at 2-day scale 
      )
    
    # Generating nodal attribute of contact status at each layer, with proportion of contact specific to each age group
    
    ## Characterize items related to individual-level cross-layer effect
    ### rural
    x_layer_indiv_stat_rural <- 
      assoc_btw_layers(
        degree_2d = netstats$formation$formation_stats_rural$degree_related$contact_count_2d,
        wai = dists_r_2d$w_ai
      )
    ### urban
    x_layer_indiv_stat_urban <- 
      assoc_btw_layers(
        degree_2d = netstats$formation$formation_stats_urban$degree_related$contact_count_2d,
        wai = dists_u_2d$w_ai
      )

    
    ## generate nodal attribute of binary contact status for the x-layer effect
    node.contact.layer.rural <- 
      node.layer.contact(deg.age.layer.dist_2day = x_layer_indiv_stat_rural$deg.age.layer.dist_2day, #  proportion of having contact based on the post-stratified degree
                         target_age_dist = target_age_distribut %>% filter( network == "rural") %>% select(target_age_grp, tar_pop) # tar_pop is the number of modeled population
      ) # rural network
    node.contact.layer.urban <- 
      node.layer.contact(deg.age.layer.dist_2day = x_layer_indiv_stat_urban$deg.age.layer.dist_2day, 
                         target_age_dist = target_age_distribut %>% filter( network == "urban") %>% select(target_age_grp, tar_pop)
      ) # urban network
    
    # Merging nodal status of age and layer-specific contact
    node.attr.rural <- 
      cbind(node.age.grp.rural %>% select(-age.grp.num), # we can safely use cbind here as the two attributes are generated from the same number of observations.
            node.contact.layer.rural %>% select(-target_age_grp)
      ) %>% 
      rename(node.age = age, node.age.grp = target_age_grp)
    
    node.attr.urban <- 
      cbind(node.age.grp.urban %>% select(-age.grp.num),
            node.contact.layer.urban %>% select(-target_age_grp)
      ) %>% 
      rename(node.age = age, node.age.grp = target_age_grp)
    
    

    ############## Target statistics (age.grp) ##############
    # Characterizing target stats of the rural network
    targetstats_age.grp <- list()
    targetstats_age.grp$formation_stats_rural <- 
      target_stats_age(form_stat = netstats$formation$formation_stats_rural, 
                       target_age_dist = target_age_distribut %>% filter(network == "rural") %>% select(target_age_grp, tar_pop)
      )
    
    # Characterizing target stats of the urban network
    targetstats_age.grp$formation_stats_urban <- 
      target_stats_age(form_stat = netstats$formation$formation_stats_urban, 
                       target_age_dist = target_age_distribut %>% filter(network == "urban")%>% select(target_age_grp, tar_pop)
      ) 
    

    
    ############## Target statistics (degree distribution) ##############

    # target statistics for degree and degrange
    ## rural
    deg_tstat_r <- 
      deg_target_stats(
        N_nodes = n_node_rural,
        deg_dist = dists_r_2d$deg_proportion_rescale$deg_proportion_w_rescale)
    
    ## urban
    deg_tstat_u <- 
      deg_target_stats(
        N_nodes = n_node_urban,
        deg_dist =  dists_u_2d$deg_proportion_rescale$deg_proportion_w_rescale)
    
    # proportion of having and not having contact calculated from the post-stratified degree
    
    
    ############## Target statistics (cross-layer effects) ##############
    nf.x.layer <- list()
    

    # target statistics
    ## Note - we adjust for the cross-layer effect between school and work and vice versa for the modeling but all effects are outputted here
    ## rural
    nf.x.layer$rural <- 
      target_stats_x_layer(
        x_layer_items = x_layer_indiv_stat_rural, # the inputs are all based on the post-stratified single-day contact count
        N=n_node_rural
      )
    ## urban
    nf.x.layer$urban <- 
      target_stats_x_layer(
        x_layer_items = x_layer_indiv_stat_urban,
        N=n_node_urban
      )
    
    
    ############## Mean degrees for simulating nodal attributes of household identifiers #################
    # calculate mean degree at home from the edge-count matrix
    ## rural
    mean_deg_rural_home = 2*sum(targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home)/n_node_rural
    ## urban
    mean_deg_urban_home = 2*sum(targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Home)/n_node_urban
    
    
    # Gathering things for output
    output <- list()
    
    # Nodal attribute
    ## nodal continuous age, age group (6- and 3-category), contact status of each layer, and household ID 
    output$attr$rural <- node.attr.rural %>%
      mutate(node.ids = row_number()) %>%  # adding a node.id, defined as the row number to keep track of the nodes for the latter workflows
      relocate(node.ids, .before = everything())
    
    output$attr$urban <- node.attr.urban %>%
      mutate(node.ids = row_number()) %>% 
      relocate(node.ids, .before = everything())
    
    # Target stats - we only export the edge counts for nodemix and the x-layer effects, as only them will be used for building the model
    ## selecting variable in edge doesn't needing output
    targetstats_age.grp$formation_stats_rural$edge <- NULL

    
    targetstats_age.grp$formation_stats_urban$edge <- NULL
    
    
    ## selecting variable in nodefactor(age.grp) dosn't needing output 
    targetstats_age.grp$formation_stats_rural$nf.age.grp <- NULL
    
    
    targetstats_age.grp$formation_stats_urban$nf.age.grp <- NULL

    
    
    ## nodemix(age.grp)
    ### Note: for school and work layers of both networks, the edge counts of some older and younger age groups were recoded to 0. Please refer to notes in the target_stats_age function for details
    
    ## nodematch(age.grp)
    ### Note: since the assortative edge counts in nodemix are the same as nodematch, we exclude the target statistics of nodemath from the final output
    targetstats_age.grp$formation_stats_rural$nm.age.grp <- targetstats_age.grp$formation_stats_urban$nm.age.grp <- 
      targetstats_age.grp$formation_stats_rural$nm.age.grp.sum <-  targetstats_age.grp$formation_stats_urban$nm.age.grp.sum <- NULL
    
    output$targetstats_age.grp <- targetstats_age.grp
    
    ## cross-layer effects
    output$targetstats_x.layer <- nf.x.layer
    ### individual contact status
    output$participant_contact_layer$rural <-  x_layer_indiv_stat_rural$data_wide;  output$participant_contact_layer$urban <-  x_layer_indiv_stat_urban$data_wide
    
    ## degree distribution
    output$degrange$rural <- deg_tstat_r; output$degrange$urban <- deg_tstat_u
    
    # home mean degrees for household assignment
    output$mean_deg_home$Rural <-  mean_deg_rural_home
    output$mean_deg_home$Urban  <- mean_deg_urban_home

   
    output
  }
