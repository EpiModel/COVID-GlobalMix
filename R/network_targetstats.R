
source("R/network_params.R")

# Characterization of target statistics and model parameterization
library("tidyverse")
library("EpiModel")
library("ggpubr")
library("knitr")
library("svglite")
library("kableExtra")

# Specifying which context to use - local or HPC
context = "local"


# load network parameters
netstats <- readRDS("data/network_params/network_params.RData")

if (context == "local") {
  n_node_rural=round(117808/10); n_node_urban=round(257977/10) # 10% of total observed numbers of populations in rural and urban sites in India
} else if (context == "hpc") {
  n_node_rural=117808; n_node_urban=257977 # total observed numbers of populations in rural and urban sites in India
} else  {
  stop("The `context` variable must be set to either 'local' or 'hpc'")
}


# Categories of age and layer variabes
target_age_grp <- netstats$formation$formation_stats_rural$edge_node_factor_match_rural$nf.age.grp$participant_age %>% unique()%>% factor() # the six age group
layers <-  netstats$formation$formation_stats_rural$edge_node_factor_match_rural$edge$contact_location %>% unique()

# target population numbers in urban & rural networks 
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
  mutate(#tar_pop = round(prop*n_node)
    tar_pop = total_pop*prop
         ) %>%  # number of node at each age group of the modeled population
  select(target_age_grp, network, tar_pop ) # variable needed for below

################# Simulate age and age group for individual nodes #################
# Function generating age and age group for each node based on distribution of target population
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

node.age.grp.rural <- 
  node.age.grp(target_age_dist_site=target_age_distribut %>% filter(network == "rural") %>% select(target_age_grp, tar_pop)
               
               ) # rural network
node.age.grp.urban <- 
  node.age.grp(target_age_dist_site=target_age_distribut %>% filter(network == "urban") %>% select(target_age_grp, tar_pop)
               
               ) # urban network


################# Simulate contact status at each layer for individual nodes #################
# Function generating nodal attribute of contact status at each layer
node.layer.contact <- function(deg.age.layer.dist_2days, target_age_dist, node.age.group){
  
  ## Extracting proportion of having any contact
  deg.layer.prop <- # deg.layer.prop stores the proportion w/ any contact at each age group
    deg.age.layer.dist_2days %>% 
    filter(contact_status == 1 # filter out the proportion of having any contact
    ) %>% select(-contact_status) %>% pivot_longer(!layer, names_to = "age.grp", values_to = "gt_0_prop") # converting to long format to facilitate data manipulation

  
  layer_attribute_single_layer   <-  data.frame() # create dataframe to store intermediate results
  layer_attribute_layers <- list() # create list to store nodeal status of contact for a whole layer
  
  ## generate nodal attribute of contact  at each layer (i), at each age group (j)
  ### explanation of the below code: for a layer, we generate nodel status of contact for each age group 
  ### using the proportion of contact and the total number of nodes in a layer. We replicate this for all age group and all layers
  for (i in 1:length(layers)
  ) {
    for (j in 1:length(target_age_grp)) {
      deg.prop_single_layer_age_grp <-  deg.layer.prop %>% filter(layer == layers[i] & age.grp == target_age_grp[j]) %>% pull(gt_0_prop) # retrieving the proportion of having any contact in a age group of a layer in a network
      
      n_pop_single <- target_age_dist %>% filter(target_age_grp == target_age_grp[j]) %>% pull(tar_pop) # number of nodes to generate 
      
      contact_attribute <- # attribute of contact in a single age group and layer
        rbinom(n =  n_pop_single, 
               size = 1,#  for bernoulli trial
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

node.age.grp.rural <- 
node.layer.contact(deg.age.layer.dist_2days = netstats$formation$formation_stats_rural$layer_assoc_rural$deg.age.layer.dist_2days, 
                   target_age_dist = target_age_distribut %>% filter( network == "rural"), 
                   node.age.group = node.age.grp.rural) # rural network

node.age.grp.urban <- 
  node.layer.contact(deg.age.layer.dist_2days = netstats$formation$formation_stats_urban$layer_assoc_urban$deg.age.layer.dist_2days, 
                     target_age_dist = target_age_distribut %>% filter( network == "urban"), 
                     node.age.group = node.age.grp.urban) # urban network

# Compare simulated proportion to observed ones
## Observed proportion, rural
netstats$formation$formation_stats_rural$layer_assoc_rural$deg.age.layer.dist_2days %>% filter(contact_status ==1) %>% mutate_if(is.numeric, round, 2)%>% 
  select("layer", "0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"   ) %>% t() %>% data.frame() 

## Simulated proportion, rural
node.age.grp.rural  %>% group_by(target_age_grp) %>% 
select(contact_attribute_Home, contact_attribute_School, contact_attribute_Work, contact_attribute_Nonhome
)%>%  summarize(Home= mean(contact_attribute_Home), 
                School= mean(contact_attribute_School), 
                Work= mean(contact_attribute_Work), 
                Nonhome= mean(contact_attribute_Nonhome)
                ) %>% arrange(target_age_grp) %>% 
  mutate_if(is.numeric, round, 2)




############## Target statistics (age.grp) ##############
# function to calculate formation target stats related to age
target_stats_age <- 
  function(form_stat, 
           target_age_dist
  ){
    
    # Note form_stat[[1]] and form_stat[[2]] respectively are the edge_node_factor_match and mix_prop
    # We first characterize the target statistics using the ARTnet approach. Then, using the matrix edge count,
    # we use the ergm.ego approach to characterize the same type of target statistics.
    
    # Number of nodes between edges per age group, node-level, for nodefactor, by the approach used in ARTnet
    form_stat[[1]]$nf.age.grp <- 
      form_stat[[1]]$nf.age.grp %>% 
      left_join(
        target_age_dist %>% 
          rename(participant_age=target_age_grp) , 
        by = "participant_age"
      ) %>% mutate(nf.ag.ego = single_day_nf_md*tar_pop # number of edges in each age group = md of each age group * number of node of each age group
      ) 
    
    
    
    # Total Edges, edge-level, for edge, by the approach used in ARTnet
    form_stat[[1]]$edge <-
      form_stat[[1]]$edge %>%
      mutate(edges=single_day_md/2*sum(target_age_dist$tar_pop) # the crude total number of edges = overall MD/2 * total population across all age groups in a network
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
        nm.ag.artnet= (nf.ag.ego/2)*single_day_nm_md # adapted from ARTnet: number of match edge in each age group = (number of nodes in each age group /2) * prop of matched nodes, the reason 2 is here is because this is an edge-level statistic
      )
    
    
    ## nodematch(diff=F)
    form_stat[[1]]$nm.age.grp.sum <- 
      form_stat[[1]]$nm.age.grp %>% group_by( contact_location) %>% summarize(nm.ag.sum.artnet = sum(nm.ag.artnet)
      )
    
    
    #  Number of edges of a specific age-mixing pattern, edge-level, for node mix - this also forms a matrix for edge count
    
    ## Define a function for the calculation of total number of edges for a single age mixing pattern
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
    
    
    ## For school and work layers, zeroing out ties in the upper triangular matrix
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
  ) # observation: at the school layer, the both the summary and target stat for the mixing in 20-29y ==0. 



############## Target statistics (cross-layer effects) ##############
# Function characterizing target statistics for the cross-layer effect
target_stats_x_layer <- 
  function(
    x_layer_items,
    N # number of population a whole network
    ){
    
    # Load things needed for characterizing the cross-layer target statistics
    prop_contact <- x_layer_items$deg.layer.dist_2days # proportion of having any contact at a 2-day scale
    cond_mean_deg <- x_layer_items$mean_deg_1day # conditioned single-day degree
    coefficient <- # Regression result for the cross-layer effects
      x_layer_items$coefficient_summary_2days %>% data.frame() %>% 
      tibble::rownames_to_column(var="association") %>% rename(coefficient=2, p_value =5) %>% 
      select(association, coefficient, p_value)
   
    
    # Characterize number of node w/o and w/ contact at each layer
    ## Home
    N_h_0=N*prop_contact %>% filter(layer=="Home") %>% pull(prop_0) # number of nodes in the Home layer as conditioning layer w/o contact
    N_h_1=N*prop_contact %>% filter(layer=="Home") %>% pull(prop_1) # number of nodes in the Home layer as conditioning layer w/o contact
    
    ## School
    N_s_0=N*prop_contact %>% filter(layer=="School") %>% pull(prop_0) # number of nodes in the School layer as conditioning layer w/o contact
    N_s_1=N*prop_contact %>% filter(layer=="School") %>% pull(prop_1) # number of nodes in the School layer as conditioning layer w/o contact
    
    ## Work
    N_w_0=N*prop_contact %>% filter(layer=="Work") %>% pull(prop_0) # number of nodes in the Work layer as conditioning layer w/o contact
    N_w_1=N*prop_contact %>% filter(layer=="Work") %>% pull(prop_1) # number of nodes in the Work layer as conditioning layer w/o contact
    
    ## Nonhome
    N_nh_0=N*prop_contact %>% filter(layer=="Nonhome") %>% pull(prop_0) # number of nodes in the Nonhome layer as conditioning layer w/o contact
    N_nh_1=N*prop_contact %>% filter(layer=="Nonhome") %>% pull(prop_1) # number of nodes in the Nonhome layer as conditioning layer w/o contact
    
    # Characterize conditioned node-level edge count
    cond_mean_deg <- 
    cond_mean_deg %>% 
      mutate(
        nf_other_layer_0 = # target stats for without contact at the conditioning layer
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
    cond_mean_deg %>% 
      mutate(
        coeffi = coefficient %>% pull(coefficient),
        p_value = coefficient %>% pull(p_value)
               )
    
  }

nf.x.layer <- list()
nf.x.layer$rural <- 
  target_stats_x_layer(x_layer_items = 
                         netstats$formation$formation_stats_rural$layer_assoc_rural, 
                       N = n_node_rural
  ) 
nf.x.layer$urban <- 
  target_stats_x_layer(x_layer_items = 
                         netstats$formation$formation_stats_urban$layer_assoc_urban, 
                       N = n_node_urban
  ) 
## Note - we adjust for the cross-layer effect between school and work and vice versa


# Gathering things for output
output <- list()

# Nodal attribute
## nodal age group and contact status of each layer
output$attr$rural <- node.age.grp.rural
output$attr$urban <- node.age.grp.urban

# Target stats
## selecting variable in edge needing output
targetstats_age.grp$formation_stats_rural$edge <- 
  targetstats_age.grp$formation_stats_rural$edge %>% 
  select(contact_location, edges.artnet, edges)

targetstats_age.grp$formation_stats_urban$edge <- 
  targetstats_age.grp$formation_stats_urban$edge %>% 
  select(contact_location, edges.artnet, edges)

 
## selecting variable in nodefactor(age.grp) needing output 
targetstats_age.grp$formation_stats_rural$nf.age.grp <- 
  targetstats_age.grp$formation_stats_rural$nf.age.grp %>% 
  select(participant_age, contact_location, nf.ag.ego, nf.ag) %>% 
  rename(age.grp=participant_age) # renaming this variable as the output is for the population to be modeled

targetstats_age.grp$formation_stats_urban$nf.age.grp <- 
  targetstats_age.grp$formation_stats_urban$nf.age.grp %>% 
  select(participant_age, contact_location, nf.ag.ego, nf.ag) %>% 
  rename(age.grp=participant_age) # renaming this variable as the output is for the population to be modeled


## nodemix(age.grp)
### Note; for urban work layer, the edge count of 10-19y of the assortative mixing in the edge-count matrix has been re-coded to zero, along with the other edge counts of 10-19y.

## nodematch(age.grp)
### Note: since the assortative edge counts in nodemix are the same as nodematch, we exclude the target statistics of nodemath from the final output
targetstats_age.grp$formation_stats_rural$nm.age.grp <- targetstats_age.grp$formation_stats_urban$nm.age.grp <- 
  targetstats_age.grp$formation_stats_rural$nm.age.grp.sum <-  targetstats_age.grp$formation_stats_urban$nm.age.grp.sum <- NULL


output$targetstats_age.grp <- targetstats_age.grp
output$targetstats_x.layer <- nf.x.layer


 

saveRDS(output, file = "data/network_params/network_targetstats.RData")

