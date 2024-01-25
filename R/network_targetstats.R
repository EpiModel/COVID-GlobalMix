# Characterization of target statistics and model parameterization
lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)



# load network parameters
netstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_params.RData")

# to-do: n_node=1e4 - we had this as a scaler of the population ( mutate(tar_pop = round(prop*n_node)) ) but think if the population of DSS is not a sample of the population in the study area, this scaler can be obviated. This'll be confirmed on 1/22

# Categories of age and layer variabes
target_age_grp <- netstats$formation$formation_stats_rural$edge_node_factor_match_rural$nf.age.grp$participant_age %>% unique()%>% factor() # the six age group
layers <-  netstats$formation$formation_stats_rural$edge_node_factor_match_rural$edge$contact_location %>% unique()

# target population numbers in urban & rural networks 
target_age_distribut <- data.frame(target_age_grp=rep(target_age_grp,2),
                                   pop_age_grp=c(c(199+750+933,1017+958,1196+1195,1309+1272,1215+1088+1067+876,777+626+499+321+437), # number of population in the rural network from DSS
                                               c(309+1144+1458,1474+1814,1805+1731,1740+1669,1471+1395+1206+975,891+572+412+236+245)  # number of population in the urban network from DSS
                                              ),
                                   network=rep(c("rural", "urban"), each = length(target_age_grp))
) %>% 
  group_by(network) %>% # proportion (relative frequency) of target population in each age group by network
  mutate(
    network_pop = sum(pop_age_grp), # total number of nodes in that network
    prop=pop_age_grp/network_pop) %>% ungroup() %>% 
  mutate(#tar_pop = round(prop*n_node)
    tar_pop = pop_age_grp
         ) # number of node at each age group of the modeled population


################# Simulate age and age group for individual nodes #################
# Function generating nodal's age and age group based on distribution of target population
node.age.grp <- 
  function(target_age_dist_site # number of target population by age group of a network (urban/rual)
  ){
    
    ## assign numeric code to each age group in order
    age.grp.df <- 
      target_age_dist_site %>% rownames_to_column() %>% 
      rename(age.grp.num=rowname) 
    
    ## generate individual nodes labeled by age group 
    age.grp.num <- age.grp.df %>% 
      slice(rep(1:n(), times= tar_pop) # 1:6 correspond to the 6 age groups from yound to told 
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


# Visualizing age distribution of study population
# ggarrange(
#   target_age_distribut %>% ggplot(aes(x=target_age_grp, y=pop_age_grp))+geom_bar(stat = "identity")+facet_wrap(~network)+
#     geom_text(aes(label=pop_age_grp), vjust=-0.3, size=3.5)+
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

################# Simulate contact status at each layer for individual nodes #################
# Function generating nodal attribute of contact status at each layer
node.layer.contact <- function(deg.age.layer.dist_2days, target_age_dist, node.age.group){
  
  deg.layer.prop <- 
    deg.age.layer.dist_2days %>% # dataframe storing relative frequency population w/ and w/o contact at each age group
    filter(contact_status == 1 # filter out the proportion of having any contact
    ) %>% select(-contact_status) %>% pivot_longer(!layer, names_to = "age.grp", values_to = "gt_0_prop") # converting to long format to facilitate data manipulation
  
  
  deg.layer.prop <- # merging total number of node of each age group to each layer
    left_join(deg.layer.prop, 
              target_age_dist %>% 
                rename(age.grp = target_age_grp) %>% select(-c(pop_age_grp, prop)) , 
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


# Compare simulated proportion to observed ones
## Observed proportion, rural
netstats$formation$formation_stats_rural$layer_assoc_rural$deg.age.layer.dist_2days %>% filter(contact_status ==1) %>% select(-contact_status) %>% t()
## Simulated proportion, rural
node.age.grp.rural  %>% group_by(target_age_grp) %>% 
select(contact_attribute_Home, contact_attribute_School, contact_attribute_Work, contact_attribute_Nonhome
)%>%  summarize(mean(contact_attribute_Home), 
                                              mean(contact_attribute_School), 
                                              mean(contact_attribute_Work), 
                                              mean(contact_attribute_Nonhome)
                ) %>% arrange(target_age_grp) 







############## Target statistics (age.grp) ##############
# function to calculate formation target stats related to age
target_stats_age <- 
  function(form_stat, 
           target_age_dist
  ){
    
    # Note form_stat[[1]] and form_stat[[2]] respectively are the edge_node_factor_match and mix_prop
    
    # Number of edges per age group, node-level, for nodefactor
    form_stat[[1]]$nf.age.grp <- 
      form_stat[[1]]$nf.age.grp %>% 
      left_join(
        target_age_dist %>% 
          rename(participant_age=target_age_grp) , 
        by = "participant_age"
      ) %>% mutate(nf.ag = single_day_nf_md*tar_pop # number of edges in each age group = md of each age group * number of node of each age group
      ) 
    
    
    
    # Total Edges, edge-level, for edge
    form_stat[[1]]$edge <- 
      form_stat[[1]]$edge %>% 
      mutate(edges=single_day_md/2*sum(target_age_dist$tar_pop) # the crude total number of edges = overall MD/2 * total population across all age groups in a network
      ) %>% # the reason /2 is used here is because this is a edge-level statistic, this way didn't adjust for the age distribution of the target population.
      left_join( 
        form_stat[[1]]$nf.age.grp %>% group_by(contact_location) %>% summarize(edges_adj_age=sum(nf.ag)/2 # total number of edges adjusting for population age distribution, the reason 2 is in the denominator is the because this is an edge-level statistics
        ),
        by = "contact_location"
      ) %>% select(-edges) # given we decided to go with the total number edges adjust for age distribution of target population, we exclude this variable  
    
  
    #  Number of matched edges in the same age group, edge-level, for node match
    ## nodematch(diff=T)
    form_stat[[1]]$nm.age.grp <- 
      form_stat[[1]]$nm.age.grp %>% 
      left_join(form_stat[[1]]$nf.age.grp %>% select(participant_age, contact_location, nf.ag), by = c("participant_age", "contact_location")  # number of nodes in age group
      ) %>% 
      mutate(
        nm.ag= (nf.ag/2)*single_day_nm_md # adapted from ARTnet: number of match edge in each age group = (number of nodes in each age group /2) * prop of matched nodes, the reason 2 is here is because this is an edge-level statistic
      )
    
    
    ## nodematch(diff=F)
    form_stat[[1]]$nm.age.grp.sum <- 
      form_stat[[1]]$nm.age.grp %>% group_by( contact_location) %>% summarize(nm_age.grp.sum = sum(nm.ag)
      )
    
    
    #  Number of edges of a specific age-mixing pattern, edge-level, for node mix
    
    ## Define a function for the calculation of total number of edges for a single mixing pattern
    mix_edge_num <- 
      function(grp.a, grp.b, asymmetric_mix_matrix., nf.ag_layer.){ 
        # grp.a and grp.b are the two contacting age groups , asymmetric_mix_matrix. is the mixing matrix with bidirectional mixing proportion (i.e., mixing between grp a and grp b and grp b and grp a),
        # nf.ag_layer. is the total number of nodes in an age group of a contact layer 
        # grp 1, 2, 3, 4, 5, 6 respectively correspond to the youngest to oldest age groups
       sum(
         asymmetric_mix_matrix.[grp.a,grp.b]* # grp.a as egocentric node, grp.b as contact
          nf.ag_layer. %>% filter(participant_age ==target_age_grp[grp.a]) %>% pull(nf.ag)/2,# the reason /2 is used here is because this is a edge-level statistic
          asymmetric_mix_matrix.[grp.b,grp.a]* # grp.b as egocentric node, grp.a as contact
          nf.ag_layer. %>% filter(participant_age ==target_age_grp[grp.b]) %>% pull(nf.ag)/2,
         na.rm = T #  for an egocentric age group that do not have contact with another age group (i.e., mixing proportion == NA), we consider the corresponding number of edges ==0
       )
      }
    
    
    ## Calculate nodemix target statistics for each layer i
    symmetric_mix_matrix <- list() # create a list to store target stats for all layers
    for (i in 1:length(layers)
         ) {
      
      nf.ag_layer <- # getting the target statistics for nodefactor(age.grp)
        form_stat[[1]]$nf.age.grp %>% 
        select(participant_age, contact_location, nf.ag) %>% 
        filter(contact_location == layers[i])
      
      asymmetric_mix_matrix <- # getting the asymmetric mixing matrix
        form_stat[[2]][[layers[i]]][1][[1]] # "[1]" is to retrieve the glm-based proportions, [[1]] is to extract data frame from list 
      
      
      ## Calculating number of non-diagonal mixing edges of each mixing pattern - non-assortative mixing
      symmetric_mix_matrix_i <- matrix(NA, 6, 6) %>% data.frame()# create a symmetric matrix to store result
      rownames(symmetric_mix_matrix_i) <- colnames(symmetric_mix_matrix_i) <- target_age_grp
      
      
      ### non-assortative mixing of 0-9y (grp 1) w/the other age.grps
      for (j in 2:6) {
        symmetric_mix_matrix_i[1,j] <- 
          mix_edge_num(grp.a =1, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 10-19y (grp 2) w/the other age.grps
      for (j in 3:6) {
        symmetric_mix_matrix_i[2,j] <- 
          mix_edge_num(grp.a =2, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 20-29y (grp 3) w/the other age.grps
      for (j in 4:6) {
        symmetric_mix_matrix_i[3,j] <- 
          mix_edge_num(grp.a =3, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 30-39y (grp 4) w/the other age.grps
      for (j in 5:6) {
        symmetric_mix_matrix_i[4,j] <- 
          mix_edge_num(grp.a =4, grp.b = j, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      }
      
      ### non-assortative mixing of 50-59y (grp 5) w/the other age.grps
      symmetric_mix_matrix_i[5,6] <- 
        mix_edge_num(grp.a =5, grp.b = 6, asymmetric_mix_matrix. =  asymmetric_mix_matrix, nf.ag_layer.=nf.ag_layer)
      
      
      ## Calculating number of diagonal mixing edges of each mixing pattern - assortative mixing
      for (j in 1:6) {
        symmetric_mix_matrix_i[j,j] <-
          nf.ag_layer %>% filter(participant_age ==target_age_grp[j]) %>% pull(nf.ag)/2* # the reason /2 is used here is because this is a edge-level statistic
            asymmetric_mix_matrix[j,j]
      }
      
      
      symmetric_mix_matrix[[i]] <- symmetric_mix_matrix_i
     
    }
    names(symmetric_mix_matrix) <- layers
    
    form_stat[[2]]$symmetric_mix_matrix <- symmetric_mix_matrix
    
    form_stat
  }



netstats$formation$formation_stats_rural <- 
  target_stats_age(form_stat = netstats$formation$formation_stats_rural, 
                         target_age_dist = target_age_distribut %>% filter(network == "rural") %>% select(target_age_grp, tar_pop)
  )


netstats$formation$formation_stats_urban <- 
  target_stats_age(form_stat = netstats$formation$formation_stats_urban, 
                         target_age_dist = target_age_distribut %>% filter(network == "urban")%>% select(target_age_grp, tar_pop)
  ) # observation: at the school layer, the both the summary and target stat for the mixing in 20-29y ==0. 



# Evaluating target stats
# Note: In the symmetric degree matrix, the NAs of the associative mixing in the school layer are caused by the corresponding egocentric group weren't observed in the data, so its matching ties weren't observed.
### nmix, school layer
netstats$formation$formation_stats_rural$mix_prop_rural_layers$symmetric_mix_matrix %>% lapply(., round)


# Numeric problem in glm-estimated proportion. The the work layer below, the reason we don't se NA is because participants of all age groups were observed, 
# and 2) although the observed matched proportion == 0, the glm approach has numberic problem and yield a non-zero very small number for the proportion.
# It may be misleading to see the degree of this age group >0
#### corresponding summmary stats in nodemix
netstats$formation$formation_stats_rural$mix_prop_rural_layers$Work


## nf
netstats$formation$formation_stats_rural$edge_node_factor_match_rural$nf.age.grp %>% 
  select(participant_age, contact_location, nf.ag ) %>% pivot_wider(names_from = participant_age, values_from = nf.ag)


############## Target statistics (cross-layer effects,  TB completed) ##############




# Gathering things for output

output <- list()

# Nodal attribute
## Age group
output$attr$age.grp$rural <- node.age.grp.rural
output$attr$age.grp$urban <- node.age.grp.urban

# Target stats
## edge
output$target.stats$edges$rural <- 
  netstats$formation$formation_stats_rural$edge_node_factor_match_rural$edge %>% 
  select(contact_location, edges_adj_age)
output$target.stats$edges$urban <- 
  netstats$formation$formation_stats_urban$edge_node_factor_match_urban$edge %>% 
  select(contact_location, edges_adj_age)

## nodefactor(age.grp)
output$target.stats$nf.age.grp$rural <- 
  netstats$formation$formation_stats_rural$edge_node_factor_match_rural$nf.age.grp %>% 
  select(participant_age, contact_location, nf.ag) %>% 
  rename(age.grp=participant_age) # renaming this variable as the output is for the population to be modeled


output$target.stats$nf.age.grp$urban <- 
  netstats$formation$formation_stats_urban$edge_node_factor_match_urban$nf.age.grp %>% 
  select(participant_age, contact_location, nf.ag) %>% 
  rename(age.grp=participant_age) # renaming this variable as the output is for the population to be modeled

## nodemix(age.grp)
output$target.stats$nmix.age.grp$rural <- netstats$formation$formation_stats_rural$mix_prop_rural_layers$symmetric_mix_matrix 
output$target.stats$nmix.age.grp$urban <- netstats$formation$formation_stats_urban$mix_prop_urban_layers$symmetric_mix_matrix 

saveRDS(output, file = "~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_targetstats.RData")
















