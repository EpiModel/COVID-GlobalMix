# Evaluating the nodemix term based on ergm.ego's approach
## Load mesa data for calculation
library("ergm.ego")
data(faux.mesa.high)
mesa.ego<- as.egor(faux.mesa.high)

## Load target and summary statistics from GlobalMix data to compare the edge count of matched edges by nodefactors based on ARTnet and ergm.ego approaches
attri_tarstats <- readRDS("data/network_params/network_targetstats.RData") # target statistics
netstats <- readRDS("data/network_params/network_params.RData") # summary statistics


## The below data are retrieved from - https://statnet.org/workshop-ergm-ego/ergm.ego_tutorial.html
edge_ct_mx <- # total number of edges in each age group of the target population
  # c( # from "mixingmatrix(mesa.ego,"Grade")"
  #   150,0,0,1,1,1, # sum of this row = 153, for grp 7
  #   0,66,2,4,2,1, # sum of this row = 75, for grp 8
  #   0,2,46,7,6,4, # sum of this row = 65, for grp 9
  #   1,4,7,18,1,5, # sum of this row = 36, for grp 10
  #   1,2,6,1,34,5, # sum of this row = 49, for grp 11
  #   1,1,4,5,5,12 # sum of this row = 28, for grp 12
  # ) %>% matrix(., 6,6)# same as the output from "sim.full <- simulate(fit.full)"
    mixingmatrix(mesa.ego,
                         "Grade")



## row proportion matrix
mx_prop <- 
  # c(
  #   0.98,0.00,0.00,0.01,0.01,0.01,
  #   0.00,0.88,0.03,0.05,0.03,0.01,
  #   0.00,0.03,0.71,0.11,0.09,0.06,
  #   0.03,0.11,0.19,0.50,0.03,0.14,
  #   0.02,0.04,0.12,0.02,0.69,0.10,
  #   0.04,0.04,0.14,0.18,0.18,0.43
  # ) %>% matrix(., 6,6)
   round(mixingmatrix(mesa.ego, "Grade", rowprob=T), 5)

## comparing target statistics between nodefactor and nodemix for "Grade".
### total number of nodes between edges
nf_tar <- edge_ct_mx %>% rowSums() 
nf_tar
#### Interpretation: these result of the total number of edge in a group by nodemix is exactly the same as the target statistics of nodefactor - 
#### suggesting if all the all the nodemix terms are used, the target stats for nodefact could be obviated.

## comparing target statistics between (nodefactor/2, summary statistics) and nodematch for "Grade".
### In "round(mixingmatrix(mesa.ego, "Grade", rowprob=T), 2), the probabilities of assortative mixing of Grades 7 to 12 are 
match_prop <- mx_prop %>% diag()
match_prop
## Calculating target statistics for edge using nf_tar
### Approach 1 - calculating edge statistics based on nodefactor statistics
(nf_tar/2) %>% sum # equals to 203 in edges, suggesting ergm.ego also calculates the total number of edges from nodefactor.
### Approach 2 - calculating edge statistics based on directing summing all edges in the ergm.ego matrix
edge_ct_mx %>% sum/2

## The total number of matched edges, non-differential, is
nmatch_tar <- sum((nf_tar/2)*match_prop)
nmatch_tar # same as "sim.full <- simulate(fit.full)"==163
edge_ct_mx %>% diag()/2 # the sum of these is the same as nmatch_tar
#### Interpretation: The ergm.ego's approach to calculate nodematch target statistics is based on the nodefactor statistics, calculated from the edge-count matrix, but not based on the nodefactor statis, calculated from the egocentric mean deg.
#### The matched edges in the matrix is counted twice


## Investigating calculation for edge counts for assortative mixing in Global Mix
### Note: In "attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp", contain the nodefactor target statistics stratified by age, we have target statistics
### calculated based on multiplying the mean deg of study participants with N (nf.ag.ego), and those calculated by summing all edge counts of a age group in a edge-count matrix (nf.ag)
### For calculating the assortative edge counts, Bill think using nf.ag.ego would be better, as it better correspond with the observed mixing proportion. While the ergm.ego use nf.ag, 
### as seen in the calculation above, or nf.ag and nf.ag.ego in ergm.ego are the same, but we cannot assess this given we don't have the summary statistics of ergm.ego.

### see how will the nodematch term be like when using nf.ag for the GM data
(
  attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp %>% filter(contact_location == "School") %>% pull(nf.ag)/2
)*
  netstats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% as.matrix() %>%  diag() #  309.72358 1023.72835   25.74787   11.51628         0         0

#### compare the above vector with those by nf.ag.ego
(
  attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp %>% filter(contact_location == "School") %>% pull(nf.ag.ego)/2
)*
  netstats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% as.matrix() %>%  diag()


### Compare the matched edge count with total amount of edges
attri_tarstats$targetstats_age.grp$formation_stats_rural$edge %>% filter(contact_location == "School") ## total edges - "edges" is calculated based on nf.ag; "edges.artnet" is based on nf.ag.ego
#### Note: the total edges are 2327.882, which is higher than 1985. However, the model still fail when 1985 is included.



## the following investigation is to assess whether the function billy proposed for nodemix is used in ergm.ego
### defining a function using either the CorporateMix or Billy's aprroach to calculate the edge count of a mixing pattern
edge_ct_test <- function(asymmetric_mix_matrix., nf.ag_layer., edge_ct_type){
  # grp.a and grp.b are the two age groups at each side of the edge, asymmetric_mix_matrix. is the mixing matrix with bidirectional mixing proportion (i.e., mixing between grp a and grp b and grp b and grp a),
  # nf.ag_layer. is the total number of nodes in an age group of a contact layer 
  # grp 1, 2, 3, 4, 5, 6 respectively correspond to the youngest to oldest age groups
if (edge_ct_type == "billy"){
mix_edge_num <- 
  function(grp.a, grp.b, asymmetric_mix_matrix., nf.ag_layer.){ 

    sum(
      asymmetric_mix_matrix.[grp.a,grp.b]* # grp.a as egocentric node, grp.b as contact
        (nf.ag_layer.[grp.a]) /2,# the reason /2 is used here is because this is a edge-level statistic
      asymmetric_mix_matrix.[grp.b,grp.a]* # grp.b as egocentric node, grp.a as contact
        (nf.ag_layer.[grp.b])/2,
      na.rm = T #  for an egocentric age group that do not have contact with another age group (i.e., mixing proportion == NA), we consider the corresponding number of edges ==0
    )
  }
}else{
  mix_edge_num <- 
    function(grp.a, grp.b, asymmetric_mix_matrix., nf.ag_layer.){ 
      
      # average prop * total edges in nodes of each side of ties.
      (
        (asymmetric_mix_matrix.[grp.a,grp.b]+asymmetric_mix_matrix.[grp.b,grp.a])/2
      )*
        (
          (nf.ag_layer.[grp.a]) /2
          +
            (nf.ag_layer.[grp.b])/2
        )
      
    }
}


## Filling the edges to the upper trianguar matrix
edge_ct_mx_1 <-  
  matrix(NA, 6, 6)

  ### non-assortative mixing of 0-9y (grp 1) w/the other age.grps
  for (j in 2:6) {
    edge_ct_mx_1[1,j] <- 
      mix_edge_num(grp.a =1, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf_tar)
  }
  
  ### non-assortative mixing of 10-19y (grp 2) w/the other age.grps
  for (j in 3:6) {
    edge_ct_mx_1[2,j] <- 
      mix_edge_num(grp.a =2, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf_tar)
  }
  
  ### non-assortative mixing of 20-29y (grp 3) w/the other age.grps
  for (j in 4:6) {
    edge_ct_mx_1[3,j] <- 
      mix_edge_num(grp.a =3, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf_tar)
  }
  
  ### non-assortative mixing of 30-39y (grp 4) w/the other age.grps
  for (j in 5:6) {
    edge_ct_mx_1[4,j] <- 
      mix_edge_num(grp.a =4, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf_tar)
  }
  
  ### non-assortative mixing of 50-59y (grp 5) w/the other age.grps
  edge_ct_mx_1[5,6] <- 
    mix_edge_num(grp.a =5, grp.b = 6, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf_tar)
  
  
  ## Calculating number of diagonal mixing edges of each mixing pattern - assortative mixing
  for (j in 1:6) {
    edge_ct_mx_1[j,j] <-
     ((nf_tar/2)[j])* # the reason /2 is used here is because this is a edge-level statistic
      mx_prop[j,j]
  }
  
  edge_ct_mx_1  %>% round(., 0)
}
  
edge_ct_billy <- 
edge_ct_test(asymmetric_mix_matrix. =  mx_prop,
             nf.ag_layer.=nf_tar, # total edges in ergm.ego's edge count matrix
             edge_ct_type = "billy")


edge_ct_cm <- 
edge_ct_test(asymmetric_mix_matrix. =  mx_prop,
             nf.ag_layer.=nf_tar,
             edge_ct_type = "cm")

 

edge_ct_billy -edge_ct_mx ## there's no difference between the edge count by Billy's approach and ergm.ego
edge_ct_cm -edge_ct_mx ##  there's difference between the edge count by CorporateMix's approach and ergm.ego
 
### Interpretation: we will use the ergm.ego approach to calculate the nodefactor (summing all values in cells of an age group) and then multiply them 
### with the mixing proportion to calculate the nodemix terms. 
 




 
 