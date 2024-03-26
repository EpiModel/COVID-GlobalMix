# Evaluating the nodemix term based on ergm.ego's approach
## Load mesa data for calculation
library("ergm.ego")
data(faux.mesa.high)
faux.mesa.high
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
### total number of nodes between edges - node level
nf_tar_ergmego <- edge_ct_mx %>% rowSums() 
nf_tar_ergmego
#### Interpretation: these result of the total number of edge in a group by nodemix is exactly the same as the target statistics of nodefactor - 
#### suggesting if all the all the nodemix terms are used, the target stats for nodefact could be obviated.


## Calculating target statistics for edge using nf_tar_ergmego
### Approach 1 - calculating edge statistics based on nodefactor (nodelevel) statistics
(nf_tar_ergmego/2) %>% sum # equals to 203 in edges, suggesting ergm.ego also calculates the total number of edges from nodefactor.
### Approach 2 - calculating edge statistics based on 1) directing summing all edges in the ergm.ego matrix, w/o using nodefactor (node-level) target statistics, 2) divide 2 to convert the edge level to node level statistics
edge_ct_mx %>% sum/2
#### Interpretation: The matched edges in the matrix is counted twice (i.e., nf*prop), compared to ours. each off-diagonal edge is counted once (i.e., (nf/2)*prop)


## comparing target statistics between (nodefactor/2, summary statistics) and nodematch for "Grade".
### In "round(mixingmatrix(mesa.ego, "Grade", rowprob=T), 2), the probabilities of assortative mixing of Grades 7 to 12 are 
match_prop <- mx_prop %>% diag()
match_prop

## The total number of matched edges, non-differential, is
nmatch_tar <- sum((nf_tar_ergmego/2)*match_prop)
nmatch_tar # same as "sim.full <- simulate(fit.full)"==163
edge_ct_mx %>% diag()/2 # the sum of these is the same as (nf_tar_ergmego/2)*match_prop
#### Interpretation: The ergm.ego's approach to calculate nodematch target statistics is based on the nodefactor statistics, calculated from the edge-count matrix, but not based on the nodefactor statis, calculated from the egocentric mean deg.


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
      mix_edge_num(grp.a =1, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf.ag_layer.)
  }
  
  ### non-assortative mixing of 10-19y (grp 2) w/the other age.grps
  for (j in 3:6) {
    edge_ct_mx_1[2,j] <- 
      mix_edge_num(grp.a =2, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf.ag_layer.)
  }
  
  ### non-assortative mixing of 20-29y (grp 3) w/the other age.grps
  for (j in 4:6) {
    edge_ct_mx_1[3,j] <- 
      mix_edge_num(grp.a =3, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf.ag_layer.)
  }
  
  ### non-assortative mixing of 30-39y (grp 4) w/the other age.grps
  for (j in 5:6) {
    edge_ct_mx_1[4,j] <- 
      mix_edge_num(grp.a =4, grp.b = j, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf.ag_layer.)
  }
  
  ### non-assortative mixing of 50-59y (grp 5) w/the other age.grps
  edge_ct_mx_1[5,6] <- 
    mix_edge_num(grp.a =5, grp.b = 6, asymmetric_mix_matrix. =  mx_prop, nf.ag_layer.=nf.ag_layer.)
  
  
  ## Calculating number of diagonal mixing edges of each mixing pattern - assortative mixing
  for (j in 1:6) {
    edge_ct_mx_1[j,j] <-
     ((nf.ag_layer./2)[j])* # the reason /2 is used here is because this is a edge-level statistic
      mx_prop[j,j]
  }
  
  edge_ct_mx_1  %>% round(., 0)
}
  
edge_ct_billy <- 
edge_ct_test(asymmetric_mix_matrix. =  mx_prop,
             nf.ag_layer.=nf_tar_ergmego, # total edges in ergm.ego's edge count matrix
             edge_ct_type = "billy")


edge_ct_cm <- 
edge_ct_test(asymmetric_mix_matrix. =  mx_prop,
             nf.ag_layer.=nf_tar_ergmego,
             edge_ct_type = "cm")


edge_ct_billy -edge_ct_mx ## there's no difference between the edge count by Billy's approach and ergm.ego
edge_ct_cm -edge_ct_mx ##  there's difference between the edge count by CorporateMix's approach and ergm.ego
 
### Interpretation: we will use the ergm.ego approach to calculate the nodefactor (summing all values in cells of an age group) and then multiply them 
### with the mixing proportion to calculate the nodemix terms. Comparing edge_ct_billy with edge_ct_mx, the edges in the latter are counted twice so the marginal row sum would yield a node level statistics in edge_ct_mx.


## Using the edge count matrix based on Billy's formula to get the edge count of each group 
### Summing the all edges in a matrix would yield the target statistics for edge 
(
edge_ct_billy
) %>% sum(.,na.rm = T)

## Using the edge count matrix based on Billy's wrong formula to get the total number of edges of each group
edge_ct_grp_wrong <-edge_ct_grp_right <- c()

for (i in 1:6) { 
  
  # Calculation using target stats based on the target population
  edge_ct_grp_wrong[i] <-  
    sum(edge_ct_billy[i,], na.rm = T) + 
    sum(edge_ct_billy[,i], na.rm = T) - 
    edge_ct_billy[i,i]
  
}  


### Check overall discrepancy: see if the discrepancy is caused by double counting the triangular matrix 
#### (wrongly calculated total edges) - (edges in the upper triangular matrix) == (correctly calculated total edges)
(edge_ct_grp_wrong %>% sum) -  
edge_ct_billy[
  edge_ct_billy %>% upper.tri()
] %>% sum() # equals to the correct edge count of 203, suggesting the off-diagonal values are mistakenly double counted
##### Interpretation: the off-diagonal edges are counted twice in the wrong approach

### Check discrepancy specific to group, using the 2nd group as an example
#### The below suggests the off-diagnoal counts need to be divided by 2 when calculating the total edge count of each group
edge_ct_grp_wrong[2]- # wrongly calculated edge count in grp 2
  (edge_ct_billy[2,][-2]/2) %>% sum(., na.rm=T) # half of off-diagnoal edge counts
##### Interpretation: the above calculation yield the correct edge count in grp 2, which is the same as the following calculation using ergm.ego's matrix
(edge_ct_mx[2,]/2) %>% sum()


### Hence, the correct way to calculate the total edge count for each age group should be
for (i in 1:6) { 
  
  # Calculation using target stats based on the target population
  edge_ct_grp_right[i] <-  
    edge_ct_billy[i,i]+
    (
      sum(edge_ct_billy[i,], na.rm = T) + 
        sum(edge_ct_billy[,i], na.rm = T)  -
    2*edge_ct_billy[i,i])/2
    

}

#### Total number of edges of each age group, correctly calculated
edge_ct_grp_right #. the 2nd group should be == 37.5

##### The above is the same as the below calculation using ergm.ego's matrix
edge_ct_mx %>% rowSums()/2

##### The the correct total number of edge can be calculated by summing all edges in each group together
sum(edge_ct_grp_right)
 





 
 