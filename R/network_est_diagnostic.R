lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)

# Loading data
attri_tarstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_targetstats.RData")


#### Discrepancies of the total number of edge in each age group by nodefactor v. nodemix, talk to Sam

netstats$formation$formation_stats_rural$mix_prop_rural_layers$Home$Home_mix_prop_matrix_2d_glm %>% round(2)%>%
  kbl(caption = "Asymmetric proportion mixing matrix at Home of rural India site") %>%
  kable_classic(full_width = F, html_font = "Cambria") # for 2/5 meeting
attri_tarstats$target.stats$nmix.age.grp$rural$Home %>% round(0)%>%
  kbl(caption = "Symmetric degree mixing matrix at Home of rural India site") %>%
  kable_classic(full_width = F, html_font = "Cambria") # for 2/5 meeting



nmix_edge_sum_h <-nmix_edge_sum_s <- nmix_edge_sum_w <-nmix_edge_sum_nh <-c()  # number of edges of each age group calculated from nodemix
for (i in 1:6) {
  nmix_edge_sum_h[i] <-  
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Home[i,], na.rm = T) + 
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Home[,i], na.rm=T) - 
    attri_tarstats$target.stats$nmix.age.grp$rural$Home[i,i]
  
  nmix_edge_sum_s[i] <-  
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$School[i,], na.rm = T) + 
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$School[,i], na.rm=T) - 
    attri_tarstats$target.stats$nmix.age.grp$rural$School[i,i]
  
  nmix_edge_sum_w[i] <-  
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Work[i,], na.rm = T) + 
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Work[,i], na.rm=T) - 
    attri_tarstats$target.stats$nmix.age.grp$rural$Work[i,i]
  
  nmix_edge_sum_nh[i] <-  
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Nonhome[i,], na.rm = T) + 
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Nonhome[,i], na.rm=T) - 
    attri_tarstats$target.stats$nmix.age.grp$rural$Nonhome[i,i]
}


# compare total number of edge by nodefactor w. those by nodemix
rbind(
attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Home") %>% mutate(nmix_edge_sum_h) %>% rename(nmix_edge_sum_h=4),
attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "School") %>% mutate(nmix_edge_sum_s) %>% rename(nmix_edge_sum_h=4),
attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Work") %>% mutate(nmix_edge_sum_w) %>% rename(nmix_edge_sum_h=4),
attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Nonhome") %>% mutate(nmix_edge_sum_nh) %>% rename(nmix_edge_sum_h=4)
)%>% 
  mutate(
    relaat_diff=
      (nmix_edge_sum_h-nf.ag)/nf.ag
  ) %>%  # characterize the relative difference
  mutate_if(is.numeric, round,2)


############## Set up vertex attribute ##############
# Total number of nodes in each network - difference caused by rounding
n_node_rural = attri_tarstats$attr$age.grp$rural %>% nrow()
n_node_urban= attri_tarstats$attr$age.grp$urban %>% nrow()
# set script to flexible number

# Initiate nodes
nw_rural <- network_initialize(n_node_rural)
nw_urban <- network_initialize(n_node_urban)

# Nodes w/ age groups, each layer of a network has the same age attribution
nw_rural <- set_vertex_attribute(nw_rural, "age.grp",
                                 attri_tarstats$attr$age.grp$rural$age.grp.num 
)

nw_urban <- set_vertex_attribute(nw_urban, "age.grp",
                                 attri_tarstats$attr$age.grp$urban$age.grp.num 
)




# nw_rural_h <- set_vertex_attribute(nw_rural, attrname = "work",
#                                    value = rep(0:1, each = n_node/2) 
# ) 

# nw_urban <- set_vertex_attribute(nw_urban, "age",
#                                  node.age.grp.urban %>% pull(age_site)
#                                  )



############## Set up target statistics  ##############

# Note: we treat the 1st age group (0-10 years old) as reference group

# Target statistics of nodemix at home, rural
matrix_h_r <- attri_tarstats$target.stats$nmix.age.grp$rural$Home %>% as.matrix()
target_nmix_vec <- c(na.omit(matrix_h_r[,1]) %>% as.numeric(), 
                     na.omit(matrix_h_r[,2]) %>% as.numeric(),
                     na.omit(matrix_h_r[,3]) %>% as.numeric(),
                     na.omit(matrix_h_r[,4]) %>% as.numeric(),
                     na.omit(matrix_h_r[,5]) %>% as.numeric(),
                     na.omit(matrix_h_r[,6]) %>% as.numeric()
                     ) # target stat of nodemix in lexicographic order

# Baseline formulation model: edge + nodefactor(age.grp)+nodemix(matched.agegrps)
edge.nf.nmatch <- 
~edges + nodefactor("age.grp", levels = -1) + 
 nodemix("age.grp", levels2 = c(1, 3, 6, 10, 15,21)) # lexicographic order of matched edges in nodemix
                                                       

# Formation target statistics 
## for base model of edge + nodefactor(age.grp)+nodemix(age.grp) for home, rural
tstat.base.nf.h.r <- c(attri_tarstats$target.stats$edges$rural %>% filter(contact_location == "Home" ) %>% pull(edges_adj_age), # edge
                       (attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Home" ) %>% pull(nf.ag))[-1],  # nodefactor
                       target_nmix_vec[c(1, 3, 6, 10, 15,21)] # # matched edges from nodemix
) 

## for fully saturated model for home, rural
tstat.full.h.r <- c(attri_tarstats$target.stats$edges$rural %>% filter(contact_location == "Home" ) %>% pull(edges_adj_age), # edge
                       (attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Home" ) %>% pull(nf.ag))[-1],  # nodefactor
                    target_nmix_vec#[-1] # matched edges from nodemix
) 


# Dissolution models
diss.h <- dissolution_coefs(dissolution = ~offset(edges), duration = 1e6) # very large number, edge doesn't dissolve
diss.nh <- dissolution_coefs(dissolution = ~offset(edges), duration = 1) # non-persistent (turnover every day)




# Model fitting and simulation
## Rural network
### Home
#### Simpliest model
##### Estimating fully saturated model ~ edge + nodefactor(age.grp) + nodematch(age.grp)
est.h.edges.nf.nm <- netest(nw_rural, 
                            formation = edge.nf.nmatch, 
                            target.stats = tstat.base.nf.h.r, 
                            coef.diss =  diss.h,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)


#### Based on est.h.edges.nf.nm, simulating all mixing pattern 
nmix_sim_all <- netdx(est.h.edges.nf.nm, nsims = 20, ncores = 8, 
                          nsteps = 1000, dynamic = T,
                          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                          nwstats.formula = 
                            ~edges + nodefactor("age.grp", levels = -1) +nodemix("age.grp", levels2 = NULL), 
                          keep.tedgelist = TRUE
) # the bias for the simulated matching edges isn't large.

nmix_sim_all_1 <- 
nmix_sim_all$stats.table.formation  %>%
  mutate(Target = 
           tstat.full.h.r,
         `Pct Diff` = 100*(`Sim Mean`-Target)/Target
  )  # same age group mixings have same target stats as nodematch



#### Full-saturated model
# Note: A fully saturated model has been run but couldn't be executed successfully.
# Interpretation: The model cannot run and observed the warning of "Model statistics ‘mix.age.grp.1.6’, ‘mix.age.grp.2.6’, ‘mix.age.grp.3.6’, ‘mix.age.grp.4.6’,
# ‘mix.age.grp.5.6’, and ‘mix.age.grp.6.6’ are linear combinations of some set of preceding statistics at the current stage of 
# the estimation. This may indicate that the model is nonidentifiable."


#### Base model +  large target stats
tarstat_h_r_lex <- # Saving target statistics and their labels in lexicographic order to a dataframe
nmix_sim_all_1[-c(1:6),] %>% rownames_to_column(var = "edge_type") %>% select(1:2) %>% 
  rownames_to_column(var="lex_order")

tarstat_h_r_lex

edges_large_1k <- # target statistics > 1k in descending order
tarstat_h_r_lex %>% arrange(desc(Target)) %>% filter(Target > 1000) 

edges_large_sort <- # target statistics > 1k in descending order
  tarstat_h_r_lex %>% arrange(desc(Target)) 

########### Model w/ nodefactor(age) ########

nmix_sim.i <- list()

for (i in 1:10) {
  


edge_large <- edges_large_1k[c(1:i),] # extracting the information of the edge types to be included in the model

lex_order <- paste(edge_large %>% pull(lex_order),  collapse = ", ")  # lexcographic order to be specified in node mix in each iteration


formula <-
as.formula(
   paste0(
  "~edges +",
  "nodefactor('age.grp', levels = -1) +",
  "nodemix('age.grp', levels2 = c(1, 3, 6, 10, 15, 21, ", lex_order, "))"
)
)


est.h.1 <- netest(nw_rural, 
                  formation = formula, 
                  target.stats = c(tstat.base.nf.h.r, edge_large %>% pull(Target)) , 
                  coef.diss =  diss.h,
                  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)


print(i)

nmix_sim.i[[i]] <- netdx(est.h.1, nsims = 20, ncores = 8, 
                      nsteps = 1000, dynamic = T,
                      set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                      nwstats.formula = 
                        formula, 
                      keep.tedgelist = TRUE
                      )
}

edge_large 
nmix_sim.i[[7]]

########### Model w/o nodefactor(age) ########
nmix_sim.wo.nf.i <- list()

for (i in 1:nrow(edges_large_sort)
     ) {
  
  
  
  edge_large <- edges_large_sort[c(1:i),] # extracting the information of the edge types to be included in the model
  
  lex_order <- paste(edge_large %>% pull(lex_order),  collapse = ", ")  # lexcographic order to be specified in node mix in each iteration
  
  
  formula <-
    as.formula(
      paste0(
        "~edges +",
        "nodemix('age.grp', levels2 = c(1, 3, 6, 10, 15, 21, ", lex_order, "))"
      )
    )
  
  
  est.h.1 <- netest(nw_rural, 
                    formation = formula, 
                    target.stats = c(tstat.base.nf.h.r[c(1, 7:12)], # just use the edge statistics here
                                     edge_large %>% pull(Target)) , 
                    coef.diss =  diss.h,
                    set.control.ergm = control.ergm(MCMLE.maxit = 500)
  )
  
  
  print(i)
  
  nmix_sim.wo.nf.i[[i]] <- netdx(est.h.1, nsims = 20, ncores = 8, 
                           nsteps = 1000, dynamic = T,
                           set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                           nwstats.formula = 
                             formula, 
                           keep.tedgelist = TRUE
  )
}

nmix_sim.wo.nf.i[[9]]

##########################################


