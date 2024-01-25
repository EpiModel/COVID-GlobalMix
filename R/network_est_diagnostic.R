lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)

# Loading data
attri_tarstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_targetstats.RData")


#### Discrepancies of the total number of edge in each age group by nodefactor v. nodemix, talk to Sam

attri_tarstats$target.stats$nmix.age.grp$rural$Home # mixing matrix at home

nmix_edge_sum <- c()  # number of edges of each age group calculated from nodemix
for (i in 1:6) {
  nmix_edge_sum[i] <-  
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Home[i,], na.rm = T) + 
    sum(attri_tarstats$target.stats$nmix.age.grp$rural$Home[,i], na.rm=T) - 
    attri_tarstats$target.stats$nmix.age.grp$rural$Home[i,i]
}


# compare total number of edge by nodefactor w. those by nodemix
attri_tarstats$target.stats$nf.age.grp$rural %>% filter(contact_location == "Home") %>% mutate(nmix_edge_sum) 





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
                       target_nmix_vec[c(1, 3, 6, 10, 15,21)] #[-1] # matched edges from nodemix
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
est.h.full <- netest(nw_rural, 
                            formation = ~edges + nodefactor("age.grp", levels = -1) + 
                              nodemix("age.grp", levels2 = c(NULL)) , 
                            target.stats = tstat.full.h.r, 
                            coef.diss =  diss.h,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)
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

#### Items for automatizing the process
i=1
edge_large <- edges_large_1k[c(1:i),] # i corresponding to the number of edge types to included in each iteration
lex_order <- edge_large %>% pull(lex_order) # lexcographic order to be specified in node mix in each iteration

##### adjust for 3.5 in the model
formula.1 <- ~edges + nodefactor("age.grp", levels = -1) + 
  nodemix("age.grp", levels2 = c(1, 3, 6, 10, 15,21, # matched ties
                                 13 # non-assortative tie to be adjusted
                                 )
  )
est.h.1 <- netest(nw_rural, 
                     formation = formula.1, 
                     target.stats = 
                    c(tstat.base.nf.h.r, # baseline target stats
                      edge_large %>% filter(lex_order==13) %>% pull(Target) # target stats of the larggest non-assortative tie
                      ), 
                     coef.diss =  diss.h,
                     set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

nmix_sim.1 <- netdx(est.h.1, nsims = 20, ncores = 8, 
                      nsteps = 1000, dynamic = T,
                      set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                      nwstats.formula = 
                       formula.1, 
                      keep.tedgelist = TRUE
)
nmix_sim.1


########### Automation scriopt in progresss ########
i=1 # we first include the largest tie





formula.1 <- 
as.formula(
~edges + nodefactor("age.grp", levels = -1) + 
  nodemix("age.grp", 
          levels2 =
            c(paste0("1, 3, 6, 10, 15, 21, ", paste(lex_order))
              )
  )
)

formula.1 # couldn't read the value from global environment.

# The other way to read the formula
paste0("1, 3, 6, 10, 15,21,", enquote(lex_order))[2]


est.h.1 <- netest(nw_rural, 
                  formation = formula.1, 
                  target.stats = c(tstat.base.nf.h.r, edge_large %>% pull(Target)) , 
                  coef.diss =  diss.h,
                  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

nmix_sim.1 <- netdx(est.h.1, nsims = 20, ncores = 8, 
                      nsteps = 1000, dynamic = T,
                      set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                      nwstats.formula = 
                        formula.1, 
                      keep.tedgelist = TRUE
)


##########################################


#### Based on est.h.edges.nf.nm, simulating mixing pattern in [1,2]
nmix_sim_2 <- netdx(est.h.edges.nf.nm, nsims = 20, ncores = 8, 
                      nsteps = 1000, dynamic = T,
                      set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                      nwstats.formula = 
                        ~edges + nodefactor("age.grp", levels = -1) + 
                        nodematch("age.grp", levels = -1, diff = T) +nodemix("age.grp", levels2 = c(2)
                                                                             ), 
                      keep.tedgelist = TRUE) 

nmix_sim_2$stats.table.formation  %>%
  mutate(Target = 
           tstat.full.h.r[c(1:11, 13)],
         `Pct Diff` = 100*(`Sim Mean`-Target)/Target
  )



#### Estimating fully saturated model ~ edge + nodefactor(age.grp) + nodemix(age.grp[1.2])
est.h.edges.nf.nm_2 <- netest(nw_rural, 
                            formation = ~edges + nodefactor("age.grp" , levels = -1) + 
                              nodematch("age.grp", levels = -1, diff = T)+ 
                              nodemix("age.grp", levels2 = c(2)), 
                            target.stats = c(tstat.base.nf.h.r, matrix_h_r[1,2]), 
                            coef.diss =  diss.h,
                            set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

#### Simulating terms of the above model + nodemix
nmix_sim_2to4 <- netdx(est.h.edges.nf.nm_2, nsims = 20, ncores = 8, 
                          nsteps = 1000, dynamic = T,
                          set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
                          nwstats.formula = 
                            ~edges + nodefactor("age.grp", levels = -1) + 
                            nodematch("age.grp", levels = -1, diff = T) +nodemix("age.grp", levels2 = c(2,3,4)), 
                          keep.tedgelist = TRUE
)

nmix_sim_2to4$stats.table.formation  %>%
  mutate(Target = 
           tstat.full.h.r[c(1:11, 13:15)],
         `Pct Diff` = 100*(`Sim Mean`-Target)/Target
  )






# 
# 
# ### Nonhome
# # Fully saturated model with ~ edge + nodefactor + nodematch
# est.nh.edges.nf.nm <- netest(nw_rural, 
#                              formation = formation.edge_age.nf.nm, 
#                              target.stats = tstats.edge_age.nf.nm.nonh.rural, 
#                              coef.diss =  diss.nh,
#                              set.control.ergm = control.ergm(MCMLE.maxit = 500)
# )
# 
# dx.nh.edges.nf.nm <- netdx(est.nh.edges.nf.nm, nsims = 1000, ncores = 8,  # incease n sim
#                            #nsteps = 1000, 
#                            dynamic = F, # this is an ERGM rather than TERGM
#                            set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                            nwstats.formula = formation.edge_age.nf.nm, 
#                            keep.tedgelist = TRUE
# ) 
# 
# dx.nh.edges.nf.nm
# 
# 
# 
# ### School
# 
# ## school, rural
# ### target stats for edge + nodefactor
# tstats.edge_age.nf.s.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "School" ) %>% pull(edges_adj_age), # edge
#                                 c( (formation_stats$rural$nf.age.grp %>% filter(contact_location == "School" ) %>% pull(nf.ag))[-c(1,5,6)],0,0) # excluding the 1st category, considering it as the default reference, passing zero to the fifth and sixth categories given the low target stat
# )
# 
# ### target stats for edge + nodefactor + nodematch
# tstats.edge.nf_nm.s.rural <- 
#   c(tstats.edge_age.nf.s.rural, 
#     c((formation_stats$rural$nm.age.grp %>% filter(contact_location == "School" ) %>% pull(nm.ag)
#     )[-c(1,3,4,5)],0,0,0,0) # the value of the last three observed groups of 3,4,5 are <20, we pass 0 to them and the 6th grp, which wasn't observed
#   )# excluding the 1st category, considering it as the default reference, passing zero to the 2nd category given low target stat
# 
# 
# # model with data of Edge + nodefactor(age.grp) 
# est.s.edges.nf <- netest(nw_rural, 
#                          formation = ~edges + nodefactor("age.grp" , levels = -1
#                          ) , 
#                          target.stats = tstats.edge_age.nf.s.rural, 
#                          coef.diss =  diss.school.rural,
#                          set.control.ergm = control.ergm(MCMLE.maxit = 500)
# )
# 
# dx.s.edges.nf <- netdx(est.s.edges.nf, nsims = 20, ncores = 8,
#                        nsteps = 1000,
#                        dynamic = T,
#                        set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                        nwstats.formula = ~edges + nodefactor("age.grp" , levels = -1
#                        ),
#                        keep.tedgelist = TRUE
# )
# dx.s.edges.nf
# 
# ### Finding out which level for nodematch should be adjusted
# #### simulation adding the nodematch term to decide what categories to include
# dx.s.edges.nf.nm_sim <- netdx(est.s.edges.nf, nsims = 20, ncores = 8, 
#                               nsteps = 1000, 
#                               dynamic = T, 
#                               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                               nwstats.formula = formation.edge_age.nf.nm, 
#                               keep.tedgelist = TRUE
# )
# 
# 
# 
# dx.s.edges.nf.nm_sim$stats.table.formation  %>% 
#   mutate(Target = tstats.edge.nf_nm.s.rural,
#          `Pct Diff` = 100*(`Sim Mean`-tstats.edge.nf_nm.s.rural)/tstats.edge.nf_nm.s.rural ) 
# 
# 
# # model with data of Edge + nodefactor(age.grp) + nodematch(age.grp)
# est.s.edges.nf.nm <- netest(nw_rural, 
#                             formation = formation.edge_age.nf.nm , 
#                             target.stats = tstats.edge.nf_nm.s.rural, 
#                             coef.diss =  diss.work.rural,
#                             set.control.ergm = control.ergm(MCMLE.maxit = 500)
# )
# # talk to Sam, this model cannot be run
# 
# # dx.s.edges.nf.nm <- netdx(est.s.edges.nf.nm, nsims = 20, ncores = 8, 
# #                    nsteps = 1000, 
# #                    dynamic = T, 
# #                set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
# #                nwstats.formula = formation.edge_age.nf.nm, 
# #                keep.tedgelist = TRUE
# #                )
# # 
# # dx.s.edges.nf.nm 
# ## Note: the bias for grp.3 seems still a little big, despite being < 5%
# 
# 
# 
# ### Work
# 
# ## Work, rural
# 
# 
# ### target stats for edge + nodefactor 
# tstats.edge_age.nf.w.rural <- c(formation_stats$rural$edge %>% filter(contact_location == "Work" ) %>% pull(edges_adj_age), # edge
#                                 c(0, 0,(formation_stats$rural$nf.age.grp %>% filter(contact_location == "Work" ) %>% pull(nf.ag))[-c(1,2)])[-3] # passing zero to the 1st and 2nd category given low target stat, excluding the 3rd category, considering it as the default reference, 
# )
# 
# ### target stats for edge + nodefactor + nodematch
# tstats.edge.nf_nm.work.rural <- 
#   c(tstats.edge_age.nf.w.rural, 
#     c(0,0, (formation_stats$rural$nm.age.grp %>% filter(contact_location == "Work" ) %>% pull(nm.ag)
#     )[-c(1,2)])[-3]
#   )# excluding the 1st category, considering it as the default reference, passing zero to the 2nd category given low target stat
# 
# # model with data of Edge + nodefactor(age.grp) 
# est.w.edges.nf <- netest(nw_rural, 
#                          formation = ~edges + nodefactor("age.grp" , levels = -3) , 
#                          target.stats = tstats.edge_age.nf.w.rural, 
#                          coef.diss =  diss.work.rural,
#                          set.control.ergm = control.ergm(MCMLE.maxit = 500)
# )
# 
# dx.w.edges.nf<- netdx(est.w.edges.nf, nsims = 20, ncores = 8, 
#                       nsteps = 1000, 
#                       dynamic = T, 
#                       set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                       nwstats.formula = ~edges + nodefactor("age.grp" , levels = -3), 
#                       keep.tedgelist = TRUE
# )
# 
# dx.w.edges.nf 
# 
# # simulation adding the nodematch term to decide what categories to include
# dx.w.edges.nf.nm_sim <- netdx(est.w.edges.nf, nsims = 20, ncores = 8, 
#                               nsteps = 1000, 
#                               dynamic = T, 
#                               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                               nwstats.formula =  ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
#                                                                                                          levels = -3, diff = T), 
#                               keep.tedgelist = TRUE
# )
# 
# dx.w.edges.nf.nm_sim 
# 
# 
# 
# dx.w.edges.nf.nm_sim$stats.table.formation  %>% 
#   mutate(Target = tstats.edge.nf_nm.work.rural,
#          `Pct Diff` = 100*(`Sim Mean`-tstats.edge.nf_nm.work.rural)/tstats.edge.nf_nm.work.rural ) 
# # Note: none of the simulated catories is less than 5 percent indicating all of them may need to be adjusted. But, the value of the 6th group is relative low, we pass 0 to it for the below run.
# 
# 
# # model with data of Edge + nodefactor(age.grp) + nodematch(age.grp)
# est.w.edges.nf.nm <- netest(nw_rural, 
#                             formation =  ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
#                                                                                                  levels = -3, diff = T), 
#                             target.stats = tstats.edge.nf_nm.work.rural,  #c(tstats.edge.nf_nm.work.rural[-length(tstats.edge.nf_nm.work.rural)],0), 
#                             coef.diss =  diss.work.rural,
#                             set.control.ergm = control.ergm(MCMLE.maxit = 500)
# )
# 
# dx.w.edges.nf.nm <- netdx(est.w.edges.nf.nm, nsims = 20, ncores = 8, 
#                           nsteps = 1000, 
#                           dynamic = T, 
#                           set.control.ergm = control.simulate.formula(MCMC.burnin = 1e5),
#                           nwstats.formula = ~edges + nodefactor("age.grp" , levels = -3)+ nodematch("age.grp", 
#                                                                                                     levels = -3, diff = T), 
#                           keep.tedgelist = TRUE
# ) #  the nodematch term for group 6 has bias > 5%. Since it is low, we can exclude it moving forward? 
# 
# dx.w.edges.nf.nm 
# ## Note: the bias for grp.3 seems still a little big, despite being < 5%
# 
# 
# formation_stats$rural$nf.age.grp %>% filter(contact_location == "Work" )
# 
# 
# ### target stats for edge + nodefactor + nodematch
# tstats.edge.nf_nm.s.rural <- 
#   c(tstats.edge_age.nf.s.rural, 
#     c((formation_stats$rural$nm.age.grp %>% filter(contact_location == "School" ) %>% pull(nm.ag)
#     )[-c(1,3,4,5)],0,0,0,0) # the value of the last three observed groups of 3,4,5 are <20, we pass 0 to them and the 6th grp, which wasn't observed
#   )
# 
# 
