# Evaluate fitting of degree statistics
library(EpiModel); library(dplyr);library(tibble)


percent_target_pop = 0.1 # or 0.001; 0.1 and 0.001 correspond to 10% and 0.1% of target population, respectively



# helper functions
source("R/netest_helper_functions.R")


# Loading data
## target statistics
node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))

N = node_attribute_target_stats$attr$rural %>% nrow # number of nodes

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")


# ############## Recode low degree at school layer to 0  ##############
# # For both the urban and rural school layers, we recode those degree <1 to 0 to make netest viable
# # For urban school layer the low values were <0.01, this threshold is used for the re-coding - this can make netest run, but netdx shows poor fit
# node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School[
#   node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School <1] <- 0
# 
# # For rural school layer the low values were <10, this threshold is used for the re-coding 
# node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School[
#   node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School <1] <- 0




############## Model estimation  ##############
# Define control argument, "sto_apoxy" is for stochastic approximation, "mcmle" is for MCMLE
control.args <-  
  list(
    sto_apoxy=
      control.ergm(
        # The following setting is copied from - https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
        main.method = "Stochastic-Approximation", 
        MCMLE.maxit = 500, # tried 5000 here but the bias didn't go away
        SAN.maxit = 3,
        SAN.nsteps.times = 4,
        MCMC.samplesize = 1e4, # tried 1e6 here (along with MCMLE.maxit = 5000) but the bias didn't go away
        MCMC.interval = 5e3,
        parallel = 1
      ),
    mcmle=
      control.ergm(
        main.method = "MCMLE",
        MCMLE.maxit = 500,
        MCMC.samplesize = 5e5,
        MCMC.interval = 25000,
        parallel = 10
      )
  )

# intiate network
nw_r <- initiate_nw(attri_tarstats = node_attribute_target_stats, network = "rural")

# Model fitting for nonhome layer
## Nodemix target statistics
target_nmix_vec_nh_r <- nmix_tar_lex(
node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome)

## Duration statistics
diss_nh_r <- 
  dissolution_coefs(dissolution = ~offset(edges), 
                    duration = netstats$dissolution %>% filter(study_site == "Rural" & contact_location == "Nonhome") %>% pull(know_contact_duration)
  )


## saturated model for age mixing
tstat_nh_r <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                target_nmix_vec_nh_r$target_nmix_vec[- 1] # ,  #  edges counts from nodemix, excluding the first non-zero edge
                #c(node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[1]
                #) # number of nodes w/ weighted degree of 0
)

model1 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +nodemix("age.grp", levels2 =  -1), 
         target.stats = tstat_nh_r , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )



##  model with saturated nodemix terms and degree(0) term
tstat_nh_r_deg <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                target_nmix_vec_nh_r$target_nmix_vec[- 1]  ,  #  edges counts from nodemix, excluding the first non-zero edge
                node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[1] 
                ) 


model2 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +nodemix("age.grp", levels2 =  -1)+degree(0), 
         target.stats = tstat_nh_r_deg , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  ) 

model2$fit$coefficients



## Edge-only model with degree(0) term - this would evaluate whether the degree term works w/o the age mixing
tstat_nh_r_deg_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                        #N*0.16 # this scenario assumes 16% of nodes are isolated
                    node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[1] 
) 

model3 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +degree(0), 
         target.stats = tstat_nh_r_deg_edge  , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )


## Edge only model with degrange(from =0, to=1) - doesn't work
tstat_nh_r_degrange_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                          sum(node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[1:2]
                               )
                         
) 

model4 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges + degrange(from = 0, to =1), 
         target.stats = tstat_nh_r_degrange_edge, 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )

## Edge only model with degrange(from = 4,to=Inf) 
tstat_nh_r_degrange_4plus_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                              node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[5]
) 
model5 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges + degrange(from = 4, to =Inf), 
         target.stats = tstat_nh_r_degrange_4plus_edge , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )



## Edge-only model with degree(0:3)
tstat_nh_r_deg_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                         node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[c(1:4)] 
) 

model10 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +degree(0:3), 
         target.stats = tstat_nh_r_deg_edge  , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )

## Edge-only model with degree(3)
tstat_nh_r_deg_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                         
                         node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[c(4)] 
) 

model8 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +degree(3), 
         target.stats = tstat_nh_r_deg_edge  , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )

## Edge-only model with degree(1:3)
tstat_nh_r_deg_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                         node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[c(2:4)] 
) 

model9 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +degree(1:3), 
         target.stats = tstat_nh_r_deg_edge  , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )



############## Model diagnosis  ##############


tstat_nh_r_deg <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                    target_nmix_vec_nh_r$target_nmix_vec[- 1]  ,  #  edges counts from nodemix, excluding the first non-zero edge
                    node_attribute_target_stats$degrange$rural$Nonhome$N_nodes_age[1] 
) 


model2 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +nodemix("age.grp", levels2 =  -1)+degree(0), 
         target.stats = tstat_nh_r_deg , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  ) 

model2 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges + nodemix("age.grp", levels2 =  -1), 
         target.stats = tstat_nh_r_deg[-length(tstat_nh_r_deg)], 
         coef.diss = diss_nh_r
  ) 

model2 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges + degree(0), 
         target.stats = c(tstat_nh_r_deg[1], tstat_nh_r_deg[length(tstat_nh_r_deg)]), 
         coef.diss = diss_nh_r
  ) 

nw <- network_initialize(n = 11781)
fit <- netest(nw = nw,
              formation = ~edges + degree(0), 
              target.stats = c(17346, 4286), 
              coef.diss = diss_nh_r
) 

dx <-
  netdx(model2, # this can be any of the above models
        nsims =  10,
        ncores = 10,
        nsteps = 1000,
        nwstats.formula =  ~edges + nodemix("age.grp", levels2 = -1) + degree(0:9),
        # set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 1000000, # 2) bumping up from 200000
        #                                                  MCMC.interval = 50000), # 2) bumping up from 25000
        # set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 100000 # 1) bumping up from 50000
        # ),
        dynamic = T,
        skip.dissolution = FALSE
  )
print(dx)

# Validation assessment for model without degree(0) term - the simulated degree is 7.3, which significantly underestimate the nodes without edge
dx <-
  netdx(model2, # this can be any of the above models
        nsims =  10,
        ncores = 10,
        nsteps = 1000,
        nwstats.formula =  ~edges +degree(0:9),
        set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 1000000, # 2) bumping up from 200000
                                                         MCMC.interval = 50000), # 2) bumping up from 25000
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 100000 # 1) bumping up from 50000
        ),
        dynamic = T,
        skip.dissolution = FALSE
  )





## check degree distribution of the simulated data
library(ggplot2)
test <- dx$stats.table.formation %>% tibble::rownames_to_column(., var = "degree type") %>% slice(-c(1)) %>%  
  mutate(`Sim Mean` = `Sim Mean`/N) %>% # convert the simulated mean to to proportion of nodes
  ggplot(., aes(x = `degree type`, y = `Sim Mean`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  labs(x = "Degree Type", y = "Sim Mean", title = "Sim Mean by Degree Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
test












