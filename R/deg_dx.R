# Evaluate fitting of degree statistics
library(EpiModel); library(dplyr);library(tibble)


percent_target_pop = 0.01 # or 0.001; 0.1 and 0.001 correspond to 10% and 0.1% of target population, respectively



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
nw_r <- initiate_nw(node_attribute_target_stats = node_attribute_target_stats, network = "rural")

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
                node_attribute_target_stats$degrange$rural$Nonhome[1] 
                ) 


model2 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +nodemix("age.grp", levels2 =  -1)+degree(0), 
         target.stats = tstat_nh_r_deg , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  ) # now there aren't errors

model2$fit$coefficients

## Edge-only model with degree(0) term - this would evaluate whether the degree term works w/o the age mixing
tstat_nh_r_deg_edge <- c(target_nmix_vec_nh_r$target_nmix_vec %>% sum(), # total edge
                        #N*0.16 # this scenario assumes 16% of nodes are isolated
                        node_attribute_target_stats$degrange$rural$Nonhome[1] 
) 

model3 <- 
  netest(nw= nw_r$nw_nh,
         formation = ~edges +degree(0), 
         target.stats = tstat_nh_r_deg_edge  , 
         coef.diss = diss_nh_r,
         set.control.ergm = control.args$sto_apoxy
  )


# Model fitting using the nodefactor term. 
nw <- network.initialize(n = N, directed = FALSE)
nw <- set.vertex.attribute(nw, "group", 1- node_attribute_target_stats$attr$rural$contact_attribute_Nonhome # we flip the contact status here so those doesn't contact will have a "1" status to work with the 0 target statistics
                           )
fit <- ergm(nw ~ edges + nodefactor("group"),
            target.stats =
              c( node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome %>% sum,
                0)
            )
## Simulated degree term
simulated_result <- 
colMeans(simulate(fit, output = "stats", monitor = ~degree(0:10), nsim = 1e4))

barplot(
simulated_result[3:13]/N)
## Observed degree term
node_attribute_target_stats$degrange$rural %>% select(deg_range_5cat,Nonhome) %>% mutate_if(is.numeric, ~./N)
plot(simulate(fit))



# Model fitting for school layer
## Edge-only model with degree(0) term - this would evaluate whether the degree term works w/o the age mixing


tstat_s_r_deg_edge <- c(node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% sum, # total edge
                        
                        node_attribute_target_stats$degrange$rural$School[1] 
) 

diss_s_r <- 
  dissolution_coefs(dissolution = ~offset(edges), 
                    duration = netstats$dissolution %>% filter(study_site == "Rural" & contact_location == "School") %>% pull(know_contact_duration)
  )


model_s <- 
  netest(nw= nw_r$nw_s,
         formation = ~edges +degree(0), 
         target.stats = tstat_s_r_deg_edge  , 
         coef.diss = diss_s_r,
         set.control.ergm = control.args$sto_apoxy
  )




############## Model diagnosis  ##############


# Validation assessment for model without degree(0) term - the simulated degree is 7.3, which significantly underestimate the nodes without edge
dx <-
  netdx(model_s, # this can be any of the above models
        nsims =  30,
        ncores = 10,
        nsteps = 1000,
        nwstats.formula =  ~edges +degree(0:9),
        # ~edges +nodemix("age.grp", levels2 =  -1)+degree(0),
        set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 200000, # 2) bumping up from 200000
                                                         MCMC.interval = 25000), # 2) bumping up from 25000
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000 # 1) bumping up from 50000
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


# proportion based on contact status
## nodal attribute of contact at the nonhome layer
table( # I suspect the marginal is the proportion of not having contact
node_attribute_target_stats$attr$rural$contact_attribute_Nonhome,
node_attribute_target_stats$attr$rural$node.age.grp
) %>% prop.table(., margin = 2) %>% 
  cbind(., marginal=
          node_attribute_target_stats$attr$rural$contact_attribute_Nonhome %>% table() %>% prop.table()
        )

## marginal
table(node_attribute_target_stats$attr$rural$contact_attribute_Nonhome) %>% prop.table() # marginal proportion is 0.35, this is based on the target population (contact of study participant + age distribution)

# proportion based on degree(0)
node_attribute_target_stats$degrange$rural %>% mutate_if(is.numeric, ~./N) # the proportion is 0.37, the small different with the above is likely caused by normalization.

# proportion based on study participants - identical to nodal attribute
table(node_attribute_target_stats$participant_contact_layer$rural$Nonhome_cat, 
      node_attribute_target_stats$participant_contact_layer$rural$participant_age) %>% prop.table(., margin = 2) %>% 
  cbind(., marginal=
node_attribute_target_stats$participant_contact_layer$rural$Nonhome_cat %>% table() %>% prop.table()
)
# associating contact status with participant's demographic data
participant_bi_deg <- 
node_attribute_target_stats$participant_contact_layer$rural %>% 
  merge(., india_participant %>% filter(study_site == "Rural") %>% select(-participant_age), by = "rec_id"
        )

independent_vars <- c("participant_sex", "participant_age", "read_write", "enrolled_school", "transport_use")

models_list <- list()


for (indep_var in independent_vars) {
  # Create the regression formula
  formula <- as.formula(paste("Nonhome_cat", "~", indep_var))
  
 
  model <- glm(formula, data = participant_bi_deg, family = binomial) %>% summary
  
  # Print the summary of the model
  cat("\nRegression of", "Nonhome_cat", "on", indep_var, ":\n")
  print(model)
  
  models_list[[indep_var]] <- model$coefficients
}
 

glm(Nonhome_cat ~ transport_use, data = participant_bi_deg, family = binomial) %>% summary()




