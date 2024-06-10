suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))

layer = c("Home","School","Work","Nonhome") # the "All" category also exists and applies to the 6-tsna script
network = c("Rural", "Urban")
est_apch =   "mcmle" #  "sto_apoxy"
percent_target_pop=0.4

# Netdx  outputs
## file names of the outputs
file.name_in_r <- 
  paste0("data/netdx_outputs/dx_", layer, "__", network[1],"__", est_apch,"__", percent_target_pop, ".Rds")
file.name_in_u <- 
  paste0("data/netdx_outputs/dx_", layer, "__", network[2],"__", est_apch,"__", percent_target_pop, ".Rds")

## Rural
dx_h_r <- 
readRDS(file.name_in_r[1])

# dx_s_r <- 
#   readRDS(file.name_in_r[2])

dx_w_r <- 
  readRDS(file.name_in_r[3])

dx_nh_r <- 
  readRDS(file.name_in_r[4])

## Urban
dx_h_u <- 
  readRDS(file.name_in_u[1])

dx_s_u <-
  readRDS(file.name_in_u[2])

dx_w_u <- 
  readRDS(file.name_in_u[3])

dx_nh_u <- 
  readRDS(file.name_in_u[4])

# Plot netdx outputs
## Rural
plot(dx_h_r)
#plot(dx_s_r)
plot(dx_w_r)
plot(dx_nh_r)

## Urban
plot(dx_h_u)
plot(dx_s_u)
plot(dx_w_u)
plot(dx_nh_u)

# Diagnose urban school using mcmc.diagnostics, dynamics = F
## Urban school netest output
est_s_u <- readRDS("../COVID-GlobalMix/data/netest_outputs/netest_School__Urban__mcmle__0.4.Rds")


dx_s_u_dyna_f <-
  netdx(est_s_u,
        nsims =  30,
        ncores = 10,
        nsteps = 1000,
        nwstats.formula = est_s_u$formation,
        set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
        set.control.tergm = control.simulate.formula.tergm(MCMC.maxchanges = 1e7),
        dynamic = F,
        skip.dissolution = FALSE
        #keep.tedgelist = TRUE
  )


par(mfrow=c(2,1))
plot(dx_s_u)
plot(dx_s_u_dyna_f)

## Compare with input target statistics for urban school layer
## target statistics
node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))

## Interpretation: the edge counts in dx_s_u_dyna_f is similar to the edge count matrix below 
node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School %>% round() 


# Diagnosing FRP results
netsim_r <- readRDS("data/netsim_outputs/sim___Rural__sto_apoxy.Rds")
str(netsim_r$Home)

library(sna)

# Get the degree of each node
node_degrees_layers <- 
  data.frame(
      Home=  degree(netsim_r$Home),
      School=  degree(netsim_r$School),
      Work=  degree(netsim_r$Work),
      Nonhome=  degree(netsim_r$Nonhome)
        )

## Interpretation to the plots below: we observe there are ~40% nodes with >100 contacts at the school layers - this likely contribute to the unrelistic FRP.
par(mfrow = c(2, 2))
node_degrees_layers$Home %>% hist(main = "Home")
node_degrees_layers$School %>% hist(main= "School") # something wrong at the school layer.
sum(node_degrees_layers$School>100)
node_degrees_layers$Work %>% hist(main= "Work")
node_degrees_layers$Nonhome %>% hist(main = "Nonhome")








