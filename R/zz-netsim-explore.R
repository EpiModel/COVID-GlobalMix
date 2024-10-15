library(sna)
# read target stats
# Loading data
## target statistics
node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", 0.001, ".Rds"))

## check household size
node_attribute_target_stats$node_hh_assign_validation$rural

## summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")

N = node_attribute_target_stats$attr$rural %>% nrow()

mean_deg = 2*sum(node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home)/N

edge_stat = (mean_deg/2)*N


nw <- network_initialize(N)
nw_hh_bound <- set_vertex_attribute(nw, "hh_id", node_attribute_target_stats$attr$rural$hh)

# dissolution model
coef.diss <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = 1e+12)




# network w/o houshold boundary
g <- 
netest(
  nw = nw,
  formation = ~edges ,
  target.stats =  c(edge_stat),
  coef.diss = coef.diss)$fit
g <- 
  simulate(g)
plot(g)
mean(get_degree(g))
table(get_degree(g))
table(component.dist(g, connected = "weak")$csize)
components(g, connected = "weak")

table(component.dist(g, connected = "weak")$membership)

# network w/ houshold boundary
## Attempt 1 - using offset(nodematch("hh_id", diff = FALSE))
g_1 <- netest(
  nw = nw_hh_bound,
  formation = ~edges + nodematch("hh_id", diff = FALSE),
  target.stats =  c(edge_stat, edge_stat),
  coef.diss = coef.diss,
  control = control.net(
    ergm.control = control.ergm(
     
      main.method = "Stochastic-Approximation"
    )
  )
  
  )$fit
g_1 <- simulate(g_1)
plot(g_1)
### Note: if we don't include hh_id in the offset, we cannot 100% preventing forming of edges between households


## Attempt 2 - using blockdiag(attr="hh_id")
## define a block constraint introduced in: https://statnet.org/workshop-advanced-ergm/advanced_ergm_tutorial.html
hh_constraints <- ~blockdiag(attr= "hh_id")
g_2 <- netest(
  nw = nw_hh_bound,
  formation = ~edges ,
  target.stats =  c(edge_stat),
  coef.diss = coef.diss_1,
  constraints = hh_constraints,
  control = control.net(
    ergm.control = control.ergm(
      
      main.method = "MCMLE"
    )
  )
  
)$fit

## this may be caused by the hh_id is too restrictive






