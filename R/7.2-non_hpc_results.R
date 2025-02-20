library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra)
# Note: the purpose of this script is to summarize result at the local environment. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop =  1 #/1/0.1



# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <-  
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))


## Individual-level summary stats
summary_stats <- 
  readRDS("data/network_stats_attributes/network_params.Rds")

## Number of nodes
n_r <- nrow(tar_stats$attr$rural)
n_u <- nrow(tar_stats$attr$urban)

## Number of nodes, age-specific
n_r_age_grp <- table(tar_stats$attr$rural$node.age.grp)
n_u_age_grp <- table(tar_stats$attr$urban$node.age.grp)




source("./R/result_helper_functions.R")



# Summary statistics table, table 1
network_stats_tb <- 
  network_stats(tar_stats, summary_stats, n_r_age_grp, n_u_age_grp)

write.csv(
network_stats_tb,
"netstats.tb.csv"
)


## Check component size at home
nw_h_r <- 
  readRDS("./data/netest_outputs/deterministic_Home__Rural__0.1.Rds")

nw_h_u <- 
  readRDS("./data/netest_outputs/deterministic_Home__Urban__0.1.Rds")


component_h_r <- sna::component.dist(nw_h_r); component_h_u<- sna::component.dist(nw_h_u)

component_h_r$csize %>% summary; component_h_u$csize %>% summary

### tabulate FRP with component size
frp_length_h_r_365 <- 
  frp_length_h_r$crude %>% select(node_id, step_365)

frp_length_h_r_365$component_node_id <- component_h_r$membership

component_h_r$csize %>% unique %>% sort
frp_length_h_r_365$step_365 %>% unique %>% sort



# FRP length per n people at different time steps
## rural
rbind(
  frp_moments_layer(frp_length_layer = frp_length_all_r, per_n_people = 10000, layer = "rural_all", N= n_r),
  frp_moments_layer(frp_length_layer = frp_length_h_r, per_n_people = 10000, layer = "rural_home", N= n_r),
  frp_moments_layer(frp_length_layer = frp_length_s_r, per_n_people = 10000, layer = "rural_school", N= n_r),
  frp_moments_layer(frp_length_layer = frp_length_w_r, per_n_people = 10000, layer = "rural_work", N= n_r),
  frp_moments_layer(frp_length_layer = frp_length_nh_r, per_n_people = 10000, layer = "rural_nonhome", N= n_r)
) %>% 
  kbl() %>% 
  kable_classic()

## urban
rbind(
  frp_moments_layer(frp_length_layer = frp_length_all_u, per_n_people = 10000, layer = "urban_all", N=n_u),
  frp_moments_layer(frp_length_layer = frp_length_h_u, per_n_people = 10000, layer = "urban_home", N=n_u),
  frp_moments_layer(frp_length_layer = frp_length_s_u, per_n_people = 10000, layer = "urban_school", N=n_u),
  frp_moments_layer(frp_length_layer = frp_length_w_u, per_n_people = 10000, layer = "urban_work", N=n_u),
  frp_moments_layer(frp_length_layer = frp_length_nh_u, per_n_people = 10000, layer = "urban_nonhome", N=n_u)
) %>% 
  kbl() %>% 
  kable_classic()

(1/n_r)*1e4


# Visualize FRP length, supplementary material
## rural
frp_all_r_plot <- frp_length_plot(frp_length =frp_length_all_r, title = "All, rural")
frp_h_r_plot <- frp_length_plot(frp_length =frp_length_h_r, title = "Home, rural")
frp_s_r_plot <- frp_length_plot(frp_length =frp_length_s_r, title = "School, rural")
frp_w_r_plot <- frp_length_plot(frp_length =frp_length_w_r, title = "Work, rural")
frp_nh_r_plot <- frp_length_plot(frp_length =frp_length_nh_r, title = "Nonhome, rural")

## urban
frp_all_u_plot <- frp_length_plot(frp_length =frp_length_all_u, title = "All, urban")
frp_h_u_plot <- frp_length_plot(frp_length =frp_length_h_u, title = "Home, urban")
frp_s_u_plot <- frp_length_plot(frp_length =frp_length_s_u, title = "School, urban")
frp_w_u_plot <- frp_length_plot(frp_length =frp_length_w_u, title = "Work, urban")
frp_nh_u_plot <- frp_length_plot(frp_length =frp_length_nh_u, title = "Nonhome, urban")

frp_plot <- 
  ggarrange(
    frp_all_r_plot,
    frp_h_r_plot,
    frp_s_r_plot,
    frp_w_r_plot,
    frp_nh_r_plot,
    frp_all_u_plot,
    frp_h_u_plot,
    frp_s_u_plot,
    frp_w_u_plot,
    frp_nh_u_plot,
    
    ncol = 5, nrow = 2,
    common.legend = TRUE, 
    legend = "bottom"
  )


frp_plot


