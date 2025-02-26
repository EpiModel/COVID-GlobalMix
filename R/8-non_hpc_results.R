library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra)
# Note: the purpose of this script is to summarize result at the local environment. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("Home","School","Work","Nonhome")
percent_target_pop =  0.5 



# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <-  
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))


## Individual-level summary stats
summary_stats <- 
  readRDS("data/network_stats_attributes/network_params.Rds")

# Load FRP outputs
file.name_frp_r <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",
  network[1],"__",
  percent_target_pop, ".Rds"
)


frp_h_r <- readRDS(file.name_frp_r[1])
frp_s_r <- readRDS(file.name_frp_r[2])
# frp_w_r <- readRDS(file.name_frp_r[3])
# frp_nh_r <- readRDS(file.name_frp_r[4])





source("./R/result_helper_functions.R")

# Summary statistics table, table 1
## Number of nodes, age-specific
n_r_age_grp <- table(tar_stats$attr$rural$node.age.grp)
n_u_age_grp <- table(tar_stats$attr$urban$node.age.grp)

network_stats_tb <- 
  network_stats(tar_stats, summary_stats, n_r_age_grp, n_u_age_grp)

# write.csv(
# network_stats_tb,
# "netstats.tb.csv"
# )

network_stats_tb

# Table 2, proportion of population reached
## Create a variable of nodal ID
tar_stats$attr$rural <- tar_stats$attr$rural %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))
tar_stats$attr$urban <- tar_stats$attr$urban %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))


prop_reached_h_r <- prop_reached_s_r <- prop_reached_w_r <- prop_reached_nh_r <- 
  prop_reached_h_u <- prop_reached_s_u <- prop_reached_w_u <- prop_reached_nh_u <- list()

## Calaulate proportion of popoulation for each node
n_reps=10

for (i in 1:n_reps) {
  
  prop_reached_h_r[[i]] <-
    frp_length_df_process(
      attr = tar_stats$attr$rural %>% select(node_id, node.age.grp), 
      frp_length = frp_h_r[[i]]
    )
  
  prop_reached_s_r[[i]] <-
    frp_length_df_process(
      attr = tar_stats$attr$rural %>% select(node_id, node.age.grp), 
      frp_length = frp_s_r[[i]]
    )
  
}

## For each n_reps, calculate the mean proportion of population reached at day 30, 180, and 365
prop_reached_h_r_df <- bind_rows(prop_reached_h_r, .id = "n_reps") %>% 
  select(n_reps, node_id, node.age.grp, step_30, step_180, step_365) %>% 
  mutate(n_reps = as.numeric(n_reps)
         )

prop_reached_h_r_overall <- 
prop_reached_h_r_df %>% 
  group_by(n_reps) %>% 
  # calculate mean under each n_reps
  summarize(step_30_avg = mean(step_30),
            step_180_avg = mean(step_180),
            step_365_avg = mean(step_365)
            ) %>% 
  ungroup() %>% 
  # get the ntile across n_reps
  summarize(
    step_30_2.5 = quantile(step_30_avg, probs = 0.025),
    step_30_50  = quantile(step_30_avg, probs = 0.5),
    step_30_97.5 = quantile(step_30_avg, probs = 0.975),
    step_180_2.5 = quantile(step_180_avg, probs = 0.025),
    step_180_50  = quantile(step_180_avg, probs = 0.5),
    step_180_97.5 = quantile(step_180_avg, probs = 0.975),
    step_365_2.5 = quantile(step_365_avg, probs = 0.025),
    step_365_50  = quantile(step_365_avg, probs = 0.5),
    step_365_97.5 = quantile(step_365_avg, probs = 0.975)
  ) %>% 
  # covert estimates to publication format
  mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>% # convert proportion to percentile
  mutate(est_30 = paste0(step_30_50, " (", step_30_2.5, ", ", step_30_97.5, ")"),
         est_180 = paste0(step_180_50, " (", step_180_2.5, ", ", step_180_97.5, ")"),
         est_365 = paste0(step_365_50, " (", step_365_2.5, ", ", step_365_97.5, ")")
  ) %>% 
  mutate(node.age.grp ="Overall") %>% 
  select(node.age.grp, est_30, est_180, est_365)

prop_reached_h_r_age_spec <- 
  prop_reached_h_r_df %>% group_by(n_reps, node.age.grp) %>% 
  # calculate mean under each n_reps and node.age.grp
  summarize(step_30_avg = mean(step_30),
            step_180_avg = mean(step_180),
            step_365_avg = mean(step_365)
  ) %>% 
  ungroup() %>% 
  # get the ntile across n_reps for each age group
  group_by(node.age.grp) %>% 
  summarize(
    step_30_2.5 = quantile(step_30_avg, probs = 0.025),
    step_30_50  = quantile(step_30_avg, probs = 0.5),
    step_30_97.5 = quantile(step_30_avg, probs = 0.975),
    step_180_2.5 = quantile(step_180_avg, probs = 0.025),
    step_180_50  = quantile(step_180_avg, probs = 0.5),
    step_180_97.5 = quantile(step_180_avg, probs = 0.975),
    step_365_2.5 = quantile(step_365_avg, probs = 0.025),
    step_365_50  = quantile(step_365_avg, probs = 0.5),
    step_365_97.5 = quantile(step_365_avg, probs = 0.975)
  ) %>% 
  ungroup() %>% 
# covert estimates to publication format
  mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>% # convert proportion to percentile
  mutate(est_30 = paste0(step_30_50, " (", step_30_2.5, ", ", step_30_97.5, ")"),
         est_180 = paste0(step_180_50, " (", step_180_2.5, ", ", step_180_97.5, ")"),
         est_365 = paste0(step_365_50, " (", step_365_2.5, ", ", step_365_97.5, ")")
         ) %>% 
  select(node.age.grp, est_30, est_180, est_365)
  

rbind(prop_reached_h_r_overall, prop_reached_h_r_age_spec)




# do this for each age group

# should denominator in the age-spcific analysis be the size of the age grp?






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


