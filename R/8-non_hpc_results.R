library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra);library(EpiModel)
# Note: the purpose of this script is to summarize result at the local environment. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("Home","School","Work","Nonhome")
percent_target_pop =  0.1
n_reps=100


source("./R/result_helper_functions.R")

#------------------------------------------------------------------------------#
# Loading and processing data
#------------------------------------------------------------------------------#
# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <-  
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))

## Individual-level summary stats
summary_stats <- 
  readRDS("data/network_stats_attributes/network_params.Rds")

# Load netdx outputs
file.name_dx_r <- paste0(
  "data/netdx_outputs/dx_",
  layers[-1], "__",
  network[1],"__",
  "mcmle", "__",
  percent_target_pop,".Rds"
)

file.name_dx_u <- paste0(
  "data/netdx_outputs/dx_",
  layers[-1], "__",
  network[2],"__",
  "mcmle", "__",
  percent_target_pop,".Rds"
)

dx_s_r <- readRDS(file.name_dx_r[1])
dx_w_r <- readRDS(file.name_dx_r[2])
dx_nh_r <- readRDS(file.name_dx_r[3])

dx_s_u <- readRDS(file.name_dx_u[1])
dx_w_u <- readRDS(file.name_dx_u[2])
dx_nh_u <- readRDS(file.name_dx_u[3])

# Load processed FRP outputs—proportion of population reached (100 simulations)
## Rural
file.name_frp_r <- paste0(
  "data/frp_outputs_processed/frp_processed_",
  layers, "__",
  network[1],"__",
  percent_target_pop, "__",
  n_reps,".Rds"
)

frp_h_r <- readRDS(file.name_frp_r[1])
frp_s_r <- readRDS(file.name_frp_r[2])
frp_w_r <- readRDS(file.name_frp_r[3])
frp_nh_r <- readRDS(file.name_frp_r[4])

## Urban
file.name_frp_u <- paste0(
  "data/frp_outputs_processed/frp_processed_",
  layers, "__",
  network[2],"__",
  percent_target_pop, "__",
  n_reps,".Rds"
)

frp_h_u <- readRDS(file.name_frp_u[1])
frp_s_u <- readRDS(file.name_frp_u[2])
frp_w_u <- readRDS(file.name_frp_u[3])
frp_nh_u <- readRDS(file.name_frp_u[4])


# Load unprocessed FRP outputs (1 simuation) from the 4th simulation among the 100 simulation
## Rural
file.name_frp_unprocess_r <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",
  network[1],"__",
  percent_target_pop,".Rds"
)


### node id and age groups
node_id_age_r <- 
  tar_stats$attr[[tolower(network[[1]])]] %>%
  rownames_to_column(var = "node_id") %>% 
  mutate(node_id = paste0("node_", node_id)) %>%  select(node_id, node.age.grp)

## Urban
file.name_frp_unprocess_u <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",
  network[2],"__",
  percent_target_pop,".Rds"
)

node_id_age_u <- 
  tar_stats$attr[[tolower(network[[2]])]] %>%
  rownames_to_column(var = "node_id") %>% 
  mutate(node_id = paste0("node_", node_id)) %>%  select(node_id, node.age.grp)

# Processed the raw FRP outputs to proportion of population reached (1 simuation)
## rural
frp_1sim_h_r <- 
  readRDS(file.name_frp_unprocess_r[1])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

frp_1sim_s_r <- 
  readRDS(file.name_frp_unprocess_r[2])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

frp_1sim_w_r <- 
  readRDS(file.name_frp_unprocess_r[3])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

frp_1sim_nh_r <- 
  readRDS(file.name_frp_unprocess_r[4])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

## urban
frp_1sim_h_u <- 
  readRDS(file.name_frp_unprocess_u[1])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )

frp_1sim_s_u <- 
  readRDS(file.name_frp_unprocess_u[2])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
frp_1sim_w_u <- 
  readRDS(file.name_frp_unprocess_u[3])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
frp_1sim_nh_u <- 
  readRDS(file.name_frp_unprocess_u[4])[[4]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
#------------------------------------------------------------------------------#
# Compiling tables for the main text and appendix
#------------------------------------------------------------------------------#
# Summary statistics table, table 1
## Number of nodes, age-specific
n_r_age_grp <- table(tar_stats$attr$rural$node.age.grp)
n_u_age_grp <- table(tar_stats$attr$urban$node.age.grp)

sum(n_r_age_grp)
sum(n_u_age_grp)

network_stats_tb <- 
  network_stats(tar_stats, summary_stats, n_r_age_grp, n_u_age_grp)

network_stats_tb
# write.csv(
# network_stats_tb,
# "netstats.tb.csv"
# )

# Table S1 and Table S2, Target statistics and dx
tar_stats_tb <- 
  dx_s_r$stats.table.formation %>% 
  rbind(., dx_s_r$stats.table.duration) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>%  
  select(Target, `Pct Diff`) %>% mutate(out_s_r = paste0(Target, " (", abs(`Pct Diff`), ")")) %>% 
  select(out_s_r) %>% 
  tibble::rownames_to_column(., var = "stat") %>% full_join(.,
                                                            
                                                            dx_w_r$stats.table.formation %>% 
                                                              rbind(., dx_w_r$stats.table.duration) %>% 
                                                              mutate_if(is.numeric, ~ round(., 2)) %>%  
                                                              select(Target, `Pct Diff`) %>% mutate(out_w_r = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
                                                              select(out_w_r) %>% 
                                                              tibble::rownames_to_column(., var = "stat"),
                                                            by ="stat"
  )  %>% full_join(.,
                   
                   dx_nh_r$stats.table.formation %>% 
                     rbind(., dx_nh_r$stats.table.duration) %>% 
                     mutate_if(is.numeric, ~ round(., 2)) %>%  
                     select(Target, `Pct Diff`) %>% mutate(out_nh_r = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
                     select(out_nh_r) %>% 
                     tibble::rownames_to_column(., var = "stat"),
                   by ="stat")%>% full_join(.,
                                            
                                            dx_s_u$stats.table.formation %>% 
                                              rbind(., dx_s_u$stats.table.duration) %>% 
                                              mutate_if(is.numeric, ~ round(., 2)) %>%  
                                              select(Target, `Pct Diff`) %>% mutate(out_s_u = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
                                              select(out_s_u) %>% 
                                              tibble::rownames_to_column(., var = "stat"),
                                            by ="stat") %>% 
  full_join(.,
            dx_w_u$stats.table.formation %>% 
              rbind(., dx_w_u$stats.table.duration) %>% 
              mutate_if(is.numeric, ~ round(., 2)) %>%  
              select(Target, `Pct Diff`) %>% mutate(out_w_u = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
              select(out_w_u) %>% 
              tibble::rownames_to_column(., var = "stat"),
            by ="stat") %>% 
  full_join(.,
            dx_nh_u$stats.table.formation %>% 
              rbind(., dx_nh_u$stats.table.duration) %>% 
              mutate_if(is.numeric, ~ round(., 2)) %>%  
              select(Target, `Pct Diff`) %>% mutate(out_nh_u = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
              select(out_nh_u) %>% 
              tibble::rownames_to_column(., var = "stat"),
            by ="stat") 


# relocate rows
row_0_0 <- which(tar_stats_tb$stat == "mix.age.grp.0-9y.0-9y")

tar_stats_tb <- tar_stats_tb %>%
  slice(c(1, row_0_0, setdiff(1:n(), c(1, row_0_0))))

# adding target statistics as reference group back
tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.0-9y.0-9y"), ]$out_s_r <- tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School$`0-9y`[1] %>% round(.,2)
tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.20-29y.20-29y"), ]$out_w_r <- tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work$`20-29y`[3]%>% round(.,2)
tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.0-9y.0-9y"), ]$out_nh_r <- tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome$`0-9y`[1] %>% round(.,2)

tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.0-9y.0-9y"), ]$out_s_u <- tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School$`0-9y`[1] %>% round(.,2)
tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.20-29y.20-29y"), ]$out_w_u <- tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work$`20-29y`[3]%>% round(.,2)
tar_stats_tb[which(tar_stats_tb$stat == "mix.age.grp.0-9y.0-9y"), ]$out_nh_u <- tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Nonhome$`0-9y`[1] %>% round(.,2)

# Number of node with contact at nonhome layer
tar_stats_tb[which(tar_stats_tb$stat == "nodefactor.no.contact.1"), ]$out_nh_r <- as.numeric((tar_stats$attr$rural$contact_attribute_Nonhome %>% table())[2])
tar_stats_tb[which(tar_stats_tb$stat == "nodefactor.no.contact.1"), ]$out_nh_u <- as.numeric((tar_stats$attr$urban$contact_attribute_Nonhome %>% table())[2])

tar_stats_tb <- 
  tar_stats_tb%>% 
  slice(c(1:23, 25, 24))

tar_stats_tb 

#write.csv(tar_stats_tb, "~/Desktop/test.csv")

# Tables S3-S6, avg proportion of population reached across 100 simulations
prop_reach <- 
cbind(
frp_h_r$prop_tb %>% rename(est_30_h = est_30, est_180_h =est_180, est_365_h = est_365),
frp_s_r$prop_tb %>% select(-node.age.grp)%>% rename(est_30_s = est_30, est_180_s =est_180, est_365_s = est_365),
frp_w_r$prop_tb %>% select(-node.age.grp)%>% rename(est_30_w = est_30, est_180_w =est_180, est_365_w = est_365),
frp_nh_r$prop_tb %>% select(-node.age.grp)%>% rename(est_30_nh = est_30, est_180_nh =est_180, est_365_nh = est_365)
) %>% mutate(network = "r") %>% rbind(., 

cbind(
  frp_h_u$prop_tb %>% rename(est_30_h = est_30, est_180_h =est_180, est_365_h = est_365),
  frp_s_u$prop_tb %>% select(-node.age.grp)%>% rename(est_30_s = est_30, est_180_s =est_180, est_365_s = est_365),
  frp_w_u$prop_tb %>% select(-node.age.grp)%>% rename(est_30_w = est_30, est_180_w =est_180, est_365_w = est_365),
  frp_nh_u$prop_tb %>% select(-node.age.grp)%>% rename(est_30_nh = est_30, est_180_nh =est_180, est_365_nh = est_365)
) %>% mutate(network = "u")
)

#write.csv(prop_reach, "~/Desktop/prop_reach.csv")
prop_reach


# Table S1. Percentage of population reached from 1 network simulation
## rural
tbs1 <- 
prop_table_layer_1_iter(prop_reached = frp_1sim_h_r) %>% 
  select(d, mean_sd,  median_quantiles) %>% rename(mean_sd_h = mean_sd, median_quantiles_h = median_quantiles) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_s_r) %>% select(-d)  %>% rename(mean_sd_s = mean_sd, median_quantiles_s = median_quantiles)
) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_w_r) %>% select(-d) %>% rename(mean_sd_w = mean_sd, median_quantiles_w = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_nh_r) %>% select(-d) %>% rename(mean_sd_nh = mean_sd, median_quantiles_nh = median_quantiles)
  ) %>% mutate(network = "r") %>% rbind(.,


## urban
prop_table_layer_1_iter(prop_reached = frp_1sim_h_u) %>% 
  select(d, mean_sd,  median_quantiles) %>% rename(mean_sd_h = mean_sd, median_quantiles_h = median_quantiles) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_s_u) %>% select(-d)  %>% rename(mean_sd_s = mean_sd, median_quantiles_s = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_w_u) %>% select(-d) %>% rename(mean_sd_w = mean_sd, median_quantiles_w = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_1sim_nh_u) %>% select(-d) %>% rename(mean_sd_nh = mean_sd, median_quantiles_nh = median_quantiles)
  ) %>% mutate(network = "u")
)

#write.csv(tbs1, "~/Desktop/tbs1.csv")




# Figure 1 (Percentages of populations reached over a 1-year period)
frp_layers <-
rbind(
frp_h_r$prop_figure_df %>% mutate(layer = "Rural home"), # unit: proportion
frp_s_r$prop_figure_df %>% mutate(layer = "Rural school"),
frp_w_r$prop_figure_df %>% mutate(layer = "Rural work"),
frp_nh_r$prop_figure_df %>% mutate(layer = "Rural other"),

frp_h_u$prop_figure_df %>% mutate(layer = "Urban home"), # unit: proportion
frp_s_u$prop_figure_df %>% mutate(layer = "Urban school"),
frp_w_u$prop_figure_df %>% mutate(layer = "Urban work"),
frp_nh_u$prop_figure_df %>% mutate(layer = "Urban other")

) %>%
  mutate(quantile_2.5 = quantile_2.5*100, # convert to percentile
         quantile_50 = quantile_50*100,
         quantile_97.5 = quantile_97.5*100
         ) %>%
  mutate(layer = factor(layer, levels = c( "Rural home",    "Rural school",  "Rural work",    "Rural other", "Urban home"  ,  "Urban school" , "Urban work", "Urban other")
                        )
         )



ggplot(
       frp_layers ,
       aes(x = step, y = quantile_50, color = node.age.grp, group = node.age.grp)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = quantile_2.5, ymax = quantile_97.5, fill = node.age.grp), alpha = 0.1, 
              color = NA, show.legend = F) +
  facet_wrap(~ layer, , scales = "free_y",
             nrow = 2, ncol = 4,) +
  theme_light() +
  labs(x = "Day", y = "Percentage of population reached (%)", color = "Age group (years)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1))+
  scale_x_continuous(
    limits = c(0, NA),
    breaks = c(0, seq(100, max(frp_layers$step, na.rm = TRUE), 100))
  )+
  scale_color_viridis_d(
    labels = function(x) sub("y$", "", x)
  ) +    
  scale_fill_viridis_d(
    labels = function(x) sub("y$", "", x)
  )

# in-house edit, final Figure 2 
plot_df <- frp_layers %>%
  mutate(setting = sub("^(Rural|Urban)\\s+", "", layer))

ylim_df <- plot_df %>%
  group_by(setting) %>%
  summarise(
    ymin = 0,
    ymax = max(quantile_97.5, na.rm = TRUE) * 1.05,
    .groups = "drop"
  ) %>%
  inner_join(distinct(plot_df, layer, setting), by = "setting")

ggplot(
  plot_df,
  aes(x = step, y = quantile_50, color = node.age.grp, group = node.age.grp)
) +
  geom_blank(
    data = ylim_df,
    aes(x = 0, y = ymin),
    inherit.aes = FALSE
  ) +
  geom_blank(
    data = ylim_df,
    aes(x = 0, y = ymax),
    inherit.aes = FALSE
  ) +
  geom_line(size = 0.7) +
  geom_ribbon(
    aes(ymin = quantile_2.5, ymax = quantile_97.5, fill = node.age.grp),
    alpha = 0.1,
    color = NA,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ layer,
    scales = "free_y",
    nrow = 2,
    ncol = 4
  ) +
  theme_light() +
  labs(
    x = "Day",
    y = "Percentage of population reached (%)",
    color = "Age group (years)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    
    strip.text = element_text(color = "black", size = 14)
  ) +
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = c(0, seq(100, max(plot_df$step, na.rm = TRUE), 100))
  ) +
  scale_color_viridis_d(
    labels = function(x) sub("y$", "", x)
  ) +
  scale_fill_viridis_d(
    labels = function(x) sub("y$", "", x)
  )

# Figure S2 (Proportion of mixing)

df <- 
  rbind(
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Home$Home_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural home") %>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural school")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Work$Work_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural work")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm%>% mutate(layer = "Rural other")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Home$Home_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban home") %>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$School$School_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban school")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban work")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm%>% mutate(layer = "Urban other")%>% rownames_to_column(., var = "ego")


)%>%
  mutate(across(everything(), ~ replace_na(.x, 0)),
         layer = factor(layer, c( "Rural home", "Rural school",  "Rural work", "Rural other", "Urban home", "Urban school", "Urban work", "Urban other" )
         )
         ) 
  

plot_heatmap_df(df= df)

## main text writing, max value at school and work
### rural school and work
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% max(., na.rm = T) %>% round(., 2)
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Work$Work_mix_prop_matrix_2d_glm  %>% max(., na.rm = T) %>% round(., 2)

summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$School$School_mix_prop_matrix_2d_glm %>% max(., na.rm = T) %>% round(., 2)
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm %>% max(., na.rm = T) %>% round(., 2)


summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm== summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm %>% max(., na.rm = T) 
# Figure - boxplot of FRP at day 180 over 100 simulations
frp_layers_d180 <- 
  bind_rows(
    frp_h_r$prop_d180_df %>% mutate(layer = "Home", network = "Rural", layer_network = "Rural home"), # unit: proportion
    frp_s_r$prop_d180_df %>% mutate(layer = "School", network = "Rural", layer_network = "Rural school"),
    frp_w_r$prop_d180_df %>% mutate(layer = "Work", network = "Rural",  layer_network = "Rural work"),
    frp_nh_r$prop_d180_df %>% mutate(layer = "Other", network = "Rural",  layer_network = "Rural other"),
    
    frp_h_u$prop_d180_df %>% mutate(layer = "Home", network = "Urban", layer_network = "Urban home"), # unit: proportion
    frp_s_u$prop_d180_df %>% mutate(layer = "School", network = "Urban", layer_network = "Urban school"),
    frp_w_u$prop_d180_df %>% mutate(layer = "Work", network = "Urban", layer_network = "Urban work"),
    frp_nh_u$prop_d180_df %>% mutate(layer = "Other", network = "Urban", layer_network = "Urban other")
    
  ) %>% mutate(step_180_avg = step_180_avg*100,
               network = factor(network, levels = c("Rural", "Urban")),
               layer = factor(layer, levels = c("Home", "School", "Work", "Other")),
               layer_network = factor(layer_network, c( "Rural home",    "Rural school",  "Rural work",    "Rural other", "Urban home"  ,  "Urban school" , "Urban work", "Urban other"   )
                                      )
               )



## each layer as a single panel
ggplot(frp_layers_d180 , 
       aes(x = node.age.grp, y = step_180_avg )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  #geom_jitter(position = position_jitter(width = 0.2), alpha = 0.05) +
  facet_wrap(~ layer_network, ncol = 4, nrow =2, scales = "free_y") +
  labs(
    x = "Age group",
    y = "Percentage of population reached (%)",
    fill = "Age group"
  ) +
  theme_light()+
  theme(strip.text = element_text(size = 10))





# Figure - histogram of FRP at day 180 from 1 simulation

frp_layers_1_sim <- # age-specific FRP
  bind_rows(
    frp_1sim_h_r %>% mutate(layer = "Home", network = "Rural", layer_network = "Rural home"), # unit: proportion
    frp_1sim_s_r %>% mutate(layer = "School", network = "Rural", layer_network = "Rural school"),
    frp_1sim_w_r %>% mutate(layer = "Work", network = "Rural",  layer_network = "Rural work"),
    frp_1sim_nh_r %>% mutate(layer = "Other", network = "Rural",  layer_network = "Rural other"),
    
    frp_1sim_h_u %>% mutate(layer = "Home", network = "Urban", layer_network = "Urban home"), # unit: proportion
    frp_1sim_s_u %>% mutate(layer = "School", network = "Urban", layer_network = "Urban school"),
    frp_1sim_w_u %>% mutate(layer = "Work", network = "Urban", layer_network = "Urban work"),
    frp_1sim_nh_u %>% mutate(layer = "Other", network = "Urban", layer_network = "Urban other")
    
  )  %>%
  select(node_id, node.age.grp, layer, network, layer_network, starts_with("step_")) %>% 
  mutate(
    across(matches("^step_\\d{1,3}$"), ~ . * 100),
    network = factor(network, levels = c("Rural", "Urban")),
    layer = factor(layer, levels = c("Home", "School", "Work", "Other")),
    layer_network = factor(layer_network, levels = c("Rural home", "Rural school", "Rural work", "Rural other",
                                                     "Urban home", "Urban school", "Urban work", "Urban other"))
  )


frp_layers_1_sim_overallage <- # overall FRP
  frp_layers_1_sim %>% mutate(node.age.grp = "overall")
  
# ggplot(frp_layers_1_sim, aes(x = step_180)) +
#   geom_histogram(bins = 30) +
#   facet_grid(node.age.grp ~ layer_network, scales = "free") +
#   labs(
#     x = "step_180 (%)",
#     y = "Count"
#   ) +
#   theme_light()





ggplot(frp_layers_1_sim %>%
         mutate(layer = factor(layer, levels = c("Home", "School", "Work", "Other"))),
       aes(x = step_180, fill = network)) +
  geom_histogram(position = "dodge") +
  facet_grid(node.age.grp ~ layer, scales = "free") +
  labs(
    x = "Percentage of population reached",
    y = "Frequency",
    fill = "Network"
  ) +
  scale_fill_viridis_d() +
  theme_light() +
  theme(legend.position = "bottom")







# Figure - line plot of the FRP of each node across all layers from 1 simulation
frp_layers_1_sim %>%
  pivot_longer(
    cols = starts_with("step_"),
    names_to = "step",
    names_prefix = "step_",
    values_to = "value"
  ) %>%
  filter(step >=1) %>% 
  mutate(step = as.integer(step)) %>% 
  ggplot(., aes(x = step, y = value, group = node_id, color = node.age.grp)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ layer_network, nrow = 2, ncol = 4, scales = "free_y") +
  labs(
    title = "",
    x = "Day",
    y = "Percentage of population reached (%)",
    color = "Age group (years)"
  ) +
  scale_color_viridis_d() +    
  scale_fill_viridis_d() +
  theme_light() +
  theme(legend.position = "bottom")








