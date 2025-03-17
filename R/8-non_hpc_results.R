library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra);library(EpiModel)
# Note: the purpose of this script is to summarize result at the local environment. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("Home","School","Work","Nonhome")
percent_target_pop =  0.1
n_reps=100


source("./R/result_helper_functions.R")

# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <-  
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))


## Individual-level summary stats
summary_stats <- 
  readRDS("data/network_stats_attributes/network_params.Rds")

# Load processed FRP outputs
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


# Load unprocessed FRP outputs
## Rural
file.name_frp_unprocess_r <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",
  network[1],"__",
  percent_target_pop,".Rds"
)

node_id_age_r <- 
  tar_stats$attr[[tolower(network[[1]])]] %>%
  rownames_to_column(var = "node_id") %>% 
  mutate(node_id = paste0("node_", node_id)) %>%  select(node_id, node.age.grp)

frp_unprocess_h_r <- 
  readRDS(file.name_frp_unprocess_r[1])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )
  
frp_unprocess_s_r <- 
  readRDS(file.name_frp_unprocess_r[2])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

frp_unprocess_w_r <- 
  readRDS(file.name_frp_unprocess_r[3])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )

frp_unprocess_nh_r <- 
  readRDS(file.name_frp_unprocess_r[4])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_r, 
    frp_length = .
  )


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

frp_unprocess_h_u <- 
  readRDS(file.name_frp_unprocess_u[1])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )

frp_unprocess_s_u <- 
  readRDS(file.name_frp_unprocess_u[2])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
frp_unprocess_w_u <- 
  readRDS(file.name_frp_unprocess_u[3])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
frp_unprocess_nh_u <- 
  readRDS(file.name_frp_unprocess_u[4])[[2]] %>% 
  frp_length_df_process(
    attr = node_id_age_u, 
    frp_length = .
  )
  
 

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


# Table S1. Percentage of population reached from 1 network simulation
## rural
tbs1 <- 
prop_table_layer_1_iter(prop_reached = frp_unprocess_h_r) %>% 
  select(d, mean_sd,  median_quantiles) %>% rename(mean_sd_h = mean_sd, median_quantiles_h = median_quantiles) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_s_r) %>% select(-d)  %>% rename(mean_sd_s = mean_sd, median_quantiles_s = median_quantiles)
) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_w_r) %>% select(-d) %>% rename(mean_sd_w = mean_sd, median_quantiles_w = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_nh_r) %>% select(-d) %>% rename(mean_sd_nh = mean_sd, median_quantiles_nh = median_quantiles)
  ) %>% mutate(network = "r") %>% rbind(.,


## urban
prop_table_layer_1_iter(prop_reached = frp_unprocess_h_u) %>% 
  select(d, mean_sd,  median_quantiles) %>% rename(mean_sd_h = mean_sd, median_quantiles_h = median_quantiles) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_s_u) %>% select(-d)  %>% rename(mean_sd_s = mean_sd, median_quantiles_s = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_w_u) %>% select(-d) %>% rename(mean_sd_w = mean_sd, median_quantiles_w = median_quantiles)
  ) %>% 
  cbind(
    prop_table_layer_1_iter(prop_reached = frp_unprocess_nh_u) %>% select(-d) %>% rename(mean_sd_nh = mean_sd, median_quantiles_nh = median_quantiles)
  ) %>% mutate(network = "u")
)

write.csv(tbs1, "~/Desktop/tbs1.csv")


# Table S2, Target statistics and dx
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


write.csv(tar_stats_tb, "~/Desktop/test.csv")

# Figure 1 (Percentages of populations reached over a 1-year period)
# frp_layers <- 
# rbind(
# frp_h_r$prop_figure_df %>% mutate(layer = "Home", network = "Rural"), # unit: proportion
# frp_s_r$prop_figure_df %>% mutate(layer = "School", network = "Rural"),
# frp_w_r$prop_figure_df %>% mutate(layer = "Work", network = "Rural"),
# frp_nh_r$prop_figure_df %>% mutate(layer = "Nonhome", network = "Rural"),
# 
# frp_h_u$prop_figure_df %>% mutate(layer = "Home", network = "Urban"), # unit: proportion
# frp_s_u$prop_figure_df %>% mutate(layer = "School", network = "Urban"),
# frp_w_u$prop_figure_df %>% mutate(layer = "Work", network = "Urban"),
# frp_nh_u$prop_figure_df %>% mutate(layer = "Nonhome", network = "Urban")
# 
# ) %>% 
#   mutate(quantile_2.5 = quantile_2.5*100, # convert to percentile
#          quantile_50 = quantile_50*100,
#          quantile_97.5 = quantile_97.5*100
#          ) %>% 
#   mutate(layer = factor(layer, levels = c( "Rural home",    "Rural school",  "Rural work",    "Rural nonhome", "Urban home"  ,  "Urban school" , "Urban work", "Urban nonhome"   )
#                         )
#          )


ggplot(frp_layers ,
       aes(x = step, y = quantile_50, color = node.age.grp, group = node.age.grp)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = quantile_2.5, ymax = quantile_97.5, fill = node.age.grp), alpha = 0.1, 
              color = NA, show.legend = F) +
  facet_wrap(~ layer, , scales = "free_y",
             nrow = 2, ncol = 4,) +
  theme_classic() +
  labs(x = "Day", y = "Percentage", color = "Age group")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1),
         fill = guide_legend(nrow = 1))+
  scale_color_viridis_d() +    
  scale_fill_viridis_d() 


# Figure S1 (Proportion of mixing)

df <- 
  rbind(
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Home$Home_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural home") %>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural school")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Work$Work_mix_prop_matrix_2d_glm %>% mutate(layer = "Rural work")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm%>% mutate(layer = "Rural nonhome")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Home$Home_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban home") %>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$School$School_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban school")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm %>% mutate(layer = "Urban work")%>% rownames_to_column(., var = "ego"),
summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm%>% mutate(layer = "Urban nonhome")%>% rownames_to_column(., var = "ego")


)%>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

plot_heatmap_df(df= df)


# Figure - boxplot of FRP at day 180
frp_layers_d180 <- 
  bind_rows(
    frp_h_r$prop_d180_df %>% mutate(layer = "Home", network = "Rural", layer_network = "Rural home"), # unit: proportion
    frp_s_r$prop_d180_df %>% mutate(layer = "School", network = "Rural", layer_network = "Rural school"),
    frp_w_r$prop_d180_df %>% mutate(layer = "Work", network = "Rural",  layer_network = "Rural work"),
    frp_nh_r$prop_d180_df %>% mutate(layer = "Nonhome", network = "Rural",  layer_network = "Rural nonhome"),
    
    frp_h_u$prop_d180_df %>% mutate(layer = "Home", network = "Urban", layer_network = "Urban home"), # unit: proportion
    frp_s_u$prop_d180_df %>% mutate(layer = "School", network = "Urban", layer_network = "Urban school"),
    frp_w_u$prop_d180_df %>% mutate(layer = "Work", network = "Urban", layer_network = "Urban work"),
    frp_nh_u$prop_d180_df %>% mutate(layer = "Nonhome", network = "Urban", layer_network = "Urban nonhome")
    
  ) %>% mutate(step_180_avg = step_180_avg*100,
               network = factor(network, levels = c("Rural", "Urban")),
               layer = factor(layer, levels = c("Home", "School", "Work", "Nonhome")),
               layer_network = factor(layer_network, c( "Rural home",    "Rural school",  "Rural work",    "Rural nonhome", "Urban home"  ,  "Urban school" , "Urban work", "Urban nonhome"   )
                                      )
               )

#box plot
# layer as top hirarchy
ggplot(frp_layers_d180 , 
       aes(x = network, y = step_180_avg, fill = node.age.grp)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ layer, ncol = 2, scales = "free_y") +
  labs(
    x = "Network",
    y = "Percentage of population reached (%)",
    fill = "Age group"
  ) +
  theme_minimal()

# each layer as a single panel
ggplot(frp_layers_d180 , 
       aes(x = node.age.grp, y = step_180_avg )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ layer_network, ncol = 4, nrow =2, scales = "free_y") +
  labs(
    x = "Age group",
    y = "Percentage of population reached (%)",
    fill = "Age group"
  ) +
  theme_minimal()


# ## Check component size at home
# nw_h_r <- 
#   readRDS("./data/netest_outputs/deterministic_Home__Rural__0.1.Rds")
# 
# nw_h_u <- 
#   readRDS("./data/netest_outputs/deterministic_Home__Urban__0.1.Rds")
# 
# 
# component_h_r <- sna::component.dist(nw_h_r); component_h_u<- sna::component.dist(nw_h_u)
# 
# component_h_r$csize %>% summary; component_h_u$csize %>% summary
# 
# ### tabulate FRP with component size
# 
# 
# 
# frp_length_h_r_365 <- 
#   frp_length_h_r$crude %>% select(node_id, step_365)
# 
# frp_length_h_r_365$component_node_id <- component_h_r$membership
# 
# component_h_r$csize %>% unique %>% sort
# frp_length_h_r_365$step_365 %>% unique %>% sort



