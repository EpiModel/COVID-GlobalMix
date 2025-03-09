library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra);library(EpiModel)
# Note: the purpose of this script is to summarize result at the local environment. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("Home","School","Work","Nonhome")
percent_target_pop =  0.1
n_reps=100


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
cbind(
frp_h_r$prop_tb,
frp_s_r$prop_tb %>% select(-node.age.grp),
frp_w_r$prop_tb %>% select(-node.age.grp),
frp_nh_r$prop_tb %>% select(-node.age.grp)
)


# Table S2, Target statistics and dx
tar_stats_tb <- 
dx_s_r$stats.table.formation %>% 
  rbind(., dx_s_r$stats.table.duration) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>%  
  select(Target, `Pct Diff`) %>% mutate(out_s_r = paste0(Target, " (", abs(`Pct Diff`), ")")) %>% 
  select(out_s_r) %>% 
  tibble::rownames_to_column(., var = "stat") %>% full_join(.,

dx_w_r$stats.table.formation %>% 
  rbind(., dx_s_r$stats.table.duration) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>%  
  select(Target, `Pct Diff`) %>% mutate(out_w_r = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
  select(out_w_r) %>% 
  tibble::rownames_to_column(., var = "stat"),
by ="stat"
)  %>% full_join(.,

dx_nh_r$stats.table.formation %>% 
  rbind(., dx_s_r$stats.table.duration) %>% 
  mutate_if(is.numeric, ~ round(., 2)) %>%  
  select(Target, `Pct Diff`) %>% mutate(out_nh_r = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
  select(out_nh_r) %>% 
  tibble::rownames_to_column(., var = "stat"),
by ="stat")%>% full_join(.,
                         
                         dx_s_u$stats.table.formation %>% 
                           rbind(., dx_s_r$stats.table.duration) %>% 
                           mutate_if(is.numeric, ~ round(., 2)) %>%  
                           select(Target, `Pct Diff`) %>% mutate(out_s_u = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
                           select(out_s_u) %>% 
                           tibble::rownames_to_column(., var = "stat"),
                         by ="stat") %>% full_join(.,
                
                dx_w_u$stats.table.formation %>% 
                  rbind(., dx_s_r$stats.table.duration) %>% 
                  mutate_if(is.numeric, ~ round(., 2)) %>%  
                  select(Target, `Pct Diff`) %>% mutate(out_w_u = paste0(Target, " (", abs(`Pct Diff`), ")"))%>% 
                  select(out_w_u) %>% 
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

write.csv(tar_stats_tb, "~/Desktop/test.csv")

# Figure 1 (Percentages of populations reached over a 1-year period)
frp_layers <- 
rbind(
frp_h_r$prop_figure_df %>% mutate(layer = "Rural home"), # unit: proportion
frp_s_r$prop_figure_df %>% mutate(layer = "Rural school"),
frp_w_r$prop_figure_df %>% mutate(layer = "Rural work"),
frp_nh_r$prop_figure_df %>% mutate(layer = "Rural nonhome"),

frp_h_u$prop_figure_df %>% mutate(layer = "Urban home"), # unit: proportion
frp_s_u$prop_figure_df %>% mutate(layer = "Urban school"),
frp_w_u$prop_figure_df %>% mutate(layer = "Urban work")#,
#frp_nh_u$prop_figure_df %>% mutate(layer = "Urban nonhome")

) %>% 
  mutate(quantile_2.5 = quantile_2.5*100, # convert to percentile
         quantile_50 = quantile_50*100,
         quantile_97.5 = quantile_97.5*100
         ) 


ggplot(frp_layers %>% filter(layer== "Rural nonhome"),
       aes(x = step, y = quantile_50, color = node.age.grp, group = node.age.grp)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = quantile_2.5, ymax = quantile_97.5, fill = node.age.grp), alpha = 0.1, color = NA) +
  facet_wrap(~ layer, , scales = "free_y") +
  theme_classic() +
  labs(x = "Day", y = "Percentile")+
  theme(plot.title = element_text(hjust = 0.5)) # change y axis to %

# diagnose nonhome rural FRP
frp_nh_r_raw <- readRDS("./data/frp_outputs/frp_length_Nonhome__Rural__0.1.Rds")

frp_nh_r_raw_1_iterat <- data.frame(frp_nh_r_raw[[1]])
frp_nh_r_raw_1_iterat_1 <- 
(frp_nh_r_raw_1_iterat/sum(n_r_age_grp))*100

frp_nh_r_raw_1_iterat_1[,31]%>% summary()
# compare to function-processed
frp_nh_r$prop_tb



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


