library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra)
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop =  0.1 #/1/0.1
nodes =NULL# the number of nodes with edges whose FRPs are calculated, the default setting is NULL, that FRPs for all nodes are calculated



# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <- 
readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))


## Individual-level summary stats
summary_stats <- 
readRDS("data/network_stats_attributes/network_params.Rds")


n_r <- nrow(tar_stats$attr$rural)
n_u <- nrow(tar_stats$attr$urban)


# Loading raw FRP result
file.name_r <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",  network[1],"__",  percent_target_pop, "__", paste0(as.character(nodes)), ".Rds"
)

file.name_u <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",  network[2],"__",  percent_target_pop, "__", paste0(as.character(nodes)), ".Rds"
)



# frp_all_r <- readRDS(file.name_r[1])
frp_h_r <- readRDS(file.name_r[2])
frp_s_r <- readRDS(file.name_r[3])
frp_w_r <- readRDS(file.name_r[4])
# frp_nh_r <- readRDS(file.name_r[5])
 
# frp_all_u <- readRDS(file.name_u[1])
# frp_h_u <- readRDS(file.name_u[2])
# frp_s_u <- readRDS(file.name_u[3])
# frp_w_u <- readRDS(file.name_u[4])
# frp_nh_u <- readRDS(file.name_u[5])


# Extract FRP length dataframe
frp_length_all_r <- frp_length_s_r <- frp_length_w_r <- frp_length_nh_r <- 
  frp_length_all_u <- frp_length_s_u <- frp_length_w_u <- frp_length_nh_u <- list()

## rural
for (i in 1:100) {
  #frp_length_all_r[[i]] <- frp_all_r[[i]]$lengths
  frp_length_s_r[[i]] <- frp_s_r[[i]]$lengths
  frp_length_w_r[[i]] <- frp_w_r[[i]]$lengths
  #frp_length_nh_r[[i]] <- frp_nh_r[[i]]$lengths
}


# Process identifiers of nodal attribute in target statistics
tar_stats$attr$rural <- tar_stats$attr$rural %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))
tar_stats$attr$urban <- tar_stats$attr$urban %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))


source("./R/result_helper_functions.R")



# Process FRP data, converting FRP length to proportion
## rural
frp_length_h_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural,
    frp_length = frp_h_r$lengths,
    denom = n_r
  )


# frp_length_all_r <-
#   frp_length_df_process(
#     attr = tar_stats$attr$rural,
#     frp_length = frp_all_r$lengths,
#     denom = n_r
#   )


for (i in 1:100) {
  

frp_length_s_r[[i]] <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_length_s_r[[i]],
    denom = n_r
  )

frp_length_w_r[[i]] <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_length_w_r[[i]],
    denom = n_r
  )

}

# frp_length_nh_r <-
#   frp_length_df_process(
#     attr = tar_stats$attr$rural, 
#     frp_length = frp_nh_r$lengths,
#     denom = n_r
#   )
# 
# ## urban
# frp_length_all_u <-
#   frp_length_df_process(
#     attr = tar_stats$attr$urban,
#     frp_length = frp_all_u$lengths,
#     denom = n_u
#   )
# 
# 
# frp_length_h_u <-
#   frp_length_df_process(
#     attr = tar_stats$attr$urban,
#     frp_length = frp_h_u$lengths,
#     denom = n_u
#   )
# 
# 
# frp_length_s_u <-
#   frp_length_df_process(
#     attr = tar_stats$attr$urban, 
#     frp_length = frp_s_u$lengths,
#     denom = n_u
#   )
# 
# frp_length_w_u <-
#   frp_length_df_process(
#     attr = tar_stats$attr$urban, 
#     frp_length = frp_w_u$lengths,
#     denom = n_u
#   )
# 
# frp_length_nh_u <-
#   frp_length_df_process(
#     attr = tar_stats$attr$urban, 
#     frp_length = frp_nh_u$lengths,
#     denom = n_u
#   )

# Remove large outputs to save memory
rm(list=
     c(paste0("frp_", c("all", "h", "s", "w", "nh"), "_r"),
       paste0("frp_", c("all", "h", "s", "w", "nh"), "_u")
     )
   )


# Statistical moments of FRP per 10K on day 365 under each simulation
frp365_s_r <- frp365_w_r <- list()

## Normalize to per 10K population
for (i in 1:100) {
frp365_s_r[[i]] <- 
frp_moments_365_layer(frp_length_layer = frp_length_s_r[[i]]$prop %>% select(node.age.grp,node_id, step_365),
                      per_n_people = 10000) 
frp365_w_r[[i]] <- 
frp_moments_365_layer(frp_length_layer = frp_length_w_r[[i]]$prop %>% select(node.age.grp,node_id, step_365),
                      per_n_people = 10000) 

}

## convert lists to dataframes
frp365_s_r <- bind_rows(frp365_s_r, .id = "i")
frp365_w_r <- bind_rows(frp365_w_r, .id = "i")

## distribution for the mean FRP of the each layers across simulation

library(dplyr)

age_group_quantiles <- function(data, age.grp = c("Overall", "0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")) {
  # Create an empty data frame to store results
  results <- data.frame(age.grp = character(),
                        median = numeric(),
                        lower_95_CI = numeric(),
                        upper_95_CI = numeric(),
                        result_string = character(),
                        stringsAsFactors = FALSE)
  
  # Loop through each age group
  for(ag in age.grp) {
    # Filter and extract the Mean values for the current age group
    mean_vals <- data %>% 
      filter(node.age.grp == ag) %>% 
      pull(Mean)
  
    
    # Compute the quantiles: median, lower 2.5% and upper 97.5%
    qs <- quantile(mean_vals, probs = c(0.5, 0.025, 0.975))
    
    # Create a result string combining the quantiles
    result_string <- paste0( qs[1],
                            " (", qs[2],
                            ", ", qs[3], ")")
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(
      age.grp = ag,
      median = qs[1],
      lower_95_CI = qs[2],
      upper_95_CI = qs[3],
      result_string = result_string,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

### rural, school
frp365_s_r_grps <- age_group_quantiles(frp365_s_r)

### rural, work
frp365_w_r_grps <- age_group_quantiles(frp365_w_r)



df <- 
cbind(
  frp365_s_r_grps %>% select(age.grp, result_string),
  frp365_w_r_grps$result_string
) %>% data.frame %>% rename("rural, school" =2, "rural, work"=3)

df

# distribution


par(mfrow = c(1,2))
hist(mean_frp365_s_r, main = "School, rural", xlab = "Standardized FRP length"); abline(v = s_r_365_tile[2], col = "red", lty = 2); abline(v = s_r_365_tile[3], col = "red", lty = 2)
hist(mean_frp365_w_r, main = "Work, rural", xlab = "Standardized FRP length"); abline(v = w_r_365_tile[2], col = "red", lty = 2); abline(v = w_r_365_tile[3], col = "red", lty = 2)


# One-year FRP by age
## rural
cbind(
frp_moments_365_layer(frp_length_layer = frp_length_all_r$prop %>% select(node.age.grp,node_id, step_365),
                      per_n_people = 10000),
frp_moments_365_layer(frp_length_layer = frp_length_h_r$prop, per_n_people = 10000)%>% select(-node.age.grp),
frp_moments_365_layer(frp_length_layer = frp_length_s_r$prop, per_n_people = 10000) %>% select(-node.age.grp),
frp_moments_365_layer(frp_length_layer = frp_length_w_r$prop, per_n_people = 10000) %>% select(-node.age.grp),
frp_moments_365_layer(frp_length_layer = frp_length_nh_r$prop, per_n_people = 10000) %>% select(-node.age.grp)
)%>%  column_to_rownames(var = "node.age.grp") %>%
  kbl() %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1, "All, rural" = 3, "Home, rural" = 3,  "School, rural" = 3, "Work, rural" = 3, "Nonhome, rural" = 3))

## urban
cbind(
  frp_moments_365_layer(frp_length_layer = frp_length_all_u$prop, per_n_people = 10000),
  frp_moments_365_layer(frp_length_layer = frp_length_h_u$prop, per_n_people = 10000)%>% select(-node.age.grp),
  frp_moments_365_layer(frp_length_layer = frp_length_s_u$prop, per_n_people = 10000) %>% select(-node.age.grp),
  frp_moments_365_layer(frp_length_layer = frp_length_w_u$prop, per_n_people = 10000) %>% select(-node.age.grp),
  frp_moments_365_layer(frp_length_layer = frp_length_nh_u$prop, per_n_people = 10000) %>% select(-node.age.grp)
)%>%  column_to_rownames(var = "node.age.grp") %>%
  kbl() %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1, "All, urban" = 3, "Home, urban" = 3,  "School, urban" = 3, "Work, urban" = 3, "Nonhome, urban" = 3))

# One-year proportion of non-isolated nodes

## rural
cbind(
## layer name
layer = 
    c("all", "home", "school", "work", "non-home"),
## rural
  rural=
c(
prop_non_isolate(frp_crude_vec = frp_length_all_r$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_h_r$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_s_r$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_w_r$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_nh_r$crude$step_365)
),

## urban
urban=
c(
prop_non_isolate(frp_crude_vec = frp_length_all_u$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_h_u$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_s_u$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_w_u$crude$step_365),
prop_non_isolate(frp_crude_vec = frp_length_nh_u$crude$step_365)
)

) %>%   kbl() %>% 
  kable_classic()



# Summary statistics table
form_stats_2_networks <- 
form_stats(tar_stats, summary_stats)

diss_stats_2_networks <- 
summary_stats$dissolution %>%
  mutate(know_contact_duration = format(know_contact_duration, scientific = T, digits = 2)) %>%
  pivot_wider(names_from = study_site, values_from = know_contact_duration) %>% data.frame() %>% mutate(contact_location = as.character(contact_location))

form_stats_2_networks%>% column_to_rownames(var = "contact_location")%>%
  kbl() %>%
  kable_classic() %>%
  add_header_above(c(" " = 1,  "Mean degree (conditioned mean degree)" = 2))


diss_stats_2_networks%>% column_to_rownames(var = "contact_location")%>%
  kbl() %>%
  kable_classic() %>%
  add_header_above(c(" " = 1,  "Duration" = 2))

# see whether frp at home is proximal to the mean deg
frp_length_h_r$crude$step_365 %>% summary # rural
frp_length_h_u$crude$step_365 %>% summary # urban
table(frp_length_h_r$crude$step_365, useNA = "always")

frp_dist <- 
frp_length_h_r$crude$step_365 %>% table %>% data.frame()


?weighted.mean
tar_stats$node_hh_assign$node_hh_assign_validation


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


