library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr); library(kableExtra)
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop =  0.1 #/1/0.1
nodes =NULL# the number of nodes with edges whose FRPs are calculated, the default setting is NULL, that FRPs for all nodes are calculated



# Load network stats to retrieve the number of node at each network
## Target stats
tar_stats <- 
readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, "_unwt_xlayer.Rds"))


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



frp_all_r <- readRDS(file.name_r[1])
frp_h_r <- readRDS(file.name_r[2])
frp_s_r <- readRDS(file.name_r[3])
frp_w_r <- readRDS(file.name_r[4])
frp_nh_r <- readRDS(file.name_r[5])
 
frp_all_u <- readRDS(file.name_u[1])
frp_h_u <- readRDS(file.name_u[2])
frp_s_u <- readRDS(file.name_u[3])
frp_w_u <- readRDS(file.name_u[4])
frp_nh_u <- readRDS(file.name_u[5])


# Process identifiers of nodal attribute in target statistics
tar_stats$attr$rural <- tar_stats$attr$rural %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))
tar_stats$attr$urban <- tar_stats$attr$urban %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))


source("./R/result_helper_functions.R")

## note - the result for the 40% pop has been erased with 2-node results
# Process FRP data
## rural
frp_length_all_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural,
    frp_length = frp_all_r$lengths,
    denom = n_r
  )


frp_length_h_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural,
    frp_length = frp_h_r$lengths,
    denom = n_r
  )

frp_length_s_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_s_r$lengths,
    denom = n_r
  )

frp_length_w_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_w_r$lengths,
    denom = n_r
  )

frp_length_nh_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_nh_r$lengths,
    denom = n_r
  )

## urban
frp_length_h_u <-
  frp_length_df_process(
    attr = tar_stats$attr$urban, 
    frp_length = frp_h_u$lengths,
    denom = n_u
  )

frp_length_s_u <-
  frp_length_df_process(
    attr = tar_stats$attr$urban, 
    frp_length = frp_s_u$lengths,
    denom = n_u
  )

frp_length_w_u <-
  frp_length_df_process(
    attr = tar_stats$attr$urban, 
    frp_length = frp_w_u$lengths,
    denom = n_u
  )

# Remove large outputs
rm(list=
     c(paste0("frp_", c("all", "h", "s", "w", "nh"), "_r"),
       paste0("frp_", c("all", "h", "s", "w", "nh"), "_u")
     )
   )

# Visualize FRP length
frp_length_plot(frp_length =frp_length_h_r, title = "Home, rural")
frp_length_plot(frp_length =frp_length_h_u, title = "Home, urban")
ggarrange(
  frp_length_plot(frp_length =frp_length_s_r, title = "School, rural"),
  frp_length_plot(frp_length =frp_length_w_r, title = "Work, rural"),
  frp_length_plot(frp_length =frp_length_nh_r, title = "Nonhome, rural"),
  frp_length_plot(frp_length =frp_length_s_u, title = "School, urban"),
  frp_length_plot(frp_length =frp_length_w_u, title = "Work, urban"),
  ncol = 3, nrow = 2,
  common.legend = TRUE, 
  legend = "bottom"
)


# FRP length per n people at different time steps
rbind(
  frp_moments_layer(frp_length_layer = frp_length_s_u, per_n_people = 10000, layer = "urban_school"),
  frp_moments_layer(frp_length_layer = frp_length_w_u, per_n_people = 10000, layer = "urban_work"),
  frp_moments_layer(frp_length_layer = frp_length_s_r, per_n_people = 10000, layer = "rural_school"),
  frp_moments_layer(frp_length_layer = frp_length_w_r, per_n_people = 10000, layer = "rural_work"),
  frp_moments_layer(frp_length_layer = frp_length_nh_r, per_n_people = 10000, layer = "rural_nonhome")
) %>% 
  kbl() %>% 
  kable_classic()


# One-year FRP by age
## rural
cbind(
frp_moments_365_layer(frp_length_layer = frp_length_s_r, per_n_people = 10000),
frp_moments_365_layer(frp_length_layer = frp_length_w_r, per_n_people = 10000) %>% select(-node.age.grp),
frp_moments_365_layer(frp_length_layer = frp_length_nh_r, per_n_people = 10000) %>% select(-node.age.grp)
)%>%  column_to_rownames(var = "node.age.grp") %>%
  kbl() %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1, "School, rural" = 3, "Work, rural" = 3, "Nonhome, rural" = 3))

## urban
cbind(
  frp_moments_365_layer(frp_length_layer = frp_length_s_u, per_n_people = 10000),
  frp_moments_365_layer(frp_length_layer = frp_length_w_u, per_n_people = 10000) %>% select(-node.age.grp)
)%>%  column_to_rownames(var = "node.age.grp") %>%
  kbl() %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1, "School, urban" = 3, "Work, urban" = 3))





# Summary statistics
## note: the reason that the target statistics on 7/30 is different from the later is because several kids are missed in the calculation of the ego mean deg that time, which was resolved later
cbind(
## formation
form_stats(tar_stats, summary_stats),
## dissolution
summary_stats$dissolution %>% 
  mutate(know_contact_duration = format(know_contact_duration, scientific = T, digits = 2)) %>% 
  pivot_wider(names_from = study_site, values_from = know_contact_duration) %>% select(-contact_location)
)%>%  column_to_rownames(var = "layer")%>%
  kbl() %>% 
  kable_classic() %>%
  add_header_above(c(" " = 1,  "Mean degree (conditioned mean degree)" = 2, "Duration" = 2))
### Interpretation - as expected, the mean degree of the layer in the rural work layer is lower than urban.
### The contact duration is longer (weaker dissolvability), which may contributes to the lower FRP.
### The mean deg at work, conditioned on school having contact, is much lower, at the rural layer - this is the primary
### factor determining the shape of the FRP length distribution


