library(dplyr); library(tidyr); library(tibble); library(ggplot2); library(ggpubr);
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop =  0.4 #/1/0.1



# Load network stats to retrieve the number of node at each network
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
  layers, "__",  network[1],"__", est_apch,"__", percent_target_pop, "__", ".Rds"
)

file.name_u <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",  network[2],"__", est_apch,"__", percent_target_pop, "__", ".Rds"
)


frp_all_r <- readRDS(file.name_r[1])
frp_h_r <- readRDS(file.name_r[2])
frp_s_r <- readRDS(file.name_r[3])
frp_w_r <- readRDS(file.name_r[4])
frp_nh_r <- readRDS(file.name_r[5])
 
# frp_all_u <- readRDS(file.name_u[1])
# frp_h_u <- readRDS(file.name_u[2])
# frp_s_u <- readRDS(file.name_u[3])
# frp_w_u <- readRDS(file.name_u[4])
# frp_nh_u <- readRDS(file.name_u[5])


# Process nodal attribute
tar_stats$attr$rural <- tar_stats$attr$rural %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))
tar_stats$attr$urban <- tar_stats$attr$urban %>% rownames_to_column(var = "node_id") %>% mutate(node_id = paste0("node_", node_id))


source("./R/result_helper_functions.R")


# Process FRP data
denom_r <-   n_r
denom_u <- n_u

frp_length_h_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_h_r$lengths
  )

frp_length_s_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_s_r$lengths,
    denom = denom_r
  )

frp_length_w_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_w_r$lengths,
    denom = denom_r
  )

frp_length_nh_r <-
  frp_length_df_process(
    attr = tar_stats$attr$rural, 
    frp_length = frp_nh_r$lengths,
    denom = denom_r
  )






# Visualize FRP length
ggarrange(
  frp_length_plot(frp_length =frp_length_s_r, title = "School, rural"),
  frp_length_plot(frp_length =frp_length_w_r, title = "Work, rural"),
  frp_length_plot(frp_length =frp_length_nh_r, title = "Nonhome, rural"),
  ncol = 2, nrow = 2,
  common.legend = TRUE, 
  legend = "bottom"
)




# FRP interpretation
rbind(
  #frp_moments_layer(frp_length_layer = frp_length_all_r, layer = layers[1]),
  frp_moments_layer(frp_length_layer = frp_length_s_r, layer = "rural_school"),
  frp_moments_layer(frp_length_layer = frp_length_w_r, layer = "rural_work"),
  frp_moments_layer(frp_length_layer = frp_length_nh_r, layer = "rural_nonhome")

)





# Define a function to round character values within a data frame
form_stats <- function(tar_stats, summary_stats){
  
round_df <- function(df) {
  # Define a helper function to round character values
  round_char_values <- function(value) {
    if (grepl("\\(", value)) {
      # Extract numbers from the value
      numbers <- unlist(regmatches(value, gregexpr("[0-9.]+", value)))
      # Round each number to 3 digits
      rounded_numbers <- sapply(numbers, function(x) sprintf("%.3f", as.numeric(x)))
      # Reconstruct the value with rounded numbers
      rounded_value <- paste(rounded_numbers[1], "(", rounded_numbers[2], ")", sep = "")
      return(rounded_value)
    } else if (grepl("^[0-9.]+$", value)) {
      return(sprintf("%.3f", as.numeric(value)))
    } else {
      return(value)
    }
  }
  
  # Apply the helper function to character columns in the data frame
  df %>% mutate(across(where(is.character), ~sapply(.x, round_char_values)))
}


## mean degree, converted from "edge" target statistics

summary_stat_tb <- 
c(
tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home %>% sum()/n_r,
paste0(tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% sum()/n_r, " (",
       summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day %>% filter(association == "s_by_w")%>% pull(other_layer.1), ")"
       ),
paste0(
tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum()/n_r, " (",
summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day %>% filter(association == "w_by_s") %>% pull(other_layer.1), ")"
),
tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome %>% sum()/n_r
) %>% cbind(., 
c(
  tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Home %>% sum()/n_r,
  paste0(tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School %>% sum()/n_r, " (",
         summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day %>% filter(association == "s_by_w")%>% pull(other_layer.1), ")"
  ),
  paste0(
    tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work %>% sum()/n_r, " (",
    summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day %>% filter(association == "w_by_s") %>% pull(other_layer.1), ")"
  ),
  tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Nonhome %>% sum()/n_r
),
layers[-1]
) %>% data.frame() %>% round_df(df=.) %>% select(3,1,2) %>% rename(layer=1, `rural`=2, `urban`=3)

summary_stat_tb
}

form_stats(tar_stats, summary_stats)

## duration
summary_stats$dissolution %>% pivot_wider(names_from = study_site, values_from = know_contact_duration) 
### Interpretation - as expected, the mean degree of the layer in the rural work layer is lower than urban.
### The contact duration is longer (weaker dissolvability), which may contributes to the lower FRP.
### The mean deg at work, conditioned on school having contact, is much lower, at the rural layer - this is the primary
### factor determining the shape of the FRP length distribution


