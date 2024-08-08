library(dplyr); library(tidyr); library(tibble)
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

# Defining functions
frp_length_df_process <- 
  function(attr, frp_length, denom){
    attr %>% select(node_id, node.age.grp) %>% 
      left_join(., 
                frp_length %>% data.frame()%>% rownames_to_column(var = "node_id"),
                by = "node_id"
    ) %>% # the NA's in the data frame till here are of node doesn't have any edges, we recode those NA's to 1
      mutate(across(everything(), ~ replace_na(., 1))) %>% 
      mutate(across(where(is.numeric), ~ . / denom))
  }



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





# mat plot
## Color options for plot
library(viridis)
library(wesanderson)
#palv6 <- grDevices::gray.colors(5)
palv6 <- c("red", "blue", "green", "purple", "orange", "cyan")
category_colors <- palv6[as.numeric(frp_length_s_r$node.age.grp)]


## Rural FRP lengths

# par(mfrow = c(2, 5))
par(mfrow = c(1, 3))


### school
matplot(t( frp_length_s_r %>% select(-c(1,2))), type = "l", 
        xlab = "", ylab = "FRP length/N",  #ylim = c(0, 1),
        lty = 1, col = category_colors, lwd = 0.5, main = "Rural school")
legend("topright", legend = levels(frp_length_s_r$node.age.grp), col = palv6, lty = 1, cex = 0.8)

### work
matplot(t( frp_length_w_r %>% select(-c(1,2))), type = "l", 
        xlab = "", ylab = "FRP length/N",  #ylim = c(0, 1),
        lty = 1, col = category_colors, lwd = 0.5, main = "Rural work")
legend("topright", legend = levels(frp_length_w_r$node.age.grp), col = palv6, lty = 1, cex = 0.8)

### nonhome
matplot(t( frp_length_nh_r %>% select(-c(1,2))), type = "l", 
        xlab = "", ylab = "FRP length/N", # ylim = c(0, 1),
        lty = 1, col = category_colors, lwd = 0.5, main = "Rural nonhome")
legend("topright", legend = levels(frp_length_nh_r$node.age.grp), col = palv6, lty = 1, cex = 0.8)


# see whether ggplot can do the same thing
# Reshape the data from wide to long format
frp_length_s_r_long <- frp_length_s_r %>% 
  pivot_longer(cols = starts_with("step_"), names_to = "time", values_to = "frp_length")%>%
  mutate(time = as.numeric(gsub("step_", "", time)))

# Define the color palette
palv6 <- c("red", "blue", "green", "purple", "orange", "cyan")

# Map the categories to the corresponding colors
color_mapping <- setNames(palv6, levels(frp_length_s_r_long$node.age.grp)
                          )

# Plot using ggplot2
library(ggplot2)
ggplot(frp_length_s_r_long, aes(x = time, y = frp_length, group = node_id, color = node.age.grp)) +
  geom_line(size = 0.2, alpha = 0.6) +
  scale_color_manual(values = color_mapping) +
  labs(title = "Rural school", x = "", y = "FRP length/N") +
  theme_classic() +
  theme(legend.position = "right")




## Urban FRP lengths
matplot(t( frp_all_u$lengths)/denom_u, type = "l", 
        # ylim = c(0, max(frp_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "All urban layers")

matplot(t( frp_h_u$lengths)/denom_u, type = "l", 
        #ylim = c(0, max(frp_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban home")

matplot(t( frp_s_u$lengths)/denom_u, type = "l", 
        #ylim = c(0, max(frp_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban school")

matplot(t( frp_w_u$lengths)/denom_u, type = "l", 
        #ylim = c(0, max(frp_w_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban work")

matplot(t( frp_nh_u$lengths)/denom_u, type = "l",
        #ylim = c(0, max(frp_nh_u_365)),
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban nonhome")



# FRP interpretation
frp_moments_layer <- function(frp_length_layer, layer){
  
# Define the function to calculate mode
  calculate_mode <- function(numbers) {
    freq_table <- table(numbers)
    mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
    mode_frequency <- as.numeric(max(freq_table))
    return(c(value = mode_value, frequency = mode_frequency))
  }
  
frp_moments <- function(numbers, scenario) {
  # Calculate summary statistics
  mean_value <- mean(numbers) %>% round(., 2)
  median_value <- median(numbers) %>% round(., 2)
  mode_result <- calculate_mode(as.numeric(numbers))%>% round(., 2)
  mode_value <- mode_result["value"]%>% round(., 2)
  mode_frequency <- mode_result["frequency"]%>% round(., 2)
  iqr_value <- IQR(numbers)%>% round(., 2)
  min_value <- min(numbers)%>% round(., 2)
  max_value <- max(numbers)%>% round(., 2)
  q1_value <- quantile(numbers, 0.25)%>% round(., 2)
  q3_value <- quantile(numbers, 0.75)%>% round(., 2)
  min_frequency <- sum(numbers == min_value)%>% round(., 2)
  
  # Create a dataframe with the results
  result <- data.frame(
    `Minimum (n)` = paste0(min_value, " (", min_frequency, ")"), 
    Mean = mean_value, 
    Median = median_value, 
    `Mode (n)` = paste0(mode_value, " (", mode_frequency, ")"),
    Maximum = max_value, 
    Q1 = q1_value, 
    Q3 = q3_value,
    IQR = iqr_value,
    check.names = FALSE
  )
  rownames(result) <- scenario

  return(result)
}

### Number of nodes with different FRP lengths at specific time points
t <- c(0, seq(from =1, to =365, length.out=3))
t_name <- paste0( "step_", t)
scenario <- paste0(layer,"_", "step_", t)

rbind(
  frp_moments(numbers = frp_length_layer[,t_name[1]]/denom,
              scenario = scenario[1]),#t=0
  frp_moments(numbers = frp_length_layer[,t_name[2]]/denom,
              scenario = scenario[2]),#t=1
  frp_moments(numbers = frp_length_layer[,t_name[3]]/denom,
              scenario = scenario[3]),#t=183
  frp_moments(numbers = frp_length_layer[,t_name[4]]/denom,
              scenario = scenario[4])#t=365
)

}

denom=1
rbind(
  #frp_moments_layer(frp_length_layer = frp_length_all_r, layer = layers[1]),
  frp_moments_layer(frp_length_layer = frp_length_h_r, layer = "rural_home"),
  frp_moments_layer(frp_length_layer = frp_length_s_r, layer = "rural_school"),
  frp_moments_layer(frp_length_layer = frp_length_w_r, layer = "rural_work"),
  frp_moments_layer(frp_length_layer = frp_length_nh_r, layer = "rural_nonhome"),
  frp_moments_layer(frp_length_layer = frp_length_s_u, layer = "urban_school"),
  frp_moments_layer(frp_length_layer = frp_length_w_u, layer = "urban_work")
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


