library(dplyr)
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


# # Loading raw FRP result
file.name_r <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",  network[1],"__", est_apch,"__", percent_target_pop, ".Rds"
)

file.name_u <- paste0(
  "data/frp_outputs/frp_length_",
  layers, "__",  network[2],"__", est_apch,"__", percent_target_pop, ".Rds"
)

frp_length_all_r <- readRDS(file.name_r[1])
frp_length_h_r <- readRDS(file.name_r[2])
frp_length_s_r <- readRDS(file.name_r[3])
frp_length_w_r <- readRDS(file.name_r[4])
frp_length_nh_r <- readRDS(file.name_r[5])
# 
frp_length_all_u <- readRDS(file.name_u[1])
frp_length_h_u <- readRDS(file.name_u[2])
frp_length_s_u <- readRDS(file.name_u[3])
frp_length_w_u <- readRDS(file.name_u[4])
frp_length_nh_u <- readRDS(file.name_u[5])



# mat plot
## Color options for plot
library(viridis)
library(wesanderson)
palv6 <- grDevices::gray.colors(5)

denom <- 1#n_r
## Rural FRP lengths
# par(mfrow = c(2, 5))
par(mfrow = c(1, 5))
matplot(t( frp_length_all_r$lengths)/denom, type = "l", 
        # ylim = c(0, max(frp_length_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length/N", 
        lty = 1, col = palv6, lwd = 0.5, main = "All rural layers")


matplot(t( frp_length_h_r$lengths)/denom, type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length/N", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural home")

matplot(t( frp_length_s_r$lengths)/denom, type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length/N", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural school")


matplot(t( frp_length_w_r$lengths)/denom, type = "l", 
        #ylim = c(0, max(frp_w_r_365)), 
        xlab = "", ylab = "FRP length/N", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural work")


matplot(t( frp_length_nh_r$lengths)/denom, type = "l",
        #ylim = c(0, max(frp_length_nh_r_365)),
        xlab = "", ylab = "FRP length/N", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural nonhome")


## Urban FRP lengths
matplot(t( frp_length_all_u$lengths), type = "l", 
        # ylim = c(0, max(frp_length_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "All urban layers")

matplot(t( frp_length_h_u$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban home")

matplot(t( frp_length_s_u$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban school")

matplot(t( frp_length_w_u$lengths), type = "l", 
        #ylim = c(0, max(frp_w_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban work")

matplot(t( frp_length_nh_u$lengths), type = "l",
        #ylim = c(0, max(frp_length_nh_u_365)),
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
  frp_moments(numbers = frp_length_layer$lengths[,t_name[1]]/denom,
              scenario = scenario[1]),#t=0
  frp_moments(numbers = frp_length_layer$lengths[,t_name[2]]/denom,
              scenario = scenario[2]),#t=1
  frp_moments(numbers = frp_length_layer$lengths[,t_name[3]]/denom,
              scenario = scenario[3]),#t=183
  frp_moments(numbers = frp_length_layer$lengths[,t_name[4]]/denom,
              scenario = scenario[4])#t=365
)

}


rbind(
  frp_moments_layer(frp_length_layer = frp_length_all_r, layer = layers[1]),
  frp_moments_layer(frp_length_layer = frp_length_h_r, layer = layers[2]),
  frp_moments_layer(frp_length_layer = frp_length_s_r, layer = layers[3]),
  frp_moments_layer(frp_length_layer = frp_length_w_r, layer = layers[4]),
  frp_moments_layer(frp_length_layer = frp_length_nh_r, layer = layers[5])
)



## FRP shape of individuals at rural school
frp_length_s_r_100$lengths %>% View()
#### Evaluate FRP of node 92
##### FRP length 
frp_length_node92 <- 
frp_length_s_r_100$lengths %>% data.frame() %>% 
  tibble::rownames_to_column(var="node_name") %>% filter(node_name == "node_92") %>% select(-node_name)

par(mfrow = c(1, 1))
matplot(t( frp_length_node92), type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Node 92 at rural school, India")


# FRP at rural and urban work layers
## Egocentric mean degree
summary_stats$formation$formation_stats_rural$edge_node_factor_match_rural$edge %>% filter(contact_location == "Work")
summary_stats$formation$formation_stats_urban$edge_node_factor_match_urban$edge %>% filter(contact_location == "Work")

## mean degree, converted from "edge" target statistics
tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum()/n_r
tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work %>% sum()/n_u

## x-layer effect
summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day %>% filter(association == "w_by_s")
summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day %>% filter(association == "w_by_s")
## duration
summary_stats$dissolution %>% filter(contact_location == "Work")
### Interpretation - as expected, the mean degree of the layer in the rural work layer is lower than urban.
### The contact duration is longer (weaker dissolvability), which may contributes to the lower FRP.
### The mean deg at work, conditioned on school having contact, is much lower, at the rural layer - this is the primary
### factor determining the shape of the FRP length distribution


