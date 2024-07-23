library(dplyr)
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop = 0.1 #0.4/1/



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

## Rural FRP lengths
par(mfrow = c(2, 5))
matplot(t( frp_length_all_r$lengths), type = "l", 
        # ylim = c(0, max(frp_length_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "All rural layers")


matplot(t( frp_length_h_r$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural home")

matplot(t( frp_length_s_r$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural school")


matplot(t( frp_length_w_r$lengths), type = "l", 
        #ylim = c(0, max(frp_w_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural work")


matplot(t( frp_length_nh_r$lengths), type = "l",
        #ylim = c(0, max(frp_length_nh_r_365)),
        xlab = "", ylab = "FRP length", 
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



# FRP of nodes 1 to 100
file.name_r_100 <- paste0(
  "data/frp_outputs/nodes100/frp_length_",
  layers, "__",  network[1],"__", est_apch,"__", percent_target_pop, ".Rds"
)

file.name_u_100 <- paste0(
  "data/frp_outputs/nodes100/frp_length_",
  layers, "__",  network[2],"__", est_apch,"__", percent_target_pop, ".Rds"
)

frp_length_all_r_100 <- readRDS(file.name_r_100[1])
frp_length_h_r_100 <- readRDS(file.name_r_100[2])
frp_length_s_r_100 <- readRDS(file.name_r_100[3])
frp_length_w_r_100 <- readRDS(file.name_r_100[4])
frp_length_nh_r_100 <- readRDS(file.name_r_100[5])

frp_length_all_u_100 <- readRDS(file.name_u_100[1])
frp_length_h_u_100 <- readRDS(file.name_u_100[2])
frp_length_s_u_100 <- readRDS(file.name_u_100[3])
frp_length_w_u_100 <- readRDS(file.name_u_100[4])
frp_length_nh_u_100 <- readRDS(file.name_u_100[5])


# FRP diagnosis
## Home layer (those changes of FRP length at time step >1 are problematic)
### Number of nodes with different FRP lengths at specific time points
t <- seq(from =1, to =300, by=50)
t_name <- paste0("step_", t)

cbind(t_name,
rbind(
table(frp_length_h_r$lengths[,t_name[1]]),
table(frp_length_h_r$lengths[,t_name[2]]),
table(frp_length_h_r$lengths[,t_name[3]]),
table(frp_length_h_r$lengths[,t_name[4]]),
table(frp_length_h_r$lengths[,t_name[5]]),
table(frp_length_h_r$lengths[,t_name[6]])
))

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

##### FRP length at t=365
frp_length_s_r_100$reached$node_92 %>% length()

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




# netdx
## file names of the outputs
file.name_dx_r <- 
  paste0("data/netdx_outputs/dx_", layers[-1], "__", network[1],"__", est_apch,"__", percent_target_pop, ".Rds")
file.name_dx_u <- 
  paste0("data/netdx_outputs/dx_", layers[-1], "__", network[2],"__", est_apch,"__", percent_target_pop, ".Rds")


## Rural
dx_h_r <- 
  readRDS(file.name_dx_r[1])

dx_s_r <-
  readRDS(file.name_dx_r[2])

dx_w_r <- 
  readRDS(file.name_dx_r[3])

dx_nh_r <- 
  readRDS(file.name_dx_r[4])

## Urban
dx_h_u <- 
  readRDS(file.name_dx_u[1])

dx_s_u <-
  readRDS(file.name_dx_u[2])

dx_w_u <- 
  readRDS(file.name_dx_u[3])

dx_nh_u <- 
  readRDS(file.name_dx_u[4])

# Plot netdx outputs
library(EpiModel)
## Rural
plot(dx_h_r)
plot(dx_s_r)
plot(dx_w_r)
plot(dx_nh_r)

## Urban
plot(dx_h_u)
plot(dx_s_u)
plot(dx_w_u)
plot(dx_nh_u)



source("R/reachable.R")
