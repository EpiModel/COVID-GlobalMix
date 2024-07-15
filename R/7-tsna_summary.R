library(dplyr)
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop = 0.1 #0.4/1/



# Load network stats to retrieve the number of node at each network
## formation stats
netstats <- 
readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))
## Dissolution stats
duration <- 
readRDS("data/network_stats_attributes/network_params.Rds")$dissolution



n_r <- nrow(netstats$attr$rural)
n_u <- nrow(netstats$attr$urban)


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
frp_length_all_u <- readRDS("data/frp_outputs/frp_length_All__Urban__mcmle__0.1.Rds")
frp_length_h_u <- readRDS(file.name_u[2])
frp_length_s_u <- readRDS(file.name_u[3])
frp_length_w_u <- readRDS(file.name_u[4])
frp_length_nh_u <- readRDS(file.name_u[5])



## Table for FRP on day 365
frp_365 <- 
list(
  All_rural = as.numeric(frp_length_all_r$lengths[, 366]),
  Home_rural = as.numeric(frp_length_h_r$lengths[, 366]),
  School_rural = as.numeric(frp_length_s_r$lengths[,366]),
  Work_rural = as.numeric(frp_length_w_r$lengths[,366]),
  Nonhome_rural = as.numeric(frp_length_nh_r$lengths[,366]),
  
All_urban = as.numeric(frp_length_all_u$lengths[, 366]),
Home_urban = as.numeric(frp_length_h_u$lengths[, 366]),
School_urban = as.numeric(frp_length_s_u$lengths[,366]),
Work_urban = as.numeric(frp_length_w_u$lengths[,366]),
Nonhome_urban = as.numeric(frp_length_nh_u$lengths[,366])
) %>% 
  lapply(., summary)%>% 
  do.call(rbind, .) %>% 
  round() %>% 
  data.frame() %>% 
  tibble::rownames_to_column(var="scenario")

frp_365

## Histogram
# par(mfrow = c(3, 2))
# hist(all_rural); hist(nonhome_rural);
# hist(school_rural); hist(work_rural);
# hist(school_urban); hist(work_urban)


# mat plot
## Color options for plot
library(viridis)
library(wesanderson)
# palv <- rainbow(8)
# palv1 <- viridis(n = 5, alpha = 0.25, option = "viridis")
# palv2 <- adjustcolor(wes_palette(4, name = "Royal2"), alpha.f = 1)
# palv3 <- adjustcolor(col = RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
# palv4 <- inferno(n = 5)
# palv5 <- RColorBrewer::brewer.pal(8, "Set2")
palv6 <- grDevices::gray.colors(5)

## Rural FRP lengths
par(mfrow = c(2, 5))
matplot(t( frp_length_all_r$lengths), type = "l", 
        # ylim = c(0, max(frp_length_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "All rural layers, India")


matplot(t( frp_length_h_r$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural home, India")

matplot(t( frp_length_s_r$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural school, India")


matplot(t( frp_length_w_r$lengths), type = "l", 
        #ylim = c(0, max(frp_w_r_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural work, India")


matplot(t( frp_length_nh_r$lengths), type = "l",
        #ylim = c(0, max(frp_length_nh_r_365)),
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural nonhome, India")


## Urban FRP lengths
matplot(t( frp_length_all_u$lengths), type = "l", 
        # ylim = c(0, max(frp_length_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "All urban layers, India")

matplot(t( frp_length_h_u$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban home, India")

matplot(t( frp_length_s_u$lengths), type = "l", 
        #ylim = c(0, max(frp_length_s_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban school, India")

matplot(t( frp_length_w_u$lengths), type = "l", 
        #ylim = c(0, max(frp_w_u_365)), 
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban work, India")

matplot(t( frp_length_nh_u$lengths), type = "l",
        #ylim = c(0, max(frp_length_nh_u_365)),
        xlab = "", ylab = "FRP length", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban nonhome, India")






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

### Evaluate duration
duration

dx_h_r$coef.diss$duration
dx_s_r$coef.diss$duration
dx_w_r$coef.diss$duration
dx_nh_r$coef.diss$duration

dx_h_u$coef.diss$duration
dx_s_u$coef.diss$duration
dx_w_u$coef.diss$duration
dx_nh_u$coef.diss$duration

source("R/reachable.R")
