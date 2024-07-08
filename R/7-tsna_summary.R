library(dplyr)
# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = c("Rural", "Urban")
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop = 0.4 #0.1/1/

# Define functions
## Function to calculate the length of FRP at each time point for each node
frp_length <- function(frp_data){
length_unlist <- function(x){
  length(unlist(x))
}

frp_length_single_time <- apply(frp_data, c(1,2), length_unlist) # each cell below counts the number of nodes (by their identifiers) in the list directly outputted from your script. 

## Calculate FRP as the cumulative sum of the number of nodes that an initial node has contact with at each time step
frp_length <- t(apply(frp_length_single_time, 1, cumsum)
)
}


# Load target stats to retrieve the number of node at each network
netstats <- 
readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))

n_r <- nrow(netstats$attr$rural)
n_u <- nrow(netstats$attr$urban)


# # Loading raw FRP result
file.name_r <- paste0(
  "data/frp_outputs/frp_",
  layers, "__",  network[1],"__", est_apch,"__", percent_target_pop, ".Rds"
)

file.name_u <- paste0(
  "data/frp_outputs/frp_",
  layers, "__",  network[2],"__", est_apch,"__", percent_target_pop, ".Rds"
)

frp_all_r <- readRDS(file.name_r[1])
frp_s_r <- readRDS(file.name_r[3])
frp_w_r <- readRDS(file.name_r[4])
frp_nh_r <- readRDS(file.name_r[5])
# 
# frp_all_u_0.1 <- readRDS("data/frp_outputs/frp_All__Urban__mcmle__0.1.Rds")
# frp_s_u <- readRDS(file.name_u[3])
# frp_w_u <- readRDS(file.name_u[4])


# Calculate cumulative FRP length
# ## Rural
# frp_all_r_length <- frp_length(frp_data = frp_all_r)
# frp_s_r_length <- frp_length(frp_data = frp_s_r)
# frp_w_r_length <- frp_length(frp_data = frp_w_r)
# frp_nh_r_length <- frp_length(frp_data = frp_nh_r)
# 
# ## Urban
# frp_s_u_length <- frp_length(frp_data = frp_s_u)
# frp_w_u_length <- frp_length(frp_data = frp_w_u)
# 
# saveRDS(
# list(
#   frp_all_r_length = frp_all_r_length,
#   frp_s_r_length = frp_s_r_length,
#   frp_w_r_length = frp_w_r_length,
#   frp_nh_r_length = frp_nh_r_length,
#   frp_s_u_length = frp_s_u_length,
#   frp_w_u_length =frp_w_u_length
# ), "./data/frp_summaries/frp_length_0.4.Rds"
# )

# frp_all_u_0.1_length <- frp_length(frp_data = frp_all_u_0.1)

# Load FRP length calculated from the above
frp_length <- readRDS("./data/frp_summaries/frp_length_0.4.Rds")
frp_all_r_length<- frp_length$frp_all_r_length
frp_s_r_length<- frp_length$frp_s_r_length
frp_w_r_length<- frp_length$frp_w_r_length
frp_nh_r_length<- frp_length$frp_nh_r_length

frp_s_u_length<- frp_length$frp_s_u_length
frp_w_u_length<- frp_length$frp_w_u_length

# FRP on day 365
all_rural= frp_all_r_length[, 366]; school_rural=frp_s_r_length[, 366]; work_rural=frp_w_r_length[, 366]
nonhome_rural=frp_nh_r_length[, 366]; school_urban=frp_s_u_length[, 366]; work_urban=frp_w_u_length[, 366]

## Table for FRP on day 365
frp_365 <- 
list(
all_rural = all_rural,
school_rural = school_rural,
work_rural = work_rural,
nonhome_rural = nonhome_rural,

school_urban = school_urban,
work_urban = work_urban
) %>% 
  lapply(., summary)%>% 
  do.call(rbind, .) %>% 
  round() %>% 
  data.frame() %>% 
  tibble::rownames_to_column(var="scenario")

frp_365

## Histogram
par(mfrow = c(3, 2))
hist(all_rural); hist(nonhome_rural);
hist(school_rural); hist(work_rural);
hist(school_urban); hist(work_urban)


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
par(mfrow = c(3, 2))
matplot(t( frp_all_r_length), type = "l", 
        # ylim = c(0, max(frp_all_r_365)
        #          ), 
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "All rural layers, India")

abline(h = n_r, col = "blue")

matplot(t( frp_nh_r_length), type = "l",
        #ylim = c(0, max(frp_nh_r_365)),
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural nonhome, India")
abline(h = n_r, col = "blue")

matplot(t( frp_s_r_length), type = "l", 
        #ylim = c(0, max(frp_s_r_365)), 
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural school, India")
abline(h = n_r, col = "blue")
matplot(t( frp_w_r_length), type = "l", 
        #ylim = c(0, max(frp_w_r_365)), 
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Rural work, India")
abline(h = n_r, col = "blue")
matplot(t( frp_s_u_length), type = "l", 
        ylim = c(0, 20000), 
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban school, India")
abline(h = n_u, col = "blue")
matplot(t( frp_w_u_length), type = "l", 
        ylim = c(0, 20000), 
        xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Urban work, India")
abline(h = n_u, col = "blue")


# Diagnose netsim objects
## Note: the following script is being run at HPC

netsim_r <- readRDS("data/netsim_outputs/sim_Rural__mcmle__0.4.Rds")
netsim_u <- readRDS("data/netsim_outputs/sim_Urban__mcmle__0.4.Rds")



library(sna); library(EpiModel)

# Get the degree of each node
node_degrees_layers_Rural <- 
  data.frame(
    Home_Rural=  degree(netsim_r$Home),
    School_Rural=  degree(netsim_r$School),
    Work_Rural=  degree(netsim_r$Work),
    Nonhome_Rural=  degree(netsim_r$Nonhome)
  )

node_degrees_layers_Urban <- 
data.frame(
Home_Urban=  degree(netsim_u$Home),
School_Urban=  degree(netsim_u$School),
Work_Urban=  degree(netsim_u$Work),
Nonhome_Urban=  degree(netsim_u$Nonhome)
)

apply(node_degrees_layers_Rural, 2, summary)
apply(node_degrees_layers_Urban, 2, summary)




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
?plot.netdx
EpiModel::plot
plot(dx_h_r, stats = "edges")
plot(dx_s_r)
plot(dx_w_r)
plot(dx_nh_r)

## Urban
plot(dx_h_u)
plot(dx_s_u)
plot(dx_w_u)
plot(dx_nh_u)

