# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = "Rural"# /"Rural"/
est_apch = "mcmle"#/"sto_apoxy"/
layers = c("All","Home","School","Work","Nonhome")
percent_target_pop = 0.4 #0.1/1/

# Define functions
cum_frp <- function(frp_data){
length_unlist <- function(x){
  length(unlist(x))
}

frp_data_count <- apply(frp_data, c(1,2), length_unlist) # each cell below counts the number of nodes (by their identifiers) in the list directly outputted from your script. 

## Calculate FRP as the cumulative sum of the number of nodes that an initial node has contact with at each time step
frp_data_count_cum <- t(apply(frp_data_count, 1, cumsum)
)
}

# Loading FRP result
file.name <- paste0(
  "data/frp_outputs/frp_",
  layers, "__",  network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

frp_all_r <- readRDS(file.name[1])
frp_s_r <- readRDS(file.name[3])
frp_w_r <- readRDS(file.name[4])
frp_nh_r <- readRDS(file.name[5])


# artnet_frp <- readRDS("data/frp_outputs/artnet.TsnaData.rda")
# 
# sf.all.frp <- 
# as.data.frame(t(artnet_frp$frp$sfa.frp))




# FRP in day 365
## Interpretation: all node had contact with with the others at day 365
n_pop <- nrow(frp_w_r_cum)
frp_w_r_365 <- frp_w_r_cum[, 366]
frp_s_r_365 <- frp_s_r_cum[, 366]


# Calculate cumulative FRP length
frp_all_r_cum <- cum_frp(frp_data = frp_all_r)
frp_s_r_cum <- cum_frp(frp_data = frp_s_r)
frp_w_r_cum <- cum_frp(frp_data = frp_w_r)
frp_nh_r_cum <- cum_frp(frp_data = frp_nh_r)

# mat plot
## Color options for plot
library(viridis)
library(wesanderson)
palv <- rainbow(8)
palv1 <- viridis(n = 5, alpha = 0.25, option = "viridis")
palv2 <- adjustcolor(wes_palette(4, name = "Royal2"), alpha.f = 1)
palv3 <- adjustcolor(col = RColorBrewer::brewer.pal(5, "Set1"), alpha.f = 0.8)
palv4 <- inferno(n = 5)
palv5 <- RColorBrewer::brewer.pal(8, "Set2")
palv6 <- grDevices::gray.colors(5)
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), xpd = NA, mgp = c(2,1,0),
    mar = c(3,3,2,1))
matplot(t( frp_s_r_cum), type = "l", ylim = c(0, max(frp_s_r_365)), xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "School, India")
matplot(t( frp_w_r_cum), type = "l", ylim = c(0, max(frp_w_r_365)), xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "Work, India")

