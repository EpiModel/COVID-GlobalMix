# Note: the purpose of this script is to summarize result the FRP calculation. 
# The following are the analytical scenarios
network = "Rural"# /"Rural"/
est_apch = "mcmle"#/"sto_apoxy"/
layer = "All"#/"Home"/"School"/"Work"/"Nonhome"/, where "ALL" means all 4 layers
percent_target_pop = 0.4 #0.1/1/

# Define functions
length_unlist <- function(x){
  length(unlist(x))
}

# Loading FRP result
file.name <- paste0(
  "data/frp_outputs/frp_",
  network,"__", est_apch,"__", percent_target_pop, ".Rds"
)

frp_data <- readRDS(file.name)

artnet_frp <- readRDS("data/frp_outputs/artnet.TsnaData.rda")

sf.all.frp <- 
as.data.frame(t(artnet_frp$frp$sfa.frp))
  
## subset a smaller item for script building
frp_data_short <- as.data.frame(frp_data)[c(1:10),]


frp_data_count <- apply(frp_data, c(1,2), length_unlist) # each cell below counts the number of nodes (by their identifiers) in the list directly outputted from your script. 

## Calculate FRP as the cumulative sum of the number of nodes that an initial node has contact with at each time step
frp_data_count_cum <- t(apply(frp_data_count, 1, cumsum)
                        )


# FRP in day 365
## Interpretation: all node had contact with with the others at day 365
n_pop <- nrow(frp_data_count_cum)
frp_365 <- frp_data_count_cum[, 366]
table(
frp_365 == n_pop
)

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
matplot(t( frp_data_count_cum[, c(1:20)]), type = "l", ylim = c(0, n_pop), xlab = "", ylab = "FRP", 
        lty = 1, col = palv6, lwd = 0.5, main = "India")

