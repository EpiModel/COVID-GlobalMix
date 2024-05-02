
library("dplyr")
library("EpiModel")

# reading estimated models and model's formulas
layers <- c("Home", "School", "Work", "Nonhome")
networks <- c("Rural", "Urban")
est_apch <- "sto_apoxy"

file.name <- 
  c(
    paste0("data/netest_outputs/netest_8_layers_", layers, "__", networks[1],"__", est_apch, ".Rds"),
    paste0("data/netest_outputs/netest_8_layers_", layers, "__", networks[2],"__", est_apch, ".Rds")
  )

est_h_r  <- 
  readRDS(file.name[1]) 
est_s_r  <- 
  readRDS(file.name[2]) 
est_w_r  <- 
  readRDS(file.name[3]) 
est_nh_r  <- 
  readRDS(file.name[4]) 
est_h_u  <- 
  readRDS(file.name[5]) 
est_s_u  <- 
  readRDS(file.name[6]) 
est_w_u  <- 
  readRDS(file.name[7]) 
est_nh_u  <- 
  readRDS(file.name[8]) 

est_nws <- list()
est_nws$Rural$Home <- est_h_r
est_nws$Rural$School <- est_s_r
est_nws$Rural$Work <- est_w_r
est_nws$Rural$Nonhome <- est_nh_r
est_nws$Urban$Home <- est_h_u
est_nws$Urban$School <- est_s_u
est_nws$Urban$Work <- est_w_u
est_nws$Urban$Nonhome <- est_nh_u


# Diagnosing layers 
source("R/layers_dx.R")
## Rural
dx_h_r <- 
layers_dx(est_nws = est_nws, 
          
          nw = "Rural", # can be "Rural" / "Urban"
          layer = "Home"
          )

dx_s_r <- 
  layers_dx(est_nws = est_nws, 
          
            nw = "Rural", # can be "Rural" / "Urban"
            layer = "School"
  ) # netdx for this layer failed

dx_w_r <- 
  layers_dx(est_nws = est_nws, 
           
            nw = "Rural", # can be "Rural" / "Urban"
            layer = "Work"
  )

dx_nh_r <- 
  layers_dx(est_nws = est_nws, 
            
            nw = "Rural", # can be "Rural" / "Urban"
            layer = "Nonhome"
  )

## Urban
dx_h_u <- 
  layers_dx(est_nws = est_nws, 
            
            nw = "Urban", # can be "Rural" / "Urban"
            layer = "Home"
  )

dx_s_u <- 
  layers_dx(est_nws = est_nws, 
            
            nw = "Urban", # can be "Rural" / "Urban"
            layer = "School"
  )

dx_w_u <- 
  layers_dx(est_nws = est_nws, 
            
            nw = "Urban", # can be "Rural" / "Urban"
            layer = "Work"
  )

dx_nh_u <- 
  layers_dx(est_nws = est_nws, 
            
            nw = "Urban", # can be "Rural" / "Urban"
            layer = "Nonhome"
  )


# Outputting netdx items of the 8 layers
file.name <- 
  c(
    paste0("data/netdx_outputs/dx_", layers, "__", networks[1],"__", est_apch, ".Rds"),
    paste0("data/netdx_outputs/dx_", layers, "__", networks[2],"__", est_apch, ".Rds")
  )

saveRDS(dx_h_r,  file = file.name[1])
saveRDS(dx_s_r,  file = file.name[2])
saveRDS(dx_w_r,  file = file.name[3])
saveRDS(dx_nh_r,  file = file.name[4])
saveRDS(dx_h_u,  file = file.name[5])
saveRDS(dx_s_u,  file = file.name[6])
saveRDS(dx_w_u,  file = file.name[7])
saveRDS(dx_nh_u,  file = file.name[8])

dx_layers <- 
readRDS("data/netdx_outputs/dx_layers.Rds")




