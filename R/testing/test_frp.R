## 1. make a network dyn object ------------------------------------------------
library(EpiModelHIV)
library(dplyr)
library(ggplot2)
theme_set(theme_light())

source("R/shared_variables.R", local = TRUE)
source("R/B-netsim_explore/z-context.R")
source("./R/frp_cuml.R")

x <- readRDS("fit_main.rds")
nw <- simulate(
  x$fit,
  basis = x$fit$newnetwork,
  control = control.simulate.ergm(MCMC.burnin = 2e5)
)

at <- 1
for (at in 1:260) {
  nw <- simulate(
    nw,
    formation = x$formation,
    dissolution = x$coef.diss$dissolution,
    coef.form = x$coef.form,
    coef.diss = x$coef.diss$coef.crude,
    time.start = at,
    time.slices = 1,
    time.offset = 0,
    monitor = "all",
    output = "networkDynamic"
  )
}

## 2. convert to an el_cuml ----------------------------------------------------
library(EpiModelHIV)
el_cuml <- netdyn2el_cuml(nw)
# check the output
head(el_cuml)
head(as.data.frame(nw))

## 3. run the FRPs -------------------------------------------------------------
# with el_cuml
start <- Sys.time()
progressr::with_progress(
  frp_parts <- get_all_frp(el_cuml, 1, 260)
)
print(Sys.time() - start)

# with net dyn object
start <- Sys.time()
progressr::with_progress(
  frp_parts_old <- get_all_frp_old(nw, 1, 260)
)
print(Sys.time() - start)

# check the result of the first 1000 nodes
node <- 1
for (node in (1:1000)) {
  a <- unlist(frp_parts[node, seq_len(260 + 1)]) |> length()
  # b <- unlist(frp_parts_op[node, seq_len(260 + 1)]) |> length()
  b <- unlist(frp_parts_old[node, seq_len(260 + 1)]) |> length()
  if (a != b) {
    print(node)
    break
  }
}
