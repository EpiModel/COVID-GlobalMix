# local setting
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# layer = "Home"/"School"/"Work"/"Nonhome"
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = 0.1/0.4/1
layer = "Home"
network = "Rural"
est_apch = "mcmle"
percent_target_pop = 0.1


dx_u <-
  netdx(est,
        nsims =  30,
        ncores = 10,
        nsteps = 1000,
        nwstats.formula = ~edges + nodemix("age.grp", levels2 = -1)+ degree(d= 1:15)+ esp(1:6),
        set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 200000,
                                                         MCMC.interval = 25000),
        set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000),
        dynamic = TRUE,
        skip.dissolution = FALSE
  )

plot(dx_u)

# r
degree_dist <- 
   dx$stats.table.formation[c(22:41),]

# u
degree_dist_u <- 
  dx_u$stats.table.formation[c(22:41),]

par(mfrow = c(2,1))
plot(x=1:20, y = degree_dist$`Sim Mean`, type = "b", xlab= "Degree", ylab = "Simulated target statistics, Rural")
plot(x=1:20, y = degree_dist_u$`Sim Mean`, type = "b", xlab= "Degree", ylab = "Simulated target statistics, Urban")
