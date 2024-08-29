
# explore netsim objects for FRP paper

library("EpiModel")
library("sna")

nss <- function(net, layer, at) {
  
  if (net == "r") {
    r <- readRDS("data/netsim_outputs/networkdynamic__Rural__mcmle__0.1.Rds")
  } else {
    r <- readRDS("data/netsim_outputs/networkdynamic__Urban__mcmle__0.1.Rds")
  }

  rh <- r[[layer]]
  rh
  
  rh1 <- network.collapse(rh, at = at)
  rh1
  
  rh1.dd <- get_degree(rh1)
  barplot(table(rh1.dd), main = "degree distribution")
  t1 <- table(rh1.dd)
  
  rh1.cs <- component.dist(rh1, connected = "weak")
  t2 <- table(rh1.cs$csize)
  # table(rh1.cs$membership)
  list(dd = t1, cs = t2)
  
}

nss("r", "Home", at = 1)
nss("r", "School", at = 1)
nss("r", "Work", at = 1)
nss("r", "Nonhome", at = 1)

nss("u", "Home", at = 1)
nss("u", "School", at = 1)
nss("u", "Work", at = 1)
nss("u", "Nonhome", at = 1)

r <- readRDS("data/netsim_outputs/networkdynamic__Rural__mcmle__0.1.Rds")
rh1 <- network.collapse(r$Home, at = 1)
summary(rh1 ~ meandeg)

rw1 <- network.collapse(r$Work, at = 1)
summary(rw1 ~ meandeg)

nw <- network_initialize(1000)
g <- ergm(nw ~ edges, target.stats = 1395) #number of edges
g <- simulate(g)
plot(g)
mean(get_degree(g))
table(get_degree(g))
table(component.dist(g, connected = "weak")$csize)
components(g, connected = "weak")

table(component.dist(g, connected = "weak")$membership)

# simulate  household id of 1 to 100, and assign each node an houshold id
nb <- sample(1:100, 1000, TRUE)
nw <- set_vertex_attribute(nw, "nb", nb)

g <- ergm(nw ~ edges + nodematch("nb"), target.stats = c(1395, 1395))
g <- simulate(g)
summary(g ~ edges + nodematch("nb"))
mean(get_degree(g))
table(get_degree(g))
table(component.dist(g, connected = "weak")$csize)
components(g, connected = "weak")

nw <- network.initialize(1000, directed = FALSE)
g <- ergm(nw ~ edges, target.stats = 1250)
g <- simulate(g, nsim = 1000, monitor = nw ~ degcor, output = "stats")
colMeans(g)

g2 <- ergm(nw ~ edges + degcor, target.stats = c(1250, -0.078))

