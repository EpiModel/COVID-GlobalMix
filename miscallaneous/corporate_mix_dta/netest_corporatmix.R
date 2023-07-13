
##
## COVID-19 Corporate Network Model
## Network Parameters and Model
##
## Authors: Samuel M. Jenness
## Date: February 2021
##

library("EpiModelCOVID")
library("dplyr")
library("tidyr")

# load CorpMix round 5 contact data----------------------------------------

# 'balanced' round 5 mixing matrix
cm2 <- readRDS("../corporate_mix_dta/rd5_matrix.rds") %>%
  ungroup()

# round 5 mixing matrix with proportions in each cell
cm.prop <- readRDS("../corporate_mix_dta/rd5_matrix_prop.rds") %>%
  ungroup()

# proportion within age group mixing by location
md.match <- cm2 %>%
  mutate(within_age_edges = total_degree*prop_match) %>%
  select(location,age,total_degree,within_age_edges) %>%
  group_by(location) %>%
  summarise(total_degree = sum(total_degree),
            within_age_edges = sum(within_age_edges)) %>%
  mutate(match = within_age_edges/total_degree)

# load POLYMOD data -------------------------------------------------------
library(socialmixr)
data("polymod")

# contacts outside the home > 15 mins
poly.nothome <- contact_matrix(polymod, age.limits = c(0,10,20,30,40,50,60,70,80,90,100),
                               filter = list(cnt_home=0, duration_multi=c(3,4,5)))$matrix
rowSums(poly.nothome)

# # contacts outside the home all durations
# poly.nothome.all <- contact_matrix(polymod, age.limits = c(0,10,20,30,40,50,60,70,80,90,100),
#                                filter = list(cnt_home=0))$matrix
# rowSums(poly.nothome.all)


# Set network ------------------------------------------------------------------
n <- 10000

# Initialize age - from http://wonder.cdc.gov/bridged-race-v2020.html
# From Karina's model
age.pyr <- c( 0.01167, 0.01179, 0.01209, 0.01232, 0.01260, 0.01279, 0.01271,
              0.01266, 0.01296, 0.01306, 0.01313, 0.01323, 0.01380, 0.01388,
              0.01383, 0.01367, 0.01369, 0.01355, 0.01351, 0.01392, 0.01391,
              0.01353, 0.01348, 0.01344, 0.01344, 0.01375, 0.01402, 0.01433,
              0.01462, 0.01480, 0.01470, 0.01413, 0.01367, 0.01334, 0.01330,
              0.01350, 0.01303, 0.01328, 0.01330, 0.01326, 0.01366, 0.01284,
              0.01262, 0.01256, 0.01218, 0.01269, 0.01242, 0.01279, 0.01338,
              0.01398, 0.01392, 0.01293, 0.01256, 0.01251, 0.01272, 0.01326,
              0.01324, 0.01297, 0.01274, 0.01265, 0.01250, 0.01197, 0.01164,
              0.01156, 0.01102, 0.01076, 0.01022, 0.00975, 0.00930, 0.00899,
              0.00880, 0.00851, 0.00841, 0.00857, 0.00626, 0.00609, 0.00576,
              0.00574, 0.00482, 0.00433, 0.00400, 0.00357, 0.00322, 0.00282,
              0.00259, 0.00223, 0.00193, 0.00166, 0.00144, 0.00124, 0.00107,
              0.00092, 0.00080, 0.00069, 0.00059, 0.00051, 0.00044, 0.00038,
              0.00033, 0.00028)
age <- sample(x = 0:99, size = n, prob = age.pyr, replace = TRUE)

plot(x = x, y = age.pyr)



age_noise <- runif(n)
age <- age + age_noise
hist(age)

age.breaks <- seq(0, 100, 10)
age.grp <- cut(age, age.breaks, labels = FALSE, right = FALSE)

age.grp.num <- as.numeric(table(age.grp))

# From: https://www.bls.gov/cps/cpsaat09.htm
# 158,291,000 employed persons aged 16+ in 2022
# 'Office' jobs:
# Management, business, and financial operations occupations: 29,350,000
# Computer + mathematical: 6,171,000
# Architecture + engineering: 3,464,000
# Life, physical, and social science: 1,840,000
# Community and social service: 2,945,000
# Legal: 1,861,000
# Sales + office: 30,412,000
## proportion office workers among employed:
p.office <- (29350+6171+3464+1840+2945+1861+30412)/158291

# From: https://www.bls.gov/cps/cpsaat03.htm
# Percent 20-24y employed: 66.0%
# Percent 25-54y employed: 79.9% (minimal heterogeneity within finer age groups)
# Percent 55-59y employed: 71.1%
# Percent 60-64y employed: 56.0%
# Percent 65-69y employed: 32.4%
# Percent 70-74y employed: 17.8%
# Percent 75y+ employed: 8.2%
## proportion office workers by age:
o.age <- c(0,0,(0.66+0.799)/2,0.799,0.799,(0.799+0.711)/2,
           (0.560+0.324)/2,(0.178+0.082)/2,0.082,0)*p.office

# determine if office worker by age group
ids.office <- NA
ids.age <- NA
for (i in 1:10) {
  ids.age <- NA
  vec.office <- NA
  ids.office.i <- NA
  
  ids.age <- which(age.grp == i)
  vec.office <- which(rbinom(length(ids.age), 1, o.age[i]) == 1)
  ids.office.i <- ids.age[vec.office]
  ifelse(i==1,
         ids.office <- ids.office.i,
         ids.office <- c(ids.office,ids.office.i))
}

non.office <- rep(NA, n)
all.ids <- 1:n
non.office <- ifelse(all.ids %in% ids.office,0,1)

n.other <- sum(non.office)
n.work <- n-n.other

table(age.grp,non.office)

# Initialize the network
nw <- network_initialize(n)
nw <- set_vertex_attribute(nw, "age", age)
nw <- set_vertex_attribute(nw, "age.grp", age.grp)
nw <- set_vertex_attribute(nw, "non.office", non.office)


## Within office network -------------------------------------------------------

# Mean degree from corp mix data
md.oo <- md %>%
  filter(location == "Work") %>%
  select(md) %>%
  as.numeric()

edges.oo <- md.oo * n.work/2

# Number of edges per age group for nodefactor()
# note this only includes age groups 20-60+
md.oo.ag <- cm2 %>%
  filter(location == "Work") %>%
  select(total_degree) %>%
  pull(total_degree) %>%
  as.numeric()
# adding 0 md for other age groups (not in CorpMix data)
md.oo.ag <- c(0,0,md.oo.ag,0,0,0)
age.grp.work <- c(0,0,as.numeric(table(age.grp[non.office==0])),0)

nf.oo.ag <- md.oo.ag * age.grp.work

# Proportion within-group mixing for nodematch()
match.oo.ag <- md.match$match[md.match$location == nw.loc]

nm.oo.ag <- edges.oo * match.oo.ag

target.stats.oo <- c(edges.oo,nf.oo.ag[3:6],nm.oo.ag)

formation.oo <- ~edges +
  nodefactor("age.grp", levels = 3:6) +
  nodematch("age.grp", diff = F) +
  offset(nodefactor("non.office", levels = -1))

coef.diss.oo <- dissolution_coefs(dissolution = ~offset(edges), duration = 1e5)

est.oo <- netest(nw, formation.oo, target.stats.oo, coef.diss.oo, coef.form = -Inf,
                 set.control.ergm = control.ergm(MCMLE.maxit = 500))
summary(est.oo)

dx.oo <- netdx(est.oo, nsims = 1, ncores = 1, nsteps = 1000, dynamic = TRUE,
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
               nwstats.formula = ~edges + nodefactor("age.grp",levels = NULL) +
                 nodematch("age.grp",diff = F) + nodefactor("non.office",levels = NULL),
               keep.tedgelist = TRUE)

print(dx.oo)

# pull positional ids and corresponding age groups
vertex.names <- get_vertex_attribute(nw, "vertex.names")
age.grp <- get_vertex_attribute(nw, "age.grp")
non.office <- get_vertex_attribute(nw, "non.office")
df <- as.data.frame(cbind(vertex.names,age.grp,non.office))
df.num <- df %>%
  group_by(age.grp,non.office) %>%
  tally(name = "num.participants")

# generate network summary statistics
dx.oo.sum <- as.data.frame(dx.oo) %>%
  rename(head = "tail", tail = "head") %>%
  add_row(as.data.frame(dx.oo)) %>%
  select(tail,head) %>%
  left_join(df, by = c("head" = "vertex.names")) %>%
  rename(head.age = age.grp) %>%
  left_join(df, by = c("tail" = "vertex.names")) %>%
  rename(tail.age = age.grp) %>%
  left_join(df.num %>% filter(non.office == 0),
            by = c("head.age" = "age.grp")) %>%
  group_by(head.age,tail.age,num.participants) %>%
  tally(name = "num.contacts") %>%
  ungroup() %>%
  group_by(head.age,num.participants) %>%
  mutate(total.contacts = sum(num.contacts)) %>%
  mutate(age.degree = num.contacts/num.participants,
         total.degree = total.contacts/num.participants)

dx.oo.match <- dx.oo.sum %>%
  filter(head.age == tail.age) %>%
  mutate(prop.match =  age.degree/total.degree)

dx.oo.mix <- dx.oo.sum %>%
  ungroup() %>%
  select(head.age,tail.age,age.degree) %>%
  pivot_wider(values_from = "age.degree",names_from = "tail.age")


## Community network -----------------------------------------------------------

# mean degree for non-office workers
md.cc.other <- sum(as.numeric(table(age.grp[non.office==1])) *
                     as.numeric(rowSums(poly.nothome)))/n.other

# mean degree for office workers
md.cc.work <- md %>%
  filter(location == "Community") %>%
  select(md) %>%
  as.numeric()

# total edges
edges.cc <- (md.cc.other * n.other + md.cc.work * n.work)/2

# contacts per age group for workers
# note this only includes age groups 20-60+
md.cc.ag.work <- cm2 %>%
  filter(location == "Community") %>%
  select(total_degree) %>%
  pull(total_degree) %>%
  as.numeric()
# adding md for other age groups (not in CorpMix data)
md.cc.ag.work <- c(0,0,md.cc.ag.work,1.7,1.7,0)
nf.cc.work <- md.cc.ag.work * age.grp.work

# contacts per age group for others
md.cc.other <- as.numeric(rowSums(poly.nothome))
nf.cc.other <- md.cc.other * (age.grp.num-age.grp.work)

# total contacts per age group
nf.cc.ag <- nf.cc.work + nf.cc.other

# within age-group mixing, using POLYMOD data
match.cc.other <- as.numeric(diag(poly.nothome/rowSums(poly.nothome)))
nm.cc <- (nf.cc.ag[1:7]/2) * match.cc.other[1:7]

target.stats.cc <- c(edges.cc,
                     md.cc.work * n.work, #community contacts among office workers
                     #nf.cc.ag,
                     nm.cc)

formation.cc <- ~edges +
  nodefactor("non.office", levels = 1) +
  nodefactor("age.grp", levels = 1:5) +
  nodematch("age.grp", diff = TRUE, levels = 1:7)

coef.diss.cc <- dissolution_coefs(dissolution = ~offset(edges), duration = 1)

est.cc <- netest(nw, formation.cc, target.stats.cc, coef.diss.cc,
                 set.control.ergm = control.ergm(MCMLE.maxit = 500))
summary(est.cc)

dx.cc <- netdx(est.cc, nsims = 1, ncores = 1, nsteps = 1000, dynamic = FALSE,
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
               nwstats.formula = ~edges + nodefactor("age.grp",levels = NULL) +
                 nodematch("age.grp",diff = T) + nodefactor("non.office",levels = NULL),
               keep.tedgelist = TRUE)

print(dx.cc)



## Save Network ----------------------------------------------------------------

est.oo <- trim_netest(est.oo)
est.cc <- trim_netest(est.cc)

est.simple <- list(est.oo, est.cc)
rm(list=setdiff(ls(), c("est.oo", "est.cc", "est.simple")))
saveRDS(est.simple, file = "data/input/est.simple.rds")



