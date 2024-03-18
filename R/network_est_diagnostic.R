lapply(c("tidyverse", "EpiModel", "ggpubr", "knitr", "svglite", "kableExtra"), require, character.only = TRUE)

# Loading data
## target statistics
attri_tarstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_targetstats.RData")

## summary statistics, provides duration of contacts
netstats <- readRDS("~/Documents/GitHub/COVID-GlobalMix/data/network_params/network_params.RData")

############## Set up vertex attribute ##############
# Total number of nodes in each network - difference caused by rounding
n_node_rural = attri_tarstats$attr$rural %>% nrow() # compared to 117,808
n_node_urban= attri_tarstats$attr$urban %>% nrow() # compared to 257,977


# Initiate nodes
nw_rural <- network_initialize(n_node_rural)
nw_urban <- network_initialize(n_node_urban)

# Nodes w/ age groups, each layer of a network has the same age attribution
nw_rural <- set_vertex_attribute(nw_rural, attrname = "age.grp",
                                value= as.character(attri_tarstats$attr$rural$target_age_grp )
                                )

## Adding nodal attribute (contact at school) of contact for the x-layer effect of work-layer predicted effect on school
nw_rural_s <- set_vertex_attribute(nw_rural, attrname = "deg.work", 
                                   value = as.character(attri_tarstats$attr$rural$contact_attribute_School
                                                        ) 
)


############## Set up target statistics  ##############
# Note: we treat the 1st age group (0-10 years old) as reference group

# Target statistics of nodemix at school, rural
## Write a function to pull target statistics from list and organize them in lexicographical order for model fitting
nmix_tar_lex <- 
function(edge_ct_mx){
  matrix <- edge_ct_mx %>% as.matrix()
  target_nmix_vec <- c(matrix[,1][1] %>% as.numeric(), 
                       matrix[,2][1:2] %>% as.numeric(),
                       matrix[,3][1:3] %>% as.numeric(),
                       matrix[,4][1:4] %>% as.numeric(),
                       matrix[,5][1:5] %>% as.numeric(),
                       matrix[,6][1:6] %>% as.numeric()
  ) # target stat of nodemix in lexicographic order
  
  data.frame(target_nmix_vec) %>% rownames_to_column(var= "lexi_order") %>% # create a column indicating the lexicographic order
    mutate(lexi_order = as.numeric(as.character(lexi_order))) %>% 
    mutate(mx_loc = c("1_1",
                      paste0(c(1:2), "_2"),
                      paste0(c(1:3), "_3"),
                      paste0(c(1:4), "_4"),
                      paste0(c(1:5), "_5"),
                      paste0(c(1:6), "_6"))
           )
}

## Target statistics of nodemix at each layer, rural
target_nmix_vec <- lapply( attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix, nmix_tar_lex)



# formation model formulas and target statistics.
## Conditioon to try
layer <- "School"
form_model = "nmix"
## model for basedline covariates 
if(form_model == "base"){
### w/ node factor and differential nodematch
frmn_fm <- 
  ~edges + 
  nodefactor("age.grp", levels = -1) +
   nodematch("age.grp", diff=T)
  #nodefactor("deg.work", levels = -1) # the category w/o contact at work layer is treated as reference group
} else if (form_model == "nmix"){
  ### fully saturate model w/o node factor
  frmn_fm <- 
    ~edges+ 
    nodemix("age.grp", levels2 = -1)# + # lexicographic order in nodemix, the 1st value excluded as reference group
} else if (form_model == "nmix_parsimonious"){
  
  lex_order_large_ct <- 
  target_nmix_vec[["School"]] %>% arrange(desc(target_nmix_vec)) %>% # arrange by edge count from big to small
    slice(1:10) %>% # take the first 10 biggest edge counts
    arrange(lexi_order) %>% pull(lexi_order)
  
  lex_order_large_ct  <- paste0(lex_order_large_ct, collapse =",")
  
  frmn_fm <- 
    paste0(
      "~edges +",
      "nodemix('age.grp', levels2 = c( " lex_order_large_ct " ))"
    )
  
}

#### corresponding target statistics under different layers

if(form_model == "base"){
tstat.w_ergm.ego <- c(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge %>% 
                            filter(contact_location == layer ) %>% 
                            pull(edges), # edge
                          
                          (attri_tarstats$targetstats_age.grp$formation_stats_rural$nf.age.grp %>% filter(contact_location == layer) %>% pull(nf.ag))[-1],  # nodefactor, excluding 1st age group, which is the reference group
                          
                         # c(target_nmix_vec[c(1, 3)], 0,0,0,0)
                        target_nmix_vec[[layer]]$target_nmix_vec[c(1, 3, 6, 10, 15,21)]  # matched edges from nodemix
                           # c( 309.72358, 1023.72835,   25.74787 ,  11.51628 ,        0     ,    0) # with this for rural School, the model can be run
                          #attri_tarstats$targetstats_x.layer$rural %>% filter(association == "s_by_w") %>% pull(nf_other_layer_1) # ties at Home layer when there's contact at work layer
)
}else{
  tstat.w_ergm.ego <- c(attri_tarstats$targetstats_age.grp$formation_stats_rural$edge %>% 
                          filter(contact_location == layer ) %>% 
                          pull(edges), # edge
                        target_nmix_vec[[layer]]$target_nmix_vec[-1]  #  edges counts from nodemix
                       )
}

attri_tarstats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School


# Dissolution model statistics
diss <-  
  if(layer == "Home"){
  dissolution_coefs(dissolution = ~offset(edges), 
                    duration =1e6)
  }else if (layer == "School"){
    dissolution_coefs(dissolution = ~offset(edges), 
                      duration = netstats$dissolution %>% filter(study_site =="Rural" & contact_location == "School") %>% pull(know_contact_duration)
                      )
    }


# Model fitting and simulation
#### Estimating using stochastic approximation and simulate using static network - ergm.ego target statistics
##### "Stochastic-Approximation"
est <- 
  netest(nw_rural, 
         formation = frmn_fm, 
         target.stats = tstat.w_ergm.ego, 
         coef.diss =  diss,
         set.control.ergm = 
           control.ergm(
             main.method = "Stochastic-Approximation", # adapted from https://github.com/EpiModel/EpiModelHIV-Template/commit/fd2f0ad58ef62dcf68824e593e2a067e226124dc
             MCMLE.maxit = 500,
             SAN.maxit = 3,
             SAN.nsteps.times = 4,
             MCMC.samplesize = 1e4,
             MCMC.interval = 5e3,
             parallel = 1
           )
         )


##### MCMLE 
est.mcmle <- 
  netest(nw_rural, 
         formation = frmn_fm, 
         target.stats = tstat.w_ergm.ego, 
         coef.diss =  diss,
         edapprox = T
         
  ) # interpretation: MCMLE running failed

#### Based on the estimates, simulating network - ergm.ego target statistics
sim <- 
  netdx(est.mcmle,  
        nsims = 100, 
        ncores = 1, 
        nsteps = 500, 
        nwstats.formula =  frmn_fm, 
        #set.control.tergm = control.simulate.formula.tergm(MCMC.burnin = 2e5),
        dynamic = T,
        skip.dissolution = T
        #keep.tedgelist = TRUE
) # encounter cryptic error 

sim





