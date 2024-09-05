library(EpiModel); library(dplyr);library(tibble);library(ggplot2); library(ggpubr)

# setting up the environment
layers = c("Home", "School", "Work", "Nonhome"); layer=layers[1]
networks = c("Rural", "Urban"); network=networks[1]
est_apch = "Stochastic-Approximation" # "mcmle"
percent_target_pop = 0.1

# reading files
# ## Simulated
# file.name_r <- 
#   paste0("./data/netest_outputs/netest_", 
#          layers, "__", networks[1],"__", est_apch,"__", percent_target_pop, ".Rds"
#   )
# 
# file.name_u <- 
#   paste0("./data/netest_outputs/netest_", 
#          layers, "__", networks[2],"__", est_apch,"__", percent_target_pop, ".Rds"
#   )
# 
# est_h_r  <- readRDS(file.name_r[1]); est_s_r  <- readRDS(file.name_r[2]); est_w_r  <- readRDS(file.name_r[3]); est_nh_r  <- readRDS(file.name_r[4])
# est_h_u  <- readRDS(file.name_u[1]); est_s_u  <- readRDS(file.name_u[2]); est_w_u  <- readRDS(file.name_u[3]); est_nh_u  <- readRDS(file.name_u[4])

## Observed
source("R/network_params_helper_functions.R")

### participant data
india_participant <- 
  readRDS("data/raw_data/india_participant_data_aim1.RDS")

### contact data 
india_contact <- 
  readRDS("data/raw_data/india_contact_data_aim1.RDS")

# Merging participant and contact data and processing them
india_mix <- participant_contact_merge(india_participant = india_participant, india_contact = india_contact)


node_attribute_target_stats <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))

### number of modeled populations
N_r=node_attribute_target_stats$attr$rural %>% nrow(); N_u=node_attribute_target_stats$attr$urban %>% nrow()



### number of study population
n_parti_r = india_participant %>% filter(study_site == "Rural") %>% pull(rec_id)  %>% length()
n_parti_u = india_participant %>% filter(study_site == "Urban") %>% pull(rec_id)  %>% length()

### summary statistics, provides duration of contacts
netstats <- readRDS("data/network_stats_attributes/network_params.Rds")
 



########################## Characterizing formation statistics ##########################
# Function characterizing 1) number of contact at the individual-level (outputted in "contact_count") over 2 day study period, 2)
# the status of if a participant and contact belonged to a specific mixing pattern of age group (outputted in "age.grp_mix_status"), and 
# 3) the status of whether participant and contact belonged to the same age group (outputted in "sameage.grp.count") for the contact data over the two-day period.
contact_count_rural <- # rural mixing
  contact_freq_site(india_mix. = india_mix, 
                    india_participant. = india_participant, 
                    india_contact. = india_contact,
                    study_site. ="Rural") 



contact_count_urban  <- # urban mixing
  contact_freq_site(india_mix. = india_mix , 
                    india_participant. = india_participant, 
                    
                    india_contact. = india_contact,
                    study_site. ="Urban") 


# Validation assessment
## Define function for validation
# degree_dx <- 
#   function(est_nw){
#     
#     # Add covariate for assessment to the formula
#     char_old_formula <- as.character(est_nw$formation)
#     char_old_formula[2] <- paste0(char_old_formula[2], "+ degree(d= 0:15)")
#     validate_formula <- as.formula(paste0(char_old_formula,   collapse = " "))
#     
#     # T-ERGM 
#     dx <-
#       netdx(est_nw,
#             nsims =  30,
#             ncores = 10,
#             nsteps = 1000,
#             nwstats.formula = validate_formula,
#             set.control.ergm = control.simulate.formula.ergm(MCMC.burnin = 200000,
#                                                              MCMC.interval = 25000),
#             set.control.tergm = control.simulate.formula.tergm(MCMC.burnin.min = 50000),
#             dynamic = TRUE,
#             skip.dissolution = FALSE
#       )
#     
#     dx
#   }
# ## netdx - rural home
# dx_h_r <- degree_dx(est_nw = est_h_r)
# ## netdx - rural school
# dx_s_r <- degree_dx(est_nw = est_s_r)
# ## netdx - rural work
# dx_w_r <- degree_dx(est_nw = est_w_r)
# ## netdx - rural nohome
# dx_nh_r <- degree_dx(est_nw = est_nh_r)
# 
# ## netdx - urban home
# dx_h_u <- degree_dx(est_nw = est_h_u)
# ## netdx - urban school
# dx_s_u <- degree_dx(est_nw = est_s_u)
# ## netdx - urban work
# dx_w_u <- degree_dx(est_nw = est_w_u)
# ## netdx - urban nohome
# dx_nh_u <- degree_dx(est_nw = est_nh_u)

## Plot netdx
# deg_dist <- function(dx, denom, title,ylab){
# degree_df_location <-
# which(rownames(dx$stats.table.formation) == "degree0"):which(rownames(dx$stats.table.formation) == "degree15")
# 
# degree_dist <- 
#   dx$stats.table.formation[degree_df_location,]
# 
# barplot(height = degree_dist$`Sim Mean`/denom,names.arg=0:15,  xlab= "Degree", ylab = ylab, main= title)
# 
# }


# # Degree distribution, simulated
# par(mfrow=c(2,4))
# ### Rural
# deg_dist(dx=dx_h_r, denom = 1, title = "Home, rural", ylab="Counts")
# deg_dist(dx=dx_s_r, denom = 1, title = "School, rural", ylab="Counts")
# deg_dist(dx=dx_w_r, denom = 1, title = "Work, rural", ylab="Counts")
# deg_dist(dx=dx_nh_r, denom = 1, title = "Nonhome, rural", ylab="Counts")
# 
# ### Urban
# deg_dist(dx=dx_h_u, denom = 1, title = "Home, urban", ylab="Counts")
# deg_dist(dx=dx_s_u, denom = 1, title = "School, urban", ylab="Counts")
# deg_dist(dx=dx_w_u, denom = 1, title = "Work, urban", ylab="Counts")
# deg_dist(dx=dx_nh_u, denom = 1, title = "Nonhome, urban", ylab="Counts")
# 
# 
# 
# 
# # Degree distribution, simulated, normalized
# par(mfrow=c(2,4))
# ### Rural
# deg_dist(dx=dx_h_r, denom = N_r, title = "Home, rural", ylab="Proportion")
# deg_dist(dx=dx_s_r, denom = N_r, title = "School, rural", ylab="Proportion")
# deg_dist(dx=dx_w_r, denom = N_r, title = "Work, rural", ylab="Proportion")
# deg_dist(dx=dx_nh_r, denom = N_r, title = "Nonhome, rural", ylab="Proportion")
# 
# ### Urban
# deg_dist(dx=dx_h_u, denom = N_u, title = "Home, urban", ylab="Proportion")
# deg_dist(dx=dx_s_u, denom = N_u, title = "School, urban", ylab="Proportion")
# deg_dist(dx=dx_w_u, denom = N_u, title = "Work, urban", ylab="Proportion")
# deg_dist(dx=dx_nh_u, denom = N_u, title = "Nonhome, urban", ylab="Proportion")


# # Checking number of people as the denominator for calculating mean degree
# ### observed count per participant
# contact_count_urban$contact_degree %>% group_by(contact_location) %>%  summarize(total_deg = sum(n_contacts), n_participant = n(), mean_deg = total_deg/n_participant)
# contact_count_rural$contact_degree %>% group_by(contact_location) %>%  summarize(total_deg = sum(n_contacts), n_participant = n(), mean_deg = total_deg/n_participant)
# ### 624 (u) +608 (r) =1232 - This is still less than 1248
# 
# india_contact %>% filter(study_site == "Rural") %>% pull(rec_id) %>% unique() %>% length()-
# india_mix %>% filter(study_site == "Rural") %>% pull(rec_id) %>% unique() %>% length()
# ### interpretation: 16 rural participants, existed in the contact data, were missed in the processed merged data
# 
# miss_parti <- 
# setdiff(
# india_contact %>% filter(study_site == "Rural") %>% pull(rec_id) %>% unique(), 
# india_mix %>% filter(study_site == "Rural") %>% pull(rec_id) %>% unique()
# )
# test <- 
# india_contact %>% filter(rec_id %in% miss_parti)
# test$rec_id %>% unique()
# ### These are the 16 rural participants (w/ contacts information) missed in the merged data. The issue is due to these participants had contact duration < 15mins in the participant_contact_merge function
# 
# india_contact %>% filter(study_site == "Urban") %>% pull(rec_id) %>% unique() %>% length()-
#   india_mix %>% filter(study_site == "Urban") %>% pull(rec_id) %>% unique() %>% length()
# ### As none of the 16 were in the rural data, the outcome is 0
# 
# unique(india_participant$rec_id) %>% length()
# unique(india_contact$rec_id) %>% length()
# setdiff(unique(india_participant$rec_id), unique(india_contact$rec_id)
#         )
# 
# 
# id_no_contact_ls15min.site_r <- # 20 participants that aren't in the merged  rural data, including 4 participants with no contacts and 16 participants whose all contacts ≤ 15 mins and w/o contacts > 15 mins 
#   india_participant %>%
#   filter(!(rec_id %in% india_mix$rec_id)) %>%
#   filter(study_site == "Rural") %>% pull(rec_id)
# 
# id_no_contact_ls15min.site_u <- # 2 aren't in the merged  urban data, including 2 participants with no contacts and 0 participants whose all contacts ≤ 15 mins and w/o contacts > 15 mins 
#   india_participant %>%
#   filter(!(rec_id %in% india_mix$rec_id)) %>%
#   filter(study_site == "Urban")  %>% pull(rec_id)
# 
# study_site.="Rural"
# test <- 
#   india_contact %>% filter(duration_contact %in% c("<5 mins", "5-15 mins") & study_site == study_site.)  %>% pull(rec_id) %>% unique()# id of participants w/ contact less than 15 mins
# 
# test1 <- 
#   india_contact%>% filter(! duration_contact %in% c("<5 mins", "5-15 mins") & study_site == study_site.)  %>% pull(rec_id) %>% unique()# id of participants w/ contact more than 15 mins
# 
# id_all_contact_ls_15min <- 
#   setdiff( test, test1) # id of participants whose all contacts ≤ 15 mins and w/o contacts > 15 mins 
# 
# setdiff(
#   id_no_contact_ls15min.site_r, id_all_contact_ls_15min
# ) # these are the 4 and 2 participant without contact when study_site.= "Rural" and "Urban", respectively
# 
# 
# ### check wither the 16 participants are included in the individual-level data for mean degree calculation
# contact_count_rural$contact_degree %>% View()

# # diagnose component size
# dynamic_r <- readRDS("~/OneDrive - Emory University/GlobalMix_COVID19/miscallaneous/networkdynamics__0.1/networkdynamic__Rural__mcmle__0.1.Rds")
# dynamic_u <- readRDS("~/OneDrive - Emory University/GlobalMix_COVID19/miscallaneous/networkdynamics__0.1/networkdynamic__Urban__mcmle__0.1.Rds")
# 
# ## evalluate component size at home, rural
# rh1 <- 
# networkDynamic::network.collapse(dynamic_r$Home, at=1)
# 
# rh1_cd <- 
# sna::component.dist(rh1, connected = "weak")
# 
# rh1_cd$csize %>% summary()
# 
# # mean deg of work 
# node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum()/N_r
# node_attribute_target_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work %>% sum()/N_u


# reweighting degree distribution
post_stratification <- 
  function(participant_dta,
           modeled_pop_dta,
           degree){
outputs <- degree_proportion <-  list()
    
# Compare age distribution of study and modeled populations
## study population age distribution
prop_parti <- participant_dta %>% pull(participant_age) %>% table %>% prop.table  

## modeled population age distribution
prop_node <- modeled_pop_dta$node.age.grp %>% table %>% prop.table 

## Visualize age distribution of the 2 populations
outputs$age_distribution <- 
rbind(
  prop_parti %>% as.data.frame() %>% rename(age.grp=1, proportion=2)%>% mutate(pop_type = "study_population"), 
  prop_node %>% as.data.frame() %>% rename(age.grp=1, proportion=2)%>% mutate(pop_type = "model_population")
  ) %>% ggplot(., aes(fill=pop_type, y = proportion, x= age.grp)
)+  geom_bar(position = "dodge", stat = "identity")+theme_classic()


# Age distribution reweighting
## post-stratification weight - higher weight assigned to the undersampled older population, lower weight assigned to the oversampled younger population
wi <- (prop_node/prop_parti) %>% as.data.frame() %>% rename(participant_age=1,weight=2)

outputs$w_ai <- wi

## Apply weight to each study participant's degre
weighted_degree <- 
degree %>%  # degree of individual i of the study population
  # joining weight to the degree dataframe
   left_join(., wi, by = "participant_age") %>% 
   mutate(
     n_contacts_1d= n_contacts/2,
     weighted_deg = round(n_contacts_1d*weight) # we consider the single-day degree as half of two-day degree
          ) 

## preparing dataframe for visualization
### weighted degree
ct_w <-  table(weighted_degree$contact_location, weighted_degree$weighted_deg) # degree count of each degree type, by contact location
ct_w <- as.data.frame.matrix(t(ct_w)) %>% rownames_to_column(., var="deg_type")  # transform to a dataframe

### unweighted degree
ct_un_w <- table(weighted_degree$contact_location, round(weighted_degree$n_contacts_1d)) # degree count of each degree type
ct_un_w <- as.data.frame.matrix(t(ct_un_w)) %>% rownames_to_column(., var="deg_type")  # transform to a dataframe

### plot w. two types of degree
prop_both <- rbind(ct_w %>% mutate(weight_type="weighted"), 
                 ct_un_w %>% 
                   mutate(weight_type="unweighted"
                          )
                 ) %>% 
  mutate_if(is.numeric, ~ . / 624) %>%  # normalize
  mutate(deg_type = as.factor(as.numeric(deg_type)))
  
outputs$deg_distribution_all <- 
ggarrange(
 ggplot(prop_both, aes(fill=weight_type, y = Home, x= deg_type)
                )+ 
   geom_bar(position = "dodge", stat = "identity")+theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)),
 ggplot(prop_both, aes(fill=weight_type, y = School, x= deg_type)
 )+ 
   geom_bar(position = "dodge", stat = "identity")+theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)),
 ggplot(prop_both, aes(fill=weight_type, y = Work, x= deg_type)
 )+ 
   geom_bar(position = "dodge", stat = "identity")+theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)),
 ggplot(prop_both, aes(fill=weight_type, y = Nonhome, x= deg_type)
 )+ 
   geom_bar(position = "dodge", stat = "identity")+theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)),
 legend = "bottom",
 common.legend = T
)

# plot with weighted degree only
create_bar_plot <- function(ct, layer) {
  ggplot(ct, aes(y = !!rlang::sym(layer), x = deg_type)) + 
    geom_bar(position = "dodge", stat = "identity", show.legend = F) +
    geom_text(aes(label = round(!!rlang::sym(layer), 2)), 
              size=2,
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              angle = 45) +
    theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


prop_w <-  prop_both %>% filter(weight_type =="weighted")
prop_un_w <- prop_both %>% filter(weight_type =="unweighted")


outputs$deg_distribution_weighted <- 
  ggarrange(
    create_bar_plot(ct=prop_w, layer = "Home"),
    create_bar_plot(ct=prop_w, layer = "School"),
    create_bar_plot(ct=prop_w, layer = "Work"),
    create_bar_plot(ct=prop_w, layer = "Nonhome"), nrow = 1, ncol = 4
  )

outputs$deg_distribution_unweighted <- 
  ggarrange(
    create_bar_plot(ct=prop_un_w, layer = "Home"),
    create_bar_plot(ct=prop_un_w, layer = "School"),
    create_bar_plot(ct=prop_un_w, layer = "Work"),
    create_bar_plot(ct=prop_un_w, layer = "Nonhome"), nrow = 1, ncol = 4
  )


# QC weighted degree, by age groups
weighted_degree <- weighted_degree %>%
  mutate(participant_age_bi = case_when(
    participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
    TRUE ~ "20+y"
  ))
## unweighted degree
ct_un_w_age <- table(weighted_degree$contact_location, round(weighted_degree$n_contacts_1d), weighted_degree$participant_age_bi) # degree count of each degree type
## weighted degree
ct_w_age <-  table(weighted_degree$contact_location, weighted_degree$weighted_deg, weighted_degree$participant_age_bi) # degree count of each degree type, by contact location







outputs$adjusted_prop <- prop_w


outputs
}

## run the function
### rural
dists_r_1d <- 
  post_stratification(
    participant_dta=india_participant %>% filter(study_site == "Rural")%>% select(study_site, participant_age),
    modeled_pop_dta = node_attribute_target_stats$attr$rural %>% select(node.age.grp),
    degree=contact_count_rural$contact_degree %>% data.frame()%>% mutate(n_contacts=round(n_contacts/2)) #contact counts at 2-day scale
  )


#### investigate the validity at k=4
dists_r_1d$age_distribution
dists_r_1d$w_ai
dists_r_1d$deg_distribution_all

###### compare the tabulations of the weighted and unweighted degree vs. group

# ggarrange(dists_r_2d$deg_distribution_weighted,
#           dists_r_1d$deg_distribution_weighted # this should be the case
#           )

### urban
dists_u_1d <- 
  post_stratification(
    participant_dta=india_participant %>% filter(study_site == "Urban")%>% select(study_site, participant_age),
    modeled_pop_dta = node_attribute_target_stats$attr$urban %>% select(node.age.grp),
    degree=contact_count_urban$contact_degree %>% data.frame()%>% mutate(n_contacts=round(n_contacts/2)) #contact counts at 2-day scale
  )

# ggarrange(dists_u_2d$deg_distribution_weighted,
#           dists_u_1d$deg_distribution_weighted # should be the case
# )




# weight distribution, 1-day, 2 networks
ggarrange(dists_r_1d$deg_distribution_weighted, dists_u_1d$deg_distribution_weighted, nrow = 2)


# age distributions and and weight distribution
ggarrange(
## weight distribution by age group and population distribution of study and target populations, rural, 1 day
ggarrange(
  dists_r_1d$age_distribution,
 ggbarplot(data= data.frame(dists_r_1d$w_ai), x="participant_age", y="weight"),
 nrow=2
),

## weight distribution by age group and population distribution of study and target populations, urban, 1day
ggarrange(
  dists_u_1d$age_distribution,
  ggbarplot(data= data.frame(dists_u_1d$w_ai), x="participant_age", y="weight"),
  nrow=2
), ncol=2

)

# weighted v. unweight degree distribution, 1 day
ggarrange(
dists_r_1d$deg_distribution_all,
dists_u_1d$deg_distribution_all, nrow=2,
labels = 
c("Rural", "Urban"),
label.y = 1
)

# weighted degree distribution, 1 day
ggarrange(
dists_r_1d$deg_distribution_weighted,
dists_u_1d$deg_distribution_weighted,
labels= c("Weighted distribution, rural", "Weighted distribution, urban"),
nrow=2
)


# target stat calculation and simulating nodal attribute
## function to calculate target statistics
deg_attribute_target_stats <- function(N_nodes, deg_dist)
  {
  output$target_stats <- output$degree_attribute <-output <- list()
  
  # nodal attribute
  ## function to generate degree nodal attribute based on the degree distribution (deg_dist)
  nodal_deg <- 
  function(deg_dist, layer){
  
  # find the last row in deg_dist having >0 probability
  rows_gt0_prob <-  which(c(deg_dist[[layer]] !=0)==T)
  
  # max_k is the deg_type whose subsequent larger degree types all have probability of 0 (max_k itself is the last degree type having >0 probability)
  max_k <- 
  deg_dist[
    rows_gt0_prob %>% max,
  ] %>% 
    pull(deg_type) %>% 
    droplevels() %>% as.character%>% as.numeric()
  
  # full degree distribution, adding unobserved degree in the empirical
  ful_deg <- 
  data.frame(deg_type=c(1:max_k)) %>% 
    left_join(deg_dist %>%
                select(deg_type,{{layer}}) %>% 
                mutate(deg_type= as.numeric(as.character(deg_type)))
              ) %>%  rename(prob = 2) %>% 
    mutate(prob = if_else(is.na(prob), 0, prob))
  
  # For each node, randomly assign a degree type, based on the full degree distribution
  nodal_degrees <- sample(x=ful_deg %>% pull(deg_type),
                          size = N_nodes, 
                          replace = TRUE, 
                          prob = ful_deg %>% pull(prob)
                          )
  
  nodal_degrees
  }
  
  output$degree_attribute$Home <- nodal_deg(deg_dist = deg_dist, layer = "Home")
  output$degree_attribute$School <- nodal_deg(deg_dist = deg_dist, layer = "School")
  output$degree_attribute$Work <- nodal_deg(deg_dist = deg_dist, layer = "Work")
  output$degree_attribute$Nonhome <- nodal_deg(deg_dist = deg_dist, layer = "Nonhome")
  
  # target statistics
  ## create range categories
  deg_dist <- 
  deg_dist %>% mutate(deg_type = as.numeric(as.character(deg_type))
                      ) %>% mutate(deg_range_6cat = case_when(deg_type >=4 & deg_type <= 20 ~ "4-20", # for the parameterization of - degree(0:3)+ degrange(from=4, to=20) + derange(from=21)
                                                         deg_type >=21  ~ ">=21",
                                                         T ~ as.character(deg_type)),
                                   deg_range_5cat = case_when(
                                                              deg_type >=4  ~ ">=4",
                                                              T ~ as.character(deg_type))
                                   )
  
  ## calculate total proportion for each range category
  ### home
  output$target_stats$deg_range_h <-
    deg_dist %>% select(deg_range_5cat, Home) %>% 
    group_by(deg_range_5cat) %>% 
    summarise(prop=sum(Home)) %>% 
    mutate(N_nodes_age= round(prop*N_nodes)) %>% 
    data.frame()
  
  ### School
  output$target_stats$deg_range_s <-
    deg_dist %>% select(deg_range_5cat, School) %>% 
    group_by(deg_range_5cat) %>% 
    summarise(prop=sum(School)) %>% 
    mutate(N_nodes_age= round(prop*N_nodes)) %>% 
    data.frame()
  
  ### Work
  output$target_stats$deg_range_w <-
    deg_dist %>% select(deg_range_5cat, Work) %>% 
    group_by(deg_range_5cat) %>% 
    summarise(prop=sum(Work)) %>% 
    mutate(N_nodes_age= round(prop*N_nodes)) %>% 
    data.frame()
  
  ### Nonhome
  output$target_stats$deg_range_nh <-
    deg_dist %>% select(deg_range_5cat, Nonhome) %>% 
    group_by(deg_range_5cat) %>% 
    summarise(prop=sum(Nonhome)) %>% 
    mutate(N_nodes_age= round(prop*N_nodes)) %>% 
    data.frame()
    
  output
  
  }

## rural
tstat_r <- 
  deg_attribute_target_stats(
  N_nodes = N_r,
  deg_dist = dists_r_1d$adjusted_prop)

##
tstat_r$target_stats

### experiment for home layer
deg_attr_h <- 
tstat_r$degree_attribute$Home %>% table() %>% data.frame() %>% rename(deg=1) %>% mutate(deg= as.numeric(as.character(deg)))

deg_attr_h $Freq %>% sum() # equal to the modeled population size

deg_attr_h %>% ggbarplot(x="deg", y = "Freq") # sample distribution as the weight distribution

tstat_r$degree_attribute$Home %>% sum # total degree, calculated from nodal attribute constraint

#### total degree from the age-mixing matrix for nodemix
node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home %>% sum # 16026< 37027 mean the total degree constraint is violated





