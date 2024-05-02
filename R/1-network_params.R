
####### Characterization of network statistics for GlobalMix India Data #######

# Methodological notes
# (1) There are two ways to derive network summary statistics, one is fitting poisson models to individual-level count data, 
# the other is based on summarizing contact data - the approach used in CorporateMix. Given it is easier to devise uncertainties using the glm method, 
# we go with this approach to calculate the summary network statistics. We regrouped the age categories in the first two decades of life by decade. 

# (2) We excluded contacts lasted ≤ 15 mins in analysis, given the interest on respiratory-related diseases. 

# (3) Given the data were egocentric, at the single-day scale, we use $M.D.=\frac{number of contacts}{n}$ to calculate mean degree. 
# Given the data are of contacts over two days, the single-day mean degree of of contact is half of the mean degree of two-day contact;
# However, the proportion of observing an edge at the two-day scale is the same as the single-day scale.

# (4) Characterization of network layers: The network layers is processed from multiple questions asking contact locations. 
# Given these questions are not mutually exclusive due to the check-box design in REDCap, we characterized contact location as the 
# location having primary contact, following the order of home, school, work, non-home (excluding work and school). There are two contacts 
# that occurred at both work (location 3 in REDCap) and school (location 2), we categorized them as at school, considering the participants 
# (and contacts) were at school age (10-19y, 0-9y). This logic is not spelled out in the below script but is adjusted by the higher priority 
# of school than work. For a contact checked none of the categories, we assigned an NA to this contact and excluded them considering them as missing data.

# (5) Zeroing out contacts in age groups. Given we observed very low level of degree in ties in the target statistics of nodemix and the observation of low degrees in some age group for nodefactor,
# we decide to zero out the contact degrees in some age groups. For the rural network, since the degrees of the modeled population and the individual-level mean degrees are low in the ≥40 age groups at 
# School and in the ≤19 age group at Work, we a priori exclude the contacts in these age groups when calibrating the summary statistics.
# For the urban network, we exclude the contacts in the ≥40 age groups at School and in the ≤9 age group at Work. 


# Load libraries and data  ------------------------------------------------
library("dplyr")
library("tidyr")
library("tibble")
library("sjlabelled")
library("stringr")
library("GGally")


## participant data
india_participant <- 
  readRDS("data/participant_contact/india_participant_data_aim1.RDS")



## contact data 
india_contact <- 
  readRDS("data/participant_contact/india_contact_data_aim1.RDS")

table(india_contact$hh_membership, india_contact$study_site )

# Note - validated study_site (i.e., rural/urban) of participant is exactly the same as those in contact

# Merging participant and contact data and processing them
## participant data
india_participant <- 
  india_participant %>%
  ### Reclassifying participant's and contact's age groups into 0-9 years old, 10-19 years old, while treating other age groups as is.
  mutate(
    participant_age = case_when(
      participant_age %in% c("<6mo", "6-11mo",   "1-4y",   "5-9y") ~ "0-9y",
      participant_age %in% c("10-14y", "15-19y") ~ "10-19y",
      .default =  participant_age
    )
  )%>% 
  mutate(participant_age= factor(participant_age, levels=c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y",   "60+y")
  )
  ) %>% mutate(participant_ageyr = floor(participant_ageyr)
  )

## contact data
india_contact <- 
  india_contact %>%     
  mutate(
    contact_age = case_when(
      contact_age %in% c("<6mo", "6-11mo",   "1-4y",   "5-9y") ~ "0-9y",
      contact_age %in% c("10-14y", "15-19y") ~ "10-19y",
      .default =  contact_age
    )
  )%>% 
  mutate(contact_age= factor(contact_age, levels=c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y",   "60+y")
  )
  )

## merge participants and contact datasets and basic data processing ------------------------------------------------
india_mix <- india_participant %>%
  dplyr::select(rec_id,participant_age, participant_ageyr) %>%
  right_join(india_contact,
             by = "rec_id") %>% 
  ### characterization of primary contact status, outputted categories are: Home, School, Work, and Nonhome
  mutate(contact_location = 
                                      # Home
                            case_when(location_contact___0 == 1  ~ "Home", # 0 - "My home"
                                      # School
                                      location_contact___2 == 1 & location_contact___0 == 0 ~ "School", # "School" but not "my home" 
                                      # Work
                                      location_contact___3 == 1 & (location_contact___0 == 0  | location_contact___2 ==0)~ "Work", # "work" but not "my home" / "school"
                                      # Nonhome
                                      (
                                        location_contact___1 == 1 | location_contact___4 == 1 | # "Other home",  "Transport/Hub"
                                          location_contact___5 == 1 | location_contact___6 == 1 | # "Market/Shop", "Street"
                                          location_contact___7 == 1 | location_contact___8 == 1 | # "Playground", "Well"
                                          location_contact___9 == 1 | location_contact___10 == 1 | # "Agricultural Field", "Market/Shop"
                                          location_contact___11 == 1 | location_contact___12 == 1 | # "Restaurant", "Religious centre"
                                          location_contact___13 == 1 | location_contact___14 == 1 | # "Govt offices", "Health center"
                                          location_contact___15 == 1 | location_contact___16 == 1 | # "Bank", "Movie theatre"
                                          location_contact___17 == 1 | location_contact___18 == 1 | location_contact___19 == 1 #"Exhibition", "Social hall", "Other"
                                      ) &
                                        !(location_contact___3 == 1 | location_contact___2 == 1 | location_contact___0 == 1) # Excluding work/school/home
                                      ~ "Nonhome") # "Nonhome" but not "home", "school", "work"
  )  %>% 
  ### Weighing in household membership in determining contact location - tabulating contact_location and hh_membership, 
  ### we found there are 58,1,3 household members had contacts in the nonhome, school, and work layers. We reclassify these as home contacts. 
  ### We also found 2176 "Non-members" at the home layer, and we reclassify them into non-home (other than school, work) contacts given the presumed turnover rate for this group is short. 
  mutate(
    contact_location = case_when(hh_membership == "Member" & contact_location %in% c("Nonhome", "School", "Work") ~ "Home",
                                 hh_membership == "Non-member" & contact_location %in% c("Home") ~ "Nonhome",
                                 T ~ contact_location
    )
    
  ) %>%
  mutate(contact_location = factor(contact_location, # leveling this variable
                                   levels = c("Home", "School", "Work", "Nonhome")
  )
  ) %>% 
  filter(!(duration_contact %in% c("<5 mins", "5-15 mins"))) %>% # filter out contact with duration ≤ 15 mins
  mutate(
    duration_contact = factor(duration_contact,
                              levels = c( "16-30 mins",  "31 mins-1 hr", "1-4 hrs", ">4 hrs", "Unknown")
    )
  )  %>% filter(!is.na(contact_location) # complete-case analysis - exclude contacts didn't have a contact location from analysis, Moses confirmed this is okay
  ) %>% 
  
  ### Distribution of indoor/outdoor status
  #### The variable "where_contact" indicating in- and out-door status. In the original data, there is a "Both" category indicates a participant contacted a contact both in- and out-doors. 
  #### Since the "Both" category could have a higher transmission potential similar to the "Indoors" category, we merge the "Both" and the "Indoors" categories. 
  #### We will adjust for this status when building the transmission model, in which we will assign lower risk for the "Outdoors" category during simulation.
  mutate(where_contact =case_when(where_contact %in% c("Indoors", "Both") ~ "Indoors_or_both",
                                  .default = where_contact)
  )


## Zeroing out contacts in age groups with very low mean degree and degree for urban and rural networks
india_mix <- 
india_mix %>% filter(
  ### Contacts to be zeroed out in the rural network, 1 contact at school of participant_age 40-59y to be excluded, 3 contacts at work of participant_age 10-19y (2 contacts) and 0-9y (1 contact) to be excluded
  ! (
    (study_site == "Rural")  &
    (
    (contact_location == "School" & participant_age %in% c("40-59y", "60+y"))|
       (contact_location == "Work" & participant_age %in% c("0-9y", "10-19y"))
     )
  )

     ) %>% 
  filter(
      ### Contacts to be zeroed out in the urban network, contacts in the ≥40 age groups at School and 4 contacts of participant_age = 0-9y at work to be excluded
      ! (
        (study_site == "Urban")  &
          (
            (contact_location == "School" & participant_age %in% c("40-59y", "60+y"))|
              (contact_location == "Work" & participant_age %in% c("0-9y"))
          )
      )
  )




## Check implementation of the cross-checking between household membership and contact location
table(
  india_mix$contact_location,
  india_mix$hh_membership
)


########################## Characterizing formation statistics ##########################
# Function characterizing 1) number of contact at the individual-level (outputted in "contact_count"), 2)
# the status of if a participant and contact belonged to a specific mixing pattern of age group (outputted in "age.grp_mix_status"), and 
# 3) the status of whether participant and contact belonged to the same age group (outputted in "sameage.grp.count") for the contact data over the two-day period.
source("R/contact_freq_site.R")
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





# function calculates two-day proportions of mixing between age groups of participant and contact 
# Note: 1)given both the numerator (i.e., number of edges matched to a patterm) and denominator (i.e., total number od edges in a layer) are divided by 2 for converting two- to one-day scale, 
#the two-day proportion is the same as the single-day propotion. 2) We characterize the proportion using both the glm and summary methods for cross-validation.
source("R/mix_prop.R")
# Characterizing mixing proportions by layer
## Note: the following use the mix_prop function to characterize the mixing proportion for all contact layers
## define function settings
locs <- c("Home", "School", "Work", "Nonhome")
unobs_grp_rural <- c("none", "40+y", "<19y", "none") # status of unobserved age group in each layer of the rural network
unobs_grp_urban <- c("none", "40+y", "<9y", "none") # status of unobserved age group in each layer of the urban network
mix_prop_rural_layers <- mix_prop_urban_layers <- list()

for (i in 1:length(locs)
) { # the warning "glm.fit: algorithm did not converge" occurs when there's no edge in an age group, yielding the glm estimated proportion to be a very small but non-zero number, but the observed proportion is equal to 0.
## mixing proportions for the rural network

  mix_prop_rural_layers[[i]] <- 
    mix_prop(mix_status_layer= contact_count_rural$age.grp_mix_status %>% filter( contact_location == locs[i]),
             unobserve_ego_age_grp  = unobs_grp_rural[i]
             )
  # adding layer's name here
  names(mix_prop_rural_layers[[i]]) <- paste0(locs[i], "_", names(mix_prop_rural_layers[[i]]))


## mixing proportions for the urban network
  mix_prop_urban_layers[[i]] <- 
    mix_prop(mix_status_layer= contact_count_urban$age.grp_mix_status %>% filter( contact_location == locs[i]),
             unobserve_ego_age_grp  = unobs_grp_urban[i]
             )
  # adding layer's name here
  names(mix_prop_urban_layers[[i]]) <- paste0(locs[i], "_", names(mix_prop_urban_layers[[i]]))
 
}

names(mix_prop_rural_layers) <- names(mix_prop_urban_layers) <-locs # assigning layer names to the list

## Compare the glm-based and crude proportions, and those proportions of matched age groups based on the ARTnet approach
### glm-based and crude proportions
mix_prop_rural_layers
mix_prop_urban_layers


# Function characterizing egocentric network statistics of age effect for edge, nodefactor, and nodematch
source("R/edge_node_factor_match.R")
edge_node_factor_match_rural <- edge_node_factor_match(contact_count_site = contact_count_rural)
edge_node_factor_match_urban <- edge_node_factor_match(contact_count_site = contact_count_urban)


# Correlation between layers
## Characterization of regression coefficients
## Function assessing associations of degree between layers using Poisson regression, with dichotomized degree as independent variable. 
source("R/assoc_btw_layers.R")
layer_assoc_rural <- assoc_btw_layers(contact_count_long=contact_count_rural$contact_degree)
layer_assoc_urban <- assoc_btw_layers(contact_count_long=contact_count_urban$contact_degree) 

## Visual evaluation of correlation between layers and assessing effect sizes
### We tabulated the degrees between layers. We observed the following - in both the rural and urban networks, the contacts at school and work are relatively separated
### spearman correlation,rural
layer_assoc_rural$data_wide %>% select(-c(rec_id, participant_age, Nonhome_cat, Home_cat, School_cat, Work_cat)) %>% 
  ggpairs(upper = list(continuous = wrap("cor", method = "spearman")
)
)
layer_assoc_rural$coefficient_summary

### spearman correlation,urban
layer_assoc_urban $data_wide %>% select(-c(rec_id, participant_age, Nonhome_cat, Home_cat, School_cat, Work_cat)) %>% 
  ggpairs(upper = list(continuous = wrap("cor", method = "spearman")
  )
  )
layer_assoc_urban$coefficient_summary


########################## Characterizing dissolution statistics of contact duration at school and work ##########################
source("R/know_dur.R")
known_dur_school_work <- know_dur(india_mix. = india_mix)



# Outputting network statistics
## formation stats
formation_stats_rural <- formation_stats_urban <- network_stats <- list()

formation_stats_rural$edge_node_factor_match_rural <- edge_node_factor_match_rural
formation_stats_rural$mix_prop_rural_layers <- mix_prop_rural_layers
formation_stats_rural$layer_assoc_rural <- layer_assoc_rural

formation_stats_urban$edge_node_factor_match_urban <- edge_node_factor_match_urban
formation_stats_urban$mix_prop_urban_layers <- mix_prop_urban_layers
formation_stats_urban$layer_assoc_urban <- layer_assoc_urban




network_stats$formation <- list(formation_stats_rural=formation_stats_rural, formation_stats_urban=formation_stats_urban)

network_stats$dissolution <- known_dur_school_work 

saveRDS(network_stats, file = "data/network_params/network_params.Rds")
