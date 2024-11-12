# Note: the purpose of this script is to calculate individual-level summary statistics

####### Characterization of network statistics for GlobalMix India Data #######

# Methodological notes
# (1) We use a common definition of an “edge” for a persistent network throughout this script and its functions: a contact between two people that has existed over a two-day period that we sampled from.
# (2) There are two ways to derive network summary statistics, one is fitting poisson models to individual-level count data, 
# the other is based on summarizing contact data - the approach used in CorporateMix. Given it is easier to devise uncertainties using the glm method, 
# we go with this approach to calculate the summary network statistics. We regrouped the age categories in the first two decades of life by decade. 
# (3) We excluded contacts lasted ≤ 15 mins in analysis, given the interest on respiratory-related diseases. 
# (4) Characterization of network layers: The network layers is processed from multiple questions asking contact locations. 
# Given these questions are not mutually exclusive due to the check-box design in REDCap, we characterized contact location as the 
# location of primary contact, following the order of home, school, work, non-home (excluding work and school). That is: 
# 4.1) if a participant reported the contact is at home, this contact (observed edge) is characterized as a home contact, regardless of the other contact locations. 
# 4.2) If a participant reported a contact is at school but not home, this contact is characterized as a school contact, regardless of the other contact locations. 
# 4.3) If a participant reported a contact is at work but not home and school, this contact is characterized as a work, regardless of the other contact locations.
# 4.4) If a participant reported a contact is at locations other than home, work, and school, this contact is characterized as a non-home contact.

# There are two contacts that occurred at both work (location 3 in REDCap) and school (location 2), we categorized them as at school, considering the participants 
# (and contacts) were at school age (10-19y, 0-9y). This logic is not spelled out in the below script but is adjusted by the higher priority 
# of school than work. For a contact checked none of the categories, we assigned an NA to this contact and excluded them considering them as missing data.

# (5) Zeroing out contacts in age groups. Given we observed very low level of degree in ties in the target statistics of nodemix and the observation of low degrees in some age group for nodefactor,
# we decide to zero out the contact degrees in some age groups. For the rural network, since the degrees of the modeled population and the individual-level mean degrees are low in the ≥40 age groups at 
# School and in the ≤19 age group at Work, we a priori exclude the contacts in these age groups when calibrating the summary statistics.
# For the urban network, we exclude the contacts in the ≥40 age groups at School and in the ≤9 age group at Work. 


# Load libraries, helper functions, and data  ------------------------------------------------
library("dplyr")
library("tidyr")
library("tibble")
library("sjlabelled")
library("stringr")
library("GGally")
library("gbp")
library("purrr")

source("R/network_params_helper_functions.R")

## participant data
india_participant <- 
  readRDS("data/raw_data/india_participant_data_aim1.RDS")

## contact data 
india_contact <- 
  readRDS("data/raw_data/india_contact_data_aim1.RDS")

table(india_contact$hh_membership, india_contact$study_site )

# Note - validated study_site (i.e., rural/urban) of participant is exactly the same as those in contact. Also, the number of rural and urban participants are both equal to 624

# Merging participant and contact data and processing them
india_mix <- participant_contact_merge(india_participant = india_participant, india_contact = india_contact) # the age group in india_participant is also been updated


## Check implementation of the cross-checking between household membership and contact location
table(
  india_mix$contact_location,
  india_mix$hh_membership
)


########################## Characterizing formation statistics ##########################
# Function characterizing 1) number of contact at the individual-level (outputted in "contact_count"), 2)
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

# Age distribution of the study population
prop_parti_rural <- 
india_participant %>% filter(study_site == "Rural") %>% pull(participant_age) %>% table %>% prop.table  

prop_parti_urban <- 
  india_participant %>% filter(study_site == "Urban") %>% pull(participant_age) %>% table %>% prop.table  


# function calculates two-day proportions of mixing between age groups of participant and contact 
# Note: We have characterized the proportion using both the glm and summary methods for cross-validation.
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
edge_node_factor_match_rural <- edge_node_factor_match(contact_count_site = contact_count_rural)
edge_node_factor_match_urban <- edge_node_factor_match(contact_count_site = contact_count_urban)



########################## Characterizing dissolution statistics of contact duration at school and work ##########################
known_dur_8_layers <- know_dur(india_mix. = india_mix)



########################## Characterizing proportions of household members at each age group  ##########################
hh_age_rural <- 
  india_mix %>% 
  filter(study_site == "Rural")%>% 
  filter(hh_membership == "Member") %>% # Considering each contact is with household members (hh_membership=="Member"), no matter the contact is unique/repeat. 
  select(rec_id, study_day, fromdayone, participant_age,contact_age)%>% 
  ## merging age categories into 3 and retrieving contacts with household members
  mutate(participant_age = case_when(participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
                                     participant_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                                     participant_age %in% c("60+y"  ) ~ "60-100y",
                                     T ~ participant_age
  ),
  contact_age = case_when(contact_age %in% c("0-9y", "10-19y") ~ "0-19y",
                          contact_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                          contact_age %in% c("60+y"  ) ~ "60-100y",
                          T ~ contact_age
  )
  ) %>% # if a contact on day 2 is a repeated contact, we filter it out
  filter(!(study_day == 2 & fromdayone == "Both days")
  )


hh_age_urban <- 
  india_mix %>% 
  filter(study_site == "Urban")%>% 
  filter(hh_membership == "Member") %>% # Considering each contact is with household members (hh_membership=="Member"), no matter the contact is unique/repeat. 
  select(rec_id, study_day, fromdayone, participant_age,contact_age)%>% 
  ## merging age categories into 3 and retrieving contacts with household members
  mutate(participant_age = case_when(participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
                                     participant_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                                     participant_age %in% c("60+y"  ) ~ "60-100y",
                                     T ~ participant_age
  ),
  contact_age = case_when(contact_age %in% c("0-9y", "10-19y") ~ "0-19y",
                          contact_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                          contact_age %in% c("60+y"  ) ~ "60-100y",
                          T ~ contact_age
  )
  )   %>% # if a contact on day 2 is a repeated contact, we filter it out
  filter(!(study_day == 2 & fromdayone == "Both days")
  )


# note: the output of function "proportion_hh_members" contains the proportions needed for generating the household ID attribute and the frequency of households having only children ("0-19y")
prop_hh_members_rural <- 
  proportion_hh_members(hh_age = hh_age_rural) 

prop_hh_members_urban <- 
  proportion_hh_members(hh_age = hh_age_urban)


# Outputting network statistics
## formation stats
formation_stats_rural <- formation_stats_urban <- network_stats <- list()

formation_stats_rural$edge_node_factor_match_rural <- edge_node_factor_match_rural
formation_stats_rural$mix_prop_rural_layers <- mix_prop_rural_layers


formation_stats_urban$edge_node_factor_match_urban <- edge_node_factor_match_urban
formation_stats_urban$mix_prop_urban_layers <- mix_prop_urban_layers


### individual-level raw 2-day degree, age distribution of participants
formation_stats_rural$degree_related$contact_count_2d <- contact_count_rural$contact_degree; formation_stats_rural$degree_related$prop_parti <- prop_parti_rural # rural
formation_stats_urban$degree_related$contact_count_2d <- contact_count_urban$contact_degree; formation_stats_urban$degree_related$prop_parti <- prop_parti_urban # urban


network_stats$formation <- list(formation_stats_rural=formation_stats_rural, formation_stats_urban=formation_stats_urban)

## Dissoluation stats
network_stats$dissolution <- known_dur_8_layers 

## Proportion of household memeber of each age group
network_stats$prop_hh_members <- list(prop_hh_members_rural =prop_hh_members_rural, prop_hh_members_urban=prop_hh_members_urban)

saveRDS(network_stats, file = "data/network_stats_attributes/network_params.Rds")
