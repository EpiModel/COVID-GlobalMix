
####### Characterization of network statistics for GlobalMix India Data #######

# Methodological notes
# (1) There are two ways to derive network summary statistics, one is fitting poisson models to individual-level count data, 
# the other is based on summarizing contact data - the approach used in CorporateMix. Given it is easier to devise uncertainties using the glm method, 
# we go with this approach to calculate the summary network statistics. We regrouped the age categories in the first two decades of life by decade. 

# (2) We excluded contacts lasted ≤ 15 mins in analysis, given the interest on respiratory-related diseases. 

# (3) Given the data were egocentric, at the single-day scale, we use $M.D.=\frac{edges}{n}$ to calculate mean degree ($M.D.$) from the number of edges. 
# Given the data are of contacts over two days, the single-day mean degree of of contact is half of the mean degree of two-day contact;
# However, the proportion of observing an edge at the two-day scale is the same as the single-day scale.

# (4) Characterization of network layers: The network layers is processed from multiple questions asking contact locations. 
# Given these questions are not mutually exclusive due to the check-box design in REDCap, we characterized contact location as the 
# location having primary contact, following the order of home, school, work, non-home (excluding work and school). There are two contacts 
# that occurred at both work (location 3 in REDCap) and school (location 2), we categorized them as at school, considering the participants 
# (and contacts) were at school age (10-19y, 0-9y). This logic is not spelled out in the below script but is adjusted by the higher priority 
# of school than work. For a contact checked non of the categories, we assigned an NA to this contact and excluded them considering them as missing data.

# (5) Zeroing out contacts in age groups. Given we observed very low level of degree in ties in the target statistics of nodemix and the observation of low degrees in some age group for nodefactor,
# we decide to zero out the contact degrees in some age groups. For the rural network, since the degrees of the modeled population and the individual-level mean degrees are low in the ≥40 age groups at 
# School and in the ≤19 age group at Work, we a priori exclude the contacts in these age group when calibrating the network statistics.
# For the urban network, we exclude the contacts in the ≥40 age groups at School and in the ≤9 age group at Work. 


# Load libraries and data  ------------------------------------------------
library("ggpubr")
library("statnet")
library("EpiModel")
library("tidyverse")
library("socialmixr")
library("knitr")
library("sjlabelled")
library("kableExtra")
library("broom")
library("stringr")
library("GGally")


## participant data
india_participant <- 
  readRDS("data/participant_contact/india_participant_data_aim1.RDS")



## contact data 
india_contact <- 
  readRDS("data/participant_contact/india_contact_data_aim1.RDS")

table(india_contact$hh_membership, india_contact$study_site )%>%
  kbl(caption = "Household membership status in GlobalMix India data") %>%
  kable_classic(full_width = F, html_font = "Cambria")

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
  mutate(contact_location = case_when(location_contact___0 == 1  ~ "Home", # 0 - "My home"
                                      location_contact___2 == 1 & location_contact___0 == 0 ~ "School", # "School" but not "my home" 
                                      location_contact___3 == 1 & (location_contact___0 == 0  | location_contact___2 ==0)~ "Work", # "work" but not "my home" / "school"
                                      
                                      
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
  ### Weighing in household membership in determining contact location - tabulating contact_location and hh_membership, we found there are 58,1,3 household members had contacts in the nonhome, school, and work layers. We reclassify these as home contacts. We also found 2176 non-members at the home layer, and we reclassify them into non-home (other than school, work) contacts given the presumed turnover rate for this group is short. 
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
  #### The variable "where_contact" indicating in- and out-door status. In the original data, there is a "Both" category indicates a participant contacted a contact both in- and out-doors. Since the "Both" category could have a higher transmission potential similar to the "Indoors" category, we merge the "Both" and the "Indoors" categories. 
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
      ### Contacts to be zeroed out in the urban network, 4 contacts of participant_age = 0-9y at work to be excluded
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

# Function characterizing uncertainties of network statistics and scaling them from two to one day scale
ci_95 <- # to-do
  function(glm_ouput, num_sample, link){
    
    if(link == "log"){
      glm_ouput %>% 
        mutate(
          single_day_md_link = log(single_day_md), # single-day mean degree, link scale
          
          # uncertainties, link-scale
          two_day_sd_link = `Std. Error`*sqrt(num_sample), # two-day standard deviation for the coefficient at the link scale
          single_day_sd_link = two_day_sd_link/2,  # single-day standard deviation for the coefficient at the link scale
          single_day_se_link = single_day_sd_link/sqrt(num_sample), # single-day standard error for the coefficient at the link scale
          
          
          # uncertainties, original scale
          single_day_lower95ci = exp(single_day_md_link -1.96*single_day_se_link),
          single_day_upper95ci =  exp(single_day_md_link +1.96*single_day_se_link)
        )
    } else {
      glm_ouput %>% 
        mutate(
          single_day_md_link = logit(single_day_md), # single-day mean degree, link scale
          
          # uncertainties, link-scale
          two_day_sd_link = `Std. Error`*sqrt(num_sample), # two-day standard deviation for the coefficient at the link scale
          single_day_sd_link = two_day_sd_link/2,  # single-day standard deviation for the coefficient at the link scale
          single_day_se_link = single_day_sd_link/sqrt(num_sample), # single-day standard error for the coefficient at the link scale
          
          
          # uncertainties, original scale
          single_day_lower95ci = expit(single_day_md_link -1.96*single_day_se_link),
          single_day_upper95ci =  expit(single_day_md_link +1.96*single_day_se_link)
        )
      
    }
    
  }


## Uncertainties of network statistics
expit <- function(x){1/(1+exp(-x))}
logit <- function(x){log(x/(1-x))}


########################## Characterizing formation statistics ##########################
# Function characterizing 1) number of contact at the individual-level (outputted in "contact_count"), 2)
# the status of if a participant and contact belonged to a specific mixing pattern of age group (outputted in "age.grp_mix_status"), and 
# 3) the status of whether participant and contact belonged to the same age group (outputted in "sameage.grp.count") for the contact data over the two-day period.
contact_freq_site <- function(india_mix., india_participant., india_contact., study_site.){
  india_mix.site <- 
    india_mix.  %>% 
    filter(study_site == study_site.) # here we split the dataset into urban or rural samples
  
  
  id_no_contact.site <- # participants with 0 contacts by study site in India
    india_participant. %>%
    filter(!(rec_id %in% india_contact.$rec_id)) %>%
    filter(study_site == study_site.) 
  
  
  locations <- levels(india_mix.site$contact_location) # retrieve the full levels of contact locations
  
  
  #### Total number of contacts over two days, for ~ edge and nodefactor ####
  contact_count <- 
    india_mix.site %>%
    group_by(rec_id, contact_location, fromdayone, .drop = F)%>% summarise(n_contacts = n()) %>% # summarizing number of contacts by characteristics included in "group_by"
    ungroup() %>% 
    mutate(
      n_contacts = case_when(
        contact_location %in% c("Home", "Work","School") ~ n_contacts, # for these layers, a replicated contact is counted as unique contact
        contact_location %in% c("Nonhome" ) & fromdayone == "Both days" ~ n_contacts*2, # for non-home layer, a replicated contact is counted as two contact
        contact_location %in% c("Nonhome" ) & fromdayone %in% c("Day 1", "Day 2") ~ n_contacts, # for non-home layer, a unique contact is counted as one contact
        T ~ n_contacts # for observation with fromdayone information missed, we return the original n_contacts
      )  
    )%>% 
    group_by(rec_id, contact_location, .drop = F) %>% summarise(n_contacts = sum(n_contacts)
    ) %>% ungroup() %>% # total number of contacts of each participant at each contact location over two days
    # add the participants w/o contact to the data
    rbind(., 
          data.frame(
            rec_id = rep(as.character(id_no_contact.site$rec_id), each = length(locations)),
            contact_location= rep(locations, 
                                  nrow(id_no_contact.site)
            ),
            n_contacts=rep(0,  nrow(id_no_contact.site)*length(locations))
          )
    ) %>% 
    # joining w/ participant's age
    left_join(india_participant. %>% select(rec_id, participant_age),
              by = "rec_id"
    )
  
  
  #### Status of whether an edge is matched to a specific pattern of mixing between two age groups, for nodemix ####
  age.grp_mix_status <- 
    india_mix.site %>% 
    
    # The following levels are used to code the age groups "0-9y"~1, "10-19y"~2, "20-29y"~3, "30-39y"~4, "40-59y"~5, "60+y"~6. 
    # If an edge/contact's age groups matched with one of the 36 patterns, it will be assigned 1 for the corresponding matching pattern
    mutate(age.grp1_1 = ifelse(participant_age == "0-9y" & contact_age ==  "0-9y", 1, 0),
           age.grp1_2 = ifelse(participant_age == "0-9y" & contact_age ==  "10-19y", 1, 0),
           age.grp1_3 = ifelse(participant_age == "0-9y" & contact_age ==  "20-29y" , 1, 0),
           age.grp1_4 = ifelse(participant_age == "0-9y" & contact_age ==  "30-39y" , 1, 0),
           age.grp1_5 = ifelse(participant_age == "0-9y" & contact_age ==  "40-59y", 1, 0),
           age.grp1_6 = ifelse(participant_age == "0-9y" & contact_age ==  "60+y", 1, 0),
           
           age.grp2_1 = ifelse(participant_age == "10-19y" & contact_age ==  "0-9y", 1, 0),
           age.grp2_2 = ifelse(participant_age == "10-19y" & contact_age ==  "10-19y", 1, 0),
           age.grp2_3 = ifelse(participant_age == "10-19y" & contact_age ==  "20-29y" , 1, 0),
           age.grp2_4 = ifelse(participant_age == "10-19y" & contact_age ==  "30-39y" , 1, 0),
           age.grp2_5 = ifelse(participant_age == "10-19y" & contact_age ==  "40-59y", 1, 0),
           age.grp2_6 = ifelse(participant_age == "10-19y" & contact_age ==  "60+y", 1, 0),
           
           age.grp3_1 = ifelse(participant_age == "20-29y" & contact_age ==  "0-9y", 1, 0),
           age.grp3_2 = ifelse(participant_age == "20-29y" & contact_age ==  "10-19y", 1, 0),
           age.grp3_3 = ifelse(participant_age == "20-29y" & contact_age ==  "20-29y" , 1, 0),
           age.grp3_4 = ifelse(participant_age == "20-29y" & contact_age ==  "30-39y" , 1, 0),
           age.grp3_5 = ifelse(participant_age == "20-29y" & contact_age ==  "40-59y", 1, 0),
           age.grp3_6 = ifelse(participant_age == "20-29y" & contact_age ==  "60+y", 1, 0),
           
           age.grp4_1 = ifelse(participant_age == "30-39y" & contact_age ==  "0-9y", 1, 0),
           age.grp4_2 = ifelse(participant_age == "30-39y" & contact_age ==  "10-19y", 1, 0),
           age.grp4_3 = ifelse(participant_age == "30-39y" & contact_age ==  "20-29y" , 1, 0),
           age.grp4_4 = ifelse(participant_age == "30-39y" & contact_age ==  "30-39y" , 1, 0),
           age.grp4_5 = ifelse(participant_age == "30-39y" & contact_age ==  "40-59y", 1, 0),
           age.grp4_6 = ifelse(participant_age == "30-39y" & contact_age ==  "60+y", 1, 0),
           
           age.grp5_1 = ifelse(participant_age == "40-59y" & contact_age ==  "0-9y", 1, 0),
           age.grp5_2 = ifelse(participant_age == "40-59y" & contact_age ==  "10-19y", 1, 0),
           age.grp5_3 = ifelse(participant_age == "40-59y" & contact_age ==  "20-29y" , 1, 0),
           age.grp5_4 = ifelse(participant_age == "40-59y" & contact_age ==  "30-39y" , 1, 0),
           age.grp5_5 = ifelse(participant_age == "40-59y" & contact_age ==  "40-59y", 1, 0),
           age.grp5_6 = ifelse(participant_age == "40-59y" & contact_age ==  "60+y", 1, 0),
           
           age.grp6_1 = ifelse(participant_age == "60+y"  & contact_age ==  "0-9y", 1, 0),
           age.grp6_2 = ifelse(participant_age == "60+y"  & contact_age ==  "10-19y", 1, 0),
           age.grp6_3 = ifelse(participant_age == "60+y"  & contact_age ==  "20-29y" , 1, 0),
           age.grp6_4 = ifelse(participant_age == "60+y"  & contact_age ==  "30-39y" , 1, 0),
           age.grp6_5 = ifelse(participant_age == "60+y"  & contact_age ==  "40-59y", 1, 0),
           age.grp6_6 = ifelse(participant_age == "60+y"  & contact_age ==  "60+y", 1, 0)
           
    ) %>% 
    # select subset of variables for output
    select(rec_id, contact_location, fromdayone, participant_age, contact_age, age.grp1_1:age.grp6_6
    )
  
  age.grp_mix_status <-
    age.grp_mix_status %>% filter(fromdayone == "Both days" & contact_location == "Nonhome")%>% slice(rep(1:n(), each = 2) # for a contacts/edges that repeated over the two-day period at the non-home layer, we replicate it to two edges (i.e., two rows).
    ) %>% 
    rbind(., 
          age.grp_mix_status %>% filter(
            (!(fromdayone == "Both days" & contact_location == "Nonhome")) | is.na(fromdayone) # for other contacts/edges (than the above ones) and for those having missing value for fromdayone, we treat it as a single edge (i.e., one row).
          ) 
    ) %>% 
    select(-fromdayone) # exclude this variable as it's not needed
  
  
  #### Status of whether age groups of participant and contact are the same, edge-level, for nodematch ####
  same.age.grp_status <- 
    india_mix.site %>% 
    mutate(same.age.grp = ifelse(participant_age == contact_age, 1, 0) # characterize whether participant and contact are in the same age group
    )  %>% 
    select(rec_id,  contact_location, fromdayone, participant_age, contact_age, same.age.grp
    )
  
  same.age.grp_status <- 
    same.age.grp_status %>% filter(fromdayone == "Both days" & contact_location == "Nonhome")%>% slice(rep(1:n(), each = 2) # for a contacts/edges that repeated over the two-day period at the non-home layer, we treat it as two edges (i.e., two rows).
    ) %>% 
    rbind(., 
          same.age.grp_status %>% filter(
            (!(fromdayone == "Both days" & contact_location == "Nonhome")) | is.na(fromdayone) # for other contacts/edges (than the above ones) and for those having missing value for fromdayone, we treat it as a single edge (i.e., one row).
          ) 
    ) %>% 
    select(-fromdayone) # exclude this variable as it's not needed
  
  
 
  
  
  out <-list(contact_count, same.age.grp_status, age.grp_mix_status)
  names(out) <- c("contact_degree", "same.age.grp_status" , "age.grp_mix_status")
  
  out
  
}

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
mix_prop <- # to-do: characterizing uncertainties 
  function(mix_status_layer,# the mixing statuses of a single layer over the two-day period 
           unobserve_ego_age_grp 
           # The "unobserve_ego_age_grp" argument indicating which egocentric age groups weren't observed to have contact or whose contacts where excluded.
           # For these unobserved egocentric age groups, their corresponding proportions won't be characterized, yielding NA
  ){
    
    age.grps <- c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y") # the 6 age groups
    
    # creating dataframes/list to store results
    ## data frames storing mixing matrix by glm and crude approaches
    mix_prop_matrix_2d_glm <- mix_prop_matrix_2d_crude<-  data.frame(matrix(NA, 6,6)
    )
    rownames(mix_prop_matrix_2d_glm) <- rownames(mix_prop_matrix_2d_crude) <-paste0("ego_",  age.grps)
    colnames(mix_prop_matrix_2d_glm) <- colnames(mix_prop_matrix_2d_crude) <-paste0("contact_",  age.grps)
    
    ## storing dataframes in a list
    output <- list()
    
    ############# GLM-based #############
    all_mix_patterns <- mix_status_layer %>% select(age.grp1_1:age.grp6_6) %>% names() # names of all mixing between age groups
    
    glm_nmix <- # regress the mixing status of each type on age group, the time scale of the data is in two days
      lapply( all_mix_patterns, function(x) { 
        
        glm(substitute(i ~  -1+participant_age, # slope-only model, where the sloop is logit(proportion) 
                       list(i = as.name(x))), family = "binomial", data = mix_status_layer) 
        
      } )
    # Note: the warning of "glm.fit: algorithm did not converge" occurs when all the mixing status == 0
    
    glm_nmix_summary <- lapply(glm_nmix, summary)
    
    
    
    # Filling the two-day proportion to the mixing matrix, the row index (i) and column index correspond to the egocentric and contact's age groups
    if(unobserve_ego_age_grp == "none"){ # if contacts existed in all the 6 egocentric age groups, we fill all the six corresponding matrix rows. This scenario applies to the home and nonhome layers
      for (i in 1:6 
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit() # convert the regression to proportion
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    }  else if (unobserve_ego_age_grp == "40+y") { # if the last two oldest egocentric age groups didn't have contact, we fill the first four corresponding matrix rows. This scenario applies to the school layer
      
      for (i in 1:4
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    } else if (unobserve_ego_age_grp == "<19y") { # if the first two youngest egocentric age groups didn't have contact, we fill the last four corresponding matrix rows. This scenario applies to the rural work layer
      
      for (i in 3:6
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit() # Given there is an mismatch between the i of the loop and the location in the coefficient, we use "-2" to handle the mismatching in coefficients[i-2,"Estimate"] 
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i-2,"Estimate"] %>% expit()
        
      }
      
    } else if (unobserve_ego_age_grp == "<9y") { # if the first youngest egocentric age group didn't have contact, we fill the last five corresponding matrix rows. This scenario applies to the urban work layer
      
      for (i in 2:6
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit() # Given there is an mismatch between the i of the loop and the location in the coefficient, we use "-1" to handle the mismatching in coefficients[i-1,"Estimate"] 
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i-1,"Estimate"] %>% expit()
        
      }
      
    }
    
    
    
    
    ############## crude calculation #########
    # fill the two-day proportion to the matrix 
    for (i in 1:6 # for each egocentric age group, we calculate its proportion of contact with the contact group. 
         #For egocentric age group that isn't available in a layer, NaN is returned for the corresponding row.
    ) { 
      mix_status_layer_i <- mix_status_layer %>% filter( participant_age == age.grps[i]) 
      mix_prop_matrix_2d_crude[i, 1] <- mix_status_layer_i[, (5+6*(i-1))] %>% mean()
      mix_prop_matrix_2d_crude[i, 2] <-  mix_status_layer_i[, (6+6*(i-1))] %>% mean()
      mix_prop_matrix_2d_crude[i, 3] <-  mix_status_layer_i[, (7+6*(i-1))] %>% mean()
      mix_prop_matrix_2d_crude[i, 4] <-  mix_status_layer_i[, (8+6*(i-1))] %>% mean()
      mix_prop_matrix_2d_crude[i, 5] <-  mix_status_layer_i[, (9+6*(i-1))] %>% mean()
      mix_prop_matrix_2d_crude[i, 6] <-  mix_status_layer_i[, (10+6*(i-1))] %>% mean()
      
    }
    
    output$mix_prop_matrix_2d_glm <- mix_prop_matrix_2d_glm 
    output$mix_prop_matrix_2d_crude <- mix_prop_matrix_2d_crude
    
    output  
    
  }




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
mix_prop_rural_layers[[1]]
mix_prop_urban_layers[[3]]

### proportions of matched age groups based on dataframe of the ARTnet approach
contact_count_rural$same.age.grp_status %>% filter(contact_location == "Home")  %>% group_by(contact_location, participant_age) %>% summarize(match.prop = mean(same.age.grp)
) 
contact_count_urban$same.age.grp_status  %>% filter(contact_location == "Home")%>% group_by(contact_location, participant_age) %>% summarize(match.prop = mean(same.age.grp)
) 




# Function characterizing network statistics of age effect for edge, nodefactor, and nodematch
## to-do: the script characterizing uncertenties needs to be replaced by ci_95 function
edge_node_factor_match <- function(contact_count_site){
  
  output <- list()
  
  # Edges 
  fit_edge <- 
    contact_count_site$contact_degree %>% 
    glm(n_contacts~ -1+contact_location, data=., family = poisson) %>% summary() # getting coefficient for two-day contact rate
  
  fit_edge <- fit_edge$coefficients%>% as.data.frame()  
  
  n_participants_site <- 
    contact_count_site$contact_degree$rec_id %>% unique() %>% length()  # number of participants of a network, all age groups
  
  fit_edge <- 
    fit_edge %>% 
    mutate(
      # single-day mean degree, original scale
      single_day_md = exp(Estimate)/2, # the 2 here is two covert the 2 to 1 day scale
      
      # calculation, log-link scale
      two_day_sd_link = `Std. Error`*sqrt(n_participants_site), # two-day standard deviation for the coefficient at the log scale
      single_day_sd_link = two_day_sd_link/2,  # single-day standard deviation for the coefficient at the log scale
      single_day_se_link = single_day_sd_link/sqrt(n_participants_site), # single-day standard error for the coefficient at the log scale
      single_day_md_link = log(single_day_md),
      
      # uncertainties, original scale
      single_day_lower95ci = exp(single_day_md_link -1.96*single_day_se_link),
      single_day_upper95ci =  exp(single_day_md_link +1.96*single_day_se_link)
    ) %>% select(single_day_md, single_day_lower95ci, single_day_upper95ci
    )
  
  
  
  output[[1]] <- fit_edge %>% rownames_to_column(var="contact_location") %>%
    mutate(contact_location=str_remove(contact_location, "contact_location")
    )
  
  
  # nodefactor - age.grp
  n_participants_nf_site <- # number of participants in each age.grp in a network
    contact_count_site$contact_degree %>% group_by( participant_age) %>% 
    summarize(n_participants_nf_site = n_distinct(rec_id)) %>% mutate(participant_age = as.character(participant_age))
  
  nf <- data.frame() 
  for (i in 1:4 # characterize mean degree and uncertainties for each layer
  ) {
    fit_nf_single <- 
      contact_count_site$contact_degree %>% filter(contact_location == levels(contact_count_site$contact_degree$contact_location)[i]) %>% 
      glm(n_contacts~ -1+participant_age, data=., family = poisson) %>% summary() # coefficients of each age group for two-day contact rate
    
    fit_nf_single <- fit_nf_single$coefficients %>% as.data.frame()
    
    fit_nf_single <-
      fit_nf_single%>% rownames_to_column() %>% 
      rename(participant_age=1) %>% mutate(participant_age= str_remove(participant_age, "participant_age") 
      ) %>% 
      left_join(
        n_participants_nf_site
      )
    
    fit_nf_single <- 
      fit_nf_single %>% 
      mutate(
        # single-day mean degree, original scale
        single_day_nf_md = exp(Estimate)/2, # the 2 here is two covert the 2 to 1 day scale
        
        # calculation, log-link scale
        two_day_nf_sd_link = `Std. Error`*sqrt(n_participants_nf_site), # two-day standard deviation for the coefficient at the log scale
        single_day_nf_sd_link = two_day_nf_sd_link/2,  # single-day standard deviation for the coefficient at the log scale
        single_day_nf_se_link = single_day_nf_sd_link/sqrt(n_participants_nf_site), # single-day standard error for the coefficient at the log scale
        single_day_nf_md_link = log(single_day_nf_md), # single-day mean degree, log scale
        
        # uncertainties, original scale
        single_day_nf_lower95ci = exp(single_day_nf_md_link -1.96*single_day_nf_se_link),
        single_day_nf_upper95ci =  exp(single_day_nf_md_link +1.96*single_day_nf_se_link)
      ) %>% select(participant_age, single_day_nf_md, single_day_nf_lower95ci, single_day_nf_upper95ci
      ) %>% 
      mutate(contact_location = levels(contact_count_site$contact_degree$contact_location)[i]) # specify layer
    
    
    
    
    
    nf <- 
      rbind(nf, fit_nf_single)
    
  }
  
  
  output[[2]] <- nf
  
  
  # nodematch - porportion of matched edges of each age group among all the edges
  
  nm <- data.frame() #matrix(NA, nrow = length(locations), ncol = length(levels(contact_count$participant_age)))
  for (i in 1:4 #length(locations)
  ) {
    
    
    data_nm <- 
      contact_count_site$same.age.grp_status %>% filter(contact_location == levels(contact_count_site$same.age.grp_status$contact_location)[i]) 
    
    n_obs_age.grp <- data_nm %>% group_by(participant_age) %>% summarize(n=n()) %>% mutate(participant_age = as.character(participant_age)
    ) # "n" is the total number of edges in each age agoup in a network
    
    
    fit_nm_single <- 
      data_nm %>% 
      glm(same.age.grp ~ -1+ participant_age,
          data = ., family = "binomial"
      )  %>% summary() # coefficients for matching proportion under each age group, this proportion is the same between the two-day and one-day scale
    
    fit_nm_single <- fit_nm_single$coefficients %>% as.data.frame()
    
    fit_nm_single <-
      fit_nm_single%>% rownames_to_column() %>% 
      rename(participant_age=1) %>% mutate(participant_age= str_remove(participant_age, "participant_age") 
      ) %>% 
      left_join(
        n_obs_age.grp
      )
    
    # 2024 uncertainty to-be-updated
    fit_nm_single <- 
      fit_nm_single %>% 
      mutate(
        # single-day mean degree, original scale
        single_day_nm_md = expit(Estimate), # the proportion at the single-day scale is the same as the two-day scale
        
        # calculation, logit-link scale
        two_day_nm_sd_link = `Std. Error`*sqrt(n), # two-day standard deviation for the coefficient at the logit scale. 
        single_day_nm_sd_link = two_day_nm_sd_link/2,  # single-day standard deviation for the coefficient at the logit scale
        single_day_nm_se_link = single_day_nm_sd_link/sqrt(n), # single-day standard error for the coefficient at the logit scale
        single_day_nm_md_link = logit(single_day_nm_md), # single-day mean degree, logit scale
        
        # uncertainties, original scale
        single_day_nm_lower95ci = expit(single_day_nm_md_link -1.96*single_day_nm_se_link),
        single_day_nm_upper95ci =  expit(single_day_nm_md_link +1.96*single_day_nm_se_link)
      ) %>% select(participant_age, single_day_nm_md, single_day_nm_lower95ci, single_day_nm_upper95ci
      ) %>% 
      mutate(contact_location = levels(contact_count_site$same.age.grp_status$contact_location)[i]) # specify layer
    
    nm <- 
      rbind(nm, fit_nm_single)
    
  }
  
  output[[3]] <- nm
  
  
  #### nodemix TB finished - porportion of edges of specific mixing among all the edges ###
  
  
  names(output) <- c("edge", "nf.age.grp", "nm.age.grp")
  
  output
}

edge_node_factor_match_rural <- edge_node_factor_match(contact_count_site = contact_count_rural)

edge_node_factor_match_urban <- edge_node_factor_match(contact_count_site = contact_count_urban)


# Correlation between layers
## Characterization of regression coefficients
### We tabulated the degrees between layers. We observed the following - in both the rural and urban networks, the contacts at school and work are relatively separated
# Function for bivariate Poisson regression for associations of degree between layers. 
# two-day regression coefficeint, single day predicted mean degs, two-day proportions of having contacts, and the wide-format data for the analysis are outputted 
assoc_btw_layers <- function(contact_count_long # contact count over the two days for each participant
                             ){
  
  ## Getting individual-level contact count at different locations by row through converting long format data to wide format 

  contact_count_wide  <- 
    contact_count_long %>% data.frame()%>% 
    tidyr::pivot_wider(names_from = "contact_location", values_from = "n_contacts"
    ) %>%  # 608 rows for rural and 624 rows for urban, each row for a participant in a layer
    as.data.frame()
  
  
  output_model <- output <-   list()
  
  # Create binarized degree category
  
  # binarized coefficient
  contact_count_wide <- 
    contact_count_wide %>% mutate(
      
      Home_cat = case_when(Home==0 ~ 0,
                           Home >0 ~ 1),
      Home_cat = factor( Home_cat),
      
      School_cat = case_when(School==0 ~ 0,
                             School >0 ~ 1),
      School_cat = factor( School_cat),
      
      Work_cat = case_when(Work==0 ~ 0,
                           Work >0 ~ 1),
      Work_cat = factor( Work_cat),
      
      Nonhome_cat = case_when(Nonhome==0 ~ 0,
                              Nonhome >0 ~ 1),
      Nonhome_cat = factor( Nonhome_cat)
    )
  
  ############### Characterizing two-day coefficients of the effect of other layers ###############
  # Effects of other layers on home 
  
  output_model$h_s <- 
    contact_count_wide %>% 
    glm(Home~ School_cat, data=., family = poisson(link=log))
  
  output_model$h_w <- 
    contact_count_wide %>% 
    glm(Home~ Work_cat, data=., family = poisson(link=log))
  
  output_model$h_nh <- 
    contact_count_wide %>% 
    glm(Home~ Nonhome_cat, data=., family = poisson(link=log)) 
  
  # Effects of other layers on School
  output_model$s_h <- 
    contact_count_wide %>% 
    glm(School~ Home_cat, data=., family = poisson(link=log))
  
  output_model$s_w <- 
    contact_count_wide %>% 
    glm(School~ Work_cat , data=., family = poisson(link=log))
  
  output_model$s_nh <- 
    contact_count_wide %>% 
    glm(School~ Nonhome_cat , data=., family = poisson(link=log))
  
  # Effects of other layers on work 
  output_model$w_h <- 
    contact_count_wide %>% 
    glm(Work~ Home_cat, data=., family = poisson(link=log))
  
  output_model$w_s <- 
    contact_count_wide %>% 
    glm(Work~ School_cat , data=., family = poisson(link=log))
  
  output_model$w_nh <- 
    contact_count_wide %>% 
    glm(Work~ Nonhome_cat , data=., family = poisson(link=log))
  
  
  # Effects of other layers on Nonhome
  output_model$nh_h <- 
    contact_count_wide %>% 
    glm(Nonhome~ Home_cat, data=., family = poisson(link=log))
  
  output_model$nh_s <- 
    contact_count_wide %>% 
    glm(Nonhome~ School_cat , data=., family = poisson(link=log))
  
  output_model$nh_w <- 
    contact_count_wide %>% 
    glm(Nonhome~ Work_cat , data=., family = poisson(link=log))
  

  ############### Summarizing the two-day coefficients on a table ###############
    tb_slope <- 
      rbind(
        summary(output_model$h_s)$coefficients, summary(output_model$h_w)$coefficients,  summary(output_model$h_nh)$coefficients, 
        summary(output_model$s_h)$coefficients,  summary(output_model$s_w)$coefficients, summary(output_model$s_nh)$coefficients,  
        summary(output_model$w_h)$coefficients  ,  summary(output_model$w_s)$coefficients  ,  summary(output_model$w_nh)$coefficients,
        summary(output_model$nh_h)$coefficients  ,  summary(output_model$nh_s)$coefficients  , summary(output_model$nh_w)$coefficients 
      )[ c(c(1:12)*2), ] # outputting the slope coefficients
    
    
    rownames(tb_slope) <- 
      paste0(
        rep(
          c("h_", "s_", "w_", "nh_"),
          each=3
        ), 
        rownames(tb_slope)
      )
    
    
  ############### Characterizing single-day predicted degree of the modeled layer by other layers ###############
    # function to characterize the network statistics based on coefficients of Poisson regression
    effect_oth_layers <- 
      function(
    layer_assoc
      ){
        
        out <- 
          c(# single-day mean degree of the outcome layer when the predictive layer didn't have contact
            exp(layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*0)/2, # the 2 here to covert the two-day MD to single-day MD
            # single-day mean degree of the outcome layer when the predictive layer have any contact
            exp(layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*1)/2 # the 2 here to covert the two-day MD to single-day MD
          )
        
        names(out) <- paste0( "other_layer", c("=0", "=1"))
        
        out
        
        
      }
    
    
    # Network statistics of the other layers at single-day scale 
    nf.deg.oth_layers <- 
      rbind(
       
        ### mean degrees of other layers on home ###
        effect_oth_layers(
          layer_assoc = output_model$h_s # school
        ),
        effect_oth_layers(
          layer_assoc = output_model$h_w # work 
        ),
        effect_oth_layers(
          layer_assoc = output_model$h_nh # nonhome
        ),
        
        ### mean degrees of other layers on school ###
        effect_oth_layers(
          layer_assoc = output_model$s_h # home
        ),
        effect_oth_layers(
          layer_assoc = output_model$s_w # work 
        ),
        effect_oth_layers(
          layer_assoc = output_model$s_nh # nonhome
        ),
 
        ### mean degrees of other layers on work ###
        effect_oth_layers(
          layer_assoc = output_model$w_h # home
        ),
        effect_oth_layers(
          layer_assoc = output_model$w_s # school 
        ),
        effect_oth_layers(
          layer_assoc = output_model$w_nh # nonhome
        ),
        
        ### mean degrees of other layers on nonhome ###
        effect_oth_layers(
          layer_assoc = output_model$nh_h # home
        ),
        effect_oth_layers(
          layer_assoc = output_model$nh_s # school 
        ),
        effect_oth_layers(
          layer_assoc = output_model$nh_w # work 
        )
        
      ) %>% data.frame()%>% 
      mutate(
        association = c(
        paste0("h", "_by_", c("s", "w", "nh")),
        paste0("s", "_by_", c("h", "w", "nh")),
        paste0("w", "_by_", c("h", "s", "nh")),
        paste0("nh", "_by_", c("h", "s", "w"))
      )
             
      )%>% 
  
      # Comparing the predicted mean degs with the unadjusted mean degs of the modeled layer at the single-day scale
      cbind(
        raw_md_modeled_layer = 
          rep(
            apply(contact_count_wide %>% select(Home, School, Work, Nonhome), MARGIN = 2, mean) %>% as.numeric()/2, # the 2 here to covert the two-day MD to single-day MD
            each=3
         
          )
      )
    
    
    ############### Characterizing the proportion of having and not having contact for each layer, and the raw mean degree ###############
    # check with Sam about whether the 2 should be used to divide the proportion for the single-day scale
    # Overall proportion, w/o stratifying for age
    deg.layer.dist_2days <- 
      rbind( # proportion
      prop.table(table(contact_count_wide$Home_cat)),
      prop.table(table(contact_count_wide$School_cat)),
      prop.table(table(contact_count_wide$Work_cat)),
      prop.table(table(contact_count_wide$Nonhome_cat))
      ) %>% data.frame() %>% 
      rename(prop_0=1, prop_1=2) %>% 
      mutate(layer = c("Home", "School", "Work", "Nonhome"))
    
    # age-stratified proportion, for generating nodal attribute
    deg.age.layer.dist_2days <- 
      rbind(
        # Home
        prop.table(
          table(contact_count_wide$participant_age, contact_count_wide$Home_cat) %>% t(), # transposing to age by column and contact status by row
          margin = 2 # calculating marginal proportion by column
        ),
        # School
        prop.table(
          table(contact_count_wide$participant_age, contact_count_wide$School_cat) %>% t(), # transposing to age by column and contact status by row
          margin = 2 # calculating marginal proportion by column
        ), 
        # Work
        prop.table(
          table(contact_count_wide$participant_age, contact_count_wide$Work_cat) %>% t(), # transposing to age by column and contact status by row
          margin = 2 # calculating marginal proportion by column
        ), 
        # Nonhome
        prop.table(
          table(contact_count_wide$participant_age, contact_count_wide$Nonhome_cat) %>% t(), # transposing to age by column and contact status by row
          margin = 2 # calculating marginal proportion by column
        ) 
      ) %>% as.data.frame() %>% 
      mutate(
        contact_status = rep(c(0,1), 4),
        layer = rep(c("Home", "School", "Work", "Nonhome"), each =2)
      ); row.names(deg.age.layer.dist_2days) <- NULL
    
    ############### Outputting ###############
    output$coefficient_summary_2days <- tb_slope; output$mean_deg_1day <-  nf.deg.oth_layers;
    output$deg.layer.dist_2days <- deg.layer.dist_2days; output$deg.age.layer.dist_2days <- deg.age.layer.dist_2days
    output$data_wide <- contact_count_wide
    
    output
    
}



 
layer_assoc_rural <- assoc_btw_layers(contact_count_long=contact_count_rural$contact_degree 
                                      )
layer_assoc_urban <- assoc_btw_layers(contact_count_long=contact_count_urban$contact_degree) 


## Visual evaluation of correlation between layers and assessing effect sizes
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
# For duration at the home layer, we assume it to be non-dissolving so it would be a large number. 
# For duration at the nonhome layer, we assume it refreshes daily. The duration of the school and work layer is characterized in the following way:
# 1) We tabulate time of knowing a contact and contact frequency to retrieve the proportions of daily contact under each category of the duration of knowing the contact. 
# 2) For a contact who is known less than ≤ 10 years, we took the mid point of each know contact category as the known duration. 
# 3) For a contact who is known > 10 years, we the population average of the mid point between participant's age and 10 year as the known duration.
# 4) We multiply the proportion of daily contact (as weight) and the known duration to calculate the adjusted known duration. 
# There are few contact whose participants' age is less than the reported know contact duration of > 10 yrs, rendering the characterized duration to be <0; we used the participant's age as the duration for these observations.
# Since the the following analysis is only applied to the school and work layer, there's no need to adjust for the replicated contacts as we consider them to be unique.

know_dur <- function(india_mix.){
  
#  Characterize the known duration for known_contact >10 yrs at the individual level
known_dur_gt_10yr <- india_mix. %>% 
  filter(known_contact == ">10 yrs" & contact_location %in% c( "School", "Work" )
                                  ) %>%
# for known_contact >10 yrs, the known duration is the middle point between 10 yrs and participants age.
    mutate(mid_point = (participant_ageyr - 10)/2 # half of the difference between the 10-yr bound and participants age.
           ) %>% 
mutate(
  known_duration = case_when(mid_point <= 0 ~ participant_ageyr,# for participants whose age is shorter than the known duration of 10 yrs, we consider the known duration the same as participant's age.
                             mid_point > 0 ~ mid_point+10) # for participants whose age is longer than the known duration, we calculate the known duration by summing the 10-yr bound and mid_point
) %>% 
  select(rec_id, study_site, contact_location, participant_age, participant_ageyr, contact_age, known_duration) # select variables which will be used below

# Considering contact age in characterizing contact duration
known_dur_gt_10yr <- 
known_dur_gt_10yr %>% mutate(contact_age_num = case_when(contact_age == "0-9y" ~ 4.5, # impute by median age
                                                           contact_age == "10-19y" ~ 14.5, # impute by median age
                                                           contact_age == "20-29y" ~ 24.5, # impute by median age
                                                           contact_age == "30-39y" ~ 34.5, # impute by median age
                                                           contact_age == "40-59y" ~ 49.5, # impute by median age
                                                           contact_age == "60+y" ~ 60, # given no characterized contact duration is > 60 yr, we impute the continuous age of this category as the lower bound.
                                                           )
                             ) %>% 
  mutate(diff= contact_age_num-known_duration) %>% # calculate the difference between contact's age and know duration
  mutate(known_duration_adj = case_when(diff>=0 ~ known_duration, # If contact's age is longer than or equal to the known duration, we treat the known duration as-is
                                    diff<0 ~contact_age_num) # If contact's age is shorter than the known duration, we use the contact age as the duration
  )

# Talk to Sam about the 4 contacts whose imputed duration are < 10 yrs

# Characterize avg. known duration of contact longer than 10 years in days by network and layer for sampled population 
known_dur_gt_10yr <- 
  known_dur_gt_10yr %>% 
  group_by(study_site, contact_location) %>% 
  summarize(known_contact_avg = mean(known_duration_adj)*365 # converting duration in years to duration in days
            ) %>% ungroup()

# Characterize known duration of contact in days for all categories of durations
known_duration <-
  india_mix. %>% 
  # characterize frequency of daily contact 
  filter(contact_location %in% c("School", "Work")) %>%  # school and work layers are those we want to characterize the duration
  droplevels() %>% 
  group_by(study_site, contact_location, frequency_contact, known_contact) %>%  
  tally() %>% # summarize frequency from tabulation of frequency_contact & known_contact, by network and layer
  spread(frequency_contact,n) %>% data.frame() %>% rename(daily = Daily..almost.daily) %>% # convert the data frame to wide format to retrieve the frequency of daily contact
  select(study_site, contact_location, known_contact, daily ) %>% 
  filter(!is.na(known_contact)) %>% # filter out the empty category, introduced by the tabulation - this category correspond to the number of rows where both frequency_contact & known_contact are missing
  ungroup() %>% 
  # impute known duration of contact - for the duration <= 10 years, we take the middle point of each categorical duration as the duration
  mutate(know_contact_d = # median duration in days
                     # for known duration less than 10 yrs, we convert the categorical value to their median in numeric days
           case_when(known_contact=="Never met before" ~0, 
                     known_contact=="<1 yr" ~182, # mid. point in days
                     known_contact=="1-2 yrs" ~ 548,# mid. point in days
                     known_contact=="3-5 yrs" ~ 1460,# mid. point in days
                     known_contact=="6-10 yrs" ~ 2922,# mid. point in days
                     # for known duration > 10 yrs, we use the age-imputed mean known duration of contact as the known duration
                     known_contact==">10 yrs" & study_site == "Rural" & contact_location == "School" ~ 
                       known_dur_gt_10yr %>% filter( study_site == "Rural" & contact_location == "School") %>% 
                       pull(known_contact_avg) ,
                     known_contact==">10 yrs" & study_site == "Rural" & contact_location == "Work" ~ 
                       known_dur_gt_10yr %>% filter( study_site == "Rural" & contact_location == "Work") %>% 
                       pull(known_contact_avg),
                     known_contact==">10 yrs" & study_site == "Urban" & contact_location == "School" ~ 
                       known_dur_gt_10yr %>% filter( study_site == "Urban" & contact_location == "School") %>% 
                       pull(known_contact_avg),
                     known_contact==">10 yrs" & study_site == "Urban" & contact_location == "Work" ~ 
                       known_dur_gt_10yr %>% filter( study_site == "Urban" & contact_location == "Work") %>% 
                       pull(known_contact_avg)
           )
  ) %>% 
  group_by(study_site, contact_location) %>% 
  
  mutate(
    daily_prop = daily/sum(daily, na.rm = T), # proportion of daily contact under each know_contact category
    weighted_known_contact_d = know_contact_d*daily_prop # duration of known contact weighted by daily contact proportion
  ) %>%   
  # weighted duration of contact
  summarize(know_contact_duration = sum(weighted_known_contact_d, na.rm = T)
  ) %>% ungroup() 

}
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



saveRDS(network_stats, file = "data/network_params/network_params.RData")
