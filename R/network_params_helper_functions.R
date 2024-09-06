participant_contact_merge <- function(india_participant, india_contact){
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
  
  
  
  india_participant <<- india_participant # assign the participant dataset with 6-category age groups to the global environment
  
  
  india_contact <<- india_contact
  return(india_mix) # output the process data
  
}


contact_freq_site <- function(india_mix., india_participant., india_contact., study_site.){
  india_mix.site <- 
    india_mix.  %>% 
    filter(study_site == study_site.) # here we split the dataset into urban or rural samples
  
  
  id_no_contact_ls15min.site <-
    # participants aren't in the merged india_mix data - 
    # In Rural, this includes 4 participants with no contacts and 16 participants whose all contacts ≤ 15 mins and w/o contacts > 15 mins 
    # In Urban, this includes 2 participants with no contacts and 0 participants whose all contacts ≤ 15 mins and w/o contacts > 15 mins 
    india_participant. %>%
    filter(!(rec_id %in% india_mix.$rec_id)) %>%
    filter(study_site == study_site.)  %>% select(rec_id)
  
  
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
            rec_id = rep(as.character(id_no_contact_ls15min.site$rec_id), each = length(locations)),
            contact_location= rep(locations, 
                                  nrow(id_no_contact_ls15min.site)
            ),
            n_contacts=rep(0,  nrow(id_no_contact_ls15min.site)*length(locations))
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


mix_prop <- 
  function(mix_status_layer,# the mixing statuses of a single layer over the two-day period 
           unobserve_ego_age_grp 
           # The "unobserve_ego_age_grp" argument indicates which egocentric age groups weren't observed to have contact or whose contacts where excluded.
           # For these unobserved egocentric age groups, their corresponding proportions won't be characterized, yielding NA
  ){
    expit <- function(x){1/(1+exp(-x))}
    
    
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
        
        glm(substitute(i ~  -1+participant_age, # slope-only model, where the slope is logit(proportion) 
                       list(i = as.name(x))), family = "binomial", data = mix_status_layer) 
        
      } )
    # Note: the warning of "glm.fit: algorithm did not converge" occurs when all the mixing status == 0
    
    glm_nmix_summary <- lapply(glm_nmix, summary)
    
    
    
    # Filling the two-day proportion to the mixing matrix, the row index (i) and column index correspond to the egocentric and contact's age groups
    if(unobserve_ego_age_grp == "none"){ # if contacts existed in all the 6 egocentric age groups, we fill all the six corresponding matrix rows. This scenario applies to the home and nonhome layers
      for (i in 1:6 
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit() # convert the regression coefficient to proportion
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    }  else if (unobserve_ego_age_grp == "40+y") { # if the last two oldest egocentric age groups didn't have contact, we fill the first four corresponding matrix rows. This scenario applies to the rural and urban school layers
      
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


edge_node_factor_match <- function(contact_count_site){
  
  logit <- function(x){log(x/(1-x))}
  expit <- function(x){1/(1+exp(-x))}
  
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
  
  
  # nodematch - proportion of matched edges of each age group among all the edges
  
  nm <- data.frame()
  for (i in 1:4 
  ) {
    
    
    data_nm <- 
      contact_count_site$same.age.grp_status %>% filter(contact_location == levels(contact_count_site$same.age.grp_status$contact_location)[i]) 
    
    n_obs_age.grp <- data_nm %>% group_by(participant_age) %>% summarize(n=n()) %>% mutate(participant_age = as.character(participant_age)
    ) # "n" is the total number of edges in each age group in a network
    
    
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
  
  names(output) <- c("edge", "nf.age.grp", "nm.age.grp")
  
  output
}


assoc_btw_layers <- function(contact_count_long # contact count over the two days for each participant
){
  
  # Note: things that are outputted from the function - Two-day regression coefficients of the associations (coefficient_summary_2days), single-day predicted mean degrees conditioning on the other layer (mean_deg_1day),
  # two-day proportions of having contacts (deg.layer.dist_2days), and the raw data in wide-format (data_wide) for the regressions 
  
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


know_dur <- function(india_mix.){
  
  #  Introduction:  
  # For duration at the home layer, we assume it to be non-dissolving so it would be a large number. 
  # For duration at the nonhome layer, we assume it refreshes daily. 
  # The duration of the school, work, and nonhome layer is characterized in the following way:
  # 1) We tabulate time of knowing a contact and contact frequency to retrieve the proportions of daily contact under each category of the duration of knowing the contact. 
  # 2) For a contact who is known less than ≤ 10 years, we took the mid point of each know contact category as the known duration. 
  # 3) For a contact who is known > 10 years, we characterize the known duration using "known_contact", participant's age, and contact's age. Details for the characterization of this category is documented in the "analysis_decision" file. 
  # 4) We multiply the proportion of daily contact (as weight) and the known duration to calculate the adjusted known duration. Then we sum over the margin of the categories as the duration.
  # There are few contact whose participants' age is less than the reported know contact duration of > 10 yrs, rendering the characterized duration to be <0; we used the participant's age as the duration for these observations.
  # Since the the following analysis is only applied to the school and work layer, there's no need to adjust for the replicated contacts as we consider them to be unique.  
  
  #  Characterize the known duration for known_contact >10 yrs at the individual level
  known_dur_gt_10yr <- india_mix. %>% 
    filter(known_contact == ">10 yrs" & contact_location %in% c( "School", "Work", "Nonhome" )
    ) %>%
    # for known_contact >10 yrs, the known duration is the middle point between 10 yrs and participants age.
    mutate(mid_point = (participant_ageyr - 10)/2 # half of the difference between the 10-yr bound and participants age.
    ) %>% 
    mutate(
      known_duration = case_when(mid_point <= 0 ~ participant_ageyr,# for participants whose age is shorter than the known duration of 10 yrs, we consider the known duration the same as participant's age.
                                 mid_point > 0 ~ mid_point+10) # for participants whose age is longer than the known duration, we calculate the known duration by summing the 10-yr lower bound and "mid_point"
    ) %>% 
    select(rec_id, study_site, contact_location, participant_age, participant_ageyr, contact_age, known_duration) # select variables which will be used below
  
  # Considering contact age in characterizing contact duration
  known_dur_gt_10yr <- 
    # Inpute a continuous contact age by the median of age category
    known_dur_gt_10yr %>% mutate(contact_age_num = case_when(contact_age == "0-9y" ~ 4.5, # impute by median age
                                                             contact_age == "10-19y" ~ 14.5, # impute by median age
                                                             contact_age == "20-29y" ~ 24.5, # impute by median age
                                                             contact_age == "30-39y" ~ 34.5, # impute by median age
                                                             contact_age == "40-59y" ~ 49.5, # impute by median age
                                                             contact_age == "60+y" ~ 60, # given no characterized contact duration is > 60 yr, we impute the continuous age of this category as the lower bound.
    )
    ) %>% 
    mutate(diff= contact_age_num-known_duration) %>% # calculate the difference between contact's age and know duration
    mutate(known_duration_adj = case_when(diff>=0 ~ known_duration, # If contact's age is larger than or equal to the known duration, we treat the known duration as-is
                                          diff<0 ~contact_age_num) # If contact's age is shorter than the known duration, we use the contact age as the duration
    ) %>% 
    # After the above characterization, we observe there are still 5 contacts of 458-1, 660-1, 664-1, and 666-1 whose known_duration_adj is < 10. 
    # Since a reported contact would be valid only if its reported contact duration is larger than both the ages of its participant and contact. We consider them as invalid and exclude them from analysis
    filter(known_duration_adj > 10)
  
  
  # Calculate avg. known duration of contact longer than 10 years (in days) by network and layer for sampled population 
  known_dur_gt_10yr <- 
    known_dur_gt_10yr %>% 
    group_by(study_site, contact_location) %>% 
    summarize(known_contact_avg = mean(known_duration_adj)*365 # converting duration in years to duration in days
    ) %>% ungroup()
  
  # Characterize known duration of contact (in days) for all categories of durations
  known_duration <-
    india_mix. %>% 
    # characterize frequency of daily contact 
    filter(contact_location %in% c("School", "Work", "Nonhome")) %>%  # school and work layers are those we want to characterize the duration
    droplevels() %>% 
    group_by(study_site, contact_location, frequency_contact, known_contact) %>%  
    tally() %>% # summarize frequency from tabulation of frequency_contact & known_contact, by network and layer
    spread(frequency_contact,n) %>% data.frame() %>% rename(daily = Daily..almost.daily) %>% # convert the data frame to wide format to retrieve the frequency of daily contact
    select(study_site, contact_location, known_contact, daily ) %>% 
    filter(!is.na(known_contact)) %>% # filter out the empty category, introduced by the tabulation - this category correspond to the number of rows where both frequency_contact & known_contact are missing
    mutate(daily = 
             # after the above steps, we observe NA when known_contact == "Never met before", this category was caused by there wasn't contact with known_contact == "Never met before" & frequency_contact == "Daily/ almost daily"
             # in the corresponding network layer, we recode the NA to 0. The na.rm = T used before is obviated after this recoding
             case_when(is.na(daily) & known_contact == "Never met before" ~ 0,
                       .default = daily
             )
    ) %>% 
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
                       known_contact==">10 yrs" & study_site == "Rural" & contact_location == "Nonhome" ~ 
                         known_dur_gt_10yr %>% filter( study_site == "Rural" & contact_location == "Nonhome") %>% 
                         pull(known_contact_avg),
                       known_contact==">10 yrs" & study_site == "Urban" & contact_location == "School" ~ 
                         known_dur_gt_10yr %>% filter( study_site == "Urban" & contact_location == "School") %>% 
                         pull(known_contact_avg),
                       known_contact==">10 yrs" & study_site == "Urban" & contact_location == "Work" ~ 
                         known_dur_gt_10yr %>% filter( study_site == "Urban" & contact_location == "Work") %>% 
                         pull(known_contact_avg),
                       known_contact==">10 yrs" & study_site == "Urban" & contact_location == "Nonhome" ~ 
                         known_dur_gt_10yr %>% filter( study_site == "Urban" & contact_location == "Nonhome") %>% 
                         pull(known_contact_avg)
             )
    ) %>% 
    group_by(study_site, contact_location) %>% 
    
    mutate(
      daily_prop = daily/sum(daily), # proportion of daily contact under each know_contact category
      weighted_known_contact_d = know_contact_d*daily_prop # duration of known contact weighted by daily contact proportion
    ) %>%   
    # weighted duration of contact
    summarize(know_contact_duration = sum(weighted_known_contact_d)
    ) %>% ungroup() 
  
  # Combining characterized duration of the School, Work, and Nonhome layers with 1e6 of the Home layers
  known_duration %>% 
    rbind(.,
          data.frame(
            study_site = c("Rural", "Urban"),
            contact_location = "Home",
            know_contact_duration = 1e12) # duration at the magnitude of a trillion
    ) %>% 
    mutate(contact_location = factor(contact_location,   levels = c("Home", "School", "Work", "Nonhome"))) %>% 
    arrange(study_site, contact_location)
  
}