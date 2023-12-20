
####### Characterization of network statistics for GlobalMix India Data #######

# # Method
# There are two ways to derive target statistics for ERGM, one is using poisson model based on individual-count data, 
# the other is based on the symmetric contact matrix, the approach for Corporate Mix. Given the symmetric contact matrix may not 
# account for the difference of distributions between the study and target populations, we go with the glm-based approach to 
# calculate the summary network statistics. We regrouped the age categories in the first two decades of life by decade. 
# We excluded contacts lasted ≤ 15 mins in analysis. The degree information was summarized as the arithmetic mean of signle-day degrees of 
# contact between group A and group B and between group B and group A of the two-day visits. Given the data were egocentric, 
# we use $M.D.=\frac{edges}{n}$ for the relationship between mean degree ($M.D.$) and the number of edges.
# Characterization of network layers: The network layers is processed from multiple questions asking contact locations. 
# Given these questions are not mutually exclusive due to the check-box design in REDCap, we characterized contact location as the 
# location having primary contact follow the order of home, school, work, non-home (excluding work and school). There are two contacts 
# that occurred at both work (location 3 in REDCap) and school (location 2), we categorized them as at school, considering the participants 
# (and contacts) were at school age (10-19y, 0-9y). This logic is not spelled out in the below script but is adjusted by the higher priority 
# of school than work. For a contact checked non of the categories, we assigned an NA to this contact and excluded them considering them as missing data.

setwd("~/Documents/GitHub/COVID-GlobalMix")

# Load libraries and data  ------------------------------------------------
lapply(c("ggpubr", "statnet", "EpiModel", "tidyverse", "socialmixr", "knitr",  "sjlabelled", "kableExtra", "broom", "stringr", "GGally"), require, character.only = TRUE)


## participant data
india_participant <- 
  readRDS("~/Documents/GitHub/COVID-GlobalMix/data/participant_contact/india_participant_data_aim1.RDS")



## contact data 
india_contact <- 
  readRDS("~/Documents/GitHub/COVID-GlobalMix/data/participant_contact/india_contact_data_aim1.RDS")


# Note - validated study_site of participant is exactly the same as those in contact

# Reclassifying participant's and contact's age groups into 0-9 years old, 10-19 years old, while treating other age groups as is.
## participant data
india_participant <- 
  india_participant %>% 
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

# merge participants and contact datasets and process data  and basic data processing ------------------------------------------------
india_mix <- india_participant %>%
  dplyr::select(rec_id,participant_age, participant_ageyr) %>%
  right_join(india_contact,
             by = "rec_id") %>% 
  # characterization of home/nonhome status
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
  ## Weighing in household membership in determining contact location - tabulating contact_location and hh_membership, we found there are 58,1,3 household member contacts in the nonhome, school, and work layers. We reclassify these as home contacts. We also found 2176 non-members at the home layer, and we reclassify them into non-home (other than school, work) contacts given the presumed turnover rate for this group is short. 
  mutate(
    contact_location = case_when(hh_membership == "Member" & contact_location %in% c("Nonhome", "School", "Work") ~ "Home",
                                 hh_membership == "Non-member" & contact_location %in% c("Home") ~ "Nonhome",
                                 T ~ contact_location
    )
    
  ) %>%
  mutate(contact_location = factor(contact_location,
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
  
  ## Distribution of indoor/outdoor status
  # We the break down of the observed categories of the variable "where_contact" indicating in- and out-door status. In the original data, there is a "Both" category indicates a participant contacted a contact both in- and out-doors. Since the "Both" category could have a higher transmission potential similar to the "Indoors" category, we merge the "Both" and the "Indoors" categories. We will adjust for this status as a nodefactor. For building the transmission model, we will assign lower risk for the "Outdoors" category during simulation.
  mutate(where_contact =case_when(where_contact %in% c("Indoors", "Both") ~ "Indoors_or_both",
                                  .default = where_contact)
  )



## Check implementation of the cross-checking between household membership and contact location
table(
  india_mix$contact_location,
  india_mix$hh_membership
)

# Function characterizing uncertainties of network statistics and scaling them from two to one day scale
ci_95 <- # to-do
  function(glm_ouput, num_sample, link){
    
    if(link = "log"){
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

# Function characterizing number of contact at the individual-level (outputted in "contact_count") and 
# the status of if a participant and contact belonged to a specific mixing pattern of age group (outputted in "age.grp_mix_status") or was in the same age group (outputted in "sameage.grp.count") for the contact data over the two-day period.
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
    ) %>% ungroup() %>% # total number of contacts over two days
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
    age.grp_mix_status %>% filter(fromdayone == "Both days" & contact_location == "Nonhome")%>% slice(rep(1:n(), each = 2) # for a contacts/edges that repeated over the two-day period at the non-home layer, we treat it as two edges (i.e., two rows).
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

# Evaluate if the proportion of nodemix can be used for nodematch
contact_count_rural$same.age.grp_status %>% filter(contact_location == "Home") %>% group_by( participant_age) %>% summarize(match.prop = mean(same.age.grp)
                                                                                                      ) %>% View()
contact_count_rural$age.grp_mix_status %>% filter(contact_location == "Home") %>% group_by( participant_age) %>% 
  summarize(match.prop1_1 = mean(age.grp1_1), 
            match.prop2_2 = mean(age.grp2_2),
            match.prop3_3 = mean(age.grp3_3),
            match.prop4_4 = mean(age.grp4_4),
            match.prop5_5 = mean(age.grp5_5),
            match.prop6_6 = mean(age.grp6_6),
            match.prop1_2 = mean(age.grp1_2),
            n=n() # number of contacts belongs to a egocentric group
            ) %>% View()




# Evaluate if the proportion of nodemix can be used for nodematch
contact_count_rural$same.age.grp_status %>% filter(contact_location == "Home") %>% group_by( participant_age) %>% summarize(match.prop = mean(same.age.grp)
) %>% View()


# function calculate proportions of mixing between age groups of participant and contact
mix_prop <-
  function(mix_status_layer,
           unobserve_ego_age_grp # could be "none", "40+y",  "60+y"
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
    
    glm_nmix <- 
      lapply( all_mix_patterns, function(x) { 
        
        glm(substitute(i ~  -1+participant_age,  
                       list(i = as.name(x))), family = "binomial", data = mix_status_layer) 
        
      } )
    
    glm_nmix_summary <- lapply(glm_nmix, summary)
    

  
    # fill the two-day proportion to the matrix 
    if(unobserve_ego_age_grp == "none"){
      for (i in 1:6
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    } else if (unobserve_ego_age_grp == "60+y") {
      
      for (i in 1:5
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    } else { # for the scenario of unobserve_ego_age_grp == "40+y"
      
      for (i in 1:4
      ) {
        
        mix_prop_matrix_2d_glm[i, 1] <- glm_nmix_summary[[1+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 2] <- glm_nmix_summary[[2+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 3] <- glm_nmix_summary[[3+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 4] <- glm_nmix_summary[[4+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 5] <- glm_nmix_summary[[5+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        mix_prop_matrix_2d_glm[i, 6] <- glm_nmix_summary[[6+6*(i-1)]]$coefficients[i,"Estimate"] %>% expit()
        
      }
      
    }
    

     
   
    ############## crude calculation #########
    # fill the two-day proportion to the matrix 
    for (i in 1:6
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

locs <- c("Home", "School", "Work", "Nonhome")
unobs_grp_rural <- c("none", "60+y", "none", "none")
unobs_grp_urban <- c("none", "40+y", "none", "none")
mix_prop_rural_layers <- mix_prop_urban_layers <- list()

# Characterizing mixing proportion for the rural network
for (i in 1:length(locs)
     ) {
  mix_prop_rural_layers[[i]] <- 
    mix_prop(mix_status_layer= contact_count_rural$age.grp_mix_status %>% filter( contact_location == locs[i]),
             unobserve_ego_age_grp  = unobs_grp_rural[i]
             )
}

# Characterizing mixing proportion for the urban network 
for (i in 1:length(locs)
) {
  mix_prop_urban_layers[[i]] <- 
    mix_prop(mix_status_layer= contact_count_urban$age.grp_mix_status %>% filter( contact_location == locs[i]),
             unobserve_ego_age_grp  = unobs_grp_urban[i]
    )
}


# for demonstration, discuss w/ Sam
# intercept-only model, w/ edge from group i as data
test <- glm(age.grp1_2 ~ 1, family = "binomial", 
            data = contact_count_rural$age.grp_mix_status %>% filter( contact_location == "Home") %>% filter(participant_age =="0-9y") 
            ) # replicate warning in method 1
expit(test$coefficients) # the point estimate w/ this formulation is good
summary(test)

# coefficient-only model, w/ edge from all groups as data, this is a simpler approach.
test_1 <- glm(age.grp1_2 ~ -1+participant_age, family = "binomial", 
              data = contact_count_rural$age.grp_mix_status %>% filter( contact_location == "Home") ) # replicate warning in method 1
expit(test_1$coefficients) # the point estimate w/ this formulation is good
summary(test_1)


# above are testing script to be removed



# Function calculating 1. unweighted mean of the mixing proportion of the two direction or 2. the proportion of each direction for the whole mixing matrix
avg_mix_prop_all_layers <- # discuss with Sam
  function(mix_degree_site, layers){ # numeric leveling of age group a
  
  # defining empty dataframes to store result 
  ## symmetric mixing matrix
  mix_matrix_sym <-
    matrix(NA, nrow = 6, ncol = 6) %>% 
    as.data.frame(., 
                  row.names = c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")
    ) %>% rename("0-9y"=1, "10-19y"=2, "20-29y"=3, "30-39y"=4, "40-59y"=5, "60+y"=6)
  
  ## asymmetric mixing matrix
  mix_matrix_asym <-
    matrix(NA, nrow = 6, ncol = 6) %>% 
    as.data.frame(., 
                  row.names = paste0("ego.", c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")
                  ),
    ); colnames(mix_matrix_asym ) <- paste0("contact.", c("0-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))
  
  ## list to save the above matrices for each layer
  mix_matrices_sym <- mix_matrices_asym <-  list() 
  
  
  for (i in 1:length(layers) # apply calculation for each of the four layers
  ) {
    loc <- layers[i]
    
    for (a.num in 1:5) {  # apply calculation for each egocentric age group
      
      for (j in (a.num+1):6
      ) { # under each egocentric age group, calculate proportion of mixing between the age group of this egocentric node and other age groups
        
        directional_prop <- 
          mix_prop(mix_degree = mix_degree_site, age.grp.a = a.num, age.grp.b = j, contact_loc = loc) 
        
        # storing result in a symmetric mixing matrix where the value of each cell is the mean proportion of two direction of contact
        mix_matrix_sym[a.num, j] <-  directional_prop %>% as.numeric() %>% mean(., na.rm = T) # mean proportion of mixings of the two directions
        
        # storing result in an asymmetric mixing matrix where the value of each cell is the proportion of contact in a single direction
        mix_matrix_asym[a.num, j] <-  directional_prop[1] %>% as.numeric() # mixing between group a and b
        mix_matrix_asym[j, a.num] <-  directional_prop[2] %>% as.numeric() # mixing between group b and a
        mix_matrix_asym
        
      }
    }
    
    mix_matrices_asym[[i]] <-  mix_matrix_asym
    
    mix_matrices_sym[[i]] <- mix_matrix_sym
    
  }
  
  names(mix_matrices_asym) <- names(mix_matrices_sym) <- layers # assign layers' name to the matrices
  
  
  output <- list(mix_matrices_asym, mix_matrices_sym); names(output) <- c("asymmetric_matrices", "symmetric_matrices")
  
  output
  
  
}

rural_mix_prop <- avg_mix_prop_all_layers(mix_degree_site = contact_count_rural$age.grp_mix_degree, 
                                          layers = c("Home", "School", "Work", "Nonhome")
)

rural_mix_prop

urban_mix_prop <- avg_mix_prop_all_layers(mix_degree_site = contact_count_urban$age.grp_mix_degree, 
                                          layers = c("Home", "School", "Work", "Nonhome")
                                          )

urban_mix_prop




# Function characterizing network statistics of age effect for edge, nodefactor, and nodematch
edge_node_factor_match <- function(contact_count_site){
  
  output <- list()
  
  # Edges 
  fit_edge <- 
    contact_count_site$contact_degree %>% 
    glm(n_contacts~ -1+contact_location, data=., family = poisson) %>% summary() # coefficient for two-day contact rate
  
  fit_edge <- fit_edge$coefficients%>% as.data.frame()  # the two is averaging two-day's contact rate to one-day
  
  n_participants_site <- 
    contact_count_site$contact_degree$rec_id %>% unique() %>% length()  # number of participants of a network
  
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
      glm(n_contacts~ -1+participant_age, data=., family = poisson) %>% summary() # coefficients for two-day contact rate
    
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
        single_day_nf_md = exp(Estimate)/2,
        
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
      )  %>% summary() # coefficients for two-day contact rate
    
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
        single_day_nm_md = expit(Estimate)/2, # the reason 2 is here is because we convert 2 day to one day statistics
        
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

network_stats_rural <- edge_node_factor_match(contact_count_site = contact_count_rural)


network_stats_urban <- edge_node_factor_match(contact_count_site = contact_count_urban)

network_stats_rural$nodemix <- rural_mix_prop
network_stats_urban$nodemix <- urban_mix_prop

network_stats <- 
  list(network_stats_rural, network_stats_urban)
names(network_stats) <- c("rural", "urban")



# Correlation between layers
## Characterization of regression coefficients
### We tabulated the degrees between layers. In both the rural and urban networks, the contacts at school and work are relatively separated

 

# Function for bivariate Poisson regression for associations of degree between layers, glm result, coefficients, and the data w/ wide format for the regression are outputted 
assoc_btw_layers <- function(contact_count_long){
  
  ## Getting individual-level contact count at different locations by row through converting long format data to wide format 
  ### rural 
  contact_count_wide  <- 
    contact_count_long %>% data.frame()%>% 
    tidyr::pivot_wider(names_from = "contact_location", values_from = "n_contacts"
    ) %>%  # 608 rows for rural and 624 rows for urban, each row for a participant in a layer
    as.data.frame()
  
  
  output_model <- output <-   list()
  
  # Create degree category - binarized, or ordinal
  
  # binarized coefficient
  contact_count_wide <- 
    contact_count_wide %>% mutate(
      Nonhome_cat = case_when(Nonhome==0 ~ 0,
                              Nonhome >0 ~ 1),
      Nonhome_cat = factor( Nonhome_cat),
      
      Home_cat = case_when(Home==0 ~ 0,
                           Home >0 ~ 1),
      Home_cat = factor( Home_cat),
      
      School_cat = case_when(School==0 ~ 0,
                             School >0 ~ 1),
      School_cat = factor( School_cat),
      
      Work_cat = case_when(Work==0 ~ 0,
                           Work >0 ~ 1),
      Work_cat = factor( Work_cat)
    )
  
  
  # Effects of other layers on home 
  output_model$h_w <- 
    contact_count_wide %>% 
    glm(Home~ Work_cat, data=., family = poisson(link=log))
  
  output_model$h_nh <- 
    contact_count_wide %>% 
    glm(Home~ Nonhome_cat, data=., family = poisson(link=log)) 
  
  output_model$h_s <- 
    contact_count_wide %>% 
    glm(Home~ School_cat, data=., family = poisson(link=log))
  
  
  # Effects of other layers on School
  output_model$s_h <- 
    contact_count_wide %>% 
    glm(School~ Home_cat, data=., family = poisson(link=log))
  
  output_model$s_nh <- 
    contact_count_wide %>% 
    glm(School~ Nonhome_cat , data=., family = poisson(link=log))
  
  output_model$s_w <- 
    contact_count_wide %>% 
    glm(School~ Work_cat , data=., family = poisson(link=log))
  
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
  
  output_model$nh_school <- 
    contact_count_wide %>% 
    glm(Nonhome~ School_cat , data=., family = poisson(link=log))
  
  output_model$nh_w <- 
    contact_count_wide %>% 
    glm(Nonhome~ Work_cat , data=., family = poisson(link=log))
  
  
  
  
#  if (output_type == "summary"){ # outputting coefficients
    
    tb <- 
      rbind(
        summary(output_model$h_w)$coefficients,  summary(output_model$h_nh)$coefficients, summary(output_model$h_s)$coefficients,
        summary(output_model$s_h)$coefficients,  summary(output_model$s_nh)$coefficients  ,  summary(output_model$s_w)$coefficients, 
        summary(output_model$w_h)$coefficients  ,  summary(output_model$w_s)$coefficients  ,  summary(output_model$w_nh)$coefficients,
        summary(output_model$nh_h)$coefficients  ,  summary(output_model$nh_school)$coefficients  , summary(output_model$nh_w)$coefficients 
      )[ c(c(1:12)*2), ] # outputting the slope coefficients
    
    
    rownames(tb) <- 
      paste0(
        rep(
          c("h_", "s_", "w_", "nh_"),
          each=3
        ), 
        rownames(tb)
      )
    
    #tb
    
 # } else if (output_type == "models") { # outputting models
    
    #output_model
    
  #} else if (output_type == "data") { # outputting processed data with binarized predictor fitted by regression
    #contact_count_wide
  #}
    
    output$coefficient_summary <- tb; output$model <- output_model; output$data_wide <- contact_count_wide
    
    output
    
} 

layer_assoc_rural <- assoc_btw_layers(contact_count_long=contact_count_rural$contact_degree)
layer_assoc_urban <- assoc_btw_layers(contact_count_long=contact_count_urban$contact_degree)

## Visual evaluation of correlation between layers and assessing effect sizes
### spearman correlation,rural
layer_assoc_rural $data_wide %>% select(-c(rec_id, participant_age, Nonhome_cat, Home_cat, School_cat, Work_cat)) %>% 
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

# frequencies of contacts, will be used to scale up the target population of the effect from anther layer
gt_0_freq_r <- # the number of study population having any contact at each layer
rbind(
  (contact_count_wide_rural$Home !=0) %>% sum(),
  (contact_count_wide_rural$School !=0) %>% sum(),
  (contact_count_wide_rural$Work !=0) %>% sum(),
  (contact_count_wide_rural$Nonhome !=0) %>% sum()
  )

gt_0_freq_u <- 
  rbind(
    (contact_count_wide_urban$Home !=0) %>% sum(),
    (contact_count_wide_urban$School !=0) %>% sum(),
    (contact_count_wide_urban$Work !=0) %>% sum(),
    (contact_count_wide_urban$Nonhome !=0) %>% sum())


data.frame(
## rural
gt_0_freq_r,

## urban
gt_0_freq_u
) %>%  mutate(layer = c("H", "S", "W", "NH"))


# function to characterize the network statistics based on coefficients of Poisson regression
effect_oth_layers <- 
  function(
    layer_assoc
  ){
    
    out <- 
      c(
        exp(layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*0)/2, # we add 2 here to covert the two-day MD to single-day MD
        
        exp(layer_assoc$coefficients[1]+layer_assoc$coefficients[2]*1)/2 # we add 2 here to covert the two-day MD to single-day MD
      )
    
    names(out) <- paste0( "predict_layer", c("=0", "=1"))
    
    out
    
    
  }

layer_assoc_rural$model

## Network statistics on one layer has on the other layer of the selected associations
netstat_oth_layers <- # at single-day scale 
rbind(
  ## rural network
  ### effect of the work layer on home layer
  effect_oth_layers(
    layer_assoc = layer_assoc_rural$model$h_w
  ),
  
  ### effect of the nonhome layer on school layer
  effect_oth_layers(
    layer_assoc = layer_assoc_rural$model$s_nh
  ),
  
  ### effect of the school layer on work layer
  effect_oth_layers(
    layer_assoc = layer_assoc_rural$model$w_s
  ),
  
  ### effect of the work layer on nonhome layer
  effect_oth_layers(
    layer_assoc = layer_assoc_rural$model$nh_w
  ),
  
  ## urban network
  ### effect of the work layer on home layer
  effect_oth_layers(
    layer_assoc = layer_assoc_urban$model$h_w
  ),
  
  ### effect of the nonhome layer on school layer
  effect_oth_layers(
    layer_assoc = layer_assoc_urban$model$s_nh
  ),
  
  ### effect of the school layer on work layer
  effect_oth_layers(
    layer_assoc = layer_assoc_urban$model$w_s
  ),
  
  ### effect of the work layer on nonhome layer
  effect_oth_layers(
    layer_assoc = layer_assoc_urban$model$nh_w
  )
) %>% data.frame()%>% 
  mutate(association = c("h_w",
                         "s_nh", 
                         "w_s",
                         "nh_w", 
                         "h_w",
                         "s_nh", 
                         "w_s",
                         "nh_w"),
         network = 
           rep(
           c("rural", "urban"),
           each = 4
         )
         
  )


# Comparing the predicted statistics and the unadjusted network statistics
netstat_oth_layers %>% cbind(
  md_main_layer = 
  c(
apply(layer_assoc_rural$data_wide %>% select(Home, School, Work, Nonhome), MARGIN = 2, mean) %>% as.numeric()/2,
apply(layer_assoc_urban$data_wide %>% select(Home, School, Work, Nonhome), MARGIN = 2, mean) %>% as.numeric()/2
)
)



### Storing the statistics to the list for output
network_stats$rural$assoc_layers <- netstat_oth_layers %>% filter(network == "rural")
network_stats$urban$assoc_layers <- netstat_oth_layers %>% filter(network == "urban")



#saveRDS(network_stats, file = "~/Documents/GitHub/COVID-GlobalMix/data/network_params/formation_stats.RData")



# ## Contact durations
# For duration at the home layer, we assume it to be non-dissolving so it would be a large number. For duration at the nonhome layer, we assume it refreshes daily. The duration of the school and work layer is characterized in the following way. We tabulate time of knowing a contact and contact frequency to retrieve the distribution of daily contact under categories of the duration of knowing the contact. For a contact is known less than ≤ 10 years, we took the mid point of each know contact category as the duration. For a contact is know > 10 years, we the population average of the mid point between participant's age and 10 year as the duration. We multiple the proportion of daily contact (as weight) and the duration to calculate the known duration. There are few contact whose participants' age is less than the reported know contact duration of > 10 yrs, randering the characterized duration to be <0; we used the participant's age as the duration for these observations.
# Since the the following analysis is only applied to the school and work layer, there's no need to adjust for the replicated contacts as we consider them to be unique.

know_dur <- function(india_mix.){
  
  #  Characterize the known duration for known_contact >10 yrs
known_dur_gt_10yr <- india_mix. %>% 
  filter(known_contact == ">10 yrs" & contact_location %in% c( "School", "Work" )
                                  ) %>%
  # for known_contact >10 yrs, the known duration is the middle point between 10 yrs and participants age.
    mutate(mid_point = (participant_ageyr - 10)/2 # the length of middle point between 10 yrs and participants age.
           ) %>% 
mutate(
  known_duration = case_when(mid_point <= 0 ~ participant_ageyr,# for participants whose age is shorter than the known duration, we consider the known duration the same as participant's age.
                             mid_point > 0 ~ mid_point+10) # for participants whose age is longer than the known duration, we calculate the known duration based on the rule.
) %>% 
  select(rec_id, study_site, contact_location, participant_ageyr, known_duration) # select variables which will be used below

# Avg. known duration of contact in days by network and layer of sampled population older than 10 years
known_dur_gt_10yr <- 
  known_dur_gt_10yr %>% 
  group_by(study_site, contact_location) %>% 
  summarize(known_contact_avg = mean(known_duration)*365) %>% ungroup()

known_duration <-
  india_mix. %>% 
  # characterize  frequency of daily contact 
  filter(contact_location %in% c("School", "Work")) %>%  # school and work layers are those we want to characterize the duration
  group_by(study_site, contact_location, frequency_contact, known_contact) %>%  
  tally() %>% # summarize frequency from tabulation of frequency_contact & known_contact, by network and layer
  spread(frequency_contact,n) %>% data.frame() %>% rename(daily = Daily..almost.daily) %>% # convert the data frame to wide format to retrieve the frequency of daily contact
  select(study_site, contact_location, known_contact, daily ) %>% 
  filter(!is.na(known_contact)) %>% # filter out the empty category, introduced by the tabulation - this category correspond to the number of rows where both frequency_contact & known_contact are missing
  ungroup() %>% 
  # impute known duration of contact
  mutate(know_contact_d = # median duration in days
                     # for known duration less than 10 yrs, we convert the categorical value to their median in numeric days
           case_when(known_contact=="Never met before" ~0, 
                     known_contact=="<1 yr" ~182, # mid. point
                     known_contact=="1-2 yrs" ~ 548,# mid. point
                     known_contact=="3-5 yrs" ~ 1460,# mid. point
                     known_contact=="6-10 yrs" ~ 2922,# mid. point
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
                       pull(known_contact_avg),
           )
  ) %>% 
  group_by(study_site, contact_location) %>% 
  
  mutate(
    daily_prop = daily/sum(daily, na.rm = T), # proportion of daily contact under each know_contact cateogry
    weighted_known_contact_d = know_contact_d*daily_prop # duration of known contact weighted by daily contact proportion
  ) %>%   
  # weighted duration of contact
  summarize(know_contact_duration = sum(weighted_known_contact_d, na.rm = T)
  ) %>% ungroup()

}
known_duration <- know_dur(india_mix. = india_mix)

#saveRDS(known_duration, file = "~/Documents/GitHub/COVID-GlobalMix/data/network_params/known_dur_school_work.RData")







