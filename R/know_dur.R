know_dur <- function(india_mix.){
  
  ###########  Method description ###########
  # For duration at the home layer, we characterize it as the average age of participants/contacts (whichever is smaller) in each network. 
  # Before using the following steps 1-4, we first duplicate each contact (edge) whose "fromdayone" == "Both days" into two contacts (edges) for the nonhome layer, following what was done for the formation statistics. 
  # The durations of the school, work, and nonhome layers are characterized in the following way:
  # 1) We tabulate time of knowing a contact and contact frequency to retrieve the proportions of daily contact under each category of the duration of knowing the contact. 
  # 2) For a contact who is known less than ≤ 10 years, we took the mid point of each know contact category as the known duration. 
  # 3) For a contact who is known > 10 years, we characterize the known duration using "known_contact", participant's age, and contact's age. Details for the characterization of this category is documented in the "analysis_decision" file. 
  # 4) We multiply the proportion of daily contact (as weight) and the known duration to calculate the adjusted known duration. Then we sum over the margin of the categories as the duration.

  ###########  Pre-processing ###########
  # Inpute a continuous contact age by the median of age category
  india_mix. <- 
    india_mix. %>% 
    # select necessary variables
    select(rec_id, study_site, fromdayone, contact_location, participant_age, participant_ageyr,  contact_age, frequency_contact, known_contact) %>% 
   
    mutate(contact_age_num = case_when(contact_age == "0-9y" ~ 4.5, # impute by median age
                                                 contact_age == "10-19y" ~ 14.5, # impute by median age
                                                 contact_age == "20-29y" ~ 24.5, # impute by median age
                                                 contact_age == "30-39y" ~ 34.5, # impute by median age
                                                 contact_age == "40-59y" ~ 49.5, # impute by median age
                                                 contact_age == "60+y" ~ 60 # given no characterized contact duration is > 60 yr, we impute the continuous age of this category as the lower bound.
                                       )
           ) 
  
  
  
  # Replicate the nonhome edges whose "fromdayone" == "Both days"
  india_mix. <- 
    india_mix. %>% filter(fromdayone == "Both days" & contact_location == "Nonhome")%>% slice(rep(1:n(), each = 2) # for a contacts/edges that repeated over the two-day period at the non-home layer, we treat it as two edges (i.e., two rows).
    ) %>% 
    rbind(., 
          india_mix. %>% filter(
            (!(fromdayone == "Both days" & contact_location == "Nonhome")) | is.na(fromdayone) # for other contacts/edges (than the above ones) and for those having missing value for fromdayone, we treat it as a single edge (i.e., one row).
          ) 
    ) %>% 
    select(-fromdayone)
  
  ###########  Characterize duration for home ###########
  known_duration_h <- 
    india_mix. %>% 
    filter(contact_location %in% c("Home"))  %>% # filter out contacts of the home layer
    mutate(known_contact_duration_yr = pmin(participant_ageyr,contact_age_num)
           ) %>% 
    group_by(study_site) %>% 
    summarize(know_contact_duration = mean(known_contact_duration_yr)*365
    )
  
    
  
  ###########  Characterize duration for school, work, nonhome ###########
  india_mix_s_w_nh <- 
  india_mix. %>% 
    filter(contact_location %in% c("School", "Work" ,
                                   "Nonhome"
                                   )) # filter out contacts of the school, work, and nonhome layer
  
  #  Characterize the known duration for known_contact >10 yrs at the individual level
  ## Considering partitipant's age in characterizing contact duration
  known_dur_gt_10yr <- india_mix_s_w_nh %>% 
    filter(known_contact == ">10 yrs" 
    ) %>%
    # for known_contact >10 yrs, the known duration is the middle point between 10 yrs and participants age.
    mutate(mid_point = (participant_ageyr - 10)/2 # half of the difference between the 10-yr bound and participants age.
    ) %>% 
    mutate(
      known_duration = case_when(mid_point <= 0 ~ participant_ageyr,# for participants whose age is shorter than the known duration of 10 yrs, we consider the known duration the same as participant's age.
                                 mid_point > 0 ~ mid_point+10) # for participants whose age is longer than the known duration, we calculate the known duration by summing the 10-yr lower bound and "mid_point"
    ) %>% 
    select(rec_id, study_site, contact_location, participant_age, participant_ageyr, contact_age_num, known_duration) # select variables which will be used below
  
  ## Considering contact's age in characterizing contact duration
  known_dur_gt_10yr <- 
    known_dur_gt_10yr %>% 
    mutate(diff= contact_age_num-known_duration) %>% # calculate the difference between contact's age and know duration
    mutate(known_duration_adj = case_when(diff>=0 ~ known_duration, # If contact's age is larger than or equal to the known duration, we treat the known duration as-is
                                          diff<0 ~contact_age_num) # If contact's age is shorter than the known duration, we use the contact age as the duration
    ) %>% 
    # After the above characterization, at school and work, we observe there are still 4 contacts of 458-1, 660-1, 664-1, and 666-1 whose known_duration_adj is < 10. 
    # At nonhome, we have 40 contacts of "24-2",  "106-1", "203-1", "417-1", "498-1", "533-1", "551-1", "608-1", "633-1", "635-1", "542-2", 
    # "177-1", "217-1", "327-1", "353-1", "368-1", "520-1", "547-1", "681-1", "726-1" whose known_duration_adj is < 10. 
    # Since a characterize contact would be valid only if it's >= the 10 year lower bound, we consider them (those duration <10) as invalid and exclude them from analysis
    filter(known_duration_adj > 10)
  
  
  # Calculate avg. known duration of contact longer than 10 years (in days) by network and layer for the study population 
  known_dur_gt_10yr <- 
    known_dur_gt_10yr %>% 
    group_by(study_site, contact_location) %>% 
    summarize(known_contact_avg = mean(known_duration_adj)*365 # converting duration in years to duration in days
    ) %>% ungroup()
  
  # Characterize known duration of contact (in days) for all categories of durations
  known_duration_s_w_nh <-
    india_mix_s_w_nh %>% 
    # characterize frequency of daily contact 
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
  
  # Combining characterized duration of the 4 layers
  known_duration <- 
  known_duration_h %>% mutate(contact_location = "Home") %>% select( study_site, contact_location, know_contact_duration) %>% 
    rbind(.,
  known_duration_s_w_nh
  ) %>% arrange(study_site) %>% 
    mutate(contact_location = factor(contact_location,   levels = c("Home", "School", "Work", "Nonhome")
                                     )
           )
  
  known_duration
  
}
