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
