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