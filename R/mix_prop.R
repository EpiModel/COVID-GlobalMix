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