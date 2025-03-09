# process data frame of FRP length to represent FRP as proportion of population reached
frp_length_df_process <- 
  function(attr, frp_length){
    frp_length_crude <- 
    attr  %>% 
      left_join(., 
                frp_length %>% data.frame()%>% rownames_to_column(var = "node_id"),
                by = "node_id"
      ) %>% # the NA's in the data frame till here are of node doesn't have any edges (frp length =1), we recode those NA's to 1
      mutate(across(everything(),~replace_na(.,1)
                    )
             ) 
    
    denom <- nrow(attr)
    
    frp_length_prop <- 
    frp_length_crude %>% 
      mutate(across(where(is.numeric), ~ . / denom))
    
    frp_length_prop
    
  }


# proportion of population reached at a layer, overall and age-specific on day 30, 180, and 365
prop_table_layer <- 
  function(prop_reached){
    ## For each n_reps, calculate the mean proportion of population reached at day 30, 180, and 365
    prop_reached_df <- bind_rows(prop_reached, .id = "n_reps") %>% 
      select(n_reps, node_id, node.age.grp, step_30, step_180, step_365) %>% 
      mutate(n_reps = as.numeric(n_reps)
      )
    
    prop_reached_overall <- 
      prop_reached_df %>% 
      group_by(n_reps) %>% 
      # calculate mean under each n_reps
      summarize(step_30_avg = mean(step_30),
                step_180_avg = mean(step_180),
                step_365_avg = mean(step_365)
      ) %>% 
      ungroup() %>% 
      # get the ntile across n_reps
      summarize(
        step_30_2.5 = quantile(step_30_avg, probs = 0.025),
        step_30_50  = quantile(step_30_avg, probs = 0.5),
        step_30_97.5 = quantile(step_30_avg, probs = 0.975),
        step_180_2.5 = quantile(step_180_avg, probs = 0.025),
        step_180_50  = quantile(step_180_avg, probs = 0.5),
        step_180_97.5 = quantile(step_180_avg, probs = 0.975),
        step_365_2.5 = quantile(step_365_avg, probs = 0.025),
        step_365_50  = quantile(step_365_avg, probs = 0.5),
        step_365_97.5 = quantile(step_365_avg, probs = 0.975)
      ) %>% 
      # covert estimates to publication format
      mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>% # convert proportion to percentile
      mutate(est_30 = paste0(step_30_50, "% (", step_30_2.5, "%, ", step_30_97.5, "%)"),
             est_180 = paste0(step_180_50, "% (", step_180_2.5, "%, ", step_180_97.5, "%)"),
             est_365 = paste0(step_365_50, "% (", step_365_2.5, "%, ", step_365_97.5, "%)")
      ) %>% 
      mutate(node.age.grp ="Overall") %>% 
      select(node.age.grp, est_30, est_180, est_365)
    
    prop_reached_age_spec <- 
      prop_reached_df %>% group_by(n_reps, node.age.grp) %>% 
      # calculate mean under each n_reps and node.age.grp
      summarize(step_30_avg = mean(step_30),
                step_180_avg = mean(step_180),
                step_365_avg = mean(step_365)
      ) %>% 
      ungroup() %>% 
      # get the ntile across n_reps for each age group
      group_by(node.age.grp) %>% 
      summarize(
        step_30_2.5 = quantile(step_30_avg, probs = 0.025),
        step_30_50  = quantile(step_30_avg, probs = 0.5),
        step_30_97.5 = quantile(step_30_avg, probs = 0.975),
        step_180_2.5 = quantile(step_180_avg, probs = 0.025),
        step_180_50  = quantile(step_180_avg, probs = 0.5),
        step_180_97.5 = quantile(step_180_avg, probs = 0.975),
        step_365_2.5 = quantile(step_365_avg, probs = 0.025),
        step_365_50  = quantile(step_365_avg, probs = 0.5),
        step_365_97.5 = quantile(step_365_avg, probs = 0.975)
      ) %>% 
      ungroup() %>% 
      # covert estimates to publication format
      mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>% # convert proportion to percentile
      mutate(est_30 = paste0(step_30_50, "% (", step_30_2.5, ", ", step_30_97.5, "%)"),
             est_180 = paste0(step_180_50, "% (", step_180_2.5, ", ", step_180_97.5, "%)"),
             est_365 = paste0(step_365_50, "% (", step_365_2.5, ", ", step_365_97.5, "%)")
      ) %>% 
      select(node.age.grp, est_30, est_180, est_365)
    
    
    rbind(prop_reached_overall, prop_reached_age_spec)
  }


# proportion of population reached at a layer, overall and age-specific on all days 
prop_figure_layer <- 
  function(prop_reached){
    
    prop_reached_df <- bind_rows(prop_reached, .id = "n_reps") %>% 
      mutate(n_reps = as.factor( as.numeric(n_reps))
      )
    
    ## For each n_reps, calculate the mean overall proportion of population reached for every time step
    prop_reached_overall <- 
    prop_reached_df %>% 
      group_by(n_reps) %>% 
      summarize(across(where(is.numeric), mean, .names = "{.col}_avg")) %>% 
      ungroup()
    
    prop_reached_overall_tile <- 
    prop_reached_overall %>% 
      pivot_longer(
        cols = ends_with("_avg"),
        names_to = "step",
        names_pattern = "(step_[0-9]+)_avg",
        values_to = "avg_value"
      ) %>% 
      group_by(step) %>% 
      summarize(
        quantile_2.5 = quantile(avg_value, probs = 0.025),
        quantile_50  = quantile(avg_value, probs = 0.5),
        quantile_97.5 = quantile(avg_value, probs = 0.975)
      ) %>% ungroup()
    
    
    ## For each n_reps, calculate the mean age-specific proportion of population reached for every time step
    prop_reached_age_spec <- 
      prop_reached_df %>% group_by(n_reps, node.age.grp) %>% 
      # calculate mean under each n_reps and node.age.grp
      summarize(across(where(is.numeric), mean, .names = "{.col}_avg")) %>% 
      ungroup() 
    
    prop_reached_age_spec_tile <- 
      # get the ntile across n_reps for each age group
      prop_reached_age_spec %>% 
      pivot_longer(
        cols = ends_with("_avg"),
        names_to = "step",
        names_pattern = "(step_[0-9]+)_avg",
        values_to = "avg_value"
      ) %>%
      group_by(node.age.grp, step) %>%
      summarize(
        quantile_2.5 = quantile(avg_value, probs = 0.025),
        quantile_50  = quantile(avg_value, probs = 0.5),
        quantile_97.5 = quantile(avg_value, probs = 0.975)
      ) %>%
      ungroup()
    
    prop_reached_tile <- 
    rbind(prop_reached_overall_tile %>% 
            mutate(node.age.grp = "Overall") %>% 
            select(node.age.grp, step, quantile_2.5, quantile_50, quantile_97.5), 
          prop_reached_age_spec_tile
          )%>%
      mutate(step = as.numeric(gsub("step_", "", step)))%>%
      arrange(step)
    
    
    prop_reached_tile 
  }



# line plot for FRP length
frp_length_plot <- 
  function(frp_length, title){
    ## Define the color palette
    palv6 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00") # a color-blind friendly palette
    
    ## Map the categories to the corresponding colors
    color_mapping <- setNames(palv6, c("0-9y",  "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))
    
    ## Custom function to format y-axis labels as percentages and round to two decimal places
    percent_format <- function(x) {
      paste0(round(x, 2), "%")
    }
    
    ## Plot using ggplot2
    frp_length %>% 
      # Reshape the data from wide to long format
      pivot_longer(cols = starts_with("step_"), names_to = "time", values_to = "frp_length") %>%
      mutate(time = as.numeric(gsub("step_", "", time))) %>% 
      ggplot(aes(x = time, y = frp_length * 100, group = node_id, color = node.age.grp)) + # Multiply frp_length by 100
      geom_line(linewidth = 0.2, alpha = 0.6) +
      scale_color_manual(values = color_mapping) +
      scale_y_continuous(labels = function(x) percent_format(x)) + # Format y-axis using custom function
      labs(title = title, x = "", y = "FRP length/N") + # Update y-axis label
      theme_classic() +
      theme(legend.position = "right")
  }


# Function producing table showing statistical moments of FRP length in % on day 0, 1, 183, 365
frp_moments_layer <-
  function(frp_length_layer, per_n_people, layer, N){

  frp_length_layer <- frp_length_layer %>%
    mutate(across(where(is.numeric), ~ . * per_n_people)) # multiple per_n_people to proportion to get the value per n people

  frp_moments <- function(numbers, scenario) {
    # Define the function to calculate mode
    calculate_mode <- function(numbers) {
      freq_table <- table(numbers)
      mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
      mode_frequency <- as.numeric(max(freq_table))
      return(c(value = mode_value, frequency = mode_frequency))
    }

    mean_value <- mean(numbers) %>% round(., 2)
    median_value <- median(numbers) %>% round(., 2)
    mode_result <- calculate_mode(as.numeric(numbers))
    mode_value <- mode_result["value"] %>% round(., 2)
    mode_prop <- paste0(round((mode_result["frequency"]/N)*100,2), "%")
    iqr_value <- IQR(numbers) %>% round(., 2)
    min_value <- min(numbers) %>% round(., 2)
    max_value <- max(numbers) %>% round(., 2)
    q1_value <- quantile(numbers, 0.25) %>% round(., 2)
    q3_value <- quantile(numbers, 0.75) %>% round(., 2)
    min_prop <- paste0(round((sum(numbers == min(numbers))/N)*100,2), "%")

    # Create a dataframe with the results
    result <- data.frame(
      `Minimum (%)` = paste0(min_value, " (", min_prop, ")"),
      Mean = mean_value,
      Median = median_value,
      `Mode (%)` = paste0(mode_value, " (", mode_prop, ")"),
      Maximum = max_value,
      Q1 = q1_value,
      Q3 = q3_value,
      IQR = iqr_value,
      check.names = FALSE
    )
    rownames(result) <- scenario

    return(result)
  }

  ### Number of nodes with different FRP lengths at specific time points
  t <- c(0, seq(from = 1, to = 365, length.out = 3))
  t_name <- paste0("step_", t)
  scenario <- paste0(layer, "_", "step_", t)

  rbind(
    frp_moments(numbers = frp_length_layer[, t_name[1]], scenario = scenario[1]), # t=0
    frp_moments(numbers = frp_length_layer[, t_name[2]], scenario = scenario[2]), # t=1
    frp_moments(numbers = frp_length_layer[, t_name[3]], scenario = scenario[3]), # t=183
    frp_moments(numbers = frp_length_layer[, t_name[4]], scenario = scenario[4])  # t=365
  )
  }

# Function producing table showing statistical moments of FRP length in % on day 365
frp_moments_365_layer <-
  function(frp_length_layer, per_n_people){
    
    
    frp_length_layer <- 
      frp_length_layer %>%
      mutate(across(where(is.numeric), ~ . * per_n_people)) # multiple per_n_people to proportion to get the value per n people
    
    summary_overall <- 
     frp_length_layer %>% summarize(Mean =mean(step_365) %>% round(.,2), 
                                   Median =median(step_365) %>% round(.,2), 
                                   IQR = paste0(
                                     quantile(step_365, 0.25)%>% round(.,2), ", ",
                                     quantile(step_365, 0.75)%>% round(.,2)
                                     )
                                   ) %>% 
      as.data.frame() %>% mutate(node.age.grp = "Overall") %>% select(4,1:3)
    
    summary_age_spec <- 
    frp_length_layer %>% 
      group_by(node.age.grp) %>% 
      summarize(Mean =mean(step_365) %>% round(.,2), 
                                   Median =median(step_365) %>% round(.,2), 
                                   IQR = paste0(
                                     quantile(step_365, 0.25)%>% round(.,2), ", ",
                                     quantile(step_365, 0.75)%>% round(.,2)
                                   ))%>% 
      as.data.frame()
    
    rbind(
      summary_overall, summary_age_spec
    )
  
  }

# Function to calculate the proportion of non-isolated nodes in a layer
prop_non_isolate <-
  function(frp_crude_vec){
    
    frp_crude_vec %>% table
    
    # proportion corresponds to frp length =1
    prop_frp <- prop.table(table(frp_crude_vec==1)) # T - isolates; F - non-isolates
    
    # convert proportion to percentage
    percentages <- round(prop_frp * 100, 2)
    
    
    
   paste0(percentages[names(percentages)=="FALSE"], "%")
   
    
  }


# Define a function to summarize formation statistics
network_stats <- function(tar_stats, summary_stats, n_r_age_grp, n_u_age_grp){
  
  n_r = sum(n_r_age_grp); n_u= sum(n_u_age_grp)
  
  md_age <- function(edge_ct_m, n_age_grp) {
    
    edge_ct_age <- function(edge_ct_m, a){
      (1/2)*(sum(edge_ct_m[a,]) + sum(edge_ct_m[,a])
      )
    }
    
    
    # calculate age-specific edge count 
    edge_ct_1 <-edge_ct_age(edge_ct_m, a=1)
    edge_ct_2 <-edge_ct_age(edge_ct_m, a=2)
    edge_ct_3 <-edge_ct_age(edge_ct_m, a=3)
    edge_ct_4 <-edge_ct_age(edge_ct_m, a=4)
    edge_ct_5 <-edge_ct_age(edge_ct_m, a=5)
    edge_ct_6 <-edge_ct_age(edge_ct_m, a=6)
    
    
    md_age <- 
      c(
        2*(edge_ct_1
        )/n_age_grp[1], #0-9y 
        2*(edge_ct_2
        )/n_age_grp[2],
        2*(edge_ct_3
        )/n_age_grp[3],
        2*(edge_ct_4
        )/n_age_grp[4],
        2*(edge_ct_5
        )/n_age_grp[5],
        2*(edge_ct_6
        )/n_age_grp[6] # 60+y
      ) %>% round(., 2)
    
    data.frame(md_age)
  }
  
  
  # overall mean degree
  
 overall_md <- 
    c( 
      (2*tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home %>% sum()/n_r) %>% round(., 2), # home, rural
      (2*tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Home %>% sum()/n_u) %>% round(., 2),# home, urban
      
      paste0((2*tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% sum()/n_r) %>% round(., 2), " (", # school, rural
             
             tar_stats$targetstats_x.layer$rural %>% 
               filter(association == "s_by_w") %>% 
               pull(md_other_layer_1)%>% round(., 2), 
             ")"
      ),
      paste0((2*tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School %>% sum()/n_u) %>% round(., 2), " (",# school, urban
             tar_stats$targetstats_x.layer$urban %>% 
               filter(association == "s_by_w") %>% 
               pull(md_other_layer_1)%>% round(., 2), 
             ")"
      ),
      
      paste0( 
        (2*tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum()/n_r) %>% round(., 2), " (", # work, rural
        tar_stats$targetstats_x.layer$rural %>% 
          filter(association == "w_by_s") %>% 
          pull(md_other_layer_1)%>% round(., 2), 
        ")"
      ),
      paste0(
        (2*tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work %>% sum()/n_u) %>% round(., 2), " (",# work, urban
        tar_stats$targetstats_x.layer$urban %>% 
          filter(association == "w_by_s") %>% 
          pull(md_other_layer_1)%>% round(., 2), 
        ")"
      ),
       (2*tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome %>% sum()/n_r) %>% round(., 2), # nonhome, rural
      (2*tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Nonhome %>% sum()/n_u) %>% round(., 2)  # nonhome, rural
    ) %>% t() %>%  data.frame() 
 
   # age-specific mean degree
 age_spec_md <- 
 cbind(
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home,
         n_age_grp = n_r_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Home,
         n_age_grp = n_u_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School,
         n_age_grp = n_r_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School,
         n_age_grp = n_u_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work,
         n_age_grp = n_r_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work,
         n_age_grp = n_u_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome,
         n_age_grp = n_r_age_grp),
  md_age(edge_ct_m = tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Nonhome,
         n_age_grp = n_u_age_grp)
 )
 
 # proportion of contact
 prop_contact <- 
 c(
   rep("-",6),
   paste0(
 (tar_stats$attr$rural$contact_attribute_Nonhome %>% table() %>% prop.table() %>% data.frame() %>% filter(.==1) %>% pull(Freq)*100) %>% round(.,2), "%"
   ),
   paste0(                                                                                                 
 (tar_stats$attr$urban$contact_attribute_Nonhome %>% table() %>% prop.table() %>% data.frame() %>% filter(.==1) %>% pull(Freq)*100) %>% round(.,2), "%"
   )
 )%>% t() %>%  data.frame() 
 
 # within-age mixing proportion
 assort_prop <- 
 cbind(
 summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Home$Home_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Home$Home_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 
 summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$School$School_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$School$School_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 
 summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Work$Work_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Work$Work_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 
 summary_stats$formation$formation_stats_rural$mix_prop_rural_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag(),
 summary_stats$formation$formation_stats_urban$mix_prop_urban_layers$Nonhome$Nonhome_mix_prop_matrix_2d_glm %>% as.matrix()%>% diag()
 )%>% data.frame() %>% 
   mutate(across(everything(), ~ ifelse(is.na(.), 0, .)),
          across(everything(), ~ paste0(round(. * 100, 2), "%"))
          )
 
 # contact duration
 dur <- 
 c(rep("-",2),
 summary_stats$dissolution %>%
   mutate(contact_location = factor(contact_location, levels = c( "School", "Work", "Nonhome"))) %>%
   arrange(contact_location) %>% pull(know_contact_duration) %>% round(., 2)
 ) %>% t() %>%  data.frame() 
 
 
 
 colnames(overall_md) <- colnames(age_spec_md) <- colnames(prop_contact) <- colnames(assort_prop) <- colnames(dur) <-    c("h_r", "h_u", "s_r", "s_u", "w_r", "w_u", "nh_r", "nh_u")
 
 summary_stat_tb <- 
 bind_rows( overall_md %>% mutate(across(everything(), as.character)) , 
        age_spec_md %>% mutate(across(everything(), as.character)), 
        prop_contact %>% mutate(across(everything(), as.character)),
        assort_prop%>% mutate(across(everything(), as.character)),
        dur%>% mutate(across(everything(), as.character))
        )
 
 row.names(summary_stat_tb) <- c("Mean degree (conditioned)", "Mean deg, 0-9y", "Mean deg, 10-19y", "Mean deg, 20-29y", "Mean deg, 30-39y", "Mean deg, 40-59y", "Mean deg, 60+y",
                                 "having contact %", "assort %, 0-9y", "assort %, 10-19y", "assort %, 20-29y", "assort %, 30-39y", "assort %, 40-59y", "assort %, 60+y", "dur")
  
 summary_stat_tb
}





