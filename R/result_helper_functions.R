# process data frame of FRP length
frp_length_df_process <- 
  function(attr, frp_length, denom){
    attr %>% select(node_id, node.age.grp) %>% 
      left_join(., 
                frp_length %>% data.frame()%>% rownames_to_column(var = "node_id"),
                by = "node_id"
      ) %>% # the NA's in the data frame till here are of node doesn't have any edges, we recode those NA's to 1
      mutate(across(everything(), ~ replace_na(., 1))) %>% 
      mutate(across(where(is.numeric), ~ . / denom))
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


# table showing statistical moments of FRP length in %
frp_moments_layer <-
  function(frp_length_layer, per_n_people, layer, d_365){

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
    mode_frequency <- mode_result["frequency"]
    iqr_value <- IQR(numbers) %>% round(., 2)
    min_value <- min(numbers) %>% round(., 2)
    max_value <- max(numbers) %>% round(., 2)
    q1_value <- quantile(numbers, 0.25) %>% round(., 2)
    q3_value <- quantile(numbers, 0.75) %>% round(., 2)
    min_frequency <- sum(numbers == min(numbers))

    # Create a dataframe with the results
    result <- data.frame(
      `Minimum (n)` = paste0(min_value, " (", min_frequency, ")"),
      Mean = mean_value,
      Median = median_value,
      `Mode (n)` = paste0(mode_value, " (", mode_frequency, ")"),
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


# Define a function to summarize formation statistics
form_stats <- function(tar_stats, summary_stats){
  
  round_df <- function(df) {
    # Define a helper function to round character values
    round_char_values <- function(value) {
      if (grepl("\\(", value)) {
        # Extract numbers from the value
        numbers <- unlist(regmatches(value, gregexpr("[0-9.]+", value)))
        # Round each number to 3 digits
        rounded_numbers <- sapply(numbers, function(x) sprintf("%.3f", as.numeric(x)))
        # Reconstruct the value with rounded numbers
        rounded_value <- paste(rounded_numbers[1], "(", rounded_numbers[2], ")", sep = "")
        return(rounded_value)
      } else if (grepl("^[0-9.]+$", value)) {
        return(sprintf("%.3f", as.numeric(value)))
      } else {
        return(value)
      }
    }
    
    # Apply the helper function to character columns in the data frame
    df %>% mutate(across(where(is.character), ~sapply(.x, round_char_values)))
  }
  
  
  ## mean degree, converted from "edge" target statistics
  
  summary_stat_tb <- 
    c(
      tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home %>% sum()/n_r,
      paste0(tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$School %>% sum()/n_r, " (",
             summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day %>% filter(association == "s_by_w")%>% pull(other_layer.1), ")"
      ),
      paste0(
        tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Work %>% sum()/n_r, " (",
        summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day %>% filter(association == "w_by_s") %>% pull(other_layer.1), ")"
      ),
      tar_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Nonhome %>% sum()/n_r
    ) %>% cbind(., 
                c(
                  tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Home %>% sum()/n_u,
                  paste0(tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$School %>% sum()/n_u, " (",
                         summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day %>% filter(association == "s_by_w")%>% pull(other_layer.1), ")"
                  ),
                  paste0(
                    tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Work %>% sum()/n_u, " (",
                    summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day %>% filter(association == "w_by_s") %>% pull(other_layer.1), ")"
                  ),
                  tar_stats$targetstats_age.grp$formation_stats_urban$edge_ct_matrix$Nonhome %>% sum()/n_u
                ),
                layers[-1]
    ) %>% data.frame() %>% round_df(df=.) %>% select(3,1,2) %>% rename(layer=1, `Rural`=2, `Urban`=3)
  
  summary_stat_tb
}





