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
    palv6 <- c("red", "blue", "green", "purple", "orange", "cyan")
    
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
      geom_line(size = 0.2, alpha = 0.6) +
      scale_color_manual(values = color_mapping) +
      scale_y_continuous(labels = function(x) percent_format(x)) + # Format y-axis using custom function
      labs(title = title, x = "", y = "FRP length/N") + # Update y-axis label
      theme_classic() +
      theme(legend.position = "right")
  }


# table showing statistical moments of FRP length in %
frp_moments_layer <- function(frp_length_layer, layer){
  
  # Define the function to calculate mode
  calculate_mode <- function(numbers) {
    freq_table <- table(numbers)
    mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
    mode_frequency <- as.numeric(max(freq_table))
    return(c(value = mode_value, frequency = mode_frequency))
  }
  
  frp_moments <- function(numbers, scenario) {
    # Convert to percentages and round appropriately
    numbers <- numbers * 100
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
      Mean = paste0(mean_value, "%"), 
      Median = paste0(median_value, "%"), 
      `Mode (n)` = paste0(mode_value, " (", mode_frequency, ")"),
      Maximum = paste0(max_value, "%"), 
      Q1 = paste0(q1_value, "%"), 
      Q3 = paste0(q3_value, "%"),
      IQR = paste0(iqr_value, "%"),
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
