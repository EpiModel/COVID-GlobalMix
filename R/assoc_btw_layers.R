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