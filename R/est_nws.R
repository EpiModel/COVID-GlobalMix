## Define function to estimate models of the 8 layers, using either stochastic approximation or MCMLE
est_nws <- 
  function(control.arg, all_layers, layer, site, model_input_items){
    
    
    if(all_layers ==T){
      
      layers <- c("Home", "School", "Work", "Nonhome") 
      ## Create lists to store results of the 8 layers, one for stochastic approximation, one for MCMLE
      est_layer <- 
        list(Rural = c(), Urban = c()
        )
      
      for (i in 1:2) { # i=1 is for rural network, i=2 is for urban network
        
        if(i==1){ 
          nw_attributes <- model_input_items$initiate_nw$rural # network attributes of all layers
          model_inputs <- model_input_items$formula_tarstats$rural # network statistics and formation model formula of all layers
        } else {
          nw_attributes <- model_input_items$initiate_nw$urban
          model_inputs <- model_input_items$formula_tarstats$urban
        }
        
        for (j in 1:length(layers) # j of 1, 2, 3, 4 corresponds to home, school, work, nonhome
        ) {
          # define nodal attribute for the model
          if(
            layers[j] %in% c("Home", "Nonhome")
          ){
            nw_attributes_layer = nw_attributes$nw
          } else if (
            layers[j] == "School"
          ){
            nw_attributes_layer = nw_attributes$nw_s
          } else if (
            layers[j] == "Work"
          ){
            nw_attributes_layer = nw_attributes$nw_w
          } else .
          
          # model fitting for each layer
          est_layer[[i]][[j]] <- 
            netest(nw= nw_attributes_layer,
                   formation = model_inputs[[layers[j]]]$frmn_fm, 
                   target.stats = model_inputs[[layers[j]]]$tstat, 
                   coef.diss = model_inputs[[layers[j]]]$diss,
                   set.control.ergm = control.arg 
                   
            )
          
          print(paste0("i=", i, " j=", j)
          )
        }
      }
      
      
      #### Assign layer names to each network
      names(est_layer$Rural) <- names(est_layer$Urban) <- layers
      
      est_layer
      
    } else {
      
      if(site=="Rural"){ 
        nw_attributes <- model_input_items$initiate_nw$rural # network attributes of all layers
        model_inputs <- model_input_items$formula_tarstats$rural # network statistics and formation model formula of all layers
      } else if (site =="Urban"){
        nw_attributes <- model_input_items$initiate_nw$urban
        model_inputs <- model_input_items$formula_tarstats$urban
      } else{}
      
      
      # define nodal attribute for the model
      if(
        layer %in% c("Home", "Nonhome")
      ){
        nw_attributes_layer = nw_attributes$nw
      } else if (
        layer == "School"
      ){
        nw_attributes_layer = nw_attributes$nw_s
      } else if (
        layer == "Work"
      ){
        nw_attributes_layer = nw_attributes$nw_w
      } else {}
      
      # model fitting for each layer
      est_layer <- 
        netest(nw= nw_attributes_layer,
               formation = model_inputs[[layer]]$frmn_fm, 
               target.stats = model_inputs[[layer]]$tstat, 
               coef.diss = model_inputs[[layer]]$diss,
               set.control.ergm = control.arg 
               
        )
      
      est_layer
    }
  }