## Define function to estimate models of the 8 layers, using either stochastic approximation or MCMLE
est_nws <- 
  function(control.arg, layer, site, model_input_items){
    
      if(site=="Rural"){ 
        nw_attributes <- model_input_items$initiate_nw$Rural # network attributes of all layers
        model_inputs <- model_input_items$formula_tarstats$Rural # network statistics and formation model formula of all layers
      } else if (site =="Urban"){
        nw_attributes <- model_input_items$initiate_nw$Urban
        model_inputs <- model_input_items$formula_tarstats$Urban
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
