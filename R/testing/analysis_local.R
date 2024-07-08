suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))

layer = c("Home","School","Work","Nonhome") # the "All" category also exists and applies to the 6-tsna script
network = c("Rural", "Urban")
est_apch =   "mcmle" #  "sto_apoxy"
percent_target_pop=0.4


# Diagnosing FRP results
netsim_r <- readRDS("data/netsim_outputs/sim___Rural__sto_apoxy.Rds")
str(netsim_r$Home)

library(sna)

# Get the degree of each node
node_degrees_layers <- 
  data.frame(
      Home=  degree(netsim_r$Home),
      School=  degree(netsim_r$School),
      Work=  degree(netsim_r$Work),
      Nonhome=  degree(netsim_r$Nonhome)
        )

## Interpretation to the plots below: we observe there are ~40% nodes with >100 contacts at the school layers - this likely contribute to the unrelistic FRP.
par(mfrow = c(2, 2))
node_degrees_layers$Home %>% hist(main = "Home")
node_degrees_layers$School %>% hist(main= "School") # something wrong at the school layer.
sum(node_degrees_layers$School>100)
node_degrees_layers$Work %>% hist(main= "Work")
node_degrees_layers$Nonhome %>% hist(main = "Nonhome")

## Tabulating nodal attributes for rural school layer
### Initial nodal attribute
table(node_attribute_target_stats$attr$rural$contact_attribute_School, node_attribute_target_stats$attr$rural$node.age.grp
      )

table(
model_input_items$initiate_nw$Rural$nw_s %v% "age.grp", model_input_items$initiate_nw$Rural$nw_s %v% "deg.x_layer"
)






