## HPC Workflow: Networks
##
## Define a workflow to run the estimation and diagnostics, network simulation, and calculation of forward-reachable path of the network models
## on the HPC


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("netest_to_tsna_0.4_u", override = TRUE)

# netest
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/3-network_est.R",
    layer=c("Home","School","Work","Nonhome"),
    MoreArgs = list(
      hpc_context = TRUE,
      network="Urban",
      est_apch="sto_apoxy",
      percent_target_pop="0.1"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "72:00:00",
    "mem" = "0"
  )
)

# netdx
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/4-network_dx.R",
    layer=c( "Home","School","Work","Nonhome"),
    MoreArgs = list(
      hpc_context = TRUE,
      network="Urban",
      est_apch="mcmle",
      percent_target_pop="0.1"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "24:00:00",
    "mail-type" = "FAIL",
    "mem" = "0"
  )
)

# # netsim
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/5-network_sim.R",
#     args = list(hpc_context = TRUE,
#                 network="Urban",
#                 est_apch="mcmle",
#                 percent_target_pop="0.4"),
#     setup_lines = hpc_node_setup
#   ),
#   sbatch_opts = list(
#     "cpus-per-task" = est_cores,
#     "time" = "24:00:00",
#     "mem" = "0"
#   )
# )
#
# # FRP calculation
# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_map_script(
#     r_script = "R/6-tsna.R",
#     layer=c("All", "Home","School","Work","Nonhome"),
#     MoreArgs = list(hpc_context = TRUE,
#                     network="Urban",
#                     est_apch="mcmle",
#                     percent_target_pop="0.4",
#                     nodes=NULL),
#     setup_lines = hpc_node_setup
#   ),
#   sbatch_opts = list(
#     "cpus-per-task" = est_cores,
#     "time" = "24:00:00",
#     "mem" = "0"
#   )
# )
#
#
