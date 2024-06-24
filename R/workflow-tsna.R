## HPC Workflow: FRP calculation


# Setup ------------------------------------------------------------------------
library(slurmworkflow)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("tsna", override = TRUE)


# FRP calculation

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/6-tsna.R",
    layer=c("Home","School","Work","Nonhome"),
    args = list(hpc_context = TRUE,
                network="Rural",
                est_apch="mcmle",
                percent_target_pop="0.4"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "24:00:00",
    "mem" = "0"
  )
)

# wf <- add_workflow_step(
#   wf_summary = wf,
#   step_tmpl = step_tmpl_do_call_script(
#     r_script = "R/6-tsna.R",
#     args = list(hpc_context = TRUE,
#                 layer = "School",
#                 network="Urban",
#                 est_apch="mcmle",
#                 percent_target_pop="0.1"),
#     setup_lines = hpc_node_setup
#   ),
#   sbatch_opts = list(
#     "cpus-per-task" = est_cores,
#     "time" = "24:00:00",
#     "mem" = "0"
#   )
# )




