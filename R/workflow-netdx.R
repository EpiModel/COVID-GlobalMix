## HPC Workflow: a workflow to run the estimation and diagnostics of the network models
# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("netdx_0.5_s_w_u_0303", override = TRUE)


# netdx
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/4-network_dx.R",
    layer=c( "School", "Work"),
    MoreArgs = list(
      hpc_context = TRUE,
      network=c("Urban"),
      est_apch="mcmle",
      percent_target_pop="0.5"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "120:00:00",
    "mem" = "0"
  )
)

