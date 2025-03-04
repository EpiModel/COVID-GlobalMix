## HPC Workflow: Network simulation & FRP calculation


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("frp_r_0.5_w_nh", override = TRUE)


# Refactor netsim files to avert file-loading issues
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "./R/utils-refactor_files.R",
    args = list(
      hpc_context = TRUE,
      network = "Rural",
      percent_target_pop = "0.5",
      n_reps = 100
    ),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "4:00:00",
    "mem" = "0"
  )
)

# FRP calculation
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/6-tsna.R",
    layer = c("Work", "Nonhome"),
    MoreArgs = list(
      hpc_context = TRUE,
      network = "Rural",
      percent_target_pop = "0.5",
      n_cores = est_cores,
      n_reps = 100
    ),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "360:00:00",
    "mem" = "0"
  )
)
