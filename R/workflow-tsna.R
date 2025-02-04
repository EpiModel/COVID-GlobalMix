## HPC Workflow: Network simulation & FRP calculation


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("frp_r_0.1_0203_1st_try", override = TRUE)



# FRP calculation
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/6-tsna.R",
    layer = c("All", "Home", "School", "Work", "Nonhome"),
    MoreArgs = list(
      hpc_context = TRUE,
      network = "Rural",
      est_apch = "mcmle",
      percent_target_pop = "0.1",
      n_cores = est_cores,
      n_reps = 100
    ),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "120:00:00",
    "mem" = "0"
  )
)

