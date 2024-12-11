## HPC Workflow: Networks
##
## Define a workflow to run the netest for home layer of both networks
## on the HPC


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("netest_0.1_h_1207", override = TRUE)

# netest
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/3-network_est.R",
    network=c("Rural", "Urban"),
    
    MoreArgs = list(
      hpc_context = TRUE,
      layer=c("Home"),
      #est_apch="mcmle",
      percent_target_pop="0.1"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "1:00:00",
    "mem" = "0"
  )
)
