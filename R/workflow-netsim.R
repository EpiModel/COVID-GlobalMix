## HPC Workflow: Network simulation


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("netsim_u_0.1", override = TRUE)


# netsim
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/5-network_sim.R",
    args = list(hpc_context = TRUE,
                network="Urban",
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




