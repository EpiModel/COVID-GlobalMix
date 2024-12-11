## HPC Workflow: Network simulation


# Setup ------------------------------------------------------------------------
library(slurmworkflow)
library(future.apply)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("netsim_frp_h_all_u_0.1_1211", override = TRUE)


# netsim
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/5-network_sim.R",
    args = list(hpc_context = TRUE,
                network="Urban",
                est_apch="mcmle",
                percent_target_pop="0.1"),
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "36:00:00",
    "mem" = "0"
  )
)

# FRP calculation
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/6-tsna.R",
    layer=c("All", "Home"),
    MoreArgs = list(hpc_context = TRUE,
                    network="Urban",
                    est_apch="mcmle",
                    percent_target_pop="0.1",
                    nodes=NULL), 
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "36:00:00",
    "mem" = "0"
  )
)




