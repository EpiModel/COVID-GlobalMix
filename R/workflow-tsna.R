## HPC Workflow: Calculation of the length of forward-reachable path


# Setup ------------------------------------------------------------------------
library(slurmworkflow)

hpc_context <- TRUE

source("R/hpc_configs.R", local = TRUE)

# Process ----------------------------------------------------------------------
wf <- make_em_workflow("tsna_u_0.4", override = TRUE)


# FRP calculation
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/6-tsna.R",
    layer=c("All", "Home","School","Work","Nonhome"),
    MoreArgs = list(hpc_context = TRUE,
                network="Urban",
                est_apch="mcmle",
                percent_target_pop="0.4",
                nodes=NULL), 
    setup_lines = hpc_node_setup
  ),
  sbatch_opts = list(
    "cpus-per-task" = est_cores,
    "time" = "24:00:00",
    "mem" = "0"
  )
)






