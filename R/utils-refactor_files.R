#   network = "Rural"/"Urban"
#   percent_target_pop = 0.1/0.4/1
#   n_reps = 100, number of simulation to run

layers <- c("Home", "School", "Work", "Nonhome")
file.name_in <- paste0(
  "data/netsim_outputs/el_cuml__",
  layers, "__",
  network,"__",
  percent_target_pop, ".Rds"
)

el_cuml_home    <- readRDS(file.name_in[["Home"]])
el_cuml_school  <- readRDS(file.name_in[["School"]])
el_cuml_work    <- readRDS(file.name_in[["Work"]])
el_cuml_nonhome <- readRDS(file.name_in[["Nonhome"]])

for (i in seq_len(n_reps)) {
  els_path <- paste0("data/netsim_outputs__", i, ".rds")
  els <- list(
    home    = el_cuml_home[[i]],
    school  = el_cuml_school[[i]],
    work    = el_cuml_work[[i]],
    nonhome = el_cuml_nonhome[[i]]
  )
  saveRDS(els, els_path)
}
