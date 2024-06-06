# Must be sourced **AFTER** "./R/utils-0_project_settings.R"

hpc_configs <- EpiModelHPC::swf_configs_rsph(
  partition = "epimodel",
  r_version = "4.3.1",
  mail_user = "dehao.chen@emory.edu"
)