. /projects/epimodel/spack/share/spack/setup-env.sh
spack unload -a
spack load r@4.3.2
Rscript "workflows/networks/SWF/steps/2/script.R"
