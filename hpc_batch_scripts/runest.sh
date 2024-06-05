#!/bin/bash

. /projects/epimodel/spack/share/spack/setup-env.sh
spack load git@2.35.1
spack load r@4.1.2 

Rscript R/3-network_est.R

R --no-save --no-restore