#!/bin/bash

. /projects/epimodel/spack/share/spack/setup-env.sh; spack load git@2.35.1; spack load r@4.1.2; R --no-save --no-restore

Rscript 3-network_est.R
