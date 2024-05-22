#!/bin/bash

sbatch -p epimodel --job-name=r_h_sa_pt4 --export=ALL,network="Rural",layer="Home",est_apch=“sto_apoxy",percent_target_pop="0.4" R/runest.sh
