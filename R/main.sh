#!/bin/bash

sbatch -p preemptable --export=ALL,network="Rural",layer="School",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
