#!/bin/bash

sbatch -p epimodel --job-name=r_h_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Home",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=r_nh_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Nonhome",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=r_w_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Work",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=r_s_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="School",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh

sbatch -p epimodel --job-name=u_h_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Home",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=u_nh_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Nonhome",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=u_w_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Work",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh
sbatch -p epimodel --job-name=u_s_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="School",est_apch="mcmle",percent_target_pop="0.4" R/runest.sh

sbatch -p epimodel --job-name=dx_r_h_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Home",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_r_nh_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Nonhome",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_r_w_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="Work",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_r_s_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Rural",layer="School",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh

sbatch -p epimodel --job-name=dx_u_h_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Home",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_u_nh_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Nonhome",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_u_w_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="Work",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh
sbatch -p epimodel --job-name=dx_u_s_mcmle_pt4 --mail-user=dehao.chen@emory.edu --mail-type=BEGIN,END --export=ALL,network="Urban",layer="School",est_apch="mcmle",percent_target_pop="0.4" R/rundx.sh

