# COVID-GlobalMix

## Introduction
This is a repository for scripts modeling human contact networks, estimating forward-reachable paths (FRPs), based on empirical data from the GlobalMix India sites.

## Table of content
Below is a list of the R scripts used and their descriptions.

- **1-network_params.R**: Calculation of individual-level summary statistics. The input data are available upon request.
- **2-network_targetstats.R**: Calculation of target statistics.
- **3-network_est.R**: Estimation of network models.
- **4-network_dx.R**: Diagnosis of network models.
- **5-network_sim.R**: Simulating home, school, work, and nonhome (other) layers for the rural and urban networks.
- **6-tsna.R**: Calculating FRP for each layer in the rural and urban networks.
- **7-hpc_results_processing.R**: Processing results generated from high-performance computing.
- **8-non_hpc_results.R**: Generating tables and figures for the manuscript.

