# Epidemic Potential in Dynamic Social Networks

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

## Abstract
Background: The epidemic potential of infectious disease transmission is determined by temporal connectivity in a contact network. Forward-reachable path (FRP)—the proportion of total population reachable from an index person via direct or indirect connections—estimates this potential in ways that cross-sectional network measures cannot. 
Methods: We fitted temporal exponential random graph models to network statistics derived from empirical social-contact data from rural and urban Tamil Nadu, India. We simulated one-year networks and calculated age- and layer-specific FRPs. 
Results: In the rural and urban networks, mean and nodal FRPs rose sharply to a threshold, then increased steadily at school and work, or plateaued at home and at the other layer (locations other than home, school, and work). Mean FRPs followed the order: home < school < work < other. The mean FRP peaked at home among those aged 60 (years) and above, at school in the 10–19 group, and at work in the 40–59 group. By day 365, the nodal FRP from one simulation in average reached 0.06% (rural) and 0.03% (urban) at home, 11.84% (rural) and 9.23% (urban) at school, 11.49% (rural) and 26.43% (urban) at work, and 40.52% (rural) and 68.03% (urban) at the other layer of the network population. 
Conclusions: Giant network components (large sets of interconnected nodes) persisted across all contact locations in our social contact networks except home, accelerating nodal FRP reachability. Lockdown measures could restrict visits across multiple layers, thereby preventing these separate components from becoming connected and averting full-population reachability.

