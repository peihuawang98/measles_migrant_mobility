# Rural-to-Urban Migrant Worker Mobility Shaped Measles Epidemics in China

This repository contains code and shareable data for the paper:

> **Rural-to-Urban Migrant Worker Mobility Shaped Measles Epidemics in China**  
> DOI: https://doi.org/10.1101/2025.06.21.25330021

Because the mobility data are not publicly available without permission, we provide a shareable version in which we add noise to two years of mobility data to support reproducibility. 

---

## Repository contents

### R scripts

- `net_model.R`  
  Networked metapopulation SEIR model.

- `plot.R`  
  Result visualization based on outputs from `net_model.R`.

### Data files

- `met.csv`  
  Climate data indexed by day of the year.

- `census.csv`  
  Census data, including:
  - `pop_tot_yrend_statgov`: total population size at the end of the year
  - `brrt_tot_yrmidadj_statgov`: birth rate based on total population size at the middle of the year
  - `dert_tot_yrmidadj_statgov`: death rate based on total population size at the middle of the year

- `vacrt_eff.csv`  
  Immunization rate.

- `smry_mig_pair.csv`  
  Host PLAD–origin PLAD pairs:
  - `resid_curr`: host PLAD
  - `resid_regis`: origin PLAD

- `mig_distri.csv`  
  Population size of the migrant worker subpopulation.

- `prov_code.csv`  
  PLAD code.

- `pop_trav.csv`  
  Annual traveler volume.

- `flow_trav_synthetic.rds`  
  Noise-added 2005–2006 traveler mobility data.

- `Sprop_init.csv`  
  Initial population susceptibility obtained from the population model.

- `rprt_bd.csv`  
  Bounds for report rate.

- `Sprop_SIA.csv`  
  Population susceptibility prior to the nationwide SIA obtained from the population model.

- `census_agestr.csv`  
  Age structure for the local population.

- `mig_agestr.csv`  
  Age structure for the migrant worker population.

- `pop_trav_out_night.csv`  
  `vol_trav_out_night`: total number of days travelers from one PLAD spend in other PLADs.  
  This metric is used to estimate the proportion of travelers by origin. For example, in 2004:
  - Beijing: 8,946,921
  - Tianjin: 3,350,088  
  Among travelers between Beijing and Tianjin, the estimated proportion originating from Beijing is:
  `8946921 / (8946921 + 3350088)`  
  and similarly for the reverse direction.

- `inc_init.csv`  
  Initial number of infectious and exposed individuals.

---

## Model configuration

The paper’s model configuration corresponds to:

- `net_mode = "trav_mig_net"`  
  Metapopulation model connected via both traveler and migrant worker networks.

- `clim_model = "aht"`  
  Absolute humidity and temperature–forced model embedded within the metapopulation model.

- School term-time forcing off  
  Set lower and upper bounds of `A_sch` in `bd_param_init` to `0`.

- `res_t_seq = "month"`  
  Monthly temporal resolution.

Public mobility data constraint: Only 2-year noise-added traveler mobility data are included here, so the model is configured to run from 2005 to 2006, i.e.,
`tid_end = which(t_seq == ymd(20061231))`.

The number of model realizations can be adjusted via `J`.

---

## Model options

### `net_mode`

- `"trav_mig_net"`: metapopulation model connected via both traveler and migrant worker networks  
- `"mig_net"`: metapopulation model connected via migrant worker network  
- `"trav_net"`: metapopulation model connected via traveler network  
- `"iso_net"`: metapopulation model without mobility network

### `clim_model`

- `"aht"`: absolute humidity and temperature forcing embedded within the metapopulation model  
- `"sin"`: sinusoidal climate forcing embedded within the metapopulation model

### `res_t_seq`

- `"month"`: monthly simulation  
- `"wk"`: weekly simulation

### Turning on school term-time forcing

Set the lower and upper bounds for `A_sch` in `bd_param_init` to values as needed.




