# Rural-to-Urban Migrant Worker Mobility Shaped Measles Epidemics in China

This repository contains model code and shareable data for the paper:

> **Rural-to-Urban Migrant Worker Mobility Shaped Measles Epidemics in China**  
> DOI: https://doi.org/10.1101/2025.06.21.25330021

---

## Public reproduction scope

The measles incidence data and the mobility data are subject to restriction.
- To access incidence data and/or to seek permission for its use, please contact the Data-center of China Public Health Science: https://www.phsciencedata.cn/Share/
- To access mobility data and/or to seek permission for its use, please contact the Ethics Committee of the School of Journalism and Communication, Beijing Normal University: wuye@bnu.edu.cn

To support a public reproduction workflow, this repository provides model code and shareable data, including a noise-added 2005–2006 traveler mobility dataset. The default model configuration is set up to run the 2005–2006 analysis rather than the full 2005–2014 analysis, which relies on restricted data.

---

## Repository contents

### R scripts

- `code/net_model.R`  
  Networked metapopulation SEIR model.
  - The script is organized with RStudio section headers, which can be navigated using the document outline (`Shift+Command+O` on macOS; `Ctrl+Shift+O` on Windows/Linux).
  - Major sections include data used, SEIR model settings, and the SEIR model.
  - The script simulates measles incidence at the finest modeled resolution, i.e., the subpopulation level, and generates outputs underlying results such as Fig. 4a–d in the paper.
  - The default model configuration and available model options are summarized below.

- `code/plot.R`  
  Result visualization based on outputs from `net_model.R`.

### Data files

- `data/met.csv`  
  Climate data indexed by day of the year.

- `data/census.csv`  
  Census data, including:
  - `pop_tot_yrend_statgov`: total population size at the end of the year
  - `brrt_tot_yrmidadj_statgov`: birth rate based on total population size at the middle of the year
  - `dert_tot_yrmidadj_statgov`: death rate based on total population size at the middle of the year

- `data/vacrt_eff.csv`  
  Immunization rate.

- `data/smry_mig_pair.csv`  
  Host PLAD–origin PLAD pairs:
  - `resid_curr`: host PLAD
  - `resid_regis`: origin PLAD

- `data/mig_distri.csv`  
  Population size of the migrant worker subpopulation.

- `data/prov_code.csv`  
  PLAD code.

- `data/pop_trav.csv`  
  Annual traveler volume.

- `data/flow_trav_synthetic.rds`  
  Noise-added 2005–2006 traveler mobility data used for the workflow.

- `data/Sprop_init.csv`  
  Initial population susceptibility obtained from the population model.

- `data/rprt_bd.csv`  
  Bounds for reporting rate.

- `data/Sprop_SIA.csv`  
  Population susceptibility prior to the nationwide SIA obtained from the population model.

- `data/census_agestr.csv`  
  Age structure for the local population.

- `data/mig_agestr.csv`  
  Age structure for the migrant worker population.

- `data/pop_trav_out_night.csv`  
  `vol_trav_out_night`: total number of days travelers from one PLAD spend in other PLADs.  
  This metric is used to estimate the proportion of travelers by origin. For example, in 2004:
  - Beijing: 8,946,921
  - Tianjin: 3,350,088  
  Among travelers between Beijing and Tianjin, the estimated proportion originating from Beijing is:
  `8946921 / (8946921 + 3350088)`
  and similarly for the reverse direction.

- `data/inc_init.csv`  
  Initial numbers of infectious and exposed individuals.

---

## Running the workflow

This workflow is intended to demonstrate the model structure and reproducibility using shareable data.
1. Clone or download the repository and open it in RStudio.
2. Open `code/net_model.R` and review the RStudio section headers.
3. Check the default model configuration described below.
4. Run `code/net_model.R` to generate model simulations.
5. Run `code/plot.R` to visualize the saved outputs from `code/net_model.R`.

---

## Default model configuration

The paper’s model configuration corresponds to:

- `net_mode = "trav_mig_net"`  
  Metapopulation model connected via both traveler and migrant worker networks.

- `clim_model = "aht"`  
  Absolute humidity and temperature–forced model embedded within the metapopulation model.

- School term-time forcing off  
  Set the lower and upper bounds of `A_sch` in `bd_param_init` to `0`.

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
