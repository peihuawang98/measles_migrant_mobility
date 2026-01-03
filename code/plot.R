rm(list=ls())

library(tidyverse)
library(ggplot2)

dir_data = "measles_migrant_mobility/data/"
dir_rslt = "measles_migrant_mobility/rslt/"



# meta --------------------
census_mig_pair = read.csv(paste0(dir_data, "smry_mig_pair.csv"))
prov_code = read.csv(paste0(dir_data, "prov_code.csv"))

net_mode = "trav_mig_net"
cluster = prov_code$prov_name 
batch = 1

smry_subpop = list()
for (p in cluster) {
  subpop_a = census_mig_pair[census_mig_pair$resid_curr == p, "resid_regis"]
  subpop_a = subpop_a[subpop_a %in% cluster]

  if (length(subpop_a) >= 1) {
    subpop_a = paste0(p, "_", subpop_a)
  } else if (length(subpop_a) == 0) {
    subpop_a = NULL
  }
  
  subpop_b = census_mig_pair[census_mig_pair$resid_regis == p, "resid_curr"]
  subpop_b = subpop_b[subpop_b %in% cluster]

  if (length(subpop_b) >= 1) {
    subpop_b = paste0(subpop_b, "_", p, "_r")
  } else if (length(subpop_b) == 0) {
    subpop_b = NULL
  }
  
  subpop = c(p, subpop_a, subpop_b)
  smry_subpop[[p]] = list(subpop = subpop)
}



# plot --------------------
p = "Beijing"

state_swarm = readRDS(paste0(dir_rslt, "state_swarm_rslt_", net_mode, "_", p, "_batch", batch, ".rds"))
param_swarm = state_swarm[["param_swarm"]]
state_swarm = state_swarm[["var_swarm"]]

subpop = smry_subpop[[p]][["subpop"]]

newI = state_swarm[grepl("^newI_", dimnames(state_swarm)[[1]]),,, drop = FALSE]
rho = param_swarm["rho",]
rho = array(rep(rho, each = dim(newI)[1]), dim = dim(newI))
newI = round(newI*rho)

newI = reshape2::melt(newI, varnames = c("subpop", "J", "date"), value.name = "inc")
newI$date = as.Date(fastPOSIXct(newI$date))

newI = newI %>%
  group_by(subpop, date) %>%
  summarise(inc_median = median(inc)) %>%
  arrange(date) %>%
  as.data.frame()

newI$subpop_label = sub("newI_", "", newI$subpop)
newI$subpop_label = factor(newI$subpop_label, levels = subpop)

ggplot(data = newI) +
  geom_line(mapping = aes(x = date, y = inc_median, color = subpop_label))







