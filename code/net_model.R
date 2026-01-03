rm(list=ls())

library(tidyverse)
library(abind)
library(tgp)
library(pryr)
library(foreach)
library(doParallel)

n_core = 8
registerDoParallel(n_core)

dir_data = "measles_migrant_mobility/data/"
dir_rslt = "measles_migrant_mobility/rslt/"

net_mode = "trav_mig_net" # iso_net, trav_net, mig_net, trav_mig_net
batch = 1

runtime_strt = Sys.time()



# meta --------------------
## simulation period ---------------
t_seq = seq(ymd(20040901), ymd(20141231), 1)
t_seq_wk = seq(ymd(20050108), ymd(20141231), 7)
t_seq_month = seq(ymd(20050201), ymd(20150101), by = "month") - 1

## school calender (turned off) ---------------
t_sch = seq(ymd(20040901), ymd(20150901), by = "year")
names(t_sch) = year(t_sch)

t_cny = ymd(c(20040122, 20050209, 20060129, 20070218, 20080207, 20090126, 20100214, 20110203, 20120123, 20130210, 20140131, 20150219))
names(t_cny) = year(t_cny)

t_seq_sch = ymd()
for (t in 2004:2014) {
  t_seq_sch = c(t_seq_sch,
                seq(t_sch[as.character(t)], t_cny[as.character(t + 1)] - 7, 1), # fall semester
                seq(t_cny[as.character(t + 1)] + 15, t_sch[as.character(t + 1)] - 71, 1)) # spring semester
}

t_seq_schlgl = ifelse(t_seq %in% t_seq_sch, 1, -1)
names(t_seq_schlgl) = as.character(t_seq)

## met ---------------
met = read.csv(paste0(dir_data, "met.csv"))

met$SH = met$SH*10^3
met_SH = c(SH_max = 18.5, SH_min = 3.1, SH_mid = 10.8)
met_T = c(T_cut = 22.1, T_diff = 2.7)

met$SH = pmax(met$SH, met_SH["SH_min"])
met$SH = pmin(met$SH, met_SH["SH_max"])

met_T["T_min"] = met_T["T_cut"] - met_T["T_diff"]
met$Temp = pmax(met$Temp, met_T["T_min"])
met$T_cut = met_T["T_cut"]

## census ---------------
### census ----------
census = read.csv(paste0(dir_data, "census.csv"))

### birth and death rates ----------
census_demo = census %>%
  group_by(prov_code) %>%
  mutate(pop_strt = lag(pop_tot_yrend_statgov),
         pop_mean = (pop_strt + pop_tot_yrend_statgov)/2) %>%
  as.data.frame()

census_demo$br = round(census_demo$pop_mean*10^4*census_demo$brrt_tot_yrmidadj_statgov*10^-3)
census_demo$de = round(census_demo$pop_mean*10^4*census_demo$dert_tot_yrmidadj_statgov*10^-3)

census_demo$brrt = ((census_demo$pop_strt*10^4 + census_demo$br)/(census_demo$pop_strt*10^4))^
  (1/(365 + leap_year(census_demo$yr))) - 1

census_demo$dert = 1 - ((census_demo$pop_strt*10^4 - census_demo$de)/(census_demo$pop_strt*10^4))^
  (1/(365 + leap_year(census_demo$yr)))

### effective vaccination rate ----------
vacrt = read.csv(paste0(dir_data, "vacrt_eff.csv"))

### census of rural-urban migrant workers ----------
census_mig_pair = read.csv(paste0(dir_data, "smry_mig_pair.csv"))
census_mig_distri = read.csv(paste0(dir_data, "mig_distri.csv"))

census_mig_distri = merge(census_mig_pair, census_mig_distri, by = c("resid_curr", "resid_regis"), all.x = TRUE)
census_mig_distri = census_mig_distri[order(census_mig_distri$yr, census_mig_distri$resid_curr, -census_mig_distri$pop_mig),
                                      c("yr", "resid_curr", "resid_regis", "pop_mig")]

### prov_code ----------
prov_code = read.csv(paste0(dir_data, "prov_code.csv"))

census_mig_a = census_mig_pair %>% # a is the number of migrant subpopulations received by a prov
  count(resid_curr) %>%
  rename(prov_name = resid_curr, a = n) %>%
  as.data.frame()

census_mig_b = census_mig_pair %>% # b is the number of migrant subpopulations provided by a prov
  count(resid_regis) %>%
  rename(prov_name = resid_regis, b = n) %>%
  as.data.frame()

prov_code = Reduce(function(x, y) merge(x, y, by = c("prov_name"), all.x = TRUE), list(prov_code, census_mig_a, census_mig_b))
prov_code = prov_code %>%
  mutate_at(vars(a, b), ~ replace_na(.x, 0)) %>%
  arrange(prov_code) %>%
  as.data.frame()

### census of travelers ----------
census_trav = read.csv(paste0(dir_data, "pop_trav.csv"))
census_trav$pop_trav = round(census_trav$pop_trav*10^8*0.3436949) # inter provincial traveling takes up 0.3436949 of all traveling

## flow ---------------
flow_trav = readRDS(paste0(dir_data, "flow_trav_synthetic.rds"))

### create flow of other workers for population alignment ----------
delta_pop_mig = census_mig_distri %>%
  group_by(yr, resid_curr) %>%
  summarise(pop_mig = sum(pop_mig)) %>%
  as.data.frame()

delta_pop_mig = delta_pop_mig %>%
  group_by(resid_curr) %>%
  arrange(yr, .by_group = TRUE) %>%
  mutate(delta_pop_mig = pop_mig - lag(pop_mig)) %>%
  select(-pop_mig) %>%
  as.data.frame()

colnames(delta_pop_mig)[colnames(delta_pop_mig) == "resid_curr"] = "prov_name"

delta_pop_mig = merge(delta_pop_mig, 
                      expand.grid(prov_name = unique(census$prov_name), yr = 2004:2014, stringsAsFactors = FALSE), 
                      by = c("yr", "prov_name"), all = TRUE)
delta_pop_mig$delta_pop_mig[is.na(delta_pop_mig$delta_pop_mig)] = 0

census_demo = merge(census_demo, delta_pop_mig, by = c("prov_name", "yr"), all.x = TRUE)

# pop_strt + br - de + delta_pop_mig + delta_pop_migo = pop_tot_yrend_statgov
census_demo$delta_pop_migo = census_demo$pop_tot_yrend_statgov*10^4 - census_demo$pop_strt*10^4 - census_demo$br + census_demo$de - census_demo$delta_pop_mig

flow_migo_in = flow_trav %>%
  group_by(date, yr, holiday, t, prov_arr) %>%
  summarise(vol_all_sep = sum(vol_all_sep)) %>%
  as.data.frame()

flow_migo_out = flow_trav %>%
  group_by(date, yr, holiday, t, prov_dep) %>%
  summarise(vol_all_sep = sum(vol_all_sep)) %>%
  as.data.frame()

tbl = expand.grid(prov_name = prov_code$prov_name,
                  yr = 2004:2014,
                  stringsAsFactors = FALSE)

flow_migo = list()
for (i in 1:nrow(tbl)) {
  p = tbl[i, "prov_name"]
  yr_p = tbl[i, "yr"]
  
  if (net_mode %in% c("iso_net", "trav_net")) {
    delta_pop = rowSums(census_demo[(census_demo$prov_name == p) & (census_demo$yr == yr_p), c("delta_pop_mig", "delta_pop_migo")])
  } else if (net_mode %in% c("mig_net", "trav_mig_net")) { # pop_migo = pop_migo
    delta_pop = census_demo[(census_demo$prov_name == p) & (census_demo$yr == yr_p), c("delta_pop_migo")]
  }

  if (delta_pop >= 0) {
    flow_migo_in_p = flow_migo_in[(flow_migo_in$prov_arr == p) & (year(flow_migo_in$date) == yr_p),]
    flow_migo_in_p$vol_all_sep = round(delta_pop/sum(flow_migo_in_p$vol_all_sep)*flow_migo_in_p$vol_all_sep)
    colnames(flow_migo_in_p)[colnames(flow_migo_in_p) == "prov_arr"] = "prov_name"
    
    flow_migo = c(flow_migo, list(flow_migo_in_p))
  } else if (delta_pop < 0) {
    flow_migo_out_p = flow_migo_out[(flow_migo_out$prov_dep == p) & (year(flow_migo_out$date) == yr_p),]
    flow_migo_out_p$vol_all_sep = round(delta_pop/sum(flow_migo_out_p$vol_all_sep)*flow_migo_out_p$vol_all_sep)
    colnames(flow_migo_out_p)[colnames(flow_migo_out_p) == "prov_dep"] = "prov_name"
    
    flow_migo = c(flow_migo, list(flow_migo_out_p))
  }
}
flow_migo = do.call(rbind, flow_migo)



# SEIR setting --------------------
## cluster ---------------
cluster = prov_code$prov_name

## state variables and parameters ---------------
J = 3000

### Sprop ----------
bd_Sprop_init = read.csv(paste0(dir_data, "Sprop_init.csv"))

### other states ----------
clim_model = "aht" # aht sin
if (clim_model == "aht") {
  param_clim = c("R0_min", "R0_diff")
} else if (clim_model == "sin") {
  param_clim = c("R0_A", "R0_phi")
}

state = c("S", "E", "I", "newI", "N",
          param_clim, "R0_cont", "beta1", "beta2", "beta3", "m1", "m2", "m3", "rho", "Z", "D", "A_sch")
var = c("S", "E", "I", "newI", "N")
param = setdiff(state, var)

bd_param_init = data.frame(param = param[!(param %in% c("rho", param_clim))],
                           lower = c(8, 1, 0, 0, 0.85, 0.4, 0.4, 7, 4, 0),
                           upper = c(20, 1, 0.4, 0.2, 1, 1, 1, 9, 6, 0))

if (clim_model == "aht") {
  bd_param_clim_init = data.frame(param = param_clim, lower = c(6, 4), upper = c(15, 18))
} else if (clim_model == "sin") {
  bd_param_clim_init = data.frame(param = param_clim, lower = c(0.01, 23 - 60), upper = c(0.9, 23 + 150))
}

bd_param_init = rbind(bd_param_clim_init, bd_param_init)

bd_rho_init = read.csv(paste0(dir_data, "rprt_bd.csv"))

### adaptive beta ----------
f_beta3_t2 = flow_trav[flow_trav$t %in% c("t2"),]
prov_beta3_t2 = census_mig_pair
colnames(prov_beta3_t2)[match(c("resid_curr", "resid_regis"), colnames(prov_beta3_t2))] = c("prov_dep", "prov_arr")

f_beta3_t2 = f_beta3_t2 %>% 
  semi_join(prov_beta3_t2, by = c("prov_dep", "prov_arr")) %>% 
  as.data.frame()

f_beta3_t2 = f_beta3_t2 %>%
  group_by(prov_dep, prov_arr) %>%
  mutate(date_diff = c(0, diff(date)),
         grp = cumsum(date_diff != 1)) %>%
  group_by(prov_dep, prov_arr, grp) %>%
  mutate(f = cumsum(lag(vol_all_sep, default = 0))/sum(vol_all_sep)) %>%
  select(date, grp, prov_dep, prov_arr, f) %>%
  as.data.frame()

f_beta3_t4 = flow_trav[flow_trav$t %in% c("t4"),]
prov_beta3_t4 = census_mig_pair
colnames(prov_beta3_t4)[match(c("resid_curr", "resid_regis"), colnames(prov_beta3_t4))] = c("prov_arr", "prov_dep")

f_beta3_t4 = f_beta3_t4 %>% 
  semi_join(prov_beta3_t4, by = c("prov_dep", "prov_arr")) %>% 
  as.data.frame()

f_beta3_t4 = f_beta3_t4 %>%
  group_by(prov_dep, prov_arr) %>%
  mutate(date_diff = c(0, diff(date)),
         grp = cumsum(date_diff != 1)) %>%
  group_by(prov_dep, prov_arr, grp) %>%
  mutate(f = 1 - cumsum(lag(vol_all_sep, default = 0))/sum(vol_all_sep)) %>%
  select(date, grp, prov_dep, prov_arr, f) %>%
  as.data.frame()

f_beta2_adapt = census_mig_distri
colnames(f_beta2_adapt)[colnames(f_beta2_adapt) == "resid_curr"] = "prov_name"
f_beta2_adapt = f_beta2_adapt %>%
  group_by(yr, prov_name) %>%
  summarise(pop_mig = sum(pop_mig)) %>%
  as.data.frame()

f_beta2_adapt = merge(f_beta2_adapt, census[,c("yr", "prov_name", "pop_tot_yrend_statgov")], by = c("yr", "prov_name"), all.x = TRUE)
f_beta2_adapt$pop_tot_yrend_statgov = f_beta2_adapt$pop_tot_yrend_statgov*10000
f_beta2_adapt = f_beta2_adapt %>%
  group_by(prov_name) %>%
  summarise(mig_pop_prop = mean(pop_mig/pop_tot_yrend_statgov)) %>%
  as.data.frame()
f_beta2_adapt$f1 = f_beta2_adapt$mig_pop_prop/(1 - f_beta2_adapt$mig_pop_prop)
f_beta2_adapt$f = f_beta2_adapt$f1/max(f_beta2_adapt$f1)

f_beta3_adapt = census_mig_distri
colnames(f_beta3_adapt)[colnames(f_beta3_adapt) == "resid_regis"] = "prov_name"
f_beta3_adapt = f_beta3_adapt %>%
  group_by(yr, prov_name) %>%
  summarise(pop_mig = sum(pop_mig)) %>%
  as.data.frame()

f_beta3_adapt$pop_mig = f_beta3_adapt$pop_mig*0.8
f_beta3_adapt = merge(f_beta3_adapt, census[,c("yr", "prov_name", "pop_tot_yrend_statgov")], by = c("yr", "prov_name"), all.x = TRUE)
f_beta3_adapt$pop_tot_yrend_statgov = f_beta3_adapt$pop_tot_yrend_statgov*10000
f_beta3_adapt = f_beta3_adapt %>%
  group_by(prov_name) %>%
  summarise(mig_pop_prop = mean(pop_mig/pop_tot_yrend_statgov)) %>%
  as.data.frame()
f_beta3_adapt$f1 = f_beta3_adapt$mig_pop_prop/(1 - f_beta3_adapt$mig_pop_prop)
f_beta3_adapt$f = f_beta3_adapt$f1/max(f_beta3_adapt$f1)

### Sprop for SIA ----------
Sprop_SIA = read.csv(paste0(dir_data, "Sprop_SIA.csv"))
census_agestr = read.csv(paste0(dir_data, "census_agestr.csv"))

Sprop_SIA$yr = year(Sprop_SIA$date)
Sprop_SIA = merge(Sprop_SIA, census_agestr[,c("prov_name", "yr", "age", "pop_prop")], by = c("prov_name", "yr", "age"), all.x = TRUE)

Sprop_SIA = Sprop_SIA %>%
  group_by(prov_name) %>%
  mutate(Sprop = sum(Sprop_mean*pop_prop)) %>%
  as.data.frame()

Sprop_SIA$f_Sprop = Sprop_SIA$Sprop_mean/Sprop_SIA$Sprop
Sprop_SIA = Sprop_SIA[Sprop_SIA$age == "1_14",]

t_SIA_strt = ymd(20100911)
t_SIA_end = ymd(20100920)

### mig_agestr for SIA ----------
# adjust mig_agestr by adding 0_15 age group
mig_agestr = read.csv(paste0(dir_data, "mig_agestr.csv")) # 农民工监测调查报告

mig_agestr_21_50 = mig_agestr %>%
  group_by(yr) %>%
  summarise(pop_prop = sum(pop_prop[age %in% c("21_30", "31_40", "41_50")])/sum(pop_prop)) %>%
  as.data.frame()

r_0_15_to_21_50 = 48.4/((704.5 - 48.4)*mig_agestr_21_50[mig_agestr_21_50$yr == 2010, "pop_prop"]) # assume this ratio is constant over years; 2011北京统计年鉴 3-10 常住外来人口 (2010年)

mig_agestr_0_15 = mig_agestr_21_50 %>%
  mutate(pop_prop = r_0_15_to_21_50*pop_prop,
         age = "0_15") %>%
  as.data.frame()

mig_agestr = rbind(mig_agestr, mig_agestr_0_15) %>%
  group_by(yr) %>%
  mutate(pop_prop = pop_prop/sum(pop_prop)) %>%
  arrange(yr, age) %>%
  as.data.frame()

mig_agestr1 = mig_agestr %>%
  group_by(age) %>%
  do({
    fit = lm(pop_prop ~ yr, data = .)
    df = data.frame(yr = 2005:2007)
    data.frame(yr = df$yr, pop_prop = predict(fit, newdata = df))
  }) %>%
  as.data.frame()

mig_agestr = rbind(mig_agestr, mig_agestr1) %>%
  arrange(yr, age) %>%
  as.data.frame()

## qf ---------------
census_trav_out_night = read.csv(paste0(dir_data, "pop_trav_out_night.csv"))



# SEIR model --------------------
## initialization ---------------
### subpop ----------
smry_subpop = list()
for (p in cluster) {
  subpop_a = census_mig_pair[census_mig_pair$resid_curr == p, "resid_regis"]
  subpop_a = subpop_a[subpop_a %in% cluster]

  if (length(subpop_a) >= 1) {
    subpop_a = paste0(p, "_", subpop_a)
  } else if (length(subpop_a) == 0) {
    subpop_a = NULL
  }
  
  if (net_mode %in% c("iso_net", "trav_net")) subpop_a = NULL
  n_subpop_a = length(subpop_a)
  
  subpop_b = census_mig_pair[census_mig_pair$resid_regis == p, "resid_curr"]
  subpop_b = subpop_b[subpop_b %in% cluster]
  
  if (length(subpop_b) >= 1) {
    subpop_b = paste0(subpop_b, "_", p, "_r")
  } else if (length(subpop_b) == 0) {
    subpop_b = NULL
  }
  
  if (net_mode %in% c("iso_net", "trav_net")) subpop_b = NULL
  n_subpop_b = length(subpop_b)
  
  subpop = c(p, subpop_a, subpop_b)
  n_subpop = length(subpop)
  
  smry_subpop[[p]] = list(subpop_a = subpop_a, subpop_b = subpop_b, subpop = subpop,
                          n_subpop_a = n_subpop_a, n_subpop_b = n_subpop_b, n_subpop = n_subpop)
}

### state swarm initialization ----------
inc_init = read.csv(paste0(dir_data, "inc_init.csv"))
state_swarm_cluster = list()
for (p in cluster) {
  p_code = prov_code[prov_code$prov_name == p, "prov_code"]

  subpop_a = smry_subpop[[p]][["subpop_a"]]
  subpop_b = smry_subpop[[p]][["subpop_b"]]
  subpop = smry_subpop[[p]][["subpop"]]
  n_subpop_a = smry_subpop[[p]][["n_subpop_a"]]
  n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
  n_subpop = smry_subpop[[p]][["n_subpop"]]

  a = gsub(paste0(p, "_"), "", subpop_a)
  b = gsub(paste0("_", p, "_r"), "", subpop_b)

  state_swarm = matrix(0, nrow = n_subpop*length(var) + length(param), ncol = J, 
                       dimnames = list(c(c(outer(var, subpop, FUN = paste, sep = "_")), param)))

  # initialize N
  N_init_a = census_mig_distri[(census_mig_distri$resid_curr == p) & (census_mig_distri$resid_regis %in% a) & (census_mig_distri$yr == 2004),]
  N_init_a = N_init_a[match(a, N_init_a$resid_regis), "pop_mig"]

  if (n_subpop_b >= 1) {
    N_init_b = rep(0, n_subpop_b)
  } else if (n_subpop_b == 0) {
    N_init_b = NULL
  }

  N_p_init = census[(census$prov_name == p) & (census$yr == 2004), "pop_tot_yrend_statgov"]*10^4 - sum(N_init_a)
  N_init = c(N_p_init, N_init_a, N_init_b)

  state_swarm[paste0("N_", subpop),] = N_init
  
  # initialize S
  mode_init_S = "runif"
  bd_Sprop_init_mean = bd_Sprop_init[match(c(p, a), bd_Sprop_init$prov_name), "Sprop_mean"]
  bd_Sprop_init_sprd = 0.0075
  
  if (mode_init_S %in% c("rnorm")) {
    Sprop_pa_init = rnorm(n = (1 + n_subpop_a)*J, 
                          mean = bd_Sprop_init_mean, sd = bd_Sprop_init_sprd)
  } else if (mode_init_S %in% c("runif")) {
    Sprop_pa_init = runif(n = (1 + n_subpop_a)*J, 
                          min = bd_Sprop_init_mean - bd_Sprop_init_sprd*2, 
                          max = bd_Sprop_init_mean + bd_Sprop_init_sprd*2)
  }

  Sprop_pa_init = matrix(Sprop_pa_init, ncol = J, byrow = FALSE)
  
  Sprop_init_adj = 1
  if ((Sprop_init_adj == 1) && (n_subpop_a >= 1)) {
    id_curr = which(c(p, a) %in% census_mig_pair$resid_curr)
    n_curr = length(id_curr)

    repeat {
      Sprop_pa_init_rank = apply(Sprop_pa_init, 2, rank)
      col_redraw = which(apply(Sprop_pa_init_rank[id_curr,, drop = FALSE], 2, function(x) !all(x <= n_curr)))

      if (length(col_redraw) == 0) {
        break
      } else if (length(col_redraw) >= 1) {
        if (mode_init_S %in% c("rnorm")) {
          Sprop_pa_init[,col_redraw] = rnorm(n = (1 + n_subpop_a)*length(col_redraw), 
                                             mean = bd_Sprop_init_mean, sd = bd_Sprop_init_sprd)
        } else if (mode_init_S %in% c("runif")) {
          Sprop_pa_init[,col_redraw] = runif(n = (1 + n_subpop_a)*length(col_redraw), 
                                             min = bd_Sprop_init_mean - bd_Sprop_init_sprd*2, 
                                             max = bd_Sprop_init_mean + bd_Sprop_init_sprd*2)
        }
      }
    }
  }
  
  Sprop_pa_init = pmax(Sprop_pa_init, 0.02)
  Sprop_pa_init = pmin(Sprop_pa_init, 0.1)
  
  Sprop_b_init = matrix(0, nrow = n_subpop_b, ncol = J)
  Sprop_init = rbind(Sprop_pa_init, Sprop_b_init)
  
  state_swarm[paste0("S_", subpop),] = round(Sprop_init*state_swarm[paste0("N_", subpop),])

  # initialize param
  bd_param_init_p = rbind(bd_param_init,
                          data.frame(param = "rho",
                                     lower = bd_rho_init[bd_rho_init$prov_code == p_code, "lower"],
                                     upper = bd_rho_init[bd_rho_init$prov_code == p_code, "upper"]))
  
  rownames(bd_param_init_p) = bd_param_init_p$param
  bd_param_init_p = bd_param_init_p[,!(colnames(bd_param_init_p) %in% c("param"))]
  bd_param_init_p = as.matrix(bd_param_init_p)
  
  param_bd = setdiff(param, c("m2", "m3"))
  state_swarm[param_bd,] = t(lhs(n = J, rect = bd_param_init_p[param_bd,]))

  if (clim_model == "aht") {
    repeat { # bd for R0_max
      col_redraw = which(colSums(state_swarm[c("R0_min", "R0_diff"),]) > 22)
      
      if (length(col_redraw) == 0) {
        break
      } else if (length(col_redraw) >= 1) {
        state_swarm[c("R0_min", "R0_diff"), col_redraw] = 
          t(lhs(n = length(col_redraw), rect = bd_param_init_p[c("R0_min", "R0_diff"),]))
      }
    }
  }

  if ((n_subpop_a >= 1) && (n_subpop_b >= 1)) {
    state_swarm["m2",] = runif(J, min = bd_param_init_p["m2", "lower"], max = state_swarm["m1",])
    state_swarm["m3",] = runif(J, min = state_swarm["m2",], max = bd_param_init_p["m3", "upper"])
  } else {
    state_swarm["m2",] = runif(J, min = bd_param_init_p["m2", "lower"], max = state_swarm["m1",])
    state_swarm["m3",] = runif(J, min = bd_param_init_p["m3", "lower"], max = bd_param_init_p["m3", "upper"])
  }
  
  if (p %in% f_beta2_adapt$prov_name) {
    state_swarm["beta2",] = f_beta2_adapt[f_beta2_adapt$prov_name == p, "f"]*state_swarm["beta2",]
  }
  
  if (p %in% f_beta3_adapt$prov_name) {
    state_swarm["beta3",] = f_beta3_adapt[f_beta3_adapt$prov_name == p, "f"]*state_swarm["beta3",]
  }
  
  # initialize E, I, and newI
  inc_init_p = inc_init[inc_init$prov_code == p_code, "inc"]
  inc_init_p = inc_init_p + 3

  preval = inc_init_p/state_swarm["rho",]/7*14/colSums(state_swarm[grepl("^N_", rownames(state_swarm)),, drop = FALSE])
  preval_p = 2*preval
  preval_ab = 2*preval

  EI_init = state_swarm[grepl("^N_", rownames(state_swarm)),, drop = FALSE]
  preval = matrix(c(preval_p, rep(preval_ab, nrow(EI_init) - 1)), ncol = J, nrow = nrow(EI_init), byrow = TRUE)
  EI_init = EI_init*preval

  r_EI_p = 8/5
  r_EI_ab = 8/5
  r_EI = matrix(c(r_EI_p, rep(r_EI_ab, nrow(EI_init) - 1)), ncol = J, nrow = nrow(EI_init), byrow = FALSE)
  
  mode_init_EI = "rnbinom"
  if (mode_init_EI == "rnorm") {
    E_init = pmax(round(rnorm(n = length(EI_init), mean = EI_init, sd = EI_init/3)), 0)
    I_init = pmax(round(rnorm(n = length(EI_init), mean = EI_init/r_EI, sd = EI_init/r_EI/3)), 0)
  } else if (mode_init_EI == "rpois") {
    E_init = rpois(n = length(EI_init), lambda = EI_init)
    I_init = rpois(n = length(EI_init), lambda = EI_init/r_EI)
  } else if (mode_init_EI == "rnbinom") {
    if (p %in% prov_code[(prov_code$a == 0) & (prov_code$b >= 1), "prov_name"]) {
      size = matrix(c(1/6, rep(1/3, nrow(EI_init) - 1)), ncol = J, nrow = nrow(EI_init), byrow = FALSE)
    } else {
      size = matrix(c(1/3, rep(1/3, nrow(EI_init) - 1)), ncol = J, nrow = nrow(EI_init), byrow = FALSE)
    }
    
    E_init = suppressWarnings(rnbinom(n = length(EI_init), mu = EI_init, size = size*EI_init))
    I_init = suppressWarnings(rnbinom(n = length(EI_init), mu = EI_init/r_EI, size = size*EI_init/r_EI))
    E_init[is.na(E_init)] = 0
    I_init[is.na(I_init)] = 0
  }

  state_swarm[grepl("^E_", rownames(state_swarm)),] = E_init
  state_swarm[grepl("^I_", rownames(state_swarm)),] = I_init
  
  state_swarm[paste0("E_", p),] = pmax(state_swarm[paste0("E_", p),], ifelse(p %in% c("Tianjin"), 3, 7))
  state_swarm[paste0("I_", p),] = pmax(state_swarm[paste0("I_", p),], ifelse(p %in% c("Tianjin"), 1, 3))
  
  if (n_subpop_a >= 1) {
    E_subpop_a = state_swarm[paste0("E_", subpop_a),]
    
    zero_mask = (E_subpop_a == 0)
    rand_mat = matrix(runif(length(E_subpop_a)), nrow = nrow(E_subpop_a))
    flip_mask = zero_mask & (rand_mat <= ifelse(p %in% c("Tianjin"), 0.12, 1))
    
    E_subpop_a[flip_mask] = 1
    state_swarm[paste0("E_", subpop_a),] = E_subpop_a
  }

  state_swarm[paste0("newI_", subpop),] = 0
  
  state_swarm_cluster[[p]] = state_swarm
}

# record initial state swarm
state_swarm_rslt_init = state_swarm_cluster

runtime_mid = as.numeric(difftime(Sys.time(), runtime_strt, units = "mins"))

## simulation ---------------
# t_seq
tbl_holiday = unique(flow_trav[,c("date", "yr", "holiday", "t")])
t_seq = seq(tbl_holiday[which((tbl_holiday$holiday %in% c("cny")) & (tbl_holiday$yr %in% c(2005)))[1] - 42 - 1, "date"], 
            ymd(20141231), 1)

tid_end = which(t_seq == ymd(20061231)) # tid_end = which(t_seq == ymd(20141231))

# tbl for the date to calculate K (NGM)
tbl_holiday_K = tbl_holiday[tbl_holiday$date %in% t_seq[1:tid_end],] %>%
  mutate(grp = cumsum(t != lag(t, default = first(t))) + 1) %>% 
  group_by(grp) %>%
  mutate(rid = ((row_number() - 1) %% 7) + 1, 
         t_K = case_when(t %in% c("t2", "t3", "t4") ~ 1,
                         rid == 1 ~ 1,
                         TRUE ~ 0)) %>%
  ungroup() %>%
  select(-grp, -rid) %>%
  as.data.frame()

# tbl for the date of t5 (unemp migrant net)
tbl_holiday_t5 = tbl_holiday[tbl_holiday$date %in% t_seq[1:tid_end],] %>%
  filter(t == "t3" & lead(t) == "t4") %>%
  pull(date)
tbl_holiday_t5 = mapply(seq, from = tbl_holiday_t5 + 8*7 + 1, to = tbl_holiday_t5 + 14*7, by = 1, SIMPLIFY = FALSE)
tbl_holiday_t5 = do.call(c, tbl_holiday_t5)
tbl_holiday_t5 = tbl_holiday[tbl_holiday$date %in% t_seq[1:tid_end],] %>% 
  mutate(t_t5 = ifelse(date %in% tbl_holiday_t5, 1, 0)) %>%
  as.data.frame()

res_t_seq = "month" # wk, month

if (res_t_seq == "wk") {
  t_seq_res = c(t_seq_wk[1] - 7, t_seq_wk)
  t_seq_res = t_seq_res[t_seq_res <= t_seq[tid_end]]
} else if (res_t_seq == "month") {
  t_seq_res = c(t_seq_month[1] - 31, t_seq_month)
  t_seq_res = t_seq_res[t_seq_res <= t_seq[tid_end]]
}

# state_swarm_rslt
state_swarm_rslt = list()
state_swarm_rslt_tmp = list()

# reuse f_cont and f_clim
f_cont_cluster = list()
f_clim_cluster = list()

for (t in t_seq[1:tid_end]) {
  t = as.Date(t, origin = "1970-01-01")
  print(t)
  
  t_curr = tbl_holiday[tbl_holiday$date == t, "t"]
  t_prev = tbl_holiday[tbl_holiday$date == (t - 1), "t"]
  t_next = tbl_holiday[tbl_holiday$date == (t + 1), "t"]

  ### migrant flow adjustment factor for t2 ----------
  if ((t_curr == "t2") && (t_prev == "t1") && (net_mode %in% c("mig_net", "trav_mig_net"))) {
    tbl_holiday_t2 = tbl_holiday %>%
      filter((date >= !!t) & (t == "t2")) %>% 
      mutate(date_diff = c(0, diff(date)),
             grp = cumsum(date_diff != 1)) %>% 
      filter(grp == 1) %>%
      as.data.frame()
    
    s = 0.5
    
    f_mig_t2 = list()
    for (p in cluster) {
      p_code = prov_code[prov_code$prov_name == p, "prov_code"]
      
      subpop_a = smry_subpop[[p]][["subpop_a"]]
      subpop_b = smry_subpop[[p]][["subpop_b"]]
      subpop = smry_subpop[[p]][["subpop"]]
      n_subpop_a = smry_subpop[[p]][["n_subpop_a"]]
      n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
      n_subpop = smry_subpop[[p]][["n_subpop"]]
      
      state_swarm = state_swarm_cluster[[p]]
      
      f_a_mig_out = list()
      for (a in subpop_a) {
        a_arr = strsplit(a, "_")[[1]][2]
        flow_mig_out = flow_trav[(flow_trav$date %in% tbl_holiday_t2$date) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr == a_arr),]
        f = (1 - s)*state_swarm[paste0("N_", a),]/sum(flow_mig_out$vol_all_sep)
        
        f_a_mig_out = c(f_a_mig_out, setNames(list(f), a))
      }
      
      f_b_mig_in = list()
      for (b in subpop_b) {
        b_dep = strsplit(b, "_")[[1]][1]
        flow_mig_in = flow_trav[(flow_trav$date %in% tbl_holiday_t2$date) & (flow_trav$prov_dep == b_dep) & (flow_trav$prov_arr == p),]
        state_swarm_b_dep = state_swarm_cluster[[b_dep]]
        b_counter = paste0(b_dep, "_", p)
        f = (1 - s)*state_swarm_b_dep[paste0("N_", b_counter),]/sum(flow_mig_in$vol_all_sep)
        
        f_b_mig_in = c(f_b_mig_in, setNames(list(f), b))
      }
      
      f_p_mig = list(f_a_mig_out = f_a_mig_out, f_b_mig_in = f_b_mig_in)
      f_mig_t2[[p]] = f_p_mig
    }
  }

  ### migrant flow adjustment factor for t4 ----------
  if ((t_curr == "t4") && (t_prev == "t3") && (net_mode %in% c("mig_net", "trav_mig_net"))) {
    L = 3
    e = 0.7
    
    f_mig_t4 = list()
    for (p in cluster) {
      p_code = prov_code[prov_code$prov_name == p, "prov_code"]
      
      subpop_a = smry_subpop[[p]][["subpop_a"]]
      subpop_b = smry_subpop[[p]][["subpop_b"]]
      subpop = smry_subpop[[p]][["subpop"]]
      n_subpop_a = smry_subpop[[p]][["n_subpop_a"]]
      n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
      n_subpop = smry_subpop[[p]][["n_subpop"]]
      
      state_swarm = state_swarm_cluster[[p]]
      
      f_a_mig_in = list()
      for (a in subpop_a) {
        a_dep = strsplit(a, "_")[[1]][2]
        state_swarm_a_dep = state_swarm_cluster[[a_dep]]
        a_counter = paste0(a, "_r")
        
        mig_census = census_mig_distri[(census_mig_distri$yr == year(t)) & (census_mig_distri$resid_curr == p) & (census_mig_distri$resid_regis == a_dep),]
        N_mig_total = mig_census$pop_mig/e
        N_mig_exist = (1 - 1/L)*(state_swarm[paste0("N_", a),] + state_swarm_a_dep[paste0("N_", a_counter),])
        
        flow_mig_in = flow_trav[(flow_trav$yr == year(t)) & (flow_trav$t == "t4") & (flow_trav$prov_dep == a_dep) & (flow_trav$prov_arr == p),]
        
        f_r = (N_mig_exist - state_swarm[paste0("N_", a),])/sum(flow_mig_in$vol_all_sep)
        f = (N_mig_total - N_mig_exist)/sum(flow_mig_in$vol_all_sep)
        
        if (all(f_r >= 0) && all(f < 0)) {
          f = rep(0, J)
          f_r = max((N_mig_total - state_swarm[paste0("N_", a),])/sum(flow_mig_in$vol_all_sep), 0)
        } else if (all(f_r < 0) && all(f >= 0)) {
          f_r = rep(0, J)
          f = max((N_mig_total - state_swarm[paste0("N_", a),])/sum(flow_mig_in$vol_all_sep), 0)
        } else if (all(f_r < 0) && all(f < 0)) {
          f = rep(0, J)
          f_r = rep(0, J)
        }
        
        f = list(f = f, f_r = f_r)
        f_a_mig_in = c(f_a_mig_in, setNames(list(f), a))
      }

      f_b_mig_out = list()
      for (b in subpop_b) {
        b_arr = strsplit(b, "_")[[1]][1]
        state_swarm_b_arr = state_swarm_cluster[[b_arr]]
        b_counter = paste0(b_arr, "_", p)
        
        mig_census = census_mig_distri[(census_mig_distri$yr == year(t)) & (census_mig_distri$resid_curr == b_arr) & (census_mig_distri$resid_regis == p),]
        N_mig_total = mig_census$pop_mig/e
        N_mig_exist = (1 - 1/L)*(state_swarm[paste0("N_", b),] + state_swarm_b_arr[paste0("N_", b_counter),])
        
        flow_mig_out = flow_trav[(flow_trav$yr == year(t)) & (flow_trav$t == "t4") & (flow_trav$prov_dep == p) & (flow_trav$prov_arr == b_arr),]
        
        f_r = (N_mig_exist - state_swarm_b_arr[paste0("N_", b_counter),])/sum(flow_mig_out$vol_all_sep)
        f = (N_mig_total - N_mig_exist)/sum(flow_mig_out$vol_all_sep)
        
        if (all(f_r >= 0) && all(f < 0)) {
          f = rep(0, J)
          f_r = max((N_mig_total - state_swarm_b_arr[paste0("N_", b_counter),])/sum(flow_mig_out$vol_all_sep), 0)
        } else if (all(f_r < 0) && all(f >= 0)) {
          f_r = rep(0, J)
          f = max((N_mig_total - state_swarm_b_arr[paste0("N_", b_counter),])/sum(flow_mig_out$vol_all_sep), 0)
        } else if (all(f_r < 0) && all(f < 0)) {
          f = rep(0, J)
          f_r = rep(0, J)
        }
        
        f = list(f = f, f_r = f_r)
        f_b_mig_out = c(f_b_mig_out, setNames(list(f), b))
      }
      
      f_p_mig = list(f_a_mig_in = f_a_mig_in, f_b_mig_out = f_b_mig_out)
      f_mig_t4[[p]] = f_p_mig
    }
  }

  ### migrant flow adjustment factor for t5 ----------
  if ((tbl_holiday_t5$t_t5[tbl_holiday_t5$date == t] == 1) &&
      (tbl_holiday_t5$t_t5[tbl_holiday_t5$date == (t - 1)] == 0) &&
      (net_mode %in% c("mig_net", "trav_mig_net"))) {
    f_mig_t5 = list()
    for (p in cluster) {
      p_code = prov_code[prov_code$prov_name == p, "prov_code"]
      
      subpop_a = smry_subpop[[p]][["subpop_a"]]
      subpop_b = smry_subpop[[p]][["subpop_b"]]
      subpop = smry_subpop[[p]][["subpop"]]
      n_subpop_a = smry_subpop[[p]][["n_subpop_a"]]
      n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
      n_subpop = smry_subpop[[p]][["n_subpop"]]
      
      state_swarm = state_swarm_cluster[[p]]
      
      f_a_mig_out = list()
      for (a in subpop_a) {
        a_arr = strsplit(a, "_")[[1]][2]
        flow_mig_out = flow_trav[(flow_trav$date %in% tbl_holiday_t5[(tbl_holiday_t5$yr == year(t)) & (tbl_holiday_t5$t_t5 == 1), "date"]) & 
                                   (flow_trav$prov_dep == p) & (flow_trav$prov_arr == a_arr),]
        f = (1 - e)*state_swarm[paste0("N_", a),]/sum(flow_mig_out$vol_all_sep)
        
        f_a_mig_out = c(f_a_mig_out, setNames(list(f), a))
      }
      
      f_b_mig_in = list()
      for (b in subpop_b) {
        b_dep = strsplit(b, "_")[[1]][1]
        flow_mig_in = flow_trav[(flow_trav$date %in% tbl_holiday_t5[(tbl_holiday_t5$yr == year(t)) & (tbl_holiday_t5$t_t5 == 1), "date"]) & 
                                  (flow_trav$prov_dep == b_dep) & (flow_trav$prov_arr == p),]
        state_swarm_b_dep = state_swarm_cluster[[b_dep]]
        b_counter = paste0(b_dep, "_", p)
        f = (1 - e)*state_swarm_b_dep[paste0("N_", b_counter),]/sum(flow_mig_in$vol_all_sep)
        
        f_b_mig_in = c(f_b_mig_in, setNames(list(f), b))
      }
      
      f_p_mig = list(f_a_mig_out = f_a_mig_out, f_b_mig_in = f_b_mig_in)
      f_mig_t5[[p]] = f_p_mig
    }
  }

  ### SIA ----------
  if (t == t_SIA_strt) {
    eff_SIA = 0.8

    dstate_swarm_SIA_cluster = list()
    for (p in cluster) {
      state_swarm = state_swarm_cluster[[p]]
      subpop_a = smry_subpop[[p]][["subpop_a"]]
      
      Sprop_1_14 = state_swarm[paste0("S_", c(p, subpop_a)),, drop = FALSE]/state_swarm[paste0("N_", c(p, subpop_a)),, drop = FALSE]
      Sprop_1_14 = Sprop_1_14*Sprop_SIA[Sprop_SIA$prov_name == p, "f_Sprop"]

      pop_prop_1_14_p = Sprop_SIA[Sprop_SIA$prov_name == p, "pop_prop"]
      pop_prop_1_14_a = mig_agestr[(mig_agestr$yr == year(t_SIA_strt)) & (mig_agestr$age == "0_15"), "pop_prop"]
      pop_prop_1_14 = c(pop_prop_1_14_p, rep(pop_prop_1_14_a, length(subpop_a)))
      
      Sprop_1_14_min = 1 - vacrt[(vacrt$prov_name == p) & (vacrt$yr == year(t_SIA_strt)), "vacrt_eff"]

      state_swarm_max = pmax(state_swarm[paste0("N_", c(p, subpop_a)),, drop = FALSE]*pop_prop_1_14*(Sprop_1_14 - Sprop_1_14_min), 0)/as.integer(t_SIA_end - t_SIA_strt + 1)
      
      state_swarm_expect = state_swarm[paste0("N_", c(p, subpop_a)),, drop = FALSE]*pop_prop_1_14*Sprop_1_14*eff_SIA/as.integer(t_SIA_end - t_SIA_strt + 1)
      state_swarm = pmin(state_swarm_expect, state_swarm_max)
      
      rownames(state_swarm) = sub("^N_", "S_", rownames(state_swarm))
      
      dstate_swarm_SIA_cluster[[p]] = -round(state_swarm)
    }
  }

  ### SEIR simulation ----------
  state_swarm_cluster_updt = foreach(p = cluster, .combine = "c") %dopar% {
    p_code = prov_code[prov_code$prov_name == p, "prov_code"]
    
    subpop_a = smry_subpop[[p]][["subpop_a"]]
    subpop_b = smry_subpop[[p]][["subpop_b"]]
    subpop = smry_subpop[[p]][["subpop"]]
    n_subpop_a = smry_subpop[[p]][["n_subpop_a"]]
    n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
    n_subpop = smry_subpop[[p]][["n_subpop"]]

    lambda = census_demo[(census_demo$yr == year(t)) & (census_demo$prov_name == p), "brrt"]
    mu = census_demo[(census_demo$yr == year(t)) & (census_demo$prov_name == p), "dert"]
    xi = vacrt[(vacrt$yr == year(t)) & (vacrt$prov_name == p), "vacrt_eff"]
    vol_migo = flow_migo[(flow_migo$prov_name == p) & (flow_migo$date == t), "vol_all_sep"]
    
    state_swarm = state_swarm_cluster[[p]]
    
    f_beta3 = 1
    if (n_subpop_b >= 1) {
      if (t_curr == "t2") {
        f_beta3 = mean(f_beta3_t2[f_beta3_t2$prov_arr == p & f_beta3_t2$date == t, "f"])
      } else if (t_curr == "t4") {
        f_beta3 = mean(f_beta3_t4[f_beta3_t4$prov_dep == p & f_beta3_t4$date == t, "f"])
      }
      
      f_beta3 = f_beta3/n_subpop_b
    }
    
    f_beta2 = 1
    if (n_subpop_a >= 1) {
      f_beta2 = f_beta2/n_subpop_a
    }
    
    #### beta -----
    # f_cont
    if (tbl_holiday_K[tbl_holiday_K$date == t, "t_K"] == 1) {
      K_eigenval_max = apply(state_swarm, MARGIN = 2, function(x) {
        beta = matrix(x["beta2"]*f_beta2, nrow = n_subpop, ncol = n_subpop)
        diag(beta) = x["beta1"]
        
        if ((1 + n_subpop_a + 1) <= n_subpop) {
          beta_id = (1 + n_subpop_a + 1):n_subpop
          beta[beta_id, 1] = x["beta3"]*f_beta3
          beta[1, beta_id] = x["beta3"]*f_beta3
          
          subbeta = beta[beta_id, beta_id, drop = FALSE]
          subbeta[row(subbeta) != col(subbeta)] = x["beta3"]*f_beta3
          beta[beta_id, beta_id] = subbeta
        }
        
        N1 = matrix(0, nrow = n_subpop, ncol = n_subpop)
        diag(N1) = x[paste0("N_", subpop)]
        
        N2 = matrix(0, nrow = n_subpop, ncol = n_subpop)
        diag(N2) = 1/x[paste0("N_", subpop)]
        
        K = N1%*%beta%*%N2*x["D"]
        K_id = !is.na(diag(K))
        K = K[K_id, K_id]
        
        eigenval = eigen(K)$values
        eigenval_max = max(Mod(eigenval))
      })
      
      f_cont_rec = state_swarm["R0_cont",]/K_eigenval_max
      f_cont = f_cont_rec
    } else {
      f_cont = f_cont_cluster[[p]]
    }

    # f_clim
    if (clim_model == "aht") {
      if (t == t_seq[1]) {
        met_yr = met[met$prov_code == p_code,]
        met_yr$param_parab = ifelse(met_yr$SH <= met_SH["SH_mid"], "left", "right")
        
        R0_min = state_swarm["R0_min",]
        R0_diff = state_swarm["R0_diff",]
        R0_max = R0_min + R0_diff
        
        ## left parabola
        SH_min_l = met_SH["SH_min"]
        SH_mid_l = met_SH["SH_mid"]
        SH_max_l = 2*SH_mid_l - SH_min_l
        
        Y_l = matrix(c(R0_max, R0_max, R0_min), byrow = T, nrow = 3) # 3*J
        X_l = matrix(c(SH_min_l^2, SH_max_l^2, SH_mid_l^2, SH_min_l, SH_max_l, SH_mid_l, 1, 1, 1), byrow = FALSE, ncol = 3) # 3*3
        param_parab_l = solve(X_l)%*%Y_l # 3*J
        param_parab_l = t(param_parab_l) # J*3
        
        ## right parabola
        SH_max_r = met_SH["SH_max"]
        SH_mid_r = met_SH["SH_mid"]
        SH_min_r = 2*SH_mid_r - SH_max_r
        
        Y_r = matrix(c(R0_max, R0_max, R0_min), byrow = T, nrow = 3)
        X_r = matrix(c(SH_min_r^2, SH_max_r^2, SH_mid_r^2, SH_min_r, SH_max_r, SH_mid_r, 1, 1, 1), byrow = FALSE, ncol = 3)
        param_parab_r = solve(X_r)%*%Y_r
        param_parab_r = t(param_parab_r)
        
        R0 = pmap(met_yr, function(param_parab, SH, T_cut, Temp, ...) {
          if (param_parab == "left") {
            R0_rec = param_parab_l %*% matrix(c(SH^2, SH, 1))
          } else if (param_parab == "right") {
            R0_rec = param_parab_r %*% matrix(c(SH^2, SH, 1))
          }
          R0_rec = R0_rec*(T_cut/Temp)^1.2
        })
        
        R0 = do.call(cbind, R0)
        
        f_clim_rec = R0/rowMeans(R0)
        f_clim = f_clim_rec[,yday(t)]
      } else {
        f_clim = f_clim_cluster[[p]][,yday(t)]
      }
    } else if (clim_model == "sin") {
      if (t == t_seq[1]) {
        R0_A = state_swarm["R0_A",]
        R0_phi = state_swarm["R0_phi",]
        
        f_clim_rec = sapply(1:366, function(x) 1 + R0_A*cos(2*pi/365.25*(x - R0_phi)))

        f_clim = f_clim_rec[,yday(t)]
      } else {
        f_clim = f_clim_cluster[[p]][,yday(t)]
      }
    }

    # f_sch
    t_schlgl = t_seq_schlgl[as.character(t)]
    f = 365.25/(365.25 + 225.25*state_swarm["A_sch",])
    f_sch = (1 + state_swarm["A_sch",]*t_schlgl)*f

    beta1 = f_cont*f_clim*f_sch*state_swarm["beta1",]
    beta2 = f_cont*f_clim*f_sch*state_swarm["beta2",]*f_beta2
    beta3 = f_cont*f_clim*f_sch*state_swarm["beta3",]*f_beta3
    
    f_track = matrix(c(f_cont, f_clim, f_sch), byrow = TRUE, nrow = 3, dimnames = list(c("f_cont_track", "f_clim_track", "f_sch_track")))
    state_swarm = rbind(state_swarm, f_track)
    rep = 10

    #### simulation for p -----
    # dS
    SE = beta1*state_swarm[paste0("I_", p),]^state_swarm["m1",]/state_swarm[paste0("N_", p),]
    for (a in subpop_a) {
      SE = SE + beta2*state_swarm[paste0("I_", a),]^state_swarm["m2",]/state_swarm[paste0("N_", a),]
    }
    for (b in subpop_b) {
      SE_b = beta3*state_swarm[paste0("I_", b),]^state_swarm["m3",]/state_swarm[paste0("N_", b),]
      SE_b[is.na(SE_b)] = 0
      SE = SE + SE_b
    }

    SE = SE*state_swarm[paste0("S_", p),]
    SE = pmin(rpois(n = J, lambda = SE), state_swarm[paste0("S_", p),])
    
    dS_brde = lambda*state_swarm[paste0("N_", p),]*(1 - xi) - mu*state_swarm[paste0("S_", p),]
    dS_migo = vol_migo*state_swarm[paste0("S_", p),]/state_swarm[paste0("N_", p),]
    alpha = rpois(n = J, lambda = 0.01)
    dS = -SE + dS_brde + dS_migo - alpha

    ## traveler net
    if (net_mode %in% c("trav_net", "trav_mig_net")) {
      flow_trav_in = flow_trav[(flow_trav$date == t) & (flow_trav$prov_arr == p) & (flow_trav$prov_dep %in% cluster),]
      flow_trav_out = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr %in% cluster),]
      
      dS_trav = numeric(J)
      for (c in cluster[cluster != p]) {
        vol_net_in_c = flow_trav_in[flow_trav_in$prov_dep == c, "vol_all_sep"] - flow_trav_out[flow_trav_out$prov_arr == c, "vol_all_sep"]

        vol_qf_c = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == c), "vol_trav_out_night"]
        vol_qf_p = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == p), "vol_trav_out_night"]
        qf_c = vol_qf_c/(vol_qf_c + vol_qf_p)
        
        state_swarm_c = state_swarm_cluster[[c]]
          
        dS_trav_c = (qf_c*state_swarm_c[paste0("S_", c),]/state_swarm_c[paste0("N_", c),] +
                       (1 - qf_c)*state_swarm[paste0("S_", p),]/state_swarm[paste0("N_", p),])*vol_net_in_c
        
        dS_trav = dS_trav + dS_trav_c
      }

      dS = dS + dS_trav
    }

    ## migrant net
    if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      b_arr = if (n_subpop_b >= 1) sapply(strsplit(subpop_b, "_"), function(x) x[[1]]) else NULL
      flow_mig_out = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr %in% b_arr),]
      
      vol = numeric(J)
      for (b in subpop_b) {
        f = f_mig_t4[[p]][["f_b_mig_out"]][[b]][["f"]]
        b_arr = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_out[flow_mig_out$prov_arr == b_arr, "vol_all_sep"]
        
        Sprop = state_swarm[paste0("S_", p),]/state_swarm[paste0("N_", p),]

        dS_mig_out = Sprop*vol_b
        dS = dS - dS_mig_out
        
        vol = vol + vol_b
      }
    }
    
    ## unemp migrant net
    if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      b_dep = if (n_subpop_b >= 1) sapply(strsplit(subpop_b, "_"), function(x) x[[1]]) else NULL
      flow_mig_in = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep %in% b_dep) & (flow_trav$prov_arr == p),]
      
      vol = numeric(J)
      for (b in subpop_b) {
        f = f_mig_t5[[p]][["f_b_mig_in"]][[b]]
        b_dep = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_in[flow_mig_in$prov_dep == b_dep, "vol_all_sep"]
        
        state_swarm_b_dep = state_swarm_cluster[[b_dep]]
        b_counter = paste0(b_dep, "_", p)

        Sprop = state_swarm_b_dep[paste0("S_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]
        
        dS_mig_in = Sprop*vol_b
        dS = dS + dS_mig_in
        
        vol = vol + vol_b
      }
    }

    # dE
    EI = state_swarm[paste0("E_", p),]/state_swarm["Z",]
    EI = pmin(rpois(n = J, lambda = EI), state_swarm[paste0("E_", p),])
    
    dE_brde = -mu*state_swarm[paste0("E_", p),]
    dE_migo = vol_migo*state_swarm[paste0("E_", p),]/state_swarm[paste0("N_", p),]
    dE = SE - EI + dE_brde + dE_migo + alpha
    
    ## traveler net
    if (net_mode %in% c("trav_net", "trav_mig_net")) {
      dE_trav = numeric(J)
      for (c in cluster[cluster != p]) {
        vol_net_in_c = flow_trav_in[flow_trav_in$prov_dep == c, "vol_all_sep"] - flow_trav_out[flow_trav_out$prov_arr == c, "vol_all_sep"]
        
        vol_qf_c = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == c), "vol_trav_out_night"]
        vol_qf_p = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == p), "vol_trav_out_night"]
        qf_c = vol_qf_c/(vol_qf_c + vol_qf_p)
        
        state_swarm_c = state_swarm_cluster[[c]]
        
        dE_trav_c = (qf_c*state_swarm_c[paste0("E_", c),]/state_swarm_c[paste0("N_", c),] +
                       (1 - qf_c)*state_swarm[paste0("E_", p),]/state_swarm[paste0("N_", p),])*vol_net_in_c
        
        dE_trav = dE_trav + sign(dE_trav_c)*rpois(n = J, lambda = abs(dE_trav_c))
      }
      
      dE = dE + dE_trav
    }
    
    ## migrant net
    if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      for (b in subpop_b) {
        f = f_mig_t4[[p]][["f_b_mig_out"]][[b]][["f"]]
        b_arr = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_out[flow_mig_out$prov_arr == b_arr, "vol_all_sep"]
        
        Eprop = state_swarm[paste0("E_", p),]/state_swarm[paste0("N_", p),]
        
        dE_mig_out = Eprop*vol_b
        dE_mig_out = rpois(n = rep*J, lambda = dE_mig_out)
        dE_mig_out = matrix(dE_mig_out, ncol = J, byrow = TRUE)
        dE_mig_out = colMeans(dE_mig_out)
        
        dE = dE - dE_mig_out
      }
    }
    
    ## unemp migrant net
    if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      for (b in subpop_b) {
        f = f_mig_t5[[p]][["f_b_mig_in"]][[b]]
        b_dep = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_in[flow_mig_in$prov_dep == b_dep, "vol_all_sep"]
        
        state_swarm_b_dep = state_swarm_cluster[[b_dep]]
        b_counter = paste0(b_dep, "_", p)

        Eprop = state_swarm_b_dep[paste0("E_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]
        
        dE_mig_in = Eprop*vol_b
        dE_mig_in = rpois(n = rep*J, lambda = dE_mig_in)
        dE_mig_in = matrix(dE_mig_in, ncol = J, byrow = TRUE)
        dE_mig_in = colMeans(dE_mig_in)
        
        dE = dE + dE_mig_in
      }
    }

    # dI
    IR = state_swarm[paste0("I_", p),]/state_swarm["D",]
    IR = pmin(rpois(n = J, lambda = IR), state_swarm[paste0("I_", p),])
    
    dI_brde = -mu*state_swarm[paste0("I_", p),]
    dI_migo = vol_migo*state_swarm[paste0("I_", p),]/state_swarm[paste0("N_", p),]
    dI = EI - IR + dI_brde + dI_migo
    
    ## traveler net
    if (net_mode %in% c("trav_net", "trav_mig_net")) {
      dI_trav = numeric(J)
      for (c in cluster[cluster != p]) {
        vol_net_in_c = flow_trav_in[flow_trav_in$prov_dep == c, "vol_all_sep"] - flow_trav_out[flow_trav_out$prov_arr == c, "vol_all_sep"]
        
        vol_qf_c = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == c), "vol_trav_out_night"]
        vol_qf_p = census_trav_out_night[(census_trav_out_night$yr == year(t)) & (census_trav_out_night$prov_name == p), "vol_trav_out_night"]
        qf_c = vol_qf_c/(vol_qf_c + vol_qf_p)
        
        state_swarm_c = state_swarm_cluster[[c]]
        
        dI_trav_c = (qf_c*state_swarm_c[paste0("I_", c),]/state_swarm_c[paste0("N_", c),] +
                       (1 - qf_c)*state_swarm[paste0("I_", p),]/state_swarm[paste0("N_", p),])*vol_net_in_c
        
        dI_trav = dI_trav + sign(dI_trav_c)*rpois(n = J, lambda = abs(dI_trav_c))
      }
      
      dI = dI + dI_trav
    }
    
    ## migrant net
    if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      for (b in subpop_b) {
        f = f_mig_t4[[p]][["f_b_mig_out"]][[b]][["f"]]
        b_arr = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_out[flow_mig_out$prov_arr == b_arr, "vol_all_sep"]
        
        Iprop = state_swarm[paste0("I_", p),]/state_swarm[paste0("N_", p),]
        
        dI_mig_out = Iprop*vol_b
        dI_mig_out = rpois(n = rep*J, lambda = dI_mig_out)
        dI_mig_out = matrix(dI_mig_out, ncol = J, byrow = TRUE)
        dI_mig_out = colMeans(dI_mig_out)
        
        dI = dI - dI_mig_out
      }
    }
    
    ## unemp migrant net
    if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      for (b in subpop_b) {
        f = f_mig_t5[[p]][["f_b_mig_in"]][[b]]
        b_dep = strsplit(b, "_")[[1]][1]
        vol_b = f*flow_mig_in[flow_mig_in$prov_dep == b_dep, "vol_all_sep"]
        
        state_swarm_b_dep = state_swarm_cluster[[b_dep]]
        b_counter = paste0(b_dep, "_", p)
        
        Iprop = state_swarm_b_dep[paste0("I_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]
        
        dI_mig_in = Iprop*vol_b
        dI_mig_in = rpois(n = rep*J, lambda = dI_mig_in)
        dI_mig_in = matrix(dI_mig_in, ncol = J, byrow = TRUE)
        dI_mig_in = colMeans(dI_mig_in)
        
        dI = dI + dI_mig_in
      }
    }

    # newI
    newI = IR

    # dN
    dN = (lambda - mu)*state_swarm[paste0("N_", p),] + vol_migo
    
    if (net_mode %in% c("trav_net", "trav_mig_net")) {
      dN = dN + sum(flow_trav_in$vol_all_sep) - sum(flow_trav_out$vol_all_sep)
    }
    
    if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      dN = dN - vol
    }
    
    if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
      dN = dN + vol
    }

    # update dstate_swarm
    dstate_swarm = state_swarm[c(outer(var, subpop, FUN = paste, sep = "_")),]
    dstate_swarm[paste0(var, "_", p),] = matrix(c(dS, dE, dI, newI, dN), byrow = TRUE, ncol = J)

    #### simulation for a -----
    for (a in subpop_a) {
      # dS
      SE = beta1*state_swarm[paste0("I_", a),]^state_swarm["m1",]/state_swarm[paste0("N_", a),] + beta2*state_swarm[paste0("I_", p),]^state_swarm["m2",]/state_swarm[paste0("N_", p),]
      for (a_other in subpop_a[!(subpop_a %in% a)]) {
        SE = SE + beta2*state_swarm[paste0("I_", a_other),]^state_swarm["m2",]/state_swarm[paste0("N_", a_other),]
      }
      for (b in subpop_b) {
        SE_b = beta2*state_swarm[paste0("I_", b),]^state_swarm["m2",]/state_swarm[paste0("N_", b),]
        SE_b[is.na(SE_b)] = 0
        SE = SE + SE_b
      }
      
      SE = SE*state_swarm[paste0("S_", a),]
      SE = pmin(rpois(n = J, lambda = SE), state_swarm[paste0("S_", a),])
      
      dS_brde = lambda*state_swarm[paste0("N_", a),]*(1 - xi) - mu*state_swarm[paste0("S_", a),]
      dS = -SE + dS_brde
      
      # migrant net
      if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        a_arr = strsplit(a, "_")[[1]][2]
        flow_mig_out = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr == a_arr),]
        f = f_mig_t2[[p]][["f_a_mig_out"]][[a]]
        vol = f*flow_mig_out$vol_all_sep
        
        dS_mig_out = state_swarm[paste0("S_", a),]/state_swarm[paste0("N_", a),]*vol

        dS = dS - dS_mig_out
      }
      
      if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        a_dep = strsplit(a, "_")[[1]][2]
        flow_mig_in = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == a_dep) & (flow_trav$prov_arr == p),]
        f = f_mig_t4[[p]][["f_a_mig_in"]][[a]][["f"]]
        f_r = f_mig_t4[[p]][["f_a_mig_in"]][[a]][["f_r"]]
        vol = f*flow_mig_in$vol_all_sep
        vol_r = f_r*flow_mig_in$vol_all_sep
        
        state_swarm_a_dep = state_swarm_cluster[[a_dep]]
        a_counter = paste0(p, "_", a_dep, "_r")
        
        Sprop_a_dep = state_swarm_a_dep[paste0("S_", a_dep),]/state_swarm_a_dep[paste0("N_", a_dep),]
        Sprop_a_counter = state_swarm_a_dep[paste0("S_", a_counter),]/state_swarm_a_dep[paste0("N_", a_counter),]
        
        dS_mig_in = Sprop_a_dep*vol + Sprop_a_counter*vol_r
        dS = dS + dS_mig_in
      }
      
      # unemp migrant net
      if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        a_arr = strsplit(a, "_")[[1]][2]
        flow_mig_out = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr == a_arr),]
        f = f_mig_t5[[p]][["f_a_mig_out"]][[a]]
        vol = f*flow_mig_out$vol_all_sep
        
        dS_mig_out = state_swarm[paste0("S_", a),]/state_swarm[paste0("N_", a),]*vol
        
        dS = dS - dS_mig_out
      }

      # dE
      EI = state_swarm[paste0("E_", a),]/state_swarm["Z",]
      EI = pmin(rpois(n = J, lambda = EI), state_swarm[paste0("E_", a),])
      
      dE_brde = -mu*state_swarm[paste0("E_", a),]
      dE = SE - EI + dE_brde
      
      ## migrant net
      if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dE_mig_out = state_swarm[paste0("E_", a),]/state_swarm[paste0("N_", a),]*vol
        dE_mig_out = rpois(n = rep*J, lambda = dE_mig_out)
        dE_mig_out = matrix(dE_mig_out, ncol = J, byrow = TRUE)
        dE_mig_out = colMeans(dE_mig_out)
        
        dE = dE - dE_mig_out
      }

      if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        Eprop_a_dep = state_swarm_a_dep[paste0("E_", a_dep),]/state_swarm_a_dep[paste0("N_", a_dep),]
        Eprop_a_counter = state_swarm_a_dep[paste0("E_", a_counter),]/state_swarm_a_dep[paste0("N_", a_counter),]
        
        dE_mig_in = Eprop_a_dep*vol + Eprop_a_counter*vol_r
        dE_mig_in = rpois(n = rep*J, lambda = dE_mig_in)
        dE_mig_in = matrix(dE_mig_in, ncol = J, byrow = TRUE)
        dE_mig_in = colMeans(dE_mig_in)
        
        dE = dE + dE_mig_in
      }
      
      # unemp migrant net
      if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dE_mig_out = state_swarm[paste0("E_", a),]/state_swarm[paste0("N_", a),]*vol
        dE_mig_out = rpois(n = rep*J, lambda = dE_mig_out)
        dE_mig_out = matrix(dE_mig_out, ncol = J, byrow = TRUE)
        dE_mig_out = colMeans(dE_mig_out)
        
        dE = dE - dE_mig_out
      }

      # dI
      IR = state_swarm[paste0("I_", a),]/state_swarm["D",]
      IR = pmin(rpois(n = J, lambda = IR), state_swarm[paste0("I_", a),])
      
      dI_brde = -mu*state_swarm[paste0("I_", a),]
      dI = EI - IR + dI_brde
      
      ## migrant net
      if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dI_mig_out = state_swarm[paste0("I_", a),]/state_swarm[paste0("N_", a),]*vol
        dI_mig_out = rpois(n = rep*J, lambda = dI_mig_out)
        dI_mig_out = matrix(dI_mig_out, ncol = J, byrow = TRUE)
        dI_mig_out = colMeans(dI_mig_out)
        
        dI = dI - dI_mig_out
      }
      
      if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        Iprop_a_dep = state_swarm_a_dep[paste0("I_", a_dep),]/state_swarm_a_dep[paste0("N_", a_dep),]
        Iprop_a_counter = state_swarm_a_dep[paste0("I_", a_counter),]/state_swarm_a_dep[paste0("N_", a_counter),]
        
        dI_mig_in = Iprop_a_dep*vol + Iprop_a_counter*vol_r
        dI_mig_in = rpois(n = rep*J, lambda = dI_mig_in)
        dI_mig_in = matrix(dI_mig_in, ncol = J, byrow = TRUE)
        dI_mig_in = colMeans(dI_mig_in)
        
        dI = dI + dI_mig_in
      }

      # unemp migrant net
      if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dI_mig_out = state_swarm[paste0("I_", a),]/state_swarm[paste0("N_", a),]*vol
        dI_mig_out = rpois(n = rep*J, lambda = dI_mig_out)
        dI_mig_out = matrix(dI_mig_out, ncol = J, byrow = TRUE)
        dI_mig_out = colMeans(dI_mig_out)
        
        dI = dI - dI_mig_out
      }

      # newI
      newI = IR
      
      # dN
      dN = (lambda - mu)*state_swarm[paste0("N_", a),]
      
      if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dN = dN - vol
      }
      
      if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dN = dN + vol + vol_r
      }
      
      if ((tbl_holiday_t5[tbl_holiday_t5$date == t, "t_t5"] == 1) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
        dN = dN - vol
      }

      # update dstate_swarm
      dstate_swarm[paste0(var, "_", a),] = matrix(c(dS, dE, dI, newI, dN), byrow = TRUE, ncol = J)
    }

    #### simulation for b -----
    if (t_curr %in% c("t2", "t3", "t4")) { # to speed up; the code still works without the `if`
      for (b in subpop_b) {
        # dS
        SE = beta1*state_swarm[paste0("I_", b),]^state_swarm["m1",]/state_swarm[paste0("N_", b),] + beta3*state_swarm[paste0("I_", p),]^state_swarm["m3",]/state_swarm[paste0("N_", p),]
        for (a in subpop_a) {
          SE = SE + beta2*state_swarm[paste0("I_", a),]^state_swarm["m2",]/state_swarm[paste0("N_", a),]
        }
        for (b_other in subpop_b[!(subpop_b %in% b)]) {
          SE = SE + beta3*state_swarm[paste0("I_", b_other),]^state_swarm["m3",]/state_swarm[paste0("N_", b_other),]
        }
        
        SE[is.na(SE)] = 0
        
        SE = SE*state_swarm[paste0("S_", b),]
        SE = rpois(n = rep*J, lambda = SE)
        SE = matrix(SE, ncol = J, byrow = TRUE)
        SE = pmin(colMeans(SE), state_swarm[paste0("S_", b),])
        
        dS_brde = lambda*state_swarm[paste0("N_", b),]*(1 - xi) - mu*state_swarm[paste0("S_", b),]
        dS = -SE + dS_brde
        
        ## migrant net
        if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          b_dep = strsplit(b, "_")[[1]][1]
          flow_mig_in = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == b_dep) & (flow_trav$prov_arr == p),]
          f = f_mig_t2[[p]][["f_b_mig_in"]][[b]]
          vol = f*flow_mig_in$vol_all_sep
          
          state_swarm_b_dep = state_swarm_cluster[[b_dep]]
          b_counter = paste0(b_dep, "_", p)
          
          dS_mig_in = state_swarm_b_dep[paste0("S_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]*vol
          
          dS = dS + dS_mig_in
        }
        
        if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          b_arr = strsplit(b, "_")[[1]][1]
          flow_mig_out = flow_trav[(flow_trav$date == t) & (flow_trav$prov_dep == p) & (flow_trav$prov_arr == b_arr),]
          f = f_mig_t4[[p]][["f_b_mig_out"]][[b]][["f_r"]]
          vol = f*flow_mig_out$vol_all_sep
          
          Sprop = state_swarm[paste0("S_", b),]/state_swarm[paste0("N_", b),]
          
          dS_mig_out = Sprop*vol
          dS = dS - dS_mig_out
        }
        
        # dE
        EI = state_swarm[paste0("E_", b),]/state_swarm["Z",]
        EI = rpois(n = rep*J, lambda = EI)
        EI = matrix(EI, ncol = J, byrow = TRUE)
        EI = pmin(colMeans(EI), state_swarm[paste0("E_", b),])
        
        dE_brde = -mu*state_swarm[paste0("E_", b),]
        dE = SE - EI + dE_brde
        
        ## migrant net
        if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          dE_mig_in = state_swarm_b_dep[paste0("E_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]*vol
          dE_mig_in = rpois(n = rep*J, lambda = dE_mig_in)
          dE_mig_in = matrix(dE_mig_in, ncol = J, byrow = TRUE)
          dE_mig_in = colMeans(dE_mig_in)
          
          dE = dE + dE_mig_in
        }

        if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          Eprop = state_swarm[paste0("E_", b),]/state_swarm[paste0("N_", b),]
          
          dE_mig_out = Eprop*vol
          dE_mig_out = rpois(n = rep*J, lambda = dE_mig_out)
          dE_mig_out = matrix(dE_mig_out, ncol = J, byrow = TRUE)
          dE_mig_out = colMeans(dE_mig_out)
          
          dE = dE - dE_mig_out
        }
        
        # dI
        IR = state_swarm[paste0("I_", b),]/state_swarm["D",]
        IR = rpois(n = rep*J, lambda = IR)
        IR = matrix(IR, ncol = J, byrow = TRUE)
        IR = pmin(colMeans(IR), state_swarm[paste0("I_", b),])
        
        dI_brde = -mu*state_swarm[paste0("I_", b),]
        dI = EI - IR + dI_brde
        
        ## migrant net
        if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          dI_mig_in = state_swarm_b_dep[paste0("I_", b_counter),]/state_swarm_b_dep[paste0("N_", b_counter),]*vol
          dI_mig_in = rpois(n = rep*J, lambda = dI_mig_in)
          dI_mig_in = matrix(dI_mig_in, ncol = J, byrow = TRUE)
          dI_mig_in = colMeans(dI_mig_in)
          
          dI = dI + dI_mig_in
        }
        
        if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          Iprop = state_swarm[paste0("I_", b),]/state_swarm[paste0("N_", b),]
          
          dI_mig_out = Iprop*vol
          dI_mig_out = rpois(n = rep*J, lambda = dI_mig_out)
          dI_mig_out = matrix(dI_mig_out, ncol = J, byrow = TRUE)
          dI_mig_out = colMeans(dI_mig_out)
          
          dI = dI - dI_mig_out
        }
        
        # newI
        newI = IR
        
        # dN
        dN = (lambda - mu)*state_swarm[paste0("N_", b),]
        
        if ((t_curr %in% c("t2")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          dN = dN + vol
        }
        
        if ((t_curr %in% c("t4")) && (net_mode %in% c("mig_net", "trav_mig_net"))) {
          dN = dN - vol
        }
        
        # update dstate_swarm
        dstate_swarm[paste0(var, "_", b),] = matrix(c(dS, dE, dI, newI, dN), byrow = TRUE, ncol = J)
      }
    }

    # update state_swarm
    state_swarm[c(outer(var, subpop, FUN = paste, sep = "_")),] = pmax(round(state_swarm[c(outer(var, subpop, FUN = paste, sep = "_")),] + dstate_swarm), 0)
    state_swarm[paste0("newI_", subpop),] = dstate_swarm[paste0("newI_", subpop),]
    
    if ((t >= t_SIA_strt) && (t <= t_SIA_end)) {
      dstate_swarm_SIA = dstate_swarm_SIA_cluster[[p]]
      state_swarm[paste0("S_", c(p, subpop_a)),] = pmax(state_swarm[paste0("S_", c(p, subpop_a)),] + dstate_swarm_SIA, 0)
    }

    rt = setNames(list(list("state_swarm" = state_swarm)), p)
    
    if (t == t_seq[1]) {
      rt[[p]][["f_clim"]] = f_clim_rec
    }

    if (tbl_holiday_K[tbl_holiday_K$date == t, "t_K"] == 1) {
      rt[[p]][["f_cont"]] = f_cont_rec
    }
    
    rt
  }

  # update state_swarm_cluster
  if (t == t_seq[1]) {
    for (p in cluster) {
      f_clim_cluster[[p]] = state_swarm_cluster_updt[[p]][["f_clim"]]
    }
  } 
  
  if (tbl_holiday_K[tbl_holiday_K$date == t, "t_K"] == 1) {
    for (p in cluster) {
      f_cont_cluster[[p]] = state_swarm_cluster_updt[[p]][["f_cont"]]
    }
  }
  
  for (p in cluster) {
    state_swarm_cluster_updt[[p]] = state_swarm_cluster_updt[[p]][["state_swarm"]]
  }

  state_swarm_cluster = lapply(state_swarm_cluster_updt, function(x) x[!(rownames(x) %in% c("f_cont_track", "f_clim_track", "f_sch_track")),])

  # store results
  state_swarm_rslt_tmp[[as.character(t)]] = state_swarm_cluster_updt
  
  if (t %in% t_seq_res[-1]) {
    t_strt = t_seq_res[which(t == t_seq_res) - 1] + 1
    t_range = as.character(seq(t_strt, t, by = 1))
    t_len = length(t_range)
    state_swarm_rslt_tmp = state_swarm_rslt_tmp[names(state_swarm_rslt_tmp) %in% t_range]
    
    state_swarm_rslt_agg = list()
    for (p in cluster) {
      state_swarm_rslt_agg_p = lapply(state_swarm_rslt_tmp, function(x) x[[p]])
      state_swarm_rslt_agg_p = abind(state_swarm_rslt_agg_p, along = 3)
      
      state_swarm_rslt_agg_p[grepl("newI", dimnames(state_swarm_rslt_agg_p)[[1]]),,t_len] = 
        drop(apply(state_swarm_rslt_agg_p[grepl("newI", dimnames(state_swarm_rslt_agg_p)[[1]]),,, drop = FALSE], c(1, 2), sum))

      state_swarm_rslt_agg_p = state_swarm_rslt_agg_p[,,t_len]
      state_swarm_rslt_agg[[p]] = state_swarm_rslt_agg_p
    }

    state_swarm_rslt_tmp = list()
    state_swarm_rslt[[as.character(t)]] = state_swarm_rslt_agg
  }
  
  # merge subpop_b into p in the last day of t4 for next iteration of t
  if ((t_curr == "t4") && identical(t_next, "t1")) {
    for (p in cluster) {
      state_swarm_cluster_merge_p = state_swarm_cluster[[p]]
      
      subpop_b = smry_subpop[[p]][["subpop_b"]]
      n_subpop_b = smry_subpop[[p]][["n_subpop_b"]]
      
      for (b in subpop_b) {
        state_swarm_cluster_merge_p[paste0(var, "_", p),] = state_swarm_cluster_merge_p[paste0(var, "_", p),] + state_swarm_cluster_merge_p[paste0(var, "_", b),]
        state_swarm_cluster_merge_p[paste0(var, "_", b),] = 0
      }
      
      state_swarm_cluster[[p]] = state_swarm_cluster_merge_p
    }
  }
}

if (identical(cluster, prov_code$prov_name)) {
  cluster_mode = "_"
} else {
  cluster_mode = "_cluster_"
}

invisible(foreach(p = cluster) %dopar% {
  state_swarm_rslt_p = lapply(state_swarm_rslt, function(x) x[[p]])
  state_swarm_rslt_p = abind(state_swarm_rslt_p, along = 3)
  
  param_swarm_rslt_p = state_swarm_rslt_p[param,,1]
  state_swarm_rslt_p = state_swarm_rslt_p[setdiff(dimnames(state_swarm_rslt_p)[[1]], param),,]
  
  state_swarm_rslt_init_p = state_swarm_rslt_init[[p]]
  
  saveRDS(object = list(var_swarm = state_swarm_rslt_p, param_swarm = param_swarm_rslt_p, state_swarm_init = state_swarm_rslt_init_p),
          file = paste0(dir_rslt, "state_swarm_rslt_", net_mode, cluster_mode, p, "_batch", batch, ".rds"))
  
  NULL
})

runtime_end = as.numeric(difftime(Sys.time(), runtime_strt, units = "mins"))
print(runtime_mid)
print(runtime_end)

rm(list=ls())
gc()


