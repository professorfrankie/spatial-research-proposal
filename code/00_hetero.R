library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)


df <- read_csv("processed_data/df_amazon_newpca.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined)) |> 
  filter(between(year, 2002, 2022))

df_share <- df %>%
  group_by(muni_id) %>%
  summarise(avg_garimpo_ha = mean(garimpo_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    total_avg_garimpo = sum(avg_garimpo_ha, na.rm = TRUE),
    share_zi0 = avg_garimpo_ha / total_avg_garimpo
  ) %>%
  select(muni_id, share_zi0)

sum(df_share$share_zi0)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  )

####################################################
# using difference in gold price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_gold1 * share_zi0,
    bartik2 = shift_gold2 * share_zi0,
    bartik3 = shift_gold3 * share_zi0,
    bartik4 = shift_gold4 * share_zi0
  )

####################################################

## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

######## dummy variables #########

## post 2014 dummy and para dummy

df_model <- df_model %>%
  mutate(
    good = ifelse(between(year, 2006, 2013), 1, 0),
    para = ifelse(substr(muni_id, 1, 2) == "15", 1, 0)
  )

######## PARA #############

df_para <- df_model %>%
  filter(para == 1)

df_nonpara <- df_model %>%
  filter(para == 0)

ols_para <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = df_para)

ols_nonpara <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change | year,
                     data = df_nonpara)

etable(
  list("Para" = ols_para, "Non-Para" = ols_nonpara),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Para", "Non-Para"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – Para vs Non-Para",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)

second_stage3_param <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                               population_change + pop_dens_change + pa_tot_ha_change + 
                               n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                             data = df_para)

summary(second_stage3_param)

second_stage3_nonparam <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                                data = df_nonpara)

summary(second_stage3_nonparam)

second_stage4_param <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                               population_change + pop_dens_change + pa_tot_ha_change + 
                               n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                             data = df_para)

summary(second_stage4_param)

second_stage4_nonparam <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                                data = df_nonpara)

summary(second_stage4_nonparam)

etable(
  list(
    "Lag 3 – Para"     = second_stage3_param, 
    "Lag 3 – Non-Para" = second_stage3_nonparam,
    "Lag 4 – Para"     = second_stage4_param, 
    "Lag 4 – Non-Para" = second_stage4_nonparam
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lag 3 – Para", "Lag 3 – Non-Para", "Lag 4 – Para", "Lag 4 – Non-Para"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Para vs Non-Para (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


#full sample of para with interaction terms

stage3_para <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       para*garimpo_ha_change ~ para*bartik3,
                     data = df_model)

stage4_para <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       para*garimpo_ha_change ~ para*bartik4,
                     data = df_model)

etable(
  list(
    "Lag 3 – Para" = stage3_para, 
    "Lag 4 – Para" = stage4_para
  ),
  dict = c(
    "fit_garimpo_ha_change"       = "Change in Garimpo Area",
    "fit_para:garimpo_ha_change"  = "Change in Garimpo Area x Para",
    "fit_para"                    = "Para",
    "forest_loss_all_gross"       = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change", "%fit_para:garimpo_ha_change", "%fit_para"),
  headers = c("Lag 3 – Para", "Lag 4 – Para"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Para vs Non-Para (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)


####################################

############ PRE POST 2014 ##########

df_good <- df_model %>%
  filter(good == 1)

df_bad <- df_model %>%
  filter(good == 0)

ols_good <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = df_good)

ols_bad <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                   population_change + pop_dens_change + pa_tot_ha_change + 
                   n_fined_change + brl_fined_change | year,
                 data = df_bad)

etable(
  list("2006-2013" = ols_good, "Other Years" = ols_bad),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("2006-2013", "Other Years"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – 2006-2013 vs Other Years",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)

second_stage3_good <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                            data = df_good)

summary(second_stage3_good)

second_stage3_bad <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                             population_change + pop_dens_change + pa_tot_ha_change + 
                             n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                           data = df_bad)

summary(second_stage3_bad)

second_stage4_good <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                            data = df_good)

second_stage4_bad <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                             population_change + pop_dens_change + pa_tot_ha_change + 
                             n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                           data = df_bad)

etable(
  list(
    "Lag 3 – Good"     = second_stage3_good, 
    "Lag 3 – Bad" = second_stage3_bad,
    "Lag 4 – Good"     = second_stage4_good, 
    "Lag 4 – Bad" = second_stage4_bad
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lag 3 – Good", "Lag 3 – Bad", "Lag 4 – Good", "Lag 4 – Bad"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Good vs Bad (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


# full sample 

stage3_good <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change |
                        year | 
                        good*garimpo_ha_change ~ good*bartik3,
                      data = df_model)
stage4_good <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change |
                        year | 
                        good*garimpo_ha_change ~ good*bartik4,
                      data = df_model)

etable(
  list(
    "Lag 3 – Good"     = stage3_good, 
    "Lag 4 – Good"     = stage4_good
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lag 3 – Good", "Lag 4 – Good"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Good vs Bad (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full")
  )
)


####### OLS #########
ols_model <- feols(forest_loss_all_gross ~ garimpo_ha_change+ spei_dry + gdp_pc_change + 
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = df_model)

ols_post2014 <- feols(forest_loss_all_gross ~ garimpo_ha_change*post2014 + spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change,
                      data = df_model)

ols_para <- feols(forest_loss_all_gross ~ garimpo_ha_change*para + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = df_model)

############# 4 Presidenti #############àà

df_pres <- df_model |> 
  mutate(
    lula = ifelse(between(year, 2003, 2010), 1, 0),
    rousseff = ifelse(between(year, 2011, 2016), 1, 0),
    temer = ifelse(between(year, 2017, 2018), 1, 0),
    bolsonaro = ifelse(between(year, 2019, 2022), 1, 0)
  )

## lula

lula <- df_pres |> 
  filter(lula == 1)

ols_lula <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change | year,
                  data = lula)

## rousseff

rousseff <- df_pres |> 
  filter(rousseff == 1)

ols_rousseff <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year,
                      data = rousseff)

## temer

temer <- df_pres |> 
  filter(temer == 1)

ols_temer <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change +
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = temer)

## bolsonaro

bolsonaro <- df_pres |> 
  filter(bolsonaro == 1)

ols_bolsonaro <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year,
                       data = bolsonaro)

etable(
  list(
    "Lula"      = ols_lula, 
    "Rousseff"  = ols_rousseff,
    "Temer"     = ols_temer, 
    "Bolsonaro" = ols_bolsonaro
  ),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – By President",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


ols_lula <- feols(forest_loss_all_gross ~ garimpo_ha_change*lula + spei_dry + gdp_pc_change + 
                    population_change + pop_dens_change + pa_tot_ha_change + 
                    n_fined_change + brl_fined_change,
                  #| #year,
                  data = df_pres)

ols_rousseff <- feols(forest_loss_all_gross ~ garimpo_ha_change*rousseff + spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change,
                      #| #year,
                      data = df_pres)

ols_temer <- feols(forest_loss_all_gross ~ garimpo_ha_change*temer + spei_dry + gdp_pc_change +
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change,
                   #| #year,
                   data = df_pres)

ols_bolsonaro <- feols(forest_loss_all_gross ~ garimpo_ha_change*bolsonaro + spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change,
                       #| #year,
                       data = df_pres)

## create etable with all presidents and ols_president

etable(
  list(
    "Lula"      = ols_lula, 
    "Rousseff"  = ols_rousseff,
    "Temer"     = ols_temer, 
    "Bolsonaro" = ols_bolsonaro
  ),
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f,
  tex     = TRUE,
  title   = "OLS Estimates – By President",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## IV strategy 

## lula 

stage3_lula <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       #year | 
                       lula*garimpo_ha_change ~ lula*bartik3,
                     data = df_pres)

summary(stage3_lula)

stage3_luls <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change | 
                       year | 
                       garimpo_ha_change ~ bartik3,
                     data = lula)

summary(stage3_luls)

## rousseff

stage3_rousseff <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                           population_change + pop_dens_change + pa_tot_ha_change + 
                           n_fined_change + brl_fined_change |
                           #year | 
                           rousseff*garimpo_ha_change ~ rousseff*bartik3,
                         data = df_pres)

summary(stage3_rousseff)

stage3_rousseffs <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change | 
                            year | 
                            garimpo_ha_change ~ bartik3,
                          data = rousseff)

summary(stage3_rousseffs)



## temer

stage3_temer <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change |
                        #year | 
                        temer*garimpo_ha_change ~ temer*bartik3,
                      data = df_pres)

summary(stage3_temer)

stage3_temers <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | 
                         year | 
                         garimpo_ha_change ~ bartik3,
                       data = temer)

summary(stage3_temers)

## bolsonaro

stage3_bolsonaro <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change |
                            #year | 
                            bolsonaro*garimpo_ha_change ~ bolsonaro*bartik3,
                          data = df_pres)

summary(stage3_bolsonaro)


stage3_bolsonaros <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                             population_change + pop_dens_change + pa_tot_ha_change + 
                             n_fined_change + brl_fined_change | 
                             year | 
                             garimpo_ha_change ~ bartik3,
                           data = bolsonaro)

summary(stage3_bolsonaros)

## create etable with all presidents and stage3_president 

etable(
  list(
    "Lula"      = stage3_lula, 
    "Rousseff"  = stage3_rousseff,
    "Temer"     = stage3_temer, 
    "Bolsonaro" = stage3_bolsonaro
  ),
  dict = c(
    "fit_garimpo_ha_change"       = "Change in Garimpo Area",
    "fit_lula:garimpo_ha_change"  = "Change in Garimpo Area x Lula",
    "fit_lula"                    = "Lula",
    "fit_rousseff:garimpo_ha_change"  = "Change in Garimpo Area x Rousseff",
    "fit_rousseff"                    = "Rousseff",
    "fit_temer:garimpo_ha_change"  = "Change in Garimpo Area x Temer",
    "fit_temer"                    = "Temer",
    "fit_bolsonaro:garimpo_ha_change"  = "Change in Garimpo Area x Bolsonaro",
    "fit_bolsonaro"                    = "Bolsonaro",
    "forest_loss_all_gross"       = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change", "%fit_lula:garimpo_ha_change", "%fit_lula",
              "%fit_rousseff:garimpo_ha_change", "%fit_rousseff",
              "%fit_temer:garimpo_ha_change", "%fit_temer",
              "%fit_bolsonaro:garimpo_ha_change", "%fit_bolsonaro"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 3)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## create etable with all presidents and stage3_presidents

etable(
  list(
    "Lula"      = stage3_luls, 
    "Rousseff"  = stage3_rousseffs,
    "Temer"     = stage3_temers, 
    "Bolsonaro" = stage3_bolsonaros
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 3)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)

## STAGE 4 ##

stage4_luls <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                       population_change + pop_dens_change + pa_tot_ha_change + 
                       n_fined_change + brl_fined_change |
                       year | 
                       garimpo_ha_change ~ bartik4,
                     data = lula)

stage4_rousseffs <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                            population_change + pop_dens_change + pa_tot_ha_change + 
                            n_fined_change + brl_fined_change | 
                            year | 
                            garimpo_ha_change ~ bartik4,
                          data = rousseff)

stage4_temers <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change +
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | 
                         year | 
                         garimpo_ha_change ~ bartik4,
                       data = temer)

stage4_bolsonaros <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | 
                              year | 
                              garimpo_ha_change ~ bartik4,
                            data = bolsonaro)
                         
## create etable for stage 4

etable(
  list(
    "Lula"      = stage4_luls, 
    "Rousseff"  = stage4_rousseffs,
    "Temer"     = stage4_temers, 
    "Bolsonaro" = stage4_bolsonaros
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lula", "Rousseff", "Temer", "Bolsonaro"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "IV Estimates – By President (Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)


###### IV SPECIFICATION #########
###### SECOND STAGE #########

# second stage for change_in_area


second_stage3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                       data = df_model)
summary(second_stage3)

second_stage3_2014 <- feols(
  forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
    population_change + pop_dens_change + pa_tot_ha_change + 
    n_fined_change + brl_fined_change + good |
    garimpo_ha_change + I(good*garimpo_ha_change) ~ 
    bartik3 + I(good*bartik3),
  data = df_model
)


summary(second_stage3_2014)

second_stage3_para <- feols(
  forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
    population_change + pop_dens_change + pa_tot_ha_change + 
    n_fined_change + brl_fined_change + para | 
    year | garimpo_ha_change + I(para*garimpo_ha_change) ~ 
    bartik3 + I(para*bartik3),
  data = df_model
)

summary(second_stage3_para)

second_stage4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                       data = df_model)
summary(second_stage4)

second_stage4_post2014 <- feols(
  forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
    population_change + pop_dens_change + pa_tot_ha_change + 
    n_fined_change + brl_fined_change | 
    garimpo_ha_change + I(post2014*garimpo_ha_change) ~ 
    bartik4 + I(post2014*bartik4),
  data = df_model
)


summary(second_stage4_post2014)

second_stage4_para <- feols(
  forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
    population_change + pop_dens_change + pa_tot_ha_change + 
    n_fined_change + brl_fined_change | 
    year | garimpo_ha_change + I(para*garimpo_ha_change) ~ 
    bartik4 + I(para*bartik4),
  data = df_model
)

summary(second_stage4_para)

# Combine second-stage models into a list
second_stage_models_change <- list(
  "t-1" = second_stage1,
  "t-2" = second_stage2,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)
# Create a summary table for second-stage models
modelsummary(
  second_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

second_stage_models_ols <- list(
  "OLS" = ols_model,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)

etable(
  second_stage_models_ols,
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("OLS", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + f + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full")
  )
)




summary(second_stage1, stage = 1)
summary(second_stage2, stage = 1)
summary(second_stage3, stage = 1)
summary(second_stage4, stage = 1)

fs_t3 <- feols(garimpo_ha_change ~ spei_dry + gdp_pc_change + population_change +
                 pop_dens_change + pa_tot_ha_change + n_fined_change +
                 brl_fined_change + bartik3 | year,
               data = df_model)

fs_t4 <- feols(garimpo_ha_change ~ spei_dry + gdp_pc_change + population_change +
                 pop_dens_change + pa_tot_ha_change + n_fined_change +
                 brl_fined_change + bartik4 | year,
               data = df_model)

first_stage_models_change <- list(
  "t-3 (1st stage)" = fs_t3,
  "t-4 (1st stage)" = fs_t4
)

etable(
  first_stage_models_change,
  dict = c(
    bartik3 = "Bartik",
    bartik4 = "Bartik"
  ),
  keep   = c("%bartik3", "%bartik4"),
  headers = c("t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + f,
  tex = TRUE,
  title = "First Stage Estimates – Change in Area",
  extralines = list("Covariates" = c("Full", "Full"))
)


first_stage_models_change <- list(
  "t-1" = summary(second_stage1, stage = 1),
  "t-2" = summary(second_stage2, stage = 1),
  "t-3" = summary(second_stage3, stage = 1),
  "t-4" = summary(second_stage4, stage = 1)
)

modelsummary(
  first_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "bartik" = "Bartik",
    "bartik2" = "Bartik",
    "bartik3" = "Bartik",
    "bartik4" = "Bartik"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)