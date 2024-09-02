#This files contains code used to develop gravity models

library(plm)
library(tidyverse)
library(car)
library(interactions)

df <- read_csv("china_data.csv") %>%
  rename(exp_to_third_states = `Export (US$ Thousand)`,
         imp_from_third_state = `Import (US$ Thousand)`) %>%
  group_by(iso3) %>%
  mutate(iso_d = "CHN",
         v2x_api = 1 - v2x_api,
         d_v2_x_api = 1 - d_v2x_api,
         v2xcl_rol = 1 - v2xcl_rol,
         d_v2xcl_rol = 1 - d_v2xcl_rol,
         log_import = log(imp_from_third_state),
         log_export = log(exp_to_third_states),
         civ_deaths = cumulative_total_deaths_civilians_in_orgvio_cy + 1,
         d_civ_deaths = d_cumulative_total_deaths_civilians_in_orgvio_cy + 1,
         log_o_GDP = log(WB_GDP),
         log_d_GDP = log(d_WB_GDP),
         log_o_pop = log(WB_population),
         log_d_pop = log(d_WB_population),
         log_o_dem = log(v2x_api),
         log_d_dem = log(d_v2x_api),
         log_o_pir = log(v2xcl_rol),
         log_d_pir = log(d_v2xcl_rol),
         log_o_CPI = log(CPI),
         log_d_CPI = log(d_CPI),
         log_o_RR = log(WB_RR),
         log_d_RR = log(d_WB_RR),
         log_o_dem_trade = log_o_dem * trade,
         ll_v2 = dplyr::lag(log(v2xcl_rol), n = 1),
         ll_d_v2 = dplyr::lag(log(d_v2xcl_rol), n = 1),
         dem_o_lag = dplyr::lag(log_o_dem, n = 1),
         dem_d_lag = dplyr::lag(log_d_dem, n = 1),
         lag_trade = dplyr::lag(trade, n =1 ),
         lag_finance = dplyr::lag(financial, n =1),
         lag_int_dem_trade = lag_trade * dem_o_lag,
         lag_int_dem_fin = lag_finance * dem_o_lag,
         rr_int = log_o_RR * log_o_dem,
         lag_rr_o = lag(log_o_RR),
         lag_rr_d = lag(log_d_RR),
         lag_rr_int = lag_rr_o * lag_rr_d) %>%
  ungroup()


formula_1 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + EAEU + landlocked  +
  log(v2x_api) + log(d_v2x_api) + log(WB_internet) + log(d_WB_internet) +
  log(WB_RR) + log(d_WB_RR) + log(civ_deaths) + log(d_civ_deaths)

model1 <- plm(formula_1, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model1)

#Excluded EAEU -- no effect on the model
#WB internet access is not effective
#Civillian deaths excluded
#Rersource rents -- should it be here? 

formula_2 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + landlocked + trade +
  log(v2x_api) + log(d_v2x_api) + log(WB_RR) + log(d_WB_RR)

model2 <- plm(formula_2, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model2)

df %>%
  dplyr::filter(iso3 == "EST") %>%
  select(v2x_api, Year)


formula_3 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + landlocked + trade +
  log(WB_RR) + log(d_WB_RR) +
  log(v2x_api) + log(d_v2x_api) + trade:log(v2x_api)

model3 <- plm(formula_3, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model3)


#Lag models
formula_4 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + EAEU + landlocked + lag(trade) +
  lag(log(v2x_api)) + lag(log(d_v2x_api)) + log(WB_RR) + log(d_WB_RR)

model4 <- plm(formula_4, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model4)



formula_5 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + landlocked + lag(trade) +
  log(WB_RR) + log(d_WB_RR) +
  lag(log(v2x_api)) + lag(log(d_v2x_api)) + lag(trade):lag(log(v2x_api))

model5 <- plm(formula_5, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model5)

df$v2x_api


#Swapping out trade and financial sanctions

formula_6 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + EAEU + landlocked + lag(financial) +
  lag(log(v2x_api)) + lag(log(d_v2x_api)) + log(WB_RR) + log(d_WB_RR)

model6 <- plm(formula_6, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model6)


formula_7 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + EAEU + landlocked + lag(financial) +
  lag(log(v2x_api)) + lag(log(d_v2x_api)) + log(WB_RR) + log(d_WB_RR) +
  lag(log(v2x_api)):lag(financial)

model7 <- plm(formula_7, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model7)



#Fixed effects (without interactions)



model2f <- plm(formula_2, data = df, model = "within", index = c("iso3", "Year"))

summary(model2f)

model3f <- plm(formula_3, data = df, model = "within", index = c("iso3", "Year"))

summary(model3f)

model4f <- plm(formula_4,data = df, model = "within", index = c("iso3", "Year"))

summary(model4f)

model5f <- plm(formula_5, data = df, model = "within", index = c("iso3", "Year"))

summary(model5f)

model6f <- plm(formula_6, data = df, model = "within", index = c("iso3", "Year"))

summary(model6f)



#RR interactions


formula_8 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + landlocked + trade +
  log_o_dem + log(d_v2x_api) + log_o_RR + log(d_WB_RR) + 
  log_o_dem:log_o_RR

model8 <- plm(formula_8, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model8)

interact_plot(model8, pred = log_o_dem, modx = log_o_RR)


formula_9 <- log_import ~ log(WB_GDP) + log(d_WB_GDP) + log(v2xcl_rol) +
  log(d_v2xcl_rol) + log(CPI) + log(d_CPI) + log(WB_population) +
  log(d_WB_population) + log(kmdist) + border + landlocked + trade +
  log(v2x_api) + log(d_v2x_api) + lag(log(WB_RR)) + log(d_WB_RR) + 
  lag(log(v2x_api)):lag(log(WB_RR))


model9 <- plm(formula_9, data = df, model = "pooling", index = c("iso3", "Year"))

summary(model9)
