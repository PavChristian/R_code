#Using this file to clean datasets obtained from the World Bank 
library(tidyverse)

#Cuts raw WB columns
clean_wb <- function(WB_path, name, c_codes = country_codes$wb) {
  WB_data <- read_csv(WB_path, 
                     skip = 3) %>%
    select(c(1, 2, 40:67)) %>%
    pivot_longer(cols = c(3:30), names_to = "Year", values_to = name) %>%
    filter(`Country Code` %in% c_codes,
           Year < 2022) %>%
    rename(state = `Country Name`,
           iso3 = `Country Code`)
  return(WB_data)
}

#Join the datasets into a single file
clean_wbs <- list.files("path",
                        full.names = TRUE)
full_tibble <- tibble
clean_wbs <- clean_wbs[grepl("WB_", clean_wbs)]
backbone <- read_csv(clean_wbs[1])
for (file in clean_wbs[c(2:10)]){
  if (!str_detect(file, "WB")) {
    next
  }
  WB_tibble <- read_csv(file) %>%
    select(-1)
  backbone <- backbone %>%
    left_join(WB_tibble, by = c("iso3", "Year"))
}

#Joining World Bank Datasets with other tibbles

level1 <- WBs %>%
  left_join(codes, by = "iso3") %>%
  select(state, iso3, Year, iso2, vdem, cow, wb, everything())

#Level2 -- adding vdem variables 

vdem <- read_csv("path2") %>%
  select(-country_name)

level2 <- level1 %>%
  left_join(vdem, by = c("Year" = "year", "iso3" = "country_iso3")) 


#Adding war 

war <- read_csv("path3") %>%
  select(c(3:6))


level3 <- level2 %>%
  left_join(war, by = c("Year" = "year_cy", "iso3"))

#Adding Confucius institutes 
confucius <- read_csv("path4")

level4 <- level3 %>%
  left_join(confucius, by = c("Year", "iso3"))


#Adding distances and institutions (all dummies)
institutions <- 
  read_csv("path5")


distances <- read_csv("path6")

level5 <- level4 %>%
  left_join(distances, by = c("cow" = "idb")) %>%
  select(-ida) %>%
  left_join(institutions, by = c("Year", "iso3"))


#Adding CPI 

cpi <- read_csv("path7")

level6 <- level5 %>%
  left_join(cpi, by = c("Year", "iso3" = "ISO3"))


#Adding trade 

trade <- read_csv("path8")

level7 <- level6 %>%
  left_join(trade, by = c("Year", "iso3" = "iso"))


#Adding China (destination side)

china_data <- read_csv("path9")

level8 <- level7 %>%
  left_join(china_data, by = c("Year" = "d_Year")) %>%
  select(-d_country_name)

#Adding sanctions 
convert_to_0 <- function(x) {
  ifelse(is.na(x), 0, x)
}
sanctions <- read_csv("path10")
level9 <- level8 %>%
  left_join(sanctions, by = c("iso3" = "sanctioned_state_iso3", 
                   "Year" = "year")) %>%
  mutate_at(vars(arms:imposed), list(~ convert_to_0(.)))

