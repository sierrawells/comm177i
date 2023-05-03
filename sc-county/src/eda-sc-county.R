# Author: Sierra Wells
# Stanford University - COMM 177I
# comm177i/sc-county/src/eda-sc-county.R

# Packages ====
pacman::p_load(tidyverse,
               here,
               stringr)

# Files ====
input_paths <- list(sc_contracts = here("sc-county/input/sc-county-dhs-contracts.csv"),
                    bay_contracts = here("sc-county/input/bay-area-local-gov-dhs-contracts.csv"))

output_paths <- list(sc_ice_year_count = here("sc-county/output/sc-ice-year-count.jpg"),
                     sc_ice_year_value = here("sc-county/output/sc-ice-year-value.jpg"),
                     bay_ice_year_count = here("sc-county/output/bay-ice-year-count.jpg"),
                     bay_ice_year_value = here("sc-county/output/bay-ice-year-value.jpg"),
                     bay_local_gov_compare_count = here("sc-county/output/bay-local-gov-compare-count.jpg"),
                     bay_local_gov_compare_val = here("sc-county/output/bay-local-gov-compare-val.jpg"))

# Read data ====
vars_contracts <- c("contract_award_unique_key",
                    "total_obligated_amount",
                    "period_of_performance_start_date",
                    "period_of_performance_current_end_date",
                    "awarding_agency_name",
                    "awarding_sub_agency_name",
                    "recipient_name_raw",
                    "prime_award_base_transaction_description",
                    "naics_code",
                    "naics_description",
                    "recipient_city_name",
                    "recipient_county_name",
                    "city_local_government", 
                    "county_local_government")

sc_contracts_raw <- read.csv(input_paths$sc_contracts)

sc_contracts <- sc_contracts_raw %>% 
  select(all_of(vars_contracts))

bay_contracts_raw <- read.csv(input_paths$bay_contracts)

bay_contracts <- bay_contracts_raw %>% 
  select(all_of(vars_contracts)) %>% 
  mutate(recipient_name = case_when(str_detect(recipient_name_raw, "SCC") |
                                      str_detect(recipient_name_raw, "SANTA CLARA") ~
                                      "SANTA CLARA, COUNTY OF",
                                    str_detect(recipient_name_raw, "COLMA") ~
                                      "COLMA, CITY OF",
                                    str_detect(recipient_name_raw, "SACRAMENTO") ~
                                      "SACRAMENTO, COUNTY OF",
                                    str_detect(recipient_name_raw, "SAN JOAQUIN") ~
                                      "SAN JOAQUIN, COUNTY OF",
                                    T  ~ recipient_name_raw))

# Just Santa Clara County & ICE over time ====
sc_ice_contracts <- sc_contracts %>% 
  filter((str_detect(recipient_name_raw, "SANTA CLARA") |
            str_detect(recipient_name_raw, "SCC")) &
           awarding_sub_agency_name == "U.S. Immigration and Customs Enforcement") %>% 
  mutate(perf_start_year = substr(period_of_performance_start_date, 1, 4),
         perf_end_year = substr(period_of_performance_current_end_date, 1, 4))

sc_ice_summ_by_year <- sc_ice_contracts %>%
  group_by(perf_start_year, perf_end_year) %>%
  summarize(num_contracts = n(),
            total_amount = sum(total_obligated_amount)) %>% 
  mutate(year = map2(perf_start_year, perf_end_year, seq)) %>%
  unnest(year) %>%
  group_by(year) %>%
  summarize(num_active_contracts = sum(num_contracts),
            total_active_amount = sum(total_amount))

sc_ice_count_by_year_gg <- ggplot(sc_ice_summ_by_year,
                               aes(x = year, y = num_active_contracts)) +
  geom_col() +
  xlab("Year") +
  ylab("Number of Active Contracts") +
  ggtitle("Active ICE - SC County Contracts") +
  scale_x_continuous(breaks = seq(min(sc_ice_summ_by_year$year),
                                  max(sc_ice_summ_by_year$year), by = 1))

sc_ice_value_by_year_gg <- ggplot(sc_ice_summ_by_year,
                               aes(x = year, y = total_active_amount)) +
  geom_col() +
  xlab("Year") +
  ylab("Total value of active contracts") +
  ggtitle("Value of Active ICE - SC County Contracts") +
  scale_x_continuous(breaks = seq(min(sc_ice_summ_by_year$year),
                                  max(sc_ice_summ_by_year$year), by = 1))

# Bay Area local govs & ICE over time====
# Bay Area includes following counties:
# Alameda, Contra Costa, Marin, Napa, SF, San Mateo, Santa Clara, Solano, and Sonoma

# Of 36 contracts b/w ICE & local gov'ts in Bay Area, 17 were w/ SC County
# Min year = 2004

bay_ice_contracts <- bay_contracts %>% 
  filter(awarding_sub_agency_name == "U.S. Immigration and Customs Enforcement") %>% 
  mutate(perf_start_year = substr(period_of_performance_start_date, 1, 4),
         perf_end_year = substr(period_of_performance_current_end_date, 1, 4))

bay_ice_summ_by_year <- bay_ice_contracts %>%
  group_by(perf_start_year, perf_end_year) %>%
  summarize(num_contracts = n(),
            total_amount = sum(total_obligated_amount)) %>% 
  mutate(year = map2(perf_start_year, perf_end_year, seq)) %>%
  unnest(year) %>%
  group_by(year) %>%
  summarize(num_active_contracts = sum(num_contracts),
            total_active_amount = sum(total_amount))

bay_ice_count_by_year_gg <- ggplot(bay_ice_summ_by_year,
                               aes(x = year, y = num_active_contracts)) +
  geom_col() +
  xlab("Year") +
  ylab("Number of Active Contracts") +
  ggtitle("Active ICE - Bay Area Local Gov Contracts") +
  scale_x_continuous(breaks = seq(min(bay_ice_summ_by_year$year),
                                  max(bay_ice_summ_by_year$year), by = 1))

bay_ice_value_by_year_gg <- ggplot(bay_ice_summ_by_year,
                               aes(x = year, y = total_active_amount)) +
  geom_col() +
  labs(
    x = "Year", y = "Total value of active contracts",
    title = "Value of Active ICE - Bay Area Local Gov Contracts") + 
  scale_x_continuous(breaks = seq(min(bay_ice_summ_by_year$year),
                                  max(bay_ice_summ_by_year$year), by = 1))

# Compare Bay Area local govs w/ contracts w/ ICE ====
bay_ice_local_gov_counts <- bay_contracts %>% 
  filter(awarding_sub_agency_name == "U.S. Immigration and Customs Enforcement") %>%
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts = n()) %>% 
  arrange(desc(total_ice_contracts))

bay_ice_local_gov_value <- bay_contracts %>% 
  filter(awarding_sub_agency_name == "U.S. Immigration and Customs Enforcement") %>%
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts_value = sum(total_obligated_amount)) %>% 
  arrange(desc(total_ice_contracts_value))

# San Jose accounts for largest value but all contracts are for parking

bay_ice_local_gov_counts$recipient_name <- factor(bay_ice_local_gov_counts$recipient_name ,
                                     levels = bay_ice_local_gov_counts$recipient_name [order(bay_ice_local_gov_counts$total_ice_contracts, decreasing = TRUE)])

bay_ice_local_gov_count_gg <- ggplot(bay_ice_local_gov_counts,
                                aes(x = recipient_name, y = total_ice_contracts)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts), vjust = -0.5) + 
  labs(x = "Recipient",
       y = "Number of total contracts",
       title = "Total Number of ICE - Bay Area Local Gov Contracts",
       subtitle = "2004 - 2022",
       caption =  "Includes any ICE contract a local government where 'primary place of performance' is any of the following counties:
       Alameda, Contra Costa, Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

bay_ice_local_gov_value$recipient_name <- factor(bay_ice_local_gov_value$recipient_name ,
                                                  levels = bay_ice_local_gov_value$recipient_name[order(bay_ice_local_gov_value$total_ice_contracts_value, decreasing = TRUE)])

bay_ice_local_gov_value_gg <- ggplot(bay_ice_local_gov_value,
                                     aes(x = recipient_name,
                                         y = total_ice_contracts_value)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts_value), vjust = -0.5) + 
  labs(x = "Recipient",
       y = "Value of total contracts",
       title = "Total Value of ICE - Bay Area Local Gov Contracts",
       subtitle = "2004 - 2022",
       caption =  "Includes any ICE contract a local government where 'primary place of performance' is any of the following counties:
       Alameda, Contra Costa, Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Explore descriptions of contracts ====
# SANTA CLARA COUNTY
# Of 11 ICE contracts w/ Santa Clara County,

# prime_award_base_transaction_description:
# SLETS (or SCLET?): 5 ("to perform law enforcement investigative research for DRO San Jose sub-office")
# CJIC criminal database access (or CJIS?): 5
# no description: 1

# naics_description:
# "DATA PROCESSING, HOSTING, AND RELATED SERVICES": 6
# none: 4
# "ADMINISTRATION OF PUBLIC HEALTH PROGRAMS": 1 (for CJIC access)

# BAY AREA
# Of 36 ICE contracts w/ local govs in Bay Area:

# prime_award_base_transaction_description
# SLETS (or SCLET): 5 (all w/ Santa Clara County)
# CLETS: 6 (none w/ Santa Clara County; all w/ either Sacramento, Kern, or San Joaquin counties)
# CJIC (or CJIS?): 4 (all w/ Santa Clara County)
# Firing range: 10
# Parking: 4
# Other mentions of databases: 2 ("LAW ENFORCEMENT DATABASE ACCESS", "ONLINE DATABASE SERVICES",)
# no description: 4

# naics_description:
# DATA PROCESSING, HOSTING, AND RELATED SERVICES: 11 
# none: 9 
# ALL OTHER AMUSEMENT AND RECREATION INDUSTRIES: 4 
# PARKING LOTS AND GARAGES: 4 
# ADMINISTRATION OF PUBLIC HEALTH PROGRAMS: 1 
# ALL OTHER MISCELLANEOUS SCHOOLS AND INSTRUCTION: 1 
# COMPUTER FACILITIES MANAGEMENT SERVICES: 1 
# FACILITIES SUPPORT SERVICES: 1 
# LESSORS OF OTHER REAL ESTATE PROPERTY: 1 
# OTHER JUSTICE, PUBLIC ORDER, AND SAFETY ACTIVITIES: 1 
# OTHER TECHNICAL AND TRADE SCHOOLS: 1 
# SECURITY GUARDS AND PATROL SERVICES: 1 

# Export graphs ====
ggsave(plot = sc_ice_count_by_year_gg,
       filename = output_paths$sc_ice_year_count, width = 10, height = 7)

ggsave(plot = sc_ice_value_by_year_gg,
       filename = output_paths$sc_ice_year_value, width = 10, height = 7)

ggsave(plot = bay_ice_count_by_year_gg,
       filename = output_paths$bay_ice_year_count, width = 10, height = 7)

ggsave(plot = bay_ice_value_by_year_gg,
       filename = output_paths$bay_ice_year_value, width = 10, height = 7)

ggsave(plot = bay_ice_local_gov_count_gg,
       filename = output_paths$bay_local_gov_compare_count,
       width = 10, height = 7)

ggsave(plot = bay_ice_local_gov_value_gg,
       filename = output_paths$bay_local_gov_compare_val,
       width = 10, height = 7)

# done.
