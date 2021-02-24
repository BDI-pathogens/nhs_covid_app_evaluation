# Authors: Chris Wymant, Luca Ferretti, Christophe Fraser
# Non-exhaustive list of abbreviations:
# LTLA = lower tier local authority = authority
# SC = synthetic control
# df = dataframe 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(gridExtra)
library(lubridate)
library(geosphere)
library(tibble)

# WARNING: the command below deletes all objects in memory
rm(list = ls()) 

# Define some functions for convenience 
filter <- dplyr::filter
check_df_complete <- function(df) { stopifnot(all(complete.cases(df))) }
stopif <- function(condition) { stopifnot(! condition) }
summarise <- function(...) dplyr::summarise(..., .groups = "drop")


################################################################################
# DEFINE REQUIRED FILES, READ IN, CHECK

dir_data_public  <- "/Users/chris/Dropbox (Infectious Disease)/covid_app_nhs_R/code_for_paper/data_for_paper_public/"

# File with previously calculated list of which authorities neighbour others.
# Loads objects "df_metadata" and "neighbours" into global namespace
file_neighbours_rda <- file.path(dir_data_public, "neighbours_201215.rda")
load(file_neighbours_rda)
ls()

# Uptake data, after transformation from by-district to by-authority:
file_uptake <- file.path(dir_data_public, "uptake_ToyExample_fake_NotRealData.csv")

# Case and death data by authority, from coronavirus.data.gov.uk
file_cases <- file.path(dir_data_public, "ltla_2021-01-11.csv")

# File mapping authorities to country
  file_mapping <- file.path(dir_data_public, "mapping_ltla_to_country.csv")

# Confounders (GDP etc.) by ltla, scraped from ONS and processed 
file_confounders <- file.path(dir_data_public, "ltla_confounders.csv")

# File of mapping between authority names and codes
  file_ons <- file.path(dir_data_public, "ltla_names_and_codes.csv")


# Files of which precalculated (k-mean) cluster each ltla is in, and what its
# synthetic control is.
file_clusters <- file.path(dir_data_public, "clusters_ltla.csv")
file_sc <- file.path(dir_data_public, "sc2_ltla_all.csv")

# Files on poverty
file_poverty_before_housing <- file.path(dir_data_public,"householdsinpovertybhcfye14.txt")
file_poverty_after_housing <- file.path(dir_data_public,"householdsinpovertyahcfye14.txt")
file_poverty_fuel <- file.path(dir_data_public,"fuel-poverty-sub-regional-tables-2020-2018-data.csv")

# File for what UTLA each LTLA is in
file_ltla_to_utla <- file.path(dir_data_public,"Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(April_2019)_Lookup_in_England_and_Wales.txt")

# Files on manual contact tracing
file_manual_tracing_sep <- file.path(dir_data_public,"Regional_contact_tracing_w18__1_.txt")
file_manual_tracing_dec <- file.path(dir_data_public,"regional_contact_tracing_w30.txt")

# Read in those csvs; rename and discard columns to taste
df_uptake <- read_csv(file_uptake) %>%
  rename(pop_authority_app = pop_authority) %>%
  select(-country)    # max = all users, med = all users running normally 
# CHECKED: fraction of users is max/population
df_epi <- read_csv(file_cases,col_types = cols(
  date = col_date(format = ""),
  areaType = col_character(),
  areaCode = col_character(),
  areaName = col_character(),
  newCasesBySpecimenDate = col_double(),
  newDeaths28DaysByDeathDate = col_double(),
  newOnsDeathsByRegistrationDate = col_double()
)
) %>%
  mutate(date = ymd(date)) %>%
  rename(authority = areaName,
         num_cases = newCasesBySpecimenDate,
         num_deaths_daily = newDeaths28DaysByDeathDate,  # absent for Wales
         num_deaths_weekly = newOnsDeathsByRegistrationDate) %>% # often not associated to LTLAs in Wales
  select(-c("areaType", "areaCode"))
df_mapping <- read_csv(file_mapping) %>%
  rename(authority = local_authority) %>%
  select(authority, country, region) %>%
  distinct()
df_confounders <- read_csv(file_confounders)
df_ons <- read_csv(file_ons) %>%
  select(c("lad19cd", "lad19nm"))
df_sc <- read_csv(file_sc) %>%
  rename(authority = areaname)
df_clusters <- read_csv(file_clusters) %>%
  rename(authority = areaname) %>%
  select(authority, clus10, clus30, clus50)


data_povertybhc<-read.csv(file_poverty_before_housing)
data_povertyahc<-read.csv(file_poverty_after_housing)
data_fuel<-read.csv(file_poverty_fuel)
u2l<-read.csv(file_ltla_to_utla)
manualSep<-read.csv(file_manual_tracing_sep)[,1:6]
manualDec<-read.csv(file_manual_tracing_dec)[,1:6]



# Check for no unexpected NAs. There are NAs in case and death numbers, fine.
check_df_complete(df_uptake)
check_df_complete(df_mapping)
stopif(anyNA(df_epi$date))
stopif(anyNA(df_epi$authority))


################################################################################
# MERGE UPTAKE, EPI AND COUNTRY DATA

# Manually override the classification of Dumfries & Galloway and Scottish 
# Borders authorities as English (which is done since they contain a small
# number of English-Welsh app users).
df_mapping[df_mapping$authority %in% c("Dumfries and Galloway",
                                       "Scottish Borders"), ]$country <- "Scotland"

# Remove from the case data those authorities with no cases or deaths ever
# (in the end this is just "Resident Outside Wales").
authorities_without_cases_or_deaths <- df_epi %>%
  group_by(authority) %>%
  summarise(num_cases_authority = sum(num_cases, na.rm = TRUE),
            num_deaths_daily_authority = sum(num_deaths_daily, na.rm = TRUE),
            num_deaths_weekly_authority = sum(num_deaths_weekly, na.rm = TRUE)) %>%
  filter(num_cases_authority == 0L &
           num_deaths_daily_authority == 0L &
           num_deaths_weekly_authority == 0L) %>%
  pull(authority)
df_epi <- df_epi %>%
  filter(! authority %in% authorities_without_cases_or_deaths)

# Add in some extra country labels for certain authorities as labelled as Welsh...
df_mapping <- df_mapping %>%
  bind_rows(tibble(authority = c("Betsi Cadwaladr University Health Board",
                                 "Powys Teaching Health Board",
                                 "Hywel Dda University Health Board",
                                 "Aneurin Bevan University Health Board",
                                 "Cardiff and Vale University Health Board",
                                 "Cwm Taf Morgannwg University Health Board",
                                 "Swansea Bay University Health Board"),
                   country = 'Wales')) %>%
  # ...and English
  bind_rows(tibble(authority = "Hackney and City of London",
                   country = "England",
                   region = "London")) %>%
  # ...and Scottish
  bind_rows(tibble(authority = "Comhairle nan Eilean Siar",
                   country = "Scotland"))

# In uptake data, Merge Hackney + City of London into one, to match epi data
df_uptake <- df_uptake %>%
  bind_rows(df_uptake %>% 
              filter(authority %in% c("City of London", "Hackney")) %>%
              group_by(date) %>%
              summarise(num_users_max = sum(num_users_max),
                        num_users_med = sum(num_users_med),
                        pop_authority_app = sum(pop_authority_app)) %>%
              mutate(authority = "Hackney and City of London",
                     fraction_users = num_users_max / pop_authority_app)) %>%
  filter(! authority %in% c("City of London", "Hackney"))
check_df_complete(df_uptake)

# Buckinghamshire is split into four things in the case data. Merge them.
# Check the sum of cases and deaths over all authorities is the same pre- and
# post-merge.
authorities_bucks <- c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe")
df_epi_bucks <- df_epi %>% 
  filter(authority %in% authorities_bucks) %>%
  group_by(date) %>%
  summarise(num_cases = sum(num_cases, na.rm = TRUE),
            num_deaths_daily = sum(num_deaths_daily, na.rm = TRUE),
            num_deaths_weekly = sum(num_deaths_weekly, na.rm = TRUE)) %>%
  mutate(authority = 'Buckinghamshire')
cases_by_date_pre_merge <- df_epi %>%
  arrange(date) %>%
  group_by(date) %>% 
  summarise(num_cases  = sum(num_cases, na.rm = TRUE),
            num_deaths_daily = sum(num_deaths_daily, na.rm = TRUE),
            num_deaths_weekly = sum(num_deaths_weekly, na.rm = TRUE))
df_epi <- bind_rows(df_epi %>% filter(! authority %in% authorities_bucks),
                    df_epi_bucks)
cases_by_date_post_merge <- df_epi %>%
  arrange(date) %>%
  group_by(date) %>% 
  summarise(num_cases  = sum(num_cases, na.rm = TRUE),
            num_deaths_daily = sum(num_deaths_daily, na.rm = TRUE),
            num_deaths_weekly = sum(num_deaths_weekly, na.rm = TRUE))
stopifnot(identical(cases_by_date_pre_merge, cases_by_date_post_merge))

# Approximate Cornwall alone (population ~570k) as equal to Isles of Scilly
# (population ~ 2k) plus Cornwall
df_uptake[df_uptake$authority == "Cornwall", ]$authority <-
  "Cornwall and Isles of Scilly"
df_mapping[df_mapping$authority == "Cornwall", ]$authority <-
  "Cornwall and Isles of Scilly"

# Merge country data into epi data; remove Scotland and Northern Ireland
stopifnot(all(df_epi$authority %in% df_mapping$authority))
df_epi <- left_join(df_epi, df_mapping, by = "authority") %>%
  filter(country %in% c("England", "Wales"))
stopif(anyNA(df_epi$date))
stopif(anyNA(df_epi$authority))
stopif(anyNA(df_epi$country))

# There are no authorities that are in uptake data but not in epi: important
# There are some authorities that are in epi data but not in uptake: the Welsh
# University Health Boards, where they record most deaths but no cases. This is 
# why we have to exclude Wales when predicting octnovdec deaths or controlling for
# old deaths.
authorities_epi <- unique(df_epi$authority)
authorities_uptake <- unique(df_uptake$authority)
stopifnot(all(df_uptake$authority %in% df_epi$authority))
#authorities_epi[!authorities_epi %in% authorities_uptake]

# Take the mean uptake in the desired date range
df_uptake_mean <- df_uptake %>%
  filter(date >= ymd('2020-11-01') &
           date <= ymd('2020-12-11')) %>%
  group_by(authority) %>%
  summarise(fraction_users = mean(fraction_users))

#check that users are relatively constant across areas over time. 
df_uptake_range <- df_uptake %>%
  filter(date >= ymd('2020-11-01') &
           date <= ymd('2020-12-11'))
#anova(lm(fraction_users~authority,data = df_uptake_range))  

plot <- FALSE
if (plot) {
  p <- ggplot(df_uptake_mean) +
    geom_histogram(aes(fraction_users)) +
    theme_classic() +
    coord_cartesian(expand = F)
  p
  ggsave("/Users/chrisw/app-data-analysis/uptake.pdf", width = 6, height = 6)
}

# Sum cases post-app
df_cases_phase_app_total <- df_epi %>%
  filter(date >= ymd('2020-10-08') & 
           date <=ymd('2020-12-31'))%>%
  group_by(authority, country) %>%
  summarise(cases_phase_app_total = sum(num_cases, na.rm = TRUE))

# Sum cases in phase 0, pre app, Aug to Sept
df_cases_phase0_total <- df_epi %>%
  filter(date >= ymd('2020-08-01') &
           date <= ymd("2020-09-30")) %>%
  group_by(authority) %>%
  summarise(cases_phase0_total = sum(num_cases, na.rm = TRUE))

# Sum cases in phase 1, app version <3.9
df_cases_phase1_total <- df_epi %>%
  filter(date >= ymd('2020-10-08') &
           date <= ymd("2020-11-06")) %>%
  group_by(authority) %>%
  summarise(cases_phase1_total = sum(num_cases, na.rm = TRUE))

# Sum cases in phase 2, app version >=3.9 until new risk score on 23 Dec)
df_cases_phase2_total <- df_epi %>%
  filter(date >= ymd("2020-11-07") & 
           date <= ymd("2020-12-31")) %>%
  group_by(authority) %>%
  summarise(cases_phase2_total = sum(num_cases, na.rm = TRUE))

# Sum cases in phase 3, new risk score
df_cases_phase3_total <- df_epi %>%
  filter(date >= ymd("2021-01-01")) %>%
  group_by(authority) %>%
  summarise(cases_phase3_total = sum(num_cases, na.rm = TRUE))

# Sum cases post-app
df_cases_novdec_total <- df_epi %>%
  filter(date >= ymd('2020-11-01')) %>%
  group_by(authority) %>%
  summarise(cases_novdec_total = sum(num_cases, na.rm = TRUE))

# Sum cases in July, August & September
df_cases_julaugsep_total <- df_epi %>%
  filter(date >= ymd('2020-07-01') &
           date <= ymd("2020-09-30")) %>%
  group_by(authority) %>%
  summarise(cases_julaugsep_total = sum(num_cases, na.rm = TRUE))

# Sum cases in July
df_cases_jul_total <- df_epi %>%
  filter(date >= ymd('2020-07-01') &
           date <= ymd("2020-07-31")) %>%
  group_by(authority) %>%
  summarise(cases_jul_total = sum(num_cases, na.rm = TRUE))

# Sum cases in August
df_cases_aug_total <- df_epi %>%
  filter(date >= ymd('2020-08-01') &
           date <= ymd("2020-08-31")) %>%
  group_by(authority) %>%
  summarise(cases_aug_total = sum(num_cases, na.rm = TRUE))

# Sum cases in September
df_cases_sep_total <- df_epi %>%
  filter(date >= ymd('2020-09-01') &
           date <= ymd("2020-09-30")) %>%
  group_by(authority) %>%
  summarise(cases_sep_total = sum(num_cases, na.rm = TRUE))

# Sum cases in Oct
df_cases_oct_total <- df_epi %>%
  filter(date >= ymd('2020-10-01') &
           date <= ymd("2020-10-31")) %>%
  group_by(authority) %>%
  summarise(cases_oct_total = sum(num_cases, na.rm = TRUE))

# Sum cases in Nov
df_cases_nov_total <- df_epi %>%
  filter(date >= ymd('2020-11-01') &
           date <= ymd("2020-11-30")) %>%
  group_by(authority) %>%
  summarise(cases_nov_total = sum(num_cases, na.rm = TRUE))

# Sum cases in Dec
df_cases_dec_total <- df_epi %>%
  filter(date >= ymd('2020-12-01') &
           date <= ymd("2020-12-31")) %>%
  group_by(authority) %>%
  summarise(cases_dec_total = sum(num_cases, na.rm = TRUE))

# Sum cases in Jan
df_cases_jan_total <- df_epi %>%
  filter(date >= ymd('2021-01-01')) %>%
  group_by(authority) %>%
  summarise(cases_jan_total = sum(num_cases, na.rm = TRUE))


# Sum first wave deaths, daily and weekly
df_deaths_total_from_daily_old <- df_epi %>%
  filter(date < ymd('2020-06-01')) %>%
  group_by(authority) %>%
  summarise(deaths_total_from_daily_old = sum(num_deaths_daily, na.rm = TRUE))
df_deaths_total_from_weekly_old <- df_epi %>%
  filter(date < ymd('2020-06-01')) %>%
  group_by(authority, country) %>%
  summarise(deaths_total_from_weekly_old = sum(num_deaths_weekly, na.rm = TRUE))

# Sum octnovdec deaths, daily and weekly
df_deaths_total_from_daily_phase_app <- df_epi %>%
  filter(date >= ymd('2020-10-15')) %>%
  group_by(authority) %>%
  summarise(deaths_total_from_daily_phase_app = sum(num_deaths_daily, na.rm = TRUE))
df_deaths_total_from_weekly_phase_app <- df_epi %>%
  filter(date >= ymd('2020-10-15')) %>%
  group_by(authority) %>%
  summarise(deaths_total_from_weekly_phase_app = sum(num_deaths_weekly, na.rm = TRUE))

# Check for NAs
check_df_complete(df_cases_phase_app_total)
check_df_complete(df_deaths_total_from_daily_old)
check_df_complete(df_deaths_total_from_weekly_old)
check_df_complete(df_deaths_total_from_daily_phase_app)
check_df_complete(df_deaths_total_from_weekly_phase_app)
check_df_complete(df_cases_novdec_total)
check_df_complete(df_cases_sep_total)

# Inspect first-wave Welsh deaths: mostly associated to University Boards rather
# than LTLAs, so can't use death data for Wales :(
#df_deaths_total_from_weekly_old %>%
#  filter(country == "Wales") %>%
#  arrange(deaths_total_from_weekly_old) %>%
#  as.data.frame()

# Merge mean uptake with case and death totals
df <- left_join(df_uptake_mean,
                df_cases_phase_app_total,
                by = "authority") %>%
  left_join(df_cases_julaugsep_total, by = "authority") %>%
  left_join(df_cases_novdec_total, by = "authority") %>%
  left_join(df_cases_aug_total, by = "authority") %>%
  left_join(df_cases_jul_total, by = "authority") %>%
  left_join(df_cases_sep_total, by = "authority") %>%
  left_join(df_cases_oct_total, by = "authority") %>%
  left_join(df_cases_nov_total, by = "authority") %>%
  left_join(df_cases_dec_total, by = "authority") %>%
  left_join(df_cases_jan_total, by = "authority")  %>%
  left_join(df_cases_phase0_total, by = "authority") %>%
  left_join(df_cases_phase1_total, by = "authority") %>%
  left_join(df_cases_phase2_total, by = "authority") %>%
  left_join(df_cases_phase3_total, by = "authority") %>%
  left_join(df_deaths_total_from_daily_old, by = "authority") %>%
  left_join(df_deaths_total_from_daily_phase_app, by = "authority") 

check_df_complete(df)
stopifnot(all(df$authority %in% df_mapping$authority))
df <- left_join(df, df_mapping %>% select(-country), by = "authority")

# Merge in confounders. Some are NA for Wales only.
stopifnot(all(df$authority %in% df_confounders$authority))
df <- left_join(df, df_confounders, by = "authority") %>%
  rename(pop_authority = population_2018)
check_df_complete(df %>% filter(country == "England"))

# Define per capita total cases and deaths
# (use daily not weekly death data; only weekly is available for Wales, but in
# the end we found it's not at the LTLA level - it's mainly by University Health
# Board).
df <- df %>%
  mutate(cases_phase_app_total_per_capita = cases_phase_app_total / pop_authority,
         cases_julaugsep_total_per_capita = cases_julaugsep_total / pop_authority,
         deaths_total_from_daily_phase_app_per_capita = deaths_total_from_daily_phase_app / pop_authority,
         deaths_total_from_daily_old_per_capita = deaths_total_from_daily_old / pop_authority,
         cases_aug_total_per_capita = cases_aug_total / pop_authority,
         cases_jul_total_per_capita = cases_jul_total / pop_authority,
         cases_sep_total_per_capita = cases_sep_total / pop_authority,
         cases_oct_total_per_capita = cases_oct_total / pop_authority,
         cases_nov_total_per_capita = cases_nov_total / pop_authority,
         cases_dec_total_per_capita = cases_dec_total / pop_authority,
         cases_jan_total_per_capita = cases_jan_total / pop_authority,
         cases_phase0_total_per_capita = cases_phase0_total / pop_authority,
         cases_phase1_total_per_capita = cases_phase1_total / pop_authority,
         cases_phase2_total_per_capita = cases_phase2_total / pop_authority,
         cases_phase3_total_per_capita = cases_phase3_total / pop_authority,
         cases_novdec_total_per_capita = cases_novdec_total / pop_authority)
check_df_complete(df %>% filter(country == "England"))

# Merge in cluster info
stopifnot(identical(sort(df %>% filter(country == "England") %>% pull(authority)),
                    sort(df_clusters$authority)))
df <- left_join(df, df_clusters, by = "authority")

# For each authority, calculate cases and uptake relative to the rest of the
# authorities in the same clus10 group (only available for England).
df_england <- df %>% filter(country == "England")
df$cases_phase_app_total_per_capita_rel_to_other_clus10 <- NA_real_
df$fraction_users_rel_to_other_clus10 <- NA_real_
for (this_authority in df_england$authority) {
  
  this_clus_10 <- df_england %>% filter(authority == this_authority) %>% pull(clus10)
  df_other_authorities_same_clus10 <- df_england %>%
    filter(clus10 == this_clus_10 &
             authority != this_authority)
  if (nrow(df_other_authorities_same_clus10) == 0L) next
  
  df[df$authority == this_authority, ]$cases_phase_app_total_per_capita_rel_to_other_clus10 <-
    df[df$authority == this_authority, ]$cases_phase_app_total_per_capita -
    sum(df_other_authorities_same_clus10$cases_phase_app_total) / 
    sum(df_other_authorities_same_clus10$pop_authority)
  
  df[df$authority == this_authority, ]$fraction_users_rel_to_other_clus10 <-
    df[df$authority == this_authority, ]$fraction_users -
    sum(df_other_authorities_same_clus10$fraction_users *
          df_other_authorities_same_clus10$pop_authority) / 
    sum(df_other_authorities_same_clus10$pop_authority)
  
}

# Same as before, now for clus30 instead of clus10
df_england <- df %>% filter(country == "England")
df$cases_phase_app_total_per_capita_rel_to_other_clus30 <- NA_real_
df$fraction_users_rel_to_other_clus30 <- NA_real_
for (this_authority in df_england$authority) {
  
  this_clus_30 <- df_england %>% filter(authority == this_authority) %>% pull(clus30)
  df_other_authorities_same_clus30 <- df_england %>%
    filter(clus30 == this_clus_30 &
             authority != this_authority)
  if (nrow(df_other_authorities_same_clus30) == 0L) next
  
  df[df$authority == this_authority, ]$cases_phase_app_total_per_capita_rel_to_other_clus30 <-
    df[df$authority == this_authority, ]$cases_phase_app_total_per_capita -
    sum(df_other_authorities_same_clus30$cases_phase_app_total) / 
    sum(df_other_authorities_same_clus30$pop_authority)
  
  df[df$authority == this_authority, ]$fraction_users_rel_to_other_clus30 <-
    df[df$authority == this_authority, ]$fraction_users -
    sum(df_other_authorities_same_clus30$fraction_users *
          df_other_authorities_same_clus30$pop_authority) / 
    sum(df_other_authorities_same_clus30$pop_authority)
  
}

stopifnot(identical(sort(df_sc$authority),
                    sort(df %>% filter(country == "England") %>% pull(authority))))

# Merge synthetic control data
df_sc <- left_join(df_sc,
                   df %>% select(authority, fraction_users,
                                 cases_phase_app_total_per_capita,
                                 cases_phase0_total_per_capita,
                                 cases_phase1_total_per_capita,
                                 cases_phase2_total_per_capita,
                                 cases_phase2_total_per_capita,
                                 cases_sep_total_per_capita), 
                   by = "authority")

# Calculate properties of each LTLA relative to its SC
df$cases_phase_app_total_per_capita_rel_sc <- NA_real_
df$cases_phase0_total_per_capita_rel_sc <- NA_real_
df$cases_phase1_total_per_capita_rel_sc <- NA_real_
df$cases_phase2_total_per_capita_rel_sc <- NA_real_
df$cases_sep_total_per_capita_rel_sc <- NA_real_
df$fraction_users_rel_sc <- NA_real_
for (authority_num in 1:nrow(df_sc)) {
  this_authority <- df_sc[authority_num, ]$authority
  sc_weights <- df_sc[[paste0("sc_", authority_num)]] 
  df[df$authority == this_authority, ]$cases_phase_app_total_per_capita_rel_sc <-
    df[df$authority == this_authority, ]$cases_phase_app_total_per_capita -
    sum(sc_weights * df_sc$cases_phase_app_total_per_capita, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  df[df$authority == this_authority, ]$cases_phase0_total_per_capita_rel_sc <-
    df[df$authority == this_authority, ]$cases_phase0_total_per_capita -
    sum(sc_weights * df_sc$cases_phase0_total_per_capita, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  df[df$authority == this_authority, ]$cases_phase1_total_per_capita_rel_sc <-
    df[df$authority == this_authority, ]$cases_phase1_total_per_capita -
    sum(sc_weights * df_sc$cases_phase1_total_per_capita, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  df[df$authority == this_authority, ]$cases_phase2_total_per_capita_rel_sc <-
    df[df$authority == this_authority, ]$cases_phase2_total_per_capita -
    sum(sc_weights * df_sc$cases_phase2_total_per_capita, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  #df[df$authority == this_authority, ]$cases_phase3_total_per_capita_rel_sc <-
  #  df[df$authority == this_authority, ]$cases_phase3_total_per_capita -
  #  sum(sc_weights * df_sc$cases_phase3_total_per_capita, na.rm = TRUE) /
  #  sum(sc_weights, na.rm = TRUE)
  df[df$authority == this_authority, ]$cases_sep_total_per_capita_rel_sc <-
    df[df$authority == this_authority, ]$cases_sep_total_per_capita -
    sum(sc_weights * df_sc$cases_sep_total_per_capita, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  df[df$authority == this_authority, ]$fraction_users_rel_sc <-
    df[df$authority == this_authority, ]$fraction_users -
    sum(sc_weights * df_sc$fraction_users, na.rm = TRUE) /
    sum(sc_weights, na.rm = TRUE)
  
}


# data on poverty before housing costs
povertybhc<-aggregate(data_povertybhc$Percentage.of.Households.Below.60..of.the.Median.Income...before.housing.costs.,list(ltla=data_povertybhc$Local.authority.name),function(x){mean(x,na.rm=T)})
rownames(povertybhc)<-as.character(povertybhc[,1])
povertybhc<-rbind(povertybhc, list("Bournemouth, Christchurch and Poole",mean(povertybhc[c("Bournemouth","Christchurch","Poole"),2])))
povertybhc<-rbind(povertybhc,list("Buckinghamshire" ,mean(povertybhc[c("Aylesbury Vale", "Chiltern", "South Bucks",  "Wycombe" ),2])))
povertybhc<-rbind(povertybhc,list("Cornwall and Isles of Scilly" ,mean(povertybhc[c("Cornwall", "Isles of Scilly"),2])))
povertybhc<-rbind(povertybhc,list("Dorset" ,mean(povertybhc[c("West Dorset", "East Dorset", "North Dorset", "Weymouth and Portland", "Purbeck" ),2])))
povertybhc<-rbind(povertybhc,list("East Suffolk" ,mean(povertybhc[c("Suffolk Coastal", "Waveney"),2])))
povertybhc<-rbind(povertybhc,list("Folkestone and Hythe" ,mean(povertybhc[c( "Shepway" ),2])))
povertybhc<-rbind(povertybhc,list("Hackney and City of London" ,mean(povertybhc[c("Hackney" , "City of London" ),2])))
povertybhc<-rbind(povertybhc,list("Somerset West and Taunton"  ,mean(povertybhc[c("West Somerset", "Taunton Deane" ),2])))
povertybhc<-rbind(povertybhc,list("West Suffolk" ,mean(povertybhc[c("Forest Heath", "St Edmundsbury"),2])))
povertybhc<-rbind(povertybhc,list("Vale of Glamorgan" ,mean(povertybhc[c("The Vale of Glamorgan"),2])))
colnames(povertybhc)<-c("authority","fraction in poverty")
rownames(povertybhc)<-as.character(povertybhc[,1])
df$povertybhc<-povertybhc[df$authority,2]

# data on poverty after housing costs
povertyahc<-aggregate(data_povertyahc$Percentage.of.Households.Below.60..of.the.Median.Income...after.housing.costs.,list(ltla=data_povertyahc$Local.authority.name),function(x){mean(x,na.rm=T)})
rownames(povertyahc)<-as.character(povertyahc[,1])
povertyahc<-rbind(povertyahc, list("Bournemouth, Christchurch and Poole",mean(povertyahc[c("Bournemouth","Christchurch","Poole"),2])))
povertyahc<-rbind(povertyahc,list("Buckinghamshire" ,mean(povertyahc[c("Aylesbury Vale", "Chiltern", "South Bucks",  "Wycombe" ),2])))
povertyahc<-rbind(povertyahc,list("Cornwall and Isles of Scilly" ,mean(povertyahc[c("Cornwall", "Isles of Scilly"),2])))
povertyahc<-rbind(povertyahc,list("Dorset" ,mean(povertyahc[c("West Dorset", "East Dorset", "North Dorset", "Weymouth and Portland", "Purbeck" ),2])))
povertyahc<-rbind(povertyahc,list("East Suffolk" ,mean(povertyahc[c("Suffolk Coastal", "Waveney"),2])))
povertyahc<-rbind(povertyahc,list("Folkestone and Hythe" ,mean(povertyahc[c( "Shepway" ),2])))
povertyahc<-rbind(povertyahc,list("Hackney and City of London" ,mean(povertyahc[c("Hackney" , "City of London" ),2])))
povertyahc<-rbind(povertyahc,list("Somerset West and Taunton"  ,mean(povertyahc[c("West Somerset", "Taunton Deane" ),2])))
povertyahc<-rbind(povertyahc,list("West Suffolk" ,mean(povertyahc[c("Forest Heath", "St Edmundsbury"),2])))
povertyahc<-rbind(povertyahc,list("Vale of Glamorgan" ,mean(povertyahc[c("The Vale of Glamorgan"),2])))
colnames(povertyahc)<-c("authority","fraction in poverty")
rownames(povertyahc)<-as.character(povertyahc[,1])
df$povertyahc<-povertyahc[df$authority,2]

# data on fuel poverty
data_fuel<-data_fuel[,c("LA.Name","Number.of.households.in.fuel.poverty1","Number.of.households1")]
data_fuel[,2]<-data_fuel[,2]/data_fuel[,3]
data_fuel<-data_fuel[,1:2]
rownames(data_fuel)<-as.character(data_fuel$LA.Name)
data_fuel<-rbind(data_fuel, list("Bournemouth, Christchurch and Poole",mean(data_fuel[c("Bournemouth","Christchurch","Poole"),2])))
data_fuel<-rbind(data_fuel,list("Buckinghamshire" ,mean(data_fuel[c("Aylesbury Vale", "Chiltern", "South Bucks",  "Wycombe" ),2])))
data_fuel<-rbind(data_fuel,list("Cornwall and Isles of Scilly" ,mean(data_fuel[c("Cornwall", "Isles of Scilly"),2])))
data_fuel<-rbind(data_fuel,list("Dorset" ,mean(data_fuel[c("West Dorset", "East Dorset", "North Dorset", "Weymouth and Portland", "Purbeck" ),2])))
data_fuel<-rbind(data_fuel,list("East Suffolk" ,mean(data_fuel[c("Suffolk Coastal", "Waveney"),2])))
data_fuel<-rbind(data_fuel,list("Folkestone and Hythe" ,mean(data_fuel[c( "Shepway" ),2])))
data_fuel<-rbind(data_fuel,list("Hackney and City of London" ,mean(data_fuel[c("Hackney" , "City of London" ),2])))
data_fuel<-rbind(data_fuel,list("Somerset West and Taunton"  ,mean(data_fuel[c("West Somerset", "Taunton Deane" ),2])))
data_fuel<-rbind(data_fuel,list("West Suffolk" ,mean(data_fuel[c("Forest Heath", "St Edmundsbury"),2])))
data_fuel<-rbind(data_fuel,list("Bristol, City of" ,mean(data_fuel[c("Bristol City of"),2])))
data_fuel<-rbind(data_fuel,list("Herefordshire, County of" ,mean(data_fuel[c("Herefordshire County of"),2])))
data_fuel<-rbind(data_fuel,list("Kingston upon Hull, City of" ,mean(data_fuel[c("Kingston upon Hull City of"),2])))
rownames(data_fuel)<-as.character(data_fuel$LA.Name)
fuelpoverty<-data_fuel[df$authority,2]
df$fuel<-fuelpoverty

# manual contact tracing data (England only)
rownames(u2l)<-as.character(u2l$LTLA19NM)
manualSep<-manualSep[order(manualSep$Category,manualSep$Upper.Tier.Local.Authority),]
manualSep_cases<-manualSep[manualSep$Category=="People transferred to contact tracing system",]
manualSep_contacts<-manualSep[manualSep$Category=="Contacts identified who were not managed by local health protection teams",]
rownames(manualSep_cases)<-as.character(manualSep_cases$Upper.Tier.Local.Authority)
rownames(manualSep_contacts)<-as.character(manualSep_contacts$Upper.Tier.Local.Authority)

manualDec<-manualDec[order(manualDec$Category,manualDec$Upper.Tier.Local.Authority),]
manualDec_cases<-manualDec[manualDec$Category=="People transferred to contact tracing system",]
manualDec_contacts<-manualDec[manualDec$Category=="Contacts identified who were not managed by local health protection teams",]
rownames(manualDec_cases)<-as.character(manualDec_cases$Upper.Tier.Local.Authority)
rownames(manualDec_contacts)<-as.character(manualDec_contacts$Upper.Tier.Local.Authority)

manual<-manualDec; manual[,4:5]<-manualDec[,4:5]-manualSep[,4:5]; manual[,6]<-manual[,5]/manual[,4]
manual_cases<-manual[manual$Category=="People transferred to contact tracing system",]
manual_contacts<-manual[manual$Category=="Contacts identified who were not managed by local health protection teams",]
rownames(manual_cases)<-as.character(manual_cases$Upper.Tier.Local.Authority)
rownames(manual_contacts)<-as.character(manual_contacts$Upper.Tier.Local.Authority)

manual_rates<-t(sapply(1:dim(df)[1],function(i){
  if(df$country[i]=="Wales"){
    return(c(NA,NA))
  } else {
    utla<-u2l[df$authority[i],]$UTLA19NM
    if(df$authority[i]=="Cornwall and Isles of Scilly") {utla <- "Cornwall & Isles of Scilly"}
    if(df$authority[i]=="Hackney and City of London"){utla <- "City of London & Hackney"}
    if(df$authority[i]=="Buckinghamshire"){utla <- "Buckinghamshire"}
    c(manual_cases[utla,]$Percentage.reached,manual_contacts[utla,]$Percentage.reached)
  }
}))
df$manual_cases<-manual_rates[,1]
df$manual_contacts<-manual_rates[,2]

stopif(is.na(df$manual_cases[df$country=="England"]))
stopif(is.na(df$manual_contacts[df$country=="England"]))


################################################################################
# CALCULATE CASES IN NEIGHBOURING OR CONNECTED AUTHORITIES

# neighbours is a list (of vectors) with extra properties; give names to the
# list elements. The nth row of df_metadata has id n, so the name of the nth
# element of neighbours can be set to nth element of the 2nd (name) column of
# df_metadata
df_metadata$id<-as.numeric(df_metadata$id)
stopifnot(all(df_metadata$id == 1:nrow(df_metadata))) 
names(neighbours) <- df_metadata[, 2]  
names(neighbours)[names(neighbours) == "Cornwall"] <- "Cornwall and Isles of Scilly"

# Compare the authorities in neighbours with those in our df.
# The difference is Buckinghamshire being split into 4 in the neighbours file.
authorities_neighbours <- names(neighbours)
authorities_df <- df$authority
#authorities_df[! authorities_df %in% authorities_neighbours]
#authorities_neighbours[! authorities_neighbours %in% authorities_df]

# Add an extra element at the end of neighbours that is Buckinghamshire, merged.
bucks_sub_authority_names <- c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe" )
bucks_sub_authority_codes <- which(authorities_neighbours %in% bucks_sub_authority_names)
bucks_neighbour_codes <- unique(unname(unlist(neighbours[bucks_sub_authority_codes])))
bucks_neighbour_codes <- bucks_neighbour_codes[! bucks_neighbour_codes %in% bucks_sub_authority_codes]
neighbours <- c(neighbours, list(bucks_neighbour_codes))
bucks_merged_code <- length(neighbours)
names(neighbours)[[bucks_merged_code]] <- "Buckinghamshire"
authorities_neighbours <- names(neighbours)

# But there are still authorities with a vector of neighbours containing a 
# sub-part of Bucks as one of their neighbours:
num_authorities_neighbouring_bucks_subpart <- 
  sum(unlist(lapply(neighbours, function(l) bucks_sub_authority_codes %in% l)))
#num_authorities_neighbouring_bucks_subpart

# So replace any appearance of a sub-part of Bucks (as a neighbour to somewhere
# else) with the new merged Bucks 
for (authority in names(neighbours)) {
  neighbours[[authority]] <- unique(replace(neighbours[[authority]],
                                            neighbours[[authority]] %in% bucks_sub_authority_codes,
                                            bucks_merged_code))
}
num_authorities_neighbouring_bucks_subpart <- 
  sum(unlist(lapply(neighbours, function(l) bucks_sub_authority_codes %in% l)))
stopifnot(num_authorities_neighbouring_bucks_subpart == 0L)

# Replace codes by names
authorities_neighbours <- names(neighbours)
neighbours <- lapply(neighbours, function(l) authorities_neighbours[l])

# Delete the four sub-parts of Bucks
neighbours <- neighbours[! names(neighbours) %in% bucks_sub_authority_names ]
authorities_neighbours <- names(neighbours)

# Check no NAs in any of the vectors of neighbour names
stopif(anyNA(unlist(lapply(neighbours, function(l) anyNA(l)))))

# authorities match in the list for what neighbours you have, and our df
stopifnot(identical(sort(df$authority),
                    sort(authorities_neighbours)))

# authorities don't match in the list of all values for neighbours and our df,
# because Islands do not appear as anyone's neighbour. Fine.
authorities_neighbours_all_values <- unique(unlist(neighbours))
#authorities_df[! authorities_df %in% authorities_neighbours]
#authorities_neighbours[! authorities_neighbours %in% authorities_neighbours_all_values]

#df$authority == names(neighbours)
#sort(names(neighbours_in_same_quintile))
#order(names(neighbours_in_same_quintile))
neighbours <- neighbours[order(names(neighbours))]
stopifnot(sum(df$authority == names(neighbours)) == nrow(df))

# Sum the cases from all of your neighbours, for octnovdec and dec only
cases_phase_app_total_vector <- df$cases_phase_app_total
names(cases_phase_app_total_vector) <- df$authority
cases_phase_app_total_in_neighbours <- unlist(lapply(neighbours, function(l) sum(cases_phase_app_total_vector[l])))
stopif(anyNA(cases_phase_app_total_in_neighbours))
cases_dec_total_vector <- df$cases_dec_total
names(cases_dec_total_vector) <- df$authority
cases_dec_total_in_neighbours <- unlist(lapply(neighbours, function(l) sum(cases_dec_total_vector[l])))
stopif(anyNA(cases_dec_total_in_neighbours))

df_original<-df

#####
#START  BOOTSTRAP HERE
# Skip this section if you don't want to bootstrap the analysis...

#n_bootstraps<-10000 # many bootstraps, for actual inference
n_bootstraps<-5 # 5 bootstraps, just for demonstration

df_bootstrap<-list()

bootstrap_pairs<-function(x){
  mynames<-names(x)
  myn<-sapply(x,length); names(myn)<-mynames
  n<-sum(myn)
  edge_matrix<-cbind(rep(mynames,times=myn),unlist(x))[sample(1:n),]
  new_matrix<-c()
  for(i in 1:n){
    v<-edge_matrix[i,]
    test1<-all(v[1]!=new_matrix[,2] & v[2]!=new_matrix[,1])
    test2<-all(table(c(new_matrix[new_matrix[,1] %in% v,2],new_matrix[new_matrix[,2] %in% v,1]))<=1)
    if(test1 & test2) {new_matrix<-rbind(new_matrix,v)}
  }
  new_n<-dim(new_matrix)[1]
  new_matrix<-new_matrix[sample(1:new_n,n,replace=T),]
  result<- lapply(mynames,function(name){new_matrix[new_matrix[,1]==name,2]}) 
  names(result)<-mynames
  return( result )
}



for(bootstrap in 1:n_bootstraps){
  print(bootstrap)
  df<-df_original
  
  
  
  # Create a list of matched neighbours whose cases in September were in the same quintile
  df$quintile_sep_cases_percapita <- cut(df$cases_sep_total_per_capita, 
                                         breaks = quantile(df$cases_sep_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                         include.lowest=TRUE,
                                         labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  df$quintile_phase0_cases_percapita <- cut(df$cases_phase0_total_per_capita, 
                                            breaks = quantile(df$cases_phase0_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                            include.lowest=TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  neighbours_in_same_quintile_phase0 <- vector(mode = "list", length=length(neighbours))
  names(neighbours_in_same_quintile_phase0) <- names(neighbours)
  for(area in names(neighbours)){
    target.quintile <- df[df$authority==area,]$quintile_phase0_cases_percapita
    matched.areas <- vector()
    for(candidate in neighbours[[area]]){
      test.quintile <- df[df$authority==candidate,]$quintile_phase0_cases_percapita
      if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
    }
    neighbours_in_same_quintile_phase0[[area]] <- matched.areas
  }
  hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
  hist(lengths(neighbours_in_same_quintile_phase0), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours", breaks = 0:7)
  
  df_figure_4A <- df %>% 
    select(authority, quintile_phase0_cases_percapita)
  save(neighbours_in_same_quintile_phase0, df_figure_4A, file = "Figure4AData.Rda")
  
  
  
  df$quintile_phase1_cases_percapita <- cut(df$cases_phase1_total_per_capita, 
                                            breaks = quantile(df$cases_phase1_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                            include.lowest=TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  neighbours_in_same_quintile_phase1 <- vector(mode = "list", length=length(neighbours))
  names(neighbours_in_same_quintile_phase1) <- names(neighbours)
  for(area in names(neighbours)){
    target.quintile <- df[df$authority==area,]$quintile_phase1_cases_percapita
    matched.areas <- vector()
    for(candidate in neighbours[[area]]){
      test.quintile <- df[df$authority==candidate,]$quintile_phase1_cases_percapita
      if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
    }
    neighbours_in_same_quintile_phase1[[area]] <- matched.areas
  }
  #hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
  #hist(lengths(neighbours_in_same_quintile_phase1), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours")
  
  df$quintile_phase2_cases_percapita <- cut(df$cases_phase2_total_per_capita, 
                                            breaks = quantile(df$cases_phase2_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                            include.lowest=TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  neighbours_in_same_quintile_phase2 <- vector(mode = "list", length=length(neighbours))
  names(neighbours_in_same_quintile_phase2) <- names(neighbours)
  for(area in names(neighbours)){
    target.quintile <- df[df$authority==area,]$quintile_phase2_cases_percapita
    matched.areas <- vector()
    for(candidate in neighbours[[area]]){
      test.quintile <- df[df$authority==candidate,]$quintile_phase2_cases_percapita
      if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
    }
    neighbours_in_same_quintile_phase2[[area]] <- matched.areas
  }
  #hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
  #hist(lengths(neighbours_in_same_quintile_phase1), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours")
  
  # bootstrap neighbours here
  neighbours_in_same_quintile_phase0 <- bootstrap_pairs(neighbours_in_same_quintile_phase0)
  neighbours_in_same_quintile_phase1 <- bootstrap_pairs(neighbours_in_same_quintile_phase1)
  neighbours_in_same_quintile_phase2 <- bootstrap_pairs(neighbours_in_same_quintile_phase2)
  
  df$n_neighbours_in_same_quintile_phase0 <- lengths(neighbours_in_same_quintile_phase0)
  df$n_neighbours_in_same_quintile_phase1 <- lengths(neighbours_in_same_quintile_phase1)
  df$n_neighbours_in_same_quintile_phase2 <- lengths(neighbours_in_same_quintile_phase2)
  
  # Sum the cases from all of your matched neighbours
  cases_phase0_total_vector <- df$cases_phase0_total
  cases_phase_app_total_vector <- df$cases_phase_app_total
  cases_phase1_total_vector <- df$cases_phase1_total
  cases_phase2_total_vector <- df$cases_phase2_total
  cases_phase3_total_vector <- df$cases_phase3_total
  cases_dec_total_vector <- df$cases_dec_total
  names(cases_phase0_total_vector) <- df$authority
  names(cases_phase1_total_vector) <- df$authority
  names(cases_phase2_total_vector) <- df$authority
  names(cases_phase3_total_vector) <- df$authority
  names(cases_dec_total_vector) <- df$authority
  names(cases_phase_app_total_vector) <- df$authority
  #names(R_phase2_vector) <- df$authority
  cases_phase0_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase0_total_vector[l])))
  cases_phase_app_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase_app_total_vector[l])))
  cases_phase1_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase1_total_vector[l])))
  cases_phase2_total_in_phase1_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) sum(cases_phase2_total_vector[l])))
  cases_phase3_total_in_phase2_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) sum(cases_phase3_total_vector[l])))
  cases_phase2_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase2_total_vector[l])))
  cases_phase3_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase3_total_vector[l])))
  
  
  stopif(anyNA(cases_phase0_total_in_phase0_matched_neighbours))
  stopif(anyNA(cases_phase_app_total_in_phase0_matched_neighbours))
  
  # Merge that into the main df. Normalise your neighbours' cases by your population.
  df_neighbours <- tibble(authority = names(cases_phase_app_total_in_neighbours),
                          neighbours_cases_phase_app_total = cases_phase_app_total_in_neighbours,
                          neighbours_cases_dec_total = cases_dec_total_in_neighbours)
  stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase0_neighbours_cases_phase0_total = 
      cases_phase0_total_in_phase0_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase0_neighbours_cases_phase_app_total = 
      cases_phase_app_total_in_phase0_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase0_neighbours_cases_phase1_total = 
      cases_phase1_total_in_phase0_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase1_neighbours_cases_phase2_total = 
      cases_phase2_total_in_phase1_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase2_neighbours_cases_phase3_total = 
      cases_phase3_total_in_phase2_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase0_neighbours_cases_phase2_total = 
      cases_phase2_total_in_phase0_matched_neighbours)
  df_neighbours <- df_neighbours %>% add_column(
    matched_phase0_neighbours_cases_phase3_total = 
      cases_phase3_total_in_phase0_matched_neighbours)
  df <- left_join(df, df_neighbours, by = "authority")
  df <- df %>%
    mutate(neighbours_cases_phase_app_total_normed_to_me = neighbours_cases_phase_app_total / pop_authority)
  
  # Sum the population of all your neighbours, and matched neighbours and merge into df
  pop_sizes <- df$pop_authority
  names(pop_sizes) <- df$authority
  neighbours_pop_size <- unlist(lapply(neighbours, function(l) sum(pop_sizes[l])))
  matched_phase0_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(pop_sizes[l])))
  matched_phase1_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) sum(pop_sizes[l])))
  matched_phase2_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) sum(pop_sizes[l])))
  df_neighbours <- tibble(authority = names(neighbours_pop_size),
                          neighbours_pop_size = neighbours_pop_size,
                          matched_phase0_neighbours_pop_size = matched_phase0_neighbours_pop_size,
                          matched_phase1_neighbours_pop_size = matched_phase1_neighbours_pop_size,
                          matched_phase2_neighbours_pop_size = matched_phase2_neighbours_pop_size)
  stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
  df <- left_join(df, df_neighbours, by = "authority")
  
  df$neighbours_cases_phase_app_total_normed_to_them <-
    df$neighbours_cases_phase_app_total / df$neighbours_pop_size
  df$neighbours_cases_dec_total_normed_to_them <-
    df$neighbours_cases_dec_total / df$neighbours_pop_size
  df$matched_phase0_neighbours_cases_phase0_total_normed_to_them <-
    df$matched_phase0_neighbours_cases_phase0_total / df$matched_phase0_neighbours_pop_size
  df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them <-
    df$matched_phase0_neighbours_cases_phase_app_total / df$matched_phase0_neighbours_pop_size
  df$matched_phase0_neighbours_cases_phase1_total_normed_to_them <-
    df$matched_phase0_neighbours_cases_phase1_total / df$matched_phase0_neighbours_pop_size
  df$matched_phase1_neighbours_cases_phase2_total_normed_to_them <-
    df$matched_phase1_neighbours_cases_phase2_total / df$matched_phase1_neighbours_pop_size
  df$matched_phase2_neighbours_cases_phase3_total_normed_to_them <-
    df$matched_phase2_neighbours_cases_phase3_total / df$matched_phase2_neighbours_pop_size
  df$matched_phase0_neighbours_cases_phase2_total_normed_to_them <-
    df$matched_phase0_neighbours_cases_phase2_total / df$matched_phase0_neighbours_pop_size
  df$matched_phase0_neighbours_cases_phase3_total_normed_to_them <-
    df$matched_phase0_neighbours_cases_phase3_total / df$matched_phase0_neighbours_pop_size
  
  
  
  # Get the mean uptake of all your neighbours:
  uptakes <- df$fraction_users
  names(uptakes) <- df$authority
  uptake_of_neighbours <- unlist(lapply(neighbours, function(l) {
    sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  uptake_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
    sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  uptake_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
    sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  uptake_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
    sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  
  
  gdp_2018_band_adj <- df$gdp_2018_band_adj
  names(gdp_2018_band_adj) <- df$authority
  gdp_2018_band_adj_of_neighbours <- unlist(lapply(neighbours, function(l) {
    sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  gdp_2018_band_adj_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
    sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  gdp_2018_band_adj_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
    sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  gdp_2018_band_adj_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
    sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  
  RUC11CD <- df$RUC11CD
  names(RUC11CD) <- df$authority
  RUC11CD_of_neighbours <- unlist(lapply(neighbours, function(l) {
    sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  RUC11CD_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
    sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  RUC11CD_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
    sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  RUC11CD_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
    sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  
  povertybhc <- df$povertybhc
  names(povertybhc) <- df$authority
  povertybhc_of_neighbours <- unlist(lapply(neighbours, function(l) {
    sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  povertybhc_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
    sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  povertybhc_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
    sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  povertybhc_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
    sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
  }))
  
  df_neighbours <- tibble(authority = names(uptake_of_neighbours),
                          neighbours_fraction_users = uptake_of_neighbours,
                          matched_phase0_neighbours_fraction_users = uptake_of_matched_phase0_neighbours,
                          matched_phase1_neighbours_fraction_users = uptake_of_matched_phase1_neighbours,
                          matched_phase2_neighbours_fraction_users = uptake_of_matched_phase2_neighbours,
                          gdp_2018_band_adj_of_neighbours = gdp_2018_band_adj_of_neighbours,
                          gdp_2018_band_adj_of_matched_phase0_neighbours = gdp_2018_band_adj_of_matched_phase0_neighbours,
                          gdp_2018_band_adj_of_matched_phase1_neighbours = gdp_2018_band_adj_of_matched_phase1_neighbours,
                          gdp_2018_band_adj_of_matched_phase2_neighbours = gdp_2018_band_adj_of_matched_phase2_neighbours,
                          RUC11CD_of_neighbours = RUC11CD_of_neighbours,
                          RUC11CD_of_matched_phase0_neighbours = RUC11CD_of_matched_phase0_neighbours,
                          RUC11CD_of_matched_phase1_neighbours = RUC11CD_of_matched_phase1_neighbours,
                          RUC11CD_of_matched_phase2_neighbours = RUC11CD_of_matched_phase2_neighbours,
                          povertybhc_of_neighbours = povertybhc_of_neighbours,
                          povertybhc_of_matched_phase0_neighbours = povertybhc_of_matched_phase0_neighbours,
                          povertybhc_of_matched_phase1_neighbours = povertybhc_of_matched_phase1_neighbours,
                          povertybhc_of_matched_phase2_neighbours = povertybhc_of_matched_phase2_neighbours)
  
  stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
  df <- left_join(df, df_neighbours, by = "authority")
  
  
  # Ready for the regression, define difference in per capita cases and in uptake,
  # relative to your neighbours.
  df$fraction_users_rel_to_neighbours <- df$fraction_users - 
    df$neighbours_fraction_users
  df$gdp_2018_band_rel_to_neighbours <- df$gdp_2018_band_adj -
    df$gdp_2018_band_adj_of_neighbours
  df$RUC11CD_rel_to_neighbours <- df$RUC11CD -
    df$RUC11CD_of_neighbours
  df$povertybhc_rel_to_neighbours <- df$povertybhc -
    df$povertybhc_of_neighbours
  
  df$fraction_users_rel_to_matched_phase0_neighbours <- df$fraction_users - 
    df$matched_phase0_neighbours_fraction_users
  df$gdp_2018_band_rel_to_matched_phase0_neighbours <- df$gdp_2018_band_adj -
    df$gdp_2018_band_adj_of_matched_phase0_neighbours
  df$RUC11CD_rel_to_matched_phase0_neighbours <- df$RUC11CD -
    df$RUC11CD_of_matched_phase0_neighbours
  df$povertybhc_rel_to_matched_phase0_neighbours <- df$povertybhc -
    df$povertybhc_of_matched_phase0_neighbours
  
  df$fraction_users_rel_to_matched_phase1_neighbours <- df$fraction_users - 
    df$matched_phase1_neighbours_fraction_users
  df$gdp_2018_band_rel_to_matched_phase1_neighbours <- df$gdp_2018_band_adj -
    df$gdp_2018_band_adj_of_matched_phase1_neighbours
  df$RUC11CD_rel_to_matched_phase1_neighbours <- df$RUC11CD -
    df$RUC11CD_of_matched_phase1_neighbours
  df$povertybhc_rel_to_matched_phase1_neighbours <- df$povertybhc -
    df$povertybhc_of_matched_phase1_neighbours
  
  df$fraction_users_rel_to_matched_phase2_neighbours <- df$fraction_users - 
    df$matched_phase2_neighbours_fraction_users
  df$gdp_2018_band_rel_to_matched_phase2_neighbours <- df$gdp_2018_band_adj -
    df$gdp_2018_band_adj_of_matched_phase2_neighbours
  df$RUC11CD_rel_to_matched_phase2_neighbours <- df$RUC11CD -
    df$RUC11CD_of_matched_phase2_neighbours
  df$povertybhc_rel_to_matched_phase2_neighbours <- df$povertybhc -
    df$povertybhc_of_matched_phase2_neighbours
  
  
  df$cases_phase_app_total_per_capita_rel_to_neighbours <- df$cases_phase_app_total_per_capita -
    df$neighbours_cases_phase_app_total_normed_to_them
  df$cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase_app_total_per_capita -
    df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them
  df$cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase0_total_per_capita -
    df$matched_phase0_neighbours_cases_phase0_total_normed_to_them
  df$cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase1_total_per_capita -
    df$matched_phase0_neighbours_cases_phase1_total_normed_to_them
  df$cases_phase2_total_per_capita_rel_to_matched_phase1_neighbours <- df$cases_phase2_total_per_capita -
    df$matched_phase1_neighbours_cases_phase2_total_normed_to_them
  df$cases_phase3_total_per_capita_rel_to_matched_phase2_neighbours <- df$cases_phase3_total_per_capita -
    df$matched_phase2_neighbours_cases_phase3_total_normed_to_them
  df$cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase2_total_per_capita -
    df$matched_phase0_neighbours_cases_phase2_total_normed_to_them
  df$cases_phase3_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase3_total_per_capita -
    df$matched_phase0_neighbours_cases_phase3_total_normed_to_them
  
  df$log_cases_phase_app_total_per_capita_rel_to_neighbours <- log(df$cases_phase_app_total_per_capita) -
    log(df$neighbours_cases_phase_app_total_normed_to_them)
  df$log_cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase_app_total_per_capita) -
    log(df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them)
  df$log_cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase0_total_per_capita) -
    log(df$matched_phase0_neighbours_cases_phase0_total_normed_to_them)
  df$log_cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase1_total_per_capita) -
    log(df$matched_phase0_neighbours_cases_phase1_total_normed_to_them)
  df$log_cases_phase2_total_per_capita_rel_to_matched_phase1_neighbours <- log(df$cases_phase2_total_per_capita) -
    log(df$matched_phase1_neighbours_cases_phase2_total_normed_to_them)
  df$log_cases_phase3_total_per_capita_rel_to_matched_phase2_neighbours <- log(df$cases_phase3_total_per_capita) -
    log(df$matched_phase2_neighbours_cases_phase3_total_normed_to_them)
  df$log_cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase2_total_per_capita) -
    log(df$matched_phase0_neighbours_cases_phase2_total_normed_to_them)
  df$log_cases_phase3_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase3_total_per_capita) -
    log(df$matched_phase0_neighbours_cases_phase3_total_normed_to_them)
  
  
  
  
  # For investigation, list the identities of the neighbours
  neighbour_list <- unlist(lapply(neighbours, function(l) paste(l, collapse = "_")))
  df_neighbours <- tibble(authority = names(neighbour_list),
                          neighbour_list = neighbour_list)
  stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
  df <- left_join(df, df_neighbours, by = "authority")
  
  
  df_bootstrap[[bootstrap]]<-df
}

# END BOOTSTRAP


#####


df<-df_original




# Create a list of matched neighbours whose cases in September were in the same quintile
df$quintile_sep_cases_percapita <- cut(df$cases_sep_total_per_capita, 
                                       breaks = quantile(df$cases_sep_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                       include.lowest=TRUE,
                                       labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
df$quintile_phase0_cases_percapita <- cut(df$cases_phase0_total_per_capita, 
                                          breaks = quantile(df$cases_phase0_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                          include.lowest=TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
neighbours_in_same_quintile_phase0 <- vector(mode = "list", length=length(neighbours))
names(neighbours_in_same_quintile_phase0) <- names(neighbours)
for(area in names(neighbours)){
  target.quintile <- df[df$authority==area,]$quintile_phase0_cases_percapita
  matched.areas <- vector()
  for(candidate in neighbours[[area]]){
    test.quintile <- df[df$authority==candidate,]$quintile_phase0_cases_percapita
    if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
  }
  neighbours_in_same_quintile_phase0[[area]] <- matched.areas
}
hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
hist(lengths(neighbours_in_same_quintile_phase0), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours", breaks = 0:7)

df_figure_4A <- df %>% 
  select(authority, quintile_phase0_cases_percapita)
#save(neighbours_in_same_quintile_phase0, df_figure_4A, file = "Figure4AData.Rda")


df$quintile_phase1_cases_percapita <- cut(df$cases_phase1_total_per_capita, 
                                          breaks = quantile(df$cases_phase1_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                          include.lowest=TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
neighbours_in_same_quintile_phase1 <- vector(mode = "list", length=length(neighbours))
names(neighbours_in_same_quintile_phase1) <- names(neighbours)
for(area in names(neighbours)){
  target.quintile <- df[df$authority==area,]$quintile_phase1_cases_percapita
  matched.areas <- vector()
  for(candidate in neighbours[[area]]){
    test.quintile <- df[df$authority==candidate,]$quintile_phase1_cases_percapita
    if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
  }
  neighbours_in_same_quintile_phase1[[area]] <- matched.areas
}
#hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
#hist(lengths(neighbours_in_same_quintile_phase1), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours")

df$quintile_phase2_cases_percapita <- cut(df$cases_phase2_total_per_capita, 
                                          breaks = quantile(df$cases_phase2_total_per_capita, probs=seq(0,1,by=0.2), na.rm=TRUE),
                                          include.lowest=TRUE,
                                          labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
neighbours_in_same_quintile_phase2 <- vector(mode = "list", length=length(neighbours))
names(neighbours_in_same_quintile_phase2) <- names(neighbours)
for(area in names(neighbours)){
  target.quintile <- df[df$authority==area,]$quintile_phase2_cases_percapita
  matched.areas <- vector()
  for(candidate in neighbours[[area]]){
    test.quintile <- df[df$authority==candidate,]$quintile_phase2_cases_percapita
    if(test.quintile == target.quintile) matched.areas <- c(matched.areas, candidate)
  }
  neighbours_in_same_quintile_phase2[[area]] <- matched.areas
}
#hist(lengths(neighbours), main = "Distribution of number of neighbours for each LTLA", xlab = "Number of neighbours")
#hist(lengths(neighbours_in_same_quintile_phase1), main = "Distribution of number of matched neighbours for each LTLA", xlab = "Number of matched neighbours")


df$n_neighbours_in_same_quintile_phase0 <- lengths(neighbours_in_same_quintile_phase0)
df$n_neighbours_in_same_quintile_phase1 <- lengths(neighbours_in_same_quintile_phase1)
df$n_neighbours_in_same_quintile_phase2 <- lengths(neighbours_in_same_quintile_phase2)

# Sum the cases from all of your matched neighbours
cases_phase0_total_vector <- df$cases_phase0_total
cases_phase_app_total_vector <- df$cases_phase_app_total
cases_phase1_total_vector <- df$cases_phase1_total
cases_phase2_total_vector <- df$cases_phase2_total
cases_phase3_total_vector <- df$cases_phase3_total
cases_dec_total_vector <- df$cases_dec_total
names(cases_phase0_total_vector) <- df$authority
names(cases_phase1_total_vector) <- df$authority
names(cases_phase2_total_vector) <- df$authority
names(cases_phase3_total_vector) <- df$authority
names(cases_dec_total_vector) <- df$authority
names(cases_phase_app_total_vector) <- df$authority
#names(R_phase2_vector) <- df$authority
cases_phase0_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase0_total_vector[l])))
cases_phase_app_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase_app_total_vector[l])))
cases_phase1_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase1_total_vector[l])))
cases_phase2_total_in_phase1_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) sum(cases_phase2_total_vector[l])))
cases_phase3_total_in_phase2_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) sum(cases_phase3_total_vector[l])))
cases_phase2_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase2_total_vector[l])))
cases_phase3_total_in_phase0_matched_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(cases_phase3_total_vector[l])))


stopif(anyNA(cases_phase0_total_in_phase0_matched_neighbours))
stopif(anyNA(cases_phase_app_total_in_phase0_matched_neighbours))

# Merge that into the main df. Normalise your neighbours' cases by your population.
df_neighbours <- tibble(authority = names(cases_phase_app_total_in_neighbours),
                        neighbours_cases_phase_app_total = cases_phase_app_total_in_neighbours,
                        neighbours_cases_dec_total = cases_dec_total_in_neighbours)
stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
df_neighbours <- df_neighbours %>% add_column(
  matched_phase0_neighbours_cases_phase0_total = 
    cases_phase0_total_in_phase0_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase0_neighbours_cases_phase_app_total = 
    cases_phase_app_total_in_phase0_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase0_neighbours_cases_phase1_total = 
    cases_phase1_total_in_phase0_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase1_neighbours_cases_phase2_total = 
    cases_phase2_total_in_phase1_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase2_neighbours_cases_phase3_total = 
    cases_phase3_total_in_phase2_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase0_neighbours_cases_phase2_total = 
    cases_phase2_total_in_phase0_matched_neighbours)
df_neighbours <- df_neighbours %>% add_column(
  matched_phase0_neighbours_cases_phase3_total = 
    cases_phase3_total_in_phase0_matched_neighbours)
df <- left_join(df, df_neighbours, by = "authority")
df <- df %>%
  mutate(neighbours_cases_phase_app_total_normed_to_me = neighbours_cases_phase_app_total / pop_authority)

# Sum the population of all your neighbours, and matched neighbours and merge into df
pop_sizes <- df$pop_authority
names(pop_sizes) <- df$authority
neighbours_pop_size <- unlist(lapply(neighbours, function(l) sum(pop_sizes[l])))
matched_phase0_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) sum(pop_sizes[l])))
matched_phase1_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) sum(pop_sizes[l])))
matched_phase2_neighbours_pop_size <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) sum(pop_sizes[l])))
df_neighbours <- tibble(authority = names(neighbours_pop_size),
                        neighbours_pop_size = neighbours_pop_size,
                        matched_phase0_neighbours_pop_size = matched_phase0_neighbours_pop_size,
                        matched_phase1_neighbours_pop_size = matched_phase1_neighbours_pop_size,
                        matched_phase2_neighbours_pop_size = matched_phase2_neighbours_pop_size)
stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
df <- left_join(df, df_neighbours, by = "authority")

df$neighbours_cases_phase_app_total_normed_to_them <-
  df$neighbours_cases_phase_app_total / df$neighbours_pop_size
df$neighbours_cases_dec_total_normed_to_them <-
  df$neighbours_cases_dec_total / df$neighbours_pop_size
df$matched_phase0_neighbours_cases_phase0_total_normed_to_them <-
  df$matched_phase0_neighbours_cases_phase0_total / df$matched_phase0_neighbours_pop_size
df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them <-
  df$matched_phase0_neighbours_cases_phase_app_total / df$matched_phase0_neighbours_pop_size
df$matched_phase0_neighbours_cases_phase1_total_normed_to_them <-
  df$matched_phase0_neighbours_cases_phase1_total / df$matched_phase0_neighbours_pop_size
df$matched_phase1_neighbours_cases_phase2_total_normed_to_them <-
  df$matched_phase1_neighbours_cases_phase2_total / df$matched_phase1_neighbours_pop_size
df$matched_phase2_neighbours_cases_phase3_total_normed_to_them <-
  df$matched_phase2_neighbours_cases_phase3_total / df$matched_phase2_neighbours_pop_size
df$matched_phase0_neighbours_cases_phase2_total_normed_to_them <-
  df$matched_phase0_neighbours_cases_phase2_total / df$matched_phase0_neighbours_pop_size
df$matched_phase0_neighbours_cases_phase3_total_normed_to_them <-
  df$matched_phase0_neighbours_cases_phase3_total / df$matched_phase0_neighbours_pop_size



# Get the mean uptake of all your neighbours:
uptakes <- df$fraction_users
names(uptakes) <- df$authority
uptake_of_neighbours <- unlist(lapply(neighbours, function(l) {
  sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
uptake_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
  sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
uptake_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
  sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
uptake_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
  sum(uptakes[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))


gdp_2018_band_adj <- df$gdp_2018_band_adj
names(gdp_2018_band_adj) <- df$authority
gdp_2018_band_adj_of_neighbours <- unlist(lapply(neighbours, function(l) {
  sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
gdp_2018_band_adj_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
  sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
gdp_2018_band_adj_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
  sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
gdp_2018_band_adj_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
  sum(gdp_2018_band_adj[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))

RUC11CD <- df$RUC11CD
names(RUC11CD) <- df$authority
RUC11CD_of_neighbours <- unlist(lapply(neighbours, function(l) {
  sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
RUC11CD_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
  sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
RUC11CD_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
  sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
RUC11CD_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
  sum(RUC11CD[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))

povertybhc <- df$povertybhc
names(povertybhc) <- df$authority
povertybhc_of_neighbours <- unlist(lapply(neighbours, function(l) {
  sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
povertybhc_of_matched_phase0_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase0, function(l) {
  sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
povertybhc_of_matched_phase1_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase1, function(l) {
  sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))
povertybhc_of_matched_phase2_neighbours <- unlist(lapply(neighbours_in_same_quintile_phase2, function(l) {
  sum(povertybhc[l] * pop_sizes[l]) / sum(pop_sizes[l])
}))

df_neighbours <- tibble(authority = names(uptake_of_neighbours),
                        neighbours_fraction_users = uptake_of_neighbours,
                        matched_phase0_neighbours_fraction_users = uptake_of_matched_phase0_neighbours,
                        matched_phase1_neighbours_fraction_users = uptake_of_matched_phase1_neighbours,
                        matched_phase2_neighbours_fraction_users = uptake_of_matched_phase2_neighbours,
                        gdp_2018_band_adj_of_neighbours = gdp_2018_band_adj_of_neighbours,
                        gdp_2018_band_adj_of_matched_phase0_neighbours = gdp_2018_band_adj_of_matched_phase0_neighbours,
                        gdp_2018_band_adj_of_matched_phase1_neighbours = gdp_2018_band_adj_of_matched_phase1_neighbours,
                        gdp_2018_band_adj_of_matched_phase2_neighbours = gdp_2018_band_adj_of_matched_phase2_neighbours,
                        RUC11CD_of_neighbours = RUC11CD_of_neighbours,
                        RUC11CD_of_matched_phase0_neighbours = RUC11CD_of_matched_phase0_neighbours,
                        RUC11CD_of_matched_phase1_neighbours = RUC11CD_of_matched_phase1_neighbours,
                        RUC11CD_of_matched_phase2_neighbours = RUC11CD_of_matched_phase2_neighbours,
                        povertybhc_of_neighbours = povertybhc_of_neighbours,
                        povertybhc_of_matched_phase0_neighbours = povertybhc_of_matched_phase0_neighbours,
                        povertybhc_of_matched_phase1_neighbours = povertybhc_of_matched_phase1_neighbours,
                        povertybhc_of_matched_phase2_neighbours = povertybhc_of_matched_phase2_neighbours)

stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
df <- left_join(df, df_neighbours, by = "authority")


# Ready for the regression, define difference in per capita cases and in uptake,
# relative to your neighbours.
df$fraction_users_rel_to_neighbours <- df$fraction_users - 
  df$neighbours_fraction_users
df$gdp_2018_band_rel_to_neighbours <- df$gdp_2018_band_adj -
  df$gdp_2018_band_adj_of_neighbours
df$RUC11CD_rel_to_neighbours <- df$RUC11CD -
  df$RUC11CD_of_neighbours
df$povertybhc_rel_to_neighbours <- df$povertybhc -
  df$povertybhc_of_neighbours

df$fraction_users_rel_to_matched_phase0_neighbours <- df$fraction_users - 
  df$matched_phase0_neighbours_fraction_users
df$gdp_2018_band_rel_to_matched_phase0_neighbours <- df$gdp_2018_band_adj -
  df$gdp_2018_band_adj_of_matched_phase0_neighbours
df$RUC11CD_rel_to_matched_phase0_neighbours <- df$RUC11CD -
  df$RUC11CD_of_matched_phase0_neighbours
df$povertybhc_rel_to_matched_phase0_neighbours <- df$povertybhc -
  df$povertybhc_of_matched_phase0_neighbours

df$fraction_users_rel_to_matched_phase1_neighbours <- df$fraction_users - 
  df$matched_phase1_neighbours_fraction_users
df$gdp_2018_band_rel_to_matched_phase1_neighbours <- df$gdp_2018_band_adj -
  df$gdp_2018_band_adj_of_matched_phase1_neighbours
df$RUC11CD_rel_to_matched_phase1_neighbours <- df$RUC11CD -
  df$RUC11CD_of_matched_phase1_neighbours
df$povertybhc_rel_to_matched_phase1_neighbours <- df$povertybhc -
  df$povertybhc_of_matched_phase1_neighbours

df$fraction_users_rel_to_matched_phase2_neighbours <- df$fraction_users - 
  df$matched_phase2_neighbours_fraction_users
df$gdp_2018_band_rel_to_matched_phase2_neighbours <- df$gdp_2018_band_adj -
  df$gdp_2018_band_adj_of_matched_phase2_neighbours
df$RUC11CD_rel_to_matched_phase2_neighbours <- df$RUC11CD -
  df$RUC11CD_of_matched_phase2_neighbours
df$povertybhc_rel_to_matched_phase2_neighbours <- df$povertybhc -
  df$povertybhc_of_matched_phase2_neighbours


df$cases_phase_app_total_per_capita_rel_to_neighbours <- df$cases_phase_app_total_per_capita -
  df$neighbours_cases_phase_app_total_normed_to_them
df$cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase_app_total_per_capita -
  df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them
df$cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase0_total_per_capita -
  df$matched_phase0_neighbours_cases_phase0_total_normed_to_them
df$cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase1_total_per_capita -
  df$matched_phase0_neighbours_cases_phase1_total_normed_to_them
df$cases_phase2_total_per_capita_rel_to_matched_phase1_neighbours <- df$cases_phase2_total_per_capita -
  df$matched_phase1_neighbours_cases_phase2_total_normed_to_them
df$cases_phase3_total_per_capita_rel_to_matched_phase2_neighbours <- df$cases_phase3_total_per_capita -
  df$matched_phase2_neighbours_cases_phase3_total_normed_to_them
df$cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase2_total_per_capita -
  df$matched_phase0_neighbours_cases_phase2_total_normed_to_them
df$cases_phase3_total_per_capita_rel_to_matched_phase0_neighbours <- df$cases_phase3_total_per_capita -
  df$matched_phase0_neighbours_cases_phase3_total_normed_to_them

df$log_cases_phase_app_total_per_capita_rel_to_neighbours <- log(df$cases_phase_app_total_per_capita) -
  log(df$neighbours_cases_phase_app_total_normed_to_them)
df$log_cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase_app_total_per_capita) -
  log(df$matched_phase0_neighbours_cases_phase_app_total_normed_to_them)
df$log_cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase0_total_per_capita) -
  log(df$matched_phase0_neighbours_cases_phase0_total_normed_to_them)
df$log_cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase1_total_per_capita) -
  log(df$matched_phase0_neighbours_cases_phase1_total_normed_to_them)
df$log_cases_phase2_total_per_capita_rel_to_matched_phase1_neighbours <- log(df$cases_phase2_total_per_capita) -
  log(df$matched_phase1_neighbours_cases_phase2_total_normed_to_them)
df$log_cases_phase3_total_per_capita_rel_to_matched_phase2_neighbours <- log(df$cases_phase3_total_per_capita) -
  log(df$matched_phase2_neighbours_cases_phase3_total_normed_to_them)
df$log_cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase2_total_per_capita) -
  log(df$matched_phase0_neighbours_cases_phase2_total_normed_to_them)
df$log_cases_phase3_total_per_capita_rel_to_matched_phase0_neighbours <- log(df$cases_phase3_total_per_capita) -
  log(df$matched_phase0_neighbours_cases_phase3_total_normed_to_them)




# For investigation, list the identities of the neighbours
neighbour_list <- unlist(lapply(neighbours, function(l) paste(l, collapse = "_")))
df_neighbours <- tibble(authority = names(neighbour_list),
                        neighbour_list = neighbour_list)
stopifnot(identical(sort(df_neighbours$authority), sort(df$authority)))
df <- left_join(df, df_neighbours, by = "authority")




## Welsh authorities are missing confounders, and the two islands are missing
## variables normalised to their neighbours' pop sizes, since they have no neighbours
# View(df[! complete.cases(df), ])

# Fraction of cases with better uptake and fewer cases, or worse uptake and more
# cases, than their neighbours. 50% if no link between uptake and cases. 
quadrant_pos_neg <- sum(df$fraction_users_rel_to_neighbours > 0 &
                          df$cases_phase_app_total_per_capita_rel_to_neighbours < 0, na.rm = T)
quadrant_neg_pos <- sum(df$fraction_users_rel_to_neighbours < 0 &
                          df$cases_phase_app_total_per_capita_rel_to_neighbours > 0, na.rm = T)
quadrant_pos_pos <- sum(df$fraction_users_rel_to_neighbours > 0 &
                          df$cases_phase_app_total_per_capita_rel_to_neighbours > 0, na.rm = T)
quadrant_neg_neg <- sum(df$fraction_users_rel_to_neighbours < 0 &
                          df$cases_phase_app_total_per_capita_rel_to_neighbours < 0, na.rm = T)
(quadrant_pos_neg + quadrant_neg_pos) / (quadrant_pos_neg + quadrant_neg_pos +
                                           quadrant_neg_neg + quadrant_pos_pos)

matrix_quadrant<-matrix(c(quadrant_pos_pos,quadrant_neg_pos,quadrant_pos_neg,quadrant_neg_neg),nrow=2)
#fisher.test(matrix_quadrant)


# generate distance matrices 

rank_dist_phase0<-abs(outer(rank(df$cases_phase0_total_per_capita),rank(df$cases_phase0_total_per_capita),"-")/length(df$cases_phase0_total_per_capita))
colnames(rank_dist_phase0)<-df$authority
rownames(rank_dist_phase0)<-df$authority

dist_cl<-outer(df$clus10,df$clus10,"!=")+0
rownames(dist_cl)<-df$authority
colnames(dist_cl)<-df$authority

rank_dist_povertybhc<-abs(outer(rank(df$povertybhc),rank(df$povertybhc),"-")/length(df$povertybhc))
colnames(rank_dist_povertybhc)<-df$authority
rownames(rank_dist_povertybhc)<-df$authority

rank_dist_povertyahc<-abs(outer(rank(df$povertyahc),rank(df$povertyahc),"-")/length(df$povertyahc))
colnames(rank_dist_povertyahc)<-df$authority
rownames(rank_dist_povertyahc)<-df$authority

rank_dist_fuel<-abs(outer(rank(df$fuel),rank(df$fuel),"-")/length(df$fuel))
colnames(rank_dist_fuel)<-df$authority
rownames(rank_dist_fuel)<-df$authority

rank_dist_rural<-abs(outer(df$RUC11CD,df$RUC11CD,"-"))
colnames(rank_dist_rural)<-df$authority
rownames(rank_dist_rural)<-df$authority

rank_dist_gdp<-abs(outer(df$gdp_2018_band_adj,df$gdp_2018_band_adj,"-"))
colnames(rank_dist_gdp)<-df$authority
rownames(rank_dist_gdp)<-df$authority


#neighbours_matched<-lapply(names(neighbours),function(a){y<-neighbours[[a]]; y[which(rank_dist_phase0[a,y]<0.2 & rank_dist_rural[a,y]<0.2 & rank_dist_povertybhc[a,y]<0.2  & rank_dist_gdp[a,y]<0.2)] })
neighbours_matched<-lapply(names(neighbours),function(a){y<-neighbours[[a]]; y[which(rank_dist_phase0[a,y]<0.011)] })
names(neighbours_matched)<-names(neighbours)
comp_users<-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){df$fraction_users[df$authority==a]-df$fraction_users[df$authority==y]})}))
mean_users<-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){(df$fraction_users[df$authority==a]+df$fraction_users[df$authority==y])/2})}))
comp_log_cases_phase_app <-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){log(df$cases_phase_app_total_per_capita[df$authority==a])-log(df$cases_phase_app_total_per_capita[df$authority==y])})}))
comp_log_cases_phase0 <-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){log(df$cases_phase0_total_per_capita[df$authority==a])-log(df$cases_phase0_total_per_capita[df$authority==y])})}))
comp_log_cases_phase1 <-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){log(df$cases_phase1_total_per_capita[df$authority==a])-log(df$cases_phase1_total_per_capita[df$authority==y])})}))
comp_log_cases_phase2 <-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){log(df$cases_phase2_total_per_capita[df$authority==a])-log(df$cases_phase2_total_per_capita[df$authority==y])})}))
comp_log_cases_phase3 <-unlist(lapply(names(neighbours_matched),function(a){sapply(neighbours_matched[[a]],function(y){log(df$cases_phase3_total_per_capita[df$authority==a])-log(df$cases_phase3_total_per_capita[df$authority==y])})}))
#stopif(is.na(comp_users))
stopif(is.na(mean_users))
#stopif(is.na(comp_cases))

#################
# England only:

df_england <- df %>% filter(country == "England")

#rownames(df_england)<-df_england$authority
neighbours_england<-neighbours[df_england$authority]
neighbours_england<-lapply(neighbours_england,function(x){x[x %in% df_england$authority]})

#neighbours_matched_england<-lapply(names(neighbours_england),function(a){y<-neighbours_england[[a]]; y[rank_dist_sep[a,y]<0.1 & dist_cl[a,y]==0] })
#neighbours_matched_england<-lapply(names(neighbours_england),function(a){y<-neighbours_england[[a]]; y[rank_dist_phase0[a,y]<0.2 & rank_dist_rural[a,y]<0.2 & rank_dist_povertybhc[a,y]<0.2  & rank_dist_gdp[a,y]<0.2] })
neighbours_matched_england<-lapply(names(neighbours_england),function(a){y<-neighbours_england[[a]]; y[rank_dist_phase0[a,y]<0.011] })
names(neighbours_matched_england)<-names(neighbours_england)
comp_users_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){df$fraction_users[df$authority==a]-df$fraction_users[df$authority==y]})}))
mean_users_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){(df$fraction_users[df$authority==a]+df$fraction_users[df$authority==y])/2})}))
comp_log_cases_england_phase_app<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){log(df$cases_phase_app_total_per_capita[df$authority==a])-log(df$cases_phase_app_total_per_capita[df$authority==y])})}))
comp_log_cases_england_phase0<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){log(df$cases_phase0_total_per_capita[df$authority==a])-log(df$cases_phase0_total_per_capita[df$authority==y])})}))
comp_log_cases_england_phase1<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){log(df$cases_phase1_total_per_capita[df$authority==a])-log(df$cases_phase1_total_per_capita[df$authority==y])})}))
comp_log_cases_england_phase2<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){log(df$cases_phase2_total_per_capita[df$authority==a])-log(df$cases_phase2_total_per_capita[df$authority==y])})}))
comp_log_cases_england_phase3<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){log(df$cases_phase3_total_per_capita[df$authority==a])-log(df$cases_phase3_total_per_capita[df$authority==y])})}))
comp_manual_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){df$manual_cases[df$authority==a]-df$manual_cases[df$authority==y]})}))
comp_contacts_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){(df$manual_contacts*df$manual_cases)[df$authority==a]-(df$manual_contacts*df$manual_cases)[df$authority==y]})}))
comp_rural_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){df$RUC11CD[df$authority==a]-df$RUC11CD[df$authority==y]})}))
comp_gdp_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){df$gdp_2018_band_adj[df$authority==a]-df$gdp_2018_band_adj[df$authority==y]})}))
comp_poverty_england<-unlist(lapply(names(neighbours_matched_england),function(a){sapply(neighbours_matched_england[[a]],function(y){df$povertybhc[df$authority==a]-df$povertybhc[df$authority==y]})}))
stopif(is.na(comp_users_england))
stopif(is.na(mean_users_england))
#stopif(is.na(comp_cases_england))
stopif(is.na(comp_manual_england))
stopif(is.na(comp_contacts_england))

