library(tidyverse)
library(Hmisc)
library(lubridate)

# set working directory
setwd("/Users/sackd/filen/Vanderbilt University/Street Medicine/Quant Screen/Code/")

# pull in raw data
raw <- read_csv("../ed.2023.csv",
                na = c("NA", "", "NULL")) %>%
  arrange(as_datetime(ed_arrival)) %>%
  filter(as_date(arrived) > as.Date("2023-01-04"), as_date(arrived) < as.Date("2023-05-17"), age_yr >= 18) # get all visits b/t Jan 5 and May 16, 2023 and adults

# get high utilizes
# high <- raw %>% 
#   mutate(vis_id = ave(pat_mrn_id, FUN = seq_along), # ED visit number in the dataset
#          vis_num = ave(yday(as_datetime(ed_arrival)), pat_mrn_id, FUN = seq_along), # ED visit number for this patient
#          housing_screen = ifelse(pastQuestion == "No" | futureQuestion == "Yes", 1, 0), # housing screen
#          housing_screen = factor(housing_screen, levels = c(0, 1), labels = c("No", "Yes"))) %>%
#   group_by(pat_mrn_id) %>%
#   mutate(tot_vis = max(vis_num),
#          tot_vis_fac = factor(ifelse(tot_vis > 4, "5+", tot_vis), levels = c("1", "2", "3", "4", "5+"))) %>% # create a unique ID for this patient
#   ungroup() %>%
#   select(pat_mrn_id, tot_vis, tot_vis_fac, housing_screen) %>%
#   filter(housing_screen == "Yes", tot_vis > 3) %>%
#   distinct(., pat_mrn_id, .keep_all = TRUE)
# 
# # export
# write_csv(high, "high_utilizers.csv")

# first do some transformations
raw <- raw %>%
  mutate(vis_id = ave(pat_mrn_id, FUN = seq_along), # ED visit number in the dataset
         vis_num = ave(yday(as_datetime(ed_arrival)), pat_mrn_id, FUN = seq_along), # ED visit number for this patient
         vis_date = as_date(arrived), # ED visit date
         vis_month = month(as_date(arrived), label = TRUE), # month of ED visit
         vis_weekday = wday(as_date(arrived), label = TRUE), # ED visit day of the week
         vis_hour = hour(arrived), # ED visit hour
         time_to_room = as.numeric(as_datetime(roomed) - as_datetime(arrived)), # time to rooming in seconds
         time_to_md = ifelse(is.na(provider_assigned), NA, as.numeric(as_datetime(provider_assigned) - as_datetime(arrived))), # time to provider assigned in seconds
         time_to_hosp_dc = ifelse(is.na(HOSPITAL_DISCHARGE_DTTM), NA, as.numeric(as_datetime(HOSPITAL_DISCHARGE_DTTM) - as_datetime(arrived))), # time to hospital discharge in seconds
         time_to_ed_dc = ifelse(is.na(ED_DISPOSITION_DTTM), NA, as.numeric(as_datetime(ED_DISPOSITION_DTTM) - as_datetime(arrived))), # time to ed disposition in seconds
         house_insecure = ifelse(futureQuestion == "Yes", 1, 0), # housing insecure
         house_insecure = ifelse(is.na(house_insecure), 99, house_insecure),
         house_insecure = factor(house_insecure, levels = c(0, 1, 99), labels = c("No", "Yes", "Missing")),
         house_less = ifelse(pastQuestion == "No", 1, 0), # houseless
         house_less = ifelse(is.na(house_less), 99, house_less),
         house_less = factor(house_less, levels = c(0, 1, 99), labels = c("No", "Yes", "Missing")),
         housing_cat = ifelse(house_insecure == "No" & house_less == "No", "Stable Housing", "Missing"),
         housing_cat = ifelse(house_insecure == "Yes" & house_less == "No", "Unstable Housing", housing_cat),
         housing_cat = ifelse(house_less == "Yes", "Homeless", housing_cat),
         housing_cat = ifelse(is.na(house_insecure) | is.na(house_less), "Missing", housing_cat),
         housing_cat = factor(housing_cat, levels = c("Stable Housing", "Unstable Housing", "Homeless", "Missing")),
         housing_screen = ifelse(housing_cat == "Homeless", 1, 0), # housing screen
         housing_screen = ifelse(housing_cat == "Missing", 99, housing_screen),
         housing_screen = factor(housing_screen, levels = c(0, 1, 99), labels = c("No", "Yes", "Missing"))) %>%
  group_by(pat_mrn_id) %>%
  mutate(id = cur_group_id()) %>% # create a unique ID for this patient
  ungroup()

# create less identified dataset (WILL HAVE TO ADD IN SW SCREEN LATER)
data <- raw %>%
    select(id, vis_id:time_to_ed_dc, acuity_level_c, priorEdVisits:ethnicity, meansOfArrival:edDispo, chief_complaint:futureQuestion, house_insecure:housing_screen, screenedQuestion:psychHist)

# need to decrease the number of categories (chief_complaint, ed_dx - just the primary, hospDispo, edDispo)
# create exportable tables
# data %>% pull(chief_complaint) %>% unique() %>% write.csv("Categories/cc.csv")
# data %>% pull(primary_ed_dx) %>% unique() %>% as_tibble() %>% write.csv("Categories/ed_dx.csv")
# data %>% pull(hospDispo) %>% unique() %>% as_tibble() %>% write.csv("Categories/hospdc.csv")
# data %>% pull(edDispo) %>% unique() %>% as_tibble() %>% write.csv("Categories/eddc.csv")
# data %>% pull(language) %>% unique() %>% as_tibble() %>% write.csv("Categories/lang.csv")
# data %>% pull(fin_name) %>% unique() %>% as_tibble() %>% write.csv("Categories/insurance.csv")

# pull in updated categories
cc_cat <- read_csv("Categories/cc_cat.csv")
ed_dx_cat <- read_csv("Categories/ed_dx_cat.csv")
hospdc_cat <- read_csv("Categories/hospdc_cat.csv")
eddc_cat <- read_csv("Categories/eddc_cat.csv")
lang_cat <- read_csv("Categories/lang_cat.csv")
insurance_cat <- read_csv("Categories/insurance_cat.csv")

# merge with data
data <- data %>% 
  left_join(cc_cat, by = "chief_complaint") %>%
  left_join(ed_dx_cat, by = "primary_ed_dx") %>%
  left_join(hospdc_cat, by = "hospDispo") %>%
  left_join(eddc_cat, by = "edDispo") %>%
  left_join(lang_cat, by = "language") %>%
  left_join(insurance_cat, by = "fin_name")

# update some other categories, TBD ###
data <- data %>%
  mutate(race = ifelse(race == "multi", "Multiple", race),
         race = ifelse(race %in% c("American Indian or Alaska Native",
                                  "Native Hawaiian or Other Pacific Islander"), "AIAN/NHPI", race),
         race = ifelse(race == "Middle Eastern or North Africian", "MENA", race),
         race = ifelse(race %in% c("Other","Prefer Not to Answer", "Unable to Provide",
                                   "None of these"), "Other", race),
         meansOfArrival = ifelse(meansOfArrival %in% c("Assist from Vehicle", "Wheelchair", "Other"), 
                                 "Other", meansOfArrival),
         meansOfArrival = ifelse(meansOfArrival %in% c("Ambulance", "Hospital Transport"), 
                                 "Ambulance/Hospital Transport", meansOfArrival),
         age_cat = ifelse(round(age_yr) <=24, "18-24", NA),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) <= 34, "25-34", age_cat),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) <= 44, "35-44", age_cat),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) <= 54, "45-54", age_cat),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) <= 64, "55-64", age_cat),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) <= 74, "65-74", age_cat),
         age_cat = ifelse(is.na(age_cat) & round(age_yr) > 74, "75+", age_cat),
         age_cat = factor(age_cat, levels = c("18-24", "25-34", "35-44", "45-54", 
                                              "55-64", "65-74", "75+")),
         eth_cat = ifelse(ethnicity %in% c("Cuban", 
                                           "Hispanic or Latino", 
                                           "Other Hispanic, Latino/a, or Spanish origin",
                                           "Mexican, Mexican American, or Chicano/a",
                                           "Puerto Rican"), 
                          "Hispanic/Latino/a", NA),
         eth_cat = ifelse(is.na(eth_cat) & ethnicity %in% c("Decline to Answer",
                                                            "Prefer Not to Answer",
                                                            "Unable to Provide"),
                          "Other", eth_cat),
         eth_cat = ifelse(is.na(eth_cat) & ethnicity %in% c("None of these",
                                                            "Not Hispanic, Latino/a, or Spanish origin"),
                          "Not Hispanic/Latino/a", eth_cat),
         eth_cat = factor(eth_cat, levels = c("Hispanic/Latino/a", "Not Hispanic/Latino/a", "Other")),
         screenedQuestion = ifelse(screenedQuestion == "Screened:  Will attempt to see" | is.na(screenedQuestion), "Not seen", screenedQuestion))

# update names and units
data <- upData(data,
               labels = c(id = "ID",
                          vis_id = "Visit Number in Dataset",
                          vis_num = "Visit Number in Study Period",
                          vis_date = "Visit Date",
                          vis_month = "Visit Month",
                          vis_weekday = "Visit Weekday",
                          vis_hour = "Visit Time (hour)",
                          time_to_room = "Time to Room",
                          time_to_md = "Time to Provider Assigned",
                          acuity_level_c = "Triage Score",
                          priorEdVisits = "Previous XX days of VUMC ED Visits",
                          priorAdmits = "Previous XX days of VUMC Admissions",
                          avg_prior_hosp_los = "Average previous VUMC length of stay",
                          age_yr = "Age (years)",
                          sex = "Sex",
                          race = "Race",
                          ethnicity = "Ethnicity",
                          meansOfArrival = "Arrival Method",
                          hospDispo = "Hospital Discharge",
                          edDispo = "ED Disposition",
                          chief_complaint = "Presenting Symptom",
                          language = "Language",
                          fin_name = "Insurance Status",
                          pastQuestion = "Currently Unhoused",
                          futureQuestion = "Unstably Housed",
                          house_insecure = "Unstably Housed",
                          house_less = "Currently Unhoused",
                          housing_screen = "Screened Positive",
                          housing_cat = "Housing Category",
                          screenedQuestion = "Social Work Screening",
                          primary_ed_dx = "Primary ED Diagnosis",
                          psychHist = "History of Psychiatric Condition",
                          cc_cat = "Presenting Symptom",
                          ed_dx_cat = "Primary ED Diagnosis",
                          hospDispoCat = "Hospital Discharge",
                          edDispoCat = "ED Disposition",
                          lang_cat = "Language",
                          insurance_cat = "Insurance",
                          age_cat = "Age",
                          eth_cat = "Ethnicity"))


describe(data)

# export for further analysis
Save(data)

# make table of categorized chief complaints and ed diagnoses

# cc
cc_cat_sup <- data %>% select(cc_cat, chief_complaint) %>% 
  pivot_wider(names_from = cc_cat, values_from = chief_complaint) %>% 
  t() %>% as.data.frame()
write_csv(tibble(cat = rownames(cc_cat_sup), list = cc_cat_sup$V1) %>% mutate(types = map_chr(list, ~ paste(.x, collapse = ", "))) %>% select(cat, types), "./Categories/cc_cat_sup.csv")

# ed_dx
ed_dx_cat_sup <- data %>% select(ed_dx_cat, primary_ed_dx) %>% 
  pivot_wider(names_from = ed_dx_cat, values_from = primary_ed_dx) %>% 
  t() %>% as.data.frame()
write_csv(tibble(cat = rownames(ed_dx_cat_sup), list = ed_dx_cat_sup$V1) %>% mutate(types = map_chr(list, ~ paste(.x, collapse = ", "))) %>% select(cat, types), "./Categories/ed_dx_cat_sup.csv")
