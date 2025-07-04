# PAPER 2 #

# Overview

# 1_Read_clean_prepare.R - Reading, cleaning and preparing of the data
# 1_EDA.R - Exploratory data analysis
# 1_Main_analysis.R - main results

# https://github.com/jdegenfellner/Paper2-dissertation/blob/main/1_Main_Analysis_git.R

# Load packages ----------------------------------------------------------------
library(pacman)
pacman::p_load(
  data.table, plyr, dplyr, tidyverse, ggmosaic, DataExplorer, crosstable,
  mgcv, fancycut, pscl, sure, blorr, glmtoolbox, caret, naniar, visdat,
  Hmisc, mice, miceFast, micemd, gtsummary, flextable, quarto, tictoc,
  MASS, officer, corrgram, rFSA, miceadds, YesSiR, janitor, PRISMAstatement,
  grid, gridExtra, htmlwidgets, webshot, magick, VIM, ResourceSelection, 
  DescTools, performance, writexl, openxlsx, officer, gt
)

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Variant 1 or 2 definition for big 5 personality scales? ----------------------
v <- 1
# Variant 1: "Correct version" adds NA if answer 'Prefer not to answer'/
# 'Do not know' was given; we deem this to be the correct way to deal with these scores

# Variant 2: adds a 0, as was done in: https://doi.org/10.1038/s41598-022-10573-6


# 1) READ cleaned/prepared data ----
if(!exists("ukb")){
  file_chosen <- file.choose() # ukb_cleaned_v1b5...RDS
  ukb <- readRDS(file_chosen) # Note that choose.files() only works in Windows
}

# __Exclusion Flowchart(s)------------------------------------------------------
# maybe improve using: library(DiagrammeR)

tot_nsw <- sum(ukb$night_shift_work == "Usually" | ukb$night_shift_work == "Always", na.rm = TRUE)
tot_nsw # 11084 now

age_65 <- nrow(ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
                 filter(age <= 65))
age_65 # now 10943
tot_nsw - age_65 # now 141

years_in_job <- nrow(ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
                       filter(age <= 65) %>% 
                       filter(time_employed_in_current_job >= 0))
years_in_job # now 10909
years_in_job - age_65 # now -34

alcohol <- nrow(ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
                  filter(age <= 65) %>% 
                  filter(time_employed_in_current_job >= 0) %>%
                  filter(alcohol_intake_frequency != "Prefer not to answer" | is.na(alcohol_intake_frequency)) ) # na=0, but leave NAs in, impute later
alcohol - years_in_job # now -19

chronotype <- nrow(ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
                     filter(age <= 65) %>% 
                     filter(time_employed_in_current_job >= 0) %>%
                     filter(alcohol_intake_frequency != "Prefer not to answer" | is.na(alcohol_intake_frequency)) %>%
                     filter(chronotype != "Prefer not to answer" | is.na(chronotype))) # leave in NAs (imputed later)
chronotype - alcohol # now -51

InsomniaDisorder_not_pref <- nrow(ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
                                  filter(age <= 65) %>% 
                                  filter(time_employed_in_current_job >= 0) %>%
                                  filter(alcohol_intake_frequency != "Prefer not to answer" | is.na(alcohol_intake_frequency)) %>%
                                  filter(chronotype != "Prefer not to answer" | is.na(chronotype)) %>%
                                  filter(InsomniaDisorder != "Prefer not to answer")) # no missings
InsomniaDisorder_not_pref - chronotype # now -52

# or
SWD_5_not_NA <- nrow(ukb %>% filter(ukb$night_shift_work == "Usually" | ukb$night_shift_work == "Always") %>% 
                       filter(age <= 65) %>% 
                       filter(time_employed_in_current_job >= 0) %>%
                       filter(alcohol_intake_frequency != "Prefer not to answer" | is.na(alcohol_intake_frequency)) %>%
                       filter(chronotype != "Prefer not to answer" | is.na(chronotype)) %>%
                       filter(!is.na(SWD_5))) # a lot missing
SWD_5_not_NA - chronotype # now -5180


# __a) Exclusions for InsomniaDisorder----
flowchart1 <- flow_exclusions(
  incl_counts = c(tot_nsw, age_65, years_in_job, alcohol, chronotype, InsomniaDisorder_not_pref),
  total_label = "Night shift workers (usually or always)",
  incl_labels = c("Age \u2264 65", 
                  "Number of years in current job provided", 
                  "Alcohol frequency provided",
                  "Chronotype provided",
                  "Insomnia disorder status available"),
  excl_labels = c("Age > 65", 
                  "Number of years in current job not provided \n (= Do not know or Prefer not to answer)", 
                  "Alcohol frequency not provided \n (=Prefer not to answer)",
                  "Chronotype not provided \n (=Prefer not to answer)",
                  "Insomnia disorder status not available \n (=Prefer not to answer)")
)
flowchart1 # 10,787
saveWidget(flowchart1, "flowchart1_17.11.24.html")

# __b) Exclusions for SWD_5 (freq insomnia def)----
flowchart2 <- flow_exclusions(
  incl_counts = c(tot_nsw, age_65, years_in_job, alcohol, chronotype, SWD_5_not_NA),
  total_label = "Night shift workers (usually or always)",
  incl_labels = c("Age \u2264 65", 
                  "Number of years in current job provided", 
                  "Alcohol frequency provided",
                  "Chronotype provided",
                  "Frequent insomnia status available"),
  excl_labels = c("Age > 65", 
                  "Number of years in current job not provided \n (= Do not know or Prefer not to answer)", 
                  "Alcohol frequency not provided \n (=Prefer not to answer)",
                  "Chronotype not provided \n (=Prefer not to answer)",
                  "Frequent insomnia status not available \n (missing)")
)
flowchart2 # 5,659
saveWidget(flowchart1, "flowchart2_17.11.24.html")



# 2) MODELS --------------------------------------------------------------------

# __Define Subset for Analysis--------------------------------------------------

# ___a) Select and filter-------------------------------------------------------
ukb_reg <- ukb %>% dplyr::select(
  
  # Definition(s) Shift Work Disorder (SWD)
  SWD_2, # Usually (or more) night shift work and not 'any insomnia symptoms' 
  SWD_5, # Usually (or more) night shift work and no 'frequent insomnia symptoms'
  InsomniaDisorder, # Validated phenotype (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5600256/)
  night_shift_work, # frequency of night shift work
  
  # Traits
  age, 
  sex,
  ethnicity_new,
  chronotype,
  #neuroticism_score, # Debut: Jan 2015, would explain missingness?
  nervousness_scale, # (caution: proxy for neuroticism, cor=0.87)
  sociability_scale, 
  warmth_scale, 
  diligence_scale, 
  curiosity_scale, 
  f.1031.0.0, # items for sociability scale (needed for passive imputation)
  f.2030.0.0,
  f.2080.0.0,
  f.6160.0.0, 
  f.2110.0.0, # items for warmth scale
  f.1940.0.0,
  f.1920.0.0,
  f.1990.0.0,
  f.1970.0.0,
  f.2060.0.0, # items for diligence scale
  f.1960.0.0,
  f.2040.0.0,
  f.2000.0.0,
  f.2020.0.0, # items for curiosity scale
  f.2010.0.0,
  f.2070.0.0,
  f.2040.0.0,
  f.1990.0.0, # items for nervousness scale
  f.1940.0.0,
  f.2060.0.0,
  f.1920.0.0,
  f.1950.0.0,
  
  # Modifiable (risk) factors
  time_employed_in_current_job,
  BMI,
  smoking_status, 
  #MET_minutes_week, # only leisure time physical activity mentioned, not really available in UKB
  #difficulty_not_smoking_for_a_day, 
  coffee_intake, 
  alcohol_intake_frequency
  # Other factors
  #f.189.0.0
  ) %>% 
  # ___b) Exclusions #-----------------
  filter(age <= 65, 
         night_shift_work == "Always" |  night_shift_work == "Usually") %>%

  filter(time_employed_in_current_job >= 0) %>%

  filter(alcohol_intake_frequency != "Prefer not to answer") %>% # these levels distort the estimates
  mutate(alcohol_intake_frequency = droplevels(alcohol_intake_frequency, "Prefer not to answer")) %>% # just to be save that the logistic regression is not singular

  filter(chronotype != "Prefer not to answer" | is.na(chronotype)) %>% # leave NA in for imputation

  filter(InsomniaDisorder != "Prefer not to answer") %>% 
  mutate(InsomniaDisorder = droplevels(InsomniaDisorder, "Prefer not to answer"))
    #filter(sociability_scale != 0) # possibly the reason for degenerate logistic regression in complete case analysis?
  #mutate(smoking_status = droplevels(smoking_status, "Prefer not to anwer"))

dim(ukb_reg) # now 10787

# [Review round 1] age and time_employed_in_current_job integrity check----
# The authors did not tell us why they divided the sample into 4 quartiles of 
# work seniority. Looking at the table, it is surprising to observe that the 
# upper quartile, with working seniority from 22 to 48 years (mean 35 years), 
# has an average age of 52.8+5.9, no different from that of the other groups. 
# It would appear that some worked shifts at four years of age or younger; on
# average, all members of this group would have started working shifts at the
# average age of 17. It seems important to verify the data

# 21003: Age when attended assessment centre
# This is a derived variable based on date of birth and date of
# attending assessment centre and refers to the age of the participant on 
# the day they attended an Assessment Centre, truncated to whole year part.
# https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=21003

# 757: ACE touchscreen question "How many years have you worked in your current
# job? (If you have more than one job please answer this, and the following
# questions on work, for your MAIN job only)"
# https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=757
# https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=100257

# 3426: ACE touchscreen question "Does your work involve night shifts?"
# https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=3426

# _Check numbers for review...-------
table(ukb_reg$time_employed_in_current_job)
hist(ukb_reg$age)
hist(ukb_reg$time_employed_in_current_job)
hist(ukb_reg$age - ukb_reg$time_employed_in_current_job)

# export:
table_export <- ukb_reg_ID %>% mutate(difference_age_time_empl = age - time_employed_in_current_job) %>% # age at baseline - time employed in current job
  dplyr::select(f.eid,difference_age_time_empl, age, time_employed_in_current_job) %>% 
  arrange(difference_age_time_empl) %>% head(10) %>% as_tibble()
table_export
writexl::write_xlsx(table_export, path = "./Tables/Suppl_age_time_empl.xlsx")

# _Check quartiles manually-------
quantile(ukb_reg_ID$time_employed_in_current_job, probs = c(0,0.25,0.5,0.75,1))  # Calculate the 1st and 99th percentiles
# 0%  25%  50%  75% 100% 
# 0    5   11   22   48 

# _Check mean age in quartiles manually-------
ukb_reg_ID %>% mutate(quartile_time_employed = ntile(time_employed_in_current_job, 4)) %>%  # Divide into quartiles
  group_by(quartile_time_employed) %>%                                         # Group by quartiles
  summarise(mean_age = mean(age, na.rm = TRUE))                                # Calculate the mean age for each quartile
# quartile_time_employed mean_age
#<int>    <dbl>
#  1                      1     50.1
#2                      2     51.2
#3                      3     50.5
#4                      4     52.6

ukb_reg_ID <- ukb %>% dplyr::select(
  f.eid,
  f.189.0.0, # Townsend deprivation index
  # Definition(s) Shift Work Disorder (SWD)
  SWD_2, # Usually (or more) night shift work and not 'any insomnia symptoms' 
  SWD_5, # Usually (or more) night shift work and no 'frequent insomnia symptoms'
  InsomniaDisorder, # Validated phenotype (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5600256/)
  night_shift_work, # frequency of night shift work
  
  # Traits
  age, 
  sex,
  ethnicity_new,
  f.738.0.0, # Income
  chronotype,
  #neuroticism_score, # Debut: Jan 2015, would explain missingness?
  nervousness_scale, # (caution: proxy for neuroticism, cor=0.87)
  sociability_scale, 
  warmth_scale, 
  diligence_scale, 
  curiosity_scale, 
  f.1031.0.0, # items for sociability scale (needed for passive imputation)
  f.2030.0.0,
  f.2080.0.0,
  f.6160.0.0, 
  f.2110.0.0, # items for warmth scale
  f.1940.0.0,
  f.1920.0.0,
  f.1990.0.0,
  f.1970.0.0,
  f.2060.0.0, # items for diligence scale
  f.1960.0.0,
  f.2040.0.0,
  f.2000.0.0,
  f.2020.0.0, # items for curiosity scale
  f.2010.0.0,
  f.2070.0.0,
  f.2040.0.0,
  f.1990.0.0, # items for nervousness scale
  f.1940.0.0,
  f.2060.0.0,
  f.1920.0.0,
  f.1950.0.0,
  
  # Modifiable (risk) factors
  time_employed_in_current_job,
  BMI,
  smoking_status, 
  #MET_minutes_week, # only leisure time physical activity mentioned, not really available in UKB
  #difficulty_not_smoking_for_a_day, 
  coffee_intake, 
  alcohol_intake_frequency
  # Other factors
  #f.189.0.0
) %>% 
  # ___b) Exclusions #-----------------
filter(age <= 65, 
       night_shift_work == "Always" |  night_shift_work == "Usually") %>%
  
  filter(time_employed_in_current_job >= 0) %>%
  
  filter(alcohol_intake_frequency != "Prefer not to answer") %>% # these levels distort the estimates
  mutate(alcohol_intake_frequency = droplevels(alcohol_intake_frequency, "Prefer not to answer")) %>% # just to be save that the logistic regression is not singular
  
  filter(chronotype != "Prefer not to answer" | is.na(chronotype)) %>% # leave NA in for imputation
  
  filter(InsomniaDisorder != "Prefer not to answer") %>% 
  mutate(InsomniaDisorder = droplevels(InsomniaDisorder, "Prefer not to answer"))
#filter(sociability_scale != 0) # possibly the reason for degenerate logistic regression in complete case analysis?
#mutate(smoking_status = droplevels(smoking_status, "Prefer not to anwer"))


# [Review round 1] "1)	General analysis: Is there information on whether-------
# participants are working multiple jobs? If so, could you evaluate whether
# that may be a contributing factor for SWSD?"

# Select unique f.eid and num_jobs_during_assessment from ukb
ukb_unique <- ukb %>%
  dplyr::select(f.eid, num_jobs_during_assessment) %>%
  distinct(f.eid, .keep_all = TRUE)
ukb_rev1 <- left_join(ukb_reg_ID, ukb_unique, by = "f.eid")
dim(ukb_rev1) # 10,787

# Calculate the percentages of InsomniaDisorder by num_jobs_during_assessment
ukb_rev1 %>%
  filter(num_jobs_during_assessment >= 1) %>%
  filter(f.eid %in% ukb_reg_ID$f.eid) %>% # only f.eid in analytic sample!
  group_by(num_jobs_during_assessment, InsomniaDisorder) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = factor(num_jobs_during_assessment), y = percentage, fill = InsomniaDisorder)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(x = "Number of Jobs During Assessment", y = "Percentage", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggtitle("Percentage of Insomnia Disorder by Number of Jobs During Assessment") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the total counts for each num_jobs_during_assessment category
total_counts <- ukb_rev1 %>%
  filter(num_jobs_during_assessment >= 1) %>%
  filter(f.eid %in% ukb_reg_ID$f.eid) %>% # only f.eid in analytic sample!
  group_by(num_jobs_during_assessment) %>%
  summarise(total_count = n())

# Calculate the percentages of InsomniaDisorder by num_jobs_during_assessment
plot_data <- ukb_rev1 %>%
  filter(num_jobs_during_assessment >= 1) %>%
  group_by(num_jobs_during_assessment, InsomniaDisorder) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100) %>%
  ungroup()

# Create a custom label for the x-axis
plot_data <- plot_data %>%
  left_join(total_counts, by = "num_jobs_during_assessment") %>%
  mutate(label = paste(num_jobs_during_assessment, "\n(N=", total_count, ")", sep = ""))

# Plot with custom x-axis labels
ggplot(plot_data, aes(x = label, y = percentage, fill = InsomniaDisorder)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(x = "Number of Jobs During Assessment", y = "Percentage", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ggtitle("Percentage of Insomnia Disorder by Number of Jobs During Assessment") +
  theme(plot.title = element_text(hjust = 0.5))

table(ukb_rev1$num_jobs_during_assessment)
sum(table(ukb_rev1$num_jobs_during_assessment)) # 9675
sum(is.na(ukb_rev1$num_jobs_during_assessment)) # 0
dim(ukb_reg) # 10787

# 3 jobs and night shift worker at time of assessment center----
three_jobs_f.eid <- ukb_rev1$f.eid[ukb_rev1$num_jobs_during_assessment == 3]
ukb %>% filter(f.eid %in% three_jobs_f.eid) %>%
  dplyr::select(f.eid, 
                night_shift_work, 
                date_first_attend_assessment_centre,
                num_jobs_during_assessment,
                f.22602.0.0, f.22603.0.0, 
                f.22602.0.1, f.22603.0.1,
                f.22602.0.2, f.22603.0.2,
                f.22602.0.3, f.22603.0.3,
                f.22602.0.4, f.22603.0.4,
                f.22602.0.5, f.22603.0.5,
                f.22602.0.6, f.22603.0.6,
                f.22602.0.7, f.22603.0.7,
                f.22602.0.8, f.22603.0.8,
                f.22602.0.9, f.22603.0.9,
                f.22602.0.10, f.22603.0.10,
                f.22602.0.11, f.22603.0.11,
                f.22602.0.12, f.22603.0.12,
                f.22602.0.13, f.22603.0.13,
                f.22602.0.14, f.22603.0.14)

# 2 jobs and night shift worker at time of assessment center----
two_jobs_f.eid <- ukb_rev1$f.eid[ukb_rev1$num_jobs_during_assessment == 2]
ukb %>% filter(f.eid %in% two_jobs_f.eid) %>%
  dplyr::select(f.eid, 
                night_shift_work, 
                date_first_attend_assessment_centre,
                num_jobs_during_assessment,
                f.22602.0.0, f.22603.0.0, 
                f.22602.0.1, f.22603.0.1,
                f.22602.0.2, f.22603.0.2,
                f.22602.0.3, f.22603.0.3,
                f.22602.0.4, f.22603.0.4,
                f.22602.0.5, f.22603.0.5,
                f.22602.0.6, f.22603.0.6,
                f.22602.0.7, f.22603.0.7,
                f.22602.0.8, f.22603.0.8,
                f.22602.0.9, f.22603.0.9,
                f.22602.0.10, f.22603.0.10,
                f.22602.0.11, f.22603.0.11,
                f.22602.0.12, f.22603.0.12,
                f.22602.0.13, f.22603.0.13,
                f.22602.0.14, f.22603.0.14)

# check if start or end-date is equal to year(date_first_attend_assessment_centre)
ukb %>%
  filter(f.eid %in% two_jobs_f.eid) %>%
  mutate(year_first_attend = year(date_first_attend_assessment_centre)) %>% # Extract year
  filter(
    if_any(starts_with("f.22602.0."), ~ . == year_first_attend) &  # Check start dates
      if_any(starts_with("f.22603.0."), ~ . == year_first_attend)   # Check end dates
  ) %>%
  dplyr::select(f.eid, 
                night_shift_work, 
                date_first_attend_assessment_centre,
                num_jobs_during_assessment,
                f.22602.0.0, f.22603.0.0, 
                f.22602.0.1, f.22603.0.1,
                f.22602.0.2, f.22603.0.2,
                f.22602.0.3, f.22603.0.3,
                f.22602.0.4, f.22603.0.4,
                f.22602.0.5, f.22603.0.5,
                f.22602.0.6, f.22603.0.6,
                f.22602.0.7, f.22603.0.7,
                f.22602.0.8, f.22603.0.8,
                f.22602.0.9, f.22603.0.9,
                f.22602.0.10, f.22603.0.10,
                f.22602.0.11, f.22603.0.11,
                f.22602.0.12, f.22603.0.12,
                f.22602.0.13, f.22603.0.13,
                f.22602.0.14, f.22603.0.14)

# How would the prevalence of InsomniaDisorder change in the categories 1,2,3 jobs  
# if one would move around the undecidable cases?

# a) Move undecidable cases in 2job category to 1job category
two_job_f.eid_undecidable <- ukb %>%
  filter(f.eid %in% two_jobs_f.eid) %>%
  mutate(year_first_attend = year(date_first_attend_assessment_centre)) %>% # Extract year
  filter(
    if_any(starts_with("f.22602.0."), ~ . == year_first_attend) &  # Check start dates
      if_any(starts_with("f.22603.0."), ~ . == year_first_attend)   # Check end dates
  ) %>%
  dplyr::select(f.eid, 
                night_shift_work, 
                date_first_attend_assessment_centre,
                num_jobs_during_assessment,
                f.22602.0.0, f.22603.0.0, 
                f.22602.0.1, f.22603.0.1,
                f.22602.0.2, f.22603.0.2,
                f.22602.0.3, f.22603.0.3,
                f.22602.0.4, f.22603.0.4,
                f.22602.0.5, f.22603.0.5,
                f.22602.0.6, f.22603.0.6,
                f.22602.0.7, f.22603.0.7,
                f.22602.0.8, f.22603.0.8,
                f.22602.0.9, f.22603.0.9,
                f.22602.0.10, f.22603.0.10,
                f.22602.0.11, f.22603.0.11,
                f.22602.0.12, f.22603.0.12,
                f.22602.0.13, f.22603.0.13,
                f.22602.0.14, f.22603.0.14) %>% pull(f.eid)

two_job_f.eid_undecidable
length(two_job_f.eid_undecidable) # 30

ukb %>%
  filter(f.eid %in% two_job_f.eid_undecidable) %>%
  dplyr::select(InsomniaDisorder) %>% table() # 5 with Insomnia disorder
# a) new prevalence in job1 category:
1827 + 30 # participants in cat 1
# cases now: 
1827*0.225
# new cases
1827*0.225 + 5
# prevalence
(1827*0.225 + 5)/(1827 + 30) # 0.223937

# What if we remove 30 cases from cat2 but none has InsomniaDisorder?
(1827*0.225)/(1827 + 30) # 0.22

# b) new prevalence in job2 category:
69 - 31
# cases now:
69*0.145
# new cases
69*0.145 - 5
# new prevalence
(69*0.145 - 5)/(69 - 30) # 0.13

# What if we remove 30 cases from cat2 but none has InsomniaDisorder?
(69*0.145)/(69 - 30) # 0.26

# c) Move undecidable cases in 3job category
ukb %>% filter(f.eid %in% three_jobs_f.eid) %>%
  dplyr::select(InsomniaDisorder) %>% table() # 1 case
# new prevalence 1 case removed
0/2 # 0%
# new prevalence 1 case not removed
1/2 # 33%




hist(ukb_reg_ID$age)
summary(ukb_reg_ID$age)
hist(ukb_reg$time_employed_in_current_job)
summary(ukb_reg_ID$time_employed_in_current_job)

ukb_reg_ID %>% mutate(difference_age_time_empl = age - time_employed_in_current_job) %>% # age at baseline - time employed in current job
  dplyr::select(f.eid,difference_age_time_empl, age, time_employed_in_current_job) %>% 
  arrange(difference_age_time_empl) %>% head(10)

ukb_reg_ID %>% mutate(difference_age_time_empl = age - time_employed_in_current_job) %>% # age at baseline - time employed in current job
  dplyr::select(difference_age_time_empl) %>% 
  arrange(difference_age_time_empl) %>%  # Sort the vector
  pull(difference_age_time_empl) %>%  # Extract the difference_age_time_empl column as a vector
  quantile(probs = c(0.01, 1), na.rm = TRUE)  # Calculate the 1st and 99th percentiles

sort(unique(ukb_reg_ID$age)) # nothing suspicious
sort(unique(ukb_reg_ID$time_employed_in_current_job)) # max 48 years, min 0 years
ukb %>% filter(f.eid == 4144504) # started at 10 years?

ukb_reg_ID %>% dplyr::select(f.eid, age, time_employed_in_current_job) %>%
  mutate(quartile_time_employed = ntile(time_employed_in_current_job, 4)) %>%  # Divide into quartiles
  group_by(quartile_time_employed) %>%                                         # Group by quartiles
  summarise(mean_age = mean(age, na.rm = TRUE))                                # Calculate the mean age for each quartile


# _Townsend deprivation index (189) vs ethnicity----
table(ukb_reg_ID$ethnicity_new)
table(is.na(ukb_reg_ID$ethnicity_new))
unique(ukb_reg_ID$f.189.0.0)

# Calculate the required statistics
overall_sd <- sd(ukb_reg_ID$f.189.0.0, na.rm = TRUE)  # Standard deviation of the deprivation index
white_median <- median(ukb_reg_ID %>% filter(ethnicity_new == "White") %>% pull(f.189.0.0), na.rm = TRUE)
black_median <- median(ukb_reg_ID %>% filter(ethnicity_new == "Black or Black British") %>% pull(f.189.0.0), na.rm = TRUE)

ukb_reg_ID %>%
  group_by(ethnicity_new) %>%
  mutate(median_deprivation = median(f.189.0.0, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ethnicity_new = factor(ethnicity_new, 
                                levels = unique(ethnicity_new[order(median_deprivation)]))) %>%  # Order by median deprivation
  filter(!is.na(ethnicity_new)) %>%
  ggplot(aes(x = ethnicity_new, y = f.189.0.0)) +
  geom_boxplot() +
  labs(title = "Townsend Deprivation Index by Ethnicity", 
       x = "", 
       y = "Deprivation Index") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels for readability
  ) +
  # Add annotations for standard deviation and medians
  annotate("text", x = 1, y = max(ukb_reg_ID$f.189.0.0, na.rm = TRUE) + 2, 
           label = paste("Overall SD deprivation index:", round(overall_sd, 2)), hjust = 0) +
  annotate("text", x = 2, y = max(ukb_reg_ID$f.189.0.0, na.rm = TRUE) + 2, 
           label = paste("Min Median:", round(white_median, 2)), hjust = -1) +
  annotate("text", x = 3, y = max(ukb_reg_ID$f.189.0.0, na.rm = TRUE) + 2, 
           label = paste("Max Median:", round(black_median, 2)), hjust = -2)


# _Income and ethnicity----
ukb_reg_ID %>%
  filter(f.738.0.0 != -1 & f.738.0.0 != -3) %>%  # Remove missing values
  filter(!is.na(f.738.0.0)) %>%
  filter(!is.na(ethnicity_new)) %>%
  ggplot(aes(x = ethnicity_new, y = f.738.0.0)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +  # Add median as red point
  labs(title = "Income by Ethnicity", 
       x = "Ethnicity", 
       y = "Income (f.738.0.0)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels for readability
  )

# _Logistic Regr: SWSD and income group (f.738.0.0)----

education_labels <- c(
  "1" = "College or University degree",
  "2" = "A levels/AS levels or equivalent",
  "3" = "O levels/GCSEs or equivalent",
  "4" = "CSEs or equivalent",
  "5" = "NVQ or HND or HNC or equivalent",
  "6" = "Other professional qualifications eg: nursing, teaching",
  "-7" = "None of the above",
  "-3" = "Prefer not to answer"
)
ukb_reg_ID$Education <- factor(ukb[f.eid %in% ukb_reg_ID$f.eid,]$f.6138.0.0, levels = names(education_labels), labels = education_labels)
ukb_reg_ID %>%
  filter(!is.na(f.738.0.0), !is.na(InsomniaDisorder)) %>%  # Filter out NAs
  #mutate(income_group = ntile(f.738.0.0, 5)) %>%  # Create quintiles of income
  group_by(f.738.0.0, InsomniaDisorder) %>%   # Group by income quintile and InsomniaDisorder
  summarise(count = n(), .groups = 'drop') %>%  # Count the number of each InsomniaDisorder level per quintile
  group_by(f.738.0.0) %>%  # Group by income quintile to calculate proportions
  mutate(proportion = count / sum(count)) %>%  # Calculate the proportion of each InsomniaDisorder level per quintile
  ggplot(aes(x = factor(f.738.0.0), y = proportion, fill = InsomniaDisorder)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "Proportion of Insomnia disorder", fill = "InsomniaDisorder") +
  theme_minimal() +
  ggtitle("Proportion of Insomnia disorder by Income Group") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


summary(ukb_reg_ID$f.189.0.0) # Townsend deprivation index (7.9.24: not on website)
sd(ukb_reg_ID$f.189.0.0, na.rm = TRUE)
ukb_reg_ID <- ukb_reg_ID %>%
  filter(f.738.0.0 != -1 & f.738.0.0 != -3) %>%  # Filter out -1 and -3
  mutate(f.738.0.0 = as.factor(f.738.0.0),  # Convert income to factor
         f.738.0.0 = fct_recode(f.738.0.0,  # Recode factor levels with meaningful labels
                                "Less than 18,000" = "1",
                                "18,000 to 30,999" = "2",
                                "31,000 to 51,999" = "3",
                                "52,000 to 100,000" = "4",
                                "Greater than 100,000" = "5"),
         f.189.0.0_scaled = scale(f.189.0.0))  # Scale deprivation index

logistic_model <- glm(InsomniaDisorder ~ f.189.0.0_scaled + 
                        f.738.0.0 + ethnicity_new + Education, 
                      data = ukb_reg_ID %>% 
                        filter(Education %nin% c("None of the above","Prefer not to answer")), 
                      family = binomial)
summary(logistic_model)
exp(coef(logistic_model))
tbl <- gtsummary::tbl_regression(logistic_model, exponentiate = TRUE)
tbl
check_model(logistic_model)
PseudoR2(logistic_model) # 0.01132935

df <- as_tibble(tbl, col_labels = TRUE)
write.xlsx(df, file = "./Tables/Suppl_logistic_regression_Insomnia_and_SES.xlsx")

# __compare model without ethnicity_new----
logistic_model_no_ethnicity <- glm(InsomniaDisorder ~ f.189.0.0_scaled + 
                                     f.738.0.0 + Education, 
                                   data = ukb_reg_ID %>% 
                                     filter(Education %nin% c("None of the above","Prefer not to answer")), 
                                   family = binomial)

# Create gtsummary table for the model with ethnicity
tbl_with_ethnicity <- gtsummary::tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    f.189.0.0_scaled ~ "Deprivation Index (Scaled)",
    f.738.0.0 ~ "Income Group",
    ethnicity_new ~ "Ethnicity"
  )
)
tbl_without_ethnicity <- gtsummary::tbl_regression(
  logistic_model_no_ethnicity,
  exponentiate = TRUE,
  label = list(
    f.189.0.0_scaled ~ "Deprivation Index (Scaled)",
    f.738.0.0 ~ "Income Group"
  )
)

combined_table <- gtsummary::tbl_merge(
  tbls = list(tbl_with_ethnicity, tbl_without_ethnicity),
  tab_spanner = c("**With Ethnicity**", "**Without Ethnicity**")
)
combined_table

# export:
combined_df <- as_tibble(combined_table, col_labels = TRUE)
write.xlsx(combined_df, file = "./Tables/Suppl_InsomniaDisorder_SES_with_without_ethn.xlsx", sheetName = "Results", rowNames = FALSE)

# Alternatively, you can use writexl to save the file
# library(writexl)
# write_xlsx(combined_df, path = "combined_regression_results.xlsx")

# Your Excel file will be saved in your working directory


# _SWSD and Deprivation Index----
ukb_reg_ID %>%
  filter(!is.na(f.189.0.0), !is.na(InsomniaDisorder)) %>%  # Filter out NAs
  mutate(deprivation_quintile = ntile(f.189.0.0, 5)) %>%  # Create quintiles of deprivation index
  group_by(deprivation_quintile, InsomniaDisorder) %>%   # Group by deprivation quintile and InsomniaDisorder
  summarise(count = n(), .groups = 'drop') %>%  # Count the number of each InsomniaDisorder level per quintile
  group_by(deprivation_quintile) %>%  # Group by deprivation quintile to calculate proportions
  mutate(proportion = count / sum(count)) %>%  # Calculate the proportion of each InsomniaDisorder level per quintile
  ggplot(aes(x = factor(deprivation_quintile), y = proportion, fill = InsomniaDisorder)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Deprivation Index Quintile", y = "Proportion of Insomnia disorder", fill = "InsomniaDisorder") +
  theme_minimal() +
  ggtitle("Proportion of Insomnia disorder by Deprivation Index Quintile") +
  theme(plot.title = element_text(hjust = 0.5))


# _SWSD and Education ----
# Coding: https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100305
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(InsomniaDisorder != "Prefer not to answer") %>%
  filter(!is.na(Education)) %>%
  group_by(Education, InsomniaDisorder) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = Education, y = proportion, fill = InsomniaDisorder)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Education Level", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Education Level") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# _SWSD and Income score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26411.0.0)) %>%  # Filter for non-missing Income score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(income_quintile = ntile(f.26411.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(income_quintile, InsomniaDisorder) %>%  # Group by Income quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(income_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Income Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Income Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Employment score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26412.0.0)) %>%  # Filter for non-missing Employment score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(employment_quintile = ntile(f.26412.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(employment_quintile, InsomniaDisorder) %>%  # Group by Employment quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(employment_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Employment Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Employment Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Health score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26413.0.0)) %>%  # Filter for non-missing Health score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(health_quintile = ntile(f.26413.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(health_quintile, InsomniaDisorder) %>%  # Group by Health quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(health_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Health Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Health Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Education score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26414.0.0)) %>%  # Filter for non-missing Education score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(education_quintile = ntile(f.26414.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(education_quintile, InsomniaDisorder) %>%  # Group by Education quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(education_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Education Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Education Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Housing score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26415.0.0)) %>%  # Filter for non-missing Housing score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(housing_quintile = ntile(f.26415.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(housing_quintile, InsomniaDisorder) %>%  # Group by Housing quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(housing_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Housing Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Housing Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Living Environment score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26417.0.0)) %>%  # Filter for non-missing Living Environment score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(living_env_quintile = ntile(f.26417.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(living_env_quintile, InsomniaDisorder) %>%  # Group by Living Environment quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(living_env_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Living Environment Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Living Environment Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# _SWSD and Crime score (England)----
ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  filter(!is.na(f.26416.0.0)) %>%  # Filter for non-missing Crime score (England)
  filter(InsomniaDisorder != "Prefer not to answer") %>%  # Filter out "Prefer not to answer" responses
  mutate(crime_quintile = ntile(f.26416.0.0, 5)) %>%  # Create quintiles (1 to 5)
  group_by(crime_quintile, InsomniaDisorder) %>%  # Group by Crime quintile and InsomniaDisorder status
  summarise(count = n()) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ggplot(aes(x = as.factor(crime_quintile), y = proportion, fill = InsomniaDisorder)) + 
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Crime Score Quintile (England)", y = "Proportion", fill = "Insomnia Disorder") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Proportion of Insomnia Disorder by Crime Quintile (England)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# _Logistic Regression InsomniaDisorder vs deprivation scores ----
filtered_data <- ukb %>%
  filter(f.eid %in% ukb_reg_ID$f.eid) %>% # Filter for participants in the regression dataset
  # Handle missing values for predictors if needed
  filter(!is.na(f.26411.0.0) & 
           !is.na(f.26412.0.0) & 
           !is.na(f.26413.0.0) & 
           !is.na(f.26414.0.0) & 
           !is.na(f.26415.0.0) & 
           !is.na(f.26416.0.0) & 
           !is.na(f.26417.0.0))

# Step 2: Define the first logistic model (with ethnicity)
logistic_model <- glm(InsomniaDisorder ~ 
                        f.26411.0.0 + 
                        f.26412.0.0 + 
                        f.26413.0.0 + 
                        f.26414.0.0 + 
                        f.26415.0.0 + 
                        f.26416.0.0 + 
                        f.26417.0.0 + 
                        ethnicity_new,  # Including ethnicity variable
                      data = filtered_data %>%
                        mutate(across(where(is.numeric), base::scale)), 
                      family = binomial)

# Step 3: Create gtsummary table for the model with ethnicity
tbl_with_ethnicity <- gtsummary::tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    f.26411.0.0 ~ "Income Score (England)",
    f.26412.0.0 ~ "Employment Score (England)",
    f.26413.0.0 ~ "Health Score (England)",
    f.26414.0.0 ~ "Education Score (England)",
    f.26415.0.0 ~ "Housing Score (England)",
    f.26416.0.0 ~ "Crime Score (England)",
    f.26417.0.0 ~ "Living Environment Score (England)",
    ethnicity_new ~ "Ethnicity"
  )
)

# Step 4: Define the second logistic model (without ethnicity)
logistic_model_without_ethnicity <- glm(InsomniaDisorder ~ 
                                          f.26411.0.0 + 
                                          f.26412.0.0 + 
                                          f.26413.0.0 + 
                                          f.26414.0.0 + 
                                          f.26415.0.0 + 
                                          f.26416.0.0 + 
                                          f.26417.0.0,  # Excluding ethnicity variable
                                        data = filtered_data %>%
                                          mutate(across(where(is.numeric), base::scale)), 
                                        family = binomial)

# Step 5: Create gtsummary table for the model without ethnicity
tbl_without_ethnicity <- gtsummary::tbl_regression(
  logistic_model_without_ethnicity,
  exponentiate = TRUE,
  label = list(
    f.26411.0.0 ~ "Income Score (England)",
    f.26412.0.0 ~ "Employment Score (England)",
    f.26413.0.0 ~ "Health Score (England)",
    f.26414.0.0 ~ "Education Score (England)",
    f.26415.0.0 ~ "Housing Score (England)",
    f.26416.0.0 ~ "Crime Score (England)",
    f.26417.0.0 ~ "Living Environment Score (England)"
  )
)

PseudoR2(logistic_model) # 0.01132935
PseudoR2(logistic_model_without_ethnicity) # 0.002

# Step 6: Combine the two gtsummary tables side-by-side
combined_tbl <- gtsummary::tbl_merge(
  tbls = list(tbl_with_ethnicity, tbl_without_ethnicity),
  tab_spanner = c("**With Ethnicity**", "**Without Ethnicity**")
)

combined_df <- as_tibble(combined_tbl, col_labels = TRUE)
write.xlsx(combined_df, file = "./Tables/Suppl_Scores_vs_InsomniaDisorder.xlsx", sheetName = "Results", rowNames = FALSE)



# _Overview deprivation scores-----
deprivation_data <- ukb %>% 
  filter(f.eid %in% ukb_reg_ID$f.eid) %>%  # Filter for participants in the regression dataset
  dplyr::select(
    Income_Score = f.26411.0.0,  # Income score (England)
    Employment_Score = f.26412.0.0,  # Employment score (England)
    Health_Score = f.26413.0.0,  # Health score (England)
    Education_Score = f.26414.0.0,  # Education score (England)
    Housing_Score = f.26415.0.0,  # Housing score (England)
    Crime_Score = f.26416.0.0,  # Crime score (England)
    Living_Environment_Score = f.26417.0.0,  # Living Environment score (England)
    Townsend_Index = f.189.0.0   # Townsend Deprivation Index
  ) %>% 
  mutate(across(where(is.numeric), scale)) %>%  # Scale all numeric columns
  filter(complete.cases(.))  # Remove rows with any missing values
deprivation_long <- deprivation_data %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Score_Type", 
    values_to = "Score"
  )
ggplot(deprivation_long, aes(x = Score_Type, y = Score)) +
  geom_boxplot(outlier.shape = NA) +  # Boxplot without outlier points (to avoid duplication)
  geom_jitter(width = 0.2, alpha = 0.03, color = "blue") +  # Add jittered points
  theme_minimal() +
  labs(x = "", y = "Score", 
       title = "(Scaled) Deprivation Scores and Townsend Index (with raw points)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))




# [Review round 1] Why include ethnicity-----
# And can you provide a discussion of what the purpose and relevance of 
# analyzing “ethnicity” is? What does it represent in the context of the research question?

# __Big 5 and ethnicity------
ukb_reg_ID %>%
  ggplot(aes(x=ethnicity_new, y=nervousness_scale)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Black or Black British lower

ukb_reg_ID %>%
  ggplot(aes(x=ethnicity_new, y=sociability_scale)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ukb_reg_ID %>%
  ggplot(aes(x=ethnicity_new, y=warmth_scale)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ukb_reg_ID %>%
  ggplot(aes(x=ethnicity_new, y=diligence_scale)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ukb_reg_ID %>%
  ggplot(aes(x=ethnicity_new, y=curiosity_scale)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Maybe do a spider plot or similar?

# __Alcohol and ethnicity----
chisq.test(ukb_reg_ID$alcohol_intake_frequency, ukb_reg_ID$ethnicity_new) 
ukb_reg_ID %>%
  filter(!is.na(alcohol_intake_frequency), !is.na(ethnicity_new)) %>%
  group_by(ethnicity_new, alcohol_intake_frequency) %>%
  dplyr::summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup() %>%
  # Reorder ethnicity based on the proportion of "Daily or almost daily" alcohol intake
  group_by(ethnicity_new) %>%
  mutate(daily_proportion = proportion[alcohol_intake_frequency == "Daily or almost daily"]) %>%
  ungroup() %>%
  mutate(ethnicity_new = fct_reorder(ethnicity_new, daily_proportion, .desc = TRUE)) %>%
  ggplot(aes(x = ethnicity_new, y = proportion, fill = alcohol_intake_frequency)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Ethnicity", y = "Proportion", fill = "Alcohol Intake Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Alcohol Intake Frequency Across Ethnic Groups Ordered by Daily or Almost Daily Intake")

# ---> ethnicity not independent from alcohol consumption

# __Smoking and ethnicity----
chisq.test(ukb_reg_ID$smoking_status, ukb_reg_ID$ethnicity_new) # sign

ukb_reg_ID %>%
  filter(!is.na(smoking_status), !is.na(ethnicity_new)) %>%
  group_by(ethnicity_new, smoking_status) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup() %>%
  # Reorder ethnicity based on the proportion of "Current" smokers
  group_by(ethnicity_new) %>%
  mutate(current_proportion = proportion[smoking_status == "Current"]) %>%
  ungroup() %>%
  mutate(ethnicity_new = fct_reorder(ethnicity_new, current_proportion, .desc = TRUE)) %>%
  ggplot(aes(x = ethnicity_new, y = proportion, fill = smoking_status)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Ethnicity", y = "Proportion", fill = "Smoking Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Smoking Proportions Across Ethnic Groups Ordered by Current Smoking")

# --> smoking not independent from ethnicity

# __BMI and ethnicity----
ukb_reg_ID %>%
  filter(!is.na(ethnicity_new)) %>%
  mutate(ethnicity_new = fct_reorder(ethnicity_new, BMI, .fun = median)) %>%
  ggplot(aes(x = ethnicity_new, y = BMI)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(aov(BMI ~ ethnicity_new, data = ukb_reg_ID)) # sign

# -> BMI not independent from ethnicity

# __Time in current job and ethnicity----
ukb_reg_ID %>%
  filter(!is.na(time_employed_in_current_job), !is.na(ethnicity_new)) %>%
  mutate(ethnicity_new = fct_reorder(ethnicity_new, time_employed_in_current_job, .fun = median)) %>%
  ggplot(aes(x = ethnicity_new, y = time_employed_in_current_job)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Ethnicity", y = "Time Employed in Current Job (years)") +
  ggtitle("Time Employed in Current Job Across Ethnic Groups Ordered by Median")
# -> for sure not independend

# __Chronotype and ethnicity----
ukb_reg_ID %>%
  filter(!is.na(chronotype), !is.na(ethnicity_new)) %>%
  group_by(ethnicity_new, chronotype) %>%
  dplyr::summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup() %>%
  # Reorder ethnicity based on the proportion of "Definitely a morning type"
  group_by(ethnicity_new) %>%
  mutate(morning_proportion = proportion[chronotype == "Definitely a morning person"]) %>%
  ungroup() %>%
  mutate(ethnicity_new = fct_reorder(ethnicity_new, morning_proportion, .desc = TRUE)) %>%
  ggplot(aes(x = ethnicity_new, y = proportion, fill = chronotype)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Ethnicity", y = "Proportion", fill = "Chronotype") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Chronotype Distribution Across Ethnic Groups Ordered by Definitely a Morning Type")
chisq.test(ukb_reg_ID$chronotype, ukb_reg_ID$ethnicity_new) # sign

# -> not independent

# __Choose an Outcome-----------------------------------------------------------
# Choose one of SWD_2/SWD_5/InsomniaDisorder:
#OLD___dep_variable <- "SWD_2" # aka any_insomnia 
#dep_variable <- "SWD_5" # aka frequent_insomnia (used in Lane et al. GWAS)
dep_variable <- "InsomniaDisorder" # (validated by Hammerschlag et al. )

if(dep_variable == "SWD_5"){
  ukb_reg <- ukb_reg %>% mutate(outcome = SWD_5) %>% 
    dplyr::select(-c(SWD_5, SWD_2, InsomniaDisorder))
  ukb_reg <- ukb_reg %>% filter(!is.na(outcome)) 
  #label(ukb_reg$outcome) <- dep_variable # label() command temporarily did not work (unsolved)
} else if(dep_variable == "SWD_2"){
  ukb_reg <- ukb_reg %>% mutate(outcome = SWD_2) %>% 
    dplyr::select(-c(SWD_5, SWD_2, InsomniaDisorder))
  ukb_reg <- ukb_reg %>% filter(!is.na(outcome)) 
  #label(ukb_reg$outcome) <- dep_variable
} else if(dep_variable == "InsomniaDisorder"){
  ukb_reg <- ukb_reg %>% mutate(outcome = InsomniaDisorder) %>% 
    dplyr::select(-c(SWD_5, SWD_2, InsomniaDisorder))
  ukb_reg <- ukb_reg %>% filter(!is.na(outcome)) 
  #label(ukb_reg$outcome) <- dep_variable
}

dim(ukb_reg) # 5659  for SWD_5; 10787 for SWD_2/InsomniaDisorder

# __Missingness plot------------------------------------------------------------
p <- vis_miss(ukb_reg)
p + theme(
  plot.margin = ggplot2::margin(1, 4, 1, 1, "cm"),
)
p + theme(
  plot.margin = ggplot2::margin(1, 4, 1, 1, "cm"),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  plot.title = element_text(size = 16),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 10)
)
p + theme(
  plot.margin = ggplot2::margin(1, 4, 1, 1, "cm"),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  axis.title.x = element_text(size = 18),
  axis.title.y = element_text(size = 18),
  plot.title = element_text(size = 20),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
)

# __Regression formula----------------------------------------------------------
reg_formula <- outcome ~ 
  ## Traits  ## 
  age + 
  sex + 
  ethnicity_new + 
  chronotype + 
  #neuroticism_score + 
  nervousness_scale +  # (caution: proxy for neuroticism, correlated!)
  sociability_scale + 
  warmth_scale + 
  diligence_scale + 
  curiosity_scale + 
  ## Modifiable risk factors ## 
  alcohol_intake_frequency + #ref-level: "Never"
  smoking_status + 
  BMI + 
  #MET_minutes_week +
  #as.factor(f.54.0.0), # Assessment Centre, see List_Ass_Centres.xlsx for Coding
  time_employed_in_current_job # Time employed in main current job


# _I) (Compl.Case) Classical logistic regression (2. column in Table2.xlsx) ----
model <- glm(formula = reg_formula,
             data = ukb_reg, family = "binomial")
#model <- glm(formula = reg_formula,
#             data = ukb_reg, family = "binomial")
summary(model)
ukb_reg_complete_case <- ukb_reg %>% dplyr::select(c("age", "sex", "ethnicity_new", "chronotype",
                            "nervousness_scale", "sociability_scale", "warmth_scale", "diligence_scale", "curiosity_scale",
                            "time_employed_in_current_job", "BMI", "smoking_status",
                            "coffee_intake", "alcohol_intake_frequency", "outcome"))
# ___N for Table 2:----
# dep_variable == "InsomniaDisorder":
dim(na.omit(ukb_reg_complete_case)) # now 4746

# dep_variable == "SWD_5")
dim(na.omit(ukb_reg_complete_case))  # 2483

# ___[Review round 1] Check linearity assumption of complete case model ----
library(mgcv)
model_gam <- gam(outcome ~ 
               ## Traits  ## 
               s(age, k = 4) +  # Based on unique values
               sex + 
               ethnicity_new + 
               chronotype + 
               s(nervousness_scale, k = 3) +  # Adjust based on unique values
               s(sociability_scale, k = 3) + 
               warmth_scale + 
               s(diligence_scale, k = 3) + 
               s(curiosity_scale, k = 3) + 
               ## Modifiable risk factors ## 
               alcohol_intake_frequency + 
               smoking_status + 
               s(BMI, k = 5) + 
               s(time_employed_in_current_job, k = 4),  
             family = binomial, data = ukb_reg)
# s() -> Function used in definition of smooth terms within gam model formulae. The function does not evaluate a (spline) smooth - it exists purely to help set up a model using spline based smooths.
summary(model_gam)
# R-sq.(adj) =  0.0631   Deviance explained = 6.41%
PseudoR2(model, which = NULL) # 0.05803668

tbl_regression(model, exponentiate = TRUE) %>%
  bold_labels() %>%
  add_q(method = "fdr") %>% # p-value adjustment with FDR
  modify_table_body(~.x %>% dplyr::select(-p.value)) # remove normal p-value column


# ____save complete case model----
if(dep_variable == "InsomniaDisorder"){
  saveRDS(model, "./Results/complete_case_InsomniaDisorder_25.7.24.RDS")  
}
if(dep_variable == "SWD_5"){
  saveRDS(model, "./Results/complete_case_freqInsomnia_25.7.24.RDS")
}


# __(Compl.Case) Model fit? -----
nullmod <- glm(outcome ~ 1, data = ukb_reg, family = "binomial")
1-logLik(model)/logLik(nullmod) # 0.29 / 0.61 (freq.ins./SWD_5)

pR2(model)['McFadden'] # 0.05803668  = log likelihood of null?/0.11 SWD_5
pR2(model)
1-pR2(model)['llh']/pR2(model)['llhNull'] # log likelihood of null differs!
logLik(model) # -2404.159 (df=26) / 'log Lik.' -1499.23 (df=26) for SWD_5
logLik(nullmod) # 'log Lik.' -6006.141 (df=1) vs log Lik.' -3910.218 (df=1) for SWD_5

#library(DescTools)
PseudoR2(model, which = NULL) # 0.05803668 / 0.1192356 SWD_5

#library(performance)
r2_mcfadden(model) # 0.60 / R2: 0.616 for SWD_5

# Maybe the answer is here: "The explanation for the large difference is 
# (I believe) that for the grouped binomial data setup, the model can 
# accurately predict the number of successes in a binomial observation 
# with n=1,000 with good accuracy. In contrast, for the individual binary 
# data model, the observed outcomes are 0 or 1, while the predicted outcomes 
# are 0.7 and 0.3 for x=0 and x=1 groups. The low R squared for the 
# individual binary data model reflects the fact that the covariate x does 
# not enable accurate prediction of the individual binary outcomes."
# https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/ 

# try: https://www.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915?journalCode=uasa20
# https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/bmsp.12289
# package sure

check_model(model) # ?

# TODO still necessary???
OR.compl.case <- round(exp(model$coefficients),3)
compl_case_CIs <- as.data.frame(exp(round(confint(model),2)))
df <- data.frame(Predictor = names(model$coefficients), 
                 log_OR = round(model$coefficients,3), 
                 OR = OR.compl.case, 
                 CIs = round(compl_case_CIs,3),
                 p.val = round(summary(model)$coefficients[,4],4))
df <- df %>% mutate(CI = paste0("(",CIs.2.5..,", ",CIs.97.5..,")")) %>%
  dplyr::select(-CIs.2.5.., -CIs.97.5..,-log_OR)

std_border = fp_border(color="black")
ft <- flextable(df)
ft <- vline(ft, j = 1, border = std_border)
#ft <- set_caption(ft, paste0("n = ",dim(ukb_reg)[1], ", n_compl = ",dim(na.omit(ukb_reg))[1], 
#                             ".; Version",v," big five; complete case; Logistic Model: 
#                             SWD_2 ~ Traits + Mod.risk.factors","; ",Sys.Date()))
ft <- width(ft, j = 4, width = 3, unit = "cm")
ft <- add_header_row(ft, top = TRUE, values = c("","Complete case"), colwidths = c(1,3))
ft

if(v == 2){
  saveRDS(df, "./UKB_data/complete_case_v2b5.RDS")
}
if(v == 1){
  saveRDS(df, "./UKB_data/complete_case_v1b5.RDS")
}



# _II) IMPUTATION:----
# Notes:
# Chapter 6 of Flexible imputation of missing data
# Which imputation model should be applied for which variable? DOUBLE-CHECK!
# The mice package contains the function quickpred() that implements the 
# predictor selection strategy of Section 6.3.2
# Are my factors already ordered? see also: https://data.library.virginia.edu/understanding-ordered-factors-in-a-linear-model/
# Interaction terms?

# ____Interaction-Terms (compl.case)? ----
tic()
lm_interactions <- FSA(formula = reg_formula, 
                       data = ukb_reg,
                       fitfunc = glm, m = 2,
                       interactions = TRUE, criterion = AIC, minmax = "min",  
                       numrs = 10,
                       family = "binomial")
toc() # 7.5s
summary(lm_interactions)
# -> potential interaction term: nervousness_scale_score:sociability_scale

# ____anova() for interaction term----------------------------------------------
reg_formula_interac <- outcome ~ age + sex + ethnicity_new + chronotype + 
  nervousness_scale + sociability_scale + warmth_scale + diligence_scale + 
  curiosity_scale + alcohol_intake_frequency + smoking_status + BMI + 
  time_employed_in_current_job + nervousness_scale:sociability_scale
model_interac <- glm(formula = reg_formula_interac,
             data = ukb_reg, family = "binomial")
vars_of_interest <- c("outcome", "age", "sex", "ethnicity_new", "chronotype",
                      "nervousness_scale", "sociability_scale", "warmth_scale",
                      "diligence_scale", "curiosity_scale", "alcohol_intake_frequency",
                      "smoking_status", "BMI", "time_employed_in_current_job")
ukb_reg <- as.data.table(ukb_reg)
ukb_reg_vars <- ukb_reg[ , ..vars_of_interest]
complete_cases <- complete.cases(ukb_reg_vars)
ukb_reg_complete <- ukb_reg[complete_cases, ]
model <- glm(formula = reg_formula,
             data = ukb_reg_complete, family = "binomial")
model_interac <- glm(formula = reg_formula_interac,
                     data = ukb_reg_complete, family = "binomial")
anova(model, model_interac)
# keep interaction?
pchisq(5.4008, df = 1, lower.tail = FALSE)
AIC(model)
AIC(model_interac)
BIC(model)
BIC(model_interac)
# -> leave out! (Note that it would be included if neuroticism_score was used)





# ___IMP1 (passive imputation of big 5 scales)----------------------------------

# ____Full model----
model <- glm(formula = reg_formula,
             data = ukb_reg, family = "binomial")
summary(model)

# ____Preparations for passive imputation of sum scores, ----
# see p. 27/28 of "mice: Multivariate Imputation by Chained Equations in R"

# Note, using passing imputation with mice:
# Redefine items BEFORE we put them in the sum score,
# Do not create NAs in the definition of the scale!

# nervousness scale
ukb_reg$f.1990.0.0 <- ifelse(ukb_reg$f.1990.0.0 == 1,1,ifelse(ukb_reg$f.1990.0.0 < 0,NA,0))
ukb_reg$f.1940.0.0 <- ifelse(ukb_reg$f.1940.0.0 == 1,1,ifelse(ukb_reg$f.1940.0.0 < 0,NA,0))
ukb_reg$f.2060.0.0 <- ifelse(ukb_reg$f.2060.0.0 %in% c(2,3,4),1,ifelse(ukb_reg$f.2060.0.0 < 0,NA,0))
ukb_reg$f.1920.0.0 <- ifelse(ukb_reg$f.1920.0.0 == 1,1,ifelse(ukb_reg$f.1920.0.0 < 0,NA,0))
ukb_reg$f.1950.0.0 <- ifelse(ukb_reg$f.1950.0.0 == 1,1,ifelse(ukb_reg$f.1950.0.0 < 0,NA,0))
# sociability scale
ukb_reg$f.1031.0.0 <- ifelse(ukb_reg$f.1031.0.0 >= 4,1,ifelse(ukb_reg$f.1031.0.0 < 0,NA,0))
ukb_reg$f.2030.0.0 <- ifelse(ukb_reg$f.2030.0.0 == 0,1,ifelse(ukb_reg$f.2030.0.0 < 0,NA,0))
ukb_reg$f.2080.0.0 <- ifelse(ukb_reg$f.2080.0.0 == 1,1,ifelse(ukb_reg$f.2080.0.0 < 0,NA,0))
ukb_reg$f.6160.0.0 <- ifelse(ukb_reg$f.6160.0.0 >= 1,1,ifelse(ukb_reg$f.6160.0.0 == -7,NA,0))
# warmth scale
ukb_reg$f.2110.0.0 <- ifelse(ukb_reg$f.2110.0.0 >= 2,1,ifelse(ukb_reg$f.2110.0.0 < 0,NA,0))
ukb_reg$f.1940.0.0 <- ifelse(ukb_reg$f.1940.0.0 == 0,1,ifelse(ukb_reg$f.1940.0.0 < 0,NA,0))
ukb_reg$f.1920.0.0 <- ifelse(ukb_reg$f.1920.0.0 == 0,1,ifelse(ukb_reg$f.1920.0.0 < 0,NA,0))
ukb_reg$f.1990.0.0 <- ifelse(ukb_reg$f.1990.0.0 == 0,1,ifelse(ukb_reg$f.1990.0.0 < 0,NA,0))
ukb_reg$f.1970.0.0 <- ifelse(ukb_reg$f.1970.0.0 == 0,1,ifelse(ukb_reg$f.1970.0.0 < 0,NA,0))
# diligence scale
ukb_reg$f.2060.0.0 <- ifelse(ukb_reg$f.2060.0.0 == 1,1,ifelse(ukb_reg$f.2060.0.0 < 0,NA,0))
ukb_reg$f.1960.0.0 <- ifelse(ukb_reg$f.1960.0.0 == 0,1,ifelse(ukb_reg$f.1960.0.0 < 0,NA,0))
ukb_reg$f.2040.0.0 <- ifelse(ukb_reg$f.2040.0.0 == 0,1,ifelse(ukb_reg$f.2040.0.0 < 0,NA,0))
ukb_reg$f.2000.0.0 <- ifelse(ukb_reg$f.2000.0.0 == 1,1,ifelse(ukb_reg$f.2000.0.0 < 0,NA,0))
# curiosity scale
ukb_reg$f.2020.0.0 <- ifelse(ukb_reg$f.2020.0.0 == 0,1,ifelse(ukb_reg$f.2020.0.0 < 0,NA,0))
ukb_reg$f.2010.0.0 <- ifelse(ukb_reg$f.2010.0.0 == 0,1,ifelse(ukb_reg$f.2010.0.0 < 0,NA,0))
ukb_reg$f.2070.0.0 <- ifelse(ukb_reg$f.2070.0.0 %in% c(2,3,4),1,ifelse(ukb_reg$f.2070.0.0 < 0,NA,0))
ukb_reg$f.2040.0.0 <- ifelse(ukb_reg$f.2040.0.0 == 1,1,ifelse(ukb_reg$f.2040.0.0 < 0,NA,0))

df_imp1 <- cbind(ukb_reg[, -c("neuroticism_score",
                              "nervousness_scale", 
                              "sociability_scale", 
                              "warmth_scale", 
                              "diligence_scale", 
                              "curiosity_scale"
)], 
nervousness_scale = NA,   # analog to beginning of p. 28
sociability_scale = NA,
warmth_scale = NA,
diligence_scale = NA,
curiosity_scale = NA
)
ini <- mice(df_imp1, max = 0, print = FALSE) # See p 266, 
# Flexible imputation, p.28 mice: Multivariate Imputation by Chained Equations in R


# Adapt methods
meth <- ini$method
meth["nervousness_scale"] <- "~I(f.1990.0.0 + f.1940.0.0 + f.2060.0.0 + f.1920.0.0 + f.1950.0.0)"
meth["sociability_scale"] <- "~I(f.1031.0.0 + f.2030.0.0 + f.2080.0.0 + f.6160.0.0)"
meth["warmth_scale"] <- "~I(f.2110.0.0 + f.1940.0.0 + f.1920.0.0 + f.1990.0.0 + f.1970.0.0)"
meth["diligence_scale"] <- "~I(f.2060.0.0 + f.1960.0.0 + f.2040.0.0 + f.2000.0.0)"
meth["curiosity_scale"] <- "~I(f.2020.0.0 + f.2010.0.0 + f.2070.0.0 + f.2040.0.0)"
# Above, the scores are defined without creating NAs.
# Note, if one defines these scores with NAs at this point, imputation will not 
# work and no complete data sets will be created. 

# ____Adapt predictor matrix ---- 
# A value of 1 indicates that the column variable is a predictor to impute the
# target (row) variable, and a 0 means that it is not used.

pred <- ini$predictorMatrix

#flextable(as.data.frame(pred))
#flextable(as.data.frame(pred) %>% rownames_to_column("column name"))

pred[, "nervousness_scale"] <- 1
pred[c("f.1990.0.0", "f.1940.0.0", "f.2060.0.0", "f.1920.0.0", "f.1950.0.0", "nervousness_scale"), "nervousness_scale"] <- 0
pred.df <- as.data.frame(pred)
pred[which(pred.df$nervousness_scale==1), c("f.1990.0.0", "f.1940.0.0", "f.2060.0.0", "f.1920.0.0", "f.1950.0.0")] <- 0

pred[,"sociability_scale"] <- 1
pred[c("f.1031.0.0", "f.2030.0.0", "f.2080.0.0", "f.6160.0.0", "sociability_scale"), "sociability_scale"] <- 0
pred.df <- as.data.frame(pred)
pred[which(pred.df$sociability_scale==1), c("f.1031.0.0", "f.2030.0.0", "f.2080.0.0", "f.6160.0.0")] <- 0

pred[,"warmth_scale"] <- 1
pred[c("f.2110.0.0", "f.1940.0.0", "f.1920.0.0", "f.1990.0.0", "f.1970.0.0", "warmth_scale"), "warmth_scale"] <- 0
pred.df <- as.data.frame(pred)
pred[which(pred.df$warmth_scale==1), c("f.2110.0.0", "f.1940.0.0", "f.1920.0.0", "f.1990.0.0", "f.1970.0.0")] <- 0

pred[,"diligence_scale"] <- 1
pred[c("f.2060.0.0", "f.1960.0.0", "f.2040.0.0", "f.2000.0.0", "diligence_scale"), "diligence_scale"] <- 0
pred.df <- as.data.frame(pred)
pred[which(pred.df$diligence_scale==1), c("f.2060.0.0", "f.1960.0.0", "f.2040.0.0", "f.2000.0.0")] <- 0

pred[,"curiosity_scale"] <- 1
pred[c("f.2020.0.0", "f.2010.0.0", "f.2070.0.0", "f.2040.0.0", "curiosity_scale"), "curiosity_scale"] <- 0
pred.df <- as.data.frame(pred)
pred[which(pred.df$curiosity_scale==1), c("f.2020.0.0", "f.2010.0.0", "f.2070.0.0", "f.2040.0.0")] <- 0



# ____Impute ----
sysinfo <- Sys.info()
if( grepl( "jaybook", sysinfo[4], fixed = TRUE) | 
    grepl( "JayBook", sysinfo[4], fixed = TRUE) ){
  tic("Passive imputation for big 5 traits")
  #imp1 <- futuremice(data = df_imp1, pred = pred, meth = meth, m = 24, n.core = 6)
  imp1 <- mice( data = df_imp1, m = 15, method = meth, predictorMatrix = pred, maxit = 20)
  toc(log = TRUE) # 934.234 sec elapsed
}
#if( sysinfo[1] == "Windows" ){
#  tic("Passive imputation for big 5 traits")
#  #imp1 <- mice(df_imp1, pred = pred, meth = meth, maxit = 20, seed = 10948, print = FALSE)
#  imp1 <- parlmice(data = df_imp1, pred = pred, meth = meth, 
#                   m = 3, maxit = 5, n.core = 6, n.imp.core = 4)
#  toc(log = TRUE)
#}

# Note, that the Warning could stem from the linear dependencies? 
# page 171 of the book Flexible imputation: 
# "The warning results from the linear dependencies among the predictors, which
# were introduced by adding whr. The mice() function checks for linear dependencies
# during the iterations, and temporarily removes predictors from the
# univariate imputation models where needed."

# Check if imputation worked:
dslist <- complete(imp1, "all", include = TRUE)
vis_miss(dslist[[1]]) # including missing values!
vis_miss(dslist[[2]]) # should be without NAs

# ____Save imputation result----
if(dep_variable == "InsomniaDisorder"){
  fullpath <- paste0("./Results/imp1_InsomniaDisorder_v",v,"b5_",Sys.Date(),".RDS")  
} else if (dep_variable == "SWD_5"){
  fullpath <- paste0("./Results/imp1_freqInsomnia_v",v,"b5_",Sys.Date(),".RDS")
}
saveRDS(imp1, fullpath)

#imp1 <- readRDS(file.choose())

# slow:
#densityplot(imp1) # shows kernel density estimates of the imputed and observed data

# ____Logistic regression with IMP1----

# Create string to execute (could be done easier)
string1 = "fit1 <- with(imp1, glm("
str2 <- deparse(reg_formula)
string2 = gsub(" ", "", paste0(str2[1],str2[2],str2[3]))
string3 = ", family = binomial))"
command_string = paste0(string1, string2, string3, sep = "")
eval(parse(text = command_string))

# _____save mice fit1----
if(dep_variable == "InsomniaDisorder"){
  saveRDS(fit1, "./Results/mice_imp1_InsomniaDisorder_25.7.24.RDS")
}
if(dep_variable == "SWD_5"){
  saveRDS(fit1, "./Results/mice_imp1_freqInsomnia_25.7.24.RDS")
}

#fit1 <- readRDS(file.choose())


pool_fit1 <- pool(fit1)
res1 <- summary(pool_fit1, conf.int = TRUE)
res1_ <- summary(pool_fit1, conf.int = TRUE, exponentiate = TRUE)
# Note: pool.r.squared can only be calculated for lm()
#flextable(res1_)
#tbl_regression(fit1) # works


colnames(res1)[length(colnames(res1))] <- "a97.5imp"
colnames(res1)[length(colnames(res1))-1] <- "a2.5imp"
colnames(res1_)[length(colnames(res1_))] <- "a97.5imp_"
colnames(res1_)[length(colnames(res1_))-1] <- "a2.5imp_"

compl_case_CIs <- as.data.frame(exp(round(confint(model),2)))
rownames(compl_case_CIs) <- NULL
colnames(compl_case_CIs) <- c("a2.5","a97.5")

df <- data.frame(
  Predictors = res1$term,
  OR.imp = round(exp(res1$estimate),3),
  CI_2.5imp = round(exp(res1$a2.5imp),2),
  CI_97.5imp = round(exp(res1$a97.5imp),2),
  CI_2.5imp_ = round(res1_$a2.5imp_,2),
  CI_97.5imp_ = round(res1_$a97.5imp_,2),
  p.val.imp = round(res1$p.value,5),
  OR.compl.case = round(exp(model$coefficients),3),
  CI_2.5 = round(compl_case_CIs$a2.5,2),
  CI_97.5 = round(compl_case_CIs$a97.5,2),
  p.val.compl.case = round(summary(model)$coefficients[,4],5)
) %>% mutate(CI_95_imputed = paste0("(",CI_2.5imp,", ",CI_97.5imp,")")) %>%
  mutate(CI_95_imputed_ = paste0("(",CI_2.5imp_,", ",CI_97.5imp_,")")) %>%
  mutate(CI_95_compl = paste0("(",CI_2.5,", ",CI_97.5,")")) %>%
  dplyr::select(-c(CI_2.5imp, CI_97.5imp, CI_2.5, CI_97.5)) %>%
  dplyr::select(Predictors, OR.imp, CI_95_imputed, p.val.imp, OR.compl.case, CI_95_compl, p.val.compl.case)

std_border = fp_border(color="black")
ft <- flextable(df)
ft <- vline(ft, j = 4, border = std_border )
ft <- vline(ft, j = 1, border = std_border )
ft <- add_header_row(ft,
                     values = c(" ","Imputation (passive)", "Complete case"),
                     colwidths = c(1, 3, 3)
)
ft <- align(
  ft,
  i = NULL,
  j = NULL,
  align = "center",
  part = "header"
)
#ft <- set_caption(ft, paste0("n_imp = ",dim(ukb_reg)[1], "; n_compl = ",dim(na.omit(ukb_reg))[1],". Logistic Regression for SWD_2 (any_ins; No/Yes); passive imputation for Big 5 (v1b5)","; ",Sys.Date()))
ft

if(v == 1){
  saveRDS(df, "./UKB_data/25_7_24_compl_case_v1_and_imp1_v1b5.RDS")
}
if(v == 2){
  saveRDS(df, "./UKB_data/22_4_24_compl_case_and_imp1_v2b5.RDS")  
}



# ___IMP2 (kNN)-----

tic()
ukb_reg_imputed <- kNN(ukb_reg, k = 5)
toc() # 90s/214.303 sec elapsed/189.829 sec elapsed
vis_miss(ukb_reg_imputed) # no missings

# Save imputed result
saveRDS(ukb_reg_imputed, "./UKB_data/ukb_reg_imputed.RDS")
# READ imputed results
ukb_reg_imputed <- readRDS("./UKB_data/ukb_reg_imputed.RDS")


# Compare distributions of original vs. imputed data set:
table(ukb_reg$nervousness_scale) / sum(table(ukb_reg$nervousness_scale))
table(ukb_reg_imputed$nervousness_scale) / sum(table(ukb_reg_imputed$nervousness_scale))

table(ukb_reg$sociability_scale) / sum(table(ukb_reg$sociability_scale))
table(ukb_reg_imputed$sociability_scale) / sum(table(ukb_reg_imputed$sociability_scale))

table(ukb_reg$warmth_scale) / sum(table(ukb_reg$warmth_scale))
table(ukb_reg_imputed$warmth_scale) / sum(table(ukb_reg_imputed$warmth_scale))

table(ukb_reg$diligence_scale) / sum(table(ukb_reg$diligence_scale))
table(ukb_reg_imputed$diligence_scale) / sum(table(ukb_reg_imputed$diligence_scale))

table(ukb_reg$curiosity_scale) / sum(table(ukb_reg$curiosity_scale))
table(ukb_reg_imputed$curiosity_scale) / sum(table(ukb_reg_imputed$curiosity_scale))
# -> looks rather nice!

# ___Logistic regression with IMP2----
reg_formula <- outcome ~ 
  ## Traits  ## 
  age + 
  sex + 
  ethnicity_new + 
  chronotype + 
  #neuroticism_score + 
  nervousness_scale +  # (caution: proxy for neuroticism)
  sociability_scale + 
  warmth_scale + 
  diligence_scale + 
  curiosity_scale + 
  ## Modifiable risk factors ## 
  alcohol_intake_frequency + #ref-level: "Never"
  smoking_status + 
  BMI + 
  #MET_minutes_week +
  #as.factor(f.54.0.0), # Assessment Centre, see List_Ass_Centres.xlsx for Coding
  time_employed_in_current_job # Time employed in main current job

ukb_reg_imputed$outcome <- ukb_reg$outcome # outcome should be InsomniaDisorder here
imp2_logreg <- glm(formula = reg_formula, data = ukb_reg_imputed, 
                   family = binomial(link = "logit"))
summary(imp2_logreg)
PseudoR2(imp2_logreg, which = NULL) # 0.06499483 
check_model(imp2_logreg)
gtsummary::tbl_regression(imp2_logreg, exponentiate = TRUE)


# ____Which where the main/max predicted probabilities for which observation?----
preds <- predict(imp2_logreg, type="response")
#max
max(preds) # 0.6962555
ukb_reg_imputed[which.max(preds),]
ukb_reg_imputed[which.max(preds),]$outcome # No
#min
min(preds) # 0.03362122
ukb_reg_imputed[which.min(preds),]
ukb_reg_imputed[which.min(preds),]$outcome # No

dfpreds <- as.data.frame(x = preds)
hist_density_plot <- ggplot(dfpreds, aes(x=preds)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.05) + 
  geom_density(alpha = .2, fill = "#FF6666") + 
  ggtitle("Distribution of probabilities for suffering from insomnia disorder (SWD)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("")

boxplot <- ggplot(dfpreds, aes(y=preds)) + 
  geom_boxplot(width=0.1) + 
  geom_jitter(aes(x=0, y=preds), width = 0.05, height = 0, 
              color = "blue", size = 1, alpha = 0.02) +  # Add jittered points
  coord_flip() + 
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    aspect.ratio = 1/7  
  ) + 
  xlab("") + ylab("") +
  theme(axis.title.y = element_blank())

combined_plot <- hist_density_plot / boxplot
print(combined_plot)

# Predictions - Visualizations--------
# Load necessary libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggeffects, ggplot2)

# Step 1: Draw samples from existing variables to create a parameter space
n_samples <- 10000  # Number of samples for parameter space

# Randomly sample from the existing data to create a parameter space
param_space <- data.frame(
  age = sample(ukb_reg_imputed$age, n_samples, replace = TRUE),
  sex = sample(ukb_reg_imputed$sex, n_samples, replace = TRUE),
  ethnicity_new = sample(ukb_reg_imputed$ethnicity_new, n_samples, replace = TRUE),
  chronotype = sample(ukb_reg_imputed$chronotype, n_samples, replace = TRUE),
  nervousness_scale = sample(ukb_reg_imputed$nervousness_scale, n_samples, replace = TRUE),
  sociability_scale = sample(ukb_reg_imputed$sociability_scale, n_samples, replace = TRUE),
  warmth_scale = sample(ukb_reg_imputed$warmth_scale, n_samples, replace = TRUE),
  diligence_scale = sample(ukb_reg_imputed$diligence_scale, n_samples, replace = TRUE),
  curiosity_scale = sample(ukb_reg_imputed$curiosity_scale, n_samples, replace = TRUE),
  alcohol_intake_frequency = sample(ukb_reg_imputed$alcohol_intake_frequency, n_samples, replace = TRUE),
  smoking_status = sample(ukb_reg_imputed$smoking_status, n_samples, replace = TRUE),
  BMI = sample(ukb_reg_imputed$BMI, n_samples, replace = TRUE),
  time_employed_in_current_job = sample(ukb_reg_imputed$time_employed_in_current_job, n_samples, replace = TRUE)
)

# Step 2: Use the `predict` function to calculate probabilities
# Predict linear predictor and probability
param_space$linear_predictor <- predict(imp2_logreg, newdata = param_space, type = "link")
param_space$predicted_probability <- predict(imp2_logreg, newdata = param_space, type = "response")

# Step 3: Add raw states (InsomniaDisorder) and a ribbon to the plot
ggplot(ukb_reg_imputed %>% 
         mutate(
           linear_predictor = predict(imp2_logreg, newdata = ., type = "link"),
           raw_outcome = as.numeric(outcome) - 1  # Adjust outcome (Yes=1, No=0)
         ),
       aes(x = linear_predictor, y = raw_outcome)) +
  geom_jitter(
    color = "darkgrey",
    alpha = 0.5,
    width = 0.2,
    height = 0.02
  ) +  # Raw states jittered for better visibility
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    fill = "blue",
    alpha = 0.2,
    color = "red"
  ) +  # Add smooth curve with ribbon
  theme_minimal() +
  labs(
    title = "Raw Outcome and Smoother for Insomnia Disorder",
    x = "Linear Predictor",
    y = "Outcome (0 = No, 1 = Yes)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Bootstrapped CIs for predicted probabilities---------
# imho, the bands are way too narrow...
library(boot)
# Define a function for bootstrapping
boot_fn <- function(data, indices) {
  # Resample data
  sample_data <- data[indices, ]
  
  # Refit the model
  model <- glm(outcome ~ age + sex + ethnicity_new + chronotype +
                 nervousness_scale + sociability_scale + warmth_scale +
                 diligence_scale + curiosity_scale +
                 alcohol_intake_frequency + smoking_status +
                 BMI + time_employed_in_current_job, 
               data = sample_data, family = binomial)
  
  # Predict probabilities on the parameter space
  predict(model, newdata = param_space, type = "response")
}

# Perform bootstrap
set.seed(123)
boot_results <- boot(data = ukb_reg_imputed, statistic = boot_fn, R = 100)

# Calculate confidence intervals for each prediction in param_space
ci <- apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975))
param_space$lower_ci <- ci[1, ]
param_space$upper_ci <- ci[2, ]

# Plot with bootstrap confidence intervals
ggplot(param_space, aes(x = linear_predictor, y = predicted_probability)) +
  geom_point(alpha = 0.1, color = "blue") +
  geom_jitter(
    data = ukb_reg_imputed %>%
      mutate(
        linear_predictor = predict(imp2_logreg, newdata = ., type = "link"),
        raw_outcome = as.numeric(outcome) - 1
      ),
    aes(x = linear_predictor, y = raw_outcome),
    color = "darkgrey",
    alpha = 0.5,
    width = 0.2,
    height = 0.02,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = predicted_probability), color = "red") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.2) +
  theme_minimal() +
  labs(
    title = "Bootstrap Confidence Intervals for Predicted Probabilities",
    x = "Linear Predictor",
    y = "Predicted Probability"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
# save
ggplot2::ggsave("./Plots/Bootstrapped_CIs_for_predicted_probabilities.png")
ggplot2::ggsave("./Plots/Bootstrapped_CIs_for_predicted_probabilities.pdf")




# ____Confusion table of predicted vs. "true" results.----
tab <- table(ukb_reg_imputed$outcome, ifelse(preds>0.5, "Yes", "No"))
tab
(tab[1,1] + tab[2,2])/sum(tab) # 0.7549828 correctly classified
# not really exciting?
# Compare to base-rate:
table(ukb_reg$outcome)
sum(ukb_reg$outcome == "No")/sum(table(ukb_reg$outcome))
# 0.7549828 correctly classified, if one just guesses "No" for everyone.
# The model does nothing here!
# Note, the classification is better for SWD_5: 0.6862709 for regression compared to 0.5338674

imp2_regtable <- gtsummary::tbl_regression(imp2_logreg, exponentiate = TRUE)%>%
  bold_labels() %>%
  add_q(method = "fdr") %>% # p-value adjustment with FDR
  modify_table_body(~.x %>% dplyr::select(-p.value)) # remove normal p-value column
imp2_regtable

imp2_regtable %>% 
  gtsummary::as_tibble() %>%
  writexl::write_xlsx(., "./Table2_kNN_supplement_25_7_24.xlsx") # TODO delete 3 "Never lines" (unsolved)

# Model fit:
check_model(imp2_logreg, 
            check = c("vif", "binned_residuals", "outliers")) # residuals not optimal, but not sure if that is even a problem here....
check_model(imp2_logreg)
hoslem.test(imp2_logreg$y, fitted(imp2_logreg))
plot(imp2_logreg)
# binned residuals:
residuals <- residuals(imp2_logreg, type = "deviance")
fitted_values <- fitted(imp2_logreg)
n_bins <- 10
bin_breaks <- quantile(fitted_values, probs = seq(0, 1, length.out = n_bins + 1))
bin_labels <- 1:n_bins
fitted_bins <- cut(fitted_values, breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE)
avg_residuals <- tapply(residuals, fitted_bins, mean)
plot_data <- data.frame(
  PredictedProbability = bin_breaks[-1],
  AverageResidual = avg_residuals
)
ggplot(plot_data, aes(x = PredictedProbability, y = AverageResidual)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Binned Residuals Plot", 
       x = "Predicted Probability", 
       y = "Average Residual") +
  theme_minimal()

# ___[Review round 1] 3)	General analysis: Please clarify the methods section----
# to describe the models used in greater detail. It looks like models were
# adjusted for multiple factors, but then the model estimates for each of 
# the factors are presented as if the factors are the exposure? Consider 
# focusing on a few specific factors and then developing individual models
# that are specific to that factor (e.g., that are adjusted for things that 
# may confound the association between the factor of interest and SWSD)

# ___[SUPPLEMENT] Build more models as sensitivity analysis---------
reg_formula <- outcome ~ 
  ## Traits  ## 
  age + 
  sex + 
  ethnicity_new + 
  chronotype + 
  #neuroticism_score + 
  nervousness_scale +  # (caution: proxy for neuroticism, correlated!)
  sociability_scale + 
  warmth_scale + 
  diligence_scale + 
  curiosity_scale + 
  ## Modifiable risk factors ## 
  alcohol_intake_frequency + #ref-level: "Never"
  smoking_status + 
  BMI + 
  #MET_minutes_week +
  #as.factor(f.54.0.0), # Assessment Centre, see List_Ass_Centres.xlsx for Coding
  time_employed_in_current_job # Time employed in main current job

data <- ukb_reg_ID
data$outcome <- ukb_reg_ID$InsomniaDisorder

tic()
data <- kNN(data, k = 5)
toc() # 90s/214.303 sec elapsed/189.829 sec elapsed

# Define the list of models with respective formulas
model_formulas <- list(
  "Model 1: Age and Sex" = "outcome ~ age + sex",
  "Model 2: Ethnicity" = "outcome ~ age + sex + ethnicity_new",
  "Model 3: Chronotype" = "outcome ~ age + sex + ethnicity_new + chronotype",
  "Model 4: Personality Traits" = "outcome ~ age + sex + ethnicity_new + chronotype + nervousness_scale + sociability_scale + warmth_scale + diligence_scale + curiosity_scale",
  "Model 5: Alcohol Intake Frequency" = "outcome ~ age + sex + ethnicity_new + chronotype + nervousness_scale + sociability_scale + warmth_scale + diligence_scale + curiosity_scale + alcohol_intake_frequency",
  "Model 6: Smoking Status" = "outcome ~ age + sex + ethnicity_new + chronotype + nervousness_scale + sociability_scale + warmth_scale + diligence_scale + curiosity_scale + alcohol_intake_frequency + smoking_status",
  "Model 7: BMI" = "outcome ~ age + sex + ethnicity_new + chronotype + nervousness_scale + sociability_scale + warmth_scale + diligence_scale + curiosity_scale + alcohol_intake_frequency + smoking_status + BMI",
  "Model 8: Full Model" = "outcome ~ age + sex + ethnicity_new + chronotype + 
                            nervousness_scale + sociability_scale + warmth_scale + 
                            diligence_scale + curiosity_scale + 
                            alcohol_intake_frequency + smoking_status + BMI + 
                            time_employed_in_current_job"
)

# Fit the models and store them in a list
models <- model_formulas %>%
  map(~ glm(as.formula(.x), data = data, family = binomial))

# Create individual gtsummary tables for each model
model_summaries <- models %>%
  map(~ tbl_regression(.x, exponentiate = TRUE))

# Merge the summaries side-by-side with model names
combined_table <- model_summaries %>%
  tbl_merge(
    tab_spanner = c("**Model 1: Age and Sex**", 
                    "**Model 2: Ethnicity**", 
                    "**Model 3: Chronotype**", 
                    "**Model 4: Personality Traits**", 
                    "**Model 5: Alcohol Intake Frequency**", 
                    "**Model 6: Smoking Status**", 
                    "**Model 7: BMI**", 
                    "**Model 8: Full Model**")
  )

combined_table

gtsummary::as_tibble(combined_table) %>%
  writexl::write_xlsx(., "./Tables/Suppl_specific_models_28_9_24.xlsx")




# __Compare distributions of m(=24) mice-imputations: -----

completed_data <- complete(imp1, 1)
completed_data_subset <- completed_data[, c("nervousness_scale", "sociability_scale", "warmth_scale", "diligence_scale", "curiosity_scale")]

plots <- lapply(names(completed_data_subset), function(x) {
  ggplot(completed_data_subset, aes_string(x)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, fill = "blue", alpha = 0.7) +
    ggtitle(paste(x)) +
    ylab("Relative Frequency") + 
    theme(plot.title = element_text(hjust = 0.5))
})
do.call(grid.arrange, plots)

completed_data <- complete(imp1, 3)
completed_data_subset <- completed_data[, c("nervousness_scale", "sociability_scale", "warmth_scale", "diligence_scale", "curiosity_scale")]

plots <- lapply(names(completed_data_subset), function(x) {
  ggplot(completed_data_subset, aes_string(x)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, fill = "red", alpha = 0.7) +
    ggtitle(paste(x)) +
    ylab("Relative Frequency") + 
    theme(plot.title = element_text(hjust = 0.5))
})
do.call(grid.arrange, plots)

# Visualize the missing data pattern before imputation
#aggr_plot <- aggr(imp1$data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(imp1$data), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))


# VERY SLOW______stripplot(imp1)
# The mice package contains several graphic functions that can be used to gain
# insight into the correspondence of the observed and imputed data: 
# bwplot(), stripplot(), densityplot() and xyplot().

dslist <- complete(imp1, "all", include = TRUE)
#bwplot(imp2)

#vis_miss(dslist[[1]]) # including missing values!
#vis_miss(dslist[[2]]) # ... no missing values
#vis_miss(dslist[[3]]) # ... no missing values

# ____Categorical variables----
# _____Ethnicity-----
for(i in seq(dslist)){
  df.imp <- dslist[[i]]
  print(table(df.imp$ethnicity)/sum(table(df.imp$ethnicity)))
}

# ______For supplement:----
result_df <- data.frame(
  Imputation = integer(),
  White = numeric(),
  Mixed = numeric(),
  AsianOrAsianBritish = numeric(),
  BlackOrBlackBritish = numeric()
)

# Loop through your data sets
for(i in seq_along(dslist)){
  df.imp <- dslist[[i]]
  tbl <- table(df.imp$ethnicity)/sum(table(df.imp$ethnicity))
  
  # Convert the table to a data frame for easier manipulation
  tbl_df <- as.data.frame(as.table(tbl))
  
  # Create a single row data frame with your calculations
  single_row <- data.frame(
    Imputation = i,
    White = tbl["White"],
    Mixed = tbl["Mixed"],
    AsianOrAsianBritish = tbl["Asian or Asian British"],
    BlackOrBlackBritish = tbl["Black or Black British"],
    Chinese = tbl["Chinese"],
    Otherethnicgroup = tbl["Other ethnic group"]
  )
  
  # Add the single row to the results data frame
  result_df <- bind_rows(result_df, single_row)
}
result_df
rownames(result_df) <- NULL
colnames(result_df)[4] <- "Asian Or Asian British"
colnames(result_df)[5] <- "Black Or Black British"
colnames(result_df)[7] <- "Other ethnic group"

write_xlsx(round(result_df,3), "./Tables/Suppl_imputation_ethnicity.xlsx")
write_xlsx(round(result_df,3), "./Suppl_imputation_ethnicity.xlsx")

my_flextable <- flextable::qflextable(result_df)
my_flextable <- set_caption(my_flextable, caption = "Ethnicity proportions in 15 imputations plus original (imputation # 1)")
my_flextable


# _____Chronotype-----
for(i in seq(dslist)){
  df.imp <- dslist[[i]]
  print(table(df.imp$chronotype)/sum(table(df.imp$chronotype)))
}

# _____Smoking status-----
for(i in seq(dslist)){
  df.imp <- dslist[[i]]
  print(table(df.imp$smoking_status)/sum(table(df.imp$smoking_status)))
}

# _____Alcohol intake frequency-----
for(i in seq(dslist)){
  df.imp <- dslist[[i]]
  print(table(df.imp$alcohol_intake_frequency)/sum(table(df.imp$alcohol_intake_frequency)))
} # identical since nothing was imputed, no missings here.

# ____Continuous variables----
# _____MET_minutes_week--
#plot(density(na.omit(dslist[[1]]$MET_minutes_week)), main = "MET minutes per week")
#for (i in  seq(dslist)[-1]) {
#  lines(density(dslist[[i]]$MET_minutes_week), col = i)
#}

# ____(not so good) Nervousness_scale----
results_list <- list()
for(i in seq_along(dslist)) {
  if(i == 1){
    proportions <- table(ukb_reg$nervousness_scale) / sum(table(ukb_reg$nervousness_scale))
    results_list[[i]] <- proportions 
  } else {
    df.imp <- dslist[[i]]
    proportions <- table(df.imp$nervousness_scale) / sum(table(df.imp$nervousness_scale))
    results_list[[i]] <- proportions 
  }
}
result_df <- do.call("rbind", results_list)
result_df


# ____(not so good) sociability_scale----
results_list <- list()
for(i in seq_along(dslist)) {
  if(i == 1){
    proportions <- table(ukb_reg$sociability_scale) / sum(table(ukb_reg$sociability_scale))
    results_list[[i]] <- proportions 
  } else {
    df.imp <- dslist[[i]]
    proportions <- table(df.imp$sociability_scale) / sum(table(df.imp$sociability_scale))
    results_list[[i]] <- proportions 
  }
}
result_df <- do.call("rbind", results_list)
result_df <- as.data.frame(result_df)
write_xlsx(round(result_df,3), "./Tables/Suppl_imputation_sociability_scale.xlsx")
write_xlsx(round(result_df,3), "./Suppl_imputation_sociability_scale.xlsx")

# ____(good) warmth_scale----
results_list <- list()
for(i in seq_along(dslist)) {
  if(i == 1){
    proportions <- table(ukb_reg$warmth_scale) / sum(table(ukb_reg$warmth_scale))
    results_list[[i]] <- proportions 
  } else {
    df.imp <- dslist[[i]]
    proportions <- table(df.imp$warmth_scale) / sum(table(df.imp$warmth_scale))
    results_list[[i]] <- proportions 
  }
}
result_df <- do.call("rbind", results_list)
result_df

# ____(fair) diligence_scale----
results_list <- list()
for(i in seq_along(dslist)) {
  if(i == 1){
    proportions <- table(ukb_reg$diligence_scale) / sum(table(ukb_reg$diligence_scale))
    results_list[[i]] <- proportions 
  } else {
    df.imp <- dslist[[i]]
    proportions <- table(df.imp$diligence_scale) / sum(table(df.imp$diligence_scale))
    results_list[[i]] <- proportions 
  }
}
result_df <- do.call("rbind", results_list)
result_df


# ____(fair) curiosity_scale----
results_list <- list()
for(i in seq_along(dslist)) {
  if(i == 1){
    proportions <- table(ukb_reg$curiosity_scale) / sum(table(ukb_reg$curiosity_scale))
    results_list[[i]] <- proportions 
  } else {
    df.imp <- dslist[[i]]
    proportions <- table(df.imp$curiosity_scale) / sum(table(df.imp$curiosity_scale))
    results_list[[i]] <- proportions 
  }
}
result_df <- do.call("rbind", results_list)
result_df

# Runs through with mere warnings at imputation, v=1 and outcome=InsomniaDisorder
today() # "2024-07-25"
writeLines(capture.output(sessionInfo()), "session_info_Main_Analysis_25.7.24.txt")

