# Main_Analysis
# https://github.com/jdegenfellner/Paper1-dissertation/blob/main/Main_Analysis.R

# Infos:
# survey regression book: https://tidy-survey-r.github.io/tidy-survey-book/c04-getting-started.html

# IMPORTANT:----
# Use the following outcomes:
# sleepWorkDays_bw for 2017 and sleepWorkDays for 2020. 
# sleepWorkDays for 2017 is the self-reported sleep duration.
# Sleep duration via bed an wake times shall be used in the main analysis.

# I am aware of the typo in "marrital_status".

# WHICH data set to USE?---------
analytical_data_set_version <- "(1)" # throw out rows
#analytical_data_set_version <- "(2)" # keep rows, set NA

# Bootstrap replicates:---------
n_bootstraps_par_global <- 1000 # define number for all 3 analyses

pacman::p_load(
  haven, data.table, foreign, Hmisc, tidyverse, plyr, ggpubr, fancycut, 
  table1, tableone, readxl, DescTools, gtsummary, lmtest, rFSA, gt, flextable, 
  spatstat, plm, DataExplorer, olsrr, MASS, jtools, interactions, randomForest, 
  cNORM, limma, performance, car, ggstance, survey, tictoc, quantreg,YesSiR, 
  igraph, ggraph, tidygraph
)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# READ prepared and cleaned files-----------------------------------------------
if(analytical_data_set_version == "(1)"){ # throw out rows
  sleep17 <- readRDS("./DATA/sleep17_an.RDS") 
  sleep20 <- readRDS("./DATA/sleep20_an.RDS")
} else if (analytical_data_set_version == "(2)"){ # keep rows, set NA
  sleep17 <- readRDS("./DATA/sleep17.RDS") 
  sleep20 <- readRDS("./DATA/sleep20.RDS")
}

analytical_data_set_version
dim(sleep17) # 784
dim(sleep20) # 847


#  SURVEY WEIGHTS Age+Sex 2017-----------
# _Austrian population 2017-----
bev2017_age_sex <- read_excel("./DATA/Bev_Alter_Geschl_Bundesl_seit_2011.xlsx", 
                              sheet = "2017_age_sex")
head(bev2017_age_sex)
tail(bev2017_age_sex)
bev2017_age_sex$Age_group <- wafflecut(
  bev2017_age_sex$Alter_Jahre, 
  c('[0,19]', '[20,24]', '[25,29]', '[30,34]', '[35,39]', '[40,44]', 
    '[45,49]', '[50,54]', '[55,59]', '[60,64]', '[65,69]', '[70,100]'), 
  c('<20 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', 
    '40-44 years', '45-49 years', '50-54 years', '55-59 years', 
    '60-64 years', '65-69 years', '70 or older')
)
bev2017_age_sex <- bev2017_age_sex %>% 
  dplyr::mutate(gender = ifelse(Geschlecht == "Mann", "Men", "Women"))

bev2017_age_sex %>%
  group_by(gender) %>%
  dplyr::summarize(Anzahl = sum(Anzahl)) %>%
  mutate(Prozent = Anzahl / sum(Anzahl))
# Frau       4480353   0.508
# Mann       4331429   0.492

# _2017 sleep sample----
unique(sleep17$age)
sleep17$Age_cat <- wafflecut(sleep17$Q1_help, c('[0,19]', '[20,24]', '[25,29]', '[30,34]', '[35,39]', '[40,44]', 
                                           '[45,49]', '[50,54]', '[55,59]', '[60,64]', '[65,69]', '[70,100]'), 
                             c('<20 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', 
                               '40-44 years', '45-49 years', '50-54 years', '55-59 years', 
                               '60-64 years', '65-69 years', '70 or older')
)
sleep17$Age_cat
sleep17$gender

# _define weights age sex 2017--------

# https://doi.org/10.3758/s13428-023-02207-0
# To achieve representativeness, the weight w should be equal
# to the ratio between the proportion of a particular stratum in the
# reference population and its proportion in the collected sample (Lumley,
# 2011; Mercer et al. 2018).
# https://stats.oarc.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/

# Compute Austrian population proportions
pop_proportions <- bev2017_age_sex %>%
  dplyr::group_by(Age_group, gender) %>%
  dplyr::summarise(Population = sum(Anzahl), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Pop_Proportion = Population / sum(Population))
head(pop_proportions)
sum(pop_proportions$Population) # 8,811,782
sum(pop_proportions$Pop_Proportion) # 1
sum(pop_proportions$Pop_Proportion[pop_proportions$gender == "Men"])    # Should be ~0.5
sum(pop_proportions$Pop_Proportion[pop_proportions$gender == "Women"])  # Should be ~0.5

# Since 70 or older is missing in 2017 (but not in 2020) -> merge last to age groups
# otherwise post stratification will not work

# Define new categorical age variable with merged category for 2017:
sleep17 <- sleep17 %>%
  dplyr::mutate(
    Age_cat_merged = ifelse(Age_cat %in% c("70 or older", "65-69 years"), "65 or older", as.character(Age_cat)),
    Age_cat_merged = factor(Age_cat_merged) %>% fct_drop()
  )
sleep17$Age_cat_merged

pop_proportions <- pop_proportions %>%
  dplyr::mutate(Age_group_merged = ifelse(Age_group %in% c("70 or older", "65-69 years"), 
                                   "65 or older", as.character(Age_group)) %>% factor()) 
# drop levels:
pop_proportions <- pop_proportions %>% dplyr::select(-Age_group)
tail(pop_proportions)
pop_proportions <- pop_proportions %>% dplyr::group_by(Age_group_merged, gender) %>%
  dplyr::mutate(Population = sum(Population)) %>% 
  dplyr::mutate(Pop_Proportion = sum(Pop_Proportion)) %>%
  distinct()
  
sample_proportions <- sleep17 %>%
  dplyr::group_by(Age_cat_merged, gender) %>%
  dplyr::summarise(Sample_Size = n(), .groups = "drop") %>%
  dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))
head(sample_proportions)
tail(sample_proportions)
table(sleep17$Age_cat_merged, sleep17$gender)

dim(sample_proportions) # 22 4
dim(pop_proportions) # 22 4

sleep17$Age_group_merged <- sleep17$Age_cat_merged
colnames(sample_proportions) <- c("Age_group_merged", "Gender", "Freq", "Proportion_in_stratum")
pop_proportions <- pop_proportions %>% dplyr::select(Age_group_merged, gender, Population, Pop_Proportion)
colnames(pop_proportions) <- colnames(sample_proportions)

colnames(pop_proportions)
colnames(sample_proportions)

sleep17$Gender <- sleep17$gender
design_unweighted <- svydesign(ids = ~1, data = sleep17, weights = ~1)

post_strata <- pop_proportions %>%
  dplyr::select(Age_group_merged, Gender, Freq)

design_weighted <- postStratify(design_unweighted, ~Age_group_merged + Gender, 
                                post_strata, partial = TRUE)

saveRDS(design_weighted, "./DATA/design_weighted.RDS")


# Extract computed weights
sleep17$weight <- weights(design_weighted)

# Check if total weight sum matches sample size
sum(sleep17$weight)  # Should be close to original sample size (8811782)
#8811782

# Compute total sample size
total_sample_size <- nrow(sleep17)

# Rescale weights so they sum to the total sample size
sleep17$weight <- weights(design_weighted) * (total_sample_size / sum(weights(design_weighted)))

getwd()
saveRDS(sleep17$weight, "./DATA/weights_to_reflect_sample_size_2017.RDS")

# Verify if rescaling worked
sum(sleep17$weight)  # Should now be close to `nrow(sleep17)`

# Inspect weight distribution
summary(sleep17$weight)
hist(sleep17$weight, main = "Post-Stratified Weights Distribution", col = "blue")

pop_proportions
sample_proportions

# check weights for <20 years:
sleep17 %>% dplyr::filter(Age_group_merged == "<20 years" & Gender == "Women") %>% 
  dplyr::select(weight) # 3.899198
0.0945/0.0242 # approx correct!

sleep17 %>% dplyr::filter(Age_group_merged == "20-24 years" & Gender == "Men") %>% 
  dplyr::select(weight) # 0.9956157
0.0317/0.0319 # approx correct!

sleep17 %>% dplyr::filter(Age_group_merged == "65 or older" & Gender == "Women") %>%
  dplyr::select(weight) # 10.40171
0.106/0.0102 # approx correct!



#  SURVEY WEIGHTS Age+Sex 2020-----------
# _Austrian population 2020-----
bev2020_age_sex <- read_excel("./DATA/Bev_Alter_Geschl_Bundesl_seit_2011.xlsx", 
                              sheet = "2020_age_sex")
#dim(bev2020_age_sex) # 202
bev2020_age_sex$Age_group <- wafflecut(
  bev2020_age_sex$Alter_Jahre, 
  c('[0,19]', '[20,24]', '[25,29]', '[30,34]', '[35,39]', '[40,44]', 
    '[45,49]', '[50,54]', '[55,59]', '[60,64]', '[65,69]', '[70,100]'), 
  c('<20 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', 
    '40-44 years', '45-49 years', '50-54 years', '55-59 years', 
    '60-64 years', '65-69 years', '70 or older')
)

bev2020_age_sex %>%
  group_by(Geschlecht) %>%
  dplyr::summarize(Anzahl = sum(Anzahl)) %>%
  mutate(Prozent = Anzahl / sum(Anzahl))

# _2020 sleep sample----
sleep20$Age_cat <- sleep20$Q1_cat

# _define weights age sex 2020--------

# Compute Austrian population proportions (2020)
pop_proportions_2020 <- bev2020_age_sex %>%
  dplyr::group_by(Age_group, Geschlecht) %>%
  dplyr::summarise(Population = sum(Anzahl), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Pop_Proportion = Population / sum(Population)) %>%
  dplyr::mutate(gender = ifelse(Geschlecht == "Mann", "Men", "Women")) %>%
  dplyr::select(-Geschlecht)

# Check results
head(pop_proportions_2020)
sum(pop_proportions_2020$Population)  # Should match total Austrian population in 2020
sum(pop_proportions_2020$Pop_Proportion)  # Should be 1

# Assign age categories (from questionnaire)
sleep20 <- sleep20 %>%
  dplyr::mutate(Age_cat = Q1_cat)

# Merge Age Groups in Sample
sleep20 <- sleep20 %>%
  dplyr::mutate(
    Age_group_merged = ifelse(Age_cat %in% c("70 or older", "65-69 years"), "65 or older", as.character(Age_cat)),
    Age_group_merged = factor(Age_group_merged) %>% fct_drop()
  )

# Check results
table(sleep20$Age_group_merged, sleep20$gender)

# Merge Age Groups in Population
pop_proportions_2020 <- pop_proportions_2020 %>%
  dplyr::mutate(Age_group_merged = ifelse(Age_group %in% c("70 or older", "65-69 years"), 
                                   "65 or older", as.character(Age_group)) %>% factor()) 

# Drop old `Age_group` column
pop_proportions_2020 <- pop_proportions_2020 %>% dplyr::select(-Age_group)

# Sum merged strata
pop_proportions_2020 <- pop_proportions_2020 %>%
  dplyr::group_by(Age_group_merged, gender) %>%
  dplyr::mutate(Population = sum(Population), Pop_Proportion = sum(Pop_Proportion)) %>%
  dplyr::distinct()

sample_proportions_2020 <- sleep20 %>%
  dplyr::group_by(Age_group_merged, gender) %>%
  dplyr::summarise(Sample_Size = n(), .groups = "drop") %>%
  dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))

# Check results
head(sample_proportions_2020)
table(sleep20$Age_group_merged, sleep20$gender)

colnames(sample_proportions_2020) <- c("Age_group_merged", "Gender", "Freq", "Proportion_in_stratum")
pop_proportions_2020 <- pop_proportions_2020 %>% 
  dplyr::select(Age_group_merged, gender, Population, Pop_Proportion)
colnames(pop_proportions_2020) <- colnames(sample_proportions_2020)

colnames(pop_proportions_2020)
colnames(sample_proportions_2020)
# same names.

sleep20$Gender <- sleep20$gender
sleep20$Age_group_merged
design_unweighted_2020 <- svydesign(ids = ~1, data = sleep20, weights = ~1)

post_strata_2020 <- pop_proportions_2020 %>%
  dplyr::select(Age_group_merged, Gender, Freq)

design_weighted_2020 <- postStratify(design_unweighted_2020, ~Age_group_merged + Gender, 
                                post_strata_2020, partial = TRUE)

saveRDS(design_weighted_2020, "./DATA/design_weighted_2020.RDS")

# Extract computed weights
sleep20$weight <- weights(design_weighted_2020)

# Check if total weight sum matches sample size
sum(sleep20$weight)  # Should be close to original sample size

# Compute total sample size
total_sample_size_2020 <- nrow(sleep20)

# Rescale weights so they sum to the total sample size
sleep20$weight <- weights(design_weighted_2020) * (total_sample_size_2020 / sum(weights(design_weighted_2020)))
hist(sleep20$weight, main = "Post-Stratified Weights Distribution", col = "blue")

saveRDS(sleep20$weight, "./DATA/weights_to_reflect_sample_size_2020.RDS")

# Verify if rescaling worked
sum(sleep20$weight)  # 847

# Inspect weight distribution
summary(sleep20$weight)
hist(sleep20$weight, main = "Post-Stratified Weights Distribution", col = "blue")

pop_proportions_2020
sample_proportions_2020

# check weights for <20 years:
sleep20 %>% dplyr::filter(Age_group_merged == "<20 years" & Gender == "Women") %>% 
  dplyr::select(weight) %>% slice(1) # 6.615419
0.0937/0.0142 # approx correct!

# Weights seem to be correct!

saveRDS(design_weighted, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/CURRENT_R_OUTPUTS/design_weighted.RDS")
saveRDS(design_weighted_2020, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/CURRENT_R_OUTPUTS/design_weighted_2020.RDS")

# FUNCTION to create SURVEY WEIGHTS for SUBSETS-------------
create_survey_design <- function(year = 2017, 
                                 given_subset,
                                 pop_proportions){
  
  if(year == 2017){
    sample_proportions <- given_subset %>%
      dplyr::group_by(Age_group_merged, gender) %>%
      dplyr::summarise(Sample_Size = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))
    
    #given_subset$Age_group_merged <- given_subset$Age_cat_merged
    colnames(sample_proportions) <- c("Age_group_merged", "Gender", "Freq", "Proportion_in_stratum")
    pop_proportions <- pop_proportions %>% 
      dplyr::select(Age_group_merged, Gender, Freq, 
      Proportion_in_stratum)
    colnames(pop_proportions) <- colnames(sample_proportions)
    
    #colnames(pop_proportions)
    #colnames(sample_proportions)
    
    given_subset$Gender <- given_subset$gender
    design_unweighted <- svydesign(ids = ~1, data = given_subset, weights = ~1)
    
    post_strata <- pop_proportions %>%
      dplyr::select(Age_group_merged, Gender, Freq)
    
    design_weighted <- postStratify(design_unweighted, ~Age_group_merged + Gender, 
                                    post_strata, partial = TRUE)
    return(design_weighted)
    
  } else if(year == 2020){
   
    sample_proportions_2020 <- given_subset %>%
      dplyr::group_by(Age_group_merged, gender) %>%
      dplyr::summarise(Sample_Size = n(), .groups = "drop") %>%
      dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))
    
    colnames(sample_proportions_2020) <- c("Age_group_merged", "Gender", "Freq", "Proportion_in_stratum")
    #print(colnames(pop_proportions_2020))
    pop_proportions_2020 <- pop_proportions_2020 %>% 
      dplyr::select(Age_group_merged, Gender, 
                    Freq, Proportion_in_stratum)
    colnames(pop_proportions_2020) <- colnames(sample_proportions_2020)
    
    given_subset$Gender <- given_subset$gender
    given_subset$Age_group_merged
    design_unweighted_2020 <- svydesign(ids = ~1, data = given_subset, weights = ~1)
    
    post_strata_2020 <- pop_proportions_2020 %>%
      dplyr::select(Age_group_merged, Gender, Freq)
    
    design_weighted_2020 <- postStratify(design_unweighted_2020, ~Age_group_merged + Gender, 
                                         post_strata_2020, partial = TRUE)
    
    return(design_weighted_2020)
  }
  
}
# test2017 <- create_survey_design(year = 2017,
#                                 given_subset = sleep17,
#                                 pop_proportions = pop_proportions)
# identical(weights(test2017),weights(design_weighted)) # TRUE
# 
# test2020 <- create_survey_design(year = 2020,
#                                  given_subset = sleep20,
#                                  pop_proportions = pop_proportions_2020)
# identical(weights(test2020),weights(design_weighted_2020)) # TRUE


# SAVE ANALYTICAL data sets----------
saveRDS(sleep17, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/DATA/sleep17_an.RDS")
saveRDS(sleep20, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/DATA/sleep20_an.RDS")


# SUBSETS for analyses:-----------------------

# _sleep17_sub-----------
sleep17_sub <- sleep17 %>% dplyr::select(sleepWorkDays_bw,
                                         #age,        # age in CATEGORIES (in numbers)!
                                         Age_group_merged,
                                         gender,     # gender
                                         marrital_status,     # (single_divorced_widowed/married_or_partnership)
                                         number_children,     # How many children (<16 years old) do you currently have?
                                         BMI,    # BMI
                                         work,   # work variable (full_time_exkl_NS(455)/full_time_inkl_NS(52)/not_currently_working(349)/
                                         # part_time_exkl_NS(104)/part_time_inkl_NS(5))                                         
                                         min_sleep_required, # What is the minimum number of hours you need to sleep to function 
                                         napping,       # napping
                                         chronotype,    # Chronotype
                                         #wakeTimeWorkDay_in_hours,
                                         diagn_sleep_disorder,    # Have you ever been doctor-diagnosed with a sleep disorder? (914 No/90 Yes)
                                         insomniaChron,   # (78 Yes/926 No)
                                         alcohol_intake,  # sum of beer, wine, shots
                                         caffeine_intake, # sum of caffeinated drinks (coffee/tea/coca-cola etc.)
                                         dinner_WorkDays, # When do you usually have dinner on working days? 
                                         # (<=18:00/18h to 19h/19h to 20h/20h to 21h/>=21:00/I usually do not have dinner)
                                         sport_yes_no,    # Q48
                                         smoking,         # smoking
                                         general_health,  # In your opinion: How is your health status in general?
)

# _sleep20_sub-----------
sleep20_sub <- sleep20 %>% dplyr::select(sleepWorkDays,
                                         #age,         # age-CATEGORIES (in numbers)!
                                         Age_group_merged, 
                                         gender,      # gender
                                         marrital_status,     # (single_divorced_widowed/married_or_partnership)
                                         number_children,     # How many children (<16 years old) do you currently have?
                                         BMI,    # BMI
                                         work,   # work variable (full_time_exkl_NS(XXX)/full_time_inkl_NS(XX)/not_currently_working(XXX)/
                                         # part_time_exkl_NS(XXX)/part_time_inkl_NS(X))                                         
                                         min_sleep_required, # What is the minimum number of hours you need to sleep to function 
                                         napping,       # napping
                                         chronotype,    # Chronotype
                                         diagn_sleep_disorder,  # Have you ever been doctor-diagnosed with a sleep disorder? (XXX No/XX Yes)
                                         insomniaChron,   # (XX Yes/XXX No)
                                         alcohol_intake,  # sum of beer, wine, shots
                                         caffeine_intake, # sum of caffeinated drings (coffee/tea/coca-cola etc.)
                                         dinner_WorkDays, # When do you usually have dinner on working days? 
                                         # (<=18:00/18h to 19h/19h to 20h/20h to 21h/>=21:00/I usually do not have dinner)
                                         sport_yes_no,    # Q48
                                         smoking,         # smoking
                                         general_health   # In your opinion: How is your health status in general?
)
# _sleep20_sub_inkl_COVID ----
sleep20_sub_inkl_COVID <- sleep20 %>% dplyr::select(sleepWorkDays,
                                         #age,     # age in CATEGORIES (in numbers)!
                                         Age_group_merged,
                                         gender,  # gender
                                         marrital_status,     # single_divorced_widowed/married_or_partnership
                                         number_children,     # How many children (<16 years old) do you currently have?
                                         BMI,    # BMI
                                         work,   # full_time_exkl_NS/full_time_inkl_NS/not_currently_working/
                                         # part_time_exkl_NS/part_time_inkl_NS                                    
                                         min_sleep_required, # What is the minimum number of hours you need to sleep to function 
                                         napping,       # napping
                                         chronotype,    # Chronotype
                                         diagn_sleep_disorder,    # Have you ever been doctor-diagnosed with a sleep disorder? No/Yes
                                         insomniaChron,   # Yes/No
                                         alcohol_intake,  # sum of beer, wine, shots
                                         caffeine_intake, # sum of caffeinated drings (coffee/tea/coca-cola etc.)
                                         dinner_WorkDays, # When do you usually have dinner on working days? 
                                         # (<=18:00/18h to 19h/19h to 20h/20h to 21h/>=21:00/I usually do not have dinner)
                                         sport_yes_no,    # Q48
                                         smoking,         # smoking
                                         general_health,  # In your opinion: How is your health status in general?
                                         # --- COVID ---  #
                                         # (lin dep) weight_change,   # gained_11_to_20_kg/gained_more_than_20_kg/gained_up_to_10_kg/
                                         # lost_11_to_20_kg/lost_more_than_20_kg/lost_up_to_10_kg/no_change
                                         #home_office,       #  No/Yes_all_the_time/Yes_partially
                                         #home_office_yes_no, #  No/Yes
                                         #"Q69",
                                         ho_new,
                                         sleep_quality_change, # improved/improved_a_lot/much_worse/no_change/worse
                                         alc_change,       # No/Yes 
                                         coff_change,      # No/Yes 
                                         diet_change,      # No/Yes
                                         # (factor error) diet_change_how, # diet_improved/diet_worsened
                                         #sport_change,    # declined/declined_strongly/increased/increased_strongly/no_change
                                         days_walking,     # 0-7 days
                                         # (factor error) smoking_change,  # less_than_before/more_than_before/no_change
                                         digital_device_use, # no_change/increased/decreased
                                         # (factor error) digital_device_use_change_percent, # <25%/>75%/25-50% 
                                         emotional_burden_measures, # not_at_all_challenging/somewhat_challenging/very_challenging 
                                         financial_burden_measures, # None/small_burden/strong_burden/almost_no_fin_resources
                                         attitude_towards_future    # no_change/somewhat_more_thoughtful/very_thoughtful/very_thoughtful_and_pessimistic 
)

# for RESULTS ------------
# ___weighted mean difference in sleep duration on workdays:----------
svymean(~sleepWorkDays_bw, design = design_weighted)
svymean(~sleepWorkDays, design = design_weighted_2020)
# t-test
plot_sleep_work <- rbind(
  sleep17[, .(sleep = sleepWorkDays_bw, Survey = "2017", weights = weights(design_weighted))],
  sleep20[, .(sleep = sleepWorkDays, Survey = "2020", weights = weights(design_weighted_2020))],ignore.attr=TRUE
)
design_combined <- svydesign(ids = ~1, data = plot_sleep_work, weights = ~weights)
hist(weights(design_combined))
svyttest(sleep ~ Survey, design_combined) # p-value = 1.117e-08

svymean(~sleepWorkDays, design = design_weighted_2020) - 
  svymean(~sleepWorkDays_bw, design = design_weighted)
# 0.68839 (SE 0.0896) (identical to above t-test)
# minutes:
0.68839 * 60 # 41.3 minutes

# ___weighted mean difference in sleep duration on free days:---------
svymean(~sleepFreeDays_bw, design = design_weighted) # 7.99
svymean(~sleepFreeDays, design = design_weighted_2020) # 8.59
# t-test
plot_sleep_free <- rbind(
  sleep17[, .(sleep = sleepFreeDays_bw, Survey = "2017", weights = weights(design_weighted))],
  sleep20[, .(sleep = sleepFreeDays, Survey = "2020", weights = weights(design_weighted_2020))]
)
design_combined <- svydesign(ids = ~1, data = plot_sleep_free, weights = ~weights)
svyttest(sleep ~ Survey, design_combined) # p-value = 0.05267
# 0.5990304 

svymean(~sleepFreeDays_bw, design = design_weighted) - # 7.99
svymean(~sleepFreeDays, design = design_weighted_2020) # 8.59
# -0.59903

# ___medians----------
svyquantile(~sleepFreeDays_bw, design = design_weighted, c(0.5)) # 8
svyquantile(~sleepFreeDays, design = design_weighted_2020, c(0.5)) # 8.5

# ___bootstrap median difference test:---------
n_boot <- 1000
median_differences <- numeric(n_boot)
for(i in 1:n_boot){
  
  choose_ind <- sample(1:nrow(sleep17), replace = TRUE)
  sleep17_sub_boot_weighted <- sleep17[choose_ind, ]
  design_2017_boot <- create_survey_design(year = 2017, 
                                           given_subset = sleep17_sub_boot_weighted,
                                           pop_proportions = pop_proportions)
  
  choose_ind <- sample(1:nrow(sleep20), replace = TRUE)
  sleep20_sub_boot_weighted <- sleep20[choose_ind, ]
  design_2020_boot <- create_survey_design(year = 2020, 
                                           given_subset = sleep20_sub_boot_weighted,
                                           pop_proportions = pop_proportions_2020)
  
  median_differences[i] <- svyquantile(~sleepFreeDays_bw, design = design_2017_boot, c(0.5))$sleepFreeDays_bw[1] -
    svyquantile(~sleepFreeDays, design = design_2020_boot, c(0.5))$sleepFreeDays[1]
}
hist(median_differences, main = "Bootstrapped Median Differences", xlab = "Median Difference (2017 - 2020)")
quantile(median_differences, c(0.025, 0.975)) # -0.5   0.0 
barplot(table(median_differences), main = "Sign of Median Differences", xlab = "Sign of Median Difference (2017 - 2020)")




# 2017 BE________________________ ----

# ___+ Subset-analysis YES/NO:----
# if(sub_working_ana == TRUE){
#   sleep17_sub <- sleep17_sub[work != "not_currently_working",]
#   dim(sleep17_sub) # 496
# }


# ___FULL MODEL-----------
# ____Full model; unweighted------------
colnames(sleep17_sub)
model <- lm(sleepWorkDays_bw ~ ., data = sleep17_sub)
summary(model) # Adjusted R-squared:  0.2572 / working: Multiple R-squared:  0.3431,	Adjusted R-squared:   0.29
check_model(model) # bump in PPC, one could model this bump better if needed. / working: not bad

hist(sleep17_sub$sleepWorkDays_bw, freq = FALSE, main = "Histogram with Normal Density", xlab = "Sleep Work Days (bw)")
x <- seq(min(sleep17_sub$sleepWorkDays_bw), max(sleep17_sub$sleepWorkDays_bw), length = 100)
y <- dnorm(x, mean = mean(sleep17_sub$sleepWorkDays_bw, na.rm = TRUE), sd = sd(sleep17_sub$sleepWorkDays_bw, na.rm = TRUE))
lines(x, y, col = "red", lwd = 2)
# -> not so bad visually

# potential interaction terms using RFA:
FSA_model <- lmFSA(sleepWorkDays_bw ~ ., data = sleep17_sub)
summary(FSA_model)
# min_sleep_required:insomniaChronChronic insomnia / same found for sub working
# makes not much sense, since people with insomnia do not require a different
# amound of sleep, the just cant sleep longer for instance.

# ____full model; Age, sex weighting 2017---------
# full model, survey weighted:
model_weighted_age_sex <- lm(sleepWorkDays_bw ~ Age_group_merged +
                                 gender + marrital_status +     
                                 number_children + BMI + work + min_sleep_required + napping +       
                                 chronotype + diagn_sleep_disorder + insomniaChron +  
                                 alcohol_intake + caffeine_intake + dinner_WorkDays + 
                                 sport_yes_no + smoking + general_health, 
                               data = sleep17_sub,
                               weights = weights(design_weighted) ) # see above "SURVEY WEIGHTS Age+Sex 2017"

summary(model_weighted_age_sex) # Multiple R-squared:  0.3388,	Adjusted R-squared:  0.3032
check_model(model_weighted_age_sex) # bad PPC and residuals could be better, VIF?
qqPlot(model_weighted_age_sex)

# Multiple R^2 (manual check):
weights <- weights(design_weighted)
y_hat <- predict(model_weighted_age_sex, type = "response")
y <- sleep17_sub$sleepWorkDays_bw
y_mean <- sum(weights * y) / sum(weights)
rss <- sum(weights * (y - y_hat) ^ 2)
tss <- sum(weights * (y - y_mean) ^ 2)
# Pseudo R²
pseudo_R2 <- 1 - (rss / tss)
pseudo_R2 # 0.3388081 (=Multiple R^2 from lm + weights)
# -> check!



# ____compare coefficients full unweighted vs sex+age weighted linear regression:-----
coef(model)
coef(model_weighted_age_sex)
plot(coef(model_weighted_age_sex), coef(model), xlab = "Weighted", ylab = "Unweighted")
abline(0, 1)
# without intercept (increases correlation):
plot(coef(model_weighted_age_sex)[-1], coef(model)[-1], xlab = "Weighted", ylab = "Unweighted")
abline(0, 1)
df_coefs <- data.frame(weighted_coefs = coef(model_weighted_age_sex), 
                       unweighted_coefs = coef(model))
df_coefs %>% dplyr::filter(sign(weighted_coefs) !=  sign(unweighted_coefs)) 
# -> diagnosed sleep disorder and smoking seems relevant.
# -> weighting changes sign in full model
# on the other hand:
cor(df_coefs$weighted_coefs, df_coefs$unweighted_coefs)  # 0.9904571
cor(df_coefs$weighted_coefs[-1], df_coefs$unweighted_coefs[-1]) # 0.9194255 without intercept



# ___BE (backward elimination) Models:----------

# ____BE: unweighted (AIC)-----
k <- ols_step_backward_aic(model, direction = "backward") # no weights
as.formula(k$model$terms)
# marrital_status + work + min_sleep_required + 
# napping + chronotype + insomniaChron + caffeine_intake + 
#   dinner_WorkDays

# estimate coefficients for this BE model:
BE_model_2017 <- lm(as.formula(k$model$terms), x = TRUE, y = TRUE, 
                    data = sleep17_sub)
summary(BE_model_2017) # Adjusted R-squared:  0.2583/sub work: 0.2692
saveRDS(BE_model_2017, "./CURRENT_R_OUTPUTS/BE_model_2017_unweighted_19.3.25.RDS")
check_model(BE_model_2017) # bump at 7 and 8h, not so bad
qqPlot(BE_model_2017) # rather ok


# ____BE: age- and sex weighted (AIC) (-> REPORT in paper):----------
k_w_sex_age <- stepAIC(model_weighted_age_sex, 
                       direction = "backward", trace = TRUE)
# extract terms:
labels(terms(as.formula(k_w_sex_age)))
# "marrital_status" "BMI" "work" "min_sleep_required" "napping" "chronotype" "diagn_sleep_disorder"
# "insomniaChron" "dinner_WorkDays" "smoking"    

# estimate coefficients for this model:
BE_model_2017_weighted <- lm(terms(as.formula(k_w_sex_age)), 
                             data = sleep17_sub,
                             weights = weights(design_weighted))
summary(BE_model_2017_weighted) # Adjusted R-squared:  0.3023
length(coef(BE_model_2017_weighted)) # 20
gtsummary::tbl_regression(BE_model_2017_weighted)
saveRDS(BE_model_2017_weighted, "./CURRENT_R_OUTPUTS/BE_model_2017_weighted_19.3.25.RDS")
check_model(BE_model_2017_weighted) # bump at 7 and 8h, not so bad

# ____DROP in Adj.R^2 (->REPORT)-----------------
# from weighted full model to BE model: 
# Adjusted R-squared:  0.3032
# Adjusted R-squared:  0.3023
0.3032 - 0.3023 # 0.0009
# Difference in number of predictors full model vs BE model?
length(coef(model_weighted_age_sex)) # 41
length(coef(BE_model_2017_weighted)) # 20
41 - 20 # 21


# # ___+ possible interaction terms (unweighted)
# lm_interactions <- FSA(formula = as.formula(k$model$terms), data = sleep17_sub, 
#                        cores = 1, m = 2,
#                        interactions = TRUE, criterion = AIC, minmax = "min",  
#                        numrs = 10)
# summary(lm_interactions) # add interaction term "min_sleep_required*dinner_WorkDays"
# BE_model_2017_interac <- lm(sleepWorkDays_bw ~ Age_group_merged +
#                               gender + marrital_status +     
#                               number_children + BMI + work + min_sleep_required*insomniaChron + napping +       
#                               chronotype + diagn_sleep_disorder +  
#                               alcohol_intake + caffeine_intake + dinner_WorkDays + 
#                               sport_yes_no + smoking + general_health, 
#                             data = sleep17_sub) # automate later.
# 
# # BE_model_2017_working_interac <- lm(sleepWorkDays_bw ~ age + marrital_status + work + 
# #                                       chronotype + diagn_sleep_disorder + insomniaChron + caffeine_intake + 
# #                                       dinner_WorkDays + min_sleep_required*dinner_WorkDays, data = sleep17_sub) # automate later.
# summary(BE_model_2017_interac) # Adjusted R-squared:  0.2568 -> almost identical
# AIC(BE_model_2017) # 2281.885
# AIC(BE_model_2017_interac) # 2307.489 -> WORSE!

#summary(BE_model_2017_working_interac) # Adjusted R-squared:  0.2757 

# Does BE_model_2017_working_interac explain more than BE_model_2017?
#anova(BE_model_2017, BE_model_2017_working_interac) # not much

# ____other data-set (2020) performance (unweighted):------
variables_used <- labels(terms(as.formula(BE_model_2017$terms)))
new_variables <- paste(variables_used, collapse = " + ")
model_formula_2017 <- as.formula(paste("sleepWorkDays", "~", new_variables))
model_2017_in_2020 <- lm(model_formula_2017, data = sleep20_sub)
summary(lm(model_formula_2017, data = sleep20_sub)) # Adjusted R-squared:  0.1644 
#saveRDS(model_2017_in_2020, "model_2017_in_2020_16.3.25.RDS")

# ____other data-set (2020) performance (weighted):------
#....


# ___+ BOOTSTRAP ------

set.seed(100325)
n <- round(dim(sleep17_sub)[1])
n_boot <- n_bootstraps_par_global

# _____vectors for unweighted bootstrapping----------
predictors_boot <- c()
variables_used_list <- list()
boot_strap_models_list <- list()
adj.r.squared <- c()
count_VIF <- 0

# _____vectors for weighted (sex, age) bootstrapping----
# w_as = weighted for age&sex of the 2017 Austrian Population
predictors_boot_w_as <- c()
variables_used_list_w_as <- list()
boot_strap_models_list_w_as <- list()
adj.r.squared_w_as <- c()
count_VIF_w_as <- 0

tic()
for(i in 1:n_boot){
  choose_ind <- sample(1:n, replace = TRUE)
  
  # unweighted:
  sleep17_sub_boot_repl <- sleep17_sub[choose_ind, ]
  model_boot <- lm(sleepWorkDays_bw ~ Age_group_merged + gender + marrital_status +     
                     number_children + BMI + work + min_sleep_required + napping +       
                     chronotype + diagn_sleep_disorder + insomniaChron +  
                     alcohol_intake + caffeine_intake + dinner_WorkDays + 
                     sport_yes_no + smoking + general_health, 
                   data = sleep17_sub_boot_repl)
  count_VIF <- count_VIF + sum(VIF(model_boot)[,3] > 3)   # multicolinearity?
  k <- ols_step_backward_aic(model_boot) # AIC on weighted model
  boot_strap_models_list[[i]] <- k
  #p_thres_model_2017 <- lm(as.formula(k$model$terms), data=sleep17_sub)
  variables_used <- labels(terms(as.formula(k$model$terms)))
  variables_used_list[[i]] <- sort(variables_used)
  predictors_boot[i] <- paste(sort(variables_used), collapse = " ")
  adj.r.squared[i] <- summary(model_boot)$adj.r.squared
  
  # weighted:
  sleep17_sub_boot_weighted <- sleep17_sub_boot_repl
  design_2017_boot <- create_survey_design(year = 2017, 
                                           given_subset = sleep17_sub_boot_weighted,
                                           pop_proportions = pop_proportions)
  model_boot_w <- lm(sleepWorkDays_bw ~ Age_group_merged + gender + marrital_status +     
                           number_children + BMI + work + min_sleep_required + napping +       
                           chronotype + diagn_sleep_disorder + insomniaChron +  
                           alcohol_intake + caffeine_intake + dinner_WorkDays + 
                           sport_yes_no + smoking + general_health, 
                         data = sleep17_sub_boot_weighted, 
                         weights = weights(design_2017_boot))
  count_VIF_w_as <- count_VIF_w_as + sum(VIF(model_boot_w)[,3] > 3)   # multicol
  k_w_as <- stepAIC(model_boot_w, direction = "backward", trace = FALSE) 
  boot_strap_models_list_w_as[[i]] <- k_w_as
  variables_used_w_as <- labels(terms(as.formula(k_w_as$terms)))
  variables_used_list_w_as[[i]] <- sort(variables_used_w_as)
  predictors_boot_w_as[i] <- paste(sort(variables_used_w_as), collapse = " ")
  adj.r.squared_w_as[i] <-  summary(model_boot_w)$adj.r.squared
  
  if(i %% 10 == 0){ # counter
    # percent progress
    cat(i, "of", n_boot, "iterations completed\n")
  }
  
}
toc() # 1600.143 sec elapsed; 19.3.25

# # _____a) bootstrap results for unweighted-----------
# 
# pred_boot_df <- data.frame(Predictors = predictors_boot)
# variables_used_list_sorted <- lapply(variables_used_list, sort)
# hist(adj.r.squared)
# quantile(adj.r.squared, p = c(0.025, 0.975)) # 0.2430321 0.3595028 
# getwd()
# saveRDS(adj.r.squared, "./CURRENT_R_OUTPUTS/adj.r.squared_boot_2017_19.3.25.RDS")
# 
# # ______Model selection frequencies (analog Table 6 Heinze): ----
# pred_boot_df <- pred_boot_df %>% 
#   dplyr::count(sort(Predictors)) %>% 
#   dplyr::arrange(desc(n)) %>%
#   top_n(20) %>% 
#   dplyr::mutate(Percent = n/n_boot*100) %>%
#   dplyr::mutate(Cumulative_Percent = cumsum(Percent))
# pred_boot_df
# 
# # ______Model sizes overview (number of predictors): ----
# plot(table(lengths(variables_used_list_sorted)), xlab = "number of predictors") # 6-15 predictors
# 
# # ______Inclusion frequencies of individual variables: ----
# all_vars <- data.frame(vars = unlist(variables_used_list_sorted))
# incl_freq_2017 <- all_vars %>% 
#   dplyr::count(vars) %>% 
#   dplyr::arrange(desc(n)) %>%
#   dplyr::mutate(Percent_of_models = n/n_boot*100)
# flextable(incl_freq_2017)
# # with beautiful labels:
# colnames(incl_freq_2017) <- c("Variables", "n", "% of models")
# incl_freq_2017$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("work", "Work", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("general_health", "General health", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("marrital_status", "Marital status", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("chronotype", "Chronotype", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("age", "Age", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("smoking", "Smoking", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("napping", "Napping", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("number_children", "Number of children", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2017$Variables)
# incl_freq_2017$Variables <- gsub("gender", "Sex", incl_freq_2017$Variables)
# flextable(incl_freq_2017) %>%
#   set_caption("2017 (Unweighted)")
# saveRDS(incl_freq_2017, "./CURRENT_R_OUTPUTS/z_BOOTSTRAP_results/incl_freq_2017_unweighted_19.3.25.RDS")
# #saveRDS(incl_freq_2017, "./DATA/incl_freq_2017_sub_working.RDS")
# #incl_freq_2017 <- readRDS("./DATA/incl_freq_2017.RDS")
# 
# #______Stability of coefficients: ----
# coefs_bootstrap <- data.frame()
# for(i in 1:n_boot){
#   model <-  boot_strap_models_list[[i]]
#   coef_names <- names(model$model$coefficients)
#   coefs <- model$model$coefficients
#   coefs_bootstrap <- rbind(coefs_bootstrap, data.frame(coef_names, coefs))
# }
# rownames(coefs_bootstrap) <- NULL
# colnames(coefs_bootstrap) <- c("coef_names", "coef_values")
# unique_coefs <- unique(coefs_bootstrap$coef_names) # how many different coefs where used in all bootstrap samples?
# 
# #for(i in 1:32){
# #  noquote(print(paste0("hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[",i,"])], main = unique_coefs[",i,"])")))
# #}
# 
# length(unique_coefs)
# 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[1])], main = unique_coefs[1])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[2])], main = unique_coefs[2])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[3])], main = unique_coefs[3])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[4])], main = unique_coefs[4]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[5])], main = unique_coefs[5])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[6])], main = unique_coefs[6])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[7])], main = unique_coefs[7])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[8])], main = unique_coefs[8])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[9])], main = unique_coefs[9])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[10])], main = unique_coefs[10])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[11])], main = unique_coefs[11])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[12])], main = unique_coefs[12])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[13])], main = unique_coefs[13])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[14])], main = unique_coefs[14])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[15])], main = unique_coefs[15])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[16])], main = unique_coefs[16])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[17])], main = unique_coefs[17])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[18])], main = unique_coefs[18]) # 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[19])], main = unique_coefs[19]) # 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[20])], main = unique_coefs[20])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[21])], main = unique_coefs[21])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[22])], main = unique_coefs[22])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[23])], main = unique_coefs[23])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[24])], main = unique_coefs[24])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[25])], main = unique_coefs[25]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[26])], main = unique_coefs[26]) # 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[27])], main = unique_coefs[27]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[28])], main = unique_coefs[28])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[29])], main = unique_coefs[29]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[30])], main = unique_coefs[30]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[31])], main = unique_coefs[31]) # sep
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[32])], main = unique_coefs[32]) # sep

# _____b) bootstrap results for weighted-------------
pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)
hist(adj.r.squared_w_as)
quantile(adj.r.squared_w_as, p = c(0.025, 0.975)) #0.2930407 0.4691073 

# ______Model selection frequencies (analog Table 6 Heinze): ----
pred_boot_df_w_as <- pred_boot_df_w_as %>% 
  dplyr::count(sort(Predictors)) %>% 
  dplyr::arrange(desc(n)) %>%
  top_n(20) %>% 
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as
saveRDS(pred_boot_df_w_as, "./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2017_w_as_19.3.25.RDS")
getwd()
#pred_boot_df_w_as <- readRDS("./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2017_w_as_19.3.25.RDS")
#pred_boot_df_w_as

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors") # 7-16 predictors

#______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$model$coefficients)
  coefs <- model$model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}

# ______Inclusion frequencies of individual variables: ----
all_vars_w_as <- data.frame(vars = unlist(variables_used_list_sorted_w_as))
incl_freq_2017_w_as <- all_vars_w_as %>% 
  dplyr::count(vars) %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(Percent_of_models = n/n_boot*100)
flextable(incl_freq_2017_w_as)
# with beautiful labels:
colnames(incl_freq_2017_w_as) <- c("Variables", "n", "% of models")
incl_freq_2017_w_as$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("work", "Work", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("general_health", "General health", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("marrital_status", "Marital status", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("chronotype", "Chronotype", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("age", "Age", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("Age_group_merged", "Age group", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("smoking", "Smoking", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("napping", "Napping", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("number_children", "Number of children", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2017_w_as$Variables)
incl_freq_2017_w_as$Variables <- gsub("gender", "Gender", incl_freq_2017_w_as$Variables)

flextable(incl_freq_2017_w_as) %>%
  set_caption("2017 Inclusion Frequencies (age and sex) weighted")

# ____SAVE all weighted vectors to ./RESULTS/z_BOOTSTRAP_results/2017_weighted/:----------
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2017_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2017_w_as_20.3.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2017_w_as_20.3.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2017_w_as_20.3.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_boot_2017_weighted_20.3.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2017_weighted_20.3.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
saveRDS(incl_freq_2017_w_as, paste0(base_dir, "incl_freq_2017_weighted_20.3.25.RDS"))

save_as_html(flextable(incl_freq_2017_w_as) %>%
               set_caption("2017 inclusion frequencies (age and sex) weighted"), 
             path = "./RESULTS/Table_3_inclusion_frequencies_2017_weighted_20.3.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2017_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2017_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2017_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2017_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_boot_2017_weighted_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2017_weighted_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
# incl_freq_2017_w_as <- readRDS(paste0(base_dir, "incl_freq_2017_weighted_20.3.25.RDS"))


# __NETWORK graph with included variables-----------
coefs_bootstrap_w_as
# Example input: 'variables_used_list_sorted_w_as' is a list of 1000 bootstrap model selections.
bootstrap_list <- variables_used_list_sorted_w_as  # Replace with your actual list variable

# Create an empty adjacency matrix
all_vars <- unique(unlist(bootstrap_list))  # Get all unique variables
co_occur_matrix <- matrix(0, nrow = length(all_vars), ncol = length(all_vars), dimnames = list(all_vars, all_vars))

# Count co-occurrences
for (model in bootstrap_list) {
  co_occur_matrix[model, model] <- co_occur_matrix[model, model] + 1
}

# Convert matrix to an edge list
co_occur_df <- as.data.frame(as.table(co_occur_matrix))
co_occur_df <- co_occur_df[co_occur_df$Freq > 0 & co_occur_df$Var1 != co_occur_df$Var2, ]  # Remove self-links

# Create an igraph object
graph <- graph_from_data_frame(co_occur_df, directed = FALSE)

# Set edge weights based on frequency
E(graph)$weight <- co_occur_df$Freq

# Network visualization with ggraph
set.seed(123)
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), color = "darkblue") +
  geom_node_point(size = 5, color = "green") +
  geom_node_text(aes(label = name), 
                 size = 5,          # Larger text
                 fontface = "bold", # Bold labels
                 color = "red",     # Red text
                 repel = TRUE) +    # Prevent overlapping labels
  theme_void() +
  ggtitle("2017: Co-Occurrence Network of Bootstrap Selected Variables") +
  scale_edge_width_continuous(range = c(0.5, 5)) +  # Adjust edge thickness
  scale_edge_alpha_continuous(range = c(0.1, 0.4)) + # Adjust transparency
  theme(plot.title = element_text(hjust = 0.5))
saveRDS(graph, "./CURRENT_R_OUTPUTS/2017_bootstrap_network_20.3.25.RDS")





# 2020 BE (without COVID)___________ ----

# sleep20_sub %>% as_tibble() %>% dplyr::summarise( across(where(is.numeric), ~ var(.x, na.rm = TRUE)) )

# ___+ subset-analysis YES/NO:----
#sleep20_sub <- sleep20_sub[work != "not_currently_working",]
# dim(sleep20_sub) # 676 obs.


# Missings:
#describe(sleep20_sub)
#create_report(sleep20_sub)
#sum(is.na(sleep20$sleepWorkDays))

# ___FULL MODEL 2020---------

# ____full model; unweighted-----------
# setdiff(colnames(sleep17_sub), colnames(sleep20_sub))
model_full_2020 <- lm(sleepWorkDays ~ ., data = sleep20_sub) 
summary(model_full_2020) # Adjusted R-squared:  0.1803 
check_model(model_full_2020) # not so bad, residuals and PPC
qqPlot(model_full_2020) # not so normal

# ____full model; weighted-----------
model_weighted_age_sex_2020 <- lm(sleepWorkDays ~ Age_group_merged +
                                   gender + marrital_status +     
                                   number_children + BMI + work + min_sleep_required + napping +       
                                   chronotype + diagn_sleep_disorder + insomniaChron +  
                                   alcohol_intake + caffeine_intake + dinner_WorkDays + 
                                   sport_yes_no + smoking + general_health, 
                                  data = sleep20_sub,
                                 weights = weights(design_weighted_2020)) # see above "SURVEY WEIGHTS Age+Sex 2017"
summary(model_weighted_age_sex_2020) # Multiple R-squared:  0.2844,	Adjusted R-squared:  0.2489 
check_model(model_weighted_age_sex_2020) # bad PPC and residuals, VIF -> question is if check_model correctly works with svyglm...
qqPlot(model_weighted_age_sex_2020)

# Multiple R^2:
weights <- weights(design_weighted_2020)
y_hat <- predict(model_weighted_age_sex_2020, type = "response")
y <- sleep20_sub$sleepWorkDays
y_mean <- sum(weights * y) / sum(weights)
rss <- sum(weights * (y - y_hat) ^ 2)
tss <- sum(weights * (y - y_mean) ^ 2)
# Pseudo R²
pseudo_R2 <- 1 - (rss / tss)
pseudo_R2 # 0.2844141
# if weights are comparatively small at large squared residuals,
# R^2 is larger compared to the normal R^2

# potential interaction terms using RFA:
FSA_model <- lmFSA(sleepWorkDays ~ ., data = sleep20_sub)
summary(FSA_model)
# min_sleep_required:caffeine_intake  0.004369   0.002771   1.577   0.1152 #ns
# and sign makes not much sense.

# ___BE Models:-----------

# ____BE: unweighed (AIC)----------
k <- ols_step_backward_aic(model_full_2020, direction = "backward")
BE_model_2020 <- lm(as.formula(k$model$terms), x=TRUE, y=TRUE, data=sleep20_sub)
summary(BE_model_2020) # Adjusted R-squared:  0.1705 (vs. 0.1803 unweighted)
#saveRDS(BE_model_2020, "./DATA/BE_model_2020_working.RDS")


# ____BE: weighted age and sex (AIC)---------
k_w_as_20 <- stepAIC(model_weighted_age_sex_2020, 
                     direction = "backward", trace = FALSE)
as.formula(terms(k_w_as_20$model))
# estimate coefficients of this model:
BE_model_2020_w_as <- lm(as.formula(terms(k_w_as_20$model)), 
                             data = sleep20_sub, 
                             weights = weights(design_weighted_2020))
summary(BE_model_2020_w_as) # Multiple R-squared:  0.262,	Adjusted R-squared:  0.2468 
length(coef(BE_model_2020_w_as)) # 18
check_model(BE_model_2020_w_as) # bad PPC and residuals
qqPlot(BE_model_2020_w_as) # not so normal
gtsummary::tbl_regression(BE_model_2020_w_as) 
saveRDS(BE_model_2020_w_as, "./CURRENT_R_OUTPUTS/BE_model_2020_weighted_19.3.25.RDS")

# DROP in Adj.R^2 (->REPORT)-----------------
# from weighted full model to BE model:
# Adjusted R-squared:  0.2489 
# Adjusted R-squared:  0.2468 
0.2489 - 0.2468 # 0.0021

# Calculate Multiple R^2 manually:
weights <- weights(design_weighted_2020)
y_hat <- predict(BE_model_2020_w_as, type = "response")
y <- sleep20_sub$sleepWorkDays
y_mean <- sum(weights * y) / sum(weights)
rss <- sum(weights * (y - y_hat) ^ 2)
tss <- sum(weights * (y - y_mean) ^ 2)
# Pseudo R²
pseudo_R2 <- 1 - (rss / tss)
pseudo_R2 # 0.2619828


# # ___+ possible interaction terms (2020); unweighted 
# lm_interactions <- FSA(formula = as.formula(k$model$terms), data = sleep20_sub, cores = 1, m = 2,
#                        interactions = TRUE, criterion = AIC, minmax = "min",
#                        numrs = 10)
# summary(lm_interactions) # possibly interaction term "min_sleep_required*dinner_WorkDays"
# BE_model_2020_no_COVID_interac <- lm(sleepWorkDays ~ Age_group_merged+gender+marrital_status+
#                                        number_children+BMI+work+min_sleep_required*caffeine_intake+napping+
#                                        chronotype+diagn_sleep_disorder+insomniaChron+alcohol_intake+
#                                        dinner_WorkDays+sport_yes_no+smoking+general_health, 
#                                      data=sleep20_sub) # automate later....
# # BE_model_2020_work_no_COVID_interac <- lm(sleepWorkDays ~ Age_group_merged+gender+marrital_status+
# #                                             number_children+BMI+work+min_sleep_required + caffeine_intake+napping+
# #                                             chronotype+diagn_sleep_disorder+insomniaChron+alcohol_intake+
# #                                             dinner_WorkDays+sport_yes_no+smoking+general_health, 
# #                                           data = sleep20_sub) # automate later....
# 
# summary(BE_model_2020_no_COVID_interac) # a bit better Adjusted R-squared:  0.1828 
# summary(BE_model_2020)
# #summary(BE_model_2020_work_no_COVID_interac) # 
# # -> omit interaction terms for now!

# ___compare coefficients full unweighted vs sex+age weighted:-----
coef(model_full_2020)
coef(model_weighted_age_sex_2020)

plot(coef(model_weighted_age_sex_2020), coef(model_full_2020), xlab = "Weighted", ylab = "Unweighted")
abline(0, 1)

# Without intercept
plot(coef(model_weighted_age_sex_2020)[-1], coef(model_full_2020)[-1], xlab = "Weighted", ylab = "Unweighted")
abline(0, 1)

# Create data frame with coefficients
df_coefs_2020 <- data.frame(weighted_coefs = coef(model_weighted_age_sex_2020), 
                            unweighted_coefs = coef(model_full_2020))

# Find coefficients where weighting changes sign
df_coefs_2020 %>% dplyr::filter(sign(weighted_coefs) != sign(unweighted_coefs)) 

# Check correlation between weighted and unweighted coefficients
cor(df_coefs_2020$weighted_coefs, df_coefs_2020$unweighted_coefs)  # 0.943046 with Intercept
cor(df_coefs_2020$weighted_coefs[-1], df_coefs_2020$unweighted_coefs[-1])  # 0.5380013!!!
# "outlier": workpart_time_inkl_NS             1.40497414      -0.08924586


# ___+ BOOTSTRAP ------

set.seed(10325) 
n <- round(dim(sleep20_sub)[1])
n_boot <- n_bootstraps_par_global

# _____vectors for unweighted bootstrapping----------
predictors_boot <- c()
variables_used_list <- list()
boot_strap_models_list <- list()
adj.r.squared <- c()
count_VIF <- 0

# _____vectors for weighted bootstrapping----
predictors_boot_w_as <- c()
variables_used_list_w_as <- list()
boot_strap_models_list_w_as <- list()
adj.r.squared_w_as <- c()
count_VIF_w_as <- 0


tic()
for(i in 1:n_boot){
  choose_ind <- sample(1:n, replace = TRUE)
  
  # unweighted:
  sleep20_sub_boot_repl <- sleep20_sub[choose_ind, ]
  model_boot <- lm(sleepWorkDays ~ Age_group_merged + gender + marrital_status +     
                     number_children + BMI + work + min_sleep_required + napping +       
                     chronotype + diagn_sleep_disorder + insomniaChron +  
                     alcohol_intake + caffeine_intake + dinner_WorkDays + 
                     sport_yes_no + smoking + general_health, 
                   data = sleep20_sub_boot_repl)
  count_VIF <- count_VIF + sum(VIF(model_boot)[,3] > 3)   # multicolinearity?
  k <- ols_step_backward_aic(model_boot) # AIC
  boot_strap_models_list[[i]] <- k
  variables_used <- labels(terms(as.formula(k$model$terms)))
  variables_used_list[[i]] <- sort(variables_used)
  predictors_boot[i] <- paste(sort(variables_used), collapse = " ")
  adj.r.squared[i] <- summary(model_boot)$adj.r.squared
  
  # weighted:
  sleep20_sub_boot_weighted <- sleep20_sub_boot_repl # same
  design_2020_boot <- create_survey_design(year = 2020, 
                                           given_subset = sleep20_sub_boot_weighted,
                                           pop_proportions = pop_proportions_2020)
  model_boot_w <- lm(sleepWorkDays ~ Age_group_merged + gender + marrital_status +     
                           number_children + BMI + work + min_sleep_required + napping +       
                           chronotype + diagn_sleep_disorder + insomniaChron +  
                           alcohol_intake + caffeine_intake + dinner_WorkDays + 
                           sport_yes_no + smoking + general_health, 
                         data = sleep20_sub_boot_weighted, 
                         weights = weights(design_2020_boot))
  count_VIF_w_as <- count_VIF_w_as + sum(VIF(model_boot_w)[,3] > 3)   # multicolinearity?
  k_w_as <- stepAIC(model_boot_w, direction = "backward", trace = FALSE) # now stepAIC
  boot_strap_models_list_w_as[[i]] <- k_w_as
  variables_used_w_as <- labels(as.formula(k_w_as$terms))
  variables_used_list_w_as[[i]] <- sort(variables_used_w_as)
  predictors_boot_w_as[i] <- paste(sort(variables_used_w_as), collapse = " ")
  adj.r.squared_w_as[i] <- summary(model_boot_w)$adj.r.squared
  
  if(i %% 10 == 0){ # counter
    # percent counter
    cat(i, "of", n_boot, "iterations completed\n")
  }
}
toc() # 1400 s/20min

# # _____a) bootstrap results for unweighted-----------
# pred_boot_df <- data.frame(Predictors = predictors_boot)
# variables_used_list_sorted <- lapply(variables_used_list, sort)
# hist(adj.r.squared)
# quantile(adj.r.squared, p = c(0.025,0.975)) # 0.1688463 0.2875807  /sub working: 0.1230593 0.2792151
# saveRDS(adj.r.squared, "./CURRENT_R_OUTPUTS/z_BOOTSTRAP_results/adj.r.squared_boot_2020_unweighted_19.3.25.RDS")
# #saveRDS(adj.r.squared, "./DATA/adj.r.squared_boot_2020_sub_working.RDS")
# 
# # ______Model selection frequencies (analog Table 6 Heinze): ----
# pred_boot_df <- pred_boot_df %>% 
#   dplyr::count(sort(Predictors)) %>% 
#   dplyr::arrange(desc(n)) %>%
#   top_n(20) %>% 
#   dplyr::mutate(Percent = n/n_boot*100) %>%
#   dplyr::mutate(Cumulative_Percent = cumsum(Percent))
# pred_boot_df
# 
# # ______Model sizes overview (number of predictors): ----
# plot(table(lengths(variables_used_list_sorted)), xlab = "number of predictors") # 2-13 predictors
# 
# # ______Inclusion frequencies of individual variables: ----
# all_vars <- data.frame(vars = unlist(variables_used_list_sorted))
# incl_freq_2020 <- all_vars %>% 
#   dplyr::count(vars) %>% 
#   dplyr::arrange(desc(n)) %>%
#   dplyr::mutate(Percent_of_models = n/n_boot*100)
# flextable(incl_freq_2020)
# # with beautiful labels:
# colnames(incl_freq_2020) <- c("Variables", "n", "% of models")
# incl_freq_2020$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("work", "Work", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("gender", "Sex", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("age", "Age", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("number_children", "Number of children", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("chronotype", "Chronotype", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("smoking", "Smoking", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("general_health", "General health", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("napping", "Napping", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("marrital_status", "Marital status", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2020$Variables)
# flextable(incl_freq_2020)
# saveRDS(incl_freq_2020, "./CURRENT_R_OUTPUTS/z_BOOTSTRAP_results/incl_freq_2020_unweighted_19.3.25.RDS")
# #saveRDS(incl_freq_2020, "./DATA/incl_freq_2020_sub_working.RDS")
# 
# #______Stability of coefficients: ----
# coefs_bootstrap <- data.frame()
# for(i in 1:n_boot){
#   model <-  boot_strap_models_list[[i]]
#   coef_names <- names(model$model$coefficients)
#   coefs <- model$model$coefficients
#   coefs_bootstrap <- rbind(coefs_bootstrap, data.frame(coef_names, coefs))
# }
# rownames(coefs_bootstrap) <- NULL
# colnames(coefs_bootstrap) <- c("coef_names", "coef_values")
# unique_coefs <- unique(coefs_bootstrap$coef_names) # how many different coefs where used in all bootstrap samples?
# 
# #for(i in 1:32){
# #  noquote(print(paste0("hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[",i,"])], main = unique_coefs[",i,"])")))
# #}
# 
# length(unique_coefs)
# 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[1])], main = unique_coefs[1])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[2])], main = unique_coefs[2])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[3])], main = unique_coefs[3])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[4])], main = unique_coefs[4])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[5])], main = unique_coefs[5])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[6])], main = unique_coefs[6])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[7])], main = unique_coefs[7])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[8])], main = unique_coefs[8])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[9])], main = unique_coefs[9])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[10])], main = unique_coefs[10])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[11])], main = unique_coefs[11])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[12])], main = unique_coefs[12])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[13])], main = unique_coefs[13])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[14])], main = unique_coefs[14])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[15])], main = unique_coefs[15])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[16])], main = unique_coefs[16])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[17])], main = unique_coefs[17])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[18])], main = unique_coefs[18])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[19])], main = unique_coefs[19])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[20])], main = unique_coefs[20])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[21])], main = unique_coefs[21])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[22])], main = unique_coefs[22])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[23])], main = unique_coefs[23])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[24])], main = unique_coefs[24])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[25])], main = unique_coefs[25])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[26])], main = unique_coefs[26])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[27])], main = unique_coefs[27])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[28])], main = unique_coefs[28])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[29])], main = unique_coefs[29])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[30])], main = unique_coefs[30])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[31])], main = unique_coefs[31])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[32])], main = unique_coefs[32])


# _____b) bootstrap results for weighted-------------
pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)
hist(adj.r.squared_w_as)
quantile(adj.r.squared_w_as, p = c(0.025, 0.975)) # 0.2175470 0.4328259

# ______Model selection frequencies (analog Table 6 Heinze): ----
pred_boot_df_w_as <- pred_boot_df_w_as %>% 
  dplyr::count(sort(Predictors)) %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(20) %>% 
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as
saveRDS(pred_boot_df_w_as, "./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2020_w_as_19.3.25.RDS")

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors")
# 5-16 predictors 

#______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$model$coefficients)
  coefs <- model$model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}


# ______Inclusion frequencies of individual variables: ----
all_vars_w_as <- data.frame(vars = unlist(variables_used_list_sorted_w_as))
incl_freq_2020_w_as <- all_vars_w_as %>% 
  dplyr::count(vars) %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(Percent_of_models = n/n_boot*100)
flextable(incl_freq_2020_w_as)

# with beautiful labels:
colnames(incl_freq_2020_w_as) <- c("Variables", "n", "% of models")
incl_freq_2020_w_as$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("work", "Work", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("general_health", "General health", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("marrital_status", "Marital status", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("chronotype", "Chronotype", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("age", "Age", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("gender", "Gender", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("Age_group_merged", "Age group", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("smoking", "Smoking", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("napping", "Napping", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("number_children", "Number of children", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2020_w_as$Variables)

flextable(incl_freq_2020_w_as) %>%
  set_caption("2020 Inclusion frequencies weighted")

# _____Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$model$coefficients)
  coefs <- model$model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}

# ___SAVE all weighted vectors into ./RESULTS/z_BOOTSTRAP_results/2020_weighted/:--------
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_boot_2020_weight_20.3.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2020_weighted_20.3.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_w_as_20.3.25.RDS"))
saveRDS(incl_freq_2020_w_as, paste0(base_dir, "incl_freq_2020_weighted_age_sex_20.3.25.RDS"))

save_as_html(flextable(incl_freq_2020_w_as) %>%
               set_caption("2020 inclusion frequencies (age and sex) weighted"), 
             path = "./RESULTS/Table_3_inclusion_frequencies_2020_weighted_20.3.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_boot_2020_weight_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2020_weighted_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_w_as_20.3.25.RDS"))
# incl_freq_2020_w_as <- readRDS(paste0(base_dir, "incl_freq_2020_weighted_age_sex_20.3.25.RDS"))

# __NETWORK graph---------
# Create an empty adjacency matrix
all_vars <- unique(unlist(variables_used_list_sorted_w_as))  # Get all unique variables
co_occur_matrix <- matrix(0, nrow = length(all_vars), ncol = length(all_vars), dimnames = list(all_vars, all_vars))

# Count co-occurrences
for (model in variables_used_list_sorted_w_as) {
  co_occur_matrix[model, model] <- co_occur_matrix[model, model] + 1
}

# Convert matrix to an edge list
co_occur_df <- as.data.frame(as.table(co_occur_matrix))
co_occur_df <- co_occur_df[co_occur_df$Freq > 0 & co_occur_df$Var1 != co_occur_df$Var2, ]  # Remove self-links

# Create an igraph object
graph_2020 <- graph_from_data_frame(co_occur_df, directed = FALSE)

# Set edge weights based on frequency
E(graph_2020)$weight <- co_occur_df$Freq

# Define transparency scaling
edge_transparency <- 0.05 + (E(graph_2020)$weight / max(E(graph_2020)$weight)) * 0.35  # Scale between 0.05 and 0.4

# Network visualization for 2020 bootstrap results
set.seed(123)
ggraph(graph_2020, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = edge_transparency), color = "darkblue") +
  geom_node_point(size = 5, color = "green") +
  geom_node_text(aes(label = name), 
                 size = 5,          # Larger text
                 fontface = "bold", # Bold labels
                 color = "red",     # Red text
                 repel = TRUE) +    # Prevent overlapping labels
  theme_void() +
  ggtitle("2020: Co-Occurrence Network of Bootstrap Selected Variables") +
  scale_edge_width_continuous(range = c(0.5, 5)) +  # Adjust edge thickness
  scale_edge_alpha_continuous(range = c(0.1, 0.4)) + # Adjust transparency
  theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center title

# Save the graph as an RDS file
saveRDS(graph_2020, "./CURRENT_R_OUTPUTS/2020_bootstrap_network_20.3.25.RDS")



# 2017 + 2020 excl. COVID COMBINED---------------
sleep17_sub_for_comb <- sleep17_sub
colnames(sleep17_sub_for_comb)[1] <- "sleepWorkDays"
identical(colnames(sleep17_sub_for_comb), colnames(sleep20_sub)) # TRUE

sleep20_sub$Survey <- "2020"
sleep17_sub_for_comb$Survey <- "2017"

sleep17_20 <- rbind(sleep17_sub_for_comb, sleep20_sub, ignore.attr = TRUE)

# __weights for combined data set:----
# individual weights are in 
sum(sleep17$weight)
sum(sleep20$weight)

# combine weights for combined 
total_weight_2017 <- sum(sleep17$weight)
total_weight_2020 <- sum(sleep20$weight)
n_2017 <- nrow(sleep17)
n_2020 <- nrow(sleep20)
total_n <- n_2017 + n_2020
scaling_2017 <- total_n / (2 * n_2017)
scaling_2020 <- total_n / (2 * n_2020)
sleep17_sub$weight_combined <- sleep17$weight * scaling_2017
sleep20_sub$weight_combined <- sleep20$weight * scaling_2020
sum(sleep17_sub$weight_combined) + sum(sleep20_sub$weight_combined) # 1631


#__ FULL model 2017 + 2020 (excl. COVID) ----

# ___full model unweighted-------------
model_full_17_20 <- lm(sleepWorkDays ~ ., data = sleep17_20)
summary(model_full_17_20) # Multiple R-squared:  0.2763,	Adjusted R-squared:  0.2576 
check_model(model_full_17_20) # PPC and residuals a bit off

# ___full model weighted-------------
model_full_17_20_weighted <- lm(sleepWorkDays ~ ., 
                                data = sleep17_20,
                                weights = c(sleep17_sub$weight_combined, sleep20_sub$weight_combined))
summary(model_full_17_20_weighted) # Multiple R-squared:  0.3126,	Adjusted R-squared:  0.2949

# ___BE: unweighted----
k_17_20 <- ols_step_backward_aic(model_full_17_20)
BE_model_17_20 <- lm(as.formula(k_17_20$model$terms), 
                      x=TRUE, y=TRUE, 
                      data = sleep17_20)
summary(BE_model_17_20) # Multiple R-squared:  0.2685,	Adjusted R-squared:  0.2567 

# ___BE: weighted----
BE_model_17_20_weighted <- stepAIC(model_full_17_20_weighted, 
                                   direction = "backward", 
                                   trace = FALSE)
summary(BE_model_17_20_weighted) # Multiple R-squared:  0.3016,	Adjusted R-squared:  0.292 
saveRDS(BE_model_17_20_weighted, "./CURRENT_R_OUTPUTS/BE_model_17_20_weighted_25.3.25.RDS")
gtsummary::tbl_regression(BE_model_17_20_weighted)
tbl_regression(
  BE_model_17_20_weighted,
  label = list(
    number_children        = "Number of children",
    BMI                    = "Body Mass Index (BMI)",
    work                   = "Employment status",
    full_time_exkl_NS      = "Full-time (excl. night shifts)",
    part_time_exkl_NS      = "Part-time (excl. night shifts)",
    full_time_inkl_NS      = "Full-time (incl. night shifts)",
    part_time_inkl_NS      = "Part-time (incl. night shifts)",
    min_sleep_required     = "Minimum sleep required (hours)",
    chronotype             = "Chronotype",
    diagn_sleep_disorder   = "Diagnosed sleep disorder",
    insomniaChron          = "Chronic insomnia",
    caffeine_intake        = "Caffeine intake",
    dinner_WorkDays        = "Dinner time on workdays",
    smoking                = "Smoking status",
    Survey                 = "Survey year"
  )
)


# ___+ BOOTSTRAP ------

set.seed(10325)
n <- round(dim(sleep17_20)[1])
n_boot <- n_bootstraps_par_global*2

# _____vectors for unweighted bootstrapping----------
predictors_boot <- c()
variables_used_list <- list()
boot_strap_models_list <- list()
adj.r.squared <- c()
count_VIF <- 0

# _____vectors for weighted bootstrapping----
predictors_boot_w_as <- c()
variables_used_list_w_as <- list()
boot_strap_models_list_w_as <- list()
adj.r.squared_w_as <- c()
count_VIF_w_as <- 0

tic()
for(i in 1:n_boot){
  choose_ind <- sample(1:n, replace = TRUE)
  
  # unweighted:
  sleep17_20_boot_repl <- sleep17_20[choose_ind, ]
  model_boot <- lm(sleepWorkDays ~ Age_group_merged + gender + marrital_status + 
                     number_children + BMI + work + min_sleep_required + napping + 
                     chronotype + diagn_sleep_disorder + insomniaChron + 
                     alcohol_intake + caffeine_intake + dinner_WorkDays + 
                     sport_yes_no + smoking + general_health + Survey,
                   data = sleep17_20_boot_repl)
  count_VIF <- count_VIF + sum(VIF(model_boot)[,3] > 3)   # multicolinearity?
  k <- ols_step_backward_aic(model_boot) # AIC
  boot_strap_models_list[[i]] <- k
  variables_used <- labels(terms(as.formula(k$model$terms)))
  variables_used_list[[i]] <- sort(variables_used)
  predictors_boot[i] <- paste(sort(variables_used), collapse = " ")
  adj.r.squared[i] <- summary(model_boot)$adj.r.squared
  
  # weighted:
  sleep17_20_boot_weighted <- sleep17_20_boot_repl # same
  design_weighted_17_20 <- create_survey_design(year = 2020, 
                                                given_subset = sleep17_20_boot_weighted,
                                                pop_proportions = pop_proportions_2020)
  model_boot_w <- lm(sleepWorkDays ~  Age_group_merged + gender + marrital_status + 
                       number_children + BMI + work + min_sleep_required + napping + 
                       chronotype + diagn_sleep_disorder + insomniaChron + 
                       alcohol_intake + caffeine_intake + dinner_WorkDays + 
                       sport_yes_no + smoking + general_health + Survey,
                     data = sleep17_20_boot_weighted,
                     weights = weights(design_weighted_17_20))
  count_VIF_w_as <- count_VIF_w_as + sum(VIF(model_boot_w)[,3] > 3)   # multicolinearity?
  k_w_as <- stepAIC(model_boot_w, direction = "backward", trace = FALSE) # now stepAIC
  boot_strap_models_list_w_as[[i]] <- k_w_as
  variables_used_w_as <- labels(as.formula(k_w_as$terms))
  variables_used_list_w_as[[i]] <- sort(variables_used_w_as)
  predictors_boot_w_as[i] <- paste(sort(variables_used_w_as), collapse = " ")
  adj.r.squared_w_as[i] <- summary(model_boot_w)$adj.r.squared
  
  if(i %% 10 == 0){ # counter
    # percent counter
    cat(i, "of", n_boot, "iterations completed\n")
  }
}
toc() # 4000s

# # _____a) bootstrap results for unweighted-----------
# pred_boot_df <- data.frame(Predictors = predictors_boot)
# variables_used_list_sorted <- lapply(variables_used_list, sort)

# hist(adj.r.squared)
# quantile(adj.r.squared, p = c(0.025,0.975)) 
# saveRDS(adj.r.squared, "./CURRENT_R_OUTPUTS/z_BOOTSTRAP_results/adj.r.squared_boot_17_20_unweighted_19.3.25.RDS")
# #saveRDS(adj.r.squared, "./DATA/adj.r.squared_boot_2020_sub_working.RDS")
# ....

# ____b) bootstrap results for weighted-------------
pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)
hist(adj.r.squared_w_as)
quantile(adj.r.squared_w_as, p = c(0.025, 0.975)) # 0.2551636 0.4059558 

# ______Model selection frequencies (analog Table 6 Heinze): ----
pred_boot_df_w_as <- pred_boot_df_w_as %>%
  dplyr::count(sort(Predictors)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(20) %>%
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors") # 8-18

#______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$model$coefficients)
  coefs <- model$model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}

# ______Inclusion frequencies of individual variables: ----
all_vars_w_as <- data.frame(vars = unlist(variables_used_list_sorted_w_as))
incl_freq_17_20_w_as <- all_vars_w_as %>%
  dplyr::count(vars) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(Percent_of_models = n/n_boot*100)
flextable(incl_freq_17_20_w_as)
  
# with beautiful labels:
colnames(incl_freq_17_20_w_as) <- c("Variables", "n", "% of models")
incl_freq_17_20_w_as$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("work", "Work", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("general_health", "General health", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("marrital_status", "Marital status", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("chronotype", "Chronotype", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("age", "Age", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("genbder", "Gender", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("smoking", "Smoking", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("napping", "Napping", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("number_children", "Number of children", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("sport_yes_no", "Sports", incl_freq_17_20_w_as$Variables)
incl_freq_17_20_w_as$Variables <- gsub("Age_group_merged", "Age group", incl_freq_17_20_w_as$Variables)

flextable(incl_freq_17_20_w_as) %>%
  set_caption("2017 + 2020 Inclusion frequencies weighted")

# _____Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$model$coefficients)
  coefs <- model$model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}

# ___SAVE all resulting vectors into ./RESULTS/z_BOOTSTRAP_results/2017_2020_combined/:---------
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2017_2020_combined/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_17_20_w_as_20.3.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_17_20_w_as_20.3.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_17_20_w_as_20.3.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_boot_17_20_weighted_20.3.25.RDS"))
saveRDS(incl_freq_17_20_w_as, paste0(base_dir, "incl_freq_17_20_weighted_20.3.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_17_20_weighted_20.3.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_17_20_weighted_20.3.25.RDS"))

save_as_html(flextable(incl_freq_17_20_w_as) %>%
               set_caption("2017 + 2020 inclusion frequencies weighted"), 
             path = "./RESULTS/Table_3_inclusion_frequencies_17_20_weighted_20.3.25.html")

# READ
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2017_2020_combined/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_17_20_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_17_20_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_17_20_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_boot_17_20_weighted_20.3.25.RDS"))
# incl_freq_17_20_w_as <- readRDS(paste0(base_dir, "incl_freq_17_20_weighted_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_17_20_weighted_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_17_20_weighted_20.3.25.RDS"))

# _NETWORK graph---------

# Create an empty adjacency matrix
all_vars <- unique(unlist(variables_used_list_sorted_w_as))  # Get all unique variables
co_occur_matrix <- matrix(0, nrow = length(all_vars), ncol = length(all_vars), dimnames = list(all_vars, all_vars))

# Count co-occurrences
for (model in variables_used_list_sorted_w_as) {
  co_occur_matrix[model, model] <- co_occur_matrix[model, model] + 1
}

# Convert matrix to an edge list
co_occur_df <- as.data.frame(as.table(co_occur_matrix))
co_occur_df <- co_occur_df[co_occur_df$Freq > 0 & co_occur_df$Var1 != co_occur_df$Var2, ]  # Remove self-links

# Create an igraph object
graph_17_20 <- graph_from_data_frame(co_occur_df, directed = FALSE)

# Set edge weights based on frequency
E(graph_17_20)$weight <- co_occur_df$Freq

# Define transparency scaling
edge_transparency <- 0.05 + (E(graph_17_20)$weight / max(E(graph_17_20)$weight)) * 0.35  # Scale between 0.05 and 0.4

# Network visualization for 2017-2020 combined bootstrap results
set.seed(123)
ggraph(graph_17_20, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = edge_transparency), color = "darkblue") +
  geom_node_point(size = 5, color = "green") +
  geom_node_text(aes(label = name), 
                 size = 5,          # Larger text
                 fontface = "bold", # Bold labels
                 color = "red",     # Red text
                 repel = TRUE) +    # Prevent overlapping labels
  theme_void() +
  ggtitle("2017-2020 Combined: Co-Occurrence Network of Bootstrap Selected Variables") +
  scale_edge_width_continuous(range = c(0.5, 5)) +  # Adjust edge thickness
  scale_edge_alpha_continuous(range = c(0.1, 0.4)) + # Adjust transparency
  theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center title

# Save the graph as an RDS file
saveRDS(graph_17_20, "./CURRENT_R_OUTPUTS/17_20_bootstrap_network_20.3.25.RDS")


# 2020 BE (with COVID)___________ ----

# ___+ subset-analysis YES/NO:----
#sleep20_sub <- sleep20_sub[work != "not_currently_working",]
# dim(sleep20_sub) # 676 obs.

# check linear dependencies in full (global) model:
# model_full <- lm(sleepWorkDays ~ ., data = sleep20_sub)
# car::vif(model_full)
# detect.lindep(model.matrix(model_full, model = "pooling"))
# "No linear dependent column(s) detected."
#alias(model_full)

# ___FULL MODEL---------

# ____full model, unweighted -------------
colnames(sleep20_sub_inkl_COVID)
model <- lm(sleepWorkDays ~ ., data = sleep20_sub_inkl_COVID) 

# ____full model, weighted-------------
model_weighted <- lm(sleepWorkDays ~ ., 
                     data = sleep20_sub_inkl_COVID,
                     weights = weights(design_weighted_2020))
summary(model_weighted) # Multiple R-squared:  0.3526,	Adjusted R-squared:  0.3041 

# Multiple R^2:
weights <- weights(design_weighted_2020)
y_hat <- predict(model_weighted, type = "response")
y <- sleep20_sub_inkl_COVID$sleepWorkDays
y_mean <- sum(weights * y) / sum(weights)
rss <- sum(weights * (y - y_hat) ^ 2)
tss <- sum(weights * (y - y_mean) ^ 2)
# Pseudo R²
pseudo_R2 <- 1 - (rss / tss)
pseudo_R2 # 0.3526121

check_model(model_weighted) # PPC and residuals suboptimal, VIF?
qqPlot(model_weighted) # residuals not so normal

FSA_model <- lmFSA(sleepWorkDays ~ ., data = sleep20_sub_inkl_COVID)
summary(FSA_model) # same as above 2020.

# ___BE Models:--------

# ____BE: unweighted (AIC)-----------
k <- ols_step_backward_aic(model)
#alias(k$model)
#plot(model)

BE_model_2020_with_COVID <- lm(as.formula(k$model$terms), 
                               x=TRUE, y=TRUE, 
                               data = sleep20_sub_inkl_COVID)
summary(BE_model_2020_with_COVID) # Adjusted R-squared:  0.1858  


#____BE: weighted (AIC)----------
BE_model_2020_with_COVID_weighted <- stepAIC(model_weighted, 
                                             direction = "backward", 
                                             trace = FALSE)
summary(BE_model_2020_with_COVID_weighted) # Adjusted R-squared:  0.3074
check_model(BE_model_2020_with_COVID_weighted) # PPC and residuals suboptimal

saveRDS(BE_model_2020_with_COVID_weighted, file = "./CURRENT_R_OUTPUTS/BE_model_2020_with_COVID_weighted_19.3.25.rds")

# DROP in R-squared:

# Full model: Adjusted R-squared:  0.3041 
# BE model weighted: Adjusted R-squared:  0.3074
0.3074 - 0.3041 # 0.0033
                                     
# # ___+ possible interaction terms (2020); unweighted
# lm_interactions <- FSA(formula = as.formula(k$model$terms), data = sleep20_sub, cores = 1, m = 2,
#                        interactions = TRUE, criterion = AIC, minmax = "min",
#                        numrs = 10)
# summary(lm_interactions) # possibly interaction term "min_sleep_required*dinner_WorkDays"
# BE_model_2020_with_COVID_interac <- lm(sleepWorkDays ~ Age_group_merged + gender + work + diagn_sleep_disorder + 
#                                          caffeine_intake + dinner_WorkDays + sleep_quality_change + 
#                                          coff_change + attitude_towards_future + 
#                                          min_sleep_required*dinner_WorkDays, data = sleep20_sub) # automate later....
# summary(BE_model_2020_with_COVID_interac) # Adjusted R-squared:  0.1906




# ___+ BOOTSTRAP ------
# _____Bootstrap resampling----

set.seed(10325) # previously, no seed was used!
n <- round(dim(sleep20_sub_inkl_COVID)[1])
n_boot <- n_bootstraps_par_global

# _____vectors for unweighted bootstrapping----------
predictors_boot <- c()
variables_used_list <- list()
boot_strap_models_list <- list()
count_VIF <- 0
adj.r.squared <- c()

# _____vectors for weighted bootstrapping----
predictors_boot_w_as <- c()
variables_used_list_w_as <- list()
boot_strap_models_list_w_as <- list()
count_VIF_w_as <- 0
adj.r.squared_w_as <- c()

tic()
for(i in 1:n_boot){
  choose_ind <- sample(1:n, replace = TRUE)
  
  # unweighted:
  sleep20_sub_boot_repl <- sleep20_sub_inkl_COVID[choose_ind, ]
  model_boot <- lm(sleepWorkDays ~ ., data = sleep20_sub_boot_repl)
  count_VIF <- count_VIF + sum(VIF(model_boot)[,3] > 3)   # multicolinearity?
  k <- ols_step_backward_aic(model_boot) # AIC
  boot_strap_models_list[[i]] <- k
  variables_used <- labels(terms(as.formula(k$model$terms)))
  variables_used_list[[i]] <- sort(variables_used)
  predictors_boot[i] <- paste(sort(variables_used), collapse = " ")
  adj.r.squared[i] <- summary(model_boot)$adj.r.squared
  
  # weighted:
  sleep20_sub_boot_weighted <- sleep20_sub_boot_repl # same
  design_2020_boot <- create_survey_design(year = 2020, 
                                           given_subset = sleep20_sub_boot_weighted,
                                           pop_proportions = pop_proportions_2020)
  model_boot_w <- lm(sleepWorkDays ~ Age_group_merged + gender + marrital_status + 
                           number_children + BMI + work + min_sleep_required + 
                           napping + chronotype + diagn_sleep_disorder + 
                           insomniaChron + alcohol_intake + caffeine_intake + 
                           dinner_WorkDays + sport_yes_no + smoking + 
                           general_health + ho_new + sleep_quality_change + 
                           alc_change + coff_change + diet_change + days_walking + 
                           digital_device_use + emotional_burden_measures + 
                           financial_burden_measures + attitude_towards_future,
                         data = sleep20_sub_boot_weighted,
                         weights = weights(design_2020_boot))
  k_w_as <- stepAIC(model_boot_w, direction = "backward", trace = FALSE) 
  count_VIF_w_as <- count_VIF_w_as + sum(VIF(model_boot_w)[,3] > 3)   # multicolinearity?
  boot_strap_models_list_w_as[[i]] <- k_w_as
  variables_used_w_as <- labels(as.formula(k_w_as$terms))
  variables_used_list_w_as[[i]] <- sort(variables_used_w_as)
  predictors_boot_w_as[i] <- paste(sort(variables_used_w_as), collapse = " ")
  adj.r.squared_w_as[i] <- summary(model_boot_w)$adj.r.squared
  
  if(i %% 10 == 0){ # counter
    # percent counter
    cat(i, "of", n_boot, "iterations completed\n")
  }

}
toc() # 4239.21 sec elapsed

pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)

# # _____a) bootstrap results for unweighted-----------
# 
# # ______Model selection frequencies (analog Table 6 Heinze): ----
# pred_boot_df <- pred_boot_df %>% 
#   dplyr::count(sort(Predictors)) %>% 
#   arrange(desc(n)) %>%
#   top_n(20) %>% 
#   mutate(Percent = n/n_boot*100) %>%
#   mutate(Cumulative_Percent = cumsum(Percent))
# pred_boot_df
# 
# # ______Model sizes overview (number of predictors): ----
# plot(table(lengths(variables_used_list_sorted)), xlab = "number of predictors") # 7-21
# 
# # ______Inclusion frequencies of individual variables: ----
# all_vars <- data.frame(vars = unlist(variables_used_list_sorted))
# incl_freq_2020 <- all_vars %>% 
#   dplyr::count(vars) %>% 
#   arrange(desc(n)) %>%
#   mutate(Percent_of_models = n/n_boot*100)
# flextable(incl_freq_2020)
# # with beautiful labels:
# colnames(incl_freq_2020) <- c("Variables", "n", "% of models")
# incl_freq_2020$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("work", "Work", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("gender", "Sex", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("sleep_quality_change", "Sleep quality change", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("attitude_towards_future", "Attitude towards the future", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("chronotype", "Chronotype", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("number_children", "Number of children", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("age", "Age", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("coff_change", "Change in coffee consumption", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("financial_burden_measures", "Financial burden of lockdown measures", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("smoking", "Smoking", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("emotional_burden_measures", "Emotional burden of lockdown measures", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("general_health", "General health", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("days_walking", "Number of days walking", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("digital_device_use", "Change in digital device use", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("napping", "Napping", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("diet_change", "Change of diet", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("alc_change", "Change in alcohol consumption", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("marrital_status", "Marital status", incl_freq_2020$Variables)
# incl_freq_2020$Variables <- gsub("ho_new", "Home office", incl_freq_2020$Variables)
# flextable(incl_freq_2020)
# saveRDS(incl_freq_2020, "./CURRENT_R_OUTPUTS/z_BOOTSTRAP_results/incl_freq_2020_weighted_incl_COVID_19.3.25.RDS")
# #saveRDS(incl_freq_2020, "incl_freq_2020_incl_COVID_sub_working.RDS")
# 
# #______Stability of coefficients: ----
# coefs_bootstrap <- data.frame()
# for(i in 1:n_boot){
#   model <-  boot_strap_models_list[[i]]
#   coef_names <- names(model$model$coefficients)
#   coefs <- model$model$coefficients
#   coefs_bootstrap <- rbind(coefs_bootstrap, data.frame(coef_names, coefs))
# }
# rownames(coefs_bootstrap) <- NULL
# colnames(coefs_bootstrap) <- c("coef_names", "coef_values")
# unique_coefs <- unique(coefs_bootstrap$coef_names) # how many different coefs where used in all bootstrap samples?
# 
# #for(i in 1:32){
# #  noquote(print(paste0("hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[",i,"])], main = unique_coefs[",i,"])")))
# #}
# 
# length(unique_coefs)
# 
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[1])], main = unique_coefs[1])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[2])], main = unique_coefs[2])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[3])], main = unique_coefs[3])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[4])], main = unique_coefs[4])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[5])], main = unique_coefs[5])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[6])], main = unique_coefs[6])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[7])], main = unique_coefs[7])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[8])], main = unique_coefs[8])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[9])], main = unique_coefs[9])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[10])], main = unique_coefs[10])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[11])], main = unique_coefs[11])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[12])], main = unique_coefs[12])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[13])], main = unique_coefs[13])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[14])], main = unique_coefs[14])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[15])], main = unique_coefs[15])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[16])], main = unique_coefs[16])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[17])], main = unique_coefs[17])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[18])], main = unique_coefs[18])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[19])], main = unique_coefs[19])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[20])], main = unique_coefs[20])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[21])], main = unique_coefs[21])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[22])], main = unique_coefs[22])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[23])], main = unique_coefs[23])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[24])], main = unique_coefs[24])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[25])], main = unique_coefs[25])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[26])], main = unique_coefs[26])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[27])], main = unique_coefs[27])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[28])], main = unique_coefs[28])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[29])], main = unique_coefs[29])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[30])], main = unique_coefs[30])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[31])], main = unique_coefs[31])
# hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[32])], main = unique_coefs[32])

# _____b) bootstrap results for weighted-------------

# ______Model selection frequencies (analog Table 6 Heinze): ----
if("Predictors" %nin% colnames(pred_boot_df_w_as)){
  pred_boot_df_w_as$Predictors <- pred_boot_df_w_as$`sort(Predictors)`
  pred_boot_df_w_as$`sort(Predictors)` <- NULL
}
pred_boot_df_w_as <- pred_boot_df_w_as %>%
  dplyr::count(sort(Predictors)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(20) %>%
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as
saveRDS(pred_boot_df_w_as, "./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2020_w_as_19.3.25.RDS")

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors") # 9-23

# ______Inclusion frequencies of individual variables: ----
all_vars <- data.frame(vars = unlist(variables_used_list_sorted_w_as))
incl_freq_2020 <- all_vars %>% 
  dplyr::count(vars) %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(Percent_of_models = n/n_boot*100)
flextable(incl_freq_2020)

# with beautiful labels:
colnames(incl_freq_2020) <- c("Variables", "n", "% of models")
incl_freq_2020$Variables <- gsub("dinner_WorkDays", "Dinner time on workdays", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("min_sleep_required", "Minimum sleep required", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("work", "Work", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("gender", "Gender", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("caffeine_intake", "Caffeine intake", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("sleep_quality_change", "Sleep quality change", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("attitude_towards_future", "Attitude towards the future", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("chronotype", "Chronotype", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("number_children", "Number of children", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("age", "Age", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("Age_group_merged", "Age group", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("diagn_sleep_disorder", "Diagnosed sleep disorder", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("coff_change", "Change in coffee consumption", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("financial_burden_measures", "Financial burden of lockdown measures", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("alcohol_intake", "Alcohol intake", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("smoking", "Smoking", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("emotional_burden_measures", "Emotional burden of lockdown measures", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("general_health", "General health", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("insomniaChron", "Chronic insomnia", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("days_walking", "Number of days walking", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("digital_device_use", "Change in digital device use", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("napping", "Napping", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("sport_yes_no", "Sports", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("diet_change", "Change of diet", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("alc_change", "Change in alcohol consumption", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("marrital_status", "Marital status", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("ho_new", "Home office", incl_freq_2020$Variables)

flextable(incl_freq_2020) %>%
  set_caption("2020 incl COVID inclusion frequencies (age and sex) weighted")


# ______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$coefficients)
  coefs <- model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}

rownames(coefs_bootstrap_w_as) <- NULL
colnames(coefs_bootstrap_w_as) <- c("coef_names", "coef_values")
unique_coefs_w_as <- unique(coefs_bootstrap_w_as$coef_names) # how many different coefs where used in all bootstrap samples?

#for(i in 1:32){
#  noquote(print(paste0("hist(coefs_bootstrap$coef_values[which(coefs_bootstrap$coef_names==unique_coefs[",i,"])], main = unique_coefs[",i,"])")))
#}

length(unique_coefs_w_as)

hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[1])], main = unique_coefs_w_as[1])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[2])], main = unique_coefs_w_as[2])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[3])], main = unique_coefs_w_as[3])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[4])], main = unique_coefs_w_as[4])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[5])], main = unique_coefs_w_as[5])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[6])], main = unique_coefs_w_as[6])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[7])], main = unique_coefs_w_as[7])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[8])], main = unique_coefs_w_as[8])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[9])], main = unique_coefs_w_as[9])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[10])], main = unique_coefs_w_as[10])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[11])], main = unique_coefs_w_as[11])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[12])], main = unique_coefs_w_as[12])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[13])], main = unique_coefs_w_as[13])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[14])], main = unique_coefs_w_as[14])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[15])], main = unique_coefs_w_as[15])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[16])], main = unique_coefs_w_as[16])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[17])], main = unique_coefs_w_as[17])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[18])], main = unique_coefs_w_as[18])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[19])], main = unique_coefs_w_as[19])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[20])], main = unique_coefs_w_as[20])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[21])], main = unique_coefs_w_as[21])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[22])], main = unique_coefs_w_as[22])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[23])], main = unique_coefs_w_as[23])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[24])], main = unique_coefs_w_as[24])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[25])], main = unique_coefs_w_as[25])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[26])], main = unique_coefs_w_as[26])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[27])], main = unique_coefs_w_as[27])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[28])], main = unique_coefs_w_as[28])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[29])], main = unique_coefs_w_as[29])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[30])], main = unique_coefs_w_as[30])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[31])], main = unique_coefs_w_as[31])
hist(coefs_bootstrap_w_as$coef_values[which(coefs_bootstrap_w_as$coef_names==unique_coefs_w_as[32])], main = unique_coefs_w_as[32])

# save all weighted results to ./RESULTS/z_BOOTSTRAP_results/2020_incl_COVID_weighted/:
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_incl_COVID_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2020_w_as_20.3.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_2020_w_as_20.3.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
saveRDS(incl_freq_2020, paste0(base_dir, "incl_freq_2020_weighted_incl_COVID_20.3.25.RDS"))

save_as_html(flextable(incl_freq_2020) %>%
               set_caption("2020 incl COVID inclusion frequencies (age and sex) weighted"),
             path = "./RESULTS/Table_3_inclusion_frequencies_2020_incl_COVID_weighted_20.3.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_incl_COVID_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2020_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_2020_w_as_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
# incl_freq_2020 <- readRDS(paste0(base_dir, "incl_freq_2020_weighted_incl_COVID_20.3.25.RDS"))

# _NETWORK graph-------
# Create an empty adjacency matrix
all_vars <- unique(unlist(variables_used_list_sorted_w_as))  # Get all unique variables
co_occur_matrix <- matrix(0, nrow = length(all_vars), ncol = length(all_vars), dimnames = list(all_vars, all_vars))

# Count co-occurrences
for (model in variables_used_list_sorted_w_as) {
  co_occur_matrix[model, model] <- co_occur_matrix[model, model] + 1
}

# Convert matrix to an edge list
co_occur_df <- as.data.frame(as.table(co_occur_matrix))
co_occur_df <- co_occur_df[co_occur_df$Freq > 0 & co_occur_df$Var1 != co_occur_df$Var2, ]  # Remove self-links

# Create an igraph object
graph_2020_covid <- graph_from_data_frame(co_occur_df, directed = FALSE)

# Set edge weights based on frequency
E(graph_2020_covid)$weight <- co_occur_df$Freq

# Define transparency scaling
edge_transparency <- 0.05 + (E(graph_2020_covid)$weight / max(E(graph_2020_covid)$weight)) * 0.35  # Scale between 0.05 and 0.4

# Network visualization for 2020 (Including COVID) bootstrap results
set.seed(123)
ggraph(graph_2020_covid, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = edge_transparency), color = "darkblue") +
  geom_node_point(size = 5, color = "green") +
  geom_node_text(aes(label = name), 
                 size = 5,          # Larger text
                 fontface = "bold", # Bold labels
                 color = "red",     # Red text
                 repel = TRUE) +    # Prevent overlapping labels
  theme_void() +
  ggtitle("2020 (Including COVID): Co-Occurrence Network of Bootstrap Selected Variables") +
  scale_edge_width_continuous(range = c(0.5, 5)) +  # Adjust edge thickness
  scale_edge_alpha_continuous(range = c(0.1, 0.4)) + # Adjust transparency
  theme(plot.title = element_text(size = 16, hjust = 0.5))  # Center title

# Save the graph as an RDS file
saveRDS(graph_2020_covid, "./CURRENT_R_OUTPUTS/2020_COVID_bootstrap_network_20.3.25.RDS")



# RUN THROUGH with 1000 bootstrap replicates until HERE without errors: 20.3.25---------


# HERE-------------
# SUPPLEMENTARY figures ----

# __Supplementary figure 1S: comparison of sleep durations 2017 (self reported and via bed and wake times) ----
df <- na.omit(data.frame(a = sleep17$sleepWorkDays_bw, b = sleep17$sleepWorkDays))
sp <- ggscatter(df, x = "a", y = "b",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = FALSE,
                xlab = "Sleep duration on workdays via bed and waketimes",
                ylab = "Self-reported sleep duration"
)
sp + stat_cor(method = "pearson", label.x = 3, label.y = 10)


# __Supplementary figure 2S: Age distributions survey vs population 2017 and 2020 ------
#__2017------
Age_count_groups_Austria_2017 <- data.frame(Age_group = c("<20 years","20-24 years","25-29 years","30-34 years",
                                                          "35-39 years", "40-44 years" ,"45-49 years", "50-54 years",
                                                          "55-59 years" ,"60-64 years" ,"65-69 years","70 or older"), 
                                            count_age_group = c(1719764,543635,601262,597640,
                                                                584948,572653,669472,715116,
                                                                641443,520605,448802,1196442)/8811782)
Age_count_groups_Austria_2017$Age_group <- as.factor(Age_count_groups_Austria_2017$Age_group)

df <- sleep17 %>% dplyr::mutate(Age_group = Q1_cat) 
df$Age_group <- as.factor(as.character(df$Age_group))
Age_count_groups_Austria_2017$Age_group <- as.factor(as.character(Age_count_groups_Austria_2017$Age_group))
merged <- left_join(df, Age_count_groups_Austria_2017, by="Age_group")
merged <- merged %>% 
  dplyr::select(sleepWorkDays_bw, count_age_group, Age_group) %>% 
  drop_na()
mean(sleep17$sleepWorkDays_bw, na.rm = TRUE) # 7.23


mean(sleep17$sleepWorkDays_bw, na.rm = TRUE) # 7.21
weighted.mean(merged$sleepWorkDays_bw, merged$count_age_group) # 7.23

median(sleep17$sleepWorkDays_bw, na.rm = TRUE) # 7.00
limma::weighted.median(merged$sleepWorkDays_bw, merged$count_age_group) # 7
matrixStats::weightedMedian(merged$sleepWorkDays_bw, merged$count_age_group) # 7
DescTools::Median(merged$sleepWorkDays_bw, merged$count_age_group) # 7.09
weighted.quantile.harrell.davis(merged$sleepWorkDays_bw, probs = 0.5, 
                                weights = merged$count_age_group) # 7.09
# ....
# ....


# __Supplementary figure 3S: Comparison of bed and wake times, both years ------
# Bed and wake times comparison
dat17 <- sleep17
dat20 <- sleep20

colnames(dat17)[which(colnames(dat17)=="sleepWorkDays_bw")] <- "Sleep_duration_workdays"
colnames(dat17)[which(colnames(dat17)=="bedTimeWorkDay_bw")] <- "Bedtime_workdays"  
colnames(dat17)[which(colnames(dat17)=="wakeTimeWorkDay_bw")] <- "Waketime_workdays"

colnames(dat20)[which(colnames(dat20)=="sleepWorkDays")] <- "Sleep_duration_workdays"
colnames(dat20)[which(colnames(dat20)=="bedTimeWorkDay")] <- "Bedtime_workdays"
colnames(dat20)[which(colnames(dat20)=="wakeTimeWorkDay")] <- "Waketime_workdays"

dat17$Survey <- 2017
dat20$Survey <- 2020

list_of_dfs <- list(dat17[, .(Sleep_duration_workdays,
                                            Bedtime_workdays,
                                            Waketime_workdays,
                                            Survey)],
                                  dat20[, .(Sleep_duration_workdays,
                                            Bedtime_workdays,
                                            Waketime_workdays,
                                            Survey)])
plot_sleep <- data.table::rbindlist(list_of_dfs, use.names = TRUE, ignore.attr = TRUE)

plot_sleep$Bedtime_workdays

# add weights
plot_sleep$weights[plot_sleep$Survey == 2017] <- weights(design_weighted)
plot_sleep$weights[plot_sleep$Survey == 2020] <- weights(design_weighted_2020)

plot_sleep$Bedtime_workdays_posix <- as.POSIXct(strptime(plot_sleep$Bedtime_workdays,format = "%H:%M"))
plot_sleep$Waketime_workdays_posix <- as.POSIXct(strptime(plot_sleep$Waketime_workdays,format = "%H:%M"))
head(plot_sleep)
hist(plot_sleep$Bedtime_workdays_posix, breaks = 48)

plot_sleep$Bedtime_workdays_hours <- hour(plot_sleep$Bedtime_workdays_posix) + minute(plot_sleep$Bedtime_workdays_posix)/60
plot_sleep$Waketime_workdays_hours <- hour(plot_sleep$Waketime_workdays_posix) + minute(plot_sleep$Waketime_workdays_posix)/60

hist(plot_sleep$Bedtime_workdays_hours)

plot_sleep$Survey <- as.factor(plot_sleep$Survey)

#shift left bed times to the right
ind <- which(plot_sleep$Bedtime_workdays_hours < 12.5)
plot_sleep[ind,]$Bedtime_workdays_hours <- plot_sleep[ind,]$Bedtime_workdays_hours + 24

hist(plot_sleep$Bedtime_workdays_hours)
hist(plot_sleep$Waketime_workdays_hours)

#__Bed times -----
# Separate survey designs
design_2017 <- svydesign(ids = ~1, data = filter(plot_sleep, Survey == 2017), weights = ~weights)
design_2020 <- svydesign(ids = ~1, data = filter(plot_sleep, Survey == 2020), weights = ~weights)

#___Survey-weighted means-------
bedtime_mean_2017 <- as.numeric(svymean(~Bedtime_workdays_hours, design_2017, na.rm = TRUE))
bedtime_mean_2020 <- as.numeric(svymean(~Bedtime_workdays_hours, design_2020, na.rm = TRUE))

ggplot(plot_sleep, aes(x = Bedtime_workdays_hours, weight = weights, 
                       fill = Survey)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = bedtime_mean_2017, color = "red", 
             linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = bedtime_mean_2020, color = "blue", 
             linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = bedtime_mean_2017, y = 0.03, 
           label = "2017 Mean", color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = bedtime_mean_2020, y = 0.03, 
           label = "2020 Mean", color = "blue", angle = 90, vjust = -0.5) +
  scale_x_continuous(
    breaks = 20:32,
    labels = c("8 PM", "9 PM", "10 PM", "11 PM", "12 AM", 
               "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
               "6 AM", "7 AM", "8 AM")
  ) +
  labs(title = "Survey-weighted Bedtime Distribution (Workdays)",
       x = "Bedtime (Hours)", y = "Weighted Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# ___Survey weighted medians----------
bedtime_median_2017 <- as.numeric(svyquantile(~Bedtime_workdays_hours, design_2017, quantiles = 0.5, na.rm = TRUE, ci = FALSE)[1])
bedtime_median_2020 <- as.numeric(svyquantile(~Bedtime_workdays_hours, design_2020, quantiles = 0.5, na.rm = TRUE, ci = FALSE)[1])

ggplot(plot_sleep, aes(x = Bedtime_workdays_hours, weight = weights, fill = Survey)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = bedtime_median_2017, color = "red", linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = bedtime_median_2020, color = "blue", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = bedtime_median_2017, y = 0.03, 
           label = paste0("2017 Median = ", round(bedtime_median_2017, 2)), 
           color = "red", angle = 0, hjust = 0, vjust = -2) +
  annotate("text", x = bedtime_median_2020, y = 0.025, 
           label = paste0("2020 Median = ", round(bedtime_median_2020, 2)), 
           color = "blue", angle = 0, hjust = 0) +
  scale_x_continuous(
    breaks = 20:32,
    labels = c("8 PM", "9 PM", "10 PM", "11 PM", "12 AM", 
               "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
               "6 AM", "7 AM", "8 AM")
  ) +
  labs(title = "Survey-weighted Bedtime distribution on workdays",
       x = "", y = "Weighted Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


# __Wake times ----
# ___Survey-weighted means-----------
wake_mean_2017 <- as.numeric(svymean(~Waketime_workdays_hours, design_2017, na.rm = TRUE))
wake_mean_2020 <- as.numeric(svymean(~Waketime_workdays_hours, design_2020, na.rm = TRUE))

ggplot(plot_sleep, aes(x = Waketime_workdays_hours, weight = weights, fill = Survey)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = wake_mean_2017, color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = wake_mean_2020, color = "blue", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = wake_mean_2017, y = 0.03, label = "2017 Mean", color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = wake_mean_2020, y = 0.03, label = "2020 Mean", color = "blue", angle = 90, vjust = -0.5) +
  scale_x_continuous(
    breaks = 4:12,
    labels = c("4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM")
  ) +
  labs(title = "Survey-weighted Wake Time Distribution (Workdays)",
       x = "Wake Time (Hours)", y = "Weighted Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# ___Survey weighted medians----------
bedtime_median_2017 <- svyquantile(~Bedtime_workdays_hours, design_2017, quantiles = 0.5, na.rm = TRUE)[1]
bedtime_median_2020 <- svyquantile(~Bedtime_workdays_hours, design_2020, quantiles = 0.5, na.rm = TRUE, ci = FALSE)[1]

wake_median_2017 <- as.numeric(
  svyquantile(~Waketime_workdays_hours, design_2017, quantiles = 0.5, na.rm = TRUE, ci = FALSE)[1]
)

wake_median_2020 <- as.numeric(
  svyquantile(~Waketime_workdays_hours, design_2020, quantiles = 0.5, na.rm = TRUE, ci = FALSE)[1]
)

ggplot(plot_sleep, aes(x = Waketime_workdays_hours, weight = weights, fill = Survey)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = wake_median_2017, color = "red", linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = wake_median_2020, color = "blue", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = wake_median_2017, y = 0.03,
           label = paste0("2017 Median: ", round(wake_median_2017, 2)),
           color = "red", angle = 0, vjust = -0.5, hjust = 1.2) +
  annotate("text", x = wake_median_2020, y = 0.03,
           label = paste0("2020 Median: ", round(wake_median_2020, 2)),
           color = "blue", angle = 0, vjust = -0.5, hjust = -0.2) +
  scale_x_continuous(
    breaks = 4:12,
    labels = c("4 AM", "5 AM", "6 AM", "7 AM", "8 AM", 
               "9 AM", "10 AM", "11 AM", "12 PM")
  ) +
  labs(title = "Survey-weighted wake time distribution on workdays",
       x = "", y = "Weighted Density") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))





# __Length of day Austria ----
sun <- read_excel("./Main_Document/Tageslaenge_Oesterreich.xlsx", sheet = 1)
sun %>% ggplot(aes(x = Tag_laufend, y = Tageslaenge_in_h)) +
  geom_line() + 
  geom_vline(xintercept = 135, linetype="dashed", color = "red") + 
  geom_vline(xintercept = 258, linetype="dashed", color = "red") + 
  xlab("Number of days in year") + 
  ylab("Length of day in hours") + 
  annotate(geom = "text", x = 105, y = 15.5, label = "May (2020)", color = "red") + 
  annotate(geom = "text", x = 305, y = 13, label = "September (2017)", color = "red")

# __Sleep duration comparison by age-groups -----
df1 <- sleep17 %>%
  group_by(age) %>%
  dplyr::summarize(Mean = mean(sleepWorkDays_bw, na.rm = TRUE))
df2 <- sleep20 %>%
  group_by(age) %>%
  dplyr::summarize(Mean = mean(sleepWorkDays, na.rm = TRUE))
df <- left_join(df1, df2, by = "age")
colnames(df) <- c("Age_group","mean_sleep_durations_2017", "mean_sleep_durations_2020")
df <- df %>% mutate(difference = mean_sleep_durations_2020 - mean_sleep_durations_2017)
df <- df %>% mutate(Age_group_label = c("<20 years","20-24 years","25-29 years","30-34 years",
                                  "35-39 years", "40-44 years" ,"45-49 years", "50-54 years",
                                  "55-59 years" ,"60-64 years" ,"65-69 years"))
df %>% ggplot(aes(x = Age_group_label, y = difference)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + 
  ylab("Difference in mean sleep durations (2020 minus 2017)")

# medians??????????????
df1 <- sleep17 %>%
  group_by(age) %>%
  dplyr::summarize(Median = as.numeric(median(sleepWorkDays_bw, na.rm = TRUE)))
df2 <- sleep20 %>%
  group_by(age) %>%
  dplyr::summarize(Median = median(sleepWorkDays, na.rm = TRUE))
df <- left_join(df1, df2, by = "age")
colnames(df) <- c("Age_group","mean_sleep_durations_2017", "mean_sleep_durations_2020")
df <- df %>% mutate(difference = mean_sleep_durations_2020 - mean_sleep_durations_2017)
df <- df %>% mutate(Age_group_label = c("<20 years","20-24 years","25-29 years","30-34 years",
                                        "35-39 years", "40-44 years" ,"45-49 years", "50-54 years",
                                        "55-59 years" ,"60-64 years" ,"65-69 years"))
df %>% ggplot(aes(x = Age_group_label, y = difference)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + 
  ylab("Difference in mean sleep durations (2020 minus 2017)")

# Sleep duration changes per age category as in:
# Changes in sleep duration, timing, and variability during the COVID-19 pandemic: 
# Large-scale Fitbit data from 6 major US cities

# __Dinner-to-bedtime and sleep duration----
sleep17$dinner_WorkDays
sleep17[!is.na(sleepWorkDays_bw)]$bedTimeWorkDay_bw
strptime(sleep17[!is.na(sleepWorkDays_bw)]$bedTimeWorkDay_bw, format = "%H:%M")
# exact calculation not possible, dinner on workdays is given as categorical variable.

# __Differences in sleep duration stratified by chronotype?
mean(sleep17[chronotype=="definitely a \"morning\" type"]$sleepWorkDays_bw, na.rm = TRUE)
mean(sleep20[chronotype=="definitely a 'morning' type"]$sleepWorkDays, na.rm = TRUE)

mean(sleep17[chronotype=="rather more a \"morning\" than an \"evening\" type"]$sleepWorkDays_bw, na.rm = TRUE)
mean(sleep20[chronotype=="rather more a 'morning' than an 'evening' type"]$sleepWorkDays, na.rm = TRUE)

mean(sleep17[chronotype=="rather more an \"evening\" than a \"morning\" type"]$sleepWorkDays_bw, na.rm = TRUE)
mean(sleep20[chronotype=="rather more an 'evening' than a 'morning' type"]$sleepWorkDays, na.rm = TRUE)

mean(sleep17[chronotype=="definitely an \"evening\" type"]$sleepWorkDays_bw, na.rm = TRUE)
mean(sleep20[chronotype=="definitely an 'evening' type"]$sleepWorkDays, na.rm = TRUE)

# chronic insomnia longer sleep duration on workdays?
mean(sleep17[insomniaChron=="Chronic insomnia",]$sleepWorkDays_bw, na.rm = TRUE)
mean(sleep17[insomniaChron=="No chronic insomnia",]$sleepWorkDays_bw, na.rm = TRUE)
wilcox.test(sleep17[insomniaChron=="Chronic insomnia",]$sleepWorkDays_bw, 
            sleep17[insomniaChron=="No chronic insomnia",]$sleepWorkDays_bw, alternative = "less")

mean(sleep20[insomniaChron=="Chronic insomnia",]$sleepWorkDays, na.rm = TRUE)
mean(sleep20[insomniaChron=="No chronic insomnia",]$sleepWorkDays, na.rm = TRUE)
wilcox.test(sleep20[insomniaChron=="Chronic insomnia",]$sleepWorkDays, 
            sleep20[insomniaChron=="No chronic insomnia",]$sleepWorkDays, alternative = "less")

# __multiple testing (bonferroni-correction) for p-values used in text ----
# unemployed
p1 <- 0.005
# sleep time diff 2017/2020
p2 <- 2.2e-16
# sleep duration chronic insomnia vs no chronic insominia 2017
p3 <- 0.002358
# sleep duration chronic insomnia vs no chronic insominia 2020
p4 <- 0.7306


# sleep duration on free days:
mean(sleep17[!is.na(sleepWorkDays_bw),]$sleepFreeDays_bw, na.rm = TRUE)
mean(sleep20[!is.na(sleepWorkDays),]$sleepFreeDays, na.rm = TRUE)
wilcox.test(sleep17[!is.na(sleepWorkDays_bw),]$sleepFreeDays_bw, sleep20[!is.na(sleepWorkDays),]$sleepFreeDays, alternative = "greater")




# EDA for discussion ----

# Comparing correlation of min_sleep_required and self 
# reported sleep duration
# see: https://www.nature.com/articles/srep35812
sleep17 %>% ggplot(aes(x = min_sleep_required, y = sleepWorkDays_bw)) + 
  geom_point()
cor(sleep17$min_sleep_required, sleep17$sleepWorkDays_bw, use = "complete.obs")

sleep20 %>% ggplot(aes(x = min_sleep_required, y = sleepWorkDays)) + 
  geom_point()
cor(sleep20$min_sleep_required, sleep20$sleepWorkDays, use = "complete.obs")
