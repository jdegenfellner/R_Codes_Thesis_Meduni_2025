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

# Bootstrap replicates:---------
n_bootstraps_par_global <- 1000 # define number for all 3 analyses

pacman::p_load(
  haven, data.table, foreign, Hmisc, tidyverse, plyr, ggpubr, fancycut, 
  table1, tableone, readxl, DescTools, gtsummary, lmtest, rFSA, gt, flextable, 
  spatstat, plm, DataExplorer, olsrr, MASS, jtools, interactions, randomForest, 
  cNORM, limma, performance, car, ggstance, survey, tictoc, quantreg,YesSiR, 
  igraph, ggraph, tidygraph, forcats
)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# READ prepared and cleaned files-----------------------------------------------
sleep17 <- readRDS("./DATA/sleep17_an.RDS") 
sleep20 <- readRDS("./DATA/sleep20_an.RDS")

dim(sleep17) # 784
dim(sleep20) # 847

# sub working:
sleep17 <- sleep17[work != "not_currently_working",]
sleep20 <- sleep20[work != "not_currently_working",]

dim(sleep17) # 496
dim(sleep20) # 583

#  SURVEY WEIGHTS Age+Sex 2017-----------

# goal is to match the population distribution of WORKING people in Austria 2017/2020
# and the sample distribution of WORKING people in the sleep study 2017/2020.
# sleep17 contains only working people

# _Austrian WORKING population 2017; percent in age/sex groups-----
# from: WKO_2018-23-erwerbstaetigenquote-2017.pdf
df_erwerb_2017 <- tibble::tribble(
  ~age_group,        ~sex,       ~prozent_erw,
  "15–19",           "Männer",    36.2, # in our sample we just have "<20 years", hence, we cannot use this category!
  "15–19",           "Frauen",    27.3, # in our sample we just have "<20 years", hence, we cannot use this category!
  "20–24",           "Männer",    66.7,
  "20–24",           "Frauen",    65.2,
  "25–29",           "Männer",    79.2,
  "25–29",           "Frauen",    81.5,
  "30–34",           "Männer",    87.4,
  "30–34",           "Frauen",    80.4,
  "35–39",           "Männer",    89.4,
  "35–39",           "Frauen",    80.8,
  "40–44",           "Männer",    89.1,
  "40–44",           "Frauen",    81.7,
  "45–49",           "Männer",    88.9,
  "45–49",           "Frauen",    83.8,
  "50–54",           "Männer",    86.7,
  "50–54",           "Frauen",    80.3,
  "55–59",           "Männer",    77.5,
  "55–59",           "Frauen",    64.1,
  "60–64",           "Männer",    38.0,
  "60–64",           "Frauen",    17.2
)
df_erwerb_2017
df_erwerb_2017$sex <- case_when(
  df_erwerb_2017$sex == "Männer" ~ "Men",
  df_erwerb_2017$sex == "Frauen" ~ "Women"
)
df_erwerb_2017 <- df_erwerb_2017 %>% 
  dplyr::filter(age_group != "15–19") # drop <20 years, we do not have that information in our sample
df_erwerb_2017

# how many people are in the brackets?
bev2017_age_sex <- read_excel("./DATA/Bev_Alter_Geschl_Bundesl_seit_2011.xlsx", 
                              sheet = "2017_age_sex")
head(bev2017_age_sex)

# 1. Mapping: Alter zu Altersklassen (20–24, 25–29, ...)
bev2017_age_sex <- bev2017_age_sex %>%
  mutate(age_group = case_when(
    Alter_Jahre >= 20 & Alter_Jahre <= 24 ~ "20–24",
    Alter_Jahre >= 25 & Alter_Jahre <= 29 ~ "25–29",
    Alter_Jahre >= 30 & Alter_Jahre <= 34 ~ "30–34",
    Alter_Jahre >= 35 & Alter_Jahre <= 39 ~ "35–39",
    Alter_Jahre >= 40 & Alter_Jahre <= 44 ~ "40–44",
    Alter_Jahre >= 45 & Alter_Jahre <= 49 ~ "45–49",
    Alter_Jahre >= 50 & Alter_Jahre <= 54 ~ "50–54",
    Alter_Jahre >= 55 & Alter_Jahre <= 59 ~ "55–59",
    Alter_Jahre >= 60 & Alter_Jahre <= 64 ~ "60–64",
    TRUE ~ NA_character_
  ))

# 2. Filter nur auf die relevanten Altersgruppen (20–64)
bev2017_grouped <- bev2017_age_sex %>%
  filter(!is.na(age_group)) %>%
  mutate(gender = case_when(
    Geschlecht == "Mann" ~ "Men",
    Geschlecht == "Frau" ~ "Women",
    TRUE ~ NA_character_
  )) %>%
  group_by(age_group, gender) %>%
  dplyr::summarise(population_n = sum(Anzahl), .groups = "drop")
bev2017_grouped

df_comb <- cbind(df_erwerb_2017, bev2017_grouped$population_n)
colnames(df_comb)[4] <- "number_in_population"
colnames(df_comb)[2] <- "gender"
df_comb <- df_comb %>% mutate(working_in_age_group = prozent_erw * number_in_population / 100)
df_comb <- df_comb %>% mutate(proportion_tot_working_pop = working_in_age_group / sum(working_in_age_group))
str(df_comb)
df_comb <- df_comb %>%
  dplyr::mutate(
    age_group = factor(age_group, levels = c(
      "20–24", "25–29", "30–34", "35–39", "40–44",
      "45–49", "50–54", "55–59", "60–64"
    )),
    gender = factor(gender, levels = c("Men", "Women"))
  )
str(df_comb)
levels(df_comb$age_group) # correct
levels(df_comb$gender) # correct
# -> population level information!


# _2017 sleep sample----
levels(sleep17$Q1_cat)
table(sleep17$Q1_cat, sleep17$gender) # leave out "< 20 years"
sleep17 <- sleep17 %>% filter(Q1_cat != "<20 years")
sleep17$Q1_cat <- droplevels(sleep17$Q1_cat)
table(sleep17$Q1_cat, sleep17$gender) # perfect.
sample_proportions <- sleep17 %>%
  dplyr::group_by(Q1_cat, gender) %>%
  dplyr::summarize(Sample_Size = n(), .groups = "drop") %>%
  dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size)) %>%
  rename(age_group = Q1_cat)
sample_proportions
levels(sample_proportions$age_group) # correct
levels(sample_proportions$gender) # 
sample_proportions <- sample_proportions %>%
  mutate(gender = relevel(factor(gender), ref = "Men"))
levels(sample_proportions$gender) # correct
# note: number of participants in the age groups ARE equal to the number 
# of people working in the sample! 

# combine population level information and sample level information:
merged <- cbind(df_comb %>% dplyr::select(age_group, gender, 
                                          proportion_tot_working_pop, number_in_population), 
                sample_proportions$Sample_Proportion)
colnames(merged)[4] <- "Number_in_ref_Population"
colnames(merged)[5] <- "Sample_Proportion"
merged <- merged %>% mutate(weight = proportion_tot_working_pop / Sample_Proportion)
hist(merged$weight)
quantile(merged$weight, c(0.025, 0.975)) # 0.673819 2.231739, mild weighting compared to total sample
str(merged)
levels(merged$age_group) # correct
levels(merged$gender) # correct

# add weights to sleep17:
length(merged$weight) # 18
sleep17$weights <- dplyr::case_when(
  sleep17$Q1_cat == "20-24 years" & sleep17$gender == "Men"   ~ merged$weight[1],
  sleep17$Q1_cat == "20-24 years" & sleep17$gender == "Women" ~ merged$weight[2],
  sleep17$Q1_cat == "25-29 years" & sleep17$gender == "Men"   ~ merged$weight[3],
  sleep17$Q1_cat == "25-29 years" & sleep17$gender == "Women" ~ merged$weight[4],
  sleep17$Q1_cat == "30-34 years" & sleep17$gender == "Men"   ~ merged$weight[5],
  sleep17$Q1_cat == "30-34 years" & sleep17$gender == "Women" ~ merged$weight[6],
  sleep17$Q1_cat == "35-39 years" & sleep17$gender == "Men"   ~ merged$weight[7],
  sleep17$Q1_cat == "35-39 years" & sleep17$gender == "Women" ~ merged$weight[8],
  sleep17$Q1_cat == "40-44 years" & sleep17$gender == "Men"   ~ merged$weight[9],
  sleep17$Q1_cat == "40-44 years" & sleep17$gender == "Women" ~ merged$weight[10],
  sleep17$Q1_cat == "45-49 years" & sleep17$gender == "Men"   ~ merged$weight[11],
  sleep17$Q1_cat == "45-49 years" & sleep17$gender == "Women" ~ merged$weight[12],
  sleep17$Q1_cat == "50-54 years" & sleep17$gender == "Men"   ~ merged$weight[13],
  sleep17$Q1_cat == "50-54 years" & sleep17$gender == "Women" ~ merged$weight[14],
  sleep17$Q1_cat == "55-59 years" & sleep17$gender == "Men"   ~ merged$weight[15],
  sleep17$Q1_cat == "55-59 years" & sleep17$gender == "Women" ~ merged$weight[16],
  sleep17$Q1_cat == "60-64 years" & sleep17$gender == "Men"   ~ merged$weight[17],
  sleep17$Q1_cat == "60-64 years" & sleep17$gender == "Women" ~ merged$weight[18],
  TRUE ~ NA_real_
)
sleep17$weights # worked

# weights for BE, no bootstrapping:
# unelegant renaming
sleep17$Age_group_merged <- sleep17$Q1_cat
merged$Age_group_merged <- merged$age_group
sleep17 <- sleep17 %>%
  mutate(Age_group_merged = gsub("–", "-", Age_group_merged))
sleep17 <- sleep17 %>%
  mutate(Age_group_merged = gsub("–", "-", Age_group_merged),
         Age_group_merged = gsub(" years", "", Age_group_merged))

design_unweighted <- svydesign(ids = ~1, data = sleep17, weights = ~1)

post_strata <- merged %>%
  dplyr::select(Age_group_merged, gender, Number_in_ref_Population) %>% # must be named Freq
  dplyr::rename(Freq = Number_in_ref_Population)
post_strata  

post_strata <- post_strata %>%
  mutate(Age_group_merged = gsub("–", "-", Age_group_merged))

design_weighted <- postStratify(design_unweighted, ~Age_group_merged + gender, 
                                post_strata, partial = TRUE)

saveRDS(design_weighted, "./DATA/design_weighted_working_25.3.25.RDS")
weights(design_weighted)

# function to create survey design for bootstrap replicates 2017:
# merged contains then numbers in the reference population.

create_survey_design_2017 <- function(given_subset = sleep17, 
                                      ref_pop = merged){
  
  # Determine proportions of working people in given_subset
  # which is equal to the number of people in the age/sex groups
  # (pre-filtered for working)
  sample_proportions <- given_subset %>%
    dplyr::group_by(Age_group_merged, gender) %>%
    dplyr::summarize(Sample_Size = n(), .groups = "drop") %>%
    dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))
  
  # combine population level information and sample level information:
  df_comb_temp <- ref_pop %>%
    dplyr::select(Age_group_merged, gender, proportion_tot_working_pop) %>%
    dplyr::left_join(sample_proportions %>%
                       dplyr::select(Age_group_merged, gender, Sample_Proportion),
                     by = c("Age_group_merged", "gender"))
  colnames(df_comb_temp)[4] <- "Sample_Proportion"
  df_comb_temp <- df_comb_temp %>% 
    dplyr::mutate(weight = proportion_tot_working_pop / Sample_Proportion)
  
  # add weights to given_subset:
  given_subset$weights <- dplyr::case_when(
    given_subset$Age_group_merged == "20-24 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[1],
    given_subset$Age_group_merged == "20-24 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[2],
    given_subset$Age_group_merged == "25-29 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[3],
    given_subset$Age_group_merged == "25-29 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[4],
    given_subset$Age_group_merged == "30-34 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[5],
    given_subset$Age_group_merged == "30-34 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[6],
    given_subset$Age_group_merged == "35-39 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[7],
    given_subset$Age_group_merged == "35-39 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[8],
    given_subset$Age_group_merged == "40-44 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[9],
    given_subset$Age_group_merged == "40-44 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[10],
    given_subset$Age_group_merged == "45-49 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[11],
    given_subset$Age_group_merged == "45-49 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[12],
    given_subset$Age_group_merged == "50-54 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[13],
    given_subset$Age_group_merged == "50-54 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[14],
    given_subset$Age_group_merged == "55-59 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[15],
    given_subset$Age_group_merged == "55-59 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[16],
    given_subset$Age_group_merged == "60-64 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[17],
    given_subset$Age_group_merged == "60-64 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[18],
    TRUE ~ NA_real_
  )
  
  given_subset <- given_subset %>%
    dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged))
  
  design_unweighted <- svydesign(ids = ~1, data = given_subset, weights = ~1)
  
  post_strata <- ref_pop %>%
    dplyr::select(Age_group_merged, gender, Number_in_ref_Population) %>% # must be names Freq
    dplyr::rename(Freq = Number_in_ref_Population)
  
  post_strata <- post_strata %>%
    dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged))
  
  design_weighted <- postStratify(design_unweighted, ~Age_group_merged + gender,
                                  post_strata, partial = TRUE)
  
  return(design_weighted)
}
# test
design_weighted_test <- create_survey_design_2017(given_subset = sleep17,
                                                  ref_pop = merged)
head(weights(design_weighted_test))
head(weights(design_weighted))
# seems to work!


#  SURVEY WEIGHTS Age+Sex 2020-----------
# _Austrian population 2020-----
ref_pop_2020 <- read_excel("./DATA/EWT_2020.xlsx", sheet = 2) # got this from Statistik Austria via Email
head(ref_pop_2020)
ref_pop_2020$number_in_ref_pop_thousands
ref_pop_2020$number_working_in_ref_pop <- ref_pop_2020$number_in_ref_pop_thousands * 1000
head(ref_pop_2020)

# _2020 sleep sample----
levels(sleep20$Q1_cat)
table(sleep20$Q1_cat, sleep20$gender) # leave out "< 20 years", "65-69 years", "70 or older"
sleep20 <- sleep20 %>% filter(Q1_cat != "<20 years" & 
                                Q1_cat != "65-69 years" & 
                                Q1_cat != "70 or older") %>%
                       droplevels()
table(sleep20$Q1_cat, sleep20$gender) # perfect.

sleep20$Age_group_merged <- sleep20$Q1_cat # consistent naming
is.factor(sleep20$Age_group_merged)
levels(sleep20$Age_group_merged) # correct

sleep20$gender <- relevel(factor(sleep20$gender), ref = "Men")
levels(sleep20$gender) # correct

# Determine proportions of working people in given_subset
# which is equal to number of people in subset (prefiltered for working)

sample_proportions_2020 <- sleep20 %>%
  dplyr::group_by(Age_group_merged, gender) %>%
  dplyr::summarize(Sample_Size = n(), .groups = "drop") %>%
  dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size)) %>%
  dplyr::mutate(Age_group_merged = gsub(" years", "", Age_group_merged))
sample_proportions_2020

# combine population level information and sample level information:
# gsub for both:
ref_pop_2020 <- ref_pop_2020 %>%
  dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged),
                Age_group_merged = gsub(" years", "", Age_group_merged),
                Age_group_merged = gsub(" ", "", Age_group_merged))
sleep20 <- sleep20 %>%
  dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged),
                Age_group_merged = gsub(" years", "", Age_group_merged),
                Age_group_merged = gsub(" ", "", Age_group_merged))
  
# ref_pop_2020, create factors Age_group_merged and gender:
ref_pop_2020 <- ref_pop_2020 %>%
  dplyr::mutate(Age_group_merged = factor(Age_group_merged, levels = c(
    "20-24", "25-29", "30-34", "35-39", "40-44",
    "45-49", "50-54", "55-59", "60-64"
  )))
# factor for gender:
ref_pop_2020 <- ref_pop_2020 %>%
  dplyr::mutate(gender = factor(gender, levels = c("Men","Women")))
ref_pop_2020

df_comb_2020 <- left_join(ref_pop_2020, sample_proportions_2020, 
                          by = c("Age_group_merged", "gender"))
df_comb_2020

df_comb_2020 <- df_comb_2020 %>% 
  dplyr::mutate(prop_in_ref_pop = number_working_in_ref_pop / sum(number_working_in_ref_pop))

df_comb_2020 <- df_comb_2020 %>%
  dplyr::mutate(weight = prop_in_ref_pop / Sample_Proportion)

# add weights to sleep2020
sleep20$weights <- dplyr::case_when(
  sleep20$Age_group_merged == "20-24" & sleep20$gender == "Men"   ~ df_comb_2020$weight[1],
  sleep20$Age_group_merged == "20-24" & sleep20$gender == "Women" ~ df_comb_2020$weight[2],
  sleep20$Age_group_merged == "25-29" & sleep20$gender == "Men"   ~ df_comb_2020$weight[3],
  sleep20$Age_group_merged == "25-29" & sleep20$gender == "Women" ~ df_comb_2020$weight[4],
  sleep20$Age_group_merged == "30-34" & sleep20$gender == "Men"   ~ df_comb_2020$weight[5],
  sleep20$Age_group_merged == "30-34" & sleep20$gender == "Women" ~ df_comb_2020$weight[6],
  sleep20$Age_group_merged == "35-39" & sleep20$gender == "Men"   ~ df_comb_2020$weight[7],
  sleep20$Age_group_merged == "35-39" & sleep20$gender == "Women" ~ df_comb_2020$weight[8],
  sleep20$Age_group_merged == "40-44" & sleep20$gender == "Men"   ~ df_comb_2020$weight[9],
  sleep20$Age_group_merged == "40-44" & sleep20$gender == "Women" ~ df_comb_2020$weight[10],
  sleep20$Age_group_merged == "45-49" & sleep20$gender == "Men"   ~ df_comb_2020$weight[11],
  sleep20$Age_group_merged == "45-49" & sleep20$gender == "Women" ~ df_comb_2020$weight[12],
  sleep20$Age_group_merged == "50-54" & sleep20$gender == "Men"   ~ df_comb_2020$weight[13],
  sleep20$Age_group_merged == "50-54" & sleep20$gender == "Women" ~ df_comb_2020$weight[14],
  sleep20$Age_group_merged == "55-59" & sleep20$gender == "Men"   ~ df_comb_2020$weight[15],
  sleep20$Age_group_merged == "55-59" & sleep20$gender == "Women" ~ df_comb_2020$weight[16],
  sleep20$Age_group_merged == "60-64" & sleep20$gender == "Men"   ~ df_comb_2020$weight[17],
  sleep20$Age_group_merged == "60-64" & sleep20$gender == "Women" ~ df_comb_2020$weight[18],
  TRUE ~ NA_real_
)

# unweighted design
design_unweighted_2020 <- svydesign(ids = ~1, data = sleep20, weights = ~1)

# post-stratification
post_strata_2020 <- ref_pop_2020 %>%
  dplyr::select(Age_group_merged, gender, number_working_in_ref_pop) %>% # must be names Freq
  dplyr::rename(Freq = number_working_in_ref_pop)
post_strata_2020

design_weighted_2020 <- postStratify(design_unweighted_2020, ~Age_group_merged + gender,
                                     post_strata_2020, partial = TRUE)
weights(design_weighted_2020)



#saveRDS(design_weighted, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/CURRENT_R_OUTPUTS/design_weighted_working.RDS")
#saveRDS(design_weighted_2020, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/CURRENT_R_OUTPUTS/design_weighted_2020_working.RDS")

create_survey_design_2020 <- function(given_subset = sleep20, 
                                      ref_pop = ref_pop_2020){
  
  # Determine proportions of working people in given_subset
  # which is equal to the number of people in the age/sex groups
  # (pre-filtered for working)
  sample_proportions <- given_subset %>%
    dplyr::group_by(Age_group_merged, gender) %>%
    dplyr::summarize(Sample_Size = n(), .groups = "drop") %>%
    dplyr::mutate(Sample_Proportion = Sample_Size / sum(Sample_Size))
  
  # combine population level information and sample level information:
  df_comb_temp <- ref_pop %>%
    dplyr::select(Age_group_merged, gender, number_working_in_ref_pop) %>%
    dplyr::left_join(sample_proportions %>%
                       dplyr::select(Age_group_merged, gender, Sample_Proportion),
                     by = c("Age_group_merged", "gender")) %>%
    dplyr::mutate(prop_in_ref_pop = number_working_in_ref_pop / sum(number_working_in_ref_pop)) %>%
    dplyr::mutate(weight = prop_in_ref_pop / Sample_Proportion)
  
  # add weights to given_subset:
  given_subset$weights <- dplyr::case_when(
    given_subset$Age_group_merged == "20-24 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[1],
    given_subset$Age_group_merged == "20-24 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[2],
    given_subset$Age_group_merged == "25-29 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[3],
    given_subset$Age_group_merged == "25-29 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[4],
    given_subset$Age_group_merged == "30-34 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[5],
    given_subset$Age_group_merged == "30-34 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[6],
    given_subset$Age_group_merged == "35-39 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[7],
    given_subset$Age_group_merged == "35-39 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[8],
    given_subset$Age_group_merged == "40-44 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[9],
    given_subset$Age_group_merged == "40-44 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[10],
    given_subset$Age_group_merged == "45-49 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[11],
    given_subset$Age_group_merged == "45-49 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[12],
    given_subset$Age_group_merged == "50-54 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[13],
    given_subset$Age_group_merged == "50-54 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[14],
    given_subset$Age_group_merged == "55-59 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[15],
    given_subset$Age_group_merged == "55-59 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[16],
    given_subset$Age_group_merged == "60-64 years" & given_subset$gender == "Men"   ~ df_comb_temp$weight[17],
    given_subset$Age_group_merged == "60-64 years" & given_subset$gender == "Women" ~ df_comb_temp$weight[18],
    TRUE ~ NA_real_
  )
  
  given_subset <- given_subset %>%
    dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged))
  
  design_unweighted <- svydesign(ids = ~1, data = given_subset, weights = ~1)
  
  post_strata <- ref_pop %>%
    dplyr::select(Age_group_merged, gender, number_working_in_ref_pop) %>% # must be names Freq
    dplyr::rename(Freq = number_working_in_ref_pop)
  
  post_strata <- post_strata %>%
    dplyr::mutate(Age_group_merged = gsub("–", "-", Age_group_merged))
  
  design_weighted <- postStratify(design_unweighted, ~Age_group_merged + gender,
                                  post_strata, partial = TRUE)
  
  return(design_weighted)
}

# test 
design_weighted_2020_test <- create_survey_design_2020(given_subset = sleep20,
                                                      ref_pop = ref_pop_2020)
head(weights(design_weighted_2020_test))
head(weights(design_weighted_2020)) # seems to work

# SAVE ANALYTICAL data sets----------
saveRDS(sleep17, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/DATA/sleep17_an_working.RDS")
saveRDS(sleep20, "/Users/juergen/Library/Mobile Documents/com~apple~CloudDocs/1_DISSERTATION/Paper1-dissertation/DATA/sleep20_an_working.RDS")


# SUBSETS for analyses:-----------------------

# _sleep17_sub-----------
sleep17_sub <- sleep17 %>% dplyr::select(sleepWorkDays_bw,
                                         #age,        # age in CATEGORIES (in numbers)!
                                         Age_group_merged,
                                         gender,     # gender
                                         ethnicity,
                                         education,
                                         optimScore,
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
                                         ethnicity,
                                         education,
                                         optimScore,
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
                                         ethnicity,
                                         education,
                                         optimScore,
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

# _merge AGE levels to fit weights----------
#___2017----

# for RESULTS ------------
# ___weighted mean difference in sleep duration on workdays:----------
svymean(~sleepWorkDays_bw, design = design_weighted) # 7.0778 0.0577
svymean(~sleepWorkDays, design = design_weighted_2020) # 7.8024 0.0619
# t-test
plot_sleep_work <- rbind(
  sleep17[, .(sleep = sleepWorkDays_bw, Survey = "2017", weights = weights(design_weighted))],
  sleep20[, .(sleep = sleepWorkDays, Survey = "2020", weights = weights(design_weighted_2020))],ignore.attr=TRUE
)
design_combined <- svydesign(ids = ~1, data = plot_sleep_work, weights = ~weights)
hist(weights(design_combined))
svyttest(sleep ~ Survey, design_combined) # p-value = 0

svymean(~sleepWorkDays, design = design_weighted_2020) - 
  svymean(~sleepWorkDays_bw, design = design_weighted)
# 0.72463 0.0619 (identical to above t-test)
# minutes:
0.72463 * 60 # 43.4778 minutes

# ___weighted mean difference in sleep duration on free days:---------
svymean(~sleepFreeDays_bw, design = design_weighted) # 8.0899 0.1286
svymean(~sleepFreeDays, design = design_weighted_2020) # 8.7219 0.1459
# t-test
plot_sleep_free <- rbind(
  sleep17[, .(sleep = sleepFreeDays_bw, Survey = "2017", weights = weights(design_weighted))],
  sleep20[, .(sleep = sleepFreeDays, Survey = "2020", weights = weights(design_weighted_2020))]
)
design_combined <- svydesign(ids = ~1, data = plot_sleep_free, weights = ~weights)
svyttest(sleep ~ Survey, design_combined) # p-value = 0.05267
# 0.6319754 

svymean(~sleepFreeDays_bw, design = design_weighted) - # 7.99
svymean(~sleepFreeDays, design = design_weighted_2020) # 8.59
# -0.63198

# ___medians----------
svyquantile(~sleepFreeDays_bw, design = design_weighted, c(0.5)) # 8
svyquantile(~sleepFreeDays, design = design_weighted_2020, c(0.5)) # 8.25

# ___bootstrap median difference test:---------
n_boot <- 1000
median_differences <- numeric(n_boot)
for(i in 1:n_boot){
  
  choose_ind <- sample(1:nrow(sleep17), replace = TRUE)
  sleep17_sub_boot_weighted <- sleep17[choose_ind, ]
  design_2017_boot <- create_survey_design_2017(given_subset = sleep17_sub_boot_weighted,
                                           ref_pop = merged)
  
  choose_ind <- sample(1:nrow(sleep20), replace = TRUE)
  sleep20_sub_boot_weighted <- sleep20[choose_ind, ]
  design_2020_boot <- create_survey_design_2020(given_subset = sleep20_sub_boot_weighted,
                                           ref_pop = ref_pop_2020)
  
  median_differences[i] <- svyquantile(~sleepFreeDays_bw, design = design_2017_boot, c(0.5))$sleepFreeDays_bw[1] -
    svyquantile(~sleepFreeDays, design = design_2020_boot, c(0.5))$sleepFreeDays[1]
}
hist(median_differences, main = "Bootstrapped Median Differences", xlab = "Median Difference (2017 - 2020)")
quantile(median_differences, c(0.025, 0.975)) # -0.5   0.25 
barplot(table(median_differences), main = "Sign of Median Differences", xlab = "Sign of Median Difference (2017 - 2020)")




# 2017 BE________________________ ----

# ___FULL MODEL-----------
# ____Full model; unweighted------------
colnames(sleep17_sub)
model <- lm(sleepWorkDays_bw ~ ., data = sleep17_sub)
summary(model) # working: Multiple R-squared:  0.35,	Adjusted R-squared:   0.29
check_model(model) # not bad
qqPlot(model) # not bad

# potential interaction terms using RFA:
FSA_model <- lmFSA(sleepWorkDays_bw ~ ., data = sleep17_sub)
summary(FSA_model)

# ____full model; Age, sex weighting 2017---------
# full model, survey weighted:
model_weighted_age_sex <- lm(sleepWorkDays_bw ~ Age_group_merged +
                                 gender + ethnicity + education + optimScore +
                               marrital_status +     
                                 number_children + BMI + work + min_sleep_required + napping +       
                                 chronotype + diagn_sleep_disorder + insomniaChron +  
                                 alcohol_intake + caffeine_intake + dinner_WorkDays + 
                                 sport_yes_no + smoking + general_health, 
                               data = sleep17_sub,
                               weights = weights(design_weighted) ) # see above "SURVEY WEIGHTS Age+Sex 2017"

summary(model_weighted_age_sex) # ultiple R-squared:  0.39,	Adjusted R-squared:  0.34 
check_model(model_weighted_age_sex) # not so bad, PPC a bit high
qqPlot(model_weighted_age_sex) # good


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
# on the other hand:
cor(df_coefs$weighted_coefs, df_coefs$unweighted_coefs)  # 0.99
cor(df_coefs$weighted_coefs[-1], df_coefs$unweighted_coefs[-1]) # 0.97 without intercept



# ___BE (backward elimination) Models:----------

# ____BE: unweighted (AIC)-----
k <- ols_step_backward_aic(model, direction = "backward") # no weights
as.formula(k$model$terms)

# estimate coefficients for this BE model:
BE_model_2017 <- lm(as.formula(k$model$terms), x = TRUE, y = TRUE, 
                    data = sleep17_sub)
summary(BE_model_2017) # Multiple R-squared:  0.33,	Adjusted R-squared:  0.29
saveRDS(BE_model_2017, "./CURRENT_R_OUTPUTS/1_Results_with_ethnicity_education_optimScore/BE_model_2017_unweighted_subworking_4.4.25.RDS")
check_model(BE_model_2017) # not bad
qqPlot(BE_model_2017) # not bad


# ____BE: age- and sex weighted (AIC) (-> REPORT in paper):----------
k_w_sex_age <- stepAIC(model_weighted_age_sex, 
                       direction = "backward", trace = TRUE)
# extract terms:
labels(terms(as.formula(k_w_sex_age)))

# estimate coefficients for this model:
BE_model_2017_weighted <- lm(terms(as.formula(k_w_sex_age)), 
                             data = sleep17_sub,
                             weights = weights(design_weighted))
summary(BE_model_2017_weighted) # Multiple R-squared:  0.39,	Adjusted R-squared:  0.35
length(coef(BE_model_2017_weighted)) # 25
gtsummary::tbl_regression(BE_model_2017_weighted)
saveRDS(BE_model_2017_weighted, "./CURRENT_R_OUTPUTS/1_Results_with_ethnicity_education_optimScore/BE_model_2017_weighted_subworking_4.4.25.RDS")
check_model(BE_model_2017_weighted) # not so bad

# Difference in number of predictors full model vs BE model?
length(coef(model_weighted_age_sex)) # 41
length(coef(BE_model_2017_weighted)) # 28




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
  model_boot <- lm(sleepWorkDays_bw ~ Age_group_merged + gender + 
                     ethnicity + education + optimScore +
                     marrital_status +     
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
  design_2017_boot <- create_survey_design_2017(given_subset = sleep17_sub_boot_weighted,
                                           ref_pop = merged)
  model_boot_w <- lm(sleepWorkDays_bw ~ Age_group_merged + gender + 
                       ethnicity + education + optimScore +
                       marrital_status +     
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
toc() # 1200s sec elapsed; 4.4.25

# _____a) bootstrap results for unweighted-------------
# ...

# _____b) bootstrap results for weighted-------------
pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)
hist(adj.r.squared_w_as)
quantile(adj.r.squared_w_as, p = c(0.025, 0.975)) # 0.31 0.50

# ______Model selection frequencies (analog Table 6 Heinze): ----
pred_boot_df_w_as <- pred_boot_df_w_as %>% 
  dplyr::count(sort(Predictors)) %>% 
  dplyr::arrange(desc(n)) %>%
  top_n(20) %>% 
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as
saveRDS(pred_boot_df_w_as, "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/1_Results_with_ethnicity_education_optimScore/2017_weighted/pred_boot_df_2017_w_as_subworking_4.4.25.RDS")
getwd()
#pred_boot_df_w_as <- readRDS("./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2017_w_as_19.3.25.RDS")
#pred_boot_df_w_as

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors") # 6-19 predictors

#______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$coefficients)
  coefs <- model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}
coefs_bootstrap_w_as

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
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/1_Results_with_ethnicity_education_optimScore/2017_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2017_w_as_subworking_4.4.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2017_w_as_subworking_4.4.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2017_w_as__subworking_4.4.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_boot_2017_weighted__subworking_4.4.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2017_weighted__subworking_4.4.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_2020_w_as__subworking_4.4.25.RDS"))
saveRDS(incl_freq_2017_w_as, paste0(base_dir, "incl_freq_2017_weighted__subworking_4.4.25.RDS"))

save_as_html(flextable(incl_freq_2017_w_as) %>%
               set_caption("2017 inclusion frequencies (age and sex) weighted"), 
             path = "./RESULTS/1_Results_with_ethnicity_education_optimScore/Table_3_inclusion_frequencies_2017_weighted_subworking_4.4.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2017_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2017_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2017_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2017_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_boot_2017_weighted_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2017_weighted_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
# incl_freq_2017_w_as <- readRDS(paste0(base_dir, "incl_freq_2017_weighted_20.3.25.RDS"))




# 2020 BE (without COVID)___________ ----

# ___FULL MODEL 2020---------

# ____full model; unweighted-----------
# setdiff(colnames(sleep17_sub), colnames(sleep20_sub))
model_full_2020 <- lm(sleepWorkDays ~ ., data = sleep20_sub) 
summary(model_full_2020) # Multiple R-squared:  0.23,	Adjusted R-squared:  0.17
check_model(model_full_2020) # residuals and PPC, worse model fit compared to 2017

# ____full model; weighted-----------
model_weighted_age_sex_2020 <- lm(sleepWorkDays ~ Age_group_merged +
                                   gender + 
                                    ethnicity + education + optimScore + 
                                    marrital_status +     
                                   number_children + BMI + work + min_sleep_required + napping +       
                                   chronotype + diagn_sleep_disorder + insomniaChron +  
                                   alcohol_intake + caffeine_intake + dinner_WorkDays + 
                                   sport_yes_no + smoking + general_health, 
                                  data = sleep20_sub,
                                 weights = weights(design_weighted_2020)) # see above "SURVEY WEIGHTS Age+Sex 2017"
summary(model_weighted_age_sex_2020) # Multiple R-squared:  0.23,	Adjusted R-squared:  0.17, not better
check_model(model_weighted_age_sex_2020) # bad PPC and residuals


# potential interaction terms using RFA:
FSA_model <- lmFSA(sleepWorkDays ~ ., data = sleep20_sub)
summary(FSA_model)


# ___BE Models:-----------

# ____BE: unweighed (AIC)----------
k <- ols_step_backward_aic(model_full_2020, direction = "backward")
BE_model_2020 <- lm(as.formula(k$model$terms), x=TRUE, y=TRUE, data=sleep20_sub)
summary(BE_model_2020) # Adjusted R-squared:  0.1683
saveRDS(BE_model_2020, "./CURRENT_R_OUTPUTS/1_Results_with_ethnicity_education_optimScore/BE_model_2020_working_4.4.25.RDS")


# ____BE: weighted age and sex (AIC)---------
k_w_as_20 <- stepAIC(model_weighted_age_sex_2020, 
                     direction = "backward", trace = FALSE)
as.formula(terms(k_w_as_20$model))
# estimate coefficients of this model:
BE_model_2020_w_as <- lm(as.formula(terms(k_w_as_20$model)), 
                             data = sleep20_sub, 
                             weights = weights(design_weighted_2020))
summary(BE_model_2020_w_as) # Multiple R-squared:  0.1875,	Adjusted R-squared:  0.1672
length(coef(BE_model_2020_w_as)) # 15
check_model(BE_model_2020_w_as) # bad PPC and residuals
qqPlot(BE_model_2020_w_as) # not so normal
gtsummary::tbl_regression(BE_model_2020_w_as) 
saveRDS(BE_model_2020_w_as, "./CURRENT_R_OUTPUTS/1_Results_with_ethnicity_education_optimScore/BE_model_2020_weighted_working_24.4.25.RDS")


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
cor(df_coefs_2020$weighted_coefs, df_coefs_2020$unweighted_coefs)  # 0.98 with Intercept
cor(df_coefs_2020$weighted_coefs[-1], df_coefs_2020$unweighted_coefs[-1])  # 0.91


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
  model_boot <- lm(sleepWorkDays ~ Age_group_merged + gender + 
                     ethnicity + education + optimScore +
                     marrital_status +     
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
  design_2020_boot <- create_survey_design_2020(given_subset = sleep20_sub_boot_weighted,
                                           ref_pop = ref_pop_2020)
  model_boot_w <- lm(sleepWorkDays ~ Age_group_merged + gender + 
                       ethnicity + education + optimScore +
                       marrital_status +     
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
toc() # 1000 s/20min

# # _____a) bootstrap results for unweighted-----------
# ...

# _____b) bootstrap results for weighted-------------
pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)
hist(adj.r.squared_w_as)
quantile(adj.r.squared_w_as, p = c(0.025, 0.975)) # 0.15 0.34

# ______Model selection frequencies (analog Table 6 Heinze): ----
pred_boot_df_w_as <- pred_boot_df_w_as %>% 
  dplyr::count(sort(Predictors)) %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(20) %>% 
  dplyr::mutate(Percent = n/n_boot*100) %>%
  dplyr::mutate(Cumulative_Percent = cumsum(Percent))
pred_boot_df_w_as
#saveRDS(pred_boot_df_w_as, "./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2020_w_as_19.3.25.RDS")

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors")
# 4-17 predictors 

#______Stability of coefficients: ----
coefs_bootstrap_w_as <- data.frame()
for(i in 1:n_boot){
  model <-  boot_strap_models_list_w_as[[i]]
  coef_names <- names(model$coefficients)
  coefs <- model$coefficients
  coefs_bootstrap_w_as <- rbind(coefs_bootstrap_w_as, data.frame(coef_names, coefs))
}
coefs_bootstrap_w_as

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
incl_freq_2020_w_as$Variables <- gsub("education", "Education", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("optimScore", "Optimism", incl_freq_2020_w_as$Variables)
incl_freq_2020_w_as$Variables <- gsub("ethnicity", "Ethnicity", incl_freq_2020_w_as$Variables)



flextable(incl_freq_2020_w_as) %>%
  set_caption("2020 Inclusion frequencies weighted")


# ___SAVE all weighted vectors into ./RESULTS/z_BOOTSTRAP_results/2020_weighted/:--------
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/1_Results_with_ethnicity_education_optimScore/2020_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2020_w_as_working_4.4.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2020_w_as_working_4.4.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2020_w_as_working_4.4.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_boot_2020_weight_working_4.4.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2020_weighted_working_4.4.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_w_as_working_4.4.25.RDS"))
saveRDS(incl_freq_2020_w_as, paste0(base_dir, "incl_freq_2020_weighted_age_sex_working_4.4.25.RDS"))

save_as_html(flextable(incl_freq_2020_w_as) %>%
               set_caption("2020 inclusion frequencies (age and sex) weighted"), 
             path = "./RESULTS/1_Results_with_ethnicity_education_optimScore/Table_3_inclusion_frequencies_2020_weighted_working_4.4.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_boot_2020_weight_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2020_weighted_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_w_as_20.3.25.RDS"))
# incl_freq_2020_w_as <- readRDS(paste0(base_dir, "incl_freq_2020_weighted_age_sex_20.3.25.RDS"))




# 2020 BE (with COVID)___________ ----

# ___FULL MODEL---------

# ____full model, unweighted -------------
colnames(sleep20_sub_inkl_COVID)
model <- lm(sleepWorkDays ~ ., data = sleep20_sub_inkl_COVID) 

# ____full model, weighted-------------
model_weighted <- lm(sleepWorkDays ~ ., 
                     data = sleep20_sub_inkl_COVID,
                     weights = weights(design_weighted_2020))
summary(model_weighted) # Multiple R-squared:  0.25,	Adjusted R-squared:  0.17
check_model(model_weighted) # PPC and residuals suboptimal

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
summary(BE_model_2020_with_COVID) # Adjusted R-squared:  0.17


#____BE: weighted (AIC)----------
BE_model_2020_with_COVID_weighted <- stepAIC(model_weighted, 
                                             direction = "backward", 
                                             trace = FALSE)
summary(BE_model_2020_with_COVID_weighted) # Multiple R-squared:  0.2189,	Adjusted R-squared:  0.1834 
check_model(BE_model_2020_with_COVID_weighted) # PPC and residuals suboptimal

saveRDS(BE_model_2020_with_COVID_weighted, file = "./CURRENT_R_OUTPUTS/1_Results_with_ethnicity_education_optimScore/BE_model_2020_with_COVID_weighted_working_4.4.25.rds")


# ___+ BOOTSTRAP ------
# _____Bootstrap resampling----

set.seed(10325) 
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
  design_2020_boot <- create_survey_design_2020(given_subset = sleep20_sub_boot_weighted,
                                           ref_pop = ref_pop_2020)
  model_boot_w <- lm(sleepWorkDays ~ Age_group_merged + gender + 
                       ethnicity + education + optimScore +
                       marrital_status + 
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
toc() # 3500 sec elapsed

pred_boot_df_w_as <- data.frame(Predictors = predictors_boot_w_as)
variables_used_list_sorted_w_as <- lapply(variables_used_list_w_as, sort)

# # _____a) bootstrap results for unweighted-----------
# 
...

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
#saveRDS(pred_boot_df_w_as, "./RESULTS/z_BOOTSTRAP_results/pred_boot_df_2020_w_as_working_19.3.25.RDS")

# ______Model sizes overview (number of predictors): ----
plot(table(lengths(variables_used_list_sorted_w_as)), xlab = "number of predictors") # 4-17

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
incl_freq_2020$Variables <- gsub("ethnicity", "Ethnicity", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("education", "Education", incl_freq_2020$Variables)
incl_freq_2020$Variables <- gsub("optimScore", "Optimism", incl_freq_2020$Variables)

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
coefs_bootstrap_w_as

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
base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/1_Results_with_ethnicity_education_optimScore/2020_incl_COVID_weighted/"
saveRDS(pred_boot_df_w_as, paste0(base_dir, "pred_boot_df_2020_w_as_working_5.4.25.RDS"))
saveRDS(variables_used_list_sorted_w_as, paste0(base_dir, "variables_used_list_sorted_2020_w_as_working_5.4.25.RDS"))
saveRDS(boot_strap_models_list_w_as, paste0(base_dir, "boot_strap_models_list_2020_w_as_working_5.4.25.RDS"))
saveRDS(count_VIF_w_as, paste0(base_dir, "count_VIF_2020_w_as_working_5.4.25.RDS"))
saveRDS(adj.r.squared_w_as, paste0(base_dir, "adj.r.squared_2020_w_as_working_5.4.25.RDS"))
saveRDS(coefs_bootstrap_w_as, paste0(base_dir, "coefs_bootstrap_2020_w_as_working_5.4.25.RDS"))
saveRDS(incl_freq_2020, paste0(base_dir, "incl_freq_2020_weighted_incl_COVID_working_5.4.25.RDS"))

save_as_html(flextable(incl_freq_2020) %>%
               set_caption("2020 incl COVID inclusion frequencies (age and sex) weighted (sub working)"),
             path = "./RESULTS/1_Results_with_ethnicity_education_optimScore/Table_3_inclusion_frequencies_2020_incl_COVID_weighted_working_5.4.25.html")

# READ:
# base_dir <- "/Users/juergen/Large_R_Files/1_Paper1/z_BOOTSTRAP_results/2020_incl_COVID_weighted/"
# pred_boot_df_w_as <- readRDS(paste0(base_dir, "pred_boot_df_2020_w_as_20.3.25.RDS"))
# variables_used_list_sorted_w_as <- readRDS(paste0(base_dir, "variables_used_list_sorted_2020_w_as_20.3.25.RDS"))
# boot_strap_models_list_w_as <- readRDS(paste0(base_dir, "boot_strap_models_list_2020_w_as_20.3.25.RDS"))
# count_VIF_w_as <- readRDS(paste0(base_dir, "count_VIF_2020_w_as_20.3.25.RDS"))
# adj.r.squared_w_as <- readRDS(paste0(base_dir, "adj.r.squared_2020_w_as_20.3.25.RDS"))
# coefs_bootstrap_w_as <- readRDS(paste0(base_dir, "coefs_bootstrap_2020_w_as_20.3.25.RDS"))
# incl_freq_2020 <- readRDS(paste0(base_dir, "incl_freq_2020_weighted_incl_COVID_20.3.25.RDS"))



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
