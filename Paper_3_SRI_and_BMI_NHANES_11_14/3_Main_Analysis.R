Sys.setenv(TMPDIR = "/Users/juergen/Large_R_Files")

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
mem.maxVSize(vsize = Inf) # otherwise R crashes

(current_directory <- getwd())
#file.exists("./1_Packages_install_load.R")
source(paste0(current_directory, "/1_Packages_install_load.R"))

# Functions----
get_factor_reference_levels <- function(df) {
  # Identify the factor columns
  factor_cols <- sapply(df, is.factor)
  
  # Create a data frame for variable names and their reference levels
  factor_reference_df <- data.frame(
    Variable = names(df)[factor_cols],
    Reference_Level = sapply(df[, ..factor_cols], function(x) levels(x)[1])
  )
  
  return(factor_reference_df)
}


# NHANES orig. Questionnaires--------
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/questionnaires.aspx?BeginYear=2011

# 1) READ combined data set ----
# Read combined data set from 3_Main_Analysis...----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
df <- readRDS(paste0(parent_directory, "/4_DATA/Combined_data_sets_2_waves.RDS"))
str(df)
dim(df) # 19931 673 check
#length(unique(df_PAX1112$SEQN)) # 6917
#length(unique(df_PAX1314$SEQN)) # 7776


# 2) (2011-14) FLOWCHART for inclusion of study participants--------
# https://github.com/bruigtp/flowchart

# _Files for Flowchart-----
# __READ filtered PAX files-----
tic()
results_1112 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1112_filtered_not_imputed.RDS")
toc() # 30s
df_PAX1112_filtered <- results_1112$filtered_data
tic()
results_1314 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1314_filtered_not_imputed.RDS")
toc() # 28s
df_PAX1314_filtered <- results_1314$filtered_data
df_PAX_filtered <- rbind(df_PAX1112_filtered, df_PAX1314_filtered)

# __READ raw accelerometer data, combine both waves:---------
df_PAX1112 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2011_12.RDS")
df_PAX1314 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2013_14.RDS")
#df_PAX <- rbind(df_PAX1112, df_PAX1314)
#df_PAX1112_age_20 <- df_PAX1112 %>% 
#  dplyr::filter(SEQN %in% df$SEQN[df$RIDAGEYR >= 20])
#df_PAX1314_age_20 <- df_PAX1314 %>%
#  dplyr::filter(SEQN %in% df$SEQN[df$RIDAGEYR >= 20])


# __df_header files-------
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "PAXHD_G_2011_12.XPT")
df1112_header <- read_xpt(file_path)
file_path <- file.path(parent_directory, "4_DATA", "PAXHD_H_2013_14.XPT")
df1314_header <- read_xpt(file_path)
df1112_header
df_header <- rbind(df1112_header, df1314_header)

stop()

# NEW flowchart:--------------
filter_accelo_data_NHANES_11_to_14 <- function(df_accel, df_header, df_non_accel) {
  
  # Step 0: Define binary_sleep variable
  df_accel <- df_accel %>%
    dplyr::mutate(binary_sleep = case_when(
      PAXPREDM == "sleep" ~ 1,
      PAXPREDM == "wake" ~ 0,
      TRUE ~ NA_real_
    ))
  
  cat("Number of participants before filtering (overall): ", 
      length(unique(df_non_accel$SEQN)), "\n")
  
  cat("Number of participants before filtering (NHANES 2011-12): ", 
      length(unique(df_non_accel$SEQN[df_non_accel$Wave == "NHANES_1112"])), "\n")
  
  cat("Number of participants before filtering (NHANES 2013-14): ",
      length(unique(df_non_accel$SEQN[df_non_accel$Wave == "NHANES_1314"])), "\n")
  
  # Step 1: Filter participants with RIDAGEYR >= 20
  SEQN_before_age_filter <- unique(df_non_accel$SEQN)
  df_non_accel <- df_non_accel %>%
    dplyr::filter(RIDAGEYR >= 20)
  df_accel <- df_accel %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  SEQN_after_age_filter <- unique(df_non_accel$SEQN)
  SEQN_excluded_by_age_filter <- setdiff(SEQN_before_age_filter, SEQN_after_age_filter)
  cat("Number participants after age filter: ", length(unique(SEQN_after_age_filter)), "\n")
  
  # Step 2: Filter participants with PAM file available (PAXSTS == 1)
  SEQN_before_pam_filter <- SEQN_after_age_filter
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% df_header$SEQN[df_header$PAXSTS == 1])
  df_accel <- df_accel %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  SEQN_after_pam_filter <- unique(df_non_accel$SEQN)
  SEQN_excluded_by_pam_filter <- setdiff(SEQN_before_pam_filter, SEQN_after_pam_filter)
  cat("Number participants after PAM filter: ", length(unique(SEQN_after_pam_filter)), "\n")
  
  # Step 3: Filter accelerometer data for 60s observations
  SEQN_before_60s_filter <- SEQN_after_pam_filter
  df_accel <- df_accel %>%
    dplyr::filter(PAXTSM == 60)
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  SEQN_after_60s_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_60s_filter <- setdiff(SEQN_before_60s_filter, SEQN_after_60s_filter)
  cat("Number participants after 60s filter: ", length(unique(SEQN_after_60s_filter)), "\n")
  
  # Step 4: Filter data quality (PAXQFM == 0)
  SEQN_before_quality_filter <- SEQN_after_60s_filter
  df_accel <- df_accel %>%
    dplyr::filter(PAXQFM == 0)
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  SEQN_after_quality_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_quality_filter <- setdiff(SEQN_before_quality_filter, SEQN_after_quality_filter)
  cat("Number participants after quality filter: ", length(unique(SEQN_after_quality_filter)), "\n")
  
  # Step 5: Filter participants with <2 hours non-wear time for at least 3 days
  SEQN_before_non_wear_filter <- SEQN_after_quality_filter
  valid_non_wear_time_days <- df_accel %>%
    dplyr::group_by(SEQN, PAXDAYM) %>%
    dplyr::summarise(non_wear_time = sum(PAXPREDM == "non-wear"), .groups = 'drop') %>%
    dplyr::filter(non_wear_time < 2 * 60) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarise(valid_non_wear_days = n_distinct(PAXDAYM), .groups = 'drop') %>%
    dplyr::filter(valid_non_wear_days >= 3)
  df_accel <- df_accel %>%
    dplyr::filter(SEQN %in% valid_non_wear_time_days$SEQN)
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% valid_non_wear_time_days$SEQN)
  df_header <- df_header %>% 
    dplyr::filter(SEQN %in% valid_non_wear_time_days$SEQN)
  SEQN_after_non_wear_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_non_wear_filter <- setdiff(SEQN_before_non_wear_filter, SEQN_after_non_wear_filter)
  cat("Number participants after non-wear filter: ", length(unique(SEQN_after_non_wear_filter)), "\n")
  
  # Step 6: Filter participants with 1440 timesteps per day
  SEQN_before_timesteps_filter <- SEQN_after_non_wear_filter
  df_accel <- df_accel %>%
    dplyr::group_by(SEQN, PAXDAYM) %>%
    dplyr::mutate(time_steps_for_PAXDAYM = n()) %>%
    dplyr::filter(time_steps_for_PAXDAYM == 1440) %>%
    dplyr::ungroup()
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  SEQN_after_timesteps_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_timesteps_filter <- setdiff(SEQN_before_timesteps_filter, SEQN_after_timesteps_filter)
  cat("Number participants after timesteps filter: ", length(unique(SEQN_after_timesteps_filter)), "\n")
  
  # Step 7: Filter participants with at least 3 consecutive days including a Saturday or Sunday
  SEQN_before_consecutive_days_filter <- SEQN_after_timesteps_filter
  df_accel <- df_accel %>%
    dplyr::group_by(SEQN) %>%
    dplyr::filter({
      day_seq <- sort(as.numeric(unique(PAXDAYWM)))
      if (sum(!is.na(day_seq)) < 3) {
        consecutive <- FALSE
      } else {
        consecutive <- max(rle(diff(day_seq))$lengths, na.rm = TRUE) >= 2
      }
      weekend_day <- any(day_seq %in% c(1, 7), na.rm = TRUE)
      consecutive && weekend_day
    }) %>%
    dplyr::ungroup()
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  SEQN_after_consecutive_days_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_consecutive_days_filter <- setdiff(SEQN_before_consecutive_days_filter, SEQN_after_consecutive_days_filter)
  cat("Number participants after consecutive days filter: ", length(unique(SEQN_after_consecutive_days_filter)), "\n")
  
  # Step 8: Filter participants with missingness rate <30%
  SEQN_before_missingness_filter <- SEQN_after_consecutive_days_filter
  missingness <- df_accel %>%
    dplyr::group_by(SEQN) %>%
    dplyr::summarise(
      total_observations = n(),
      missing_count = sum(is.na(binary_sleep)),
      missingness_rate = missing_count / total_observations
    ) %>%
    dplyr::ungroup()
  df_accel <- df_accel %>%
    dplyr::filter(SEQN %in% missingness$SEQN[missingness$missingness_rate < 0.3])
  df_non_accel <- df_non_accel %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% unique(df_accel$SEQN))
  SEQN_after_missingness_filter <- unique(df_accel$SEQN)
  SEQN_excluded_by_missingness_filter <- setdiff(SEQN_before_missingness_filter, SEQN_after_missingness_filter)
  cat("Number participants after missingness filter: ", length(unique(SEQN_after_missingness_filter)), "\n")
  
  # Step 8: SRI_no_imputation not NA in df_non_accel:
  SEQN_before_SRI_sleep_filter <- SEQN_after_missingness_filter
  df_non_accel <- df_non_accel %>%
    dplyr::filter(!is.na(SRI_no_imputation))
  df_accel <- df_accel %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  df_header <- df_header %>%
    dplyr::filter(SEQN %in% df_non_accel$SEQN)
  SEQN_after_SRI_sleep_filter <- unique(df_non_accel$SEQN)
  SEQN_excluded_by_SRI_sleep_filter <- setdiff(SEQN_before_SRI_sleep_filter, SEQN_after_SRI_sleep_filter)
  cat("Number participants after SRI_no_impution filter: ", length(unique(SEQN_after_SRI_sleep_filter)), "\n")
  
}

# Filter NHANES 2011-12 accelerometer data
tic()
filtered_data_NHANES_11_to_12 <- filter_accelo_data_NHANES_11_to_14(df_PAX1112, 
                                                                    df1112_header, df)
toc() # 70s

# Number of participants before filtering (overall):  19931 
# Number of participants before filtering (NHANES 2011-12):  9756 
# Number of participants before filtering (NHANES 2013-14):  10175 
# Number participants after age filter:  11329 
# Number participants after PAM filter:  4664 
# Number participants after 60s filter:  4664 
# Number participants after quality filter:  4664 
# Number participants after non-wear filter:  4307 
# Number participants after timesteps filter:  4299 
# Number participants after consecutive days filter:  4011 
# Number participants after missingness filter:  3879 
# Number participants after SRI_no_impution filter:  3677 


# Filter NHANES 2013-14 accelerometer data
tic()
filtered_data_NHANES_13_to_14 <- filter_accelo_data_NHANES_11_to_14(df_PAX1314, 
                                                                    df1314_header, df)
toc() # 80s 

# Number of participants before filtering (overall):  19931 
# Number of participants before filtering (NHANES 2011-12):  9756 
# Number of participants before filtering (NHANES 2013-14):  10175 
# Number participants after age filter:  11329 
# Number participants after PAM filter:  4840 
# Number participants after 60s filter:  4840 
# Number participants after quality filter:  4838 
# Number participants after non-wear filter:  4365 
# Number participants after timesteps filter:  4352 
# Number participants after consecutive days filter:  4075 
# Number participants after missingness filter:  3916 
# Number participants after SRI_no_impution filter:  3408 


# CHECK:
# age:
19931 - 11329 # 8602 # OK
# pam:
4664 + 4840 # 9504 # OK

# Accelo quality filters:
# _60s:
4664 + 4840 # 9504 # OK
# _quality:
4664 + 4838 # 9502 # DIFF: 2
# _non_wear:
4307 + 4365 # 8672 # DIFF: 830
# _timesteps:
4299 + 4352 # 8651 # DIFF: 21
# _consecutive_days:
4011 + 4075 # 8086 # DIFF: 565
# _missingness:
3879 + 3916 # 7795 # DIFF: 291

# SRI_no_imputation:
3677 + 3408 # 7085 # OK


# _Flowchart NHANES 11-14------------------
as_fc(N = 19931, label = "NHANES 2011-2014 Public release:") |>
  
  # Step 1: Age filter
  fc_filter(N = 11329, label = "Age ≥ 20 years:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: <20 years old") |>
  
  # Step 2: PAM file filter
  fc_filter(N = 9504, label = "Physical Activity Monitor (PAM) file available:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: No PAM file available") |>
  
  # Step 3: Accelerometer data quality filters
  fc_filter(N = 7795, label = "Quality-filtered accelerometer data:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = paste0(
              "Excluded:\n",
              " 2 due to quality flag issues\n",
              " 0 due to 60s per minute requirement\n",
              " 830 due to non-wear time >2 hours/day\n",
              " 21 due to <1,440 timesteps per day\n",
              " 565 due to missing consecutive days incl. a weekend day\n",
              " 291 due >30% missingness for binary sleep variable within participant")) |>
  
  # Step 4: Missingness filter for SRI
  fc_filter(N = 7085, label = "SRI assessable: both sleep/wake values available 24h apart:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: Missing at least one sleep/wake state 24h apart") |>
  
  # Draw the flowchart
  fc_draw()

2+0+830+21+565+291 # 1709 OK


# _Flowchart NHANES 11-12------------------
as_fc(N = 9756, label = "NHANES 2011-12 Public release:") |>
  
  # Step 1: Age filter
  fc_filter(N = 5564, label = "Age ≥ 20 years:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: <20 years old") |>
  
  # Step 2: PAM file filter
  fc_filter(N = 4664, label = "Physical Activity Monitor (PAM) file available:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: No PAM file available") |>
  
  # Step 3: Accelerometer data quality filters
  fc_filter(N = 3879, label = "Quality-filtered accelerometer data:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = paste0(
              "Excluded:\n",
              " 0 due to quality flag issues\n",
              " 0 due to 60s per minute requirement\n",
              " 357 due to non-wear time >2 hours/day\n",
              " 8 due to <1,440 timesteps per day\n",
              " 288 due to missing consecutive days incl. a weekend day\n",
              " 132 due >30% missingness for binary sleep variable within participant")) |>
  
  # Step 4: Missingness filter for SRI
  fc_filter(N = 3677, label = "SRI assessable: both sleep/wake values available 24h apart:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: Missing at least one sleep/wake state 24h apart") |>
  
  # Draw the flowchart
  fc_draw()

# _Flowchart NHANES 13-14------------------
as_fc(N = 10175, label = "NHANES 2013-14 Public release:") |>
  
  # Step 1: Age filter
  fc_filter(N = 5765, label = "Age ≥ 20 years:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: <20 years old") |>
  
  # Step 2: PAM file filter
  fc_filter(N = 4840, label = "Physical Activity Monitor (PAM) file available:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: No PAM file available") |>
  
  # Step 3: Accelerometer data quality filters
  fc_filter(N = 3916, label = "Quality-filtered accelerometer data:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = paste0(
              "Excluded:\n",
              " 2 due to quality flag issues\n",
              " 0 due to 60s per minute requirement\n",
              " 473 due to non-wear time >2 hours/day\n",
              " 13 due to <1,440 timesteps per day\n",
              " 277 due to missing consecutive days incl. a weekend day\n",
              " 159 due >30% missingness for binary sleep variable within participant")) |>
  
  # Step 4: Missingness filter for SRI
  fc_filter(N = 3408, label = "SRI assessable: both sleep/wake values available 24h apart:", 
            show_exc = TRUE, 
            text_pattern = "{label}\n {n}",
            text_pattern_exc = "{label}\n {n}",
            label_exc = "Excluded: Missing at least one sleep/wake state 24h apart") |>
  
  # Draw the flowchart
  fc_draw()

# __SUM checks:--------
9756+10175
# 19931
4192+4410
# 8602
900+925
# 1825
785+924
# 1709
202+508
# 710
3677+3408
# 7085

0+0+357+8+288+132
# 785
2+0+473+13+277+159
# 924


# 3) DEFINE Analytic set--------
SEQN_in_filtered_PAX_files11_to_14 <- unique(c(df_PAX1112_filtered$SEQN, df_PAX1314_filtered$SEQN))
df_an <- df %>%
  dplyr::filter(SEQN %in% df_header$SEQN[df_header$PAXSTS == 1]) %>%
  dplyr::filter(SEQN %in% SEQN_in_filtered_PAX_files11_to_14) %>%
  dplyr::filter(!is.na(SRI_no_imputation)) %>%
  dplyr::filter(RIDAGEYR >= 20) # 20 is given by NHANES, at least by some variables and also used in other NHANES studies
dim(df_an) # 7085 506/673 check

# _SAVE analytic data set----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
saveRDS(df_an, paste0(parent_directory, "/4_DATA/Combined_data_sets_2_waves_analytic_data_set.RDS"))
#df_an <- readRDS(paste0(parent_directory, "/4_DATA/Combined_data_sets_2_waves_analytic_data_set.RDS"))

# _READ analytic data set-------
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
df_an <- readRDS(paste0(parent_directory, "/4_DATA/Combined_data_sets_2_waves_analytic_data_set.RDS")) 
dim(df_an) # 7085  673

# 3.1) Analytic data set for models (includes more restrictions)----


# START_EDA---------
# go to 3.0_EDA.R


# _3.1) Select variables for final analysis based on EDA----

df_an1 <- df_an %>% dplyr::select(
  SEQN,
  SRI_no_imputation,    # SRI (exposure)
  BMXBMI,               # [Outcome] BMI
  BMXWAIST,             # [Outcome] Waist circumference (WC)
  BMXHT,                # Standing Height
  BMDAVSAD ,            # [Outcome] Average Sagittal Abdominal Diameter (cm)
  DXDTOPF,              # [Outcome] Total Percent Fat
  DXDTOFAT,             # Total Fat (g)
  Avg_Systolic_BP,      # Systolic BP
  LBXVIDMS,             # 25-hydroxyvitamin D,
  DR1TKCAL,             # Total energy (kcal)
  LBDSTRSI,             # Triglycerides (mmol/L) (TG)
  LBDHDDSI,             # Direct HDL-Cholesterol (mmol/L) (HDL-C)
  RIDAGEYR,             # Age; https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#RIDAGEYR
  ALQ101,               # Had at least 12 alcohol drinks/1 yr
  ALQ110,               # Had at least 12 alcohol drinks/lifetime?
  ALQ130,               # Avg # alcoholic drinks/day - past 12 mos
  ALQ141Q,              # Alcohol use frequency
  ALQ141U,              # Alcohol use frequency unit
  ALQ120Q,              # Alcohol use frequency
  ALQ120U,              # Alcohol use frequency unit
  SMQ020,               # Smoked at least 100 cigarettes in life; https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.htm#SMQ020
  SMQ040,               # Do you now smoke cigarettes; https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.htm#SMQ040
  SMD650,               # Avg # cigarettes/day during past 30 days; https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.htm#SMD650
  DIQ010,               # Diabetes diagnosis
  Depression_score_PHQ_9, # Depression score (PHQ-9)
  DMDEDUC2,             # Education
  RIDRETH3,             # Ethnicity
  INDHHIN2,             # H.H.income
  DMDMARTL,             # Marital Status
  OCD150,               # Occupation (last week)
  OCQ380,               # Main reason did not work last week
  activity_level,       # Physical activity (mean triaxial value)
  RIAGENDR,             # Sex
  LBDHDDSI,             # Direct HDL Cholesterol
  Wave                  # Wave (1112 or 1314)
)


vis_miss(df_an1 %>% sample_n(1000)) # both waves
vis_miss(df_an1 %>% dplyr::filter(Wave == "NHANES_1112") %>% sample_n(1000)) # -> impute Alcohol, Depression and Vitamin D?
vis_miss(df_an1 %>% dplyr::filter(Wave == "NHANES_1314") %>% sample_n(1000))


# 3.2) Define/Adjust Variables for analysis (missings, recode, etc.)----

#____SRI [Exposure]------
hist(df_an1$SRI_no_imputation, main = "SRI_no_imputation") # 
boxplot(df_an1$SRI_no_imputation, main = "SRI_no_imputation") 
# One could clean very high and very low SRI values manually by looking at the sleep wake patterns.

# _____Option 1: Winsorize SRI with rule----
#table(df_an1$SRI_no_imputation < -10) # 7!
#hist(df_an1$SRI_no_imputation, main="SRI_no_imputation") #
#df_an1$SRI_no_imputation_winsorized <- ifelse(df_an1$SRI_no_imputation < -10, -10, df_an1$SRI_no_imputation)

# _____Option 2: Winsorize by using noise---------

# Simulate SRI for random sleep:
# 1 day = 1440 minutes
set.seed(3455)
n_sim <- 100000
SRI_random_sim <- numeric(n_sim)
for(i in 1:n_sim){
  SRI_random_sim[i] <- -100 + 200*sum(rbinom(60*24, 1, 0.5))/1440
}
hist(SRI_random_sim, main="SRI_random_sim") #
quantile(SRI_random_sim, probs = c(0.005, 0.995)) # +/- 6.8 (99% quantile)
# As sum of Bernoulli random variables, the SRI should be normally distributed

mean_sri <- mean(SRI_random_sim, na.rm = TRUE)
sd_sri <- sd(SRI_random_sim, na.rm = TRUE)
ggplot(data.frame(SRI_random_sim), aes(x = SRI_random_sim)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, color = "black", fill = "lightblue") +  # Histogram
  stat_function(fun = dnorm, args = list(mean = mean_sri, sd = sd_sri), 
                color = "red", linewidth = 1) +  # Normal density curve
  labs(title = "Histogram of SRI_random_sim with Normal Curve",
       x = "SRI_random_sim", y = "Density") +
  theme_minimal() # This is normal according to the CLT

df_an1$SRI_no_imputation_winsorized_rand <- 
  ifelse(df_an1$SRI_no_imputation < -6.8, rnorm(1, mean_sri, sd_sri), df_an1$SRI_no_imputation)
# Why only in one direction?
# Since 0 should mark the place where sleep is random and the higher the SRI value, 
# the more regular sleep patterns should be. Hence, we only replace values smaller
# than a certain cutoff with random noise.

# How many were replaced?
sum(df_an1$SRI_no_imputation_winsorized_rand != df_an1$SRI_no_imputation) # 8

#____[Outcome] BMI - Body mass index------
bmi_p1 <- ggplot(df_an1, aes(x = factor(1), y = BMXBMI)) +
  geom_boxplot(outlier.shape = NA, , width = 0.4) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.1, color = "blue") +  # Jitter with some transparency
  labs(x = "", y = "BMI") +
  theme_minimal() + 
  coord_flip()
bmi_p1
# Winsorize BMI
df_an1$BMXBMI_winsorized <- DescTools::Winsorize(df_an1$BMXBMI, 
                                    val = quantile(df_an1$BMXBMI, probs = c(0.01, 0.99), na.rm = TRUE))
quantile(df_an1$BMXBMI, probs = c(0.01, 0.99), na.rm = TRUE) # 18.01 51.80 

bmi_p2 <- ggplot(df_an1 %>% dplyr::filter(!is.na(BMXBMI_winsorized)), aes(x = factor(1), y = BMXBMI_winsorized)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.1, color = "blue") +  # Jitter with some transparency
  labs(x = "", y = "BMI") +
  theme_minimal() + 
  coord_flip()
bmi_p2

# SUPPLEMENT - Visualize winsorized BMI
df_long <- df_an1 %>% 
  pivot_longer(cols = c("BMXBMI", "BMXBMI_winsorized"), 
               names_to = "Type", 
               values_to = "BMI")
ggplot(df_long, aes(x = factor(Type), y = BMI, fill = Type)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "blue") +
  labs(x = "", y = "BMI") +
  theme_minimal() + 
  scale_fill_manual(values = c("BMXBMI" = "skyblue", "BMXBMI_winsorized" = "blue")) +
  coord_flip() +
  ggtitle("Comparison of Original and Winsorized BMI") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# ____[Outcome] WC - Waist circumference in cm ---------
df_an1$BMXWAIST %>% hist() # winsorize?
summary(df_an1$BMXWAIST)
quantile(df_an1$BMXWAIST, probs = c(0.01, 0.99), na.rm = TRUE) # 
df_an1$BMXWAIST_winsorized <- DescTools::Winsorize(df_an1$BMXWAIST, 
                                      val = quantile(df_an1$BMXWAIST, probs = c(0.01, 0.99), na.rm = TRUE))
hist(df_an1$BMXWAIST_winsorized) # looks nice
table(is.na(df_an1$BMXWAIST_winsorized))/length(df_an1$BMXWAIST_winsorized) # 4.1%

# ____[Outcome] ABSI - A Body shape Index (ABSI) = WC/(BMI^(2/3)*HT^(1/2))----------
# https://de.wikipedia.org/wiki/Body-Shape-Index
# Index to measure body fat percentage, 2012 by Nir Y. und Jesse C. Krakauer
df_an1$ABSI <- df_an1$BMXWAIST_winsorized/100 / (df_an1$BMXBMI_winsorized^(2/3) * (df_an1$BMXHT/100)^(1/2))
df_an1$ABSI %>% hist() # looks nice
range(df_an1$ABSI, na.rm = TRUE) # 0.06390279 0.10795481
table(is.na(df_an1$ABSI))/length(df_an1$ABSI) # 4.4%

# ____[Outcome] WHtR - Waist to Height Ratio------
# https://en.wikipedia.org/wiki/Waist-to-height_ratio
# no dimension, measure of the distribution of body fat. 2022 NICE guidelines recommend boundary values
df_an1$WHtR <- df_an1$BMXWAIST_winsorized / df_an1$BMXHT
df_an1$WHtR %>% hist() # 
table(is.na(df_an1$WHtR))/length(df_an1$WHtR) # 4.3%

#____[Outcome] SAD Average Sagittal Abdominal Diameter (BMDAVSAD)------
# https://en.wikipedia.org/wiki/Sagittal_abdominal_diameter
# a measure of visceral obesity, the amount of fat in the gut region. 
# SAD is the distance from the small of the back to the upper abdomen
df_an1$BMDAVSAD %>% hist() # in cm
boxplot(df_an1$BMDAVSAD) # 
df_an1$BMDAVSAD_winsorized <- DescTools::Winsorize(df_an1$BMDAVSAD, 
                                      val = quantile(df_an1$BMDAVSAD, probs = c(0.01, 0.99), na.rm = TRUE))
hist(df_an1$BMDAVSAD_winsorized) # skewed
table(is.na(df_an1$BMDAVSAD_winsorized))/length(df_an1$BMDAVSAD_winsorized) # 7.2%

#____[Outcome] SADHtR - Average Sagittal Abdominal Diameter to height Ratio------
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4445126/#:~:text=The%20SAD%2Fheight%20ratios%20were,between%20the%2030s%20and%2040s.
# SAD in cm divided by height in cm
df_an1$SADHtR <- df_an1$BMDAVSAD_winsorized / df_an1$BMXHT # note: height in cm
df_an1$SADHtR %>% hist() # skewed

# ____Height (HT)---------
# in cm
df_an1$BMXHT %>% hist() # looks very nice, leave as is.
table(is.na(df_an1$BMXHT))/length(df_an1$BMXHT) # 0.8%

# ____LBDSTRSI = Triglycerides (mmol/L) (TG)-----
df_an1 %>% dplyr::filter(LBDSTRSI < 30) %>% dplyr::select(LBDSTRSI) %>% hist()
df_an1$LBDSTRSI_winsorized <- DescTools::Winsorize(df_an1$LBDSTRSI, 
                                      val = quantile(df_an1$LBDSTRSI, probs = c(0.01, 0.99), na.rm = TRUE))
hist(df_an1$LBDSTRSI_winsorized) # very skewed
table(is.na(df_an1$LBDSTRSI_winsorized))/length(df_an1$LBDSTRSI_winsorized) # 5%

# ____LBDHDDSI = Direct HDL-Cholesterol (mmol/L) (HDL-C)-----
df_an1 %>% dplyr::filter(LBDHDDSI < 3) %>% dplyr::select(LBDHDDSI) %>% hist()
df_an1$LBDHDDSI_winsorized <- DescTools::Winsorize(df_an1$LBDHDDSI, 
                                      val = quantile(df_an1$LBDHDDSI, probs = c(0.01, 0.99), na.rm = TRUE))
hist(df_an1$LBDHDDSI_winsorized) # skewed
table(is.na(df_an1$LBDHDDSI_winsorized))/length(df_an1$LBDHDDSI_winsorized) # 4.8%

# ____[Outcome] VAI - Visceral Adiposity index----
# https://www.nature.com/articles/s41440-023-01173-6
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4009335/#:~:text=The%20Visceral%20Adiposity%20Index%20(VAI)%20is%20an%20empirical%2Dmathematical,distribution%20and%20function%20%5B8%5D.
df_an1$RIAGENDR # 1=male
# LBDSTRSI = Triglycerides (mmol/L) (TG)
# LBDHDDSI = Direct HDL-Cholesterol (mmol/L) (HDL-C)
df_an1$VAI <- ifelse(df_an1$RIAGENDR == 1,
                     # Formula for males
                     (df_an1$BMXWAIST_winsorized / (39.68 + (1.88 * df_an1$BMXBMI_winsorized))) *
                       (df_an1$LBDSTRSI_winsorized / 1.03) * (1.31 / df_an1$LBDHDDSI_winsorized),
                     # Formula for females
                     (df_an1$BMXWAIST_winsorized / (36.58 + (1.89 * df_an1$BMXBMI_winsorized))) *
                       (df_an1$LBDSTRSI_winsorized / 0.81) * (1.52 / df_an1$LBDHDDSI_winsorized))
df_an1%>% dplyr::filter(VAI < 45) %>% dplyr::select(VAI) %>% hist() 
summary(df_an1$VAI)
# missings
sum(is.na(df_an1$VAI))/length(df_an1$VAI) # ~9%
df_an1 %>% dplyr::filter(VAI > 60)
hist(df_an1$VAI) # very skewed

# ____[Outcome; 48% missing] DXDTOFAT - Total Fat (g)----
df_an1 <- df_an1 %>% mutate(DXDTOFAT_kg = DXDTOFAT/1000)
df_an1$DXDTOFAT_kg_winsorized <- DescTools::Winsorize(df_an1$DXDTOFAT_kg, 
                                      val = quantile(df_an1$DXDTOFAT_kg, probs = c(0.01, 0.99), na.rm = TRUE))
df_an1$DXDTOFAT_kg_winsorized %>% hist() # skewed
table(is.na(df_an1$DXDTOFAT_kg_winsorized))/length(df_an1$DXDTOFAT_kg_winsorized) # 48%

# ____[Outcome; 48% mising] FMI - Fat Mass Index -----
# https://pmc.ncbi.nlm.nih.gov/articles/PMC2929934/#:~:text=A%20potential%20indicator%20of%20body,and%20dividing%20by%20height%20squared
# https://www.sciencedirect.com/science/article/abs/pii/S000291652316998X
# Body Fat mass / height^2
df_an1$FMI <- df_an1$DXDTOFAT_kg_winsorized / (df_an1$BMXHT/100)^2
df_an1$FMI %>% hist() # skewed
table(is.na(df_an1$FMI))/length(df_an1$FMI) # 48%

# ____[Outcome; 48% missing] Total Percent Fat----
# DXDTOPF
df_an1$DXDTOPF %>% hist() #
summary(df_an1$DXDTOPF)
df_an1$DXDTOPF_winsorized <- DescTools::Winsorize(df_an1$DXDTOPF, 
                                      val = quantile(df_an1$DXDTOPF, probs = c(0.01, 0.99), na.rm = TRUE))
df_an1$DXDTOPF_winsorized %>% hist() # bimodal (mixture of 2 distributions, female male)
table(is.na(df_an1$DXDTOPF_winsorized))/length(df_an1$DXDTOPF_winsorized) # 48%

#____[Outcome] BRI - Body Roundness Index --------
df_an1$BRI <- 364.2 - 365.5 * sqrt(1 - (df_an1$BMXWAIST_winsorized / (2 * pi))^2 / (0.5 * df_an1$BMXHT)^2)
hist(df_an1$BRI, main = "Histogram of Body Roundness Index (BRI)", xlab = "BRI", col = "blue", breaks = 30) # skewed
summary(df_an1$BRI)

# Check for missing values
table(is.na(df_an1$BRI)) / nrow(df_an1) # 4.3%

# ____[Outcome] LAP - Lipid accumulation product------
df_an1$LAP <- ifelse(df_an1$RIAGENDR == 2,  # 2 = Female
                     (df_an1$BMXWAIST_winsorized - 58) * df_an1$LBDSTRSI_winsorized,  # Formula for females
                     (df_an1$BMXWAIST_winsorized - 65) * df_an1$LBDSTRSI_winsorized)  # Formula for males
hist(df_an1$LAP, main = "Histogram of Lipid Accumulation Product (LAP)", 
     xlab = "LAP", col = "green", breaks = 30) # skewed
table(is.na(df_an1$LAP)) / nrow(df_an1) # 8.6%


stop()

# Scatterplot ggpairs of outcome variables ------

# Sex-variable for legend:
df_an1 <- df_an1 %>%
  dplyr::mutate(
    RIAGENDR_factor = factor(
      RIAGENDR,
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

# Part 1: First 5 outcomes--------
scatter_part1 <- df_an1 %>%
  dplyr::select(
    `BMI` = BMXBMI_winsorized,
    `Waist Circumference` = BMXWAIST_winsorized,
    `ABSI` = ABSI,
    `Waist-to-Height Ratio` = WHtR,
    `BRI` = BRI
  ) %>%
  ggpairs( legend = 1, 
    aes(color = df_an1$RIAGENDR_factor, alpha = 0.3)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()
        ) +
  labs(color = "Sex")
scatter_part1

# Part 2: Next 5 outcomes----------
scatter_part2 <- df_an1 %>%
  dplyr::select(
    `SAD` = BMDAVSAD_winsorized,
    `SADHtR` = SADHtR,
    `Height (cm)` = BMXHT,
    `Triglycerides (mmol/L)` = LBDSTRSI_winsorized,
    `HDL-Cholesterol (mmol/L)` = LBDHDDSI_winsorized
  ) %>%
  ggpairs( legend = 1,
    aes(color = df_an1$RIAGENDR_factor, alpha = 0.7)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()
  ) +
  labs(color = "Sex")
scatter_part2

# Part 3: Last 5 outcomes------------
scatter_part3 <- df_an1 %>%
  dplyr::select(
    `VAI` = VAI,
    `Total Fat (kg)` = DXDTOFAT_kg_winsorized,
    `FMI` = FMI,
    `Total Percent Fat` = DXDTOPF_winsorized,
    `LAP` = LAP
  ) %>%
  ggpairs( legend = 1,
    aes(color = df_an1$RIAGENDR_factor, alpha = 0.7)
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()
  ) +
  labs(color = "Sex")
scatter_part3

# Save scatterplot matrices
file_path_part1 <- file.path(dirname(getwd()), "1_Main_Document", "Figures", "Scatterplot_Adiposity_Measures_Part1.pdf")
file_path_part2 <- file.path(dirname(getwd()), "1_Main_Document", "Figures", "Scatterplot_Adiposity_Measures_Part2.pdf")
file_path_part3 <- file.path(dirname(getwd()), "1_Main_Document", "Figures", "Scatterplot_Adiposity_Measures_Part3.pdf")

ggsave(file_path_part1, plot = scatter_part1, width = 12, height = 12)
ggsave(file_path_part2, plot = scatter_part2, width = 12, height = 12)
ggsave(file_path_part3, plot = scatter_part3, width = 12, height = 12)


# Correlation matrix for all 15 outcomes------------
cor_matrix <- df_an1 %>%
  dplyr::select(
    BMXBMI_winsorized,
    BMXWAIST_winsorized,
    ABSI,
    WHtR,
    BRI,
    BMDAVSAD_winsorized,
    SADHtR,
    BMXHT,
    LBDSTRSI_winsorized,
    LBDHDDSI_winsorized,
    VAI,
    DXDTOFAT_kg_winsorized,
    FMI,
    DXDTOPF_winsorized,
    LAP
  ) %>%
  cor(use = "pairwise.complete.obs")  # Use pairwise deletion for missing values
cor_matrix
target_variable <- "BMXBMI_winsorized"
cor_with_bmi <- cor_matrix[target_variable, , drop = FALSE]


# Save correlation matrix as CSV
cor_matrix_path <- file.path(dirname(getwd()), "1_Main_Document", "Figures", "Correlation_Matrix.csv")
write.csv(cor_matrix, file = cor_matrix_path, row.names = TRUE)

cor_df <- as.data.frame(as.table(cor_matrix))

# Correlations with |correlation| > 0.9-----------
high_correlations <- cor_df %>%
  dplyr::filter(Var1 != Var2, abs(Freq) > 0.9) %>%
  dplyr::distinct(Freq, .keep_all = TRUE) %>%
  dplyr::arrange(desc(abs(Freq)))
high_correlations
# -> BMI is highly correlated to many other variables like 
#    Total fat(high missingness), WHtR, BRI and WC.
# -> One could expect somewhat similar predictions. Although, SRI is not 
#    strongly associated with the adiposity outcomes and could therefore 
#    show different behaviour.

# BRI WHtR: r = 0.99 -> Checking the random predictions from the model. practically IDENTICAL.
# BMI and WHtR: r = 0.91 Checking the random predictions from the model. very similar!



cor(df_an1$BMXBMI_winsorized, df_an1$DXDTOFAT_kg_winsorized, use = "pairwise.complete.obs") # 0.91
cor(df_an1$BMXBMI_winsorized, df_an1$FMI, use = "pairwise.complete.obs") # 0.88
cor(df_an1$BMXBMI_winsorized, df_an1$ABSI, use = "pairwise.complete.obs") # ~0 !!


#____Type of work done last week (OCD150) ----
df_an1$OCD150 %>% table() # 7 = refused, 9 = don't know
df_an1$OCD150 <- ifelse(df_an1$OCD150 %in% c(7, 9), NA, df_an1$OCD150)
df_an1$OCD150_factor <- factor(df_an1$OCD150,
                               levels = c(1, 2, 3, 4),
                               labels = c("Working at a job or business",
                                          "With a job or business but not at work",
                                          "Looking for work",
                                          "Not working at a job or business"))
table(df_an1$OCD150_factor)
df_an1$RIDAGEYR %>% hist() # all >=20 years
# Maybe there are many students in the data set?

# ____Main reason did not work last week (OCQ380)----
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/OCQ_H.htm#OCQ380
df_an1$OCQ380 <- ifelse(df_an1$OCQ380 %in% c(77, 99), NA, 
                        df_an1$OCQ380)
df_an1$OCQ380_factor <- factor(df_an1$OCQ380,
                               levels = c(1, 2, 3, 4, 5, 6, 7),
                               labels = c("Taking care of house or family",
                                          "Going to school",
                                          "Retired",
                                          "Unable to work for health reasons",
                                          "On layoff",
                                          "Disabled",
                                          "Other"))
table(df_an1$OCQ380_factor)
levels(df_an1$OCQ380_factor)

# ____Expanded occupation variable----
df_an1$Occupation_combined <- ifelse(
  # Subdivide "Not working at a job or business" using OCQ380
  df_an1$OCD150_factor == "Not working at a job or business" &
    !is.na(df_an1$OCQ380_factor),
  as.character(df_an1$OCQ380_factor), # Use specific reason from OCQ380
  as.character(df_an1$OCD150_factor)  # Keep other original OCD150 categories
)

df_an1$Occupation_combined <- factor(df_an1$Occupation_combined,
                                     levels = c("Working at a job or business",
                                                "With a job or business but not at work",
                                                "Looking for work",
                                                "Taking care of house or family",
                                                "Going to school",
                                                "Retired",
                                                "Unable to work for health reasons",
                                                "On layoff",
                                                "Disabled",
                                                "Other"))

table(df_an1$Occupation_combined)/nrow(df_an1) 

# Grouping into broader categories, combining Retired and Students
df_an1$Occupation_grouped <- factor(ifelse(
  df_an1$Occupation_combined %in% c("Working at a job or business", "With a job or business but not at work"),
  "Working",
  ifelse(df_an1$Occupation_combined %in% c("Looking for work", "On layoff"),
         "Unemployed",
         ifelse(df_an1$Occupation_combined %in% c("Retired", "Going to school"),
                "Retired or Student",
                ifelse(df_an1$Occupation_combined %in% c("Taking care of house or family",
                                                         "Unable to work for health reasons",
                                                         "Disabled"),
                       "Unable to Work (Health or Family Reasons)",
                       "Other")))))

table(df_an1$Occupation_grouped)/nrow(df_an1) 
# Reorder factor levels
df_an1$Occupation_grouped <- factor(df_an1$Occupation_grouped,
                                    levels = c("Working",
                                               "Retired or Student",
                                               "Unable to Work (Health or Family Reasons)",
                                               "Unemployed",
                                               "Other"))

levels(df_an1$Occupation_grouped)
table(df_an1$Occupation_grouped)/nrow(df_an1)




#____Education (DMDEDUC2)-----
# Education level - Adults 20+
df_an1$DMDEDUC2 %>% table() # 7 = refused, 9 = don't know
df_an1$DMDEDUC2 <- ifelse(df_an1$DMDEDUC2 %in% c(7, 9), NA, df_an1$DMDEDUC2)
df_an1$DMDEDUC2_factor <- factor(df_an1$DMDEDUC2,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("Less Than 9th Grade",
                                            "9-11th Grade (Includes 12th grade with no diploma)",
                                            "High School Grad/GED or Equivalent",
                                            "Some College or AA degree",
                                            "College Graduate or above"))
table(df_an1$DMDEDUC2_factor)


#____Race/Ethnicity (RIDRETH3)----
# Race/Hispanic origin w/ NH Asian
df_an1$RIDRETH3 %>% table() # 
sum(is.na(df_an1$RIDRETH3)) # 0
df_an1$RIDRETH3_factor <- factor(df_an1$RIDRETH3,
                                 levels = c(1, 2, 3, 4, 6, 7),
                                 labels = c("Mexican American",
                                            "Other Hispanic",
                                            "Non-Hispanic White",
                                            "Non-Hispanic Black",
                                            "Non-Hispanic Asian",
                                            "Other Race - Including Multi-Racial"))
table(df_an1$RIDRETH3_factor)


# ____Gender (RIAGENDR)----
df_an1$RIAGENDR
table(df_an1$RIAGENDR) # 
sum(is.na(df_an1$RIAGENDR)) # 0
df_an1$RIAGENDR_factor <- factor(df_an1$RIAGENDR,
                                 levels = c(1, 2),
                                 labels = c("Male", "Female"))
table(df_an1$RIAGENDR_factor)


#____Annual household income (INDHHIN2)------
df_an1$INDHHIN2 %>% table() # 170 said 77 = refused, 163 said 99 = don't know
df_an1$INDHHIN2 <- ifelse(df_an1$INDHHIN2 %in% c(77, 99), NA, df_an1$INDHHIN2)
df_an1$INDHHIN2_factor <- factor(df_an1$INDHHIN2,
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15),
                                 labels = c("$0 to $4,999",
                                            "$5,000 to $9,999",
                                            "$10,000 to $14,999",
                                            "$15,000 to $19,999",
                                            "$20,000 to $24,999",
                                            "$25,000 to $34,999",
                                            "$35,000 to $44,999",
                                            "$45,000 to $54,999",
                                            "$55,000 to $64,999",
                                            "$65,000 to $74,999",
                                            "$20,000 and Over",
                                            "Under $20,000",
                                            "$75,000 to $99,999",
                                            "$100,000 and Over"))
table(df_an1$INDHHIN2_factor)


# ____Smoking------
# One option to define smoking is to define the missings in SMD650 with 
# SMQ020 == 2 (never smoked 100 cigarettes) as non-smokers
# Note: This will probably introduce zero-inflated distribution.
df_an1 <- as.data.table(df_an1)
sum(is.na(df_an1[Wave == "NHANES_1112" & SMQ020 == 2,]$SMQ040)) # 2066
sum(is.na(df_an1[Wave == "NHANES_1112",]$SMQ040)) # 2071

# Option 1:
#df_an1$SMD650 <- ifelse(is.na(df_an1$SMQ040) & df_an1$SMQ020 == 2, 0, df_an1$SMQ040)
#table(is.na(df_an1$SMD650)) # 7 missings remain
#table(df_an1$SMD650) # 1 refused
#df_an1$SMD650 <- ifelse(df_an1$SMD650 == 7, NA, df_an1$SMD650)
#table(is.na(df_an1$SMD650)) # 8 missings remain!

# Option 2: (as in https://www.frontiersin.org/journals/psychiatry/articles/10.3389/fpsyt.2024.1407741/full#supplementary-material)
# "Smoking status was determined using the NHANES questionnaire item
# “SMQ020 - Smoked at least 100 cigarettes in life”. 
# Participants who reported having smoked less than 100 cigarettes in their 
# lifetime were classified as “non-smokers”. Among those who had smoked at 
# least 100 cigarettes, we define the previous smoker by determining subjects
# whose answer for item “SMQ040 - Do you now smoke cigarettes?” is “3, Not at all”. 
# For participants currently smoking, the intensity of smoking was assessed 
# using the questionnaire item “SMD650 - Avg # cigarettes/day during past 30 days”."
# There is an error in the coding of the frontiers paper: SDM560==5 is not included in the definition....
conflicts_prefer(DescTools::`%nin%`)
df_an1 <- df_an1 %>%
  mutate(smoking_status = case_when(
    SMQ020 == 2 ~ "Non_smokers",  # Less than 100 cigarettes
    SMQ020 == 1 & SMQ040 == 3 ~ "Previous_smoker",  
    SMQ040 %in% c(1,2) & SMD650 %in% c(1,2,3,4,5) ~ "Light_smoker",
    SMQ040 %in% c(1,2) & SMD650 %in% c(6,7,8,9) ~ "Moderate_smoker",
    SMQ040 %in% c(1,2) & SMD650 >= 10 & SMD650 %nin% c(777,999) ~ "Heavy_smoker",
    TRUE ~ NA_character_))           # If none of the conditions are met
table(df_an1$smoking_status) #
# df_an1 %>% dplyr::filter(SMQ040 %in% c(1,2) & SMD650 >= 10 & SMD650 %nin% c(777,999)) # check!
table(is.na(df_an1$smoking_status)) # 25 missings

df_an1$smoking_status <- as.factor(df_an1$smoking_status)
table(df_an1$smoking_status)


#____Alcohol------
# https://wwwn.cdc.gov/nchs/nhanes/2011-2012/alq_g.htm

# _____Orig. Questionnaires-----
# Audio Computer-Assisted Self-Interview (ACASI) Questionnaire:
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/alq_acasi.pdf
# Computer-Assisted Personal Interview (CAPI) Questionnaire (Target Group: SPs 18+ (CAPI):
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/alq_capi.pdf

# Explore meaning of variables:
df_an1 %>% dplyr::select(ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130) %>% head(20)
table(is.na(df_an1$ALQ101))/sum(table(is.na(df_an1$ALQ101))) # 7%

df_an1 %>%
  dplyr::select(ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))  # Calculate % of missing for each column

#df %>% dplyr::filter(Wave == "NHANES_1112") %>% dplyr::filter(is.na(ALQ110)) %>% dplyr::select(ALQ101) %>% table()
#df%>% dplyr::filter(Wave == "NHANES_1314") %>% dplyr::filter(is.na(ALQ110)) %>% dplyr::select(ALQ101) %>% table()

# _____If a person had a year with at least 12 drinks, then this person also had 12 drinks in his/her entire life!--------
df_an1$ALQ110_completed <- ifelse(df_an1$ALQ101 == 1, 1, df_an1$ALQ110)

df_an1 %>% # now same low missingness rate as in ALQ101 (6.7%)
dplyr::select(ALQ101, ALQ110 ,ALQ110_completed, ALQ120Q, ALQ120U, ALQ130) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))  # Calculate % of missing for each column

# Option 1: (as in https://www.sciencedirect.com/science/article/pii/S0147651323008503)
# The ALQ120Q variable of the NHANES Alcohol Use Questionnaire quantifies the 
# intensity of alcohol exposure in the past 12 months based on the frequency of
# alcohol consumption (number of drinks per day). 
# Alcohol consumption was defined as 
# heavy drinking (≥2 drinks/day), 
# mild drinking (1 drink/day), and
# never or quit drinking (never or no drink).
#df_an1 <- df_an1 %>%
#  mutate(alcohol_consumption = case_when(
#    ALQ110_completed == 2 ~ "Never/Non-Drinker",  # 2 = No; Had at least 12 alcohol drinks/lifetime? Why 12?
#    ALQ130 == 1 ~ "Mild Drinker",        # 1 drink/day on drinking days
#    ALQ130 >= 2 & ALQ130 %nin% c(777,999) ~ "Heavy Drinker",       # >=2 drinks/day on drinking days; Avg # alcoholic drinks/day - past 12 mos
#    TRUE ~ NA_character_))               # NA for others
#table(df_an1$alcohol_consumption) #
#table(df_an1$alcohol_consumption)/sum(table(df_an1$alcohol_consumption)) # 

#df_an1$alcohol_consumption <- as.factor(df_an1$alcohol_consumption)
#table(df_an1$alcohol_consumption)

#table(is.na(df_an1$alcohol_consumption))/sum(table(is.na(df_an1$alcohol_consumption))) #24%
#table(is.na(df_an1$ALQ110))/sum(table(is.na(df_an1$ALQ110))) # 77%
#table(is.na(df_an1$ALQ130))/sum(table(is.na(df_an1$ALQ130))) # 38%

# Option 2: (https://bmjopen.bmj.com/content/bmjopen/14/8/e082851.full.pdf)
# "Never drinking was defined based on the answer of ‘no’ to variable ALQ101. 
# And those who answered ‘yes’ were divided into <once a week or ≥once a week based on variables ALQ120Q and ALQ120U"

# check percent of missings in the variables ALQ101, ALQ110, ALQ120Q, ALQ120U
#df_an1 %>% dplyr::select(ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130) %>% summarise(across(everything(), ~ mean(is.na(.)) * 100))  
# ALQ101   ALQ110  ALQ120Q  ALQ120U
# 6.718419 73.30981 21.25618 38.88497

#df_an1 <- df_an1 %>%
#  mutate(alcohol_consumption = case_when(
#    ALQ101 == 2 ~ "Never/Non-Drinker",  # 2 = No; Had at least 12 alcohol drinks/1 yr??
#    ALQ101 == 1 & ALQ120Q == 1 ~ "less_once_a_week",  # 1 = <once a week
#    ALQ101 == 1 & ALQ120Q >= 2 & ALQ120Q %nin% c(777,999) & ALQ120U == 1 ~ "greater_once_a_week", # 2 = ≥once a week
#    ALQ101 == 1 & ALQ120Q >= 8 & ALQ120Q %nin% c(777,999) & ALQ120U == 2 ~ "greater_once_a_week", # 2 = ≥once a week
#    ALQ101 == 1 & ALQ120Q >= 104 & ALQ120Q %nin% c(777,999) & ALQ120U == 3 ~ "greater_once_a_week", # 2 = ≥once a week
#    ALQ130 > 1 & ALQ130 %nin% c(777,999) ~ "greater_once_a_week",       # >=2 drinks/day on drinking days; Avg # alcoholic drinks/day - past 12 mos
#    TRUE ~ NA_character_))               # NA for others
#table(df_an1$alcohol_consumption)
#table(is.na(df_an1$alcohol_consumption))/sum(table(is.na(df_an1$alcohol_consumption))) # 0.26!
#hist(df_an1$RIDAGEYR[is.na(df_an1$alcohol_consumption)])


# _____Def.Option 3 -------
# using CDC definition of moderate and heavy drinker depending on sex
# https://www.cdc.gov/alcohol/about-alcohol-use/moderate-alcohol-use.html
# https://www.cdc.gov/alcohol/excessive-drinking-data/index.html

# Questionnaire:
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/alq_acasi.pdf
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/alq_capi.pdf
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/manuals/mec_interviewers_manual.pdf

# For women, moderate drinking is defined as up to 1 drink per day 
# For men, moderate drinking is defined as up to 2 drinks per day

# For women, heavy drinking is defined as consuming 8 drinks or more per week
# For men, heavy drinking is defined as consuming 15 drinks or more per week

# Definitions more clearly:
# https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/moderate-binge-drinking
# drink in moderation by limiting intake to 2 drinks or less in a day for men 
# and 1 drink or less in a day for women, when alcohol is consumed. 

# binge: 5 or more alcoholic drinks for males or 4 or more alcoholic drinks for 
# females on the same occasion

# NIAAA defines heavy drinking as follows:
# For women, consuming four or more on any day or 8 or more drinks per week
# For men, consuming five or more drinks on any day or 15 or more per week

# Variables:
# ALQ120Q: How often drink alcohol over past 12 mos
# ALQ120U: UNIT OF MEASURE.
# ALQ130: Avg # alcoholic drinks/day - past 12 mos
# ALQ141Q: # days have 4/5 drinks - past 12 mos
# ALQ141U: UNIT OF MEASURE.
# ALQ130: Avg # alcoholic drinks/day - past 12 mos

df_an1 <- df_an1 %>%
  mutate(alcohol_consumption = case_when(
    # Never/Non-Drinker:
    ALQ110_completed == 2 ~ "Never/Non-Drinker",  # 2 = No; Had at least 12 alcohol drinks/lifetime? Why 12?
    
    # Heavy Drinker: -> this is only for drinking days!
    #RIAGENDR_factor == "Female" & ALQ130 >= 2 & ALQ130 %nin% c(777,999) ~ "Heavy Drinker", # 2 or more per day
    #RIAGENDR_factor == "Male" & ALQ130 >= 3 & ALQ130 %nin% c(777,999) ~ "Heavy Drinker", # 3 or more per day
    
    RIAGENDR_factor == "Female" & ALQ120Q >= 8 & ALQ120Q %nin% c(777,999) & ALQ120U == 1 ~ "Heavy Drinker", # per week
    RIAGENDR_factor == "Female" & ALQ120Q >= 4*8 & ALQ120Q %nin% c(777,999) & ALQ120U == 2 ~ "Heavy Drinker", # per month
    RIAGENDR_factor == "Female" & ALQ120Q >= 52*8 & ALQ120Q %nin% c(777,999) & ALQ120U == 3 ~ "Heavy Drinker", # per year
    
    RIAGENDR_factor == "Male" & ALQ120Q >= 15 & ALQ120Q %nin% c(777,999) & ALQ120U == 1 ~ "Heavy Drinker", # per week
    RIAGENDR_factor == "Male" & ALQ120Q >= 4*15 & ALQ120Q %nin% c(777,999) & ALQ120U == 2 ~ "Heavy Drinker", # per month
    RIAGENDR_factor == "Male" & ALQ120Q >= 52*15 &  ALQ120Q %nin% c(777,999) & ALQ120U == 3 ~ "Heavy Drinker", # per year
    
    RIAGENDR_factor == "Female" & ALQ141Q >= 2 & ALQ141Q %nin% c(777,999) & ALQ141U == 1 ~ "Heavy Drinker", # >=8 per week
    RIAGENDR_factor == "Female" & ALQ141Q >= 8 & ALQ141Q %nin% c(777,999) & ALQ141U == 2 ~ "Heavy Drinker", # >=32 per month
    RIAGENDR_factor == "Female" & ALQ141Q >= 52*8/4 & ALQ141Q %nin% c(777,999) & ALQ141U == 3 ~ "Heavy Drinker", # >=104 per year
    
    RIAGENDR_factor == "Male" & ALQ141Q >= 4 & ALQ141Q %nin% c(777,999) & ALQ141U == 1 ~ "Heavy Drinker", # >=16 per week
    RIAGENDR_factor == "Male" & ALQ141Q >= 15 & ALQ141Q %nin% c(777,999) & ALQ141U == 2 ~ "Heavy Drinker", # >=60 per month
    RIAGENDR_factor == "Male" & ALQ141Q >= 52*15/4 & ALQ141Q %nin% c(777,999) & ALQ141U == 3 ~ "Heavy Drinker", # >=780 per year
  
    # Moderate Drinker:
    RIAGENDR_factor == "Female" & ALQ130 %in% c(0,1) ~ "Moderate Drinker", # up to 1 per day
    RIAGENDR_factor == "Male" & ALQ130 %in% c(0,1,2) ~ "Moderate Drinker", # up to 2 per day
    
    RIAGENDR_factor == "Female" & ALQ120Q < 8 & ALQ120Q %nin% c(777,999) & ALQ120U == 1 ~ "Moderate Drinker", # per week
    RIAGENDR_factor == "Female" & ALQ120Q < 4*8 & ALQ120Q %nin% c(777,999) & ALQ120U == 2 ~ "Moderate Drinker", # per month
    RIAGENDR_factor == "Female" & ALQ120Q < 52*8 & ALQ120Q %nin% c(777,999) & ALQ120U == 3 ~ "Moderate Drinker", # per year
    
    RIAGENDR_factor == "Male" & ALQ120Q < 15 & ALQ120Q %nin% c(777,999) & ALQ120U == 1 ~ "Moderate Drinker", # per week
    RIAGENDR_factor == "Male" & ALQ120Q < 4*15 & ALQ120Q %nin% c(777,999) & ALQ120U == 2 ~ "Moderate Drinker", # per month
    RIAGENDR_factor == "Male" & ALQ120Q < 52*15 & ALQ120Q %nin% c(777,999) & ALQ120U == 3 ~ "Moderate Drinker", # per year
    
    TRUE ~ NA_character_))  # NA for others
table(df_an1$alcohol_consumption)
table(df_an1$alcohol_consumption)/sum(table(df_an1$alcohol_consumption)) #
table(is.na(df_an1$alcohol_consumption))/sum(table(is.na(df_an1$alcohol_consumption))) # 24% still
df_an1$alcohol_consumption <- as.factor(df_an1$alcohol_consumption)

# ____Marital Status------
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/dmq.pdf (orig questionnaire)
table(df_an1$DMDMARTL) # 6 = refused, 9 = don't know
unique(df_an1$DMDMARTL) # 77 refused
table(is.na(df_an1$DMDMARTL)) # no missings

# What about the large data set?
df %>% dplyr::filter(RIDAGEYR < 20) %>% count() # 8602
df %>% dplyr::filter(RIDAGEYR < 20) %>% dplyr::select(DMDMARTL) %>% is.na() %>% sum() # 8602
df %>% dplyr::filter(RIDAGEYR >= 20) %>% dplyr::select(DMDMARTL) %>% is.na() %>% sum() # 0

# Replace Refused/Don't know with NA:
df_an1$DMDMARTL <- ifelse(df_an1$DMDMARTL %in% c(77, 99), NA, df_an1$DMDMARTL)
table(is.na(df_an1$DMDMARTL)) # 3 missings
table(df_an1$DMDMARTL) 

# Option 1 (original):
df_an1$DMDMARTL_factor <- factor(df_an1$DMDMARTL,
                                 levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Married",
                                            "Widowed",
                                            "Divorced",
                                            "Separated",
                                            "Never married",
                                            "Living with partner"))
table(df_an1$DMDMARTL_factor)

# Option 2:
# https://onlinelibrary.wiley.com/doi/full/10.1111/resp.14799
# Marital status categories were created by collapsing the original NHANES categories from the variable DMDMARTL. 
# ‘Married or Living with Partner’ includes the responses ‘Married’ and ‘Living with partner’; 
# ‘Never Married’ includes the response ‘Never married’; 
# ‘Other’ includes the responses ‘Widowed’, ‘Divorced’ and ‘Separated’.
#df_an1$Marital_status_collapsed <- factor(df_an1$DMDMARTL,
#                                          levels = c(1, 2, 3, 4, 5, 6),
#                                          labels = c("Married or Living with Partner",  # 1 and 6 will be combined
#                                                     "Other",                          # 2 (Widowed)
#                                                     "Other",                          # 3 (Divorced)
#                                                     "Other",                          # 4 (Separated)
#                                                     "Never Married",                  # 5
#                                                     "Married or Living with Partner")) # 6
#table(df_an1$Marital_status_collapsed)


# ____activity level---------
# PAXMTSM: MIMS triaxial value for the minute
# Filtered for PAXMTSM>0
# Defined as median of PAXMTSM.
df_an1$activity_level %>% hist() # good

# Alternatively, one can look at:
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/paq.pdf (orig questionnaire)
# https://wwwn.cdc.gov/nchs/nhanes/2011-2012/PAQ_G.htm


# ____Depression score------
# Note: Refused/Dont know were set to NA in the score-defining elements
df_an1$Depression_score_PHQ_9 %>% hist() # heavily right skewed
boxplot(df_an1$Depression_score_PHQ_9, main = "Depression_score_PHQ_9")
sum(is.na(df_an1$Depression_score_PHQ_9))
#dim(df_an1_imputed)

# ____Avg_Systolic_BP------
df_an1$Avg_Systolic_BP %>% hist() # 
table(is.na(df_an1$Avg_Systolic_BP)) # 189 of almost 7000

# ____direct Cholesterol (LBDHDDSI)------
df_an1$LBDHDDSI %>% hist() #
df_an1$LBDHDDSI_winsorized %>% hist() #

# ____Vitamin D (LBXVIDMS) ------
# 25OHD2+25OHD3 (nmol/L)
df_an1$LBXVIDMS %>% hist() #
# Winsorise
df_an1$LBXVIDMS_winsorized <- DescTools::Winsorize(df_an1$LBXVIDMS, 
                                      val = quantile(df_an1$LBXVIDMS, probs = c(0.01, 0.99), na.rm = TRUE))
summary(df_an1$LBXVIDMS) # 
# hist
df_an1$LBXVIDMS_winsorized %>% hist() #

# ____Diabetes (DIQ010)----
df_an1$DIQ010 %>% table() 
df_an1$DIQ010 <- ifelse(df_an1$DIQ010 %in% c(7, 9), NA, df_an1$DIQ010)
df_an1$DIQ010_factor <- factor(df_an1$DIQ010,
                               levels = c(1, 2, 3),
                               labels = c("Yes", "No", "Borderline"))
table(df_an1$DIQ010_factor)

# ____Diet-total caloric intake ---------
# Total Nutrient Intakes File
# https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DR1TOT_G.htm#DR1TKCAL
# Info: The objective of the dietary interview component is to obtain detailed
# dietary intake information from NHANES participants. The dietary intake data
# are used to estimate the types and amounts of foods and beverages (including
# all types of water) consumed during the 24-hour period prior to the interview
# (midnight to midnight), and to estimate intakes of energy, nutrients, and
# other food components from those foods and beverages.

df_an1$DR1TKCAL %>% hist() #
quantile(df_an1$DR1TKCAL, probs = c(0.01, 0.99), na.rm = TRUE)
summary(df_an1$DR1TKCAL) # 0-13687, 0 probably fasting
df_an1$DR1TKCAL_winsorized <- DescTools::Winsorize(df_an1$DR1TKCAL, 
                                      val = quantile(df_an1$DR1TKCAL, probs = c(0.01, 0.99), na.rm = TRUE))
# missing
table(is.na(df_an1$DR1TKCAL))/sum(table(is.na(df_an1$DR1TKCAL))) # 6%

# SAVE df_an1:--------
current_directory <- getwd()
(parent_directory <- dirname(current_directory))
saveRDS(df_an1, paste0(parent_directory, "/z_intermediate_results/df_an1_28.5.25.RDS"))

# 4) Impute missing values----
# check missings
vis_miss(df_an1 %>% dplyr::select(-c(ALQ101, ALQ110, ALQ130, ALQ120Q, ALQ120U, SMQ040, SMQ020, SMD650)) %>% 
           sample_n(1000))  +
  theme(plot.margin = unit(c(1, 2, 1, 1), "cm")) +  # Increase margins (top, right, bottom, left)
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
df_an1 %>% dplyr::select(-c(ALQ101, ALQ110, ALQ130, ALQ120Q, ALQ120U, 
                            SMQ040, SMQ020, SMD650,
                            OCQ380)) %>% 
  sample_n(1000) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

# __Missingness plot for Supplement---------
# use only the Variables occuring in DAG/regression models
vis_miss(df_an1 %>% dplyr::select(-c(ALQ101, ALQ110, ALQ130, 
                            ALQ120Q, ALQ120U, SMQ040, 
                            SMQ020, SMD650, Avg_Systolic_BP,
                            DIQ010_factor, DIQ010,
                            OCQ380, OCD150_factor,
                            DMDMARTL,
                            LBDHDDSI, Wave,
                            SRI_no_imputation,
                            SRI_no_imputation_winsorized_rand,
                            BMXBMI, DMDEDUC2,
                            ALQ110_completed, 
                            SEQN)) %>% 
  sample_n(1000)) + 
  theme(plot.margin = unit(c(1, 2, 1, 1), "cm")) +  # Increase margins (top, right, bottom, left)
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# impute using kNN--------
tic()
df_an1_imputed <- kNN(df_an1, k = 5)
toc() # 304.91/369s sec elapsed
vis_miss(df_an1_imputed %>% sample_n(1000)) # check
df_an1_imputed %>%
  dplyr::summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  dplyr::select(where(~ . > 0))
print(object.size(df_an1_imputed), units = "Mb") # 5 MB -> on HD 1 MB
(current_directory <- getwd()) 
(parent_directory <- dirname(current_directory))
saveRDS(df_an1_imputed, paste0(parent_directory, "/z_intermediate_results/df_an1_imputed_22.4.25.RDS"))


# Missingness-Plot for supplement---------
colnames(df_an1)
dat_missingness_plot <- df_an1 %>% dplyr::select(
  SRI_no_imputation_winsorized_rand,
  BMXBMI_winsorized,
  BMXWAIST_winsorized,
  BMDAVSAD_winsorized,
  LBDHDDSI_winsorized,
  DXDTOFAT_kg_winsorized,
  DXDTOPF_winsorized,
  ABSI,
  SADHtR,
  VAI,
  FMI,
  LAP,
  BRI,
  WHtR, 
  RIDRETH3_factor,
  INDHHIN2_factor,
  #OCD150_factor,
  Occupation_combined,
  DMDEDUC2_factor,
  DMDMARTL_factor,
  alcohol_consumption,
  smoking_status,
  LBXVIDMS_winsorized,
  LBDSTRSI_winsorized,
  #LBDHDDSI_winsorized,
  DR1TKCAL_winsorized,
  Depression_score_PHQ_9,
  activity_level
  )

# Rename variables for better readability in vis_miss()
dat_missingness_plot <- dat_missingness_plot %>%
  dplyr::rename(
    'SRI' = SRI_no_imputation_winsorized_rand,
    'BMI' = BMXBMI_winsorized,
    'Waist Circumference' = BMXWAIST_winsorized,
    'SAD' = BMDAVSAD_winsorized,
    'SADHtR' = SADHtR,
    'Direct HDL Cholesterol' = LBDHDDSI_winsorized,
    'Total Fat Mass (kg)' = DXDTOFAT_kg_winsorized,
    'ABSI' = ABSI,
    'VAI' = VAI,
    'FMI' = FMI,
    'LAP' = LAP,
    'BRI' = BRI,
    'WHtR' = WHtR,
    'Race/Ethnicity' = RIDRETH3_factor,
    'Household Income' = INDHHIN2_factor,
    #'Occupation' = OCD150_factor,
    'Occupation' = Occupation_combined,
    'Education Level' = DMDEDUC2_factor,
    'Marital Status' = DMDMARTL_factor,
    'Alcohol Consumption' = alcohol_consumption,
    'Smoking Status' = smoking_status,
    'Vitamin D (ng/mL)' = LBXVIDMS_winsorized,
    'Triglycerides (mg/dL)' = LBDSTRSI_winsorized,
    #'HDL Cholesterol (mg/dL)' = LBDHDDSI_winsorized,
    'Total Energy Intake (kcal)' = DR1TKCAL_winsorized,
    'Percent Body Fat' = DXDTOPF_winsorized,
    'Depression Score (PHQ-9)' = Depression_score_PHQ_9,
    'Activity Level' = activity_level
  )

dat_missingness_plot %>% vis_miss() +
  theme(plot.margin = unit(c(1, 2, 1, 1), "cm")) +  # Increase margins (top, right, bottom, left)
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# SAVE imputed data---------
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
saveRDS(df_an1_imputed, paste0(parent_directory, "/4_DATA/df_an1_imputed.RDS"))

# READ imputed data----------
# (current_directory <- getwd())
# (parent_directory <- dirname(current_directory))
# df_an1_imputed <- readRDS(paste0(parent_directory, "/4_DATA/df_an1_imputed.RDS")) # "df_an1_imputed.RDS"




# 5) MODELS (for BMI)----

# Define WEIGHTS--------
# https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx
# When combining two or more two-year cycles from 2001–2002 onward,
# new multi-year sample weights can be computed by simply dividing the 
# two-year sample weights by the number of two-year cycles in the analysis.
# Formulas are provided in the table below.wtmec4yr

file_path <- file.path(parent_directory, "4_DATA", "DEMO_G_2011_12.XPT")
df1112_demo <- read_xpt(file_path)
file_path <- file.path(parent_directory, "4_DATA", "DEMO_H_2013_14.XPT")
df1314_demo <- read_xpt(file_path)

df1112_demo$WTMEC2YR
df1112_demo$SDDSRVYR
df1314_demo$WTMEC2YR

df$SDDSRVYR
unique(df$SDDSRVYR) # 1, 2
df$WTMEC4YR <- ifelse(df$SDDSRVYR %in% c(7,8), df$WTMEC2YR/2, NA) # https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx
# hist(df$WTMEC4YR) 

# WEIGHTS____Survey design object---------
df_an1_imputed <- df_an1_imputed %>%
  left_join(df %>% dplyr::select(SEQN, WTMEC4YR, SDMVPSU, SDMVSTRA), by = "SEQN") # df is the combined data set from the beginning of the file
nhanesDesign <- svydesign(id = ~SDMVPSU,    # PSU: Primary Sampling Units
                          strata = ~SDMVSTRA,  # Stratification variable
                          weights = ~WTMEC4YR,  # 4-year MEC weight
                          nest = TRUE,         # Indicating nesting of PSUs within strata
                          data = df_an1_imputed)

# design object for unimputed data set:
df_an1 <- df_an1 %>%
  left_join(df %>% dplyr::select(SEQN, WTMEC4YR, SDMVPSU, SDMVSTRA), by = "SEQN")
df_an1$SDMVPSU
nhanesDesign_unimputed <- svydesign(id = ~SDMVPSU,    # PSU: Primary Sampling Units
                          strata = ~SDMVSTRA,  # Stratification variable
                          weights = ~WTMEC4YR,  # 4-year MEC weight
                          nest = TRUE,         # Indicating nesting of PSUs within strata
                          data = df_an1)


# Model 1, no weights: Raw (unadjusted)--------
model1 <- lm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand, 
             data = df_an1_imputed)
summary(model1)
coef(model1)[names(coef(model1)) == "SRI_no_imputation_winsorized_rand"]
check_model(model1) # not that bad, residuals could be better,
qqPlot(model1, main = "QQ Plot of Model 1", envelope = TRUE, level = 0.9) # not so nice; according to simulation this is not normal (see "./z_stuff/sim_qqPlot_under_normal.R").
pred <- ggpredict(model1, terms = "SRI_no_imputation_winsorized_rand [0:100]", 
                  back_transform = FALSE,  typical = "mean") # typical=mean takes the mean of the covariates for the adjusted prediction; Names of those terms from model, for which predictions should be displayed (so called focal terms).
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
#qqPlot(model1_BMI_winsorized, main="QQ Plot of Model 1", envelope=TRUE) 
# Decreasing, ~ 4 points over SRI Range


# _Parameter interpretation:-------
# A one unit increase in SRI_no_imputation_winsorized
# is associated with a (1-exp(-0.001578833 ))*100 = 0.1577% decrease in BMI

# try raw, without log()
# model1_ <- lm(BMXBMI ~ SRI_no_imputation_winsorized_rand, 
#              data = df_an1_imputed)
# summary(model1_)
# check_model(model1_)
# qqPlot(model1_, main = "QQ Plot of Model 1", envelope = TRUE, level = 0.9) # BAD! definitely not normal.
# 
# # Compare log likelihoods
# logLik(model1) - sum(log(df_an1_imputed$BMXBMI)) # (p. 132 Understanding Regression Analysis, Peter Westfall) higher LL
# logLik(model1_) 

# Boxcox transformation
#boxcox(model1_) # ~ - 0.6? # -> 0 not in CI -> log model does not seem reasonable
#model_boxcox <- lm(BMXBMI^(-0.6) ~ SRI_no_imputation_winsorized, 
#                   data = df_an1_imputed)
#summary(model_boxcox)
#check_model(model_boxcox)


# Model 1 with survey WEIGHTS--------
model1_svy <- svyglm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand, 
                     design = nhanesDesign)
summary(model1_svy)
summary(model1)
coef(model1_svy)[names(coef(model1_svy)) == "SRI_no_imputation_winsorized"]
coef(model1)[names(coef(model1)) == "SRI_no_imputation_winsorized"]

# Compare qq Plots without and with weights:
qqPlot(model1, main="QQ Plot of Model 1", envelope = TRUE, level = 0.9) # not so nice
qqPlot(model1_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # a bit worse than unweighted, not so normal


# Model 2, no weights: Age and Sex adjusted------
model2 <- lm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand + RIDAGEYR + RIAGENDR_factor, 
             data = df_an1_imputed)
summary(model2)
check_model(model2) # all green, residuals could be better
qqPlot(model2, main = "QQ Plot of Model 2", envelope = TRUE) # could be better, not so normal

pred <- ggpredict(model2, terms = "SRI_no_imputation_winsorized_rand [0:100]", 
                  back_transform = FALSE,  typical = "mean") # typical=mean takes the mean of the covariates for the adjusted prediction; Names of those terms from model, for which predictions should be displayed (so called focal terms).
pred # ref Male, mean Age = 51 years
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Gender = Male
# Falling relationship with SRI, about 4 BMI points over SRI Range - ~ 10% if starting with 30! 
# here, beta for SRI is relatively large compared to model 3 below which is
# the reason for the stronger decrease.
# (5-10% clinically relevant in the literature on losing weight)


# REPORT Model 1 in paper---------
# Model 2 with survey WEIGHTS--------
# REPORT MODEL 2 svy IN PAPER--------
model2_svy <- svyglm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand + RIDAGEYR + RIAGENDR_factor, 
                     design = nhanesDesign)
summary(model2_svy) # factor RIAGENDR_factorFemale estimate changed!
summary(model2)
coef(model2_svy)[names(coef(model2_svy)) == "SRI_no_imputation_winsorized_rand"]
coef(model2)[names(coef(model2)) == "SRI_no_imputation_winsorized_rand"]
qqPlot(model2, main = "QQ Plot of Model 2", envelope = TRUE, level = 0.9) # not so nice
qqPlot(model2_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # a bit worse than unweighted, not so normal

# _Predictions (mod 2 with survey weights)--------
# __all--------
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")
SRI_range <- seq(0, 100, length.out = 8)  # You can adjust this range as needed
tic()
pred_svy <- bootstrap_predictions_svyglm(model2_svy, df_an1_imputed, 
                                         SRI_range, Sex = "Male", n_boot = 300)
toc() # ~ 10s
ggplot(pred_svy, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.3) +
  labs(title = "(Bootstrapped) Predicted BMI based on SRI (Survey-Weighted)",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Overall relationship decreasing, over range of SRI only ~2.5 points BMI less!

# __Vary Sex and ethnicity---------
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)
SRI_range <- seq(0, 100, by = 5)

# Predict for mean age
mean(df_an1_imputed$RIDAGEYR, na.rm = TRUE) # 51.32689 (raw)
svymean(~RIDAGEYR, nhanesDesign, na.rm = TRUE) # 49.775 (with survey weights)
# -> this was defined in all of the functions bootstrap_predictions_.....
tic()
bootstrap_summary <- bootstrap_predictions_svyglm_sex_vary(model2_svy, 
                                                           df_an1_imputed, 
                                                           SRI_range, sex_levels, 
                                                           n_boot = 300)
toc() # 14s
bootstrap_summary

ggplot(bootstrap_summary, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(size = 1, color = "blue") +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.4) +  # Shaded confidence interval
  facet_wrap(~RIAGENDR_factor, scales = "fixed") +  # Facet by sex and ethnicity
  labs(title = "(Boostrapped) Predictions of BMI stratified by Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
# -> all decreasing
# Note, that equal slopes for SRI are forced, since no interaction effects

# Note: To improve model fit further, one could consider transforming the
# inputs more. According to Gelman (p.46), normality of errors is the least
# important assumption.



# Model 3, no weights: Fully adjusted model (according to DAG)----------
# Adjustment set according to dag:
# { Age, Alcohol, CaloricIntake, Depression, Education, Ethnicity, 
# H.H.income, Mar.Status, Occupation, Phys.Activity, Sex, Smoking, Vitamin D }
model3 <- lm(log(BMXBMI) ~ 
              SRI_no_imputation_winsorized_rand +           # SRI (exposure)
              RIDAGEYR +                    # Age
              RIAGENDR_factor +             # Sex
              RIDRETH3_factor +             # Ethnicity
              DMDEDUC2_factor +             # Education
              INDHHIN2_factor +             # H.H.income
              OCD150_factor +               # Occupation (last week)
              #Avg_Systolic_BP +            # Systolic BP
              alcohol_consumption +         # Alcohol use frequency
              smoking_status +              # Smoking status
              LBXVIDMS +                    # 25-hydroxyvitamin D
              DR1TKCAL_winsorized +                    # Total caloric intake
              #DIQ010_factor +              # Diabetes diagnosis
              Depression_score_PHQ_9 +      # Depression score (PHQ-9)
              #Marital_status_collapsed +   # Marital Status (definition see above)
              DMDMARTL_factor +             # Marital Status
              activity_level,               # Physical activity
              #LBDHDDSI,                    # Direct HDL Cholesterol
            data = df_an1_imputed)
summary(model3)

check_model(model3) # all green, residuals could be better
qqPlot(model3, main = "QQ Plot of Model 3", envelope = TRUE, level = 0.9) # not so bad; visually better than the previous models, slight deviation on one side, still not so normal

# _Visualize predictions (mod 3 no weights)--------
# _1) all------
pred <- ggpredict(model3, terms = "SRI_no_imputation_winsorized_rand [0:100]", 
                  back_transform = FALSE,  typical = "median") # typical=mean takes the mean of the covariates for the adjusted prediction; Names of those terms from model, for which predictions should be displayed (so called focal terms).
pred # note the adjustments - some reference levels are not so representative (heavy drinker, heavy smoker.....)

get_factor_reference_levels(df_an1_imputed)

ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Overall relationship falling, but over the entire range of SRI only ~2 points BMI less!

# try modal values and mean age:
pred <- ggpredict(model3, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIDAGEYR [51.3]"), 
                  back_transform = FALSE,  typical = "mode") 
pred # (Female, White, College, 100k over, Working, heavy Drinker, Non smoker)
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Decreasing relationship: ~2 points -> qualitatively the same as for the initial (unrealistic) levels

# _2) Sex and ethnicity vary--------
pred <- ggpredict(model3, terms = c("SRI_no_imputation_winsorized_rand [0:100]", 
                                    "RIAGENDR_factor", 
                                    "RIDRETH3_factor"),back_transform = FALSE, typical = "mean")
pred
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Gender and Ethnicity",
       x = "SRI",
       y = "Predicted BMI",
       color = "Gender",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~facet)
# Note that we force equal slopes since no interaction effects are included.
# -> Overall, a decreasing relationship with shifts in Sex and ethnicity is observed.
# -> Female higher than Male across all ethnicities.
# -> lowest ethnic group are Non-Hispanic Asians

# REPORT Model 2 in paper---------
# Model 3 with survey WEIGHTS--------
model3_svy <- svyglm(log(BMXBMI) ~ 
                       SRI_no_imputation_winsorized_rand +   # SRI (exposure)
                       RIDAGEYR +                    # Age
                       RIAGENDR_factor +             # Sex
                       RIDRETH3_factor +             # Ethnicity
                       DMDEDUC2_factor +             # Education
                       INDHHIN2_factor +             # H.H.income
                       OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +            # Systolic BP
                       alcohol_consumption +         # Alcohol use frequency
                       smoking_status +              # Smoking status
                       LBXVIDMS +                    # 25-hydroxyvitamin D
                       DR1TKCAL_winsorized +                    # Total caloric intake
                       #DIQ010_factor +              # Diabetes diagnosis
                       Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +   # Marital Status (definition see above)
                       DMDMARTL_factor +             # Marital Status
                       activity_level,               # Physical activity
                     #LBDHDDSI,                      # Direct HDL Cholesterol
                   design = nhanesDesign)
summary(model3_svy)


# COMPARE beta-coefficients of SRI in Models 1-3 and 1-3 svy------
df_coefs <- data.frame(
  Models = 1:3, 
  coefs = c(beta_SRI_mod1 = coef(model1)[names(coef(model1)) == "SRI_no_imputation_winsorized_rand"],
            beta_SRI_mod2 = coef(model2)[names(coef(model2)) == "SRI_no_imputation_winsorized_rand"],
            beta_SRI_mod3 = coef(model3)[names(coef(model3)) == "SRI_no_imputation_winsorized_rand"]),
  
  coefs_svy = c(beta_SRI_mod1_svy = coef(model1_svy)[names(coef(model1_svy)) == "SRI_no_imputation_winsorized_rand"],
                beta_SRI_mod2_svy = coef(model2_svy)[names(coef(model2_svy)) == "SRI_no_imputation_winsorized_rand"],
                beta_SRI_mod3_svy = coef(model3_svy)[names(coef(model3_svy)) == "SRI_no_imputation_winsorized_rand"]),
  
  lower_ci = c(confint(model1)["SRI_no_imputation_winsorized_rand", 1],
               confint(model2)["SRI_no_imputation_winsorized_rand", 1],
               confint(model3)["SRI_no_imputation_winsorized_rand", 1]),
  
  upper_ci = c(confint(model1)["SRI_no_imputation_winsorized_rand", 2],
               confint(model2)["SRI_no_imputation_winsorized_rand", 2],
               confint(model3)["SRI_no_imputation_winsorized_rand", 2]),
  
  # Confidence intervals for the survey-weighted models
  lower_ci_svy = c(confint(model1_svy)["SRI_no_imputation_winsorized_rand", 1],
                   confint(model2_svy)["SRI_no_imputation_winsorized_rand", 1],
                   confint(model3_svy)["SRI_no_imputation_winsorized_rand", 1]), # NA
  
  upper_ci_svy = c(confint(model1_svy)["SRI_no_imputation_winsorized_rand", 2],
                   confint(model2_svy)["SRI_no_imputation_winsorized_rand", 2],
                   confint(model3_svy)["SRI_no_imputation_winsorized_rand", 2]) # NA
)
ggplot(df_coefs, aes(x = Models)) +
  geom_line(aes(y = coefs), color = "black") +      # Line for the regular models
  geom_point(aes(y = coefs), color = "black") +     # Points for the regular models
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, color = "black") + # Error bars for the regular models
  geom_line(aes(y = coefs_svy), color = "blue") +   # Line for the survey models
  geom_point(aes(y = coefs_svy), color = "blue") +  # Points for the survey models
  geom_errorbar(aes(ymin = lower_ci_svy, ymax = upper_ci_svy), width = 0.1, color = "blue") + # Error bars for survey models
  ggtitle("SRI-coefficients (for log(BMI)) in Models 1-3 (Unweighted and Survey-weighted (blue))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Model number") + ylab("SRI-beta-coefficient") +
  scale_x_continuous(breaks = 1:3)
# -> beta coefficient for SRI gets closer to 0 in Model 3 compared to Models 1 and 2.
# -> weighted coefficients are further away from 0 compared to unweighted








# (bootstrap-)confidence intervals for coefficients see:
source("./z_stuff/bootstrap_CIs_for_svy_models.R")
tic()
bootstrap_svyglm(model3_svy, df_an1_imputed, 
                 n_boot = 200, seed = 136)
toc() # 70s (n_boot=1000), 10s (n_boot=100)
# HH income under 20k USD shows very wide CIs
# -> Maybe standardize continuous variables (SRI, VitaminD, Depression, activity) - 
#    now they are all on different scales and ordered categorical and nominal variables are mixed....

# _Interpretation of SRI-coefficient in fully adjusted model (model3_svy):------
# *) A one-unit increase in SRI_no_imputation_winsorized is associated with 
#    a (1-exp(-6.923e-04))*100 = 0.0692% decrease in BMI.
# *) a 20-unit increase in SRI_no_imputation_winsorized is associated with
#    a (1-exp(-6.923e-04*20))*100 = 1.375059% decrease in BMI.
# *) a 50-unit increase in SRI_no_imputation_winsorized is associated with
#    a (1-exp(-6.923e-04*50))*100 = 3.4022% decrease in BMI.
# *) a 100-unit increase in SRI_no_imputation_winsorized is associated with
#    a (1-exp(-6.923e-04*100))*100 = 6.688796% decrease in BMI.
# -> This should not constitute a relevant effect size.
#    (see https://pmc.ncbi.nlm.nih.gov/articles/PMC4212133/pdf/12916_2014_Article_175.pdf)

# _Visualize predictions--------
# _1) all------
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")
SRI_range <- seq(0, 100, length.out = 8)  # You can adjust this range as needed
# Male:
tic()
pred_svy <- bootstrap_predictions_svyglm(model3_svy, df_an1_imputed, 
                                         SRI_range, Sex = "Male", n_boot = 300)
toc() # ~ 20s
ggplot(pred_svy, aes(x = SRI_no_imputation_winsorized, y = exp(Predicted_BMI))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.3) +
  labs(title = "(Bootstrapped) Predicted BMI based on SRI (Survey-Weighted)",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal()
# Overall relationship decreasing, over range of SRI only ~2.5 points BMI less!

# Female:
tic()
pred_svy <- bootstrap_predictions_svyglm(model3_svy, df_an1_imputed, 
                                         SRI_range, Sex = "Female", n_boot = 300)
toc() # ~ 20s
ggplot(pred_svy, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.3) +
  labs(title = "(Bootstrapped) Predicted BMI based on SRI (Survey-Weighted)",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal()
# Overall relationship falling, but over the entire range of SRI only ~2.5 points BMI less!

# _2) Sex varied-----
# this uses a different bootstrap function now:
SRI_range <- seq(0, 100, length.out = 6)
sex_levels <- c("Male", "Female")
tic()
pred_svy_sex <- bootstrap_predictions_svyglm_sex_vary(model3_svy, df_an1_imputed, 
                                                      SRI_range, sex_levels, n_boot = 300)
toc() # 
pred_svy_sex
ggplot(pred_svy_sex, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI), color = RIAGENDR_factor)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper), fill = RIAGENDR_factor), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Sex",
       x = "SRI",
       y = "Predicted BMI",
       color = "Sex",
       fill = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~RIAGENDR_factor)
# Both decreasing - consistent with previous plots



# COMPARE effect sizes for model 3 (with/without survey weights)-------
model3_svy_tidy <- tidy(model3_svy)
model3_tidy <- tidy(model3)
comparison_df <- model3_svy_tidy %>%
  dplyr::select(term, estimate_svy = estimate, std.error_svy = std.error) %>%
  full_join(model3_tidy %>%
              dplyr::select(term, estimate_lm = estimate, std.error_lm = std.error), 
            by = "term")
comparison_df %>%
  kable(digits = 3, caption = "Comparison of Coefficients: Survey-Weighted vs. Unweighted Model")

comparison_df %>% ggplot(aes(x = estimate_svy, y = estimate_lm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Survey-Weighted Estimate", y = "Unweighted Estimate") +
  theme_minimal()+
  ggtitle("Comparison of Coefficients: Survey-Weighted vs. Unweighted Model") +
theme(plot.title = element_text(hjust = 0.5))
#__without large effect:----
comparison_df %>% 
  dplyr::filter(estimate_svy <1) %>% 
  ggplot(aes(x = estimate_svy, y = estimate_lm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Survey-Weighted Estimate", y = "Unweighted Estimate") +
  theme_minimal() + 
  ggtitle("Comparison of Coefficients: Survey-Weighted vs. Unweighted Model") +
  theme(plot.title = element_text(hjust = 0.5))
# -> rather close



# Model 4, no weights: Fully adj w int for Sex-----------
# Now we are allowing different slopes for Sex/Gender.
# Slopes within different Ethnicities are still forced to be equal.
model4 <- lm(log(BMXBMI_winsorized) ~ 
               SRI_no_imputation_winsorized_rand*RIAGENDR_factor +           # SRI (exposure)
               RIDAGEYR +                    # Age
               #RIAGENDR_factor +             # Sex
               RIDRETH3_factor +             # Ethnicity
               DMDEDUC2_factor +             # Education
               INDHHIN2_factor +             # H.H.income
               OCD150_factor +               # Occupation (last week)
               #Avg_Systolic_BP +            # Systolic BP
               alcohol_consumption +         # Alcohol use frequency
               smoking_status +              # Smoking status
               LBXVIDMS +                    # 25-hydroxyvitamin D
               #DIQ010_factor +              # Diabetes diagnosis
               Depression_score_PHQ_9 +      # Depression score (PHQ-9)
               DR1TKCAL_winsorized +                    # Total caloric intake
               #Marital_status_collapsed +   # Marital Status (definition see above)
               DMDMARTL_factor +             # Marital Status
               activity_level,                # Physical activity
             #LBDHDDSI,                    # Direct HDL Cholesterol
             data = df_an1_imputed)
summary(model4)
check_model(model4) # all green, bump in PPC is because of winsorizing.
qqPlot(model4, main = "QQ Plot of Model 4", envelope = TRUE) # slight deviation on one side
anova(model3, model4) # very small p; 2 points less RSS
# p = 2.761e-09 


# Visualize predictions (Model 4)--------
# _1) all----
# Male:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIAGENDR_factor [Male]"), # note that Female gives a negative slope
                  back_transform = FALSE, typical = "mean")
pred # unrealistic reference levels
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Slightly increasing (!) Relationship as opposed to model 3.
summary(model4)
# main interaction of SRI not "significant", but has to remain in the model due to variable inclusion principle.

# try modal values and mean age:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIDAGEYR [51.3]"), 
                  back_transform = FALSE,  typical = "mode")
pred # Female now
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# ~4 points decrease over SRI range

# Female:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIAGENDR_factor [Female]"), # note that Female gives a negative slope
                  back_transform = FALSE, typical = "mean")
pred
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Decreasing relationship ~ 5 points over the SRI range


# _2) Sex/Gender varied-----
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]", 
                                    "RIAGENDR_factor"), back_transform = FALSE)
pred # unrealistic reference levels
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Gender", 
       x = "SRI", 
       y = "Predicted BMI", 
       color = "Gender",      # Add legend title for color
       fill = "Gender") +     # Add legend title for fill
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# A falling relationship for females (~5 BMI points over SRI range - clinically relevant),
# but a slightly increasing for males.

# try modal values and mean age:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIDAGEYR [51.3]"), 
                  back_transform = FALSE,  typical = "mode")
pred # Female
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI") +
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# ~4 points decrease over SRI range


# _3) Ethnicity varied-------
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]", 
                                    "RIDRETH3_factor"), back_transform = FALSE, typical = "mean")
pred # note the adjustments
# Male:
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Ethnicity") +
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal()
# Ref: male (reason for increasing). Non-Hispanic Asian ethnicity
# is shifted downwards in (predicted) BMI.
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) + # visually better discernable
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Ethnicity") +
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal() + 
  facet_wrap(~group)

# try modal values and mean age:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIDAGEYR [51.3]"), 
                  back_transform = FALSE,  typical = "mode")
pred
ggplot(pred, aes(x = x, y = exp(predicted), color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = 0.2) +
  labs(title = "Predicted BMI") +
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# ~4 points decrease over SRI range

# _4) Vary both Sex and Ethnicity------
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]", 
                                    "RIAGENDR_factor", 
                                    "RIDRETH3_factor"), back_transform = FALSE, typical = "mean")
ggplot(pred, aes(x = x, y = exp(predicted), color = interaction(group, facet))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = interaction(group, facet)), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Gender and Ethnicity",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI",
       color = "Gender & Ethnicity",
       fill = "Gender & Ethnicity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ group + facet, scales = "fixed")  # Ensure same y-axis scale across all facets
# This needs to be explained: increasing relationship for males, 
# decreasing for females (up to 6 BMI points over the SRI range)
# We could try to "verify" that quickly in a stratified Model 3 (see Sex_stratified_Model3.R) --> could not be verified!!
# Note that we force equal slopes within Sex/Ethnicity-subsets -> one could try random effects

# try modal values and mean age:
pred <- ggpredict(model4, terms = c("SRI_no_imputation_winsorized_rand [0:100]", 
                                    "RIAGENDR_factor", 
                                    "RIDRETH3_factor",
                                    "RIDAGEYR [51.3]"), back_transform = FALSE, typical = "mode")
pred
ggplot(pred, aes(x = x, y = exp(predicted), color = interaction(group, facet))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = interaction(group, facet)), alpha = 0.2) +
  labs(title = "Predicted BMI based on SRI, by Gender and Ethnicity",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI",
       color = "Gender & Ethnicity",
       fill = "Gender & Ethnicity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ group + facet, scales = "fixed")  # Ensure same y-axis scale across all facets
# -> still slightly increasing in Male


# Model 4 with survey weights--------
# REPORT as model 3 in paper-----------
model4_svy <- svyglm(log(BMXBMI_winsorized) ~ 
                      SRI_no_imputation_winsorized_rand*RIAGENDR_factor +           # SRI (exposure)
                      RIDAGEYR +                    # Age
                      #RIAGENDR_factor +
                      RIDRETH3_factor +
                      DMDEDUC2_factor +
                      INDHHIN2_factor +
                      OCD150_factor +
                      #Avg_Systolic_BP +
                      alcohol_consumption +
                      smoking_status +
                      LBXVIDMS +
                      #DIQ010_factor +
                      Depression_score_PHQ_9 +
                      DR1TKCAL_winsorized +
                      #Marital_status_collapsed +
                      DMDMARTL_factor +
                      activity_level,
                    #LBDHDDSI,
                    design = nhanesDesign)
summary(model4_svy)

#tic() # Bootstrap CIs for coefficients:
#bootstrap_svyglm(model4_svy, df_an1_imputed, n_boot = 300)
#toc() # 79
# Caution when interpreting coefficients in this model!
# Note that we cannot compare the beta-coefficients for the interaction term-model with
# the previous models. We could compare the predictions.

# _Visualize predictions (Model 4 weights)--------
# Male:
tic() 
pred_svy_4 <- bootstrap_predictions_svyglm(model4_svy, df_an1_imputed, 
                                        SRI_range, Sex = "Male", n_boot = 300)
toc() # ~ 20s
ggplot(pred_svy_4, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.3) +
  labs(title = "(Bootstrapped) Predicted BMI based on SRI (Survey-Weighted)",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal()
# -> Very slightly decreasing (consistent with unweighted model 4), wide CIs

# Female:
tic() 
pred_svy_4 <- bootstrap_predictions_svyglm(model4_svy, df_an1_imputed,
                                           SRI_range, Sex = "Female", n_boot = 300)
toc() # ~ 20s
ggplot(pred_svy_4, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.3) +
  labs(title = "(Bootstrapped) Predicted BMI based on SRI (Survey-Weighted)",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal()
# -> Up to 5 points decreasing over SRI range (consistent with unweighted model 4)
# -> Clearly different compared to Male

# Sex varied:
tic() 
sex_levels <- c("Male", "Female")
pred_svy <- bootstrap_predictions_svyglm_sex_vary(model4_svy, df_an1_imputed, 
                                                  SRI_range, sex_levels, n_boot = 400)
toc() # 48s
ggplot(pred_svy, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI), color = RIAGENDR_factor)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper), fill = RIAGENDR_factor), alpha = 0.2) +
  labs(title = "Predicted BMI") + 
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~RIAGENDR_factor)
# -> Both decreasing, Female ~ 5 points, male very slightly decreasing
# -> Consistent with previous 2 plots


# Model 5, no weights: Fully adj w interaction for Ethnicity----------
# Now we assume constant slopes for the 2 Genders and
# vary slopes for Ethnicity
model5 <- lm(log(BMXBMI) ~ 
               SRI_no_imputation_winsorized_rand*RIDRETH3_factor +           # SRI (exposure)
               RIDAGEYR +                    # Age
               RIAGENDR_factor +             # Sex
               #RIDRETH3_factor +             # Ethnicity
               DMDEDUC2_factor +             # Education
               INDHHIN2_factor +             # H.H.income
               OCD150_factor +               # Occupation (last week)
               #Avg_Systolic_BP +            # Systolic BP
               alcohol_consumption +         # Alcohol use frequency
               smoking_status +              # Smoking status
               LBXVIDMS +                    # 25-hydroxyvitamin D
               DR1TKCAL_winsorized +                    # Total caloric intake
               #DIQ010_factor +              # Diabetes diagnosis
               Depression_score_PHQ_9 +      # Depression score (PHQ-9)
               #Marital_status_collapsed +   # Marital Status (definition see above)
               DMDMARTL_factor +             # Marital Status
               activity_level,                # Physical activity
             #LBDHDDSI,                    # Direct HDL Cholesterol
             data = df_an1_imputed)
summary(model5)
anova(model3, model5) # rather small p
# p = 0.0007638
check_model(model5) # all green
qqPlot(model5, main = "QQ Plot of Model 5", envelope=TRUE) # not so bad, slight deviation on one side

# _Visualize predictions (model 5 no weights)--------
pred <- ggpredict(model5, terms = c("SRI_no_imputation_winsorized_rand [0:100]"), 
                  back_transform = FALSE, typical = "mean")
pred # Male
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# -> neither increasing nor decreasing over SRI range

# try modal values and mean age:
pred <- ggpredict(model5, terms = c("SRI_no_imputation_winsorized_rand [0:100]",
                                    "RIDAGEYR [51.3]"), 
                  back_transform = FALSE,  typical = "mode")
pred # Female
ggplot(pred, aes(x = x, y = exp(predicted))) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high)), fill = "lightblue", alpha = 0.3) +
  labs(title = "Predicted BMI based on SRI",
       x = "SRI",
       y = "Predicted BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# -> decreasing ~ 4 points over SRI range


# Model 5 with survey weights--------
# Equal slopes for Sex/Gender are enforced
model5_svy <- svyglm(log(BMXBMI) ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor +           # SRI (exposure)
                       RIDAGEYR +                    # Age
                       RIAGENDR_factor +
                       #RIDRETH3_factor +
                       DMDEDUC2_factor +
                       INDHHIN2_factor +
                       OCD150_factor +
                       #Avg_Systolic_BP +
                       alcohol_consumption +
                       smoking_status +
                       LBXVIDMS +
                       #DIQ010_factor +
                       Depression_score_PHQ_9 +
                       DR1TKCAL_winsorized +
                       #Marital_status_collapsed +
                       DMDMARTL_factor +
                       activity_level,
                     #LBDHDDSI,
                     design = nhanesDesign)
summary(model5_svy)
qqPlot(model5_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # a bit worse than unweighted, not so normal
# bootstrap CIs:
#tic()
#bootstrap_svyglm(model5_svy, df_an1_imputed)
#toc() # 79


# _Visualize predictions (Model 5 with weights)--------
# Sex varied:
tic() 
sex_levels <- c("Male", "Female")
pred_svy <- bootstrap_predictions_svyglm_sex_vary(model5_svy, df_an1_imputed, 
                                                  SRI_range, sex_levels, n_boot = 300)
toc() # 48s
pred_svy
ggplot(pred_svy, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI), color = RIAGENDR_factor)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper), fill = RIAGENDR_factor), alpha = 0.2) +
  labs(title = "Predicted BMI") + 
  xlab("SRI") + ylab("Predicted BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~RIAGENDR_factor)
# -> Slopes are identical as per assumption.
# -> Increasing relationship both Male Female, ~ 1.5-2 points over SRI range
#    This could be since "Mexican American" is the reference level and 
#    Females in this group have only a slight decrease according to Model 6. So in sum,
#    this could be increasing within this group



# Ethnicity varied:
ethnicity_levels <- c("Mexican American", "Non-Hispanic White", "Non-Hispanic Black")  # Customize based on your data
SRI_range <- seq(0, 100, by = 5)  # Customize the range of SRI
tic()
bootstrap_summary <- bootstrap_predictions_svyglm_ethnicity_varied(model5_svy, 
                                                                   df_an1_imputed, 
                                                                   SRI_range, 
                                                                   ethnicity_levels, n_boot = 100)
toc() # 21s
# ERROR?
bootstrap_summary

ggplot(bootstrap_summary, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI))) +
  geom_line(size = 1, color = "blue") +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.4) +  # Shaded confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "free_y") +  # Facet by ethnicity with independent y-scales
  labs(title = "Predicted BMI vs SRI for Different Ethnic Groups",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  
# -> decreasing ~ 2.5 points over SRI range

# Sex and Ethnicity varied:
# Define the levels for sex and ethnicity
sex_levels <- c("Male", "Female")
ethnicity_levels <- c("Mexican American", "Non-Hispanic White", "Non-Hispanic Black")
SRI_range <- seq(0, 100, by = 5)
tic()
bootstrap_summary <- bootstrap_predictions_svyglm_sex_ethnicity_varied(model3_svy, 
                                                                       df_an1_imputed, 
                                                                       SRI_range, sex_levels, 
                                                                       ethnicity_levels, n_boot = 100)
toc() # 40s
bootstrap_summary

ggplot(bootstrap_summary, aes(x = SRI_no_imputation_winsorized, y = exp(Predicted_BMI))) +
  geom_line(size = 1, color = "blue") +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), fill = "lightblue", alpha = 0.4) +  # Shaded confidence interval
  facet_wrap(~RIAGENDR_factor + RIDRETH3_factor, scales = "fixed") +  # Facet by sex and ethnicity
  labs(title = "Predicted BMI vs SRI by Sex and Ethnicity",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
# -> all decreasing





# [Model 4 in paper]
# Model 6: BMI [fair model fit], with weights: Fully adj w interaction for Sex and Ethnicity------
# REPORT as model 4 in paper---------------
model6_svy <- svyglm(log(BMXBMI_winsorized) ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                       SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                       RIDAGEYR +                    # Age
                       #RIAGENDR_factor +
                       #RIDRETH3_factor +
                       DMDEDUC2_factor +             # Education
                       INDHHIN2_factor +             # H.H.income
                       OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +
                       alcohol_consumption +         # Alcohol use frequency
                       smoking_status +              # Smoking status
                       LBXVIDMS +                    # 25-hydroxyvitamin D
                       DR1TKCAL_winsorized +         # caloric intake
                       #DIQ010_factor +
                       Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +
                       DMDMARTL_factor +             # Marital Status
                       activity_level,               # Physical activity
                     #LBDHDDSI,
                     design = nhanesDesign)

summary(model6_svy)

# Model6_svy_without_adjustments_only_2_interactions---------------
model6_svy_without_adjustments_only_2_interactions <- svyglm(log(BMXBMI_winsorized) ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                       SRI_no_imputation_winsorized_rand*RIAGENDR_factor,        # SRI (exposure)
                       #RIDAGEYR +                    # Age
                       #RIAGENDR_factor +
                       #RIDRETH3_factor +
                       #DMDEDUC2_factor +             # Education
                       #INDHHIN2_factor +             # H.H.income
                       #OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +
                       #alcohol_consumption +         # Alcohol use frequency
                       #smoking_status +              # Smoking status
                       #LBXVIDMS +                    # 25-hydroxyvitamin D
                       #DR1TKCAL_winsorized +         # caloric intake
                       #DIQ010_factor +
                       #Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +
                       #DMDMARTL_factor +             # Marital Status
                       #activity_level,               # Physical activity
                     #LBDHDDSI,
                     design = nhanesDesign)

#_Visualize for internal use: model6_svy_without_adjustments_only_2_interactions)---------
# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)


# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for Waist Circumference
tic()
bootstrap_summary_BMI_without_interaction <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model6_svy_without_adjustments_only_2_interactions, 
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_BMI_without_interaction$RIDRETH3_factor <- factor(
  bootstrap_summary_BMI_without_interaction$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_BMI_without_interaction$RIAGENDR_factor <- factor(
  bootstrap_summary_BMI_without_interaction$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# change colname for WC
bootstrap_summary_BMI_without_interaction <- bootstrap_summary_BMI_without_interaction %>%
  rename(Predicted_BMI = Predicted_BMI)

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_BMI_without_interaction, aes(x = SRI_no_imputation_winsorized_rand, 
                                 y = exp(Predicted_BMI), # correct var 
                                 color = RIAGENDR_factor, 
                                 fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted BMI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted WC",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
# ...









# Model 6 without weights--------------
model6 <- lm(log(BMXBMI_winsorized) ~ 
               SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
               SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
               RIDAGEYR +                    # Age
               #RIAGENDR_factor +
               #RIDRETH3_factor +
               DMDEDUC2_factor +             # Education
               INDHHIN2_factor +             # H.H.income
               OCD150_factor +               # Occupation (last week)
               #Avg_Systolic_BP +
               alcohol_consumption +         # Alcohol use frequency
               smoking_status +              # Smoking status
               LBXVIDMS +                    # 25-hydroxyvitamin D
               DR1TKCAL_winsorized +         # caloric intake
               #DIQ010_factor +
               Depression_score_PHQ_9 +      # Depression score (PHQ-9)
               #Marital_status_collapsed +
               DMDMARTL_factor +             # Marital Status
               activity_level,               # Physical activity
             #LBDHDDSI,
             data = df_an1_imputed)
check_model(model6) # all green
# the bumps at around 52 BMI or so are due to winsorization

# _Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model6_svy, type = "pearson")
fitted_values <- fitted(model6_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) # some structure due to winsorization
# or
plot(model6_svy, which = 1) # Linearity, ok

# (2) Hetereoscedasticity
plot(model6_svy$fitted.values, abs(model6_svy$residuals)) # upward trend
data.frame(fitted_values = model6_svy$fitted.values, 
           abs_residuals = abs(model6_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
        
fit.glejser <- lm(abs(model6_svy$residuals) ~ model6_svy$fitted.values)
summary(fit.glejser) # suggests heteroscedasticity
tbl_regression(fit.glejser) 
plot(model6_svy$fitted.values, model6_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model6_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # ns

# (4) Normality
# should only be checked if one is reasonably comfortable with the constant variance assumption (Westfall p.113)
qqPlot(model6_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> rather strong deviations in the tails
hist(model6_svy$residuals) # nice
plot(model6_svy$residuals) # nice

mean_res <- mean(model6_svy$residuals)
sd_res <- sd(model6_svy$residuals)
data.frame(res = model6_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model6_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# -> One could try improve model fit further by using weighted least squares
# to model heteroscedasticity explicitly. 

# Compare predictions of survey-weighted model 6 vs. unweighted model 6
# _Visualize predictions (Model 6 no weights)-----
predict(model6)
predict(model6_svy)
plot(predict(model6), predict(model6_svy)) # 
              
unweighted_predictions <- exp(predict(model6))
weighted_predictions <- exp(as.numeric(predict(model6_svy)))
diffs <- abs(unweighted_predictions - weighted_predictions)
max_diff_index <- which.max(diffs)
max(diffs) # 2.040818
summary(diffs)

pred_df <- data.frame(
  unweighted_predictions = unweighted_predictions,
  weighted_predictions = weighted_predictions,
  diffs = diffs
)
cor(pred_df$unweighted_predictions, pred_df$weighted_predictions) # 0.9794637
ggplot(pred_df, aes(x = unweighted_predictions, y = weighted_predictions)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_point(aes(x = unweighted_predictions[max_diff_index], 
                 y = weighted_predictions[max_diff_index]), 
             color = "blue", size = 3) +  # Highlight max difference
  labs(title = "Comparison of BMI Predictions from Unweighted and Weighted Model 3",
       x = "Unweighted Predictions (BMI)",
       y = "Weighted Predictions (BMI)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# _Visualize predictions (Model 6 with weights)-------

# Sex and Ethnicity varied:
# _Adapt covariate levels:---------
# RIDAGEYR = 49.775 # survey weighted mean.

svymean(~RIDAGEYR, nhanesDesign, na.rm = TRUE) # 49.775
svyquantile(~RIDAGEYR, nhanesDesign, quantiles = 0.5, na.rm = TRUE) # 50

prop.table(svytable(~DMDEDUC2_factor, nhanesDesign)) # -> mode: "Some College or AA degree"

prop.table(svytable(~INDHHIN2_factor, nhanesDesign)) # -> mode: "$100,000 and Over"

prop.table(svytable(~OCD150_factor, nhanesDesign)) # -> mode: "Working at a job or business"

prop.table(svytable(~alcohol_consumption, nhanesDesign)) # -> mode: "Moderate Drinker"

prop.table(svytable(~smoking_status, nhanesDesign)) # -> mode: "Non_smokers"

svymean(~LBXVIDMS, nhanesDesign, na.rm = TRUE) # 71.255
svyquantile(~LBXVIDMS, nhanesDesign, quantiles = 0.5, na.rm = TRUE) # 68.3

prop.table(svytable(~Depression_score_PHQ_9, nhanesDesign)) # -> mode: 0
svymean(~Depression_score_PHQ_9, nhanesDesign, na.rm = TRUE) # 3.0373
svyquantile(~Depression_score_PHQ_9, nhanesDesign, quantiles = 0.5, na.rm = TRUE) # 1

prop.table(svytable(~DMDMARTL_factor, nhanesDesign)) # -> mode: "Married"

svymean(~activity_level, nhanesDesign, na.rm = TRUE) # 8.1682
svyquantile(~activity_level, nhanesDesign, quantiles = 0.5, na.rm = TRUE) # 7.772

svymean(~DR1TKCAL_winsorized, nhanesDesign, na.rm = TRUE) # 2142.6
svyquantile(~DR1TKCAL_winsorized, nhanesDesign, quantiles = 0.5, na.rm = TRUE) # 1991
# -> changed all the functions bootstrap_predictions_svyglm....

sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)
SRI_range <- seq(0, 100, by = 5)
getwd()
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")
tic()
bootstrap_summary <- bootstrap_predictions_svyglm_sex_ethnicity_varied(model6_svy, 
                                                                       df_an1_imputed, 
                                                                       SRI_range, sex_levels, 
                                                                       ethnicity_levels, n_boot = 300)
toc() # 300s
bootstrap_summary

ggplot(bootstrap_summary, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI), color = RIAGENDR_factor, fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Shaded confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted BMI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary$RIDRETH3_factor <- factor(
  bootstrap_summary$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

ggplot(bootstrap_summary, aes(x = SRI_no_imputation_winsorized_rand, y = exp(Predicted_BMI), color = RIAGENDR_factor, fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Shaded confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted BMI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted BMI",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# -> Slopes are now varying
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_BMI_strat_by_Sex_and_Ethnicity_BMI_wins_4.2.25.pdf")
ggsave(file_path)


# ANOVAS for interactions:--------
# We do not decide on these tests if we want to include the interaction term
#anova(model3_svy, model4_svy, test = "Chisq") # small p
#anova(model3_svy, model5_svy, test = "Chisq") # small p
anova(model3_svy, model6_svy, test = "Chisq") # small p = 5.347e-05 # no interaction compared to both interactions: sex*SRI + ethnicity*SRI
anova(model4_svy, model6_svy, test = "Chisq") # small p = 0.0021244  # interaction sex*SRI compared to both interactions: sex*SRI + ethnicity*SRI
anova(model5_svy, model6_svy, test = "Chisq") # small p = 0.00044679 # interaction for ethnicity*SRI compared to both interactions: sex*SRI + ethnicity*SRI



# Model 6 - better visualization-----------------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_BMI <- predict(model6_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_BMI <- exp(param_space$predicted_log_BMI)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_BMI, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_BMI, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_BMI, color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,  # Horizontal range for each bin
  #  y = q2_5, yend = q2_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 2.5% quantile
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,
  #  y = q97_5, yend = q97_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 97.5% quantile
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted BMI with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted BMI"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)


# MODELS for OTHER adiposity measures----

high_correlations
# -> BMI is highly correlated to many other variables like 
#    Total fat(high missingness), WHtR, BRI and WC.
# -> One could expect somewhat similar predictions. Although, SRI is not 
#    strongly associated with the adiposity outcomes and could therefore 
#    show different behaviour.

# BRI WHtR: r = 0.99 -> Checking the random predictions from the model. practically IDENTICAL.
# BMI and WHtR: r = 0.91 Checking the random predictions from the model. very similar!

# _Model 7: Waist circumference (WC) [good model fit]-----
hist(df_an1_imputed$BMXWAIST_winsorized) # skewed
hist(log(df_an1_imputed$BMXWAIST_winsorized)) # better?
model7_svy <- svyglm(log(BMXWAIST_winsorized) ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                       SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                       RIDAGEYR +                    # Age
                       #RIAGENDR_factor +
                       #RIDRETH3_factor +
                       DMDEDUC2_factor +             # Education
                       INDHHIN2_factor +             # H.H.income
                       OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +
                       alcohol_consumption +         # Alcohol use frequency
                       smoking_status +              # Smoking status
                       LBXVIDMS +                    # 25-hydroxyvitamin D
                       DR1TKCAL_winsorized +         # caloric intake
                       #DIQ010_factor +
                       Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +
                       DMDMARTL_factor +             # Marital Status
                       activity_level,               # Physical activity
                     #LBDHDDSI,
                     design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model7_svy, type = "pearson")
fitted_values <- fitted(model7_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) # structure due to winsorization
# or
plot(model7_svy, which = 1) # Linearity, ok

# (2) Hetereoscedasticity
plot(model7_svy$fitted.values, abs(model7_svy$residuals)) # upward trend
data.frame(fitted_values = model7_svy$fitted.values, 
           abs_residuals = abs(model7_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model7_svy$residuals) ~ model7_svy$fitted.values)
summary(fit.glejser) # no sign of heteroscedasticity
tbl_regression(fit.glejser) 
plot(model7_svy$fitted.values, model7_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model7_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model7_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> good
hist(model7_svy$residuals) # nice
plot(model7_svy$residuals) # nice

mean_res <- mean(model7_svy$residuals)
sd_res <- sd(model7_svy$residuals)
data.frame(res = model7_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model7_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # nice


# __Visualize WC predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_WC <- predict(model7_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_WC <- exp(param_space$predicted_log_WC)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_WC, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_WC, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_WC, color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,  # Horizontal range for each bin
  #  y = q2_5, yend = q2_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 2.5% quantile
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,
  #  y = q97_5, yend = q97_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 97.5% quantile
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted WC with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted WC"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_WC_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

# __Visualize for Suppl----------
# Extract weighted means/modes for covariates
RIDAGEYR_mean <- 49.775  # Weighted mean age
LBXVIDMS_mean <- 71.255  # Weighted mean Vitamin D
Depression_score_mean <- 3.0373  # Weighted mean Depression score
activity_level_mean <- 8.1682  # Weighted mean physical activity
caloric_intake_mean <- 2142.6  # Weighted mean calorie intake

# Most common categories (mode) from weighted tables
DMDEDUC2_mode <- "Some College or AA degree"
INDHHIN2_mode <- "$100,000 and Over"
OCD150_mode <- "Working at a job or business"
alcohol_mode <- "Moderate Drinker"
smoking_mode <- "Non_smokers"
marital_status_mode <- "Married"

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for Waist Circumference
tic()
bootstrap_summary_WC <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model7_svy, 
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_WC$RIDRETH3_factor <- factor(
  bootstrap_summary_WC$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_WC$RIAGENDR_factor <- factor(
  bootstrap_summary_WC$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# change colname for WC
bootstrap_summary_WC <- bootstrap_summary_WC %>%
  rename(Predicted_WC = Predicted_BMI)

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_WC, aes(x = SRI_no_imputation_winsorized_rand, 
                              y = exp(Predicted_WC), # correct var 
                              color = RIAGENDR_factor, 
                              fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted BMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted WC by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted WC",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_WC_strat_by_Sex_and_Ethnicity_WC_9.2.25.pdf")

ggsave(file_path)



# _Model 8: ABSI A Body shape Index (ABSI) [good model fit]----
hist(df_an1$ABSI, breaks = 30) # symmetric
table(is.na(df_an1$ABSI)) 
cor(df_an1$ABSI, df_an1$BMXBMI_winsorized, use = "complete.obs") # -0.03034775
model8_svy <- svyglm(ABSI ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                       SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                       RIDAGEYR +                    # Age
                       #RIAGENDR_factor +
                       #RIDRETH3_factor +
                       DMDEDUC2_factor +             # Education
                       INDHHIN2_factor +             # H.H.income
                       OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +
                       alcohol_consumption +         # Alcohol use frequency
                       smoking_status +              # Smoking status
                       LBXVIDMS +                    # 25-hydroxyvitamin D
                       DR1TKCAL_winsorized +         # caloric intake
                       #DIQ010_factor +
                       Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +
                       DMDMARTL_factor +             # Marital Status
                       activity_level,               # Physical activity
                     #LBDHDDSI,
                     design = nhanesDesign)
head(predict(model8_svy))

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model8_svy, type = "pearson")
fitted_values <- fitted(model8_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) # -> excellent
# or
plot(model8_svy, which = 1) # Linearity, very nice!

# (2) Hetereoscedasticity
plot(model8_svy$fitted.values, abs(model8_svy$residuals)) # not so bad?
data.frame(fitted_values = model8_svy$fitted.values, 
           abs_residuals = abs(model8_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model8_svy$residuals) ~ model8_svy$fitted.values)
summary(fit.glejser) # low heteroscedasticity?
tbl_regression(fit.glejser) 
plot(model8_svy$fitted.values, model8_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model8_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model8_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> not so bad
hist(model8_svy$residuals) # nice
plot(model8_svy$residuals) # nice

mean_res <- mean(model8_svy$residuals)
sd_res <- sd(model8_svy$residuals)
# Create the plot
data.frame(res = model8_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = ..density..), fill = "gray", color = "black", bins = 30) +  # Adjusted for density
  geom_density(color = "red", size = 1) +  # Density curve in red
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res),
    color = "blue", linetype = "dashed", size = 1  # Normal curve in blue (dashed)
  ) +
  labs(
    title = "Histogram of Residuals with Density Overlay",
    x = "Residuals",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
# really nice overlay!


# __Visualize ABSI predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_ABSI <- predict(model8_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_ABSI <- exp(param_space$predicted_log_ABSI)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_ABSI, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_ABSI, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_ABSI, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,  # Horizontal range for each bin
  #  y = q2_5, yend = q2_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 2.5% quantile
  #geom_segment(data = quantiles, aes(
  #  x = SRI_mid - 2.5, xend = SRI_mid + 2.5,
  #  y = q97_5, yend = q97_5, color = RIAGENDR_factor
  #), inherit.aes = FALSE, linetype = "dashed", alpha = 0.7) +  # 97.5% quantile
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted ABSI with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted ABSI"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_ABSI_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

# __Visualize for Suppl----------

# Extract weighted means/modes for covariates
RIDAGEYR_mean <- 49.775  # Weighted mean age
LBXVIDMS_mean <- 71.255  # Weighted mean Vitamin D
Depression_score_mean <- 3.0373  # Weighted mean Depression score
activity_level_mean <- 8.1682  # Weighted mean physical activity
caloric_intake_mean <- 2142.6  # Weighted mean calorie intake

# Most common categories (mode) from weighted tables
DMDEDUC2_mode <- "Some College or AA degree"
INDHHIN2_mode <- "$100,000 and Over"
OCD150_mode <- "Working at a job or business"
alcohol_mode <- "Moderate Drinker"
smoking_mode <- "Non_smokers"
marital_status_mode <- "Married"

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for ABSI
tic()
bootstrap_summary_ABSI <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model8_svy, 
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_ABSI$RIDRETH3_factor <- factor(
  bootstrap_summary_ABSI$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_ABSI$RIAGENDR_factor <- factor(
  bootstrap_summary_ABSI$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# rename predicted BMI col:
bootstrap_summary_ABSI <- bootstrap_summary_ABSI %>%
  rename(Predicted_ABSI = Predicted_BMI)

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_ABSI, aes(x = SRI_no_imputation_winsorized_rand, 
                                   y = Predicted_ABSI,  # NO exp() here!!
                                   color = RIAGENDR_factor, 
                                   fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted ABSI
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted ABSI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted ABSI",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_ABSI_strat_by_Sex_and_Ethnicity_ABSI_9.2.25.pdf")

ggsave(file_path)


# _Model 9: Waist to Height Ratio (WHtR) [excellent model fit]--------
hist(df_an1_imputed$WHtR) # rather symmetric
hist(log(df_an1_imputed$WHtR)) # symmetric
model9_svy <- svyglm(log(WHtR) ~ 
                       SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                       SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                       RIDAGEYR +                    # Age
                       #RIAGENDR_factor +
                       #RIDRETH3_factor +
                       DMDEDUC2_factor +             # Education
                       INDHHIN2_factor +             # H.H.income
                       OCD150_factor +               # Occupation (last week)
                       #Avg_Systolic_BP +
                       alcohol_consumption +         # Alcohol use frequency
                       smoking_status +              # Smoking status
                       LBXVIDMS +                    # 25-hydroxyvitamin D
                       DR1TKCAL_winsorized +         # caloric intake
                       #DIQ010_factor +
                       Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                       #Marital_status_collapsed +
                       DMDMARTL_factor +             # Marital Status
                       activity_level,               # Physical activity
                     #LBDHDDSI,
                     design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model9_svy, type = "pearson")
fitted_values <- fitted(model9_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# or
plot(model9_svy, which = 1) # Linearity, ok

# (2) Hetereoscedasticity
plot(model9_svy$fitted.values, abs(model9_svy$residuals)) # upward trend
data.frame(fitted_values = model9_svy$fitted.values, 
           abs_residuals = abs(model9_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model9_svy$residuals) ~ model9_svy$fitted.values)
summary(fit.glejser) # no heteroscedasticity
tbl_regression(fit.glejser) 
plot(model9_svy$fitted.values, model9_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model9_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model9_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> good
hist(model9_svy$residuals) # nice
plot(model9_svy$residuals) # nice

mean_res <- mean(model9_svy$residuals)
sd_res <- sd(model9_svy$residuals)
data.frame(res = model9_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model9_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# __Visualize WHtR predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_WHtR <- predict(model9_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_WHtR <- exp(param_space$predicted_log_WHtR)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_WHtR, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_WHtR, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_WHtR, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted WHtR with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted WHtR"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_WHtR_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

# __Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for WHtR
tic()
bootstrap_summary_WHtR <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model9_svy,  # Model for WHtR
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_WHtR$RIDRETH3_factor <- factor(
  bootstrap_summary_WHtR$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_WHtR$RIAGENDR_factor <- factor(
  bootstrap_summary_WHtR$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename column for WHtR predictions (fix if needed)
colnames(bootstrap_summary_WHtR)[which(colnames(bootstrap_summary_WHtR) == "Predicted_BMI")] <- "Predicted_WHtR"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_WHtR, aes(x = SRI_no_imputation_winsorized_rand, 
                                   y = exp(Predicted_WHtR),  # Now correctly set
                                   color = RIAGENDR_factor, 
                                   fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted WHtR
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted WHtR by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted WHtR",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_WHtR_strat_by_Sex_and_Ethnicity_WHtR_9.2.25.pdf")

ggsave(file_path)



#_ Model 10: SAD Average Sagittal Abdominal Diameter [excellent model fit]-------
hist(df_an1_imputed$BMDAVSAD_winsorized) # 
hist(log(df_an1_imputed$BMDAVSAD_winsorized)) # rather symmetric now
model10_svy <- svyglm(log(BMDAVSAD_winsorized) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +         # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model10_svy, type = "pearson")
fitted_values <- fitted(model10_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# or
plot(model10_svy, which = 1) # Linearity, ok

# (2) Hetereoscedasticity
plot(model10_svy$fitted.values, abs(model10_svy$residuals)) # upward trend
data.frame(fitted_values = model10_svy$fitted.values, 
           abs_residuals = abs(model10_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model10_svy$residuals) ~ model10_svy$fitted.values)
summary(fit.glejser) # no heteroscedasticity
tbl_regression(fit.glejser) 
plot(model10_svy$fitted.values, model10_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model10_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model10_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> perfect!
hist(model10_svy$residuals) # nice
plot(model10_svy$residuals) # nice

mean_res <- mean(model10_svy$residuals)
sd_res <- sd(model10_svy$residuals)
data.frame(res = model10_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model10_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# __Visualize BMDAVSAD predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_ASAD <- predict(model10_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_ASAD <- exp(param_space$predicted_log_ASAD)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_ASAD, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_ASAD, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_ASAD, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted ASAD with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted ASAD"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_ASAD_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

# __Visualize for Suppl----------
# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for SAD
tic()
bootstrap_summary_SAD <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model10_svy,  # Model for SAD
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_SAD$RIDRETH3_factor <- factor(
  bootstrap_summary_SAD$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_SAD$RIAGENDR_factor <- factor(
  bootstrap_summary_SAD$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename column for SAD predictions (fix if needed)
colnames(bootstrap_summary_SAD)[which(colnames(bootstrap_summary_SAD) == "Predicted_BMI")] <- "Predicted_SAD"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_SAD, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_SAD),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted SAD
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted SAD by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted SAD",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_SAD_strat_by_Sex_and_Ethnicity_SAD_9.2.25.pdf")

ggsave(file_path)


# _Model 11: Visceral Adiposity index (VAI) [good model fit]----
hist(df_an1_imputed$VAI) #
hist(log(df_an1_imputed$VAI)) # rather symmetric now
model11_svy <- svyglm(log(VAI) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +                    # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model11_svy, type = "pearson")
fitted_values <- fitted(model11_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# or
plot(model11_svy, which = 1) # Linearity, ok

# (2) Hetereoscedasticity
plot(model11_svy$fitted.values, abs(model11_svy$residuals)) # upward trend
data.frame(fitted_values = model11_svy$fitted.values, 
           abs_residuals = abs(model11_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model11_svy$residuals) ~ model11_svy$fitted.values)
summary(fit.glejser) # no heteroscedasticity
tbl_regression(fit.glejser) 
plot(model11_svy$fitted.values, model11_svy$residuals) # seems to have larger variance for higher fitted values
# Gelman p. 46, unequal variance does not affect the most important part of the model,
# the linearity X*beta.

# (3) Independence
resid <- model11_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model11_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> not so bad
hist(model11_svy$residuals) # nice
plot(model11_svy$residuals) # nice

mean_res <- mean(model11_svy$residuals)
sd_res <- sd(model11_svy$residuals)
data.frame(res = model11_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model11_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# __Visualize VAI predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_VAI <- predict(model11_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_VAI <- exp(param_space$predicted_log_VAI)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_VAI, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_VAI, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_VAI, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted VAI with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted VAI"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_VAI_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

# __Visualize for Suppl----------
# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for VAI
tic()
bootstrap_summary_VAI <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model11_svy,  # Model for VAI
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_VAI$RIDRETH3_factor <- factor(
  bootstrap_summary_VAI$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_VAI$RIAGENDR_factor <- factor(
  bootstrap_summary_VAI$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_VAI (fix if necessary)
colnames(bootstrap_summary_VAI)[which(colnames(bootstrap_summary_VAI) == "Predicted_BMI")] <- "Predicted_VAI"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_VAI, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_VAI),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted VAI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted VAI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted VAI",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_VAI_strat_by_Sex_and_Ethnicity_VAI_9.2.25.pdf")

ggsave(file_path)


# _Model 12: DXDTOFAT - Total Fat (kg) [fair model fit]----
hist(df_an1_imputed$DXDTOFAT_kg_winsorized) # could be more symmetric
hist(log(df_an1_imputed$DXDTOFAT_kg_winsorized)) # still not symmetric
hist(df_an1$DXDTOFAT_kg_winsorized)
hist(log(df_an1$DXDTOFAT_kg_winsorized)) # 
model12_svy <- svyglm(log(DXDTOFAT_kg_winsorized) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +         # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign_unimputed)
qqPlot(model12_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> good


# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model12_svy, type = "pearson")
fitted_values <- fitted(model12_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model12_svy$fitted.values, abs(model12_svy$residuals)) #
data.frame(fitted_values = model12_svy$fitted.values, 
           abs_residuals = abs(model12_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model12_svy$residuals) ~ model12_svy$fitted.values)
summary(fit.glejser) # some heteroscedasticity
plot(model12_svy$fitted.values, model12_svy$residuals) # seems to have larger variance for higher fitted values

# (3) Independence
resid <- model12_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # 

# (4) Normality
qqPlot(model12_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> good
hist(model12_svy$residuals) # nice
plot(model12_svy$residuals) # nice

mean_res <- mean(model12_svy$residuals)
sd_res <- sd(model12_svy$residuals)
data.frame(res = model12_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model12_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# __Visualize Total FAT predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_TOTFAT <- predict(model12_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_TOTFAT <- exp(param_space$predicted_log_TOTFAT)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_TOTFAT, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_TOTFAT, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_TOTFAT, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted TOTFAT with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted TOTFAT"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_TOTFAT_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

#__Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for DXDTOFAT_kg_winsorized
tic()
bootstrap_summary_DXDTOFAT <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model12_svy,  # Model for Total fat mass, DXDTOFAT_kg_winsorized
  df_an1, # not imputed due to high missingness
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_DXDTOFAT$RIDRETH3_factor <- factor(
  bootstrap_summary_DXDTOFAT$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_DXDTOFAT$RIAGENDR_factor <- factor(
  bootstrap_summary_DXDTOFAT$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_SAD (fix if necessary)
colnames(bootstrap_summary_DXDTOFAT)[which(colnames(bootstrap_summary_DXDTOFAT) == "Predicted_BMI")] <- "Predicted_DXDTOFAT"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_DXDTOFAT, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_DXDTOFAT),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted SAD
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted Total Fat Mass by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Total Fat Mass",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_TOTFAT_strat_by_Sex_and_Ethnicity_DXDTOFAT_9.2.25.pdf")

ggsave(file_path)


# _Model 13: Fat Mass Index (FMI) [fair model fit] -----
hist(df_an1$FMI)
hist(log(df_an1$FMI)) #
model13_svy <- svyglm(log(FMI) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +                    # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign_unimputed)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model13_svy, type = "pearson")
fitted_values <- fitted(model13_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model13_svy$fitted.values, abs(model13_svy$residuals)) #
data.frame(fitted_values = model13_svy$fitted.values, 
           abs_residuals = abs(model13_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model13_svy$residuals) ~ model13_svy$fitted.values)
summary(fit.glejser) # some heteroscedasticity
plot(model13_svy$fitted.values, model13_svy$residuals) # seems to have larger variance for higher fitted values

# (3) Independence
resid <- model13_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model13_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> rather bad on one side
hist(model13_svy$residuals) # nice
plot(model13_svy$residuals) # nice

mean_res <- mean(model13_svy$residuals)
sd_res <- sd(model13_svy$residuals)
data.frame(res = model13_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model13_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# looks ok, some deviation

# __Visualize Total FMI predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_FMI <- predict(model13_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_FMI <- exp(param_space$predicted_log_FMI)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_FMI, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_FMI, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_FMI, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted FMI with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted FMI"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_FMI_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)


#__Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for FMI
tic()
bootstrap_summary_FMI <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model13_svy,  # Model for FMI
  df_an1, # not imputed due to high missingness 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_FMI$RIDRETH3_factor <- factor(
  bootstrap_summary_FMI$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_FMI$RIAGENDR_factor <- factor(
  bootstrap_summary_FMI$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_FMI (fix if necessary)
colnames(bootstrap_summary_FMI)[which(colnames(bootstrap_summary_FMI) == "Predicted_BMI")] <- "Predicted_FMI"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_FMI, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_FMI),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted FMI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted FMI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Fat Mass Index (FMI)",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_FMI_strat_by_Sex_and_Ethnicity_FMI_9.2.25.pdf")

ggsave(file_path)


# _Model 14: Body Roundness Index (BRI) [excellent model fit]----
hist(df_an1$BRI) # could be more symmetric
hist(log(df_an1$BRI)) # very symmetric
model14_svy <- svyglm(log(BRI) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +                    # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model14_svy, type = "pearson")
fitted_values <- fitted(model14_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model14_svy$fitted.values, abs(model14_svy$residuals)) #
data.frame(fitted_values = model14_svy$fitted.values, 
           abs_residuals = abs(model14_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model14_svy$residuals) ~ model14_svy$fitted.values)
summary(fit.glejser) # some heteroscedasticity
plot(model14_svy$fitted.values, model14_svy$residuals) # 

# (3) Independence
resid <- model14_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model14_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> really good!
hist(model14_svy$residuals) # nice
plot(model14_svy$residuals) # nice

mean_res <- mean(model14_svy$residuals)
sd_res <- sd(model14_svy$residuals)
data.frame(res = model14_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model14_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# very good!

# __Visualize Total BRI predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_BRI <- predict(model14_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_BRI <- exp(param_space$predicted_log_BRI)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_BRI, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_BRI, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_BRI, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted BRI with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted BRI"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_BRI_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

#__Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for BRI
tic()
bootstrap_summary_BRI <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model14_svy,  # Model for BRI
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_BRI$RIDRETH3_factor <- factor(
  bootstrap_summary_BRI$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_BRI$RIAGENDR_factor <- factor(
  bootstrap_summary_BRI$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_BRI (fix if necessary)
colnames(bootstrap_summary_BRI)[which(colnames(bootstrap_summary_BRI) == "Predicted_BMI")] <- "Predicted_BRI"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_BRI, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_BRI),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted BRI
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted BRI by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Body Roundness Index (BRI)",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_BRI_strat_by_Sex_and_Ethnicity_BRI_9.2.25.pdf")

ggsave(file_path)



# _Model 15: Average Sagittal Abdominal Diameter SADHtR [excellent model fit]------
hist(df_an1_imputed$SADHtR) # could be more symmetric
hist(log(df_an1_imputed$SADHtR)) # better
model15_svy <- svyglm(log(SADHtR) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +                    # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model15_svy, type = "pearson")
fitted_values <- fitted(model15_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model15_svy$fitted.values, abs(model15_svy$residuals)) #
data.frame(fitted_values = model15_svy$fitted.values, 
           abs_residuals = abs(model15_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model15_svy$residuals) ~ model15_svy$fitted.values)
summary(fit.glejser) # no heteroscedasticity
plot(model15_svy$fitted.values, model15_svy$residuals) # seems to have larger variance for higher fitted values

# (3) Independence
resid <- model14_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model15_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> really good!
hist(model15_svy$residuals) # nice
plot(model15_svy$residuals) # nice

mean_res <- mean(model15_svy$residuals)
sd_res <- sd(model15_svy$residuals)
data.frame(res = model15_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model15_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# very good!


# __Visualize Total SADHtR predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_SADHtR <- predict(model15_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_SADHtR <- exp(param_space$predicted_log_SADHtR)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_SADHtR, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_SADHtR, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_SADHtR, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted SADHtR with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted SADHtR"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_SADHtR_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

#__Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for SADHtR
tic()
bootstrap_summary_SADHtR <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model15_svy,  # Model for SADHtR
  df_an1_imputed, 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_SADHtR$RIDRETH3_factor <- factor(
  bootstrap_summary_SADHtR$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_SADHtR$RIAGENDR_factor <- factor(
  bootstrap_summary_SADHtR$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_SADHtR (fix if necessary)
colnames(bootstrap_summary_SADHtR)[which(colnames(bootstrap_summary_SADHtR) == "Predicted_BMI")] <- "Predicted_SADHtR"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_SADHtR, aes(x = SRI_no_imputation_winsorized_rand, 
                                     y = exp(Predicted_SADHtR),  # Now correctly set
                                     color = RIAGENDR_factor, 
                                     fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted SADHtR
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted SADHtR by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Sagittal Abdominal Diameter Height Ratio (SADHtR)",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_SADHtR_strat_by_Sex_and_Ethnicity_SADHtR_9.2.25.pdf")

ggsave(file_path)


# _ Model 16: Lipid accumulation product (LAP) [excellent model fit]------
hist(df_an1_imputed$LAP) # skewed
hist(log(df_an1_imputed$LAP)) # better
model16_svy <- svyglm(log(LAP) ~ 
                        SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                        SRI_no_imputation_winsorized_rand*RIAGENDR_factor +       # SRI (exposure)
                        RIDAGEYR +                    # Age
                        #RIAGENDR_factor +
                        #RIDRETH3_factor +
                        DMDEDUC2_factor +             # Education
                        INDHHIN2_factor +             # H.H.income
                        OCD150_factor +               # Occupation (last week)
                        #Avg_Systolic_BP +
                        alcohol_consumption +         # Alcohol use frequency
                        smoking_status +              # Smoking status
                        LBXVIDMS +                    # 25-hydroxyvitamin D
                        DR1TKCAL_winsorized +                    # caloric intake
                        #DIQ010_factor +
                        Depression_score_PHQ_9 +      # Depression score (PHQ-9)
                        #Marital_status_collapsed +
                        DMDMARTL_factor +             # Marital Status
                        activity_level,               # Physical activity
                      #LBDHDDSI,
                      design = nhanesDesign)

# __Model assumptions:----------
# Understanding Regression, p. 97. Test:
# (1) Linearity, (2) constant variance, (3) independence, (4) normality

# (1) Linearity
residuals <- residuals(model16_svy, type = "pearson")
fitted_values <- fitted(model16_svy)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model16_svy$fitted.values, abs(model16_svy$residuals)) #
data.frame(fitted_values = model16_svy$fitted.values, 
           abs_residuals = abs(model16_svy$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model16_svy$residuals) ~ model16_svy$fitted.values)
summary(fit.glejser) # some heteroscedasticity
plot(model16_svy$fitted.values, model16_svy$residuals) # seems to have larger variance for higher fitted values

# (3) Independence
resid <- model14_svy$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model16_svy$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> really good!
hist(model16_svy$residuals) # nice
plot(model16_svy$residuals) # nice

mean_res <- mean(model16_svy$residuals)
sd_res <- sd(model16_svy$residuals)
data.frame(res = model16_svy$residuals) %>%
  ggplot(aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  geom_density(aes(y = after_stat(count) * 0.1), color = "red") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_res, sd = sd_res) * length(model16_svy$residuals) * 0.1,
    color = "blue", linetype = "dashed"
  ) +
  labs(title = "Histogram of Residuals", x = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# very good!


# __Visualize Total LAP predictions--------
n_samples <- 2*10^5  # You can adjust this number

# Random sampling from the parameter space
#set.seed(123)  # For reproducibility
param_space <- data.frame(
  SRI_no_imputation_winsorized_rand = sample(df_an1_imputed$SRI_no_imputation_winsorized_rand, n_samples, replace = TRUE),
  RIDRETH3_factor = sample(df_an1_imputed$RIDRETH3_factor, n_samples, replace = TRUE),  # Ethnicity
  RIAGENDR_factor = sample(df_an1_imputed$RIAGENDR_factor, n_samples, replace = TRUE),  # Gender
  RIDAGEYR = sample(df_an1_imputed$RIDAGEYR, n_samples, replace = TRUE),  # Age
  DMDEDUC2_factor = sample(df_an1_imputed$DMDEDUC2_factor, n_samples, replace = TRUE),  # Education
  INDHHIN2_factor = sample(df_an1_imputed$INDHHIN2_factor, n_samples, replace = TRUE),  # Income
  OCD150_factor = sample(df_an1_imputed$OCD150_factor, n_samples, replace = TRUE),  # Occupation
  alcohol_consumption = sample(df_an1_imputed$alcohol_consumption, n_samples, replace = TRUE),  # Alcohol
  smoking_status = sample(df_an1_imputed$smoking_status, n_samples, replace = TRUE),  # Smoking
  LBXVIDMS = sample(df_an1_imputed$LBXVIDMS_winsorized, n_samples, replace = TRUE), # Vitamin D
  DR1TKCAL_winsorized = sample(df_an1_imputed$DR1TKCAL_winsorized, n_samples, replace = TRUE),  # Caloric intake
  Depression_score_PHQ_9 = sample(df_an1_imputed$Depression_score_PHQ_9, n_samples, replace = TRUE), # Depression score
  DMDMARTL_factor = sample(df_an1_imputed$DMDMARTL_factor, n_samples, replace = TRUE),  # Marital status
  activity_level = sample(df_an1_imputed$activity_level, n_samples, replace = TRUE)  # Physical activity
)

# Predict outcomes using the sampled parameter space
tic()
param_space$predicted_log_LAP <- predict(model16_svy, newdata = param_space, type = "response")
toc() # 1s

param_space$predicted_LAP <- exp(param_space$predicted_log_LAP)

param_space <- param_space %>%
  mutate(
    SRI_bin = cut(SRI_no_imputation_winsorized_rand, breaks = seq(0, 100, by = 5), include.lowest = TRUE),
    SRI_mid = as.numeric(gsub("[^0-9.]", "", gsub(".*,", "", SRI_bin))) - 2.5  # Calculate midpoints
  )

quantiles <- param_space %>%
  group_by(SRI_bin, SRI_mid, RIAGENDR_factor, RIDRETH3_factor) %>%
  summarise(
    q2_5 = quantile(predicted_LAP, probs = 0.025, na.rm = TRUE),
    q97_5 = quantile(predicted_LAP, probs = 0.975, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(param_space, aes(x = SRI_no_imputation_winsorized_rand, y = predicted_LAP, 
                        color = RIAGENDR_factor)) +
  geom_point(alpha = 0.1) +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed trend line
  facet_wrap(~ RIDRETH3_factor) +  # Facet by ethnicity
  geom_line(data = quantiles, aes(x = SRI_mid, y = q2_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 2.5% quantiles
  geom_line(data = quantiles, aes(x = SRI_mid, y = q97_5, 
                                  group = interaction(RIAGENDR_factor, RIDRETH3_factor), color = RIAGENDR_factor), 
            inherit.aes = FALSE, linetype = "solid") +  # Connect 97.5% quantiles
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    title = "Predicted LAP with Quantile Lines across SRI values",
    x = "SRI (Sleep Regularity Index)",
    y = "Predicted LAP"
  )
# save
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, 
                       "1_Main_Document", "Figures", "Model_2_OTHER_adiposity_measures_LAP_predictions_via_sampling_8.12.24.pdf")
ggsave(file_path)

#__Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1_imputed$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for LAP
tic()
bootstrap_summary_LAP <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model16_svy,  # Model for LAP
  df_an1, # not from imputed 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D" # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_LAP$RIDRETH3_factor <- factor(
  bootstrap_summary_LAP$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_LAP$RIAGENDR_factor <- factor(
  bootstrap_summary_LAP$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted BMI column to Predicted_LAP (fix if necessary)
colnames(bootstrap_summary_LAP)[which(colnames(bootstrap_summary_LAP) == "Predicted_BMI")] <- "Predicted_LAP"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_LAP, aes(x = SRI_no_imputation_winsorized_rand, 
                                  y = exp(Predicted_LAP),  # Now correctly set
                                  color = RIAGENDR_factor, 
                                  fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted LAP
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted LAP by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Lipid Accumulation Product (LAP)",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_LAP_strat_by_Sex_and_Ethnicity_LAP_9.2.25.pdf")

ggsave(file_path)

# _Model 17: Total Percent Fat (DXDTOPF)----
hist(df_an1$DXDTOPF_winsorized, main = "Histogram of Winsorized Total Percent Fat", 
     xlab = "Total Percent Fat", breaks = 30)
hist(log(df_an1$DXDTOPF_winsorized), main = "Histogram of Log-Transformed Total Percent Fat", 
     xlab = "Log-Transformed Total Percent Fat", breaks = 30)
# -> no log
table(is.na(df_an1$DXDTOPF_winsorized))  # ~50%
cor(df_an1$DXDTOPF_winsorized, df_an1$BMXBMI_winsorized, use = "complete.obs")  # 0.5747371
model_total_percent_fat <- svyglm(DXDTOPF_winsorized ~ 
                                    SRI_no_imputation_winsorized_rand*RIDRETH3_factor + 
                                    SRI_no_imputation_winsorized_rand*RIAGENDR_factor + # SRI (exposure)
                                    RIDAGEYR +                     # Age
                                    DMDEDUC2_factor +              # Education
                                    INDHHIN2_factor +              # Household Income
                                    OCD150_factor +                # Occupation (last week)
                                    alcohol_consumption +          # Alcohol use frequency
                                    smoking_status +               # Smoking status
                                    LBXVIDMS +                     # 25-hydroxyvitamin D
                                    DR1TKCAL_winsorized +          # Caloric intake
                                    Depression_score_PHQ_9 +       # Depression score (PHQ-9)
                                    DMDMARTL_factor +              # Marital status
                                    activity_level,                # Physical activity
                                  design = nhanesDesign_unimputed)
summary(model_total_percent_fat)

# Model fit
# (1) Linearity
residuals <- residuals(model_total_percent_fat, type = "pearson")
fitted_values <- fitted(model_total_percent_fat)
df_residuals <- data.frame(Fitted = fitted_values, Residuals = residuals)
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Hetereoscedasticity
plot(model_total_percent_fat$fitted.values, abs(model_total_percent_fat$residuals)) #
data.frame(fitted_values = model_total_percent_fat$fitted.values, 
           abs_residuals = abs(model_total_percent_fat$residuals)) %>%
  ggplot(aes(x = fitted_values, y = abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "abs. Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

fit.glejser <- lm(abs(model_total_percent_fat$residuals) ~ model_total_percent_fat$fitted.values)
summary(fit.glejser) # some heteroscedasticity

# (3) Independence
resid <- model_total_percent_fat$residuals
lag.resid <- c(NA, resid[1:(length(resid)-1)])
cor.test(resid, lag.resid) # looks nice

# (4) Normality
qqPlot(model_total_percent_fat$residuals, main = "QQ Plot of Weighted Model", envelope = TRUE) # -> really good!


# __Visualize for Suppl----------

# Define range of SRI values
SRI_range <- seq(0, 100, by = 5)

# Define stratification variables
sex_levels <- c("Male", "Female")
ethnicity_levels <- levels(df_an1$RIDRETH3_factor)

# Load bootstrap function
source("./z_stuff/bootstrap_prediction_CIs_for_svy_models.R")

# Run bootstrapped predictions for DXDTOPF
tic()
bootstrap_summary_DXDTOPF <- bootstrap_predictions_svyglm_sex_ethnicity_varied(
  model_total_percent_fat,  # Model for Total Percent Fat
  df_an1, # not imputed due to high missingness 
  SRI_range, 
  sex_levels, 
  ethnicity_levels, 
  n_boot = 300
)
toc()  # ~300s execution time

# Define specific colors for "Male" and "Female"
female_color <- "#00BFC4"   # Cyan
male_color <- "#F8766D"     # Salmon red

# Reorder the levels of RIDRETH3_factor
bootstrap_summary_DXDTOPF$RIDRETH3_factor <- factor(
  bootstrap_summary_DXDTOPF$RIDRETH3_factor,
  levels = c(
    "Mexican American", 
    "Other Hispanic", 
    "Non-Hispanic White", 
    "Non-Hispanic Black", 
    "Non-Hispanic Asian", 
    "Other Race - Including Multi-Racial"
  )
)

# Ensure correct order for sex levels
bootstrap_summary_DXDTOPF$RIAGENDR_factor <- factor(
  bootstrap_summary_DXDTOPF$RIAGENDR_factor,
  levels = c("Male", "Female")
)

# Rename predicted column to Predicted_DXDTOPF (fix if necessary)
colnames(bootstrap_summary_DXDTOPF)[which(colnames(bootstrap_summary_DXDTOPF) == "Predicted_BMI")] <- "Predicted_DXDTOPF"

# Generate ggplot with correct order and colors
ggplot(bootstrap_summary_DXDTOPF, aes(x = SRI_no_imputation_winsorized_rand, 
                                      y = exp(Predicted_DXDTOPF),  # Now correctly set
                                      color = RIAGENDR_factor, 
                                      fill = RIAGENDR_factor)) +
  geom_line(size = 1) +  # Line for predicted DXDTOPF
  geom_ribbon(aes(ymin = exp(CI_Lower), ymax = exp(CI_Upper)), alpha = 0.3, color = NA) +  # Confidence interval
  facet_wrap(~RIDRETH3_factor, scales = "fixed") +  # Facet by ethnicity only
  labs(title = "Predicted Total Percent Fat by SRI, Stratified by Ethnicity and Sex",
       x = "SRI (Sleep Regularity Index)",
       y = "Predicted Total Percent Fat (DXDTOPF)",
       color = "Sex",
       fill = "Sex") +
  scale_color_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Custom colors for sexes
  scale_fill_manual(values = c("Male" = male_color, "Female" = female_color)) +  # Matching fill colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12)
  )

# Save plot with correct file path
file_path <- file.path(parent_directory, 
                       "1_Main_Document", 
                       "Figures", 
                       "Model_2_predictions_DXDTOPF_strat_by_Sex_and_Ethnicity_DXDTOPF_9.2.25.pdf")

ggsave(file_path)



# For Discussion - SRI Visualization-----------
dim(df_an1_imputed)[1]
SRI_df <- data.frame(SRI = df_an1_imputed$SRI_no_imputation_winsorized_rand)
# Define the colors based on quantile ranges
quantiles <- quantile(SRI_df$SRI, 
                      probs = c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1), na.rm = TRUE)
p2 <- ggplot(SRI_df, aes(x = SRI)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(ifelse(
    ..x.. < quantiles[2], "#c4322b",
    ifelse(..x.. < quantiles[3], "#FC8D59",
           ifelse(..x.. < quantiles[4], "#FEE08B",
                  ifelse(..x.. < quantiles[5], "#D9EF8B",
                         ifelse(..x.. < quantiles[6], "#91BFDB", "#4575B4"))))))), 
    bins = 30, color = NA, alpha = 0.7) +
  scale_fill_identity() +
  geom_density(color = "blue", linewidth = 1) +
  geom_boxplot(aes(y = -0.005, x = SRI), width = 0.01, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.005), 
             position = position_jitter(width = 0.002, height = 0.005), 
             size = 1, alpha = 0.05) +
  ggtitle("SRI distribution of 7,085 NHANES participants (2011-14)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p2

mean(SRI_df$SRI)
median(SRI_df$SRI)

# _Weighted SRI:-----
svyquantile(~SRI_no_imputation_winsorized_rand, nhanesDesign, c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1))
svymean(~SRI_no_imputation_winsorized_rand, nhanesDesign) # 61.128
svyquantile(~SRI_no_imputation_winsorized_rand, nhanesDesign, c(0.5)) # 63.89

svymean(~  RIDAGEYR, nhanesDesign, na.rm = TRUE) # 

output_path <- ".../1_Main_Document/Figures_Supplement/SRI_distribution.pdf"
ggsave(filename = output_path, plot = p2, device = "pdf", width = 8, height = 6) 


# _Sex differences----
df_an1_imputed %>%
  ggplot(aes(x = as.factor(RIAGENDR), 
             y = SRI_no_imputation_winsorized_rand)) + # 1=Male, 2=Female
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Gender (1=Male, 2=Female)", y = "SRI_no_imputation_winsorized_rand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
svyby(~SRI_no_imputation_winsorized_rand, ~RIAGENDR, nhanesDesign, svymean)


# _Ethnicity differences----
df_an1_imputed %>%
  ggplot(aes(x = fct_reorder(RIDRETH3_factor, SRI_no_imputation_winsorized_rand, .fun = median), 
             y = SRI_no_imputation_winsorized_rand)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Ethnicity", y = "SRI_no_imputation_winsorized_rand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
svyby(~SRI_no_imputation_winsorized_rand, ~RIDRETH3_factor, nhanesDesign, 
      svyquantile, quantiles = 0.5, keep.var = FALSE)

# _SRI differences with respect to BMI----
correlation <- cor(df_an1_imputed$BMXBMI_winsorized, df_an1_imputed$SRI_no_imputation_winsorized_rand, use = "complete.obs")
cor.test(df_an1_imputed$BMXBMI_winsorized, df_an1_imputed$SRI_no_imputation_winsorized_rand, method = "pearson")
# 95 percent confidence interval:
# -0.14419980 -0.09831327
df_an1_imputed %>%
  ggplot(aes(x = BMXBMI_winsorized, y = SRI_no_imputation_winsorized_rand)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "SRI vs BMI", x = "BMI", y = "SRI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(correlation, 2)),
           hjust = 1.1, vjust = 1.1, size = 5, color = "blue")

# _SRI differences with respect to Age----
correlation <- cor(df_an1_imputed$RIDAGEYR, df_an1_imputed$SRI_no_imputation_winsorized_rand, use = "complete.obs")
cor.test(df_an1_imputed$RIDAGEYR, df_an1_imputed$SRI_no_imputation_winsorized_rand, method = "pearson")
# predict SRI diffs in the simple model:
model_sri_age <- lm(SRI_no_imputation_winsorized_rand ~ RIDAGEYR, data = df_an1_imputed)
summary(model_sri_age)
predict(model_sri_age, newdata = data.frame(RIDAGEYR = 25)) # 61.6889 
predict(model_sri_age, newdata = data.frame(RIDAGEYR = 45)) # 59.64327
predict(model_sri_age, newdata = data.frame(RIDAGEYR = 65)) # 57.59764 

# BMI differences in SRI Tertiles-----
df_an1_imputed %>%
  mutate(SRI_tertile = ntile(SRI_no_imputation_winsorized_rand, 3)) %>%
  ggplot(aes(x = factor(SRI_tertile), y = BMXBMI_winsorized)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -1.5, color = "blue") +
  labs(x = "SRI Tertiles", y = "BMI (winsorized)", title = "Boxplot of BMI across SRI Tertiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# BMI changes, clinically relevant?-----
# Load necessary library
library(survey)

# Calculate the survey-weighted mean and variance
# Calculate the survey-weighted variance of SRI
var_sri <- svyvar(~SRI_no_imputation_winsorized_rand, nhanesDesign)
sd_sri <- sqrt(var_sri[1])
sd_sri
sd(df_an1_imputed$SRI_no_imputation_winsorized_rand)

# Abstract----
# _Overall effect female :-----------
# Ensure SRI quintiles are computed using the full dataset
df_an1_imputed <- df_an1_imputed %>%
  mutate(SRI_quintile = ntile(SRI_no_imputation_winsorized_rand, 5))

# Compute SRI medians for each quintile from the full dataset
SRI_medians <- df_an1_imputed %>%
  dplyr::group_by(SRI_quintile) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation_winsorized_rand, na.rm = TRUE)) %>%
  pull(median_SRI)

# Ensure medians are stored correctly
if (length(SRI_medians) == 5) {
  SRI_medians <- c(SRI_medians[1], SRI_medians[2], SRI_medians[3], SRI_medians[4], SRI_medians[5])
} else {
  stop("Error: SRI medians were not computed correctly.")
}

# Function to calculate the multiplicative effect for FEMALES
calculate_overall_effect_females <- function(model, SRI_start, SRI_end) {
  fixed_values <- data.frame(
    SRI_no_imputation_winsorized_rand = SRI_start,
    RIDAGEYR = 49.775,  # Mean age
    RIAGENDR_factor = "Female",
    RIDRETH3_factor = "Non-Hispanic White",
    DMDEDUC2_factor = "Some College or AA degree",
    INDHHIN2_factor = "$100,000 and Over",
    OCD150_factor = "Working at a job or business",
    alcohol_consumption = "Moderate Drinker",
    smoking_status = "Non_smokers",
    LBXVIDMS = 68.3,
    DR1TKCAL_winsorized = 1991,
    Depression_score_PHQ_9 = 1,
    DMDMARTL_factor = "Married",
    activity_level = 7.772
  )
  
  # Sicherstellen, dass alle Faktor-Variablen die korrekten Levels haben
  factor_vars <- names(model$xlevels)
  for (v in factor_vars) {
    fixed_values[[v]] <- factor(fixed_values[[v]], levels = model$xlevels[[v]])
  }
  
  # Vorhersage Start
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Vorhersage Endwert
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Multiplikativer Effekt
  exp(logBMI_end - logBMI_start)
}

# Bootstrap function for FEMALES using the full model
bootstrap_overall_effects_females <- function(data, n_boot = 500, SRI_medians) {
  bootstrap_results <- matrix(NA, nrow = n_boot, ncol = 4)
  
  for (i in 1:n_boot) {
    # Resample with replacement from the full dataset
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    
    # Create the survey design for bootstrap sample
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    # Fit model on the full sample, including gender
    boot_model <- svyglm(log(BMXBMI_winsorized) ~ 
                           SRI_no_imputation_winsorized_rand * RIDRETH3_factor + 
                           SRI_no_imputation_winsorized_rand * RIAGENDR_factor +
                           RIDAGEYR +
                           DMDEDUC2_factor +
                           INDHHIN2_factor +
                           OCD150_factor +
                           alcohol_consumption +
                           smoking_status +
                           LBXVIDMS +
                           DR1TKCAL_winsorized +
                           Depression_score_PHQ_9 +
                           DMDMARTL_factor +
                           activity_level, 
                         design = boot_design)
    
    # Compute multiplicative effects, setting sex to Female (2)
    bootstrap_results[i, ] <- c(
      calculate_overall_effect_females(boot_model, SRI_medians[1], SRI_medians[2]),
      calculate_overall_effect_females(boot_model, SRI_medians[1], SRI_medians[3]),
      calculate_overall_effect_females(boot_model, SRI_medians[1], SRI_medians[4]),
      calculate_overall_effect_females(boot_model, SRI_medians[1], SRI_medians[5])
    )
  }
  
  colnames(bootstrap_results) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")
  return(bootstrap_results)
}

# Run bootstrap for FEMALES using full model
tic()
bootstrap_results_females <- bootstrap_overall_effects_females(df_an1_imputed, 
                                                               n_boot = 500, 
                                                               SRI_medians)
toc() # 56s

# Convert bootstrap results into a data frame
bootstrap_results_combined_females <- as.data.frame(bootstrap_results_females)
colnames(bootstrap_results_combined_females) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")

# Summarize results: Mean and 95% CI
bootstrap_summary_females <- bootstrap_results_combined_females %>%
  dplyr::summarize(
    `Q1 → Q2_mean` = mean(`Q1 → Q2`),
    `Q1 → Q2_CI_Lower` = quantile(`Q1 → Q2`, 0.025),
    `Q1 → Q2_CI_Upper` = quantile(`Q1 → Q2`, 0.975),
    `Q1 → Q3_mean` = mean(`Q1 → Q3`),
    `Q1 → Q3_CI_Lower` = quantile(`Q1 → Q3`, 0.025),
    `Q1 → Q3_CI_Upper` = quantile(`Q1 → Q3`, 0.975),
    `Q1 → Q4_mean` = mean(`Q1 → Q4`),
    `Q1 → Q4_CI_Lower` = quantile(`Q1 → Q4`, 0.025),
    `Q1 → Q4_CI_Upper` = quantile(`Q1 → Q4`, 0.975),
    `Q1 → Q5_mean` = mean(`Q1 → Q5`),
    `Q1 → Q5_CI_Lower` = quantile(`Q1 → Q5`, 0.025),
    `Q1 → Q5_CI_Upper` = quantile(`Q1 → Q5`, 0.975)
  )

# Format results for table
formatted_results_females <- bootstrap_summary_females %>%
  dplyr::summarize(
    `Q1 → Q2` = paste0(
      round(`Q1 → Q2_mean`, 3), 
      " (", 
      round(`Q1 → Q2_CI_Lower`, 3), 
      "; ", 
      round(`Q1 → Q2_CI_Upper`, 3), 
      ")"
    ),
    `Q1 → Q3` = paste0(
      round(`Q1 → Q3_mean`, 3), 
      " (", 
      round(`Q1 → Q3_CI_Lower`, 3), 
      "; ", 
      round(`Q1 → Q3_CI_Upper`, 3), 
      ")"
    ),
    `Q1 → Q4` = paste0(
      round(`Q1 → Q4_mean`, 3), 
      " (", 
      round(`Q1 → Q4_CI_Lower`, 3), 
      "; ", 
      round(`Q1 → Q4_CI_Upper`, 3), 
      ")"
    ),
    `Q1 → Q5` = paste0(
      round(`Q1 → Q5_mean`, 4), 
      " (", 
      round(`Q1 → Q5_CI_Lower`, 3), 
      "; ", 
      round(`Q1 → Q5_CI_Upper`, 3), 
      ")"
    )
  )

# Create flextable
ft_model1_females <- formatted_results_females %>%
  flextable() %>%
  set_header_labels(
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  add_header_row(
    values = c("Multiplicative Effects of SRI on BMI (FEMALES)"), 
    colwidths = 4  
  ) %>%
  add_footer_row(
    values = "Parentheses (x; y) represent 95% confidence intervals.",
    colwidths = 4  
  )

ft_model1_females

setwd(dirname(getwd()))
saveRDS(ft_model1_females, "./RESULTS/overall_effect_females_Abstract_22.4.25.RDS")
save_as_html(ft_model1_females, path = "./RESULTS/overall_effect_females_Abstract_22.4.25.html")

# _Overall effect males----------
calculate_overall_effect_males <- function(model, SRI_start, SRI_end) {
  fixed_values <- data.frame(
    SRI_no_imputation_winsorized_rand = SRI_start,
    RIDAGEYR = 49.775,  # Mean age
    RIAGENDR_factor = "Male",
    RIDRETH3_factor = "Non-Hispanic White",
    DMDEDUC2_factor = "Some College or AA degree",
    INDHHIN2_factor = "$100,000 and Over",
    OCD150_factor = "Working at a job or business",
    alcohol_consumption = "Moderate Drinker",
    smoking_status = "Non_smokers",
    LBXVIDMS = 68.3,
    DR1TKCAL_winsorized = 1991,
    Depression_score_PHQ_9 = 1,
    DMDMARTL_factor = "Married",
    activity_level = 7.772
  )
  
  # Faktorstruktur auf Modelllevels anpassen
  factor_vars <- names(model$xlevels)
  for (v in factor_vars) {
    fixed_values[[v]] <- factor(fixed_values[[v]], levels = model$xlevels[[v]])
  }
  
  # Vorhersage Start
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Vorhersage Endwert
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Multiplikativer Effekt
  exp(logBMI_end - logBMI_start)
}

bootstrap_overall_effects_males <- function(data, n_boot = 500, SRI_medians) {
  bootstrap_results <- matrix(NA, nrow = n_boot, ncol = 4)
  
  for (i in 1:n_boot) {
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    boot_model <- svyglm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand + RIDAGEYR + RIAGENDR_factor, 
                         design = boot_design)
    
    bootstrap_results[i, ] <- c(
      calculate_overall_effect_males(boot_model, SRI_medians[1], SRI_medians[2]),
      calculate_overall_effect_males(boot_model, SRI_medians[1], SRI_medians[3]),
      calculate_overall_effect_males(boot_model, SRI_medians[1], SRI_medians[4]),
      calculate_overall_effect_males(boot_model, SRI_medians[1], SRI_medians[5])
    )
  }
  
  colnames(bootstrap_results) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")
  return(bootstrap_results)
}

# Run bootstrap for MALES
tic()
bootstrap_results_males <- bootstrap_overall_effects_males(df_an1_imputed, n_boot = 500, SRI_medians)
toc() # 18s

# Convert to data frame
bootstrap_results_combined_males <- as.data.frame(bootstrap_results_males)
colnames(bootstrap_results_combined_males) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")

# Summarize
bootstrap_summary_males <- bootstrap_results_combined_males %>%
  dplyr::summarize(
    `Q1 → Q2_mean` = mean(`Q1 → Q2`),
    `Q1 → Q2_CI_Lower` = quantile(`Q1 → Q2`, 0.025),
    `Q1 → Q2_CI_Upper` = quantile(`Q1 → Q2`, 0.975),
    `Q1 → Q3_mean` = mean(`Q1 → Q3`),
    `Q1 → Q3_CI_Lower` = quantile(`Q1 → Q3`, 0.025),
    `Q1 → Q3_CI_Upper` = quantile(`Q1 → Q3`, 0.975),
    `Q1 → Q4_mean` = mean(`Q1 → Q4`),
    `Q1 → Q4_CI_Lower` = quantile(`Q1 → Q4`, 0.025),
    `Q1 → Q4_CI_Upper` = quantile(`Q1 → Q4`, 0.975),
    `Q1 → Q5_mean` = mean(`Q1 → Q5`),
    `Q1 → Q5_CI_Lower` = quantile(`Q1 → Q5`, 0.025),
    `Q1 → Q5_CI_Upper` = quantile(`Q1 → Q5`, 0.975)
  )

# Format for table
formatted_results_males <- bootstrap_summary_males %>%
  dplyr::summarize(
    `Q1 → Q2` = paste0(
      round(`Q1 → Q2_mean`, 3), 
      " (", round(`Q1 → Q2_CI_Lower`, 3), "; ", round(`Q1 → Q2_CI_Upper`, 3), ")"
    ),
    `Q1 → Q3` = paste0(
      round(`Q1 → Q3_mean`, 3), 
      " (", round(`Q1 → Q3_CI_Lower`, 3), "; ", round(`Q1 → Q3_CI_Upper`, 3), ")"
    ),
    `Q1 → Q4` = paste0(
      round(`Q1 → Q4_mean`, 3), 
      " (", round(`Q1 → Q4_CI_Lower`, 3), "; ", round(`Q1 → Q4_CI_Upper`, 3), ")"
    ),
    `Q1 → Q5` = paste0(
      round(`Q1 → Q5_mean`, 4), 
      " (", round(`Q1 → Q5_CI_Lower`, 3), "; ", round(`Q1 → Q5_CI_Upper`, 3), ")"
    )
  )

ft_model1_males <- formatted_results_males %>%
  flextable() %>%
  set_header_labels(
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  add_header_row(
    values = c("Multiplicative Effects of SRI on BMI (MALES)"), 
    colwidths = 4  
  ) %>%
  add_footer_row(
    values = "Parentheses (x; y) represent 95% confidence intervals.",
    colwidths = 4  
  )

ft_model1_males

# Save as .rds and .html
saveRDS(ft_model1_males, "./RESULTS/overall_effect_males_Abstract_22.4.25.RDS")
save_as_html(ft_model1_males, path = "./RESULTS/overall_effect_males_Abstract_22.4.25.html")


# Results addendum---------
# SRI and BMI mean, survey weighted:--------
svymean(~SRI_no_imputation_winsorized_rand, nhanesDesign)
mean(df_an1_imputed$SRI_no_imputation_winsorized_rand)

svymean(~BMXBMI_winsorized, nhanesDesign)

# proportion of participants with BMI >= 30:
df_an1_imputed <- df_an1_imputed %>%
  mutate(BMI_obese = as.numeric(BMXBMI_winsorized >= 30))
# Compute the survey-weighted proportion of individuals with BMI ≥ 30
svymean(~BMI_obese, nhanesDesign)

# unweighted proportion of participants with BMI >= 30:
sum(df_an1_imputed$BMXBMI_winsorized >= 30) / nrow(df_an1_imputed)


# Reviewer comment at table 1: Do Non-Hispanic Black have a lower BMI than Non-Hispanic White?-------
# no weights
t.test(
  df_an1_imputed$BMXBMI_winsorized[df_an1_imputed$RIDRETH3_factor == "Non-Hispanic Black"],
  df_an1_imputed$BMXBMI_winsorized[df_an1_imputed$RIDRETH3_factor == "Non-Hispanic White"],
  var.equal = TRUE
)
mean(df_an1_imputed$BMXBMI_winsorized[df_an1_imputed$RIDRETH3_factor == "Non-Hispanic Black"], na.rm = TRUE)
mean(df_an1_imputed$BMXBMI_winsorized[df_an1_imputed$RIDRETH3_factor == "Non-Hispanic White"], na.rm = TRUE)

# weights
res <- svyttest(BMXBMI_winsorized ~ RIDRETH3_factor, 
         design = subset(nhanesDesign, 
         RIDRETH3_factor %in% c("Non-Hispanic Black", "Non-Hispanic White")))
res
svymean(~BMXBMI_winsorized, subset(nhanesDesign, RIDRETH3_factor == "Non-Hispanic Black"))
svymean(~BMXBMI_winsorized, subset(nhanesDesign, RIDRETH3_factor == "Non-Hispanic White"))


# __SessionInfo for replicability__--------
session_info <- capture.output(sessionInfo())
file_name <- paste0("./SessionInfos_Replicability/3_Main_Analysis_",today(),".txt")
writeLines(session_info, con = file_name)
