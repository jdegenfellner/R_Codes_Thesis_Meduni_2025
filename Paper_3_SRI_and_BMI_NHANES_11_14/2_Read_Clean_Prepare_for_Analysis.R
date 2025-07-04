# This script 
# - reads NHANES 2011-12 and 2013-14 data sets, 
# - cleans the accelerometer data,
# - calculates the sleep regularity index (SRI),
# - cleans bad SRI values
# - imputes missing values using kNN imputation
# - combines the two waves,
# - creates analytic data set for further analysis

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#update.packages(ask = FALSE) # only uncomment if you want to use sleepreg-package

source("1_Packages_install_load.R") # NOTE: In the file 1_Packages_install_load, set sleepreg_use <- TRUE/FALSE depending on use of sleepreg-package
#packageVersion("GGIR") # 2.0.0 needed to use sleepreg
conflicts_prefer(stats::filter)
conflicts_prefer(stats::lag)

# 1) READ-Data sets----
mem.maxVSize(vsize = Inf) # otherwise R crashes

# 1.1) Data:NHANES 2011-12---------
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))

# _Demographics ----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm
file_path <- file.path(parent_directory, "4_DATA", "DEMO_G_2011_12.XPT")
df1112_demo <- read_xpt(file_path)
head(df1112_demo)
dim(df1112_demo) # 9756   48
length(unique(df1112_demo$SEQN)) # 9756 different participants - each row different person. Not all of them have a PAM (physical activity monitoring) file

# _physical activity monitor (PAM) HEADER FILE----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXHD_G.htm
file_path <- file.path(parent_directory, "4_DATA", "PAXHD_G_2011_12.XPT")
df1112_header <- read_xpt(file_path)
head(df1112_header)
table(df1112_header$PAXSTS) # 6917, Indicates which participants have PAM data available in the summary
dim(df1112_header) # 7821    9

# _Accelerometer data [minute]--------
file_path <- "/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/PAXMIN_G_2011_12.XPT" # 8 GB (orange LaCie external drive)

read_fresh <- FALSE
if(read_fresh){
  stop("Please check if data should be loaded freshly - takes a while.")
}
tic()
if(read_fresh){
  df_PAX1112 <- read_xpt(file_path) 
  df_PAX1112$PAXPREDM <- factor(df_PAX1314$PAXPREDM, levels = c("1", "2", "3", "4"),
                        labels = c("wake", "sleep", "non-wear", "unknown"))
} else { # read from RDS instead
  #df_PAX1112 <- readRDS("/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/PAXMIN_2011_12.RDS")
  df_PAX1112 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2011_12.RDS")
}
toc() # 579.576 sec elapsed, fresh load, 50 sec just loading
#saveRDS(df_PAX1112, "/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2011_12.RDS") # ~1 GB

# _Body Measures ----
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/BMX_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BMX_G.XPT")
df1112_body_measures <- read_xpt(file_path)
head(df1112_body_measures)

# _Diet behaviour, nutrition----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DBQ_G.XPT")
df1112_diet_behaviour <- read_xpt(file_path)
head(df1112_diet_behaviour)

# _Blood pressure----
#https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPX_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BPX_G.XPT")
df1112_blood_pressure <- read_xpt(file_path)
head(df1112_blood_pressure)

# _Alcohol consumption----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALQ_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "ALQ_G.XPT")
df1112_alcohol_use <- read_xpt(file_path)
head(df1112_alcohol_use)

# _Smoking----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.htm
# causally linked to BMI: https://www.sciencedirect.com/science/article/pii/S0167629617301030?casa_token=zkkTXq-j3LYAAAAA:tLSgdf1KrQr0Fb-yxsmLTjIWK42aylemlSaDwxulF37HULYeBKzhT0Szk2m1WVIUYBRSWcmNxkQ
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "SMQ_G.XPT")
df1112_smoking <- read_xpt(file_path)
head(df1112_smoking)

brand_frequencies <- table(df1112_smoking$SMD100BR)
brands_to_keep <- names(brand_frequencies[brand_frequencies > 1])
df_filtered <- df1112_smoking[df1112_smoking$SMD100BR %in% brands_to_keep, ]
df_filtered$SMD100BR <- factor(df_filtered$SMD100BR, levels = names(sort(table(df_filtered$SMD100BR), decreasing = TRUE)))
ggplot(df_filtered, aes(x = SMD100BR)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Smoking Brands (Excluding Single Responses)", x = "Brand", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# _Sleep disorders----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SLQ_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "SLQ_G.XPT")
df1112_sleep_disorders <- read_xpt(file_path)
head(df1112_sleep_disorders)

# _Mental health, depression----
# Depression associated with BMI: https://www.nature.com/articles/ijo2008268
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DPQ_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DPQ_G.XPT")
df1112_depression <- read_xpt(file_path)
head(df1112_depression)
dim(df1112_depression)

# _Diabetes----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.htm#DIQ010
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DIQ_G.XPT")
df1112_diabetes <- read_xpt(file_path)
head(df1112_diabetes)
dim(df1112_diabetes)

# _Early childhood----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ECQ_G.htm#ECQ020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "ECQ_G.XPT")
df1112_early_childhood <- read_xpt(file_path)
head(df1112_early_childhood)
dim(df1112_early_childhood)

# _Drug use----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DUQ_G.XPT")
df1112_drug_use <- read_xpt(file_path)
head(df1112_drug_use)
dim(df1112_drug_use)

# _Occupation-----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "OCQ_G.XPT")
df1112_occupation <- read_xpt(file_path)
head(df1112_occupation)
dim(df1112_occupation)

# _Cardiovascular Health (CDQ_G)----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "CDQ_G.XPT")
df1112_cardiovascular_health <- read_xpt(file_path)
head(df1112_cardiovascular_health)
dim(df1112_cardiovascular_health)

# _Vitamin D----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "VID_G.XPT")
df1112_vitamin_d <- read_xpt(file_path)
head(df1112_vitamin_d)

# _Dietary Interview - Total Nutrient Intakes, First Day (DR1TOT_G)--------
# https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DR1TOT_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR1TOT_G.XPT")
df1112_dietary_intake <- read_xpt(file_path)
head(df1112_dietary_intake)

# _Dietary Interview - Total Nutrient Intakes, Second Day (DR2TOT_G)--------
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DR2TOT_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR2TOT_G.XPT")
df1112_dietary_intake_2 <- read_xpt(file_path)
head(df1112_dietary_intake_2)

# _Dietary Interview - Individual Foods, First Day (DR1IFF_G)---------
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DR1IFF_G.htm#DR1_020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR1IFF_G.XPT")
df1112_dietary_intake_individual_foods <- read_xpt(file_path)
head(df1112_dietary_intake_individual_foods)

# _Dietary Interview - Individual Foods, Second Day (DR2IFF_G)---------
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DR2IFF_G.htm#DR2_020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR2IFF_G.XPT")
df1112_dietary_intake_individual_foods_2 <- read_xpt(file_path)
head(df1112_dietary_intake_individual_foods_2)

# _Cholesterol----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HDL_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "HDL_G.XPT")
df1112_cholesterol <- read_xpt(file_path)
head(df1112_cholesterol)

# _Dual-Energy X-ray Absorptiometry----------
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DXXAG_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DXXAG_G.XPT")
df1112_dxa <- read_xpt(file_path)
head(df1112_dxa)

# _Dual-Energy X-ray Absorptiometry - Whole Body (DXX_G)----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DXX_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DXX_G.XPT")
df1112_dxa_whole_body <- read_xpt(file_path)
head(df1112_dxa_whole_body)

# _Standard Biochemistry Profile (BIOPRO_G)----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BIOPRO_G.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BIOPRO_G.XPT")
df1112_biochemistry <- read_xpt(file_path)
head(df1112_biochemistry)



# 1.2) Data:NHANES 2013-14-------
current_directory <- getwd()
parent_directory <- dirname(current_directory)
# _Demographic data----
file_path <- file.path(parent_directory, "4_DATA", "DEMO_H_2013_14.XPT")
df1314_demo <- read_xpt(file_path)
head(df1314_demo)

# _physical activity monitor (PAM) HEADER FILE----
file_path <- file.path(parent_directory, "4_DATA", "PAXHD_H_2013_14.XPT")
df1314_header <- read_xpt(file_path)
head(df1314_header)
# PAXSTS: This header file (PAXHD_H) variable indicates if the participant has
# PAM data available in the summary files: "1 (Yes)"
table(df1314_header$PAXSTS) # 7776 

length(unique(df1314_header$SEQN)) # 8913
#intersect(df1112_demo$SEQN, df1314_demo$SEQN) # no overlapping IDs 2011/12 and 2013/14

# _Accelerometer data [minute]--------
file_path <- "/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/PAXMIN_H_2013_14.XPT"
read_fresh <- FALSE
if(read_fresh){
  stop("Please check if data should be loaded freshly - takes a while.")
}
tic()
if(read_fresh){
  df_PAX1314 <- read_xpt(file_path) 
  df_PAX1314$PAXPREDM <- factor(df_PAX1314$PAXPREDM, levels = c("1", "2", "3", "4"),
                        labels = c("wake", "sleep", "non-wear", "unknown"))
} else { # read from RDS instead
  #df_PAX1314 <- readRDS("/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/PAXMIN_2013_14.RDS")
  df_PAX1314 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2013_14.RDS")
  # https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PAXMIN_H.htm#Analytic_Notes
}
toc() # ~50s
#saveRDS(df_PAX1314, "/Users/juergen/Large_R_Files/NHANES_2011_14/PAXMIN_2013_14.RDS")

# _Body Measures ----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BMX_H.XPT")
df1314_body_measures <- read_xpt(file_path)
head(df1314_body_measures)

# _Diet behaviour, nutrition----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DBQ_H.XPT") # https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DBQ_H.htm
df1314_diet_behaviour <- read_xpt(file_path)
head(df1314_diet_behaviour)

# _Blood pressure----
#https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPX_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BPX_H.XPT")
df1314_blood_pressure <- read_xpt(file_path)
head(df1314_blood_pressure)

# _Alcohol consumption----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/ALQ_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "ALQ_H.XPT")
df1314_alcohol_use <- read_xpt(file_path)
head(df1314_alcohol_use)

# _Smoking----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.htm
# causally linked to BMI: https://www.sciencedirect.com/science/article/pii/S0167629617301030?casa_token=zkkTXq-j3LYAAAAA:tLSgdf1KrQr0Fb-yxsmLTjIWK42aylemlSaDwxulF37HULYeBKzhT0Szk2m1WVIUYBRSWcmNxkQ
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "SMQ_H.XPT")
df1314_smoking <- read_xpt(file_path)
head(df1314_smoking)

# _Sleep disorders----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SLQ_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "SLQ_H.XPT")
df1314_sleep_disorders <- read_xpt(file_path)
head(df1314_sleep_disorders)

# _Mental health, depression----
# Depression associated with BMI: https://www.nature.com/articles/ijo2008268
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DPQ_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DPQ_H.XPT")
df1314_depression <- read_xpt(file_path)
head(df1314_depression)

# _Diabetes----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DIQ_H.XPT")
df1314_diabetes <- read_xpt(file_path)
head(df1314_diabetes)
dim(df1314_diabetes)

# _Early childhood----
# https://wwwn.cdc.gov/Nchs/Nhanes/2012-2013/ECQ_H.htm#ECQ020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "ECQ_H.XPT")
df1314_early_childhood <- read_xpt(file_path)
head(df1314_early_childhood)
dim(df1314_early_childhood)

# _Drug use----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DUQ_H.XPT")
df1314_drug_use <- read_xpt(file_path)
head(df1314_drug_use)
dim(df1314_drug_use)

# _[only 2013/14]Coffee laboratory-------
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/CAFE_H.htm#URXMX7
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "CAFE_H.XPT")
df1314_cafe <- read_xpt(file_path)
head(df1314_cafe)
dim(df1314_cafe)

# _Occupation-----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "OCQ_H.XPT")
df1314_occupation <- read_xpt(file_path)
head(df1314_occupation)
dim(df1314_occupation)

# _Cardiovascular Health (CDQ_H)----
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "CDQ_H.XPT")
df1314_cardiovascular_health <- read_xpt(file_path)
head(df1314_cardiovascular_health)

# _Vitamin D----
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VID_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "VID_H.XPT")
df1314_vitamin_d <- read_xpt(file_path)
head(df1314_vitamin_d)

# _Dietary Interview - Total Nutrient Intakes, First Day (DR1TOT_H)--------
# https://www.cdc.gov/nchs/nhanes/2013-2014/DR1TOT_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR1TOT_H.XPT")
df1314_dietary_intake <- read_xpt(file_path)
head(df1314_dietary_intake)

# _Dietary Interview - Total Nutrient Intakes, second Day (DR2TOT_H)--------
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR2TOT_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR2TOT_H.XPT")
df1314_dietary_intake_2 <- read_xpt(file_path)
head(df1314_dietary_intake_2)

# _Dietary Interview - Individual Foods, First Day (DR1IFF_H)---------
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR1IFF_H.htm#DR1_020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR1IFF_H.XPT")
df1314_dietary_intake_individual_foods <- read_xpt(file_path)
head(df1314_dietary_intake_individual_foods)

# _Dietary Interview - Individual Foods, Second Day (DR2IFF_H)---------
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR2IFF_H.htm#DR2_020
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DR2IFF_H.XPT")
df1314_dietary_intake_individual_foods_2 <- read_xpt(file_path)
head(df1314_dietary_intake_individual_foods_2)

# _Cholesterol----
# https://www.cdc.gov/Nchs/Nhanes/2013-2014/HDL_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "HDL_H.XPT")
df1314_cholesterol <- read_xpt(file_path)
head(df1314_cholesterol)

# _Dual-Energy X-ray Absorptiometry----------
# https://www.cdc.gov/Nchs/Nhanes/2013-2014/DXXAG_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DXXAG_H.XPT")
df1314_dxa <- read_xpt(file_path)
head(df1314_dxa)

# _Dual-Energy X-ray Absorptiometry - Whole Body (DXX_H)----
# https://www.cdc.gov/Nchs/Nhanes/2013-2014/DXX_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "DXX_H.XPT")
df1314_dxa_whole_body <- read_xpt(file_path)
head(df1314_dxa_whole_body)

# _Standard Biochemistry Profile (BIOPRO_H)----
# https://www.cdc.gov/Nchs/Nhanes/2013-2014/BIOPRO_H.htm
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "BIOPRO_H.XPT")
df1314_biochemistry <- read_xpt(file_path)
head(df1314_biochemistry)


# 2) Clean Accelerometer data ----
head(df_PAX1314)
dim(df_PAX1314)
# Note: PAXMTSM = PAXMXM + PAXMYM + PAXMZM
barplot(table(df_PAX1314$PAXPREDM)) # wake/sleep/non-wear/unknown predetermined by hidden markov model; relatively large proportion of non-wear
sum(is.na(df_PAX1314$PAXPREDM)) # 0; all states are labeled
hist(df_PAX1314$PAXTSM) # here, all are 60 seconds of data for the respective minute
length(unique(df_PAX1314$SEQN)) # 6917 different people have PAM file


# PAXPREDM (Predicted wake/sleep/non-wear status during the minute)
# This variable in the minute summary file (PAXMIN_H) was established
# using a machine learning algorithm with the raw data collected
# during the minute. 
# The PAM wearing status of the minute was # classified as
# "wake", "sleep", "non-wear", or "unknown". 
# - A "wake" status indicates the participant is likely to be awake during the minute, 
# - and the "sleep" status indicates the participant is likely to be asleep based on the algorithm.
# - The "non-wear" status indicates the algorithm suggested that the participant did not wear the PAM during this minute, 
# - and the "unknown" status indicates that the algorithm could not determine the status for this minute. 

# Not every eligible NHANES participant has a data file. For example, if a
# participant refused to wear the PAM, did not return it, or the data could
# not be retrieved from a damaged PAM, then the participant does not have a
# data file. Each participant may have up to 9 days of summary records. Per the
# protocol, the first and last day of data collection for each participant are
# partial days. For most participants, a complete data collection constitutes
# 193 hours. In some cases, e.g., due to battery depletion, data collection was
# shorter. A small proportion of participant data files include 194 hours.

# FILTER PAXMIN-minute(accelerometer-)data --------
filter_accelo_data_NHANES_11_to_14 <- function(df, n_sub) {
    
    # Filter the data based on n_sub
    if (n_sub == "all") {
      df_filtered <- df
    } else {
      df_filtered <- df %>%
        dplyr::filter(SEQN %in% unique(df$SEQN)[1:n_sub])
    }
    
    # Initialize lists to store the number of participants and excluded SEQNs after each step
    participants_tracker <- list()
    excluded_seqn_tracker <- list()
    participants_tracker$initial <- length(unique(df$SEQN))
    
    # Step 0: Define binary_sleep variable
    df_filtered <- df_filtered %>%
      dplyr::mutate(binary_sleep = case_when(
        PAXPREDM == "sleep" ~ 1,
        PAXPREDM == "wake" ~ 0,
        TRUE ~ NA_real_
      )) 
    
    # Step 1: Filter for only 60s observations for the current minute
    initial_seqn <- unique(df_filtered$SEQN)
    df_filtered <- df_filtered %>%
      dplyr::filter(PAXTSM == 60)
    participants_tracker$after_60s_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_60s_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 2: Filter for data quality flag count
    initial_seqn <- unique(df_filtered$SEQN)
    df_filtered <- df_filtered %>%
      dplyr::filter(PAXQFM == 0)
    participants_tracker$after_quality_flag_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_quality_flag_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 3: Filter for at least 22 hours of wearing (non-wear filter) (see Validation paper)
    # filter out participants with less than 3 days of <120min non-wear time
    initial_seqn <- unique(df_filtered$SEQN)
    valid_non_wear_time_days <- df_filtered %>% # defined as: days with less than 2 hours of non-wear time
      dplyr::group_by(SEQN, PAXDAYM) %>%  # unique(df_PAX1112$PAXDAYM) : "1" "2" "3" "4" "5" "6" "7" "8" "9"
      dplyr::summarise(non_wear_time = sum(PAXPREDM == "non-wear"), .groups = 'drop') %>% 
      dplyr::filter(non_wear_time < 2*60) %>%  # Keeping days with less than 2 hours (120 minutes) of non-wear time
      dplyr::ungroup() %>%
      dplyr::group_by(SEQN) %>% 
      dplyr::summarise(valid_non_wear_days = n_distinct(PAXDAYM), .groups = 'drop') %>% 
      dplyr::filter(valid_non_wear_days >= 3) %>% # valid non wear day is a days with less than 2 hours of non-wear time
      dplyr::ungroup()
    df_filtered <- df_filtered %>%
      dplyr::filter(SEQN %in% valid_non_wear_time_days$SEQN)
    participants_tracker$after_non_wear_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_non_wear_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 4: filter for 1440 time steps per day
    # filter out participants with less than 5 days of 1440 time steps days.
    initial_seqn <- unique(df_filtered$SEQN)
    
    df_filtered <- df_filtered %>%
      dplyr::group_by(SEQN, PAXDAYM) %>%
      dplyr::mutate(time_steps_for_PAXDAYM = n()) %>%
      dplyr::filter(time_steps_for_PAXDAYM == 1440) %>% 
      dplyr::ungroup()
    
    participants_tracker$after_time_steps_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_time_steps_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 5: Filter for at least X consecutive days including a Saturday or Sunday
    initial_seqn <- unique(df_filtered$SEQN)
    df_filtered <- df_filtered %>%
      dplyr::group_by(SEQN) %>%
      dplyr::filter({
        day_seq <- sort(as.numeric(unique(PAXDAYWM))) # Convert day of week PAXDAYWM to numeric
        if (sum(!is.na(day_seq)) < 3) { # At least 3 days of data per participant
          consecutive <- FALSE # formally not entirely correct: there are just not 3 days, but the 2 can be consecutive nevertheless, but should work.
        } else {
          #consecutive <- max(rle(diff(day_seq))$lengths, na.rm = TRUE) >= 4 # At least 5 consecutive days
          consecutive <- max(rle(diff(day_seq))$lengths, na.rm = TRUE) >= 2 # At least 3 consecutive days
        }
        weekend_day <- any(day_seq %in% c(1, 7), na.rm = TRUE) # Includes Saturday or Sunday; see for example: https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PAXDAY_H.htm#PAXDAYWD
        consecutive && weekend_day
      }) %>%
      dplyr::ungroup()
    participants_tracker$after_consecutive_days_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_consecutive_days_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 6: Calculate missingness for each SEQN and exclude participants
    # with missingness rate > 30%
    initial_seqn <- unique(df_filtered$SEQN)
    
    missingness <- df_filtered %>%
      dplyr::group_by(SEQN) %>%
      dplyr::summarize(
        total_observations = n(),
        missing_count = sum(is.na(binary_sleep)),
        missingness_rate = missing_count / total_observations
      ) %>%
      dplyr::ungroup() %>%
      as.data.table()
    
    df_filtered <- df_filtered %>% 
      dplyr::filter(SEQN %in% missingness$SEQN[missingness$missingness_rate < 0.3]) # missingness per SEQN < 30%
    
    participants_tracker$after_missingness_filter <- length(unique(df_filtered$SEQN))
    excluded_seqn_tracker$after_missingness_filter <- setdiff(initial_seqn, unique(df_filtered$SEQN))
    
    # Step 7: Calculate the percentage of rows and participants lost to filtering
    if (n_sub == "all") {
      rows_lost_percentage <- 100 - (dim(df_filtered)[1] / dim(df)[1] * 100)
    } else {
      rows_lost_percentage <- 100 - (dim(df_filtered)[1] / dim(df %>%
                                                                 filter(SEQN %in% unique(df$SEQN)[1:n_sub]))[1] * 100) 
    }
    percent_participants_lost <- 100 - length(unique(df_filtered$SEQN)) / participants_tracker$initial * 100
    
    # Step 8: Check missingness in binary_sleep
    missingness_binary_sleep <- sum(is.na(df_filtered$binary_sleep)) / length(df_filtered$binary_sleep)
    
    # Step 9: Return the filtered data, summary statistics, participants tracker, and excluded SEQNs
    return(list(
      filtered_data = df_filtered,
      rows_lost_percentage = rows_lost_percentage,
      missingness_binary_sleep = missingness_binary_sleep,
      participants_tracker = participants_tracker,
      percent_participants_lost = percent_participants_lost,
      excluded_seqn_tracker = excluded_seqn_tracker
    ))
}


conflicts_prefer(dplyr::lag)
# Filter each wave separately
tic()
res_wave_1112 <- filter_accelo_data_NHANES_11_to_14(df_PAX1112, n_sub = "all")
toc() # 110s
tic()
res_wave_1314 <- filter_accelo_data_NHANES_11_to_14(df_PAX1314, n_sub = "all")
toc() # 150s
# Combine participant trackers
combined_participants_tracker <- list(
  wave_1112 = res_wave_1112$participants_tracker,
  wave_1314 = res_wave_1314$participants_tracker
)

# Combine SEQN trackers
combined_excluded_seqn_tracker <- list(
  wave_1112 = res_wave_1112$excluded_seqn_tracker,
  wave_1314 = res_wave_1314$excluded_seqn_tracker
)

# Final combined results
final_results <- list(
  participants_tracker = combined_participants_tracker,
  excluded_seqn_tracker = combined_excluded_seqn_tracker
)


# _2011/12 wave-------
length(unique(df_PAX1112$SEQN)) # 6917
#results_1112 <- filter_accelo_data_NHANES_11_to_14(df_PAX1112, n_sub = 1000)
tic()
results_1112 <- filter_accelo_data_NHANES_11_to_14(df_PAX1112, n_sub = "all")
toc() # 119.682 sec elapsed
results_1112$filtered_data 
results_1112$rows_lost_percentage # ~ 38.9 % 
results_1112$missingness_binary_sleep # ~ 6.6%
results_1112$participants_tracker # 
results_1112$excluded_seqn_tracker

# double check SEQN lost:
# 6917 - 5243 = 1674 participants lost due to filtering
length(unique(df_PAX1112$SEQN)) # 6917
length(unique(results_1112$filtered_data$SEQN)) # 5243
1-length(unique(results_1112$filtered_data$SEQN))/length(unique(df_PAX1112$SEQN)) # 24.1% participants lost due to filtering
# double check rows lost:
1-dim(results_1112$filtered_data)[1]/dim(df_PAX1112)[1] # 38.9% rows lost due to filtering

# 2011-12 Flowchart of filtering process----
df_filtering_1112 <- data.frame(SEQN = seq(1, results_1112$participants_tracker$initial))  # Simulate the df with SEQN values

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "1_Main_document/Figures", "Filter_accel_NHANES_2011_12.pdf")
pdf(file_path, width = 7, height = 10)

df_filtering_1112 |> as_fc(label = "NHANES 2011-2012 Public release") |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN, 
            label = paste0("Initial: ", results_1112$participants_tracker$initial)) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_60s_filter], 
            label = paste0("After 60s Filter: ", results_1112$participants_tracker$after_60s_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_quality_flag_filter], 
            label = paste0("After Quality Flag Filter: ", results_1112$participants_tracker$after_quality_flag_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_non_wear_filter], 
            label = paste0("After Non-wear Filter: ", results_1112$participants_tracker$after_non_wear_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_time_steps_filter], 
            label = paste0("After Time Steps Filter: ", results_1112$participants_tracker$after_time_steps_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_consecutive_days_filter], 
            label = paste0("After Consecutive Days Filter: ", results_1112$participants_tracker$after_consecutive_days_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1112$SEQN[1:results_1112$participants_tracker$after_missingness_filter], 
            label = paste0("After Missingness Filter: ", results_1112$participants_tracker$after_missingness_filter), show_exc = TRUE) |>
  
  fc_draw()

dev.off()

tic()
conflicts_prefer(stats::filter)
saveRDS(results_1112, "/Users/juergen/Large_R_Files/NHANES_2011_14/results_1112_filtered_not_imputed.RDS")
toc() # 156s

# READ 1112 filtered--------
#results_1112 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1112_filtered_not_imputed.RDS")




# _2013/14 wave-------
length(unique(df_PAX1314$SEQN)) # 7776
#results_1314 <- filter_accelo_data_NHANES_11_to_14(df_PAX1314, n_sub = 1000)
tic()
results_1314 <- filter_accelo_data_NHANES_11_to_14(df_PAX1314, n_sub = "all")
toc() # 126.347 sec elapsed
results_1314$rows_lost_percentage # ~ 43 % rows lost due to filtering
results_1314$missingness_binary_sleep # ~ 6.6%
results_1314$participants_tracker

# 2013-14 Flowchart of filtering process----
df_filtering_1314 <- data.frame(SEQN = seq(1, results_1314$participants_tracker$initial))  # Simulate the df with SEQN values

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "1_Main_document/Figures", "Filter_accel_NHANES_2013_14.pdf")
pdf(file_path, width = 7, height = 10)

df_filtering_1314 |> as_fc(label = "NHANES 2013-2014 Public release") |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN, 
            label = paste0("Initial: ", results_1314$participants_tracker$initial)) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_60s_filter], 
            label = paste0("After 60s Filter: ", results_1314$participants_tracker$after_60s_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_quality_flag_filter], 
            label = paste0("After Quality Flag Filter: ", results_1314$participants_tracker$after_quality_flag_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_non_wear_filter], 
            label = paste0("After Non-wear Filter: ", results_1314$participants_tracker$after_non_wear_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_time_steps_filter], 
            label = paste0("After Time Steps Filter: ", results_1314$participants_tracker$after_time_steps_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_consecutive_days_filter], 
            label = paste0("After Consecutive Days Filter: ", results_1314$participants_tracker$after_consecutive_days_filter), show_exc = TRUE) |>
  
  fc_filter(SEQN %in% df_filtering_1314$SEQN[1:results_1314$participants_tracker$after_missingness_filter], 
            label = paste0("After Missingness Filter: ", results_1314$participants_tracker$after_missingness_filter), show_exc = TRUE) |>
  
  fc_draw()

dev.off()

tic()
saveRDS(results_1314, "/Users/juergen/Large_R_Files/NHANES_2011_14/results_1314_filtered_not_imputed.RDS")
toc() # 

# READ 1314 filtered--------
#results_1314 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1314_filtered_not_imputed.RDS")



# end FILTERING -> passing data sets-----
df_PAX1112_filtered <- results_1112$filtered_data
df_PAX1314_filtered <- results_1314$filtered_data


# 3) SRI calculation----
# Custom Function for SRI-calculation--------

# https://www.accelting.com/updates/ggir-release-2-5-0/
# "The SRI is proposed to only be calculated based on seven, or a multitude 
# of seven, consecutive days of data without missing values. This to avoid a 
# possible role of imbalanced data to the final estimate. However, this renders
# many datasets unsuitable for analysis and leads to a painful loss in sample 
# size and statistical power."

# Function to perform one round of imputation considering only adjacent days
impute_na_values <- function(filtered_data, 
                             M, # Number of time steps in a day, in our case 1440=60*24
                             simple_rule_imputation, # 1=Maximize SRI, 2=Minimize SRI, 3=LOCF
                             fancy_imputation = FALSE # use other imputation methods: kNN_VIM, .... 
) {
  # Info: 
  # - Solve heuristically to find the maximum/minimum
  #   possible SRI by imputing missing values in the binary_sleep column.
  #   It looks at the current value and +/- M values to decide on the imputation.
  # - It also uses other imputation methods to impute binary_sleep
  
  number_rows <- nrow(filtered_data)
  na_indices <- which(is.na(filtered_data$binary_sleep))
  
  if(fancy_imputation == "LOCF"){
    filtered_data$binary_sleep <- imputeTS::na_locf(filtered_data$binary_sleep, option = "locf")
    return(filtered_data)
  }
  
  if(fancy_imputation == "NOCB"){
    filtered_data$binary_sleep <- imputeTS::na_locf(filtered_data$binary_sleep, option = "nocb")
    return(filtered_data)
  }
  
  if(fancy_imputation == "random_values"){
    filtered_data$binary_sleep[is.na(filtered_data$binary_sleep)] <- sample(0:1, sum(is.na(filtered_data$binary_sleep)), replace = TRUE)
    return(filtered_data)
  }
  
  if(fancy_imputation == "random_values_base_proportions"){
    # Impute missing values with random values based on the proportions of 0s and 1s in the data
    binary_sleep_proportions <- table(filtered_data$binary_sleep) / nrow(filtered_data)
    filtered_data$binary_sleep[is.na(filtered_data$binary_sleep)] <- sample(c(0, 1), sum(is.na(filtered_data$binary_sleep)), replace = TRUE, prob = binary_sleep_proportions)
    return(filtered_data)
  }
  
  if(fancy_imputation == "kNN_VIM"){
    # Impute missing values using kNN imputation from the package VIM; https://cran.r-project.org/web/packages/VIM/VIM.pdf
    print(paste0("Missing values before kNN: ", sum(is.na(filtered_data$binary_sleep))))
    filtered_data <- VIM::kNN(filtered_data, 
                              k = 5, 
                              variable = "binary_sleep")
    print(paste0("Missing values after kNN: ", sum(is.na(filtered_data$binary_sleep))))
    return(filtered_data)
  }
  
  for (i in na_indices) {
    left_index <- i - M
    right_index <- i + M
    
    # Ensure left_index and right_index are within bounds
    if (left_index > 0 && right_index <= number_rows) {
      left_value <- filtered_data$binary_sleep[left_index]
      right_value <- filtered_data$binary_sleep[right_index]
      
      # Convert days to numeric to ensure proper comparison
      current_day <- as.numeric(filtered_data$PAXDAYWM[i])
      left_day <- as.numeric(filtered_data$PAXDAYWM[left_index])
      right_day <- as.numeric(filtered_data$PAXDAYWM[right_index])
      
      # Adjust day comparison to account for wrapping around day 7 to day 1
      consecutive_left <- (current_day - left_day == 1) || (current_day == 1 && left_day == 7)
      consecutive_right <- (right_day - current_day == 1) || (current_day == 7 && right_day == 1)
      
      if (consecutive_left && consecutive_right) {
        # Perform imputation only if the days are consecutive
        if (is.na(left_value) && is.na(right_value)) {
          # Case: NA -> NA -> NA
          if (simple_rule_imputation == 1) { # Maximize SRI
            filtered_data$binary_sleep[left_index] <- 1
            filtered_data$binary_sleep[i] <- 1
            filtered_data$binary_sleep[right_index] <- 1
          } else if (simple_rule_imputation == 2) { # Minimize SRI
            filtered_data$binary_sleep[left_index] <- 1
            filtered_data$binary_sleep[i] <- 0
            filtered_data$binary_sleep[right_index] <- 1
          }
        } else if (is.na(left_value) && !is.na(right_value)) {
          # Case: NA -> NA -> 0 or 1
          if (right_value == 1) {
            if (simple_rule_imputation == 1) { # Maximize SRI
              filtered_data$binary_sleep[left_index] <- 1
              filtered_data$binary_sleep[i] <- 1
            } else if (simple_rule_imputation == 2) { # Minimize SRI
              filtered_data$binary_sleep[left_index] <- 1
              filtered_data$binary_sleep[i] <- 0
            }
          } else if (right_value == 0) {
            if (simple_rule_imputation == 1) { # Maximize SRI
              filtered_data$binary_sleep[left_index] <- 0
              filtered_data$binary_sleep[i] <- 0
            } else if (simple_rule_imputation == 2) { # Minimize SRI
              filtered_data$binary_sleep[left_index] <- 0
              filtered_data$binary_sleep[i] <- 1
            }
          }
        } else if (!is.na(left_value) && is.na(right_value)) {
          # Case: 0 or 1 -> NA -> NA
          if (left_value == 1) {
            if (simple_rule_imputation == 1) { # Maximize SRI
              filtered_data$binary_sleep[i] <- 1
              filtered_data$binary_sleep[right_index] <- 1
            } else if (simple_rule_imputation == 2) { # Minimize SRI
              filtered_data$binary_sleep[i] <- 0
              filtered_data$binary_sleep[right_index] <- 1
            }
          } else if (left_value == 0) {
            if (simple_rule_imputation == 1) { # Maximize SRI
              filtered_data$binary_sleep[i] <- 0
              filtered_data$binary_sleep[right_index] <- 0
            } else if (simple_rule_imputation == 2) { # Minimize SRI
              filtered_data$binary_sleep[i] <- 1
              filtered_data$binary_sleep[right_index] <- 0
            }
          }
        } else if (!is.na(left_value) && !is.na(right_value)) {
          # Case: 0 or 1 -> NA -> 0 or 1
          if (simple_rule_imputation == 1) { # Maximize SRI
            filtered_data$binary_sleep[i] <- ifelse(left_value == 1 || right_value == 1, 1, 0)
          } else if (simple_rule_imputation == 2) { # Minimize SRI
            filtered_data$binary_sleep[i] <- ifelse(left_value == 1 || right_value == 1, 0, 1)
          }
        }
      }
    }
  }
  
  return(filtered_data)
}

calculate_sri <- function(data, 
                          seqn, 
                          visualize_sleep_wake = FALSE,
                          raster_plot = FALSE,
                          simple_rule_imputation = FALSE,
                          fancy_imputation = FALSE, # use other imputation methods: kNN_VIM, ....
                          theoretical_imputation_limits = FALSE) { 
  
  if(seqn %nin% data$SEQN){
    print(paste0("SEQN = ", seqn, " not in data set."))
    return(NA)
  }
  
  filtered_data <- data %>% dplyr::filter(SEQN == seqn) # only one participant at a time
  print(seqn)
  
  M <- 1440 # Number of necessary daily epochs to be considered a full day
  day_seq <- sort(as.numeric(unique(filtered_data$PAXDAYWM))) # Convert PAXDAYWM to numeric
  print(day_seq)
  print(sum(diff(day_seq)>1))
  
  if (max(diff(day_seq)) > 1) {
    cat("Warning: Non-consecutive days detected for SEQN:", seqn, "\n")
  }
  
  # Imputation:
  if(simple_rule_imputation > 0 | # 1 = (heuristally) Maximize SRI, 2 = (heuristally) Minimize SRI
     fancy_imputation != FALSE){ # use other imputation methods: kNN_VIM, ....
    
    theoretical_imputation_limits <- FALSE # This calculation below makes no sense.
    filtered_data <- impute_na_values(filtered_data, 
                                      M, 
                                      simple_rule_imputation,
                                      fancy_imputation = fancy_imputation)
  }
  
  # Calculate the final delta_sum
  number_rows <- nrow(filtered_data)
  delta_sum <- 0
  N_v <- 0 # count number of valid epoch-comparisons
  for (i in 1:(number_rows - M)) {
    current_day <- as.numeric(filtered_data$PAXDAYWM[i])
    next_day <- as.numeric(filtered_data$PAXDAYWM[i + M])
    
    if ((next_day - current_day == 1) || (current_day == 7 && next_day == 1)) { # Check for consecutive days
      if (!is.na(filtered_data$binary_sleep[i]) && 
          !is.na(filtered_data$binary_sleep[i + M])){
        N_v <- N_v + 1
        if(filtered_data$binary_sleep[i] == filtered_data$binary_sleep[i + M]) {
          delta_sum <- delta_sum + 1
        }
      }
    }
  }
  
  SRI <- -100 + 200/N_v * delta_sum
  
  if(theoretical_imputation_limits == TRUE){
    n_star <- length(which(is.na(filtered_data$binary_sleep)))
    SRI_lower_th <- round(-100 + 200/(N_v + n_star) * (delta_sum + 2*n_star*0),2)
    SRI_upper_th <- round(-100 + 200/(N_v + n_star) * (delta_sum + 2*n_star*1),2)
    print(paste0("SRI theoretical imputation limits = [",SRI_lower_th,", ",SRI_upper_th,"]"))
  }
  
  print(paste0("Missings after imputation: ", sum(is.na(filtered_data$binary_sleep)) ))
  print(paste0("N_v = ", N_v))
  print(paste0("delta_sum = ", delta_sum))
  
  # if(visualize_sleep_wake == TRUE){
  #   
  #   p1 <- filtered_data %>% 
  #     mutate(row_num = row_number()) %>%
  #     ggplot(aes(x = row_num, y = binary_sleep)) + 
  #     geom_point(size = 0.1) +
  #     geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
  #                color = "green", linetype = "solid", size = 0.1) +
  #     geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
  #                color = "turquoise", linetype = "solid", size = 0.1) +
  #     geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  #     scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  #     labs(title = paste("Binary Sleep with NA (green=non-wear; turquoise=unknown) Indicators for SEQN", seqn),
  #          x = "Time Step",
  #          y = "Binary Sleep") +
  #     theme_minimal() +  # Use a minimal theme
  #     theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
  #           panel.grid.minor = element_blank()) +     # Remove minor grid lines 
  #     theme(plot.title = element_text(hjust = 0.5))
  #   print(p1)
  # }
  
  if(visualize_sleep_wake == TRUE){
    # Create a column that indicates whether the data point was used for SRI calculation
    filtered_data <- filtered_data %>%
      mutate(row_num = row_number()) %>%
      mutate(used_for_SRI = ifelse(!is.na(binary_sleep) & 
                                     lead(!is.na(binary_sleep), M) & 
                                     (as.numeric(PAXDAYWM) - lag(as.numeric(PAXDAYWM), M) == 1 | 
                                        (PAXDAYWM == 7 & lead(PAXDAYWM, M) == 1)), 1, 0))
    
    # Plot showing Binary Sleep with non-wear and unknown indicators, and marking data used for SRI
    p1 <- ggplot(filtered_data, aes(x = row_num, y = binary_sleep)) + 
      geom_point(size = 0.1) +
      geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
                 color = "green", linetype = "solid", size = 0.1) +
      geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
                 color = "turquoise", linetype = "solid", size = 0.1) +
      geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line at 0.5
      geom_point(aes(x = row_num, y = 0.5), data = function(x) x[x$used_for_SRI == 1, ], color = "orange", size = 0.5) +  # Mark the points used in SRI calculation
      scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
      labs(title = paste("Binary Sleep with NA (green=non-wear; turquoise=unknown) and SRI Data (orange) for SEQN", seqn),
           x = "Time Step",
           y = "Binary Sleep") +
      theme_minimal() +  # Use a minimal theme
      theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
            panel.grid.minor = element_blank()) +     # Remove minor grid lines 
      theme(plot.title = element_text(hjust = 0.5))
    
    print(p1)
  }
  
  if(raster_plot == TRUE){
    p2 <- ggplot(filtered_data, aes(x = UNIX_TIMESTAMP, y = as.factor(PAXDAYWM), fill = factor(binary_sleep))) +
      geom_tile(color = "white", width = 600, height = 1) +
      scale_fill_manual(values = c("1" = "green", "0" = "blue", "NA" = "red")) +
      labs(title = "Sleep Patterns Over Days", x = "Time (UNIX timestamp)", y = "Day Number") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 10), labels = scales::date_format("%Y-%m-%d")) +
      theme(axis.text.x = element_text(angle = 45))
    print(p2)
  }
  
  return(SRI)
}

# Function to calculate sleep/wake proportions and identify top/bottom 5%
calculate_sleep_wake_proportion <- function(data, seqn) {
  # Filter data for the given SEQN
  individual_data <- data %>% filter(SEQN == seqn)
  
  if (nrow(individual_data) == 0) {
    print(paste0("SEQN = ", seqn, " not in data set."))
    return(NA)
  }
  
  # Calculate the total number of rows (time points) for the individual
  total_rows <- nrow(individual_data)
  
  # Calculate the sum of sleep (binary_sleep == 1) and wake (binary_sleep == 0) states
  sleep_time <- sum(individual_data$binary_sleep == 1, na.rm = TRUE)
  wake_time <- sum(individual_data$binary_sleep == 0, na.rm = TRUE)
  
  # Calculate proportions
  sleep_proportion <- sleep_time / total_rows
  wake_proportion <- wake_time / total_rows
  
  # Determine top/bottom 5% thresholds for sleep and wake proportions
  top_5_sleep_threshold <- 0.95 # More than 95% sleep
  top_5_wake_threshold <- 0.95  # More than 95% wake (sleep proportion < 5%)
  
  # Classify the individual
  if (sleep_proportion >= top_5_sleep_threshold) {
    classification <- "Top 5% sleepers"
  } else if (wake_proportion >= top_5_wake_threshold) {
    classification <- "Top 5% awake"
  } else {
    classification <- "Normal range"
  }
  
  # Return a summary for the individual
  return(list(SEQN = seqn,
              sleep_proportion = sleep_proportion,
              wake_proportion = wake_proportion,
              classification = classification))
}

# Example usage of the function for one SEQN
unique_seqns <- unique(df_PAX1112_filtered$SEQN)
seqn_random_sel <- sample(unique_seqns, 1)
seqn_random_sel
result <- calculate_sleep_wake_proportion(data = df_PAX1112_filtered, seqn = seqn_random_sel)
print(result)

# for all SEQN in df_PAX1112_filtered:
unique_seqns <- unique(df_PAX1112_filtered$SEQN)
tic()
results <- pblapply(unique_seqns, function(seqn) {
  calculate_sleep_wake_proportion(df_PAX1112_filtered, seqn)
})
toc() # 1498.939 sec elapsed
results_df <- do.call(rbind, lapply(results, as.data.frame))
head(results_df)

# SAVE sleep_wake_proportions_1112---------
saveRDS(results_df, ".../RESULTS/SRIs/sleep_wake_proportions_1112.RDS")

# READ sleep_wake_proportions_1112---------
results_df <- readRDS(".../RESULTS/SRIs/sleep_wake_proportions_1112.RDS")
# 

hist(results_df$sleep_proportion, main = "Histogram of Sleep Proportions", xlab = "Sleep Proportion")
hist(results_df$wake_proportion, main = "Histogram of Wake Proportions", xlab = "Wake Proportion")

table(results_df$sleep_proportion > 0.8) # 0.8*24h = 19.2h; 4
table(results_df$sleep_proportion > 0.7) # 0.7*24h = 16.8h; 7
table(results_df$sleep_proportion > 0.6) # 0.6*24h = 14.4h; 30

# 2011/12----

# Example usage

# random SEQN:
unique_seqns <- unique(df_PAX1112_filtered$SEQN)

seqn_random_sel <- sample(unique_seqns, 1)
seqn_random_sel

tic()
calculate_sri(df_PAX1112_filtered, seqn = seqn_random_sel, 
              visualize_sleep_wake = TRUE, 
              simple_rule_imputation = FALSE, 
              fancy_imputation = "kNN_VIM",
              theoretical_imputation_limits = TRUE) 
toc() # up to 8 sec
# This is a very slow process, especially for large data sets.
cat("Duration of hole data set in hours: ",length(unique_seqns)*8/3600)

calculate_sri(df_PAX1112_filtered, seqn = seqn_random_sel, simple_rule_imputation = 1) # max
calculate_sri(df_PAX1112_filtered, seqn = seqn_random_sel, simple_rule_imputation = 2) # min
# It seems that the min is much further away than the max. 
# Values SRI>100 occur for theoretical limits... improve

calculate_sri(df_PAX1112_filtered, seqn = 63544, 
              visualize_sleep_wake = TRUE, raster_plot = FALSE) 
calculate_sri(df_PAX1112_filtered, seqn = 63544, 
              visualize_sleep_wake = TRUE, raster_plot = FALSE, simple_rule_imputation = 1) # max
calculate_sri(df_PAX1112_filtered, seqn = 63544, 
              visualize_sleep_wake = TRUE, raster_plot = FALSE, simple_rule_imputation = 2) # min

calculate_sri(df_PAX1112_filtered, seqn = 63544, visualize_sleep_wake = TRUE, simple_rule_imputation = 1) # 
calculate_sri(df_PAX1112_filtered, seqn = 63276, visualize_sleep_wake = TRUE) # not in data anymore
calculate_sri(df_PAX1112_filtered, seqn = 63630, visualize_sleep_wake = TRUE) # works 80
calculate_sri(df_PAX1112_filtered, seqn = 63630, visualize_sleep_wake = FALSE, simple_rule_imputation = FALSE) # works 80
calculate_sri(df_PAX1112_filtered, seqn = 63630, visualize_sleep_wake = FALSE, simple_rule_imputation = 1) # works 80
calculate_sri(df_PAX1112_filtered, seqn = 63630, visualize_sleep_wake = FALSE, simple_rule_imputation = 2)
calculate_sri(df_PAX1112_filtered, seqn = 71913, visualize_sleep_wake = TRUE) # works, 26
calculate_sri(df_PAX1112_filtered, seqn = 71908, visualize_sleep_wake = TRUE) # works
calculate_sri(df_PAX1112_filtered, seqn = 69168, visualize_sleep_wake = TRUE) # works, -10; looks very random indeed
calculate_sri(df_PAX1112_filtered, seqn = 63771, visualize_sleep_wake = TRUE, raster_plot = TRUE) # works, ~0; rather random
calculate_sri(df_PAX1112_filtered, seqn = 70354, visualize_sleep_wake = TRUE) # SRI = -100 -> most data is missing!
calculate_sri(df_PAX1112_filtered, seqn = 62197, visualize_sleep_wake = TRUE)


# Visualize individual binary_sleep (sleep/wake/NA)-------
seqn_plot <- 68170 # not in data set anymore
seqn_plot <- 69168 # small, sleep wake pattern looks rather random
seqn_plot <- 65046
seqn_plot <- 69296
seqn_plot <- 62479
seqn_plot <- 63544
seqn_plot <- sri_results[SRI < 0,]$SEQN[8] # negative SRIs
seqn_plot <- sample(sri_results[SRI > 70,]$SEQN, 1) # larger SRIs
seqn_plot <- sample(sri_results[SRI < 0,]$SEQN, 1)

calculate_sri(df_PAX1112_filtered, seqn = seqn_plot)

# simple sleep wake plot----
df_PAX1112_filtered %>% dplyr::filter(SEQN == seqn_plot) %>%
  mutate(row_num = n()) %>%
  ggplot(aes(x = 1:row_num[1], y = binary_sleep)) + 
  geom_point(size = 0.1)

# annotate missings (non-wear and unknown)
df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>%
  mutate(row_num = row_number()) %>%
  #filter(row_num < 2500)  %>% # only show certain time steps
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  #geom_vline(aes(xintercept = row_num), data = function(x) x[is.na(x$binary_sleep), ], 
  #           color = "red", linetype = "dashed", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep")

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank())    # Remove minor grid lines



# Calculate SRI for all SEQN values (11/12)--------
unique_seqns <- unique(df_PAX1112_filtered$SEQN)
#n_sub <- 100
n_sub <- length(unique_seqns)
num_cores <- detectCores() - 1

# Function to calculate SRI for all scenarios
calculate_all_sri <- function(seqn) {
  sri_no_imputation <- calculate_sri(df_PAX1112_filtered, seqn, simple_rule_imputation = FALSE)
  sri_maximize <- calculate_sri(df_PAX1112_filtered, seqn, simple_rule_imputation = 1)
  sri_minimize <- calculate_sri(df_PAX1112_filtered, seqn, simple_rule_imputation = 2)
  
  return(c(SRI_no_imputation = sri_no_imputation, 
           SRI_maximize = sri_maximize, 
           SRI_minimize = sri_minimize))
}
tic()
sri_values <- pblapply(unique_seqns[1:n_sub], calculate_all_sri, cl = num_cores)
toc()
sri_values_df <- do.call(rbind, sri_values)
sri_results <- data.frame(SEQN = unique_seqns[1:n_sub], sri_values_df)

head(sri_results)

quantiles_no_imputation <- quantile(na.omit(sri_results$SRI_no_imputation), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))
quantiles_maximize <- quantile(na.omit(sri_results$SRI_maximize), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))
quantiles_minimize <- quantile(na.omit(sri_results$SRI_minimize), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))

quantiles_no_imputation
quantiles_maximize
quantiles_minimize

# SAVE SRIs 2011_12:----
path <- ".../RESULTS/SRIs/27.8.24_calculate_sri_2011_12.RDS"
saveRDS(sri_results, path)

# READ SRIs 1112:---------
path <- ".../RESULTS/SRIs/27.8.24_calculate_sri_2011_12.RDS"
sri_results <- readRDS(file.choose())


# Look at negativ SRIs closer----
sri_results %>% 
  dplyr::filter(SRI_no_imputation < 0)
# File 2.2. shows that under a realistic sleep probability under random sleep/wake patterns
# -60 SRI can hardly happen.
# The other cases could happen by accident in a random sleep pattern.

# Check SRI <0 individually:---------
# All are very irregular!
seqn_plot <- 71478 # does not look that irregular
seqn_plot <- 67466
seqn_plot <- 69319
seqn_plot <- 69531
seqn_plot <- 70479

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>%
  #slice(1:1440) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  geom_vline(xintercept = 1440*c(1,2,3,4), color="red") + 
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank())    # Remove minor grid lines

# Check high SRIs----
sri_results %>% arrange(desc(SRI_no_imputation)) %>% head(10)
seqn_plot <- 71189 # almost only sleep!
seqn_plot <- 71596 # much non-wear and almost only sleep!
seqn_plot <- 62480 # very regular!
seqn_plot <- 63922 # almost only wake!
seqn_plot <- 68370 # very regular
seqn_plot <- 70784 # much non-wear, almost only wake, seems like it was taken off during sleep
seqn_plot <- 64924 # very regular
seqn_plot <- 70097 # regular, but a lot non wear time:

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>% 
  dplyr::summarize(non_wear_time = sum(PAXPREDM == "non-wear")) # more than 120 min in total at least

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>%
  #slice(1:1440) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  geom_vline(xintercept = 1440*c(1,2,3,4), color="red") + 
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))    # Remove minor grid lines

# Check low SRIs------
sri_results %>% arrange(SRI_no_imputation) %>% head(10)
(first_low_SRI_SEQN <- sri_results %>% arrange(SRI_no_imputation) %>% head(10) %>% pull(SEQN))

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == first_low_SRI_SEQN[4]) %>% 
  dplyr::summarize(non_wear_time = sum(PAXPREDM == "non-wear")) 

df_PAX1112_filtered %>% 
  dplyr::filter(SEQN == first_low_SRI_SEQN[4]) %>%
  #slice(1:1440) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  geom_vline(xintercept = 1440*c(1,2,3,4), color="red") + 
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))    # Remove minor grid lines



# Note that:
# -No survey weights for NHANES where used
# -The comparison group in the UKB had much higher age
# -The ukb only calculated SRIs for 5 valid days or more


# 2013/14---------
unique_seqns <- unique(df_PAX1314_filtered$SEQN)
seqn_random_sel <- sample(unique_seqns, 1)
seqn_random_sel
calculate_sri(df_PAX1314_filtered, seqn = seqn_random_sel, 
              visualize_sleep_wake = TRUE, simple_rule_imputation = FALSE, theoretical_imputation_limits = TRUE) 
calculate_sri(df_PAX1314_filtered, seqn = seqn_random_sel, simple_rule_imputation = 1) # max
calculate_sri(df_PAX1314_filtered, seqn = seqn_random_sel, simple_rule_imputation = 2) # min

seqn_plot <- sample(unique_seqns, 1)
seqn_plot
calculate_sri(df_PAX1314_filtered, seqn = seqn_plot, 
              visualize_sleep_wake = FALSE, simple_rule_imputation = FALSE, theoretical_imputation_limits = TRUE) 
maximum_number_days_to_display <- df_PAX1314_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>% 
  dplyr::summarize(max_days_to_display = floor(max(row_number())/1440)) %>%
  as.numeric()
df_PAX1314_filtered %>% 
  dplyr::filter(SEQN == seqn_plot) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Add a horizontal line between 0 and 1
  geom_vline(xintercept = 1440*1:maximum_number_days_to_display, color="red") + 
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", seqn_plot),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank())    # Remove minor grid lines

# Calculate SRI for all SEQN values (13/14)--------
unique_seqns <- unique(df_PAX1314_filtered$SEQN)
#n_sub <- 100
n_sub <- length(unique_seqns)
num_cores <- detectCores() - 1

# Function to calculate SRI for all scenarios
calculate_all_sri <- function(seqn) {
  sri_no_imputation <- calculate_sri(df_PAX1314_filtered, seqn, simple_rule_imputation = FALSE)
  sri_maximize <- calculate_sri(df_PAX1314_filtered, seqn, simple_rule_imputation = 1)
  sri_minimize <- calculate_sri(df_PAX1314_filtered, seqn, simple_rule_imputation = 2)
  
  return(c(SRI_no_imputation = sri_no_imputation, 
           SRI_maximize = sri_maximize, 
           SRI_minimize = sri_minimize))
}

tic()
sri_values <- pblapply(unique_seqns[1:n_sub], calculate_all_sri, cl = num_cores)
toc() # 8395.887 sec elapsed

sri_values_df <- do.call(rbind, sri_values)
sri_results <- data.frame(SEQN = unique_seqns[1:n_sub], sri_values_df)

head(sri_results)

quantiles_no_imputation <- quantile(na.omit(sri_results$SRI_no_imputation), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))
quantiles_maximize <- quantile(na.omit(sri_results$SRI_maximize), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))
quantiles_minimize <- quantile(na.omit(sri_results$SRI_minimize), probs=c(0,0.05,0.1,0.2,0.5,0.8,1))

quantiles_no_imputation
quantiles_maximize
quantiles_minimize

# SAVE SRIs 2013_14:----
path <- ".../RESULTS/SRIs/28.8.24_calculate_sri_2013_14.RDS"
saveRDS(sri_results, path)

# READ SRIs 1314:---------
path <- ".../RESULTS/SRIs/28.8.24_calculate_sri_2013_14.RDS"
sri_results <- readRDS(file.choose())

# # For Supplement:------
# adapt values accordingly....

# Negative SRIs 13/14----
sri_results %>% 
  dplyr::filter(SRI_no_imputation < 0) %>% arrange(SRI_no_imputation) # 11

conflicts_prefer(dplyr::between)
sri_results %>% 
  dplyr::filter(between(SRI_no_imputation, 90, 100)) %>% 
  dplyr::arrange(SRI_no_imputation) %>% head(10)
(SRI_SEQN <- sri_results %>% 
    dplyr::filter(between(SRI_no_imputation, 90, 100)) %>% 
    dplyr::arrange(SRI_no_imputation) %>% pull(SEQN))
 

to_plot <- 10
df_PAX1314_filtered %>% 
  dplyr::filter(SEQN == SRI_SEQN[to_plot]) %>% 
  dplyr::summarize(non_wear_time = sum(PAXPREDM == "non-wear")) 

# Get the specific SRI value for the selected SEQN
sri_value <- sri_results %>% 
  dplyr::filter(SEQN == SRI_SEQN[to_plot]) %>% pull(SRI_no_imputation)

# Plot with SRI value in the title
df_PAX1314_filtered %>% 
  dplyr::filter(SEQN == SRI_SEQN[to_plot]) %>%
  mutate(row_num = row_number()) %>%
  ggplot(aes(x = row_num, y = binary_sleep)) + 
  geom_point(size=0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "non-wear", ], 
             color = "green", linetype = "solid", size = 0.1) +
  geom_vline(aes(xintercept = row_num), data = function(x) x[x$PAXPREDM == "unknown", ], 
             color = "turquoise", linetype = "solid", size = 0.1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dotted", size = 0.5) +  # Horizontal line between 0 and 1
  geom_vline(xintercept = 1440*c(1,2,3,4,5,6), color="red") + 
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +  # Limit y-axis to show only 0 and 1
  labs(title = paste("Binary Sleep with NA Indicators for SEQN", SRI_SEQN[to_plot], 
                     "(SRI:", round(sri_value, 2), ")"),
       x = "Time Step",
       y = "Binary Sleep") +
  theme_minimal() +  # Use a minimal theme
  theme(panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Center the title



# Visualize SRI-distribution--------

#SRI_df <- sri_results %>% filter(!is.na(sri_results$SRI))
SRI_df <- sri_results %>% 
  dplyr::filter(!is.na(sri_results$SRI_no_imputation))
quantiles <- quantile(sri_results$SRI_no_imputation, probs=c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1), na.rm = TRUE)
p2 <- ggplot(SRI_df, aes(x = SRI_no_imputation)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(ifelse(
    ..x.. < quantiles[2], "red",
    ifelse(..x.. < quantiles[3], "orange",
           ifelse(..x.. < quantiles[4], "yellow",
                  ifelse(..x.. < quantiles[5], "lightblue",
                         ifelse(..x.. < quantiles[6], "blue", "darkblue"))))))), 
    bins = 30, color = NA, alpha = 0.7) +
  scale_fill_identity() +
  geom_density(color = "blue", linewidth = 1) +
  geom_boxplot(aes(y = -0.005, x = SRI_no_imputation), width = 0.01, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.005), position = position_jitter(width = 0.002, height = 0.005), size = 1, alpha = 0.05) +
  ggtitle("SRI distribution") +
  #scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p2

sri_results
sri_results <- as.data.table(sri_results)
hist(sri_results$SRI_no_imputation)
sum(is.na(sri_results$SRI_no_imputation))
#sri_results[SRI == -100,]
hist(sri_results[SRI_no_imputation < 0,]$SRI_no_imputation)




# _____Visualize SRIs with extreme values____-------
path <- ".../RESULTS/SRIs/27.8.24_calculate_sri_2011_12.RDS"
SRI_1112 <- readRDS(path)
path <- ".../RESULTS/SRIs/28.8.24_calculate_sri_2013_14.RDS"
SRI_1314 <- readRDS(path)

str(SRI_1112)
str(SRI_1314)

#_____2011/12----
SRI_1112 %>% arrange(SRI_no_imputation)  %>%
  ggplot() +
  geom_line(aes(x = 1:nrow(SRI_1112), y = SRI_no_imputation, colour = "black"), size = 1) +
  geom_point(aes(x = 1:nrow(SRI_1112), y = SRI_maximize, colour = "blue"), size = 0.1) +
  geom_point(aes(x = 1:nrow(SRI_1112), y = SRI_minimize, colour = "green"), size = 0.1) +
  labs(x = "Index", y = "SRI Values") +
  ggtitle("(ordered) SRI-values NHANES 11/12 and imputation bounds") + 
  scale_color_identity(name = "Legend", labels = c("SRI_no_imputation", "SRI_maximize", "SRI_minimize"), guide = "legend") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

SRI_df <- SRI_1112 %>% filter(!is.na(SRI_1112$SRI_no_imputation))
quantiles <- quantile(SRI_1112$SRI_no_imputation, probs=c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1), na.rm = TRUE)
p2 <- ggplot(SRI_df, aes(x = SRI_no_imputation)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(ifelse(
    ..x.. < quantiles[2], "red",
    ifelse(..x.. < quantiles[3], "orange",
           ifelse(..x.. < quantiles[4], "yellow",
                  ifelse(..x.. < quantiles[5], "lightblue",
                         ifelse(..x.. < quantiles[6], "blue", "darkblue"))))))), 
    bins = 30, color = NA, alpha = 0.7) +
  scale_fill_identity() +
  geom_density(color = "blue", linewidth = 1) +
  geom_boxplot(aes(y = -0.005, x = SRI_no_imputation), width = 0.01, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.005), position = position_jitter(width = 0.002, height = 0.005), size = 1, alpha = 0.05) +
  ggtitle("SRI distribution NHANES 11/12") +
  #scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p2

# _____201314----
SRI_1314 %>% arrange(SRI_no_imputation)  %>%
  ggplot() +
  geom_line(aes(x = 1:nrow(SRI_1314), y = SRI_no_imputation, colour = "black"), size = 1) +
  geom_point(aes(x = 1:nrow(SRI_1314), y = SRI_maximize, colour = "blue"), size = 0.1) +
  geom_point(aes(x = 1:nrow(SRI_1314), y = SRI_minimize, colour = "green"), size = 0.1) +
  labs(x = "Index", y = "SRI Values") +
  ggtitle("(ordered) SRI-values NHANES 13/14 and imputation bounds") + 
  scale_color_identity(name = "Legend", labels = c("SRI_no_imputation", "SRI_maximize", "SRI_minimize"), guide = "legend") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

SRI_df <- SRI_1314 %>% filter(!is.na(SRI_1314$SRI_no_imputation))
quantiles <- quantile(SRI_1314$SRI_no_imputation, probs=c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1), na.rm = TRUE)
p2 <- ggplot(SRI_df, aes(x = SRI_no_imputation)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(ifelse(
    ..x.. < quantiles[2], "red",
    ifelse(..x.. < quantiles[3], "orange",
           ifelse(..x.. < quantiles[4], "yellow",
                  ifelse(..x.. < quantiles[5], "lightblue",
                         ifelse(..x.. < quantiles[6], "blue", "darkblue"))))))), 
    bins = 30, color = NA, alpha = 0.7) +
  scale_fill_identity() +
  geom_density(color = "blue", linewidth = 1) +
  geom_boxplot(aes(y = -0.005, x = SRI_no_imputation), width = 0.01, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.005), position = position_jitter(width = 0.002, height = 0.005), size = 1, alpha = 0.05) +
  ggtitle("SRI distribution NHANES 13/14") +
  #scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p2



# add UNIX TIMESTAMP: SRI NHANES 2011/12-------
binarydir <- "/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/sleep_diary_data_folder_1112"

df1112_header$SEQN <- as.numeric(df1112_header$SEQN)
df1112_header <- as.data.table(df1112_header) # otherwise error
seconds_from_start <- start_times <- rep(NA, length(df1112_header$SEQN))
l <- length(seconds_from_start) # 7821
SEQN_with_PAM_data <- unique(df_PAX1112_filtered$SEQN[df1112_header$PAXSTS == 1]) # 6917 people have a PAM file
length(SEQN_with_PAM_data) # 3814
# setdiff(SEQN_with_PAM_data,df1112_header$SEQN) # 0

tic()
j <- 1
for(seqn in SEQN_with_PAM_data){
  start_times[j] <- as.POSIXct(paste0("1970-01-01 ", 
                                  df1112_header[SEQN == seqn,]$PAXFTIME), tz = "UTC")
  j <- j + 1
}
toc() # 3.6s

# check:
start_times
length(start_times) # 7821
sum(is.na(start_times)) # 904

df1112_header <- df1112_header %>%
  mutate(start_time = start_times)

seconds_since_1970_01_01 <- as.numeric(difftime(start_times, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "secs"))
#length(seconds_since_1970_01_01) # 7821
#length(unique(df_PAX1112$SEQN)) # 6917

df_PAX1112_filtered <- df_PAX1112_filtered %>%
  mutate(
    seconds_since_start = PAXSSNMP / 80, # 80 Hertz; PAXSSNMP=Starting sample number for minute
    binary_sleep = case_when(
      PAXPREDM == "sleep" ~ 1,
      PAXPREDM == "wake" ~ 0,
      TRUE ~ NA_real_  # "unknown" and "non-wear" as NA
    ))

dim(df_PAX1112_filtered)

df_PAX1112_filtered <- df_PAX1112_filtered %>%
  group_by(SEQN) %>%
  mutate(
    UNIX_TIMESTAMP = seconds_since_1970_01_01[cur_group_id()] + (PAXSSNMP / 80) # Adding the seconds from start for each group
  ) %>%
  ungroup()
  
# check: 
# df_PAX1112_filtered %>% dplyr::select(SEQN, PAXPREDM, PAXMTSM, seconds_since_start, binary_sleep, UNIX_TIMESTAMP)




# add UNIX TIMESTAMP: SRI NHANES 2013/14-------
binarydir <- "/Volumes/LaCie/NHANES_1112_1314/NHANES_2011_14/sleep_diary_data_folder1314"

df1314_header$SEQN <- as.numeric(df1314_header$SEQN)
df1314_header <- as.data.table(df1314_header) # otherwise error
seconds_from_start <- start_times <- rep(NA, length(df1314_header$SEQN))
l <- length(seconds_from_start) # 8913
SEQN_with_PAM_data <- unique(df_PAX1314$SEQN[df1314_header$PAXSTS == 1]) # 7776 people have a PAM file
length(SEQN_with_PAM_data) # 7776
# setdiff(SEQN_with_PAM_data,df1314_header$SEQN) # 0

tic()
j <- 1
for(seqn in SEQN_with_PAM_data){
  start_times[j] <- as.POSIXct(paste0("1970-01-01 ", 
                                      df1314_header[SEQN == seqn,]$PAXFTIME), tz = "UTC")
  j <- j + 1
}
toc() # 3.6s

# check:
start_times
length(start_times) # 8913
sum(is.na(start_times)) # 1137 (= 8913 - 1137)

df1314_header <- df1314_header %>%
  mutate(start_time = start_times)

seconds_since_1970_01_01 <- as.numeric(difftime(start_times, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "secs"))
#length(seconds_since_1970_01_01) # 7821
#length(unique(df_PAX1314$SEQN)) # 6917

df_PAX1314_filtered <- df_PAX1314_filtered %>%
  mutate(
    seconds_since_start = PAXSSNMP / 80, # 80 Hertz; PAXSSNMP=Starting sample number for minute
    binary_sleep = case_when(
      PAXPREDM == "sleep" ~ 1,
      PAXPREDM == "wake" ~ 0,
      TRUE ~ NA_real_  # "unknown" and "non-wear" as NA
    ))

df_PAX1314_filtered <- df_PAX1314_filtered %>%
  group_by(SEQN) %>%
  mutate(
    UNIX_TIMESTAMP = seconds_since_1970_01_01[cur_group_id()] + (PAXSSNMP / 80) # Adding the seconds from start for each group
  ) %>%
  ungroup()

# check: 
# df_PAX1314 %>% dplyr::select(SEQN, PAXPREDM, PAXMTSM, seconds_since_start, binary_sleep, UNIX_TIMESTAMP)

# (CO)VARIATES (and possible associations)----

# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm

# _Age----
# 2018 Validation - SRI decreasing with age, -0.06
# Younger people are more irregular sleepers
df1112_demo$RIDAGEYR # Age in years at screening
hist(df1112_demo$RIDAGEYR)
sum(is.na(df1112_demo$RIDAGEYR)) # 0

df1314_demo$RIDAGEYR # Age in years at screening
hist(df1314_demo$RIDAGEYR)
sum(is.na(df1314_demo$RIDAGEYR)) # 0

# _Sex----
# 2017 nature - "no" sex difference in SRIs, college students...61 only!
# 2018 Validation paper - 2 points difference, higher in women (72 vs 70) - clinically relevant?
# Sex differences are inconclusive: https://youtu.be/b6fZ-5GO2RA?si=wkz68JAHSwibDVGw&t=308
df1112_demo$RIAGENDR # Gender of the participant.
table(df1112_demo$RIAGENDR) # 1=Male, 2=Female
sum(is.na(df1112_demo$RIAGENDR)) # 0

df1314_demo$RIAGENDR # Gender of the participant.
table(df1314_demo$RIAGENDR)
sum(is.na(df1314_demo$RIAGENDR)) # 0

# _Ethnicity----
# # Race/Hispanic origin w/ NH Asian
# 2018 Validation - black lower than all other groups, also lower in Hispanics
# Race differences are inconclusive: https://youtu.be/b6fZ-5GO2RA?si=wkz68JAHSwibDVGw&t=308
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#RIDRETH3
df1112_demo$RIDRETH3
df1314_demo$RIDRETH3

# _BMI [Outcome]----

# associated with marital status:
# https://www.mdpi.com/2079-9721/12/7/146#:~:text=The%20studies%20presented%20thus%20far,risk%20factors%20for%20such%20diseases.

df1112_body_measures$BMXBMI # Body Mass Index (kg/m**2)
df1314_body_measures$BMXBMI

# _Pregnancy status at exam----
df1112_demo$RIDEXPRG # (1=Yes, 2=No, 3=Cannot ascertain)
table(df1112_demo$RIDEXPRG)
df1314_demo$RIDEXPRG

table(df1112_demo$RIDEXPRG) 
# https://journals.lww.com/greenjournal/abstract/2022/08000/common_sleep_disorders_in_pregnancy.27.aspx
# probably associated with sleep regularity...SRI

# _Ambient light (Mean lux value for the minute)----
# 2018 validation - irregular sleepers less exposed to light
#summary(df_PAX1112_filtered$PAXLXMM)

# _Light Regularity index?-
# Bidirectional causal relationship with SRI: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10424172/

# __Visualize mean lux (per participant)---
# n_th_seqn <- 103 #th SEQN number in data frame
# maximum_number_days_to_display <- df_PAX1112_filtered %>% 
#   dplyr::filter(SEQN == df_PAX1112_filtered$SEQN[n_th_seqn]) %>% 
#   dplyr::summarize(max_days_to_display = floor(max(row_number())/1440)) %>%
#   as.numeric()
# df_PAX1112_filtered %>% 
#   dplyr::filter(SEQN == df_PAX1112_filtered$SEQN[n_th_seqn]) %>%
#   #slice(2000) %>%
#   dplyr::mutate(number_of_rows = n()) %>%
#   ggplot(aes(x = 1:number_of_rows[1], y = PAXLXMM)) + 
#     geom_point(size = 0.1, color = "blue") + 
#     geom_vline(xintercept = 1440*1:maximum_number_days_to_display, color="red") + 
#     xlab("time-steps [min]") + ylab("Mean lux value for the minute")
# 
# df_PAX1314_filtered$PAXLXMM

# Light exposure seems to be associated with SRI: https://youtu.be/b6fZ-5GO2RA?si=OX6JL7rpdUjnkcB2&t=340

# _Chronotype--
# later Chronotype, lower SRI: https://youtu.be/b6fZ-5GO2RA?si=evbPFPq2Oh1-pXZC&t=327

# _Educational level-------
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#DMDEDUC2
# (for sure correlated with age)
df1112_demo$DMDEDUC2 # Education level - Adults 20+
table(df1112_demo$DMDEDUC2)

df1314_demo$DMDEDUC2
table(df1314_demo$DMDEDUC2)

# _Annual household income--
df1112_demo$INDHHIN2 # https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#INDHHIN2
hist(df1112_demo$INDHHIN2) 
table(df1112_demo$INDHHIN2) # 1-10: 0-75k USD; 12,13: 20k and over, under 20k, 14: 75k-90k; 15: 100k and over
sum(df1112_demo$INDHHIN2 == 77, na.rm = TRUE) # 252 refused
sum(df1112_demo$INDHHIN2 == 99, na.rm = TRUE) # don't know
sum(is.na(sum(df1112_demo$INDHHIN2))) # 1

df1314_demo$INDHHIN2

# _More covariates about income----
#https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/INQ_G.htm#INDFMMPC
# Income from salary, self employment, interest payments, 
# poverty index... 3 levels

# _Occupation--
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OCQ_G.htm
# Type of work done last week
# Hours worked last week at all jobs
# Usually work 35 or more hours per week
# Industry group code: current job
# Occupation group code: current job
# Description of job/work situation
# Fumes?

# _Marital status--
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#DMDMARTL
df1112_demo$DMDMARTL
table(df1112_demo$DMDMARTL)

# associated with physical activity: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8588853/

# _Physical activity-
# How can I use this?
# Directly influences (causally) BMI
# Validation 2018 - irregular sleepers less active than regular sleepers (suppl fig 3)

# _Employment-
# 2018 paper - SRI higher in workforce

# _Diurnal preference----
# 2018 Validation, morningness more irregular

# _Other sleep parameters?
# sleep midpoint
# total sleep time
# sleep onset offset

# _Educational level--
df1112_demo$DMDEDUC2 # Education level - Adults 20+
# Objektive measurement paper - higher education, higher SRI

# _Cardiovascular health----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CDQ_G.htm

# _Further covariates----
# Chronic Health Conditions: Conditions such as diabetes, hypertension, and cardiovascular disease can influence both sleep and BMI.
# Medication Use: Certain medications can affect sleep patterns and body weight, so this should be considered if the data is available.

# Other sleep variables----
# Total sleep time (TST), sleep onset, sleep midpoint, 

# Read SRIs--------
#path_SRI <- ".../4_RESULTS/SRIs"
SRI_1112 <- readRDS(file.choose()) # "/27.8.24_calculate_sri_2011_12.RDS"
SRI_1314 <- readRDS(file.choose()) # "/28.8.24_calculate_sri_2013_14.RDS"

results1112 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1112_filtered_not_imputed.RDS")
results1314 <- readRDS("/Users/juergen/Large_R_Files/NHANES_2011_14/results_1314_filtered_not_imputed.RDS")

df_PAX1112_filtered <- results1112$filtered_data
df_PAX1314_filtered <- results1314$filtered_data

# 2011/12
dim(df1112_header)
length(unique(df1112_header$SEQN)) # 7821
dim(df1112_demo)
length(unique(df1112_demo$SEQN)) # 9756
dim(df_PAX1112)
length(unique(df_PAX1112$SEQN)) # 6917 (why not equal as in 13/14?)
df1112_blood_pressure


# 2013/14
dim(df1314_header)
length(unique(df1314_header$SEQN)) # 8913

dim(df1314_demo)
length(unique(df1314_demo$SEQN)) # 10175

dim(df_PAX1314)
length(unique(df_PAX1314$SEQN)) # 7776

length(unique(df1112_demo$SEQN)) + length(unique(df1314_demo$SEQN)) # 19931

# COMBINE 2 waves (bind_rows())--------

#rbind(df1112_demo, df1314_demo)
setdiff(colnames(df1112_demo), colnames(df1314_demo)) # "RIDEXAGY"
sum(df1112_demo$RIDEXAGY>19, na.rm = TRUE)
table(df1112_demo$RIDEXAGY) # 10 20-year-olds included.
# "RIDEXAGY": Age in years of the participant at the time of examination. 
# Reported for persons aged 2-19 years at the time of screening.
df1112_demo$RIDAGEYR # explanation above does not make sense since ages > 19 are included.
df1314_demo$RIDAGEYR

# _Combine demographics---------
df_demo <- bind_rows(df1112_demo %>% dplyr::select(-RIDEXAGY), 
                     df1314_demo)
dim(df_demo) # 19931 (aligns with Figure 1 in https://doi.org/10.1038/s41598-022-11848-8)

# _Combine header files----
setdiff(colnames(df1112_header), colnames(df1314_header)) # 0
df_header <- bind_rows(df1112_header, df1314_header)

# _Combine PAX_SEQN-numbers after filtering----
SEQN_in_filtered_PAX_files11_to_14 <- c(unique(df_PAX1112_filtered$SEQN), 
                                        unique(df_PAX1314_filtered$SEQN))

dim(df_header) # 16734
table(df_header$PAXSTS) # Indicates which participants have PAM data available in the summary data files
# 1=Yes 2=No 
#14693  2041

# _Combine body measures-----
setdiff(colnames(df1314_body_measures), colnames(df1112_body_measures))
colnames(df1314_body_measures)
colnames(df1112_body_measures)
df_body_measures <- bind_rows(df1112_body_measures, df1314_body_measures)
dim(df_body_measures) # 19151

# _Combine Diet bahaviour, nutrition----
diet_cols_not_in_both <- setdiff(colnames(df1112_diet_behaviour), colnames(df1314_diet_behaviour))
existing_diet_cols_1112 <- intersect(diet_cols_not_in_both, colnames(df1112_diet_behaviour))
existing_diet_cols_1314 <- intersect(diet_cols_not_in_both, colnames(df1314_diet_behaviour))

df_diet_behaviour <- bind_rows(
  df1112_diet_behaviour %>% dplyr::select(-all_of(existing_diet_cols_1112)),
  df1314_diet_behaviour %>% dplyr::select(-all_of(existing_diet_cols_1314))
)
head(df_diet_behaviour)   
dim(df_diet_behaviour) # 19931

# _Combine Blood pressure----
bp_cols_not_in_both <- setdiff(colnames(df1112_blood_pressure), colnames(df1314_blood_pressure))
existing_bp_cols_1112 <- intersect(bp_cols_not_in_both, colnames(df1112_blood_pressure))
existing_bp_cols_1314 <- intersect(bp_cols_not_in_both, colnames(df1314_blood_pressure))
df_blood_pressure <- bind_rows(
  df1112_blood_pressure %>% dplyr::select(-all_of(existing_bp_cols_1112)),
  df1314_blood_pressure %>% dplyr::select(-all_of(existing_bp_cols_1314))
)
head(df_blood_pressure)   
dim(df_blood_pressure) # 19151 

# _Combine Alcohol consumption----
alc_cols_not_in_both <- setdiff(colnames(df1112_alcohol_use), colnames(df1314_alcohol_use))
existing_alc_cols_1112 <- intersect(alc_cols_not_in_both, colnames(df1112_alcohol_use))
existing_alc_cols_1314 <- intersect(alc_cols_not_in_both, colnames(df1314_alcohol_use))
df_alcohol_use <- bind_rows(
  df1112_alcohol_use %>% dplyr::select(-all_of(existing_alc_cols_1112)),
  df1314_alcohol_use %>% dplyr::select(-all_of(existing_alc_cols_1314))
)
head(df_alcohol_use)   
dim(df_alcohol_use) # 11539 

# _Combine Smoking----
smoke_cols_not_in_both <- setdiff(colnames(df1112_smoking), colnames(df1314_smoking))
existing_smoke_cols_1112 <- intersect(smoke_cols_not_in_both, colnames(df1112_smoking))
existing_smoke_cols_1314 <- intersect(smoke_cols_not_in_both, colnames(df1314_smoking))
df_smoking <- bind_rows(
  df1112_smoking %>% dplyr::select(-all_of(existing_smoke_cols_1112)),
  df1314_smoking %>% dplyr::select(-all_of(existing_smoke_cols_1314))
)
head(df_smoking)   
dim(df_smoking) # 13958

# _Combine Sleep disorders----
sleep_cols_not_in_both <- setdiff(colnames(df1112_sleep_disorders), colnames(df1314_sleep_disorders))
existing_sleep_cols_1112 <- intersect(sleep_cols_not_in_both, colnames(df1112_sleep_disorders))
existing_sleep_cols_1314 <- intersect(sleep_cols_not_in_both, colnames(df1314_sleep_disorders))
df_sleep_disorders <- bind_rows(
  df1112_sleep_disorders %>% dplyr::select(-all_of(existing_sleep_cols_1112)),
  df1314_sleep_disorders %>% dplyr::select(-all_of(existing_sleep_cols_1314))
)
head(df_sleep_disorders)   
dim(df_sleep_disorders) # 12639 

# _Combine Mental health, depression----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DPQ_G.htm
depression_cols_not_in_both <- setdiff(colnames(df1112_depression), colnames(df1314_depression))
existing_depression_cols_1112 <- intersect(depression_cols_not_in_both, colnames(df1112_depression))
existing_depression_cols_1314 <- intersect(depression_cols_not_in_both, colnames(df1314_depression))
df_depression <- bind_rows(
  df1112_depression %>% dplyr::select(-all_of(existing_depression_cols_1112)),
  df1314_depression %>% dplyr::select(-all_of(existing_depression_cols_1314))
)
head(df_depression)   
dim(df_depression) # 11539

# _Combine diabetes-------
# Identify columns present in one dataset but not the other
diabetes_cols_not_in_both <- setdiff(colnames(df1112_diabetes), colnames(df1314_diabetes))
existing_diabetes_cols_1112 <- intersect(diabetes_cols_not_in_both, colnames(df1112_diabetes))
existing_diabetes_cols_1314 <- intersect(diabetes_cols_not_in_both, colnames(df1314_diabetes))
df_diabetes <- bind_rows(
  df1112_diabetes %>% dplyr::select(-all_of(existing_diabetes_cols_1112)),
  df1314_diabetes %>% dplyr::select(-all_of(existing_diabetes_cols_1314))
)

# _Combine Early childhood----
early_childhood_cols_not_in_both <- setdiff(colnames(df1112_early_childhood), colnames(df1314_early_childhood))
existing_early_childhood_cols_1112 <- intersect(early_childhood_cols_not_in_both, colnames(df1112_early_childhood))
existing_early_childhood_cols_1314 <- intersect(early_childhood_cols_not_in_both, colnames(df1314_early_childhood))
df_early_childhood <- bind_rows(
  df1112_early_childhood %>% dplyr::select(-all_of(existing_early_childhood_cols_1112)),
  df1314_early_childhood %>% dplyr::select(-all_of(existing_early_childhood_cols_1314))
)

# _Combine Drug Use ----
drug_use_cols_not_in_both <- setdiff(colnames(df1112_drug_use), colnames(df1314_drug_use))
existing_drug_use_cols_1112 <- intersect(drug_use_cols_not_in_both, colnames(df1112_drug_use))
existing_drug_use_cols_1314 <- intersect(drug_use_cols_not_in_both, colnames(df1314_drug_use))
df_drug_use <- bind_rows(
  df1112_drug_use %>% dplyr::select(-all_of(existing_drug_use_cols_1112)),
  df1314_drug_use %>% dplyr::select(-all_of(existing_drug_use_cols_1314))
)

# _Combine occupation----
occupation_cols_not_in_both <- setdiff(colnames(df1112_occupation), colnames(df1314_occupation))
existing_occupation_cols_1112 <- intersect(occupation_cols_not_in_both, colnames(df1112_occupation))
existing_occupation_cols_1314 <- intersect(occupation_cols_not_in_both, colnames(df1314_occupation))
df_occupation <- bind_rows(
  df1112_occupation %>% dplyr::select(-all_of(existing_occupation_cols_1112)),
  df1314_occupation %>% dplyr::select(-all_of(existing_occupation_cols_1314))
)
#df_occupation


# _Combine cardiovascular health----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CDQ_G.htm
cardiovascular_cols_not_in_both <- setdiff(colnames(df1112_cardiovascular_health), colnames(df1314_cardiovascular_health))
existing_cardiovascular_cols_1112 <- intersect(cardiovascular_cols_not_in_both, colnames(df1112_cardiovascular_health))
existing_cardiovascular_cols_1314 <- intersect(cardiovascular_cols_not_in_both, colnames(df1314_cardiovascular_health))
df_cardiovascular_health <- bind_rows(
  df1112_cardiovascular_health %>% dplyr::select(-all_of(existing_cardiovascular_cols_1112)),
  df1314_cardiovascular_health %>% dplyr::select(-all_of(existing_cardiovascular_cols_1314))
)

# _Combine Vitamin D----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.htm
vitamin_d_cols_not_in_both <- setdiff(colnames(df1112_vitamin_d), colnames(df1314_vitamin_d))
existing_vitamin_d_cols_1112 <- intersect(vitamin_d_cols_not_in_both, colnames(df1112_vitamin_d))
existing_vitamin_d_cols_1314 <- intersect(vitamin_d_cols_not_in_both, colnames(df1314_vitamin_d))
df_vitamin_D <- bind_rows(
  df1112_vitamin_d %>% dplyr::select(-all_of(existing_vitamin_d_cols_1112)),
  df1314_vitamin_d %>% dplyr::select(-all_of(existing_vitamin_d_cols_1314))
)

# _Combine dietary interview TOT day 1---------
dietary_interview_cols_not_in_both <- setdiff(colnames(df1112_dietary_intake), colnames(df1314_dietary_intake))
existing_dietary_intake_cols_1112 <- intersect(dietary_interview_cols_not_in_both, colnames(df1112_dietary_intake))
existing_dietary_intake_cols_1314 <- intersect(dietary_interview_cols_not_in_both, colnames(df1314_dietary_intake))
df_dietary_intake <- bind_rows(
  df1112_dietary_intake %>% dplyr::select(-all_of(existing_dietary_intake_cols_1112)),
  df1314_dietary_intake %>% dplyr::select(-all_of(existing_dietary_intake_cols_1314))
)

# _Combine dietary interview day TOT day 2----------
dietary_interview2_cols_not_in_both <- setdiff(colnames(df1112_dietary_intake_2), colnames(df1314_dietary_intake_2))
existing_dietary_intake2_cols_1112 <- intersect(dietary_interview2_cols_not_in_both, colnames(df1112_dietary_intake_2))
existing_dietary_intake2_cols_1314 <- intersect(dietary_interview2_cols_not_in_both, colnames(df1314_dietary_intake_2))
df_dietary_intake2 <- bind_rows(
  df1112_dietary_intake_2 %>% dplyr::select(-all_of(existing_dietary_intake2_cols_1112)),
  df1314_dietary_intake_2 %>% dplyr::select(-all_of(existing_dietary_intake2_cols_1314))
)

# DETERMINE eating times -----
# CAUTION:---------
# When looking at the data, we can see that some participants report 
# their first meal at 1 am or so. this could possibly be seen as the last
# meal of the previous day. 
# Days 1 (in person) and 2 (telephone) are not on the same day
# but 3-10 days later.
# -> using the eating duration as predictor could therefore be problematic
# one could start with using 
# - last meal of the day respectively 
# - the difference of the last meal on Mo-Do and Fr-So as predictor.

# -> one could do a sensitivity analysis within the subset analysis of
# people who did not eat their first meal between 00:00 and, say, 4:00 am.

# Where the dietary interviews overlapping with actigraphy data?--------
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PAXMIN_H.htm
# Participants were asked to wear the PAM starting on the day of their exam 
# in the NHANES Mobile Examination Center (MEC) and to keep wearing the PAM all
# day and night for seven full days (midnight to midnight) and remove it on the
# morning of the 9th day.

# All NHANES participants are eligible for two 24-hour dietary recall
# interviews. The first dietary recall interview is collected in-person
# in the Mobile Examination Center (MEC) and the second interview is
# collected by telephone 3 to 10 days later.
# The examination protocol and data collection methods are fully documented 
# in the NHANES dietary interviewer procedures manuals 
# (In-person interview: https://wwwn.cdc.gov/nchs/data/nhanes/2013-2014/manuals/2013_mec-person_dietary_interview-manual.pdf;  
# phone follow-up interview: https://wwwn.cdc.gov/nchs/data/nhanes/2013-2014/manuals/phone_follow-up_dietary_interviewers_manual.pdf).


# _2011_12----------

# __eating times day 1:---------
eating_times_11_12_day_1 <- df1112_dietary_intake_individual_foods %>%
  dplyr::filter(!is.na(DR1_020)) %>% 
  dplyr::mutate(
    time_sec = as.numeric(DR1_020)  # direkt als Sekunden
  ) %>%
  dplyr::group_by(SEQN) %>%
  dplyr::summarise(
    first_meal_sec = min(time_sec, na.rm = TRUE),
    last_meal_sec  = max(time_sec, na.rm = TRUE),
    DR1DAY = dplyr::first(DR1DAY),  # optional: prüf vorher, ob es mehrere Werte gibt!
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    duration_sec = last_meal_sec - first_meal_sec,
    eating_window_hours = duration_sec / 3600, # within the current day (00:00 - 24:00)
    first_meal_time = lubridate::seconds_to_period(first_meal_sec),
    last_meal_time  = lubridate::seconds_to_period(last_meal_sec)
  ) %>%
  dplyr::select(SEQN, DR1DAY, first_meal_time, 
                last_meal_time, eating_window_hours) %>%
  dplyr::rename_with(~ paste0(.x, "_11_12_day_1"), -SEQN)
dim(eating_times_11_12_day_1) # 8519
# DR1DAY: 1=Sunday, 2=Monday, 3=Tuesday, 4=Wednesday, 5=Thursday, 6=Friday, 7=Saturday

summary(eating_times_11_12_day_1$eating_window_hours_11_12_day_1)

#View(eating_times_11_12_day_1)
#str(eating_times_11_12_day_1)

# CHECKS:
# how often eating over midnight, day1:
eating_times_11_12_day_1 %>%
  dplyr::mutate(
    last_sec = lubridate::period_to_seconds(last_meal_time_11_12_day_1),
    first_sec = lubridate::period_to_seconds(first_meal_time_11_12_day_1),
    over_midnight = last_sec < first_sec
  ) %>%
  dplyr::count(over_midnight) # never
dim(eating_times_11_12_day_1) # 8519 

eating_times_11_12_day_2 %>%
  dplyr::mutate(
    last_sec = lubridate::period_to_seconds(last_meal_time_11_12_day_2),
    first_sec = lubridate::period_to_seconds(first_meal_time_11_12_day_2),
    over_midnight = last_sec < first_sec
  ) %>%
  dplyr::count(over_midnight) # never
dim(eating_times_11_12_day_2) # 7605 
# sum dim:
dim(eating_times_11_12_day_1)[1] + dim(eating_times_11_12_day_2)[1] # 16124


# __eating times day 2:---------
eating_times_11_12_day_2 <- df1112_dietary_intake_individual_foods_2 %>%
  dplyr::filter(!is.na(DR2_020)) %>%
  dplyr::mutate(
    time_sec = as.numeric(DR2_020)  # direkt als Sekunden
  ) %>%
  dplyr::group_by(SEQN) %>%
  dplyr::summarise(
    first_meal_sec = min(time_sec, na.rm = TRUE),
    last_meal_sec  = max(time_sec, na.rm = TRUE),
    DR2DAY = dplyr::first(DR2DAY),  # Tag merken
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    duration_sec = last_meal_sec - first_meal_sec,
    eating_window_hours = duration_sec / 3600,
    first_meal_time = lubridate::seconds_to_period(first_meal_sec),
    last_meal_time  = lubridate::seconds_to_period(last_meal_sec)
  ) %>%
  dplyr::select(SEQN, DR2DAY, first_meal_time, last_meal_time, eating_window_hours) %>%
  dplyr::rename_with(~ paste0(.x, "_11_12_day_2"), -SEQN)
dim(eating_times_11_12_day_2) # 7605

hist(eating_times_11_12_day_1$eating_window_hours_11_12_day_1,
     breaks = 30,
     main = "Essenszeitfenster – Tag 1 (2011/12)",
     xlab = "Essenszeitfenster (Stunden)",
     col = "lightblue",
     border = "white")

hist(eating_times_11_12_day_2$eating_window_hours_11_12_day_2,
     breaks = 30,
     main = "Essenszeitfenster – Tag 2 (2011/12)",
     xlab = "Essenszeitfenster (Stunden)",
     col = "lightblue",
     border = "white")

# _2013_14----------

# __eating times day 1:---------
eating_times_13_14_day_1 <- df1314_dietary_intake_individual_foods %>%
  dplyr::filter(!is.na(DR1_020)) %>%
  dplyr::mutate(
    time_sec = as.numeric(DR1_020)
  ) %>%
  dplyr::group_by(SEQN) %>%
  dplyr::summarise(
    first_meal_sec = min(time_sec, na.rm = TRUE),
    last_meal_sec  = max(time_sec, na.rm = TRUE),
    DR1DAY = dplyr::first(DR1DAY),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    duration_sec = last_meal_sec - first_meal_sec,
    eating_window_hours = duration_sec / 3600,
    first_meal_time = lubridate::seconds_to_period(first_meal_sec),
    last_meal_time  = lubridate::seconds_to_period(last_meal_sec)
  ) %>%
  dplyr::select(SEQN, DR1DAY, first_meal_time, last_meal_time, eating_window_hours) %>%
  dplyr::rename_with(~ paste0(.x, "_13_14_day_1"), -SEQN)
dim(eating_times_13_14_day_1) # 8661

# __eating times day 2:---------
eating_times_13_14_day_2 <- df1314_dietary_intake_individual_foods_2 %>%
  dplyr::filter(!is.na(DR2_020)) %>%
  dplyr::mutate(
    time_sec = as.numeric(DR2_020)
  ) %>%
  dplyr::group_by(SEQN) %>%
  dplyr::summarise(
    first_meal_sec = min(time_sec, na.rm = TRUE),
    last_meal_sec  = max(time_sec, na.rm = TRUE),
    DR2DAY = dplyr::first(DR2DAY),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    duration_sec = last_meal_sec - first_meal_sec,
    eating_window_hours = duration_sec / 3600,
    first_meal_time = lubridate::seconds_to_period(first_meal_sec),
    last_meal_time  = lubridate::seconds_to_period(last_meal_sec)
  ) %>%
  dplyr::select(SEQN, DR2DAY, first_meal_time, last_meal_time, eating_window_hours) %>%
  dplyr::rename_with(~ paste0(.x, "_13_14_day_2"), -SEQN)
dim(eating_times_13_14_day_2) # 7573

hist(eating_times_13_14_day_1$eating_window_hours_13_14_day_1,
     breaks = 30,
     main = "Essenszeitfenster – Tag 1 (2013/14)",
     xlab = "Essenszeitfenster (Stunden)",
     col = "lightblue",
     border = "white")

hist(eating_times_13_14_day_2$eating_window_hours_13_14_day_2,
     breaks = 30,
     main = "Essenszeitfenster – Tag 2 (2013/14)",
     xlab = "Essenszeitfenster (Stunden)",
     col = "lightblue",
     border = "white")

# How many SEQN have one week day and one weekend day (friday, saturday, sunday also??)?---------
# DR1DAY: 1=Sunday, 2=Monday, 3=Tuesday, 4=Wednesday, 5=Thursday, 6=Friday, 7=Saturday

diet_days <- dplyr::bind_rows(
  eating_times_11_12_day_1 %>%
    dplyr::select(SEQN, DR1DAY_11_12_day_1, 
                  first_meal_time_11_12_day_1, 
                  last_meal_time_11_12_day_1,
                  eating_window_hours_11_12_day_1) %>%
    dplyr::left_join(eating_times_11_12_day_2 %>% dplyr::select(SEQN, DR2DAY_11_12_day_2,
                                                                first_meal_time_11_12_day_2,
                                                                last_meal_time_11_12_day_2,
                                                                eating_window_hours_11_12_day_2), by = "SEQN"),
  
  eating_times_13_14_day_1 %>%
    dplyr::select(SEQN, DR1DAY_13_14_day_1,
                  first_meal_time_13_14_day_1,
                  last_meal_time_13_14_day_1,
                  eating_window_hours_13_14_day_1) %>%
    dplyr::left_join(eating_times_13_14_day_2 %>% dplyr::select(SEQN, DR2DAY_13_14_day_2,
                                                                first_meal_time_13_14_day_2,
                                                                last_meal_time_13_14_day_2,
                                                                eating_window_hours_13_14_day_2), by = "SEQN")
) %>%
  dplyr::filter(!is.na(DR1DAY_11_12_day_1) | !is.na(DR1DAY_13_14_day_1)) %>%
  dplyr::mutate(
    DR1 = dplyr::coalesce(DR1DAY_11_12_day_1, DR1DAY_13_14_day_1),
    DR2 = dplyr::coalesce(DR2DAY_11_12_day_2, DR2DAY_13_14_day_2),
    weekday_1 = DR1 %in% 2:5,  # Mo–Do
    weekend_2 = DR2 %in% c(6,7,1),  # Fr–So
    combo_1 = weekday_1 & weekend_2,
    
    weekday_2 = DR2 %in% 2:5, # one could change this to Mo-Fr and Sa-Su
    weekend_1 = DR1 %in% c(6,7,1),
    combo_2 = weekday_2 & weekend_1
  )
dim(diet_days) # 17180
head(diet_days)

# _Calculate difference in hours between last meal on weekend and weekday----------
diet_days <- diet_days %>%
  dplyr::mutate(
    latest_meal_difference = dplyr::case_when(
      combo_1 ~ dplyr::coalesce(last_meal_time_11_12_day_2, last_meal_time_13_14_day_2) - # day 1 weekday (Mo-Do), day 2 weekend (Fr-So)
        dplyr::coalesce(last_meal_time_11_12_day_1, last_meal_time_13_14_day_1),
      combo_2 ~ dplyr::coalesce(last_meal_time_11_12_day_1, last_meal_time_13_14_day_1) - # day 1 weekend (Fr-So), day 2 weekday (Mo-Do)
        dplyr::coalesce(last_meal_time_11_12_day_2, last_meal_time_13_14_day_2),
      TRUE ~ NA
    ),
    latest_meal_diff_hours = as.numeric(latest_meal_difference, units = "hours")
  )

# _Calculate differences between eating windows-----------

diet_days <- dplyr::bind_rows(
  eating_times_11_12_day_1 %>%
    dplyr::select(SEQN, DR1DAY_11_12_day_1, 
                  first_meal_time_11_12_day_1, 
                  last_meal_time_11_12_day_1,
                  eating_window_hours_11_12_day_1) %>%
    dplyr::left_join(eating_times_11_12_day_2 %>% dplyr::select(SEQN, DR2DAY_11_12_day_2,
                                                                first_meal_time_11_12_day_2,
                                                                last_meal_time_11_12_day_2,
                                                                eating_window_hours_11_12_day_2), by = "SEQN"),
  
  eating_times_13_14_day_1 %>%
    dplyr::select(SEQN, DR1DAY_13_14_day_1,
                  first_meal_time_13_14_day_1,
                  last_meal_time_13_14_day_1,
                  eating_window_hours_13_14_day_1) %>%
    dplyr::left_join(eating_times_13_14_day_2 %>% dplyr::select(SEQN, DR2DAY_13_14_day_2,
                                                                first_meal_time_13_14_day_2,
                                                                last_meal_time_13_14_day_2,
                                                                eating_window_hours_13_14_day_2), by = "SEQN")
) %>%
  dplyr::filter(!is.na(DR1DAY_11_12_day_1) | !is.na(DR1DAY_13_14_day_1)) %>%
  dplyr::mutate(
    DR1 = dplyr::coalesce(DR1DAY_11_12_day_1, DR1DAY_13_14_day_1),
    DR2 = dplyr::coalesce(DR2DAY_11_12_day_2, DR2DAY_13_14_day_2),
    weekday_1 = DR1 %in% 2:5,  # Mo–Do
    weekend_2 = DR2 %in% c(6,7,1),  # Fr–So
    combo_1 = weekday_1 & weekend_2,
    
    weekday_2 = DR2 %in% 2:5, # one could change this to Mo-Fr and Sa-Su
    weekend_1 = DR1 %in% c(6,7,1),
    combo_2 = weekday_2 & weekend_1
  )
dim(diet_days) # 17180
head(diet_days)

# _add Wave variable----------
diet_days <- diet_days %>%
  dplyr::mutate(
    Wave = dplyr::case_when(
      SEQN %in% df1112_demo$SEQN ~ "Wave_11_12",
      SEQN %in% df1314_demo$SEQN ~ "Wave_13_14",
      TRUE ~ NA_character_
    )
  )

# _Calculate difference in hours between last meal on weekend and weekday----------
diet_days <- diet_days %>%
  dplyr::mutate(
    latest_meal_difference = dplyr::case_when(
      combo_1 ~ dplyr::coalesce(last_meal_time_11_12_day_2, last_meal_time_13_14_day_2) - # day 1 weekday (Mo-Do), day 2 weekend (Fr-So)
        dplyr::coalesce(last_meal_time_11_12_day_1, last_meal_time_13_14_day_1),
      combo_2 ~ dplyr::coalesce(last_meal_time_11_12_day_1, last_meal_time_13_14_day_1) - # day 1 weekend (Fr-So), day 2 weekday (Mo-Do)
        dplyr::coalesce(last_meal_time_11_12_day_2, last_meal_time_13_14_day_2),
      TRUE ~ NA
    ),
    latest_meal_diff_hours = as.numeric(latest_meal_difference, units = "hours")
  )

# _Calculate difference between eating windows-----------
diet_days <- diet_days %>%
  dplyr::mutate(
    eating_window_difference = dplyr::case_when(
      combo_1 ~ dplyr::coalesce(eating_window_hours_11_12_day_2, eating_window_hours_13_14_day_2) -
        dplyr::coalesce(eating_window_hours_11_12_day_1, eating_window_hours_13_14_day_1),
      combo_2 ~ dplyr::coalesce(eating_window_hours_11_12_day_1, eating_window_hours_13_14_day_1) -
        dplyr::coalesce(eating_window_hours_11_12_day_2, eating_window_hours_13_14_day_2),
      TRUE ~ NA_real_
    )
  )


table(is.na(diet_days$latest_meal_diff_hours)) # 
summary(diet_days$latest_meal_diff_hours) # mean=0.149hours -> slightly later eating
# on average on weekend-days

diet_days %>%
  #dplyr::filter(Wave == "Wave_11_12") %>%
  dplyr::filter(Wave == "Wave_13_14") %>%
ggplot(aes(x = latest_meal_diff_hours)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Latest meal time: Weekend vs Weekday",
       x = "Difference in hours (weekend - weekday)",
       y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

diet_days %>%
  #dplyr::filter(Wave == "Wave_11_12") %>%
  dplyr::filter(Wave == "Wave_13_14") %>%
ggplot(aes(x = eating_window_difference)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Eating time window: Weekend vs Weekday",
       x = "Difference in hours (weekend - weekday)",
       y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

#View(diet_days %>% dplyr::filter(latest_meal_diff_hours > 5))
quantile(diet_days$latest_meal_diff_hours, probs = c(0.01, 0.99), na.rm = TRUE)
quantile(diet_days$eating_window_difference, probs = c(0.01, 0.99), na.rm = TRUE)

# look at outliers
diet_days %>%
  dplyr::filter(latest_meal_diff_hours > 5) %>%
  dplyr::arrange(desc(latest_meal_diff_hours)) %>% View(title = "Outliers latest meal time difference")

# _Winsorize latest meal diff---------
diet_days$latest_meal_diff_hours_wins <- DescTools::Winsorize(
  x = diet_days$latest_meal_diff_hours,
  val = quantile(diet_days$latest_meal_diff_hours, probs = c(0.01, 0.99), na.rm = TRUE)
)
# _Winsorize eating window diff----------
diet_days$eating_window_difference_wins <- DescTools::Winsorize(
  x = diet_days$eating_window_difference,
  val = quantile(diet_days$eating_window_difference, probs = c(0.01, 0.99), na.rm = TRUE)
)

# _Winsorize latest meals:---------
diet_days$latest_meal_time_11_12_day_1_wins <- DescTools::Winsorize(
  x = diet_days$last_meal_time_11_12_day_1,
  val = quantile(diet_days$last_meal_time_11_12_day_1, probs = c(0.01, 0.99), na.rm = TRUE)
)
diet_days$latest_meal_time_11_12_day_2_wins <- DescTools::Winsorize(
  x = diet_days$last_meal_time_11_12_day_2,
  val = quantile(diet_days$last_meal_time_11_12_day_2, probs = c(0.01, 0.99), na.rm = TRUE)
)
diet_days$latest_meal_time_13_14_day_1_wins <- DescTools::Winsorize(
  x = diet_days$last_meal_time_13_14_day_1,
  val = quantile(diet_days$last_meal_time_13_14_day_1, probs = c(0.01, 0.99), na.rm = TRUE)
)
diet_days$latest_meal_time_13_14_day_2_wins <- DescTools::Winsorize(
  x = diet_days$last_meal_time_13_14_day_2,
  val = quantile(diet_days$last_meal_time_13_14_day_2, probs = c(0.01, 0.99), na.rm = TRUE)
)

ggplot(diet_days, aes(x = latest_meal_diff_hours_wins)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Latest meal time: Weekend vs Weekday (Winsorized)",
       x = "Difference in hours (weekend - weekday)",
       y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(diet_days, aes(x = eating_window_difference_wins)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Latest meal time: Weekend vs Weekday (Winsorized)",
       x = "Difference in hours (weekend - weekday)",
       y = "Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Latest meal distributions:
plot_meal_hist <- function(data, column_name, 
                           tag = NULL, winsorize = TRUE, 
                           lower_q = 0.01, upper_q = 0.99) {
  # 1. extract time
  raw_time <- data[[column_name]]
  clean_time <- gsub("H", ":", gsub("M", ":", gsub("S", "", raw_time)))
  meal_time <- hms(clean_time)
  meal_hours <- period_to_seconds(meal_time) / 3600
  
  # 2. Winsorizing (optional)
  if (winsorize) {
    q_low <- quantile(meal_hours, lower_q, na.rm = TRUE)
    q_high <- quantile(meal_hours, upper_q, na.rm = TRUE)
    meal_hours <- pmin(pmax(meal_hours, q_low), q_high)
  }
  
  # 3. Median 
  med <- median(meal_hours, na.rm = TRUE)
  med_label <- sprintf("Median: %02d:%02d", floor(med), round((med %% 1) * 60))
  
  # 4. Plot
  ggplot(data.frame(meal_hours), aes(x = meal_hours)) +
    geom_histogram(binwidth = 0.25, fill = "skyblue", color = "black") +
    geom_vline(xintercept = med, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", x = med, y = 0, label = med_label, vjust = -1, color = "red", size = 4, fontface = "bold") +
    scale_x_continuous(
      name = "Letzte Mahlzeit (Uhrzeit)",
      breaks = seq(15, 30, by = 1),
      labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60))
    ) +
    labs(
      title = paste("Verteilung der letzten Mahlzeiten", if (!is.null(tag)) paste0("– ", tag) else ""),
      y = "Anzahl"
    ) +
    coord_cartesian(ylim = c(0, NA)) +  # Verhindert Warnungen bei y=Inf
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
plot_meal_hist(diet_days, "last_meal_time_11_12_day_1", "day 1 (2011/12)")
plot_meal_hist(diet_days, "last_meal_time_11_12_day_2", "day 2 (2011/12)")
plot_meal_hist(diet_days, "last_meal_time_13_14_day_1", "day 1 (2013/14)")  
plot_meal_hist(diet_days, "last_meal_time_13_14_day_2", "day 2 (2013/14)")  

# save to z_intermediate_results
parent_directory <- dirname(getwd())
saveRDS(diet_days, file = file.path(parent_directory, "z_intermediate_results", "diet_days_17.4.25.rds"))


# Anzahl SEQNs mit 1x Weekday & 1x Fri/Sat/Sun
diet_days %>%
  dplyr::filter(combo_1 | combo_2) %>%
  dplyr::summarise(n = dplyr::n_distinct(SEQN)) %>%
  dplyr::pull(n)
# 10884

# _Combine cholesterol----
chol_cols_not_in_both <- setdiff(colnames(df1112_cholesterol), colnames(df1314_cholesterol))
existing_chol_cols_1112 <- intersect(chol_cols_not_in_both, colnames(df1112_cholesterol))
existing_chol_cols_1314 <- intersect(chol_cols_not_in_both, colnames(df1314_cholesterol))
df_cholesterol <- bind_rows(
  df1112_cholesterol %>% dplyr::select(-all_of(existing_chol_cols_1112)),
  df1314_cholesterol %>% dplyr::select(-all_of(existing_chol_cols_1314))
)

# _Combine SRIs----
SRIs <- bind_rows(SRI_1112, SRI_1314)

#df_PAX_filtered %>% filter(binary_sleep == 0) %>% dplyr::select(PAXMTSM) %>% hist()

# _Combine activity patterns----
df_activity_patterns1112 <- df_PAX1112_filtered %>%
  dplyr::select(SEQN, PAXMTSM) %>%
  dplyr::filter(PAXMTSM > 0) %>%                       # Filter for PAXMTSM > 0
  dplyr::group_by(SEQN) %>%
  dplyr::mutate(activity_level = median(PAXMTSM, na.rm = TRUE))  # Calculate median

df_activity_patterns1314 <- df_PAX1314_filtered %>%
  dplyr::select(SEQN, PAXMTSM) %>%
  dplyr::filter(PAXMTSM > 0) %>%                       # Filter for PAXMTSM > 0
  dplyr::group_by(SEQN) %>%
  mutate(activity_level = median(PAXMTSM, na.rm = TRUE))  # Calculate median

# Plot histograms of the median activity levels
#hist(df_activity_patterns1112$activity_level, main = "Histogram of Median Activity Level (2011-2012)", xlab = "Median PAXMTSM", col = "lightblue", border = "black")
#hist(df_activity_patterns1314$activity_level, main = "Histogram of Median Activity Level (2013-2014)", xlab = "Median PAXMTSM", col = "lightgreen", border = "black")
df_activity_patterns <- rbind(df_activity_patterns1112, df_activity_patterns1314)
#hist(df_activity_patterns$activity_level)

df_activity_patterns_unique <- df_activity_patterns %>%
  group_by(SEQN) %>%
  slice(1) %>%
  ungroup()
dim(df_activity_patterns_unique) # 10805

# _Combine DUAL ENERGY X-RAY ABSORPTIOMETRY (DXA)----
dax_cols_not_in_both <- setdiff(colnames(df1112_dxa), colnames(df1314_dxa))
existing_dax_cols_1112 <- intersect(dax_cols_not_in_both, colnames(df1112_dxa))
existing_dax_cols_1314 <- intersect(dax_cols_not_in_both, colnames(df1314_dxa))
df_dxa <- bind_rows(
  df1112_dxa %>% dplyr::select(-all_of(existing_dax_cols_1112)),
  df1314_dxa %>% dplyr::select(-all_of(existing_dax_cols_1314))
)
dim(df_dxa) # 11725

# _Combine standard biochemistry----
biochem_cols_not_in_both <- setdiff(colnames(df1112_biochemistry), colnames(df1314_biochemistry))
existing_biochem_cols_1112 <- intersect(biochem_cols_not_in_both, colnames(df1112_biochemistry))
existing_biochem_cols_1314 <- intersect(biochem_cols_not_in_both, colnames(df1314_biochemistry))
df_biochemistry <- bind_rows(
  df1112_biochemistry %>% dplyr::select(-all_of(existing_biochem_cols_1112)),
  df1314_biochemistry %>% dplyr::select(-all_of(existing_biochem_cols_1314))
)
dim(df_biochemistry) # 13528

# _Combine Dual Energy X-ray Absorptiometry (DXA) whole body----
dax_whole_cols_not_in_both <- setdiff(colnames(df1112_dxa_whole_body), colnames(df1314_dxa_whole_body))
existing_dax_whole_cols_1112 <- intersect(dax_whole_cols_not_in_both, colnames(df1112_dxa_whole_body))
existing_dax_whole_cols_1314 <- intersect(dax_whole_cols_not_in_both, colnames(df1314_dxa_whole_body))
df_dxa_whole_body <- bind_rows(
  df1112_dxa_whole_body %>% dplyr::select(-all_of(existing_dax_whole_cols_1112)),
  df1314_dxa_whole_body %>% dplyr::select(-all_of(existing_dax_whole_cols_1314))
)
dim(df_dxa_whole_body) # 11725

# _Combine all together----
# __List of datasets with names, ordered----
datasets <- list(
  df_demo = df_demo,
  df_diet_behaviour = df_diet_behaviour,
  df_body_measures = df_body_measures,
  df_blood_pressure = df_blood_pressure,
  df_header = df_header,
  df_smoking = df_smoking,
  df_sleep_disorders = df_sleep_disorders,
  df_alcohol_use = df_alcohol_use,
  df_depression = df_depression,
  df_diabetes = df_diabetes,
  df_early_childhood = df_early_childhood,
  df_drug_use = df_drug_use,
  df_occupation = df_occupation,
  df_cardiovascular_health = df_cardiovascular_health,
  df_vitamin_D = df_vitamin_D,
  df_dietary_intake,
  df_dietary_intake2,
  df_cholesterol = df_cholesterol,
  df_activity_patterns = df_activity_patterns_unique,
  df_dxa = df_dxa,
  df_dxa_whole_body = df_dxa_whole_body,
  df_biochemistry = df_biochemistry,
  SRIs = SRIs
)
unique_seqn_counts <- sapply(datasets, function(df) length(unique(df$SEQN)))
unique_seqn_counts_df <- data.frame(
  Dataset = names(unique_seqn_counts),
  Unique_SEQN = unique_seqn_counts
)
unique_seqn_counts_df <- unique_seqn_counts_df[order(-unique_seqn_counts_df$Unique_SEQN), ]
print(unique_seqn_counts_df)
# -> demo file contains the most SEQN.

# check sizes of datasets to be joined:
lapply(datasets, dim)

# __join----
join_by_seqn <- function(x, y) {
  left_join(x, y, by = "SEQN")
}
df <- reduce(datasets, join_by_seqn)
dim(df) # 19931
head(df)


# _add wave-Variable----
dim(df)
df <- df %>% mutate(Wave = case_when(
  SEQN %in% df1112_demo$SEQN ~ "NHANES_1112",
  SEQN %in% df1314_demo$SEQN ~ "NHANES_1314"
))
table(df$Wave)
sum(table(df$Wave)) # 19931 check.

# _add Depression Score----
df <- df %>% 
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)


# _add blood pressure-------
df <- df %>%
  rowwise() %>%
  mutate(
    Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE), # 4 readings
    Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE) # 4 readings
  ) %>%
  ungroup() # ungroup after rowwise operations

# check
hist(df$Avg_Systolic_BP)
hist(df$Avg_Diastolic_BP)

# 3) SAVE COMBINED data set----
# data set for EDA and main analysis
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
file_path <- file.path(parent_directory, "4_DATA", "Combined_data_sets_2_waves_15.4.25_incl_meal_timing.RDS")
saveRDS(df, file_path)

# READ COMBINED data set----
# (current_directory <- getwd())
# (parent_directory <- dirname(current_directory))
# file_path <- file.path(parent_directory, "4_DATA", "Combined_data_sets_2_waves_15.4.25_incl_meal_timing.RDS")
# df <- readRDS(file_path)


# __SessionInfo for replicability__--------
session_info <- capture.output(sessionInfo())
file_name <- paste0("./SessionInfos_Replicability/2_Read_Clean_Prepare_for_Analysis_",today(),".txt")
writeLines(session_info, con = file_name)



