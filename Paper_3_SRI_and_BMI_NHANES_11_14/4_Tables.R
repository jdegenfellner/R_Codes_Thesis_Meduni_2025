library(pacman)
p_load(tidyverse,
       gt, openxlsx, flextable, officer, survey, gtsummary, flextable, officer,
       knitr, boot, survey, purrr, tictoc
)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# READ--------

# read: saveRDS(df_an1, paste0(parent_directory, "/z_intermediate_results/df_an1_28.5.25.RDS"))

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
df_an1_imputed <- readRDS(paste0(parent_directory, "/4_DATA/df_an1_imputed.RDS"))
df_an1 <- readRDS(paste0(parent_directory, "//z_intermediate_results/df_an1_28.5.25.RDS"))



# Redefine Income for Table 1----------
# HH Income was asked here (using 3 questions):
# https://wwwn.cdc.gov/nchs/data/nhanes/2011-2012/questionnaires/inq_family.pdf
# INQ.200, INQ.220, INQ.230

# Formally, one can only lump together the categories >=20k USD / <20k USD
# Redefine Income for Table 1
df_an1_imputed$Income_lumped_20k_table1 <- ifelse(df_an1_imputed$INDHHIN2 %in% c(77, 99), NA,
                                          ifelse(df_an1_imputed$INDHHIN2 %in% c(1, 2, 3, 4, 13), "< $20,000", "≥ $20,000"))
df_an1_imputed$Income_lumped_20k_table1 <- factor(df_an1_imputed$Income_lumped_20k_table1, 
                                          levels = c("< $20,000", "≥ $20,000"))
table(df_an1_imputed$Income_lumped_20k_table1, useNA = "ifany")

model_vars <- df_an1_imputed %>%
  dplyr::select(
    SRI_no_imputation_winsorized_rand, # SRI (Exposure)
    RIDAGEYR,                          # Age
    RIAGENDR_factor,                   # Gender
    RIDRETH3_factor,                   # Ethnicity
    #BMXBMI_winsorized,                 # BMI (log-transformed in model)
    DMDEDUC2_factor,                   # Education
    #INDHHIN2_factor,                  # Income
    Income_lumped_20k_table1,          # Income
    #OCD150_factor,                    # Occupational category
    Occupation_grouped,               # Occupational category
    DMDMARTL_factor,                   # Marital status
    alcohol_consumption,               # Alcohol consumption
    smoking_status,                    # Smoking status
    LBXVIDMS_winsorized,               # Vitamin D
    DR1TKCAL_winsorized,               # Total caloric intake
    Depression_score_PHQ_9,            # Depression score
    activity_level,                    # Activity level
    Wave                               # Wave
  )

# Rename the variables for better readability
model_vars_readable <- model_vars %>%
  rename(
    SRI = SRI_no_imputation_winsorized_rand,
    Age = RIDAGEYR,
    Gender = RIAGENDR_factor,
    Ethnicity = RIDRETH3_factor,
    #BMI = BMXBMI_winsorized,
    Education = DMDEDUC2_factor,
    #Income = INDHHIN2_factor,
    Income = Income_lumped_20k_table1,
    #Occupation = OCD150_factor,
    Occupation = Occupation_grouped,
    Marital_Status = DMDMARTL_factor,
    Alcohol = alcohol_consumption,
    Smoking = smoking_status,
    VitaminD = LBXVIDMS_winsorized,
    Depression = Depression_score_PHQ_9,
    Total_Calories = DR1TKCAL_winsorized,
    Activity_Level = activity_level,
    Wave = Wave
  )
  
#Hmisc::label(model_vars_readable$BMI) <- "Body Mass Index (BMI)"
Hmisc::label(model_vars_readable$SRI) <- "Sleep Regularity Index (SRI)"
Hmisc::label(model_vars_readable$Ethnicity) <- "Race/Ethnicity"
Hmisc::label(model_vars_readable$Gender) <- "Gender"
Hmisc::label(model_vars_readable$Age) <- "Age"
Hmisc::label(model_vars_readable$Education) <- "Education Level"
Hmisc::label(model_vars_readable$Income) <- "Household Income"
Hmisc::label(model_vars_readable$Occupation) <- "Occupational Category"
Hmisc::label(model_vars_readable$Alcohol) <- "Alcohol Consumption"
Hmisc::label(model_vars_readable$Smoking) <- "Smoking Status"
Hmisc::label(model_vars_readable$VitaminD) <- "Vitamin D Level"
Hmisc::label(model_vars_readable$Depression) <- "Depression Score (PHQ-9)"
Hmisc::label(model_vars_readable$Total_Calories) <- "Total Caloric Intake"
Hmisc::label(model_vars_readable$Marital_Status) <- "Marital Status"
Hmisc::label(model_vars_readable$Activity_Level) <- "Activity Level"


# >>Table 1 ---------
# _Table 1 by sex-----------

table1.1 <- model_vars_readable %>%
  dplyr::select(-Wave) %>%
  tbl_summary(
    by = Gender,  # Stratify by Gender
    missing = "no"  # Exclude missing values from summary
  ) %>%
  modify_header(label = "") %>%  # Customize the header
  bold_labels()  # Bold the labels for better readability
table1.1


# _Table 1 by ethnicity -------

# Create Table 1 stratified by Ethnicity (no p-values)
table1.2 <- model_vars_readable %>%
  dplyr::select(-Wave) %>%
  tbl_summary(
    by = Ethnicity,  # Stratification by Ethnicity
    missing = "no"  # Exclude missing values from summary
  ) %>%
  modify_header(label = "") %>%  # Customize the header
  bold_labels()  # Bold the labels for better readability
table1.2

# _Table 1 by Gender and Ethnicity------
# large...
table1.3 <- model_vars_readable %>%
  dplyr::select(-Wave) %>%
  tbl_strata(
    strata = Gender,  # First stratification by Gender
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = Ethnicity,  # Second stratification by Ethnicity
        missing = "no"  # Exclude missing values from summary
      )
  ) %>%
  modify_header(label = "**Variable**") %>%  # Modify the headers
  bold_labels()  # Bold the labels for readability
table1.3

# _Table 1 by Wave------

# Customize the summary statistics with explicit variable name for Age
table1.4 <- model_vars_readable %>%
  tbl_summary(
    by = Wave,  # Stratify by Wave
    missing = "no",  # Exclude missing values from summary
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",  # Median (IQR) for all continuous variables
      Age ~ "{mean} ({sd})",  # Mean (SD) specifically for Age
      all_categorical() ~ "{n} ({p}%)"  # Frequency (Percentage) for categorical variables
    )
  ) %>%
  add_overall() %>%  # Add total column with overall summary
  modify_header(label = "") %>%  # Customize the header
  bold_labels()  # Bold the labels for better readability
table1.4

# Convert to flextable and set column widths
table1.4_flextable <- table1.4 %>%
  as_flex_table() %>%
  flextable::width(j = 1, width = 2) %>%  # Set width for the first column (adjust as needed)
  flextable::width(j = 2:4, width = 1.5)  # Set width for remaining columns (adjust as needed)

# Export to Word document
flextable::save_as_docx(
  table1.4_flextable,
  path = paste0(parent_directory,"/RESULTS/Tables/Table1_by_Wave.docx")
)

# _Table 1 by Quintiles of SRI------
table1.5 <- model_vars_readable %>%
  dplyr::select(-Wave) %>%
  mutate(SRI_quintile = cut(SRI,
                            breaks = quantile(SRI, probs = seq(0, 1, by = 0.2), 
                                              na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("SRI Q1", "SRI Q2", "SRI Q3", "SRI Q4", "SRI Q5"))) %>%
  tbl_summary(
    by = SRI_quintile,  # Stratify by SRI quintiles
    missing = "no"      # Exclude missing values from summary
  ) %>%
  add_overall() %>%       # Add the overall column
  modify_header(label = "") %>%  # Customize the header
  bold_labels()  # Bold the labels for better readability
table1.5

1417*5 # 7085

# Convert to flextable and set column widths
table1.5_flextable <- table1.5 %>%
  as_flex_table() %>%
  flextable::width(j = 1, width = 2) %>%  # Set width for the first column (adjust as needed)
  flextable::width(j = 2:4, width = 1.5)  # Set width for remaining columns (adjust as needed)

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))

# Export to Word document
flextable::save_as_docx(
  table1.5_flextable,
  path = paste0(parent_directory, "/RESULTS/Tables/Table1_by_SRI_Quintiles.docx")
)

# _Table 1 by Quintiles for Waves separately-------

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))

# Filter and create Table 1 for Wave NHANES_1112
table1_wave_1112 <- model_vars_readable %>%
  dplyr::filter(Wave == "NHANES_1112") %>%
  dplyr::select(-Wave) %>%
  mutate(SRI_quintile = cut(SRI,
                            breaks = quantile(SRI, probs = seq(0, 1, by = 0.2), 
                                              na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("SRI Q1", "SRI Q2", "SRI Q3", "SRI Q4", "SRI Q5"))) %>%
  tbl_summary(
    by = SRI_quintile,  # Stratify by SRI quintiles
    missing = "no"      # Exclude missing values from summary
  ) %>%
  add_overall() %>%
  modify_header(label = "") %>%
  bold_labels()

# Filter and create Table 1 for Wave NHANES_1314
table1_wave_1314 <- model_vars_readable %>%
  dplyr::filter(Wave == "NHANES_1314") %>%
  dplyr::select(-Wave) %>%
  mutate(SRI_quintile = cut(SRI,
                            breaks = quantile(SRI, probs = seq(0, 1, by = 0.2), 
                                              na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("SRI Q1", "SRI Q2", "SRI Q3", "SRI Q4", "SRI Q5"))) %>%
  tbl_summary(
    by = SRI_quintile,  # Stratify by SRI quintiles
    missing = "no"      # Exclude missing values from summary
  ) %>%
  add_overall() %>%
  modify_header(label = "") %>%
  bold_labels()

# Convert tables to flextables
table1_wave_1112_flextable <- table1_wave_1112 %>%
  as_flex_table() %>%
  flextable::width(j = 1, width = 2) %>%
  flextable::width(j = 2:4, width = 1.5)

table1_wave_1314_flextable <- table1_wave_1314 %>%
  as_flex_table() %>%
  flextable::width(j = 1, width = 2) %>%
  flextable::width(j = 2:4, width = 1.5)

# Export both tables to Word documents
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))

flextable::save_as_docx(
  table1_wave_1112_flextable,
  path = paste0(parent_directory, "/RESULTS/Tables/Table1_by_SRI_Quintiles_NHANES_1112.docx")
)

flextable::save_as_docx(
  table1_wave_1314_flextable,
  path = paste0(parent_directory, "/RESULTS/Tables/Table1_by_SRI_Quintiles_NHANES_1314.docx")
)

# >>Table 2 obesity measures (incl. BMI)-----------

# Load required libraries
library(gtsummary)
library(dplyr)
library(Hmisc)

# Define nice labels for obesity measures
Hmisc::label(df_an1$BMXBMI_winsorized) <- "Body Mass Index (BMI)"
Hmisc::label(df_an1$BMXWAIST_winsorized) <- "Waist Circumference (cm)"
Hmisc::label(df_an1$ABSI) <- "A Body Shape Index (ABSI)"
Hmisc::label(df_an1$WHtR) <- "Waist-to-Height Ratio (WHtR)"
Hmisc::label(df_an1$BMDAVSAD_winsorized) <- "Sagittal Abdominal Diameter (cm)"
Hmisc::label(df_an1$SADHtR) <- "SAD-to-Height Ratio (SADHtR)"
Hmisc::label(df_an1$VAI) <- "Visceral Adiposity Index (VAI)"
Hmisc::label(df_an1$DXDTOFAT_kg_winsorized) <- "Total Fat Mass (kg)"
Hmisc::label(df_an1$FMI) <- "Fat Mass Index (FMI)"
Hmisc::label(df_an1$DXDTOPF_winsorized) <- "Percent Body Fat (%)"
Hmisc::label(df_an1$BRI) <- "Body Roundness Index (BRI)"
Hmisc::label(df_an1$LAP) <- "Lipid Accumulation Product (LAP)"
Hmisc::label(df_an1$SRI_no_imputation_winsorized_rand) <- "Sleep Regularity Index (SRI)"

# Create quintiles of SRI
df_an1 <- df_an1 %>%
  mutate(SRI_quintile = cut(SRI_no_imputation_winsorized_rand,
                            breaks = quantile(SRI_no_imputation_winsorized_rand, probs = seq(0, 1, by = 0.2), 
                                              na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("SRI Q1", "SRI Q2", "SRI Q3", "SRI Q4", "SRI Q5")))

# Select the obesity measures and SRI quintiles
obesity_measures <- df_an1 %>%
  dplyr::select(
    SRI_quintile,
    BMXBMI_winsorized, 
    BMXWAIST_winsorized, 
    ABSI, 
    WHtR, 
    BMDAVSAD_winsorized, 
    SADHtR, 
    VAI, 
    DXDTOFAT_kg_winsorized, 
    FMI, 
    DXDTOPF_winsorized, 
    BRI, 
    LAP
  )

# Calculate missingness rates
missingness <- obesity_measures %>%
  dplyr::summarise(across(where(is.numeric), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missingness (%)")

# Create the summary table with an overall column
table_obesity_SRI <- obesity_measures %>%
  tbl_summary(
    by = SRI_quintile,  # Stratify by SRI quintiles
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",  # Median (IQR)
    missing = "no",  # Exclude missing values from summary
    label = list(
      BMXBMI_winsorized ~ "Body Mass Index (BMI)",
      BMXWAIST_winsorized ~ "Waist Circumference (cm)",
      ABSI ~ "A Body Shape Index (ABSI)",
      WHtR ~ "Waist-to-Height Ratio (WHtR)",
      BMDAVSAD_winsorized ~ "Sagittal Abdominal Diameter (cm)",
      SADHtR ~ "SAD-to-Height Ratio (SADHtR)",
      VAI ~ "Visceral Adiposity Index (VAI)",
      DXDTOFAT_kg_winsorized ~ "Total Fat Mass (kg)",
      FMI ~ "Fat Mass Index (FMI)",
      DXDTOPF_winsorized ~ "Percent Body Fat (%)",
      BRI ~ "Body Roundness Index (BRI)",
      LAP ~ "Lipid Accumulation Product (LAP)"
    )
  ) %>%
  add_overall() %>%  # Add the overall column
  modify_header(label = "", stat_0 = "**Median (IQR)**") %>%  # Customize header
  modify_caption("") %>%  # Add caption
  bold_labels()  # Bold variable labels

# Append missingness to the table body
table_obesity_SRI <- table_obesity_SRI %>%
  modify_table_body(
    ~ .x %>%
      left_join(
        missingness %>%
          rename(Missingness = `Missingness (%)`) %>%
          mutate(Missingness = sprintf("%.2f%%", Missingness)),
        by = c("variable" = "Variable")
      )
  ) %>%
  modify_header(Missingness = "**Missingness (%)**")  # Add a header for the new column

# Print the table
table_obesity_SRI

(current_directory <- getwd())
(parent_directory <- dirname(current_directory))

# Save table to Word document
flextable::save_as_docx(
  as_flex_table(table_obesity_SRI),
  path = paste0(parent_directory, "/RESULTS/Tables/Table2_Obesity_Measures_by_SRI_Quintiles.docx")
)


# Regression summary tables Model 2 svy/Model 6 svy---------
# Model 2 svy---------
# Create the tbl_regression table with formatting
model2_table <- tbl_regression(
  model2_svy, 
  exponentiate = TRUE,  # Exponentiate to interpret log-transformed response
  label = list(
    SRI_no_imputation_winsorized_rand ~ "SRI",
    RIDAGEYR ~ "Age",
    RIAGENDR_factor ~ "Gender"
  ),
  estimate_fun = purrr::partial(style_ratio, digits = 4),
  pvalue_fun = purrr::partial(style_sigfig, digits = 4)
)

# Convert the gtsummary table to a flextable object for Word
model2_flextable <- as_flex_table(model2_table) %>%
  flextable::set_caption("Model 2: Survey-weighted regression of log(BMI)")

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(model2_flextable)
print(doc, target = paste0(parent_directory, "/RESULTS/Tables/Model2_Table.docx"))

# Model 4 svy----------
# Create the tbl_regression table with formatting
model4_table <- tbl_regression(
  model4_svy,
  exponentiate = TRUE,
  label = list(
    SRI_no_imputation_winsorized_rand ~ "Sleep Regularity Index (SRI)",
    RIAGENDR_factor ~ "Sex",
    RIDAGEYR ~ "Age (years)",
    RIDRETH3_factor ~ "Race/Ethnicity",
    DMDEDUC2_factor ~ "Education level",
    INDHHIN2_factor ~ "Household income",
    OCD150_factor ~ "Occupational category",
    alcohol_consumption ~ "Alcohol consumption",
    smoking_status ~ "Smoking status",
    LBXVIDMS ~ "Vitamin D (ng/mL)",
    Depression_score_PHQ_9 ~ "Depression score (PHQ-9)",
    DR1TKCAL_winsorized ~ "Total energy intake (kcal)",
    DMDMARTL_factor ~ "Marital status",
    activity_level ~ "Physical activity level"
  )
)

# Convert the gtsummary table to a flextable object for Word
model4_flextable <- as_flex_table(model4_table) %>%
  flextable::set_caption("Model 4: Survey-weighted regression of log(BMI)")




# Model 6 svy---------

# Step 1: Prepare regression results with exponentiated values
coefs <- coef(model6_svy)
se <- sqrt(diag(vcov(model6_svy)))
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
ci_lower <- coefs - z_value * se
ci_upper <- coefs + z_value * se

# Create a data frame with only exponentiated values
regression_results <- data.frame(
  Term = names(coefs),
  `Estimate (Exp)` = exp(coefs),
  `95% CI Lower (Exp)` = exp(ci_lower),
  `95% CI Upper (Exp)` = exp(ci_upper),
  check.names = FALSE
)

# Step 2: Create a nicely formatted flextable
# Create a nicely formatted flextable
reg_table <- regression_results %>%
  flextable() %>%
  set_header_labels(
    Term = "Predictor",
    `Estimate (Exp)` = "Estimate (Exp)",
    `95% CI Lower (Exp)` = "95% CI Lower (Exp)",
    `95% CI Upper (Exp)` = "95% CI Upper (Exp)"
  ) %>%
  add_header_row(
    values = c("Exponentiated Regression Results"),  # Single header
    colwidths = 4  # Must match the total number of columns
  ) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all")

# Adjust column widths in the flextable
reg_table <- reg_table %>%
  width(j = ~ Term, width = 2) %>%  # Adjust Predictor column width
  width(j = ~ `Estimate (Exp)`, width = 1.5) %>%  # Adjust Estimate column width
  width(j = ~ `95% CI Lower (Exp)`, width = 1.5) %>%  # Adjust CI Lower column
  width(j = ~ `95% CI Upper (Exp)`, width = 1.5) %>%  # Adjust CI Upper column
  flextable::set_caption("Model 6: Exponentiated Regression Summary")

# Step 3: Export to Word
# Add a caption for the flextable
reg_table <- reg_table %>%
  flextable::set_caption("Model 6: Exponentiated Regression Summary")

# Define the output Word document path
output_word <- paste0(parent_directory,"/RESULTS/Tables/Model6_Table.docx")

# Create a new Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(reg_table)

# Save the Word document
print(doc, target = output_word)

cat("Model 6 table successfully saved to Word file at:", output_word)

# >>Table 3 for Main Document----------
# __4 Models:------------
# Model 1 (model 2 svy): age + sex
# Model 2 (model 3 svy): all but interactions
# Model 3 (model 4 svy): all and interaction sex*SRI
# Model 4 (model 6 svy): all + both interactions sex*SRI + ethnicity*SRI

# __Model 1 (model 2 svy)---------
df_an1_imputed <- df_an1_imputed %>%
  mutate(SRI_quintile = ntile(SRI_no_imputation_winsorized_rand, 5))

SRI_medians <- df_an1_imputed %>%
  dplyr::group_by(SRI_quintile) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation_winsorized_rand, na.rm = TRUE)) %>%
  pull(median_SRI)

SRI_Q1_median <- SRI_medians[1]
SRI_Q2_median <- SRI_medians[2]
SRI_Q3_median <- SRI_medians[3]
SRI_Q4_median <- SRI_medians[4]
SRI_Q5_median <- SRI_medians[5]

SRI_medians <- c(SRI_Q1_median, SRI_Q2_median, 
                 SRI_Q3_median, SRI_Q4_median, SRI_Q5_median)

# Function to calculate the overall multiplicative effect for Model 1
calculate_overall_effect_model1 <- function(model, SRI_start, SRI_end) {
  # Set fixed values for covariates (without gender stratification)
  fixed_values <- data.frame(
    RIDAGEYR = mean(df_an1_imputed$RIDAGEYR, na.rm = TRUE),
    RIAGENDR_factor = factor(unique(df_an1_imputed$RIAGENDR_factor)[1], 
                             levels = unique(df_an1_imputed$RIAGENDR_factor)),
    SRI_no_imputation_winsorized_rand = SRI_start
  )
  
  # Predict log(BMI) for the start SRI value
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Update SRI to the end value and predict again
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Compute and return the multiplicative effect
  exp(logBMI_end - logBMI_start)
}

# Bootstrap function for overall multiplicative effects in Model 1
bootstrap_overall_effects_model1 <- function(data, n_boot = 500, SRI_medians) {
  bootstrap_results <- matrix(NA, nrow = n_boot, ncol = 4)  # Storing results for Q1→Q2, Q1→Q3, Q1→Q4, Q1→Q5
  
  for (i in 1:n_boot) {
    # Resample with replacement
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    
    # Recreate the survey design for the bootstrap sample
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    # Fit Model 1 (simpler model without interactions)
    boot_model <- svyglm(log(BMXBMI) ~ SRI_no_imputation_winsorized_rand + 
                           RIDAGEYR + RIAGENDR_factor, # age + sex 
                         design = boot_design)
    
    # Compute the multiplicative effects from Q1 to each subsequent quintile
    bootstrap_results[i, ] <- c(
      calculate_overall_effect_model1(boot_model, SRI_medians[1], SRI_medians[2]),
      calculate_overall_effect_model1(boot_model, SRI_medians[1], SRI_medians[3]),
      calculate_overall_effect_model1(boot_model, SRI_medians[1], SRI_medians[4]),
      calculate_overall_effect_model1(boot_model, SRI_medians[1], SRI_medians[5])
    )
  }
  
  colnames(bootstrap_results) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")
  return(bootstrap_results)
}

# Run bootstrap for Model 1
tic()
bootstrap_results_model1 <- bootstrap_overall_effects_model1(df_an1_imputed, 
                                                             n_boot = 500, 
                                                             SRI_medians)
toc() # 20s

# Convert bootstrap results into a data frame
bootstrap_results_combined <- as.data.frame(bootstrap_results_model1)
colnames(bootstrap_results_combined) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")

# Summarize results: Calculate mean and 95% confidence intervals
bootstrap_summary <- bootstrap_results_combined %>%
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

# Format results in the required style
formatted_results_model1 <- bootstrap_summary %>%
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

ft_model1 <- formatted_results_model1 %>%
  flextable() %>%
  set_header_labels(
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  add_header_row(
    values = c("Multiplicative Effects of SRI on BMI"), 
    colwidths = 4  # Adjusted to match the four columns
  ) %>%
  add_footer_row(
    values = "Parentheses (x; y) represent 95% confidence intervals.",
    colwidths = 4  # Ensure it covers all columns
  )

ft_model1

# ___p_trend------------
mod <- lm(log(BMXBMI_winsorized) ~ SRI_quintile, 
          data = df_an1_imputed)
summary(mod) # p-value: < 2.2e-16 (I would have known this without the model)


# ...Export to Word
#doc_gender <- read_docx() %>%
#  body_add_flextable(ft_gender)#

# ...Define file path and save the document
#(current_directory <- getwd())
#(parent_directory <- dirname(current_directory))
#print(doc_gender, target = paste0(parent_directory, "/RESULTS/Tables/multiplicative_effects_model2_svy.docx"))

# __Model 2 (model 3 svy)---------
# Function to calculate the overall multiplicative effect for Model 2
calculate_overall_effect_model2 <- function(model, SRI_start, SRI_end) {
  # Set fixed values for covariates (ensuring consistent factor levels)
  fixed_values <- data.frame(
    RIDAGEYR = mean(df_an1_imputed$RIDAGEYR, na.rm = TRUE),
    RIAGENDR_factor = factor(unique(df_an1_imputed$RIAGENDR_factor)[1], levels = unique(df_an1_imputed$RIAGENDR_factor)),
    RIDRETH3_factor = factor(unique(df_an1_imputed$RIDRETH3_factor)[1], levels = unique(df_an1_imputed$RIDRETH3_factor)),
    DMDEDUC2_factor = factor(unique(df_an1_imputed$DMDEDUC2_factor)[1], levels = unique(df_an1_imputed$DMDEDUC2_factor)),
    INDHHIN2_factor = factor(unique(df_an1_imputed$INDHHIN2_factor)[1], levels = unique(df_an1_imputed$INDHHIN2_factor)),
    OCD150_factor = factor(unique(df_an1_imputed$OCD150_factor)[1], levels = unique(df_an1_imputed$OCD150_factor)),
    alcohol_consumption = factor(unique(df_an1_imputed$alcohol_consumption)[1], levels = unique(df_an1_imputed$alcohol_consumption)),
    smoking_status = factor(unique(df_an1_imputed$smoking_status)[1], levels = unique(df_an1_imputed$smoking_status)),
    DMDMARTL_factor = factor(unique(df_an1_imputed$DMDMARTL_factor)[1], levels = unique(df_an1_imputed$DMDMARTL_factor)),
    LBXVIDMS = mean(df_an1_imputed$LBXVIDMS, na.rm = TRUE),
    DR1TKCAL_winsorized = mean(df_an1_imputed$DR1TKCAL_winsorized, na.rm = TRUE),
    Depression_score_PHQ_9 = mean(df_an1_imputed$Depression_score_PHQ_9, na.rm = TRUE),
    activity_level = mean(df_an1_imputed$activity_level, na.rm = TRUE),
    SRI_no_imputation_winsorized_rand = SRI_start
  )
  
  # Predict log(BMI) for the start SRI value
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Update SRI to the end value and predict again
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Compute and return the multiplicative effect
  exp(logBMI_end - logBMI_start)
}

# Bootstrap function for overall multiplicative effects in Model 2
bootstrap_overall_effects_model2 <- function(data, n_boot = 500, SRI_medians) {
  bootstrap_results <- matrix(NA, nrow = n_boot, ncol = 4)  # Storing results for Q1→Q2, Q1→Q3, Q1→Q4, Q1→Q5
  
  for (i in 1:n_boot) {
    # Resample with replacement
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    
    # Recreate the survey design for the bootstrap sample
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    # Fit Model 2 (all covariates, no interaction terms)
    boot_model <- svyglm(log(BMXBMI_winsorized) ~ 
                           SRI_no_imputation_winsorized_rand + 
                           RIDAGEYR + 
                           RIAGENDR_factor + 
                           RIDRETH3_factor + 
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
    
    # Compute the multiplicative effects from Q1 to each subsequent quintile
    bootstrap_results[i, ] <- c(
      calculate_overall_effect_model2(boot_model, SRI_medians[1], SRI_medians[2]),
      calculate_overall_effect_model2(boot_model, SRI_medians[1], SRI_medians[3]),
      calculate_overall_effect_model2(boot_model, SRI_medians[1], SRI_medians[4]),
      calculate_overall_effect_model2(boot_model, SRI_medians[1], SRI_medians[5])
    )
  }
  
  colnames(bootstrap_results) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")
  return(bootstrap_results)
}

# Run bootstrap for Model 2
tic()
bootstrap_results_model2 <- bootstrap_overall_effects_model2(df_an1_imputed, n_boot = 500, SRI_medians)
toc() # 50s

# Convert bootstrap results into a data frame
bootstrap_results_combined <- as.data.frame(bootstrap_results_model2)
colnames(bootstrap_results_combined) <- c("Q1 → Q2", "Q1 → Q3", "Q1 → Q4", "Q1 → Q5")

# Summarize results: Calculate mean and 95% confidence intervals
bootstrap_summary <- bootstrap_results_combined %>%
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

# Format results in the required style
formatted_results_model2 <- bootstrap_summary %>%
  dplyr::summarize(
    `Q1 → Q2` = paste0(round(`Q1 → Q2_mean`, 3), " (", round(`Q1 → Q2_CI_Lower`, 3), "; ", round(`Q1 → Q2_CI_Upper`, 3), ")"),
    `Q1 → Q3` = paste0(round(`Q1 → Q3_mean`, 3), " (", round(`Q1 → Q3_CI_Lower`, 3), "; ", round(`Q1 → Q3_CI_Upper`, 3), ")"),
    `Q1 → Q4` = paste0(round(`Q1 → Q4_mean`, 3), " (", round(`Q1 → Q4_CI_Lower`, 3), "; ", round(`Q1 → Q4_CI_Upper`, 3), ")"),
    `Q1 → Q5` = paste0(round(`Q1 → Q5_mean`, 4), " (", round(`Q1 → Q5_CI_Lower`, 3), "; ", round(`Q1 → Q5_CI_Upper`, 3), ")")
  )

ft_model2 <- formatted_results_model2 %>%
  flextable() %>%
  set_header_labels(
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit()

ft_model2

# __Model 3 (model 4 svy)---------
# Multiplicative effects of SRI on BMI by SRI-Quintiles for model 4
# Define combinations: now only varying by Sex (Ethnicity is kept fixed per default)
combinations_model3 <- expand.grid(
  RIAGENDR_factor = unique(df_an1_imputed$RIAGENDR_factor)
)

# Function to calculate the multiplicative effect for model4
calculate_effect_model3 <- function(sex, model, SRI_start, SRI_end) {
  # Set fixed values for all covariates
  fixed_values <- data.frame(
    RIAGENDR_factor = factor(sex, levels = levels(df_an1_imputed$RIAGENDR_factor)),
    RIDRETH3_factor = factor(names(sort(table(df_an1_imputed$RIDRETH3_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$RIDRETH3_factor)),
    RIDAGEYR = mean(df_an1_imputed$RIDAGEYR, na.rm = TRUE),
    DMDEDUC2_factor = factor(names(sort(table(df_an1_imputed$DMDEDUC2_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$DMDEDUC2_factor)),
    INDHHIN2_factor = factor(names(sort(table(df_an1_imputed$INDHHIN2_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$INDHHIN2_factor)),
    OCD150_factor = factor(names(sort(table(df_an1_imputed$OCD150_factor), decreasing = TRUE))[1],
                           levels = levels(df_an1_imputed$OCD150_factor)),
    alcohol_consumption = factor(names(sort(table(df_an1_imputed$alcohol_consumption), decreasing = TRUE))[1],
                                 levels = levels(df_an1_imputed$alcohol_consumption)),
    smoking_status = factor(names(sort(table(df_an1_imputed$smoking_status), decreasing = TRUE))[1],
                            levels = levels(df_an1_imputed$smoking_status)),
    LBXVIDMS = mean(df_an1_imputed$LBXVIDMS, na.rm = TRUE),
    DR1TKCAL_winsorized = mean(df_an1_imputed$DR1TKCAL_winsorized, na.rm = TRUE),
    Depression_score_PHQ_9 = mean(df_an1_imputed$Depression_score_PHQ_9, na.rm = TRUE),
    DMDMARTL_factor = factor(names(sort(table(df_an1_imputed$DMDMARTL_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$DMDMARTL_factor)),
    activity_level = mean(df_an1_imputed$activity_level, na.rm = TRUE),
    SRI_no_imputation_winsorized_rand = SRI_start
  )
  
  # Predict log(BMI) for start quintile
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Update SRI to end quintile and predict again
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Calculate and return multiplicative effect
  exp(logBMI_end - logBMI_start)
}

# Bootstrap function for model4
bootstrap_effects_model3 <- function(data, n_boot = 1000, SRI_medians) {
  results <- list()
  for (i in 1:n_boot) {
    # Resample data
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    boot_sample$RIAGENDR_factor <- factor(boot_sample$RIAGENDR_factor, levels = levels(data$RIAGENDR_factor))
    boot_sample$RIDRETH3_factor <- factor(boot_sample$RIDRETH3_factor, levels = levels(data$RIDRETH3_factor))
    
    # Recreate survey design
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    # Refit model 4
    boot_model <- svyglm(log(BMXBMI_winsorized) ~ 
                           SRI_no_imputation_winsorized_rand * RIAGENDR_factor + # SRI*sex
                           RIDAGEYR + 
                           RIDRETH3_factor + 
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
    
    # Calculate effects for each sex
    effects_list <- list()
    for (j in 1:nrow(combinations_model3)) {
      sex <- combinations_model3$RIAGENDR_factor[j]
      
      effects <- c(
        calculate_effect_model3(sex, boot_model, SRI_medians[1], SRI_medians[2]),
        calculate_effect_model3(sex, boot_model, SRI_medians[1], SRI_medians[3]),
        calculate_effect_model3(sex, boot_model, SRI_medians[1], SRI_medians[4]),
        calculate_effect_model3(sex, boot_model, SRI_medians[1], SRI_medians[5])
      )
      effects_list[[j]] <- data.frame(Sex = sex, t(effects))
    }
    results[[i]] <- do.call(rbind, effects_list)
  }
  return(results)
}

# Berechnen
tic()
bootstrap_results_model3 <- bootstrap_effects_model3(df_an1_imputed, n_boot = 100, SRI_medians)
toc() # 12s

# Combine and format
bootstrap_combined_model3 <- do.call(rbind, bootstrap_results_model3)
colnames(bootstrap_combined_model3)[2:5] <- c("Q1Q2", "Q1Q3", "Q1Q4", "Q1Q5")

formatted_results_model3 <- bootstrap_combined_model3 %>%
  dplyr::group_by(Sex) %>%
  dplyr::summarize(
    `Q1 → Q2` = paste0(
      round(mean(Q1Q2), 3), " (", round(quantile(Q1Q2, 0.025), 3), "; ", round(quantile(Q1Q2, 0.975), 3), ")"
    ),
    `Q1 → Q3` = paste0(
      round(mean(Q1Q3), 3), " (", round(quantile(Q1Q3, 0.025), 3), "; ", round(quantile(Q1Q3, 0.975), 3), ")"
    ),
    `Q1 → Q4` = paste0(
      round(mean(Q1Q4), 3), " (", round(quantile(Q1Q4, 0.025), 3), "; ", round(quantile(Q1Q4, 0.975), 3), ")"
    ),
    `Q1 → Q5` = paste0(
      round(mean(Q1Q5), 3), " (", round(quantile(Q1Q5, 0.025), 3), "; ", round(quantile(Q1Q5, 0.975), 3), ")"
    )
  )

# Create flextable
ft_model3 <- formatted_results_model3 %>%
  flextable() %>%
  set_header_labels(
    Sex = "Sex",
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  add_header_row(
    values = c("Multiplicative Effects of SRI on BMI", "By Sex for Each Quintile Comparison"),
    colwidths = c(1, 4)
  )

ft_model3


# __Model 4 (model 6 svy)-------

# Multiplicative effects of SRI on BMI by SRI-Quintiles
# Define combinations of ethnicity and sex
combinations <- expand.grid(
  RIDRETH3_factor = unique(df_an1_imputed$RIDRETH3_factor),
  RIAGENDR_factor = unique(df_an1_imputed$RIAGENDR_factor)
)

# Function to calculate the multiplicative effect of SRI for any two quintiles
calculate_effect <- function(ethnicity, sex, model, SRI_start, SRI_end) {
  # Set fixed values for all covariates
  fixed_values <- data.frame(
    RIDRETH3_factor = factor(ethnicity, levels = levels(df_an1_imputed$RIDRETH3_factor)),
    RIAGENDR_factor = factor(sex, levels = levels(df_an1_imputed$RIAGENDR_factor)),
    RIDAGEYR = mean(df_an1_imputed$RIDAGEYR, na.rm = TRUE),
    DMDEDUC2_factor = factor(names(sort(table(df_an1_imputed$DMDEDUC2_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$DMDEDUC2_factor)),
    INDHHIN2_factor = factor(names(sort(table(df_an1_imputed$INDHHIN2_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$INDHHIN2_factor)),
    OCD150_factor = factor(names(sort(table(df_an1_imputed$OCD150_factor), decreasing = TRUE))[1],
                           levels = levels(df_an1_imputed$OCD150_factor)),
    alcohol_consumption = factor(names(sort(table(df_an1_imputed$alcohol_consumption), decreasing = TRUE))[1],
                                 levels = levels(df_an1_imputed$alcohol_consumption)),
    smoking_status = factor(names(sort(table(df_an1_imputed$smoking_status), decreasing = TRUE))[1],
                            levels = levels(df_an1_imputed$smoking_status)),
    LBXVIDMS = mean(df_an1_imputed$LBXVIDMS, na.rm = TRUE),
    DR1TKCAL_winsorized = mean(df_an1_imputed$DR1TKCAL_winsorized, na.rm = TRUE),
    Depression_score_PHQ_9 = mean(df_an1_imputed$Depression_score_PHQ_9, na.rm = TRUE),
    DMDMARTL_factor = factor(names(sort(table(df_an1_imputed$DMDMARTL_factor), decreasing = TRUE))[1],
                             levels = levels(df_an1_imputed$DMDMARTL_factor)),
    activity_level = mean(df_an1_imputed$activity_level, na.rm = TRUE),
    SRI_no_imputation_winsorized_rand = SRI_start  # Start with SRI at the first quintile
  )
  
  # Predict log(BMI) for the start quintile
  logBMI_start <- predict(model, newdata = fixed_values, type = "response")
  
  # Update SRI to the end quintile and predict again
  fixed_values$SRI_no_imputation_winsorized_rand <- SRI_end
  logBMI_end <- predict(model, newdata = fixed_values, type = "response")
  
  # Calculate and return the multiplicative effect
  exp(logBMI_end - logBMI_start)
}

# Define the SRI medians for each quintile
SRI_medians <- df_an1_imputed %>%
  mutate(SRI_quintile = ntile(SRI_no_imputation_winsorized_rand, 5)) %>%
  dplyr::group_by(SRI_quintile) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation_winsorized_rand, na.rm = TRUE)) %>%
  pull(median_SRI)

# Bootstrap function for model6_svy
bootstrap_effects <- function(data, n_boot = 1000, SRI_medians) {
  results <- list()
  for (i in 1:n_boot) {
    # Resample data
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    boot_sample$RIAGENDR_factor <- factor(boot_sample$RIAGENDR_factor, levels = levels(data$RIAGENDR_factor))
    boot_sample$RIDRETH3_factor <- factor(boot_sample$RIDRETH3_factor, levels = levels(data$RIDRETH3_factor))
    
    # Recreate survey design
    boot_design <- svydesign(
      id = ~SDMVPSU,
      strata = ~SDMVSTRA,
      weights = ~WTMEC4YR,
      nest = TRUE,
      data = boot_sample
    )
    
    # Refit the model
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
    
    # Calculate effects for each combination of ethnicity and sex
    effects_list <- list()
    for (j in 1:nrow(combinations)) {
      ethnicity <- combinations$RIDRETH3_factor[j]
      sex <- combinations$RIAGENDR_factor[j]
      
      effects <- c(
        calculate_effect(ethnicity, sex, boot_model, SRI_medians[1], SRI_medians[2]),
        calculate_effect(ethnicity, sex, boot_model, SRI_medians[1], SRI_medians[3]),
        calculate_effect(ethnicity, sex, boot_model, SRI_medians[1], SRI_medians[4]),
        calculate_effect(ethnicity, sex, boot_model, SRI_medians[1], SRI_medians[5])
      )
      effects_list[[j]] <- data.frame(Ethnicity = ethnicity, Sex = sex, t(effects))
    }
    results[[i]] <- do.call(rbind, effects_list)
  }
  return(results)
}

tic()
bootstrap_results <- bootstrap_effects(df_an1_imputed, n_boot = 100, SRI_medians)
toc() # 34s

bootstrap_combined <- do.call(rbind, bootstrap_results)
colnames(bootstrap_combined)[3:6] <- c("Q1Q2", "Q1Q3", "Q1Q4", "Q1Q5")

formatted_results_model4 <- bootstrap_combined %>%
  dplyr::group_by(Ethnicity, Sex) %>%
  dplyr::summarize(
    `Q1 → Q2` = paste0(
      round(mean(Q1Q2), 3), " (", round(quantile(Q1Q2, 0.025), 3), "; ", round(quantile(Q1Q2, 0.975), 3), ")"
    ),
    `Q1 → Q3` = paste0(
      round(mean(Q1Q3), 3), " (", round(quantile(Q1Q3, 0.025), 3), "; ", round(quantile(Q1Q3, 0.975), 3), ")"
    ),
    `Q1 → Q4` = paste0(
      round(mean(Q1Q4), 3), " (", round(quantile(Q1Q4, 0.025), 3), "; ", round(quantile(Q1Q4, 0.975), 3), ")"
    ),
    `Q1 → Q5` = paste0(
      round(mean(Q1Q5), 3), " (", round(quantile(Q1Q5, 0.025), 3), "; ", round(quantile(Q1Q5, 0.975), 3), ")"
    )
  )

# Create the flextable with updated values
ft_model4 <- formatted_results_model4 %>%
  flextable() %>%
  set_header_labels(
    Ethnicity = "Ethnicity",
    Sex = "Sex",
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  add_header_row(
    values = c("Multiplicative Effects of SRI on BMI", "By Ethnicity and Sex for Each Quintile Comparison"),
    colwidths = c(2, 4)
  )

ft_model4

# Combine 4 tables--------
# TODO
# Add Ethnicity and Sex for Model 3
formatted_results_model3 <- formatted_results_model3 %>%
  mutate(
    Model = "Model 3"
  )

# Ensure consistent columns for Model 1 and Model 2
formatted_results_model1 <- formatted_results_model1 %>%
  mutate(
    Model = "Model 1",
    Ethnicity = NA,  # Add empty columns for Ethnicity and Sex
    Sex = NA
  )

formatted_results_model2 <- formatted_results_model2 %>%
  mutate(
    Model = "Model 2",
    Ethnicity = NA,
    Sex = NA
  )

# Add Model 4 information
formatted_results_model4 <- formatted_results_model4 %>%
  mutate(
    Model = "Model 4"
  )

# Arrange columns properly for model 4
formatted_results_model4 <- formatted_results_model4 %>%
  dplyr::select(Model, Ethnicity, Sex, `Q1 → Q2`, `Q1 → Q3`, `Q1 → Q4`, `Q1 → Q5`)

# Combine all models (1, 2, 3, 4)
combined_results <- bind_rows(
  formatted_results_model1,
  formatted_results_model2,
  formatted_results_model3,
  formatted_results_model4
)

# Final arrangement
combined_results <- combined_results %>%
  dplyr::select(Model, Ethnicity, Sex, `Q1 → Q2`, `Q1 → Q3`, `Q1 → Q4`, `Q1 → Q5`)


# Arrange columns properly
combined_results <- combined_results %>%
  dplyr::select(Model, Ethnicity, Sex, `Q1 → Q2`, `Q1 → Q3`, `Q1 → Q4`, `Q1 → Q5`)

# Create the final flextable
ft_combined <- combined_results %>%
  flextable() %>%
  set_header_labels(
    Model = "Model",
    Ethnicity = "Ethnicity",
    Sex = "Sex",
    `Q1 → Q2` = "Q1 → Q2",
    `Q1 → Q3` = "Q1 → Q3",
    `Q1 → Q4` = "Q1 → Q4",
    `Q1 → Q5` = "Q1 → Q5"
  ) %>%
  autofit() %>%
  merge_v(j = "Model") %>%
  merge_v(j = "Ethnicity") %>%
  merge_v(j = "Sex") %>%
  theme_vanilla() %>%
  add_footer_row(
    values = "Parentheses (x; y) represent 95% confidence intervals.",
    colwidths = 7
  )

ft_combined

# Export to Word document
doc <- read_docx() %>%
  body_add_flextable(ft_combined)

# Save the Word document
(current_directory <- getwd())
(parent_directory <- dirname(current_directory))
output_path <- file.path(paste0(parent_directory, "/RESULTS/Tables/Table3_1.7.26.docx"))
print(doc, target = output_path)

output_path <- file.path(paste0(parent_directory, "/RESULTS/Tables/Table3_1.7.26.RDS"))
saveRDS(
  ft_combined,
  file = output_path
)

# save as html
save_as_html(
  x = ft_combined,
  path = "./RESULTS/Tables/Table3_new_26.4.25.html"
)
