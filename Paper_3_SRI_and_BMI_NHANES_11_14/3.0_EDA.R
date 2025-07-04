# INFO: EDA is done without using weights but in the analytic sample (SRI available, Age range, ....)----

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("1_Packages_install_load.R")


# Read ANALYTIC data set from 3_Main_Analysis...----
df_an <- readRDS(".../4_DATA/Combined_data_sets_2_waves_analytic_data_set.RDS")
str(df_an)
dim(df_an) # 7085  check
#sum(!is.na(df_an$SRI_no_imputation))

df <- df_an # easier handling

# 1) DAG(s)----

dag <- dagitty::dagitty('dag {
"Vitamin D" [pos="-1.411,0.366"]
Age [pos="-0.460,1.185"]
Alcohol [pos="-1.823,0.718"]
BMI [outcome,pos="1.400,1.621"]
CaloricIntake [pos="-1.389,0.292"]
Depression [pos="-0.690,0.823"]
Education [pos="-1.574,0.863"]
Ethnicity [pos="-0.572,1.428"]
H.H.income [pos="-1.456,1.102"]
Mar.Status [pos="-0.318,1.005"]
Occupation [pos="-1.551,0.983"]
Phys.Activity [pos="-1.449,0.188"]
SRI [exposure,pos="-2.200,1.597"]
Sex [pos="-0.524,1.316"]
Smoking [pos="-1.852,0.574"]
"Vitamin D" -> BMI
"Vitamin D" -> SRI
Age -> BMI
Age -> Depression
Age -> Mar.Status
Age -> SRI
Age -> Smoking
Alcohol -> BMI
Alcohol -> SRI
CaloricIntake -> BMI
CaloricIntake -> SRI
Depression -> BMI
Depression -> SRI
Education -> BMI
Education -> SRI
Ethnicity -> BMI
Ethnicity -> SRI
Ethnicity -> Smoking
H.H.income -> BMI
H.H.income -> SRI
Mar.Status -> BMI
Mar.Status -> SRI
Occupation -> BMI
Occupation -> SRI
Phys.Activity -> BMI
Phys.Activity -> SRI
SRI -> BMI
Sex -> BMI
Sex -> Depression
Sex -> SRI
Smoking -> BMI
Smoking -> SRI
}')

ggdag(dag, text = FALSE) + 
  theme_dag() + 
  geom_dag_node(size = 0) +  # This increases the size of the bubbles
  geom_dag_text(size = 2)    # Adjust text size for readability

dagitty::impliedConditionalIndependencies(dag)
dagitty::adjustmentSets(dag, exposure = "SRI", outcome = "BMI") 
adjSet <- dagitty::adjustmentSets(dag, exposure = "SRI", outcome = "BMI") 
length(adjSet$`1`)  # 13 if no interaction terms
  
# 1.1) DAG with mealtiming difference variables--------
dag1.1 <- dagitty::dagitty('dag {
"Vitamin D" [pos="-1.338,0.395"]
Age [pos="-0.460,1.185"]
Alcohol [pos="-1.823,0.718"]
BMI [outcome,pos="1.400,1.621"]
CaloricIntake [pos="-1.389,0.292"]
Depression [pos="-0.690,0.823"]
Education [pos="-1.574,0.863"]
Ethnicity [pos="-1.208,1.197"]
H.H.income [pos="-1.456,1.102"]
Mar.Status [pos="-0.318,1.005"]
Occupation [pos="-1.551,0.983"]
Phys.Activity [pos="-1.449,0.188"]
SRI [exposure,pos="-2.200,1.597"]
Sex [pos="-0.524,1.316"]
Smoking [pos="-1.852,0.574"]
latest_meal_difference [pos="-0.702,0.431"]
"Vitamin D" -> BMI
"Vitamin D" -> SRI
Age -> BMI
Age -> Depression
Age -> Mar.Status
Age -> SRI
Age -> Smoking
Alcohol -> BMI
Alcohol -> SRI
CaloricIntake -> BMI
CaloricIntake -> SRI
Depression -> BMI
Depression -> SRI
Education -> BMI
Education -> SRI
Ethnicity -> BMI
Ethnicity -> SRI
Ethnicity -> Smoking
Ethnicity -> latest_meal_difference
H.H.income -> BMI
H.H.income -> SRI
Mar.Status -> BMI
Mar.Status -> SRI
Occupation -> BMI
Occupation -> SRI
Occupation -> latest_meal_difference
Phys.Activity -> BMI
Phys.Activity -> SRI
SRI -> BMI
Sex -> BMI
Sex -> Depression
Sex -> SRI
Smoking -> BMI
Smoking -> SRI
latest_meal_difference -> BMI
latest_meal_difference -> SRI
}
')
ggdag(dag1.1, text = FALSE) + 
  theme_dag() + 
  geom_dag_node(size = 0) +  # This increases the size of the bubbles
  geom_dag_text(size = 2)    # Adjust text size for readability

dagitty::impliedConditionalIndependencies(dag1.1)
dagitty::adjustmentSets(dag1.1, exposure = "SRI", outcome = "BMI") 


# 2) Raw associations, plots-------

# SRI and BMI within gender and ethnicity ----
# MESA study: lower SRI associated with higher BMI
# Which direction is the relationship causally, if anything?
df %>% 
  mutate(variable_for_x_axis = SRI_no_imputation) %>%
  filter(variable_for_x_axis > -10) %>%
  filter(BMXBMI < 80) %>%
  #filter(SRI_no_imputation > 0) %>%
  #filter(RIAGENDR == 2) %>% # 1=Male/2=Female -> cor = -0.07 / Female -> cor = -0.19
  #filter(RIDRETH3 == 3) %>% # 1=Mexican American/2=Other Hispanic/3=Non-Hispanic White/4=Non Hispanic Black/6=Non-Hispanic Asian/7=Other Race incl Multi-Racial
  dplyr::select(variable_for_x_axis, SRI_no_imputation, SRI_maximize, SRI_minimize, BMXBMI, RIAGENDR, RIDRETH3) %>%
  na.omit() %>% # only complete cases for now
  ggplot(aes(x = variable_for_x_axis, y = BMXBMI, colour = RIAGENDR)) +
  #ggplot(aes(x = variable_for_x_axis, y = BMXBMI, colour = RIDRETH3)) +
  geom_point() +  # Plot the SRI_no_imputation vs BMI in black
  #geom_point(aes(x = SRI_maximize, y = BMXBMI), color = "lightgreen", size = 0.3) +
  #geom_point(aes(x = SRI_minimize, y = BMXBMI), color = "lightblue", size = 0.3) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", after_stat(r.label))), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", after_stat(r.label))), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue", vjust = 4) +
  labs(x = "SRI", y = "BMI") +
  #ggtitle("SRI Values (no imputation / max(green) / min(lightblue)") +
  ggtitle("SRI vs. BMI") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# One could color the points for subgroup analysis later (Ethnicity and Sex sub groups according to the current title)

cor.test(df$SRI_no_imputation, df$BMXBMI, method = "pearson") 
cor.test(df$SRI_no_imputation, df$BMXBMI, method = "spearman") # -0.1284247 
cor.test(df$SRI_maximize, df$BMXBMI, method = "spearman") # -0.1270902
cor.test(df$SRI_minimize, df$BMXBMI, method = "spearman") # -0.1352907
# The good news is, that this does not change much
# The results are very consistent with: Validation of the Sleep Regularity Index in Older Adults and Associations with Cardiometabolic Risk

# _Bootstrapp CIs for rho----
spearman_corr <- function(data, indices) {
  d <- data[indices, ]
  cor_value <- try(cor(d$SRI_no_imputation, d$BMXBMI, method = "spearman"), silent = TRUE)
  
  if (inherits(cor_value, "try-error") || is.na(cor_value)) {
    return(NA)  # Return NA if the correlation can't be computed
  } else {
    return(cor_value)
  }
}
data <- na.omit(data.frame(SRI_no_imputation = df$SRI_no_imputation, 
                           BMXBMI = df$BMXBMI,
                           RIAGENDR = df$RIAGENDR,
                           RIDRETH3 = df$RIDRETH3)) %>% 
  filter(SRI_no_imputation > -10) %>%
  filter(RIAGENDR == 2) %>% # 1=Male/2=Female -> cor = -0.07 / Female -> cor = -0.19
  filter(RIDRETH3 == 2) %>% # 1=Mexican American/2=Other Hispanic/3=Non-Hispanic White/4=Non Hispanic Black/6=Non-Hispanic Asian/7=Other Race incl Multi-Racial
  #filter(SRI_no_imputation > 25) %>%
  #filter(BMXBMI < 60) %>%
  #filter(SRI_no_imputation > 0) %>%
  filter(RIAGENDR == 2) # Male/Female
tic()
results <- boot(data = data, statistic = spearman_corr, R = 2000)
toc() # 6s
boot.ci(results, type = "perc", conf = 0.99)

# Male: (-0.1149, -0.0240)
# Female: (-0.2277, -0.1435)  
# most extreme possibly in White Females: (-0.2770, -0.1528); r=-0.21
# also in Non-Hispanic Asian Females: (-0.3016, -0.0512); r=-0.18
# also in Other Race incl Multi-Racial: (-0.5131, -0.0651 ) ; r=-0.28

# _Mutual information-----
sub <- na.omit(df %>% dplyr::select(SRI_no_imputation, BMXBMI))
sub$SRI_no_imputation <- infotheo::discretize(sub$SRI_no_imputation, disc = "equalwidth")
sub$BMXBMI <- infotheo::discretize(sub$BMXBMI, disc = "equalwidth")
mutinformation(sub$SRI_no_imputation, sub$BMXBMI, method = "emp") #0.02
mutinformation(sub$SRI_no_imputation, sub$SRI_no_imputation, method = "emp") # ~ 2
mutinformation(sub$BMXBMI, sub$BMXBMI, method = "emp") # ~ 2
# -> low mutual information


# SRI and Age----
hist(df$RIDAGEYR)
df %>% 
  filter(RIDAGEYR >= 20) %>% 
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  #filter(RIAGENDR == 2) %>% # 1=Male/2=Female 
  #filter(RIDRETH3 == 7) %>% # 1=Mexican American/2=Other Hispanic/3=Non-Hispanic White/4=Non Hispanic Black/6=Non-Hispanic Asian/7=Other Race incl Multi-Racial
  dplyr::select(SRI_no_imputation, RIDAGEYR) %>%
  na.omit() %>% # only complete cases for now
  ggplot(aes(x = RIDAGEYR, y = SRI_no_imputation)) +
  geom_point(color = "black", alpha = 0.8) +  
  #geom_smooth(method = "lm",color = "red", se = TRUE) +
  geom_smooth(method = "loess",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "Age (years)", y = "SRI") +
  theme_minimal()

# nominally, there is not so much to see: -0.1 SRI per year
# similar in women/men: -0.09/-0.11
# all subgroups is about the same

# Create age groups in 5-year intervals and plot boxplots
df %>% 
  filter(RIDAGEYR >= 20) %>%
  filter(SRI_no_imputation > -10) %>%
  #filter(RIAGENDR == 2) %>% # 1=Male/2=Female 
  #filter(RIDRETH3 == 7) %>% # 1=Mexican American/2=Other Hispanic/3=Non-Hispanic White/4=Non Hispanic Black/6=Non-Hispanic Asian/7=Other Race incl Multi-Racial
  dplyr::select(SRI_no_imputation, RIDAGEYR) %>%
  na.omit() %>%
  mutate(AgeGroup = cut(RIDAGEYR, breaks = seq(20, 85, by = 5), right = FALSE, include.lowest = TRUE)) %>% 
  ggplot(aes(x = AgeGroup, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by 5-Year Age Groups",
       x = "Age Group (years)",
       y = "SRI Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# this looks like a somewhat curved relationship

# The direction of the relationship is inverse to Dorothees
# presentation: https://youtu.be/b6fZ-5GO2RA?si=wkz68JAHSwibDVGw&t=308

# Raw correlations and plots-
#numeric_vars <- df %>% dplyr::select(SRI_no_imputation, BMXBMI, RIDAGEYR) %>% na.omit()
#cor_matrix <- cor(numeric_vars, use = "complete.obs")
#corrplot(cor_matrix, method = "color", addCoef.col = "black")

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(RIAGENDR == 2) %>%  # 1=Male/2=Female
  filter(RIDRETH3 == 2) %>%  # Other Race incl Multi-Racial
  dplyr::select(SRI_no_imputation, BMXBMI, RIDAGEYR) %>%
ggpairs(upper = list(continuous = wrap("cor", size = 4)), 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        title = "Pairwise Relationships between SRI, BMI, and Age in Selected Subgroup") + 
  theme(plot.title = element_text(hjust = 0.5))
  

# SRI and Sex----
# According to Dorothee not clear...
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = as.factor(RIAGENDR), y = SRI_no_imputation)) + # 1=Male, 2=Female
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Gender (1=Male, 2=Female)", y = "SRI_no_imputation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# 3 points difference; clinically relevant?
  
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR)) %>%
  group_by(RIAGENDR) %>%
  dplyr::summarise(median_SRI = median(SRI_no_imputation)) # 3 points difference; clinically relevant?
  
# SRI maximize
df %>%
  filter(SRI_maximize > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR)) %>%
  group_by(RIAGENDR) %>%
  dplyr::summarise(median_SRI = median(SRI_maximize)) # 2.5 points difference; clinically relevant?

# SRI minimize  
df %>%
  filter(SRI_minimize > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR)) %>%
  group_by(RIAGENDR) %>%
  dplyr::summarise(median_SRI = median(SRI_minimize)) # 4 points difference; clinically relevant?

# Differences range from 2.5-4 points depending what kind of imputation (if any) is used.
# What is a clinically important difference?

# SRI and Marital status------
marital_status_labels <- c(
  "1" = "Married",
  "2" = "Widowed",
  "3" = "Divorced",
  "4" = "Separated",
  "5" = "Never Married",
  "6" = "Living with Partner",
  "77" = "Refused",
  "99" = "Don't Know"
)
# SRI and Marital Status with labels
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = as.factor(DMDMARTL), y = SRI_no_imputation)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Marital Status", y = "SRI_no_imputation") +
  scale_x_discrete(labels = marital_status_labels) +  # Apply labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians

# SRI and Ethnicity----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#RIDRETH3
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm#RIDRETH3
custom_labels <- c("1" = "Mexican American", 
                   "2" = "Other Hispanic", 
                   "3" = "Non-Hispanic White", 
                   "4" = "Non-Hispanic Black", 
                   "6" = "Non-Hispanic Asian", 
                   "7" = "Other Race - Including Multi-Racial")
medians <- df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  group_by(RIDRETH3) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation))
median_difference <- max(medians$median_SRI) - min(medians$median_SRI)

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIDRETH3 = factor(RIDRETH3, 
                           levels = names(sort(tapply(SRI_no_imputation, RIDRETH3, median), decreasing = FALSE)))) %>%
  ggplot(aes(x = RIDRETH3, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = custom_labels) +
  labs(title = "SRI_no_imputation by Race/Hispanic Origin",
       x = "",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$SRI_no_imputation, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians (largest minus smallest):", round(median_difference, 2)),  #9.46 diff
           color = "red")


medians_maximize <- df %>%
  filter(SRI_maximize > -10) %>%
  filter(BMXBMI < 60) %>%
  group_by(RIDRETH3) %>%
  dplyr::summarize(median_SRI = median(SRI_maximize))
median_difference_maximize <- max(medians_maximize$median_SRI) - min(medians_maximize$median_SRI)

df %>%
  filter(SRI_maximize > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIDRETH3 = factor(RIDRETH3, 
                           levels = names(sort(tapply(SRI_maximize, RIDRETH3, median), decreasing = FALSE)))) %>%
  ggplot(aes(x = RIDRETH3, y = SRI_maximize)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = custom_labels) +
  labs(title = "SRI_maximize by Race/Hispanic Origin",
       x = "Race/Hispanic Origin",
       y = "SRI_maximize") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$SRI_maximize, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians (largest minus smallest):", round(median_difference_maximize, 2)), # 9.21
           color = "red")


medians_minimize <- df %>%
  filter(SRI_minimize > -10) %>%
  filter(BMXBMI < 60) %>%
  group_by(RIDRETH3) %>%
  dplyr::summarize(median_SRI = median(SRI_minimize))
median_difference_minimize <- max(medians_minimize$median_SRI) - min(medians_minimize$median_SRI)

df %>%
  filter(SRI_minimize > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIDRETH3 = factor(RIDRETH3, 
                           levels = names(sort(tapply(SRI_minimize, RIDRETH3, median), decreasing = FALSE)))) %>%
  ggplot(aes(x = RIDRETH3, y = SRI_minimize)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = custom_labels) +
  labs(title = "SRI_minimize by Race/Hispanic Origin",
       x = "Race/Hispanic Origin",
       y = "SRI_minimize") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$SRI_minimize, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians (largest minus smallest):", round(median_difference_minimize, 2)), # 8.8 diff
           color = "red")

# Median differences between the largest and smallest SRI group with regards to ethnicity range 
# from 8.8 to 9.46


# SRI and Sex+Ethnicity----

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(RIDRETH3 = factor(RIDRETH3, 
                           levels = names(sort(tapply(SRI_no_imputation, RIDRETH3, median), decreasing = FALSE))),
         RIAGENDR = factor(RIAGENDR, labels = c("Male", "Female"))) %>%
  ggplot(aes(x = RIDRETH3, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = custom_labels) +
  labs(title = "SRI_no_imputation by Race/Hispanic Ethnicity and Sex",
       x = "",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(~ RIAGENDR)


# SRI and household income----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm#INDHHIN2
income_labels <- c(
  "1" = "$0 to $4,999",
  "2" = "$5,000 to $9,999",
  "3" = "$10,000 to $14,999",
  "4" = "$15,000 to $19,999",
  "5" = "$20,000 to $24,999",
  "6" = "$25,000 to $34,999",
  "7" = "$35,000 to $44,999",
  "8" = "$45,000 to $54,999",
  "9" = "$55,000 to $64,999",
  "10" = "$65,000 to $74,999",
  "12" = "$20,000 and Over",
  "13" = "Under $20,000",
  "14" = "$75,000 to $99,999",
  "15" = "$100,000 and Over",
  "77" = "Refused",
  "99" = "Don't know"
)

medians <- df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!INDHHIN2 %in% c("77", "99", ".")) %>%
  filter(!is.na(INDHHIN2)) %>%
  group_by(INDHHIN2) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation))
median_difference <- max(medians$median_SRI) - min(medians$median_SRI)

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!INDHHIN2 %in% c("77", "99", ".")) %>%
  filter(!is.na(INDHHIN2)) %>%
  mutate(INDHHIN2 = factor(INDHHIN2, levels = names(sort(tapply(SRI_no_imputation, INDHHIN2, median), decreasing = FALSE)))) %>%
  ggplot(aes(x = INDHHIN2, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = income_labels) +
  labs(title = "SRI_no_imputation by Household Income",
       x = "Household Income",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$SRI_no_imputation, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians:", round(median_difference, 2)), # diff 12.38
           color = "red")

# There are non-trivial differences regarding the household income


# SRI and depression----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DPQ_G.htm
# Custom labels for feeling down, depressed, or hopeless levels
depression_labels <- c(
  "0" = "Not at all",
  "1" = "Several days",
  "2" = "More than half the days",
  "3" = "Nearly every day"
)
df_filtered <- df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!DPQ020 %in% c("7", "9", ".")) %>%  # Exclude 'Refused', 'Don't Know', and 'Missing'
  filter(!is.na(DPQ020)) %>%
  mutate(DPQ020 = factor(DPQ020, levels = names(sort(tapply(SRI_no_imputation, DPQ020, median), decreasing = FALSE))))

medians <- df_filtered %>%
  group_by(DPQ020) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation))
median_difference <- max(medians$median_SRI) - min(medians$median_SRI)

ggplot(df_filtered %>% filter(!is.na(DPQ020)), aes(x = DPQ020, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = depression_labels) +
  labs(title = "SRI_no_imputation by Feeling Down, Depressed, or Hopeless",
       x = "",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df_filtered$SRI_no_imputation, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians:", round(median_difference, 2)), # 11 points difference
           color = "red")

# _Depression score: ----
# Depression was measured using the Patient Health 
# Questionnaire (PHQ-9), a nine-item screening instrument that asked questions
# about the frequency of symptoms of depression over the past 2 weeks. 

model <- df %>% 
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
  filter(!is.na(Depression_score_PHQ_9)) %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  lm(SRI_no_imputation ~ Depression_score_PHQ_9, data = .)
adj_r_squared <- summary(model)$adj.r.squared

df %>% 
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
  filter(!is.na(Depression_score_PHQ_9)) %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = Depression_score_PHQ_9, y = SRI_no_imputation)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(color = "red", se = TRUE) +  # Add a regression line
  stat_cor(method = "pearson", aes(label = paste(..r.label.., sep = "~`,`~")), 
           label.x = 20, label.y = 95) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 20, label.y = 85, aes(label = ..eq.label..)) +  # Add regression equation
  annotate("text", x = 20, y = 75, 
           label = paste("Adj. R² =", round(adj_r_squared, 3)), 
           color = "blue", size = 5, hjust = 0) +  # Add adjusted R² value
  labs(title = "Relationship between Depression Score (PHQ-9) and SRI_no_imputation",
       x = "Depression Score (PHQ-9)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Assuming 'Depression_score_PHQ_9' is your depression score variable
df %>%
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
  filter(!is.na(Depression_score_PHQ_9)) %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  mutate(Depression_quintile = ntile(Depression_score_PHQ_9, 5)) %>%  # Divide into quintiles
  ggplot(aes(x = as.factor(Depression_quintile), y = SRI_no_imputation)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Depression Score Quintiles", y = "SRI_no_imputation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# shows a nice downward trend

# Interpretation
# Level of Depression Severity, PHQ-9 Score
# Minimal, 0–4
# Mild, 5–9
# Moderate, 10–14
# Moderately severe, 15–19
# Severe, 20–27

# SRI and blood pressure/Hypertension, cholesterol----
# MESA Sleep Ancilliary study - lower SRI associated with higher hypertension
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.htm
intersect(colnames(df1112_blood_pressure), colnames(df1314_blood_pressure))

df_bp <- df %>%
  rowwise() %>%
  mutate(
    Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
    Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
  ) %>%
  ungroup()

df_bp <- df_bp %>%
  filter(!is.na(Avg_Systolic_BP) & !is.na(Avg_Diastolic_BP)) %>%
  filter(SRI_no_imputation > -10)

# Systolic
ggplot(df_bp, aes(x = Avg_Systolic_BP, y = SRI_no_imputation)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method="lm",se = TRUE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Average Systolic Blood Pressure",
       x = "Average Systolic Blood Pressure (mm Hg)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = min(df_bp$Avg_Systolic_BP), label.y = max(df_bp$SRI_no_imputation)) +  # Add correlation coefficient
  stat_regline_equation(label.x = min(df_bp$Avg_Systolic_BP), label.y = max(df_bp$SRI_no_imputation) - 5)  # Add regression equation
# Slight downward trend visible...

# Diastolic
ggplot(df_bp, aes(x = Avg_Diastolic_BP, y = SRI_no_imputation)) +
  geom_point(color = "green", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Average Diastolic Blood Pressure",
       x = "Average Diastolic Blood Pressure (mm Hg)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = min(df_bp$Avg_Diastolic_BP), label.y = max(df_bp$SRI_no_imputation)) +  # Add correlation coefficient
  stat_regline_equation(label.x = min(df_bp$Avg_Diastolic_BP), label.y = max(df_bp$SRI_no_imputation) - 5)  # Add regression equation
# Not so much trend here...




# SRI and diabetes----
# MESA study: lower SRI higher HbA1c and fasting glucose levels
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.htm
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(DIQ010)) %>%  # Ensure NAs are removed before factor conversion
  mutate(DIQ010 = factor(DIQ010, 
                         levels = c(1, 3, 2),  # Order as Yes, Borderline, No
                         labels = c("Yes", "Borderline", "No"))) %>%
  droplevels() %>%
  dplyr::select(DIQ010, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = DIQ010, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_jitter(width = 0.1, color = "blue", alpha = 0.05) +  # Add jittered points
  labs(title = "SRI_no_imputation by Diabetes Diagnosis",
       x = "Doctor Told You Have Diabetes",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# -> This seems very clear, 10 points median difference Yes/no

# SRI and Smoking-----
df %>%
  filter(SRI_no_imputation > -10) %>%    # Filter for valid SRI values
  filter(BMXBMI < 60) %>%                 # Filter for valid BMI values
  filter(SMQ040 %in% c(1, 2, 3)) %>%      # Filter for specific smoking status (1, 2, 3)
  mutate(SMQ040 = factor(SMQ040, 
                         levels = c(1, 2, 3), 
                         labels = c("Every day", "Some days", "Not at all"))) %>%
  droplevels() %>%
  dplyr::select(SMQ040, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = SMQ040, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Smoking Status",
       x = "Current Smoking Status",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# nice, 5 points lower for everyday and some days smokers
# Could be related to smoking while going out and when going out, this - by definition - 
# could reduce sleep regularity

# Another smoking-variable: Avg # cigarettes/day during past 30 days
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(SMD650 %in% 2:90) %>%     # Include only relevant continuous values (2-90)
  filter(!is.na(SMD650)) %>%       # Exclude missing values
  droplevels() %>%
  dplyr::select(SMD650, SRI_no_imputation) %>%
  na.omit() %>%
  filter(SMD650 <= 25) %>% # ignore the very heavy smokers
  ggplot(aes(x = SMD650, y = SRI_no_imputation)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Avg # Cigarettes/Day",
       x = "Avg # Cigarettes/Day (past 30 days)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 5, label.y = max(df$SRI_no_imputation, na.rm = TRUE) - 5) +  # Pearson correlation
  annotate("text", x = Inf, y = -Inf, label = "Linear Regression", hjust = 2, vjust = -10, color = "darkred", size = 4)

# Calculate quintiles
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(SMD650 %in% 2:90) %>%     # Include only relevant continuous values (2-90)
  filter(!is.na(SMD650)) %>%       # Exclude missing values
  mutate(SMD650_quintile = ntile(SMD650, 5)) %>%  # Divide into quintiles
  ggplot(aes(x = as.factor(SMD650_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "SRI_no_imputation by Smoking Quintiles", 
       x = "Smoking Quintiles", 
       y = "SRI_no_imputation") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# no differences visible

# SRI and alcohol------
df %>% # Avg # alcoholic drinks/day - past 12 mos
  filter(ALQ130 %in% 1:82) %>%       # Include only relevant values (1 to 82)
  filter(!is.na(ALQ130)) %>%         # Exclude missing values
  droplevels() %>%
  dplyr::select(ALQ130, SRI_no_imputation) %>%
  na.omit() %>%
  filter(ALQ130 < 80) %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = ALQ130, y = SRI_no_imputation)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(se = TRUE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Avg # Alcoholic Drinks/Day - Past 12 Months",
       x = "Avg # Alcoholic Drinks/Day",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 5, label.y = max(df$SRI_no_imputation, na.rm = TRUE) - 5)
# -> Trend visible
# How many data points?

# Create quintiles and visualize
df %>%
  filter(ALQ130 %in% 1:82) %>%       # Include only relevant values (1 to 82)
  filter(!is.na(ALQ130)) %>%         # Exclude missing values
  filter(ALQ130 < 80) %>%
  filter(SRI_no_imputation > -10) %>%
  mutate(ALQ130_quintile = ntile(ALQ130, 5)) %>%  # Create quintiles
  ggplot(aes(x = as.factor(ALQ130_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(aes(color = as.factor(ALQ130_quintile)), width = 0.2, alpha = 0.5) +  # Add jittered points
  labs(title = "SRI_no_imputation by Alcoholic Drinks/Day Quintiles", 
       x = "Alcoholic Drinks/Day Quintiles", 
       y = "SRI_no_imputation") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, color = "red") +  # Annotate the medians
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# slight downward trend visible
# 6 points median difference.

df %>% # Had at least 12 alcohol drinks/1 yr?
  filter(ALQ101 %in% c(1, 2)) %>%   # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(ALQ101)) %>%        # Exclude missing values
  mutate(ALQ101 = factor(ALQ101, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(ALQ101, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = ALQ101, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Having Had at Least 12 Alcoholic Drinks in 1 Year",
       x = "Had at Least 12 Alcoholic Drinks in 1 Year",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# No visible difference

df %>% # # days have 4/5 drinks - past 12 mos
  filter(ALQ151 %in% c(1, 2)) %>%   # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(ALQ151)) %>%        # Exclude missing values
  mutate(ALQ151 = factor(ALQ151, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(ALQ151, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = ALQ151, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Having 4/5 or More Drinks Every Day",
       x = "Ever Had 4/5 or More Drinks Every Day",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# Visible difference. It could be that the drinking days are behind,
# but some of it could still be present in the accelerometer-wearing time
# which could explain the lower SRI.

df %>%
  filter(ALQ110 %in% c(1, 2)) %>%   # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(ALQ110)) %>%        # Exclude missing values
  mutate(ALQ110 = factor(ALQ110, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(ALQ110, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = ALQ110, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Having Had at Least 12 Alcoholic Drinks in Lifetime",
       x = "Had at Least 12 Alcoholic Drinks in Lifetime",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# very small difference, but irrelevant





# SRI and drug use----
df %>% # Time since last used marijuana regularly
  filter(DUQ215Q %in% 0:245) %>%  # Include only relevant values (0 to 245)
  filter(!is.na(DUQ215Q)) %>%      # Exclude missing values
  droplevels() %>%
  dplyr::select(DUQ215Q, SRI_no_imputation) %>%
  na.omit() %>%
  filter(DUQ215Q < 50) %>%
  ggplot(aes(x = DUQ215Q, y = SRI_no_imputation)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(se = TRUE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Time Since Last Used Marijuana Regularly",
       x = "Time Since Last Used Marijuana Regularly (days)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 20, label.y = max(df$SRI_no_imputation, na.rm = TRUE) - 5) +  # Pearson correlation
  annotate("text", x = Inf, y = -Inf, label = "Linear Regression", hjust = 1.1, vjust = -0.1, color = "darkred", size = 4)

# Create quintiles and visualize
df %>%
  filter(DUQ215Q %in% 0:245) %>%  # Include only relevant values (0 to 245)
  filter(!is.na(DUQ215Q)) %>%      # Exclude missing values
  filter(DUQ215Q < 50) %>%
  filter(SRI_no_imputation > -10) %>%
  mutate(DUQ215Q_quintile = ntile(DUQ215Q, 5)) %>%  # Create quintiles
  ggplot(aes(x = as.factor(DUQ215Q_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(aes(color = as.factor(DUQ215Q_quintile)), width = 0.2, alpha = 0.5) +  # Add jittered points
  labs(title = "SRI_no_imputation by Quintiles of Time Since Last Used Marijuana",
       x = "Quintiles of Time Since Last Used Marijuana Regularly",
       y = "SRI_no_imputation") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, color = "red") +  # Annotate the medians
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# -> this seems interesting at first glance, lets restrict to less than 50 days.
# -> 10 points difference in medians
# cor 0.13/0.15
# There seems to be a slight trend...

df %>% # injected drugs?
  filter(DUQ370 %in% c(1, 2)) %>%  # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(DUQ370)) %>%       # Exclude missing values
  mutate(DUQ370 = factor(DUQ370, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(DUQ370, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = DUQ370, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_jitter(aes(color = DUQ370), width = 0.2, alpha = 0.5, size = 2) +  # Add jitter points for better visualization
  labs(title = "SRI_no_imputation by Ever Used a Needle to Inject Illegal Drugs",
       x = "Ever Used a Needle to Inject Illegal Drugs",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# Also a couple of SRI points in median lower, very low sample size in the injection group
# -> 7 points median difference, very few drug users... 

df %>%
  filter(DUQ370 %in% c(1, 2)) %>%  # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(DUQ370)) %>%       # Exclude missing values
  mutate(DUQ370 = factor(DUQ370, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(DUQ370, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = DUQ370, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(aes(color = DUQ370), width = 0.2, alpha = 0.5, size = 2) +  # Add jitter points for better visualization
  labs(title = "SRI_no_imputation by Ever Used a Needle to Inject Illegal Drugs",
       x = "Ever Used a Needle to Inject Illegal Drugs",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# This could warrant an exploratory significance test, but the sample size is very low.

# Perform the t-test
t_test_result <- t.test(SRI_no_imputation ~ DUQ370, data = df %>% filter(DUQ370 %in% c(1, 2)))

# Create the boxplot with jitter and annotated t-test result
df %>%
  filter(DUQ370 %in% c(1, 2)) %>%  # Include only relevant values (1 = Yes, 2 = No)
  filter(!is.na(DUQ370)) %>%       # Exclude missing values
  mutate(DUQ370 = factor(DUQ370, 
                         levels = c(1, 2), 
                         labels = c("Yes", "No"))) %>%  # Convert to factor with labels
  droplevels() %>%
  dplyr::select(DUQ370, SRI_no_imputation) %>%
  na.omit() %>%
  ggplot(aes(x = DUQ370, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(aes(color = DUQ370), width = 0.2, alpha = 0.5, size = 2) +  # Add jitter points for better visualization
  labs(title = "SRI_no_imputation by Ever Used a Needle to Inject Illegal Drugs",
       x = "Ever Used a Needle to Inject Illegal Drugs",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +
  annotate("text", x = 1.5, y = max(df$SRI_no_imputation, na.rm = TRUE) + 5, 
           label = paste0("p-value: ", round(t_test_result$p.value, 3)), 
           size = 5, color = "darkred")
# This seems rather clear nominally...

# SRI and coffee [only 2013/14]-----
df_cafe_plot <- left_join(df, df1314_cafe, by="SEQN")
df_cafe_plot %>% 
  filter(SRI_no_imputation > -10) %>%
  filter(!is.na(URXMX7)) %>%
  filter(URXMX7 < 100) %>%
ggplot(aes(x = URXMX7, y = SRI_no_imputation)) +
  geom_point(color = "purple", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a regression line
  labs(title = "SRI_no_imputation by Caffeine Concentration in Urine",
       x = "Caffeine Concentration in Urine (umol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Almost nothing visible....

df_cafe_plot <- left_join(df, df1314_cafe, by="SEQN")

# Create quintiles for the caffeine concentration
df_cafe_plot <- df_cafe_plot %>%
  filter(SRI_no_imputation > -10) %>%
  filter(!is.na(URXMX7)) %>%
  filter(URXMX7 < 100) %>%
  mutate(URXMX7_quintiles = ntile(URXMX7, 5))  # Create quintiles

# Plot with boxplot, jitter and quintiles
df_cafe_plot %>%
  ggplot(aes(x = as.factor(URXMX7_quintiles), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(URXMX7_quintiles)), width = 0.2, alpha = 0.5, size = 2) +
  labs(title = "SRI_no_imputation by Caffeine Concentration in Urine Quintiles",
       x = "Caffeine Concentration Quintiles",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# seems higher concentrations have a slightly higher SRI, but not much
# 4 points median difference



# SRI and occupation-----
df %>% # Type of work done last week
  filter(
    !is.na(SRI_no_imputation),
    OCD150 != 7,
    OCD150 != 9,
    !is.na(OCD150)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = as.factor(OCD150), y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Type of work done last week (OCD150)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Type of Work Done Last Week"
  ) +
  scale_x_discrete(labels = c("1" = "Working at a job or business", 
                              "2" = "With a job or business but not at work", 
                              "3" = "Looking for work", 
                              "4" = "Not working at a job or business")) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  theme_minimal()
# -> 10 points median difference

df %>% # Hours worked last week at all jobs
  filter(
    !is.na(SRI_no_imputation),
    OCQ180 != 77777,
    OCQ180 != 99999,
    !is.na(OCQ180)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = OCQ180, y = SRI_no_imputation)) +
  geom_point(alpha = 0.6, color = "blue") + # Plot points with some transparency
  geom_smooth() + 
  labs(
    x = "Hours worked last week at all jobs (OCQ180)",
    y = "SRI_no_imputation",
    title = "Scatter Plot of SRI_no_imputation vs Hours Worked Last Week"
  ) +
  theme_minimal()
table(df$OCQ180) # many work 30 or 40 hours

df %>% 
  filter(
    !is.na(SRI_no_imputation),
    OCQ180 != 77777,
    OCQ180 != 99999,
    !is.na(OCQ180)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  mutate(OCQ180_quintile = ntile(OCQ180, 5)) %>%  # Create quintiles based on OCQ180
  ggplot(aes(x = as.factor(OCQ180_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +  # Boxplots for quintiles
  geom_jitter(color = "blue", alpha = 0.6, width = 0.2) +  # Add jitter for individual points
  labs(
    x = "Quintiles of Hours Worked Last Week (OCQ180)",
    y = "SRI_no_imputation",
    title = "SRI_no_imputation by Quintiles of Hours Worked Last Week"
  ) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# -> 4 points median difference

df %>% # Usually work 35 or more hours per week
  filter(
    !is.na(SRI_no_imputation),
    OCQ210 != 7,  # Exclude "Refused"
    OCQ210 != 9,  # Exclude "Don't know"
    !is.na(OCQ210)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = as.factor(OCQ210), y = SRI_no_imputation)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  # Jitter points to avoid overlap
  geom_boxplot(alpha = 0.2) +  # Add a boxplot for better visualization
  labs(
    x = "Usually work 35 or more hours per week (OCQ210)",
    y = "SRI_no_imputation",
    title = "SRI_no_imputation by Usual Weekly Work Hours"
  ) +
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +  # Label the x-axis
  theme_minimal()

df %>% # Industry group code: current job
  filter(
    !is.na(SRI_no_imputation),
    !is.na(OCD231)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  group_by(OCD231) %>%
  mutate(median_SRI = median(SRI_no_imputation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OCD231 = reorder(factor(OCD231), median_SRI)) %>%
  ggplot(aes(x = OCD231, y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Industry Group Code: Current Job (OCD231)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Industry Group Code (Ordered by Median)"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Agriculture, Forestry, Fishing",
    "2" = "Mining",
    "3" = "Utilities",
    "4" = "Construction",
    "5" = "Manufacturing: Durable Goods",
    "6" = "Manufacturing: Non-Durable Goods",
    "7" = "Wholesale Trade",
    "8" = "Retail Trade",
    "9" = "Transportation, Warehousing",
    "10" = "Information Services",
    "11" = "Finance, Insurance",
    "12" = "Real Estate, Rental, Leasing",
    "13" = "Professional, Scientific, Technical Services",
    "14" = "Management, Administrative, Waste Services",
    "15" = "Educational Services",
    "16" = "Health Care, Social Assistance",
    "17" = "Arts, Entertainment, Recreation",
    "18" = "Accommodation, Food Services",
    "19" = "Other Services",
    "20" = "Private Household",
    "21" = "Public Administration",
    "22" = "Armed Forces"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# -> Max Median difference ~ 10 points
# -> Armed forces have the lowest median SRI

df %>%
  filter(
    !is.na(SRI_no_imputation),
    !is.na(OCD241)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  group_by(OCD241) %>%
  mutate(median_SRI = median(SRI_no_imputation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OCD241 = reorder(factor(OCD241), median_SRI)) %>%
  ggplot(aes(x = OCD241, y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Occupation Group Code: Current Job (OCD241)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Occupation Group Code (Ordered by Median)"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Management Occupations",
    "2" = "Business, Financial Operations Occupations",
    "3" = "Computer, Mathematical Occupations",
    "4" = "Architecture, Engineering Occupations",
    "5" = "Life, Physical, Social Science Occupations",
    "6" = "Community, Social Services Occupations",
    "7" = "Legal Occupations",
    "8" = "Education, Training, Library Occupations",
    "9" = "Arts, Design, Entertainment, Sports, Media Occupations",
    "10" = "Healthcare Practitioner, Technical Occupations",
    "11" = "Healthcare Support Occupations",
    "12" = "Protective Service Occupations",
    "13" = "Food Preparation, Serving Occupations",
    "14" = "Building & Grounds Cleaning, Maintenance Occupations",
    "15" = "Personal Care, Service Occupations",
    "16" = "Sales & Related Occupations",
    "17" = "Office, Administrative Support Occupations",
    "18" = "Farming, Fishing, Forestry Occupations",
    "19" = "Construction, Extraction Occupations",
    "20" = "Installation, Maintenance, Repair Occupations",
    "21" = "Production Occupations",
    "22" = "Transportation, Material Moving Occupations",
    "23" = "Armed Forces"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Quite similar

df %>%
  filter(
    !is.na(SRI_no_imputation),
    !is.na(OCQ260),
    OCQ260 != 77,
    OCQ260 != 99
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  group_by(OCQ260) %>%
  mutate(median_SRI = median(SRI_no_imputation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OCQ260 = reorder(factor(OCQ260), median_SRI)) %>%
  ggplot(aes(x = OCQ260, y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Description of Job/Work Situation (OCQ260)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Job/Work Situation (Ordered by Median)"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Employee of a private company",
    "2" = "Federal government employee",
    "3" = "State government employee",
    "4" = "Local government employee",
    "5" = "Self-employed",
    "6" = "Working without pay in family business or farm"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# No large differences, maybe Federal gov jobs sleep a little more regular :)

df %>%
  filter(
    !is.na(SRI_no_imputation),
    !is.na(OCD392)
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  group_by(OCD392) %>%
  mutate(median_SRI = median(SRI_no_imputation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OCD392 = reorder(factor(OCD392), median_SRI)) %>%
  ggplot(aes(x = OCD392, y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Occupation Group Code: Longest Job (OCD392)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Occupation Group (Longest Job)"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Management Occupations",
    "2" = "Business, Financial Operations Occupations",
    "3" = "Computer, Mathematical Occupations",
    "4" = "Architecture, Engineering Occupations",
    "5" = "Life, Physical, Social Science Occupations",
    "6" = "Community, Social Services Occupations",
    "7" = "Legal Occupations",
    "8" = "Education, Training, Library Occupations",
    "9" = "Arts, Design, Entertainment, Sports, Media Occupations",
    "10" = "Healthcare Practitioner, Technical Occupations",
    "11" = "Healthcare Support Occupations",
    "12" = "Protective Service Occupations",
    "13" = "Food Preparation, Serving Occupations",
    "14" = "Building & Grounds Cleaning, Maintenance Occupations",
    "15" = "Personal Care, Service Occupations",
    "16" = "Sales & Related Occupations",
    "17" = "Office, Administrative Support Occupations",
    "18" = "Farming, Fishing, Forestry Occupations",
    "19" = "Construction, Extraction Occupations",
    "20" = "Installation, Maintenance, Repair Occupations",
    "21" = "Production Occupations",
    "22" = "Transportation, Material Moving Occupations",
    "23" = "Armed Forces"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df %>%
  filter(
    !is.na(SRI_no_imputation),
    !is.na(OCQ380),
    OCQ380 != 77,  # Exclude 'Refused'
    OCQ380 != 99   # Exclude 'Don't know'
  ) %>%
  filter(SRI_no_imputation > -10) %>%
  group_by(OCQ380) %>%
  mutate(median_SRI = median(SRI_no_imputation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OCQ380 = reorder(factor(OCQ380), median_SRI)) %>%
  ggplot(aes(x = OCQ380, y = SRI_no_imputation)) +
  geom_boxplot() +
  labs(
    x = "Main reason did not work last week (OCQ380)",
    y = "SRI_no_imputation",
    title = "Boxplot of SRI_no_imputation by Main Reason Did Not Work Last Week"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Taking care of house or family",
    "2" = "Going to school",
    "3" = "Retired",
    "4" = "Unable to work for health reasons",
    "5" = "On layoff",
    "6" = "Disabled",
    "7" = "Other"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SRI and cardiovascular-------
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CDQ_G.htm

# Filter and plot CDQ001 - SP ever had pain or discomfort in chest
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ001 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ001)) %>%
  mutate(CDQ001 = factor(CDQ001, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = CDQ001, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Chest Pain/Discomfort Diagnosis",
       x = "Doctor Told You Have Chest Pain/Discomfort",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# -> 4-5 points median difference

# Filter and plot CDQ002 SP get it walking uphill or in a hurry
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(CDQ002 %in% c(1, 2)) %>%  # Only include 1 = Yes, 2 = No
  mutate(CDQ002 = factor(CDQ002, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = CDQ002, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Chest Pain/Discomfort Diagnosis",
       x = "Chest Pain/Discomfort Diagnosis",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# -> Visible difference, 4 points median

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ003 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ003)) %>%
  mutate(CDQ003 = factor(CDQ003, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = CDQ003, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Pain at Ordinary Pace",
       x = "Pain at Ordinary Pace on Level Ground",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# small difference

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ004 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ004)) %>%
  mutate(CDQ004 = factor(CDQ004, levels = c(1, 2), labels = c("Stop/Slow Down", "Continue"))) %>%
  ggplot(aes(x = CDQ004, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Continuation or Slowing Down",
       x = "Continuation or Slowing Down",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# small difference

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ005 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ005)) %>%
  mutate(CDQ005 = factor(CDQ005, levels = c(1, 2), labels = c("Relieved", "Not Relieved"))) %>%
  ggplot(aes(x = CDQ005, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Pain Relief When Standing",
       x = "Pain Relief When Standing",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# almost no difference

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ006 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ006)) %>%
  mutate(CDQ006 = factor(CDQ006, levels = c(1, 2), labels = c("10 Minutes or Less", "More Than 10 Minutes"))) %>%
  ggplot(aes(x = CDQ006, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Time to Pain Relief",
       x = "Time to Pain Relief",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")
# a bit of difference

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ009A %in% c(77, 99)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ009A)) %>%
  mutate(CDQ009A = factor(CDQ009A, levels = c(1), labels = c("Pain in Right Arm"))) %>%
  ggplot(aes(x = CDQ009A, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Pain in Right Arm",
       x = "Pain in Right Arm",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")


df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ008 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ008)) %>%
  mutate(CDQ008 = factor(CDQ008, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = CDQ008, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Severe Chest Pain (>30min)",
       x = "Severe Chest Pain > 30min",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# some difference, 4 points median

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!CDQ010 %in% c(7, 9)) %>%  # Exclude 'Refused', 'Don't Know'
  filter(!is.na(CDQ010)) %>%
  mutate(CDQ010 = factor(CDQ010, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  ggplot(aes(x = CDQ010, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "SRI_no_imputation by Shortness of Breath on Stairs/Inclines",
       x = "Shortness of Breath on Stairs/Inclines",
       y = "SRI_no_imputation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red")  # Annotate medians
# -> 6 points median difference

# SRI and Vitamin D----
# https://www.ingentaconnect.com/content/ben/cpd/2020/00000026/00000021/art00005
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.htm

# Filter and visualize Vitamin D (LBXVIDMS) vs. SRI_no_imputation

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBXVIDMS)) %>%
  filter(LBXVIDMS < 250) %>%
  ggplot(aes(x = LBXVIDMS, y = SRI_no_imputation)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method="lm",color = "red", se = TRUE) +  # Add a regression line
  stat_cor(method = "pearson", aes(label = paste(..r.label.., sep = "~`,`~")), 
           label.x = 200, label.y = 90) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 200, label.y = 85, aes(label = ..eq.label..)) +  # Add regression equation
  labs(title = "Relationship between Vitamin D (25-hydroxyvitamin D) and SRI_no_imputation",
       x = "25-hydroxyvitamin D (nmol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# slight trend up, but very high variability, hard to see a "trend"

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBXVIDMS)) %>%
  filter(LBXVIDMS < 250) %>%
  mutate(LBXVIDMS_quintile = ntile(LBXVIDMS, 5)) %>%  # Create quintiles of Vitamin D levels
  ggplot(aes(x = as.factor(LBXVIDMS_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +  # Create boxplots for each quintile
  geom_jitter(color = "blue", alpha = 0.5, width = 0.2) +  # Add jitter to the data points
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate the medians
  labs(title = "SRI_no_imputation by Quintiles of Vitamin D (25-hydroxyvitamin D)",
       x = "Vitamin D (25-hydroxyvitamin D) Quintiles",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Up to 7 points median difference

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBXVD3MS)) %>%
  filter(LBXVD3MS < 250) %>%
  ggplot(aes(x = LBXVD3MS, y = SRI_no_imputation)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add a regression line
  stat_cor(method = "pearson", aes(label = paste(..r.label.., sep = "~`,`~")), 
           label.x = 200, label.y = 90) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 200, label.y = 85, aes(label = ..eq.label..)) +  # Add regression equation
  labs(title = "Relationship between 25-hydroxyvitamin D3 and SRI_no_imputation",
       x = "25-hydroxyvitamin D3 (nmol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# same as above, upward "trend" but very high variability

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBXVD3MS)) %>%
  filter(LBXVD3MS < 250) %>%
  mutate(LBXVD3MS_quintile = ntile(LBXVD3MS, 5)) %>%  # Create quintiles for Vitamin D3 levels
  ggplot(aes(x = as.factor(LBXVD3MS_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +  # Create boxplots for each quintile
  geom_jitter(color = "green", alpha = 0.5, width = 0.2) +  # Add jitter to the data points
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate the medians
  labs(title = "SRI_no_imputation by Quintiles of 25-hydroxyvitamin D3",
       x = "25-hydroxyvitamin D3 Quintiles (nmol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# same, up to 7 points median difference

# SRI and cholesterol (LBDHDDSI)----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HDL_G.htm

# Plot Direct HDL-Cholesterol (mg/dL) against SRI_no_imputation
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBDHDD)) %>%
  ggplot(aes(x = LBDHDD, y = SRI_no_imputation)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  labs(title = "Relationship between Direct HDL-Cholesterol (mg/dL) and SRI_no_imputation",
       x = "Direct HDL-Cholesterol (mg/dL)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 100, label.y = 75) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 100, label.y = 70)  # Add regression equation

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBDHDD)) %>%
  mutate(LBDHDD_quintile = ntile(LBDHDD, 5)) %>%  # Create quintiles for Direct HDL-Cholesterol
  ggplot(aes(x = as.factor(LBDHDD_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +  # Create boxplots for each quintile
  geom_jitter(color = "blue", alpha = 0.5, width = 0.2) +  # Add jitter to the data points
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate the medians
  labs(title = "SRI_no_imputation by Quintiles of Direct HDL-Cholesterol",
       x = "Direct HDL-Cholesterol Quintiles (mg/dL)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Up to 7 points difference, the higher HDL choleresol the higher SRI?

# Plot Direct HDL-Cholesterol (mmol/L) against SRI_no_imputation
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBDHDD)) %>%
  ggplot(aes(x = LBDHDDSI, y = SRI_no_imputation)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between Direct HDL-Cholesterol (mmol/L) and SRI_no_imputation",
       x = "Direct HDL-Cholesterol (mmol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 3.5, label.y = 75) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 3.5, label.y = 70)  # Add regression equation
# both slight upward trend, but very high variability

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBDHDDSI)) %>%
  mutate(LBDHDDSI_quintile = ntile(LBDHDDSI, 5)) %>%  # Create quintiles for HDL-Cholesterol (mmol/L)
  ggplot(aes(x = as.factor(LBDHDDSI_quintile), y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue") +  # Create boxplots for each quintile
  geom_jitter(color = "blue", alpha = 0.5, width = 0.2) +  # Add jitter to the data points
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate the medians
  labs(title = "SRI_no_imputation by Quintiles of Direct HDL-Cholesterol (mmol/L)",
       x = "Direct HDL-Cholesterol Quintiles (mmol/L)",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# same, upt to 7 points difference

# ?SRI and diet/food intake-------
# Seems obvious, late night snacking, alcohol intake etc. affects sleep!

# ?SRI and sleep disorders----
# Ever told doctor had trouble sleeping?
# Ever told by doctor have sleep disorder?
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SLQ_G.htm

# _How much sleep do you get?----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SLQ_G.htm#SLD010H
# Should be intedpendet from SRI

# ?SRI and early childhood-
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ECQ_G.htm#ECQ020
# Available only for children.....

# ?SRI and housing characteristics-------
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HOQ_G.htm#HOD050

# ?SRI and health insurance coverage----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HIQ_G.htm#HIQ011

# ?SRI and food security----
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/FSQ_G.htm#FSD032A

# ?SRI and Light Regularity Index?----
# https://academic.oup.com/sleep/article/46/8/zsad001/6982548
# Inspired by the formulation of the SRI, we devised an equivalent metric for light.
# First, we converted the light time series into a binary signal (similar to sleep 
# vs. wake), based on whether the light level was above or below an absolute
# threshold (i.e. 10, 20, 50, 100, 300, and 500 lux). The LRI was then calculated
# by determining the probability that an individual was in the same state (above
# vs. below a threshold) at any two time points, 24 h apart. The index was scaled
# so that an individual who records light exposure greater than the threshold light
# level at exactly the same times each day would score 100, whereas an individual
# who records random light exposure patterns would score 0. Participants whose LRI
# value was calculated using less than 5 days of overlapping valid epochs were
# considered missing for that period (school = 0.00%; vacation = 2.00%).

# SRI and Total abdominal fat mass----------
sum(!is.na(df$DXXTATM))
df %>% 
  dplyr::select(SRI_no_imputation, DXXTATM) %>% # Total abdominal fat mass
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXTATM)) +
  geom_point(color = "black", alpha = 0.8) +  
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Total abdominal fat mass") +
  ggtitle("Relationship between Total abdominal fat mass and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SRI and Total body fat volume----------
df %>% 
  dplyr::select(SRI_no_imputation, DXXTATV) %>% # Total body fat volume
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXTATV)) +
  geom_point(color = "black", alpha = 0.8) +  
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Total body fat volume") +
  ggtitle("Relationship between Total body fat volumen and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# SRI and Visceral adipose tissue mass----
# DXXVFATM
df %>% 
  dplyr::select(SRI_no_imputation, DXXVFATM) %>% # Visceral adipose tissue mass
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXVFATM)) +
  geom_point(color = "black", alpha = 0.8) +  
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Visceral adipose tissue mass") +
  ggtitle("Relationship between Visceral adipose tissue mass and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SRI and Visceral adipose tissue volume----
# DXXVFATV
df %>% 
  dplyr::select(SRI_no_imputation, DXXVFATV) %>% # Visceral adipose tissue volume
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXVFATV)) +
  geom_point(color = "black", alpha = 0.8) +  
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Visceral adipose tissue volume") +
  ggtitle("Relationship between Visceral adipose tissue volume and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SRI and Android fat mass----
# DXXANFM
df %>% 
  dplyr::select(SRI_no_imputation, DXXANFM) %>% # Android fat mass
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXANFM)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Android fat mass") +
  ggtitle("Relationship between Android fat mass and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# cor = -0.08

# SRI and Gynoid fat mass----
# DXXGYFM
df %>%
  dplyr::select(SRI_no_imputation, DXXGYFM) %>% # Gynoid fat mass
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXGYFM)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Gynoid fat mass") +
  ggtitle("Relationship between Gynoid fat mass and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# cor = -0.04

# SRI and Android to Gynoid ratio----
# DXXAGRAT
df %>%
  dplyr::select(SRI_no_imputation, DXXAGRAT) %>% # Android to Gynoid ratio
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXAGRAT)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Android to Gynoid ratio") +
  ggtitle("Relationship between Android to Gynoid ratio and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# why y-axis discrete??

# SRI and Subcutaneous fat mass----
# DXXSATM
df %>%
  dplyr::select(SRI_no_imputation, DXXSATM) %>% # Subcutaneous fat mass
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXSATM)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Subcutaneous fat mass") +
  ggtitle("Relationship between Subcutaneous fat mass and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# cor = -0.05

# SRI and Subcutaneous fat volume----
# DXXSATV
df %>% 
  dplyr::select(SRI_no_imputation, DXXSATV) %>% # Subcutaneous fat volume
  na.omit() %>%
  filter(SRI_no_imputation > -10) %>%
  ggplot(aes(x = SRI_no_imputation, y = DXXSATV)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "SRI", y = "Subcutaneous fat volume") +
  ggtitle("Relationship between Subcutaneous fat volume and SRI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# cor = -0.05

# missings...
variables <- c("DXXAGST", "DXXVATV", "DXXANTV", "DXXGYTV", "DXXANFM", "DXXANLM", "DXXANTOM", 
               "DXXGYFM", "DXXGYLM", "DXXGYTOM", "DXXAGRAT", "DXXAPFAT", "DXXGPFAT", "DXXSATA", 
               "DXXSATM", "DXXSATV", "DXXVFATA", "DXXVFATM", "DXXVFATV")
df_subset <- df[ , variables]
vis_miss(df_subset) # ~45$% missing


# BMI and Total Energy Intake DR1TKCAL----
# Calculate the 0.5% and 99.5% quantiles for winsorization
quantiles <- quantile(df$DR1TKCAL, probs = c(0.01, 0.99), na.rm = TRUE)

df %>%
  filter(BMXBMI < 60) %>%
  mutate(DR1TKCAL = ifelse(DR1TKCAL < quantiles[1], quantiles[1],
                           ifelse(DR1TKCAL > quantiles[2], quantiles[2], DR1TKCAL))) %>%
  dplyr::select(DR1TKCAL, BMXBMI) %>%
  na.omit() %>%
  ggplot(aes(x = DR1TKCAL, y = BMXBMI)) +
  geom_point(color = "black", alpha = 0.8) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "Total Energy Intake (kcal)", y = "BMI") +
  ggtitle("Relationship between BMI and Total Energy Intake") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# -> positive association of course.


# BMI and Age----
df %>% 
  filter(BMXBMI < 60) %>%
  dplyr::select(RIDAGEYR, BMXBMI) %>%
  na.omit() %>% 
  ggplot(aes(x = RIDAGEYR, y = BMXBMI)) +
  geom_point(color = "black", alpha = 0.8) +  
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(aes(label = paste("Pearson: ", ..r.label..)), method = "pearson", size = 5, vjust = 1) +
  stat_cor(aes(label = paste("Spearman: ", ..r.label..)), method = "spearman", size = 5, vjust = 3) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), 
                        formula = y ~ x, size = 5, color = "blue") +
  labs(x = "Age (years)", y = "BMI") +
  ggtitle("Relationship between Age and BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# BMI and Sex----
df %>%
  filter(BMXBMI < 60) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR)) %>%
  ggplot(aes(x = RIAGENDR, y = BMXBMI)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(x = "Sex (1 = Male, 2 = Female)", y = "BMI") +
  ggtitle("BMI Distribution by Sex") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Not much difference

# BMI and Marital status------
marital_status_labels <- c(
  "1" = "Married",
  "2" = "Widowed",
  "3" = "Divorced",
  "4" = "Separated",
  "5" = "Never Married",
  "6" = "Living with Partner",
  "77" = "Refused",
  "99" = "Don't Know"
)
df %>%
  filter(BMXBMI < 60) %>%
  mutate(DMDMARTL = as.factor(DMDMARTL)) %>%
  ggplot(aes(x = DMDMARTL, y = BMXBMI)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(x = "Marital Status", y = "BMI") +
  scale_x_discrete(labels = marital_status_labels) +  # Apply marital status labels
  ggtitle("BMI Distribution by Marital Status") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# -> "Refused" median 35!!!, "Never Married" lowest BMI (27.1)

# BMI and Ethnicity------
custom_labels <- c("1" = "Mexican American", 
                   "2" = "Other Hispanic", 
                   "3" = "Non-Hispanic White", 
                   "4" = "Non-Hispanic Black", 
                   "6" = "Non-Hispanic Asian", 
                   "7" = "Other Race - Including Multi-Racial")

df %>%
  filter(BMXBMI < 60) %>%
  mutate(RIDRETH3 = factor(RIDRETH3, levels = names(custom_labels))) %>%
  ggplot(aes(x = RIDRETH3, y = BMXBMI)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = custom_labels) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(title = "BMI by Ethnicity",
       x = "Ethnicity",
       y = "BMI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Asians smaller BMI
# -> 5 points median difference

# BMI and Education----
education_labels <- c(
  "1" = "Less than 9th grade",
  "2" = "9-11th grade (No diploma)",
  "3" = "High school graduate/GED",
  "4" = "Some college or AA degree",
  "5" = "College graduate or above"
)

df %>%
  filter(BMXBMI < 60) %>%
  filter(!DMDEDUC2 %in% c("7", "9", ".")) %>%
  mutate(DMDEDUC2 = factor(DMDEDUC2, levels = names(education_labels))) %>%
  ggplot(aes(x = DMDEDUC2, y = BMXBMI)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = education_labels) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(title = "BMI by Education Level",
       x = "Education Level",
       y = "BMI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# college lower BMI, 2 points median difference

# BMI and Education----
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>% 
  filter(DMDMARTL %nin% c(77,99)) %>% 
  mutate(DMDMARTL = as.factor(DMDMARTL)) %>%
  filter(DMDEDUC2 %nin% c(7,9)) %>%
  mutate(DMDEDUC2 = as.factor(DMDEDUC2)) %>%
  filter(DMDEDUC2 %in% c(1,5)) %>% # Less than 9th grade vs. College graduate or above
  group_by(DMDEDUC2) %>%
  dplyr::summarize(median_BMI = median(BMXBMI))
# surprisingly little difference

education_labels <- c(
  "1" = "Less than 9th grade",
  "2" = "9-11th grade (No diploma)",
  "3" = "High school graduate/GED",
  "4" = "Some college or AA degree",
  "5" = "College graduate or above"
)
medians <- df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!DMDEDUC2 %in% c("7", "9", ".")) %>%
  filter(!is.na(DMDEDUC2)) %>%
  group_by(DMDEDUC2) %>%
  dplyr::summarize(median_SRI = median(SRI_no_imputation))
median_difference <- max(medians$median_SRI) - min(medians$median_SRI)
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  filter(!DMDEDUC2 %in% c("7", "9", ".")) %>%
  filter(!is.na(DMDEDUC2)) %>%
  mutate(DMDEDUC2 = factor(DMDEDUC2, levels = names(sort(tapply(SRI_no_imputation, DMDEDUC2, median), decreasing = FALSE)))) %>%
  ggplot(aes(x = DMDEDUC2, y = SRI_no_imputation)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = education_labels) +
  labs(title = "SRI_no_imputation by Education Level",
       x = "Education Level",
       y = "SRI_no_imputation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$SRI_no_imputation, na.rm = TRUE) + 5, 
           label = paste("Difference in Medians:", round(median_difference, 2)), # 7 points
           color = "red")
# Only College graduate or above seems to make a difference.
# Up to 7 points difference in medians

# BMI and Occupation----
df %>% 
  filter(
    !is.na(BMXBMI),
    OCD150 != 7,   # Exclude specific values for occupation (if needed)
    OCD150 != 9,
    !is.na(OCD150)
  ) %>%
  filter(BMXBMI < 60) %>%  # Filter out potential BMI outliers
  ggplot(aes(x = as.factor(OCD150), y = BMXBMI)) +
  geom_boxplot() +
  labs(
    x = "Type of work done last week (OCD150)",
    y = "BMI",
    title = "Boxplot of BMI by Type of Work Done Last Week"
  ) +
  scale_x_discrete(labels = c("1" = "Working at a job or business", 
                              "2" = "With a job or business but not at work", 
                              "3" = "Looking for work", 
                              "4" = "Not working at a job or business")) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  theme_minimal()

# BMI and household income-----
income_labels <- c(
  "1" = "$0 to $4,999",
  "2" = "$5,000 to $9,999",
  "3" = "$10,000 to $14,999",
  "4" = "$15,000 to $19,999",
  "5" = "$20,000 to $24,999",
  "6" = "$25,000 to $34,999",
  "7" = "$35,000 to $44,999",
  "8" = "$45,000 to $54,999",
  "9" = "$55,000 to $64,999",
  "10" = "$65,000 to $74,999",
  "12" = "$20,000 and Over",
  "13" = "Under $20,000",
  "14" = "$75,000 to $99,999",
  "15" = "$100,000 and Over",
  "77" = "Refused",
  "99" = "Don't know"
)

# Filter and calculate medians for each income group
medians <- df %>%
  filter(BMXBMI < 60) %>%
  filter(!INDHHIN2 %in% c("77", "99", ".")) %>%
  filter(!is.na(INDHHIN2)) %>%
  group_by(INDHHIN2) %>%
  summarise(median_BMI = median(BMXBMI, na.rm = TRUE))

# Calculate maximal median difference
median_difference <- max(medians$median_BMI) - min(medians$median_BMI)

# Reorder household income factor by median BMI
df %>%
  filter(BMXBMI < 60) %>%
  filter(!INDHHIN2 %in% c("77", "99", ".")) %>%
  filter(!is.na(INDHHIN2)) %>%
  mutate(INDHHIN2 = factor(INDHHIN2, levels = medians$INDHHIN2[order(medians$median_BMI)])) %>%
  ggplot(aes(x = INDHHIN2, y = BMXBMI)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.1, color = "darkblue") +  # Add jitter to show individual points
  scale_x_discrete(labels = income_labels) +
  labs(title = "BMI by Household Income",
       x = "Household Income",
       y = "BMI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 3, y = max(df$BMXBMI, na.rm = TRUE) + 2, 
           label = paste("Max Median Diff:", round(median_difference, 2)), 
           color = "red")
# 2.3 median difference, clinically relevant?


# BMI and depression----
df %>%
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
  filter(!is.na(Depression_score_PHQ_9)) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = Depression_score_PHQ_9, y = BMXBMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method="lm",color = "red", se = TRUE) +
  stat_cor(method = "pearson", aes(label = paste(..r.label.., sep = "~`,`~")), 
           label.x = 20, label.y = 45) +  # Pearson correlation coefficient
  stat_regline_equation(label.x = 20, label.y = 40, aes(label = ..eq.label..)) +  # Regression equation
  labs(title = "Relationship between Depression Score (PHQ-9) and BMI",
       x = "Depression Score (PHQ-9)",
       y = "BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# no clear trend, maybe a little up

# BMI and blood pressure----
df_bp <- df %>%
  rowwise() %>%
  mutate(
    Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
    Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
  ) %>%
  ungroup()

# Systolic Blood Pressure vs BMI
ggplot(df_bp, aes(x = Avg_Systolic_BP, y = BMXBMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(se = TRUE, color = "red") +
  labs(title = "BMI vs Average Systolic Blood Pressure",
       x = "Average Systolic Blood Pressure (mm Hg)",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Diastolic Blood Pressure vs BMI
ggplot(df_bp, aes(x = Avg_Diastolic_BP, y = BMXBMI)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(se = TRUE, color = "red") +
  labs(title = "BMI vs Average Diastolic Blood Pressure",
       x = "Average Diastolic Blood Pressure (mm Hg)",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))




# BMI and diabetes----

df %>%
  filter(!is.na(DIQ010)) %>%
  mutate(DIQ010 = factor(DIQ010, levels = c(1, 3, 2), labels = c("Yes", "Borderline", "No"))) %>%
  ggplot(aes(x = DIQ010, y = BMXBMI)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(title = "BMI by Diabetes Diagnosis",
       x = "Diabetes Diagnosis",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# No -> lower BMI, 3 points median difference

# BMI and Alcohol----
df %>% 
  filter(ALQ130 %in% 1:82) %>%       # Include only relevant values (1 to 82)
  filter(!is.na(ALQ130)) %>%         # Exclude missing values
  filter(BMXBMI < 60) %>%            # Filter BMI values to remove outliers
  dplyr::select(ALQ130, BMXBMI) %>%
  na.omit() %>%
  filter(ALQ130 < 80) %>%            # Filter for reasonable alcohol consumption values
  ggplot(aes(x = ALQ130, y = BMXBMI)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot points
  geom_smooth(se = TRUE, color = "red") +  # Add a regression line
  labs(title = "BMI by Avg # Alcoholic Drinks/Day - Past 12 Months",
       x = "Avg # Alcoholic Drinks/Day",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 5, label.y = max(df$BMXBMI, na.rm = TRUE) - 5)

# BMI and smoking----
# Current Smoking Status (SMQ040)
df %>%
  filter(SMQ040 %in% c(1, 2, 3)) %>%
  mutate(SMQ040 = factor(SMQ040, levels = c(1, 2, 3), labels = c("Every day", "Some days", "Not at all"))) %>%
  ggplot(aes(x = SMQ040, y = BMXBMI)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "red") +  # Annotate medians
  labs(title = "BMI by Smoking Status",
       x = "Smoking Status",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# No large difference, 1.5 points median BMI difference between "Every day" and "Not at all"

# Cigarettes per Day (SMD650)
df %>%
  filter(SMD650 %in% 2:90) %>%  # Cigarettes per day range
  filter(!is.na(SMD650)) %>%
  ggplot(aes(x = SMD650, y = BMXBMI)) +
  geom_point(color = "purple", size = 2, alpha = 0.6) +  # Scatter plot
  geom_smooth(se = TRUE, color = "red") +  # Add a regression line
  labs(title = "BMI by Average Number of Cigarettes/Day",
       x = "Cigarettes/Day",
       y = "BMI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# No trend

# BMI and Vitamin D----
df %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBXVIDMS)) %>%
  filter(LBXVIDMS < 250) %>%
  ggplot(aes(x = LBXVIDMS, y = BMXBMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add a regression line
  stat_cor(method = "pearson", aes(label = paste(..r.label.., sep = "~`,`~")), 
           label.x = 200, label.y = 40) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 200, label.y = 38, aes(label = ..eq.label..)) +  # Add regression equation
  labs(title = "Relationship between 25-hydroxyvitamin D and BMI",
       x = "25-hydroxyvitamin D (nmol/L)",
       y = "BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# BMI and cholesterol----
df %>%
  filter(BMXBMI < 60) %>%
  filter(!is.na(LBDHDD)) %>%
  ggplot(aes(x = LBDHDD, y = BMXBMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between Direct HDL-Cholesterol (mg/dL) and BMI",
       x = "Direct HDL-Cholesterol (mg/dL)",
       y = "BMI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_cor(method = "pearson", label.x = 100, label.y = 50) +  # Add Pearson correlation coefficient
  stat_regline_equation(label.x = 100, label.y = 48)  # Add regression equation

# ?BMI and food intake-------
# There are obvious connections, which one should one take...




# Rescaling of the SRI----
# Phillips et al. 2017 scientific reports:
# y=200*(x-0.5)
x <- c(0,1)
# y=
200*(x-0.5) # -100, 100
# i.e. -100 (=0) is equivalent to completely inverted sleep/wake patterns; all comparisons are 0.
# 100 (=1) is equivalent to completely regular sleep/wake patterns; all comparisons are 1.






# 3) Modeling exploration----
# _Try 0---------
mod <- lm(log(BMXBMI) ~ SRI_no_imputation + RIDAGEYR + RIAGENDR, 
          data = df %>%
            filter(SRI_no_imputation > -10) %>%
            filter(BMXBMI < 60) %>%
            mutate(RIAGENDR = as.factor(RIAGENDR))) 
df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = log(BMXBMI))) + 
  geom_histogram() # skewed without log

df %>%
  filter(SRI_no_imputation > -10) %>%
  filter(BMXBMI < 60) %>% dplyr::slice_sample(n=100) %>% vis_miss()

summary(mod) # almost NO variability explained by the model!!! 2%
# Coefficients seem to be practically zero: 100 points difference in SRI would be a 0.2 difference in BMI, so practically zero according to this simple model
check_model(mod) # Rather nice I would say, PPC could be slightly improved
hist(residuals(mod))
plot(residuals(mod)) # no apparent structure apart from the skewed nature
qqPlot(residuals(mod)) # some departure visible

# with SRI maximize
mod_SRI_maximize <- lm(log(BMXBMI) ~ SRI_maximize + RIDAGEYR + RIAGENDR, 
          data = df %>%
            filter(SRI_maximize > -10) %>%
            filter(BMXBMI < 60) %>%
            mutate(RIAGENDR = as.factor(RIAGENDR))) 
df %>%
  filter(SRI_maximize > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = log(BMXBMI))) + 
  geom_histogram() # skewed without log

summary(mod_SRI_maximize) # almost NO variability explained by the model!!! 2%
# Coefficients very similar... 
check_model(mod_SRI_maximize) # Rather nice I would say.
hist(residuals(mod_SRI_maximize))
plot(residuals(mod_SRI_maximize)) # no apparent structure apart from the skewed nature
qqPlot(residuals(mod_SRI_maximize)) # some departure visible

# SRI minimize
mod_SRI_minimize <- lm(log(BMXBMI) ~ SRI_minimize + RIDAGEYR + RIAGENDR, 
                       data = df %>%
                         filter(SRI_minimize > -10) %>%
                         filter(BMXBMI < 60) %>%
                         mutate(RIAGENDR = as.factor(RIAGENDR)))

df %>%
  filter(SRI_minimize > -10) %>%
  filter(BMXBMI < 60) %>%
  ggplot(aes(x = log(BMXBMI))) + 
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Histogram of log(BMXBMI) with SRI_minimize",
       x = "log(BMXBMI)",
       y = "Frequency") +
  theme_minimal()
summary(mod_SRI_minimize)
check_model(mod_SRI_minimize) # very nice
# very close again.


# _Try 1----
mod1 <- lm(log(BMXBMI) ~ SRI_no_imputation + 
            RIDAGEYR + # Age in years
            RIAGENDR + # Gender
            DMDMARTL + # Marital status
            DMDEDUC2 +  # Education level
            INDHHIN2, # Household income
          data = df %>%
            filter(SRI_no_imputation > -10) %>%
            filter(BMXBMI < 60) %>% 
            filter(DMDMARTL %nin% c(77,99)) %>%
            mutate(DMDMARTL = as.factor(DMDMARTL)) %>%
            filter(DMDEDUC2 %nin% c(7,9)) %>%
            mutate(DMDEDUC2 = as.factor(DMDEDUC2)) %>%
            mutate(INDHHIN2 = as.factor(INDHHIN2)) %>%
            filter(!INDHHIN2 %in% c("77", "99", ".")) %>%
            filter(!is.na(INDHHIN2))
          ) 
summary(mod1) # Still, explains very few variability (4.2%) of BMI
check_model(mod1) # still good, normality of residuals could be better
hist(residuals(mod1)) # very symmetric
plot(residuals(mod1)) # nice
mean(residuals(mod1)) # 0

# _Try 2----
reg_fit <- regsubsets(log(BMXBMI) ~ SRI_no_imputation + RIDAGEYR + RIAGENDR + 
                        RIDRETH3 + Depression_score_PHQ_9 + ALQ130 + SMD650 + 
                        LBDHDDSI, # add more covariates
                      data = df %>% 
                        mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                                      ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
                        mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
                                 DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
                        filter(BMXBMI < 60), 
                      nvmax = 10)
summary(reg_fit)
plot(reg_fit, scale = "bic")


# _Try 3----
hist(df$BMXBMI[df$BMXBMI < 60])
mod <- lm(log(BMXBMI) ~ SRI_no_imputation + # SRI, not imputed
                       RIDAGEYR + # age in years
                       RIAGENDR + # gender
                       RIDRETH3 + # ethnicity
                       Depression_score_PHQ_9 + # PHQ-9 score 
                       LBDHDDSI + 
                       Avg_Systolic_BP, 
   data = df %>% 
     mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                   ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
     mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
              DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
     filter(BMXBMI < 60) %>%
     rowwise() %>%
     mutate(
       Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
       Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
     ) %>%
     ungroup())
summary(mod) # 15.4% of variability explained
check_model(mod) # rather nice! normality of residuals could be better...
plot(residuals(mod))

# Load necessary libraries
library(ggplot2)
library(broom)
library(dplyr)

# Extract residuals using broom::augment()
residuals_df <- augment(mod)

# Create a residuals plot
ggplot(residuals_df, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


library(patchwork)

# Create residuals plot and QQ plot
p1 <- ggplot(residuals_df, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  )

p2 <- ggplot(residuals_df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(
    title = "QQ Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Combine both plots using patchwork
p1 + p2 + plot_layout(ncol = 2)

# still not optimal.. residuals could be more normal

# _Try 4.1 ----
library(rms)
library(MASS)
# Note, that rlm does not show much more normality
mod_spline <- lm(BMXBMI_log ~ rcs(RIDAGEYR, 4) + # Spline for Age with 4 knots
                   rcs(Depression_score_PHQ_9, 4) + # Spline for Depression Score
                   rcs(SRI_no_imputation,4) + # SRI, not imputed
                   RIAGENDR + # gender
                   RIDRETH3 + # ethnicity
                   rcs(LBDHDDSI,4) + # HDL cholesterol
                   rcs(Avg_Systolic_BP,4) + 
                   DIQ010 + # diabetes
                   rcs(LBXVIDMS,4) + # Vitamin D
                   ALQ130 + # alcohol
                   SMQ040, # smoking
                 data = df %>% 
                   mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                                 ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
                   mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
                            DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
                   filter(BMXBMI < 60) %>%
                   filter(SRI_no_imputation > -10) %>%
                   rowwise() %>%
                   mutate(
                     Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
                     Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
                   ) %>%
                   ungroup() %>%
                   mutate(BMXBMI_log = log(BMXBMI)) %>%
                   filter(LBXVIDMS < 250) %>%
                   filter(SMQ040 %in% c(1, 2, 3)) %>%
                   filter(ALQ130 %in% 1:82))
summary(mod_spline) # ~ 25% of variability explained
check_model(mod_spline) # Check diagnostics, no green lines for rlm()
check_model(mod_spline, check = "pp_check") # slightly off
plot(residuals(mod_spline)) # residuals look good

residuals_df <- data.frame(
  .fitted = fitted(mod_spline),
  .resid = residuals(mod_spline)
)
library(car)
qqPlot(mod_spline, main = "QQ Plot with Confidence Intervals") # slightly off, but better than the smaller models I think

influence_points <- cooks.distance(mod_spline)
high_influence <- which(influence_points > 4 / nrow(df))  # Common threshold for Cook's distance
df[high_influence, ]

# Residuals vs Fitted plot
p1 <- ggplot(residuals_df, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  )

# QQ Plot of Residuals
p2 <- ggplot(residuals_df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(
    title = "QQ Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

p1 + p2 + plot_layout(ncol = 2)

# _Try4 without splines----
mod_no_spline <- lm(BMXBMI_log ~ RIDAGEYR + 
                      Depression_score_PHQ_9 + 
                      SRI_no_imputation + 
                      RIAGENDR + 
                      RIDRETH3 + 
                      LBDHDDSI + 
                      Avg_Systolic_BP + 
                      DIQ010 + 
                      LBXVIDMS + 
                      ALQ130 + 
                      SMQ040, 
                    data = df %>% 
                      mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                                    ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
                      mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
                               DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
                      filter(BMXBMI < 60) %>%
                      filter(SRI_no_imputation > -10) %>%
                      rowwise() %>%
                      mutate(
                        Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
                        Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
                      ) %>%
                      ungroup() %>%
                      mutate(BMXBMI_log = log(BMXBMI)) %>%
                      filter(LBXVIDMS < 250) %>%
                      filter(SMQ040 %in% c(1, 2, 3)) %>%
                      filter(ALQ130 %in% 1:82))
summary(mod_no_spline) # Adjusted R-squared:  0.2225
check_model(mod_no_spline) # slightly worse model fit, but on the surface it looks quite good
check_model(mod_no_spline, check = "pp_check") # slightly off, not worse than with splines
plot(residuals(mod_spline)) # residuals look good


library(MASS)

# Fit a full model with interaction terms
full_model <- lm(BMXBMI_log ~ (RIDAGEYR + Depression_score_PHQ_9 + SRI_no_imputation + 
                                 RIAGENDR + RIDRETH3 + LBDHDDSI + Avg_Systolic_BP + 
                                 DIQ010 + LBXVIDMS + ALQ130 + SMQ040)^2, data = df %>% 
                   mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                                 ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
                   mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
                            DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
                   filter(BMXBMI < 60) %>%
                   filter(SRI_no_imputation > -10) %>%
                   rowwise() %>%
                   mutate(
                     Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
                     Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
                   ) %>%
                   ungroup() %>%
                   mutate(BMXBMI_log = log(BMXBMI)) %>%
                   filter(LBXVIDMS < 250) %>%
                   filter(SMQ040 %in% c(1, 2, 3)) %>%
                   filter(ALQ130 %in% 1:82))

# Apply stepwise regression
tic()
stepwise_model <- stepAIC(full_model, direction = "both")
toc() # 23s
summary(stepwise_model) # Adjusted R-squared:  0.283
check_model(stepwise_model)  # Collinearity bad because of interaction?
check_model(stepwise_model, check = "pp_check") # slightly off, not worse than with splines
plot(residuals(stepwise_model)) # residuals look good
qqPlot(stepwise_model, main = "QQ Plot with Confidence Intervals") # slightly off, but better than the smaller models I think


# Create NAs were needed, now they are filtered out....
df_for_imputation <- df %>% 
  mutate(across(c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090), 
                ~ ifelse(. %in% c(7, 9), NA, .))) %>%  # Replace 7 and 9 with NA
  mutate(Depression_score_PHQ_9 = DPQ010 + DPQ020 + DPQ030 + DPQ040 + 
           DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090) %>%
  filter(BMXBMI < 60) %>% # Filter out extreme BMI values
  filter(SRI_no_imputation > -10) %>% # Filter out missing SRI values
  rowwise() %>%
  mutate(
    Avg_Systolic_BP = mean(c(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE), # Average systolic BP
    Avg_Diastolic_BP = mean(c(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE) # Average diastolic BP
  ) %>%
  ungroup() %>%
  mutate(BMXBMI_log = log(BMXBMI)) %>%  # Log-transformed BMI
  filter(LBXVIDMS < 250) %>%  # Vitamin D
  filter(SMQ040 %in% c(1, 2, 3)) %>%  # Smoking status
  filter(ALQ130 %in% 1:82) %>%  # Alcohol consumption
  dplyr::select(BMXBMI_log, RIDAGEYR, Depression_score_PHQ_9, SRI_no_imputation, RIAGENDR, RIDRETH3, LBDHDDSI, Avg_Systolic_BP, DIQ010, LBXVIDMS, ALQ130, SMQ040)

vis_miss(df_for_imputation) 



# __[slow, maybe avoid...]Save Workspace___----
#timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
#file_path <- paste0("/Users/juergen/Large_R_Files/R_Workspaces/R_workspace_", timestamp, ".RData")
#tic()
#save.image(file = file_path)
#toc() # 4 GB, 875.229 sec elapsed

# __SessionInfo for replicability__--------
session_info <- capture.output(sessionInfo())
file_name <- paste0("./SessionInfos_Replicability/3.0_EDA_",today(),".txt")
writeLines(session_info, con = file_name)

