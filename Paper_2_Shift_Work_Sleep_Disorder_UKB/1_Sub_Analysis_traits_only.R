# 1_Sub_Analysis_traits_only.R

# TODO update

# Load packages ----
load_packages <- function(){
  library(data.table)
  library(plyr)
  library(dplyr)
  library(tidyverse)
  library(ggmosaic)
  #library(DataExplorer)
  library(crosstable)
  library(mgcv)
  library(fancycut)
  library(pscl) # contains McFadden's R-squared for model, pR2()
  library(sure)
  library(blorr)
  #BiocManager::install("limma")
  #library(MKmisc)
  #library(ResourceSelection)
  library(glmtoolbox)
  library(caret)
  #library(pROC)
  #library(ROCR)
  library(naniar)
  #library(skimr)
  library(visdat)
  library(Hmisc) # Harrell Miscellaneous
  #reptools.R (Harrell)
  #source("C:/Users/admin/Meine Ablage/AAA_Meduni/AAA_Doktoratsstudium_Scient_Med_Application/Paper2_/R_Codes_and_data/reptools.R")
  #getRs('reptools.r')
  #library(rms)   # rms: Regression Modeling Strategies
  library(mice)  # Multivariate Imputation by Chained Equations
  library(miceFast)
  library(micemd)
  library(gtsummary)
  library(flextable)
  library(quarto) # Quarto? is an open-source scientific and technical publishing system built on Pandoc
  library(tictoc)
  library(MASS)
  library(officer)
  library(corrgram)
  library(rFSA)
  library(miceadds)
  library(remotes)
  #remotes::install_github("Sebastien-Le/YesSiR")
  library(YesSiR) # Export a flextable into a .xlsx file
  # Set working directory to source file location:
  #setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  library(janitor)
}
load_packages()

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Variant 1 or 2 definition for big 5 scales? ----
# Variant 1: "Correct version" adds NA if answer 'Prefer not to answer'/'Do not know' was given;
# we deem this to be the correct way to deal with these scores
# Variant 2: adds a 0, as was done in: https://doi.org/10.1038/s41598-022-10573-6
v <- 1

# 1) READ data ----
file_chosen <- file.choose()
ukb <- readRDS(file_chosen) # Note that choose.files() only works in Windows

# Traits only:
ukb_reg <- ukb %>% dplyr::select(
  
  # Items for definition of shift work disorder (SWD)
  SWD_2, # Usually (or more) night shift work and not 'any insomnia symptoms' 
  SWD_5, # Usually (or more) night shift work and no 'frequent insomnia symptoms'
  night_shift_work,
  
  # Traits
  age, 
  sex,
  ethnicity,
  chronotype,
  neuroticism_score, # Debut: Jan 2015, would explain missingness?
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
  f.1950.0.0
) %>% 
  filter(age <= 65, 
         night_shift_work == "Always" |  night_shift_work == "Usually",
         !is.na(SWD_5)) %>% # CAUTION with !is.na(SWD_XX), depending on analysis!
  filter(chronotype != "Prefer not to answer")

dim(ukb_reg) # 5461 (traits only; freq.insomnia/SWD_5)

#vis_dat(ukb_reg)
p <- vis_miss(ukb_reg)
p + theme(
  plot.margin = margin(1, 4, 1, 1, "cm"),
)

# _I) (Compl.Case) Classical logistic regression ----
model <- glm(SWD_5 ~ 
               ## Traits (from SWT literature) ## 
               age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  # (caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale,
               data = ukb_reg, family = "binomial") # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
#dim(na.omit(ukb_reg))[1] # 1952
#dim(ukb_reg)[1]
#paste("Lost",round((dim(ukb_reg)[1]-dim(na.omit(ukb_reg))[1])/dim(ukb_reg)[1]*100,2),"percent of cases.")

# __(Compl.Case) Model fit? -----
nullmod <- glm(SWD_5 ~ 1, data = ukb_reg, family = "binomial")
1-logLik(model)/logLik(nullmod)

pR2(model)['McFadden'] # 0.113724
pR2(model)
1-pR2(model)['llh']/pR2(model)['llhNull'] # log likelihood of null differs!
logLik(model) 
logLik(nullmod) 

library(DescTools)
PseudoR2(model, which = NULL) # 0.1137249 

library(performance)
r2_mcfadden(model) # 0.617


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
ft


if(v == 2){
  saveRDS(df, "./UKB_data/complete_case_traits_only_v2b5.RDS")  
}
if(v == 1){
  saveRDS(df, "./UKB_data/complete_case_traits_only_v1b5.RDS")
}


# TODO ... imputation und co 
