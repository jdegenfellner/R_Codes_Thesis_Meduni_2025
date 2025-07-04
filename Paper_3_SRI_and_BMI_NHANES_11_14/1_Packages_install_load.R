# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages---------------
library(pacman)

# Are you using the package sleepreg?
sleepreg_use <- FALSE

if(sleepreg_use){
  remove.packages("GGIR")
  remotes::install_version("GGIR", version = "2.0.0") # sleepreg needs 2.0.0, 
  # if error -> restart RStudio and try again
  devtools::install_github("dpwindred/sleepreg")
  # if asked: do not update any packages
  #devtools::install_github("andrew-leroux/rnhanesdata")
  #devtools::install_github("warnes/SASxport")
  #devtools::install_github("xinyue-L/hmmacc")
  #devtools::install_github("mayer79/missRanger")
  
}

pacman::p_load(readxl, tidyverse, ggeffects, ggpubr, GGally, boot, data.table, broom, car, leaps, 
               flowchart, purrr, pbapply, corrplot, lubridate, scales, writexl, visdat, gtsummary,
               imputeTS, parallel, survey, flowchart, infotheo, entropy, survey, emmeans,
               haven, tictoc, sleephmm, here, missRanger, Amelia, mi, mtsdi,
               Hmisc, Rfast, ks, GGIR, readr, VIM, performance, DescTools, MASS,
               rnhanesdata, # tools for processing the NHANES 2003-2004 and 2005-2006 waves' accelerometry data as well as the resulting processed data. 
               nhanesA, # https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
               NHANES, # NHANES 2009-2012 with adjusted weighting
               #sleepreg,
               SASxport, foreign, knitr, accelmissing, keras, ggdag, dagitty,
               flextable, officer, tidyr, gt, interactions, conflicted)


# __SessionInfo for replicability__--------
session_info <- capture.output(sessionInfo())
file_name <- paste0("./SessionInfos_Replicability/1_Packages_install_load_",today(),".txt")
writeLines(session_info, con = file_name)
