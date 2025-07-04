# PAPER 2
# https://github.com/jdegenfellner/Paper2-dissertation/blob/main/1_Read_clean_prepare_git.R

# Run-through code at 17.11.24: no errors.

# Load packages----
library(pacman)
p_load(data.table, plyr, dplyr, 
       tidyverse, Hmisc, tictoc, vroom,
       arrow, duckdb, tictoc, patchwork)

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####
# 1) READ UBK-dataset ukb35288.tab-------
####

#sysinfo <- Sys.info()
path <- "/Users/juergen/Large_R_Files/UKBiobank/ukb35288.tab"

# Variant 1 or 2 definition for big 5 scales? ----
# Variant 1: "Correct version" adds NA if the definition for a point on a scale is not met
# Variant 2: adds a 0, as in: https://doi.org/10.1038/s41598-022-10573-6
v <- 1

#--
read_file_in_chunks <- function(path, chunk_size = 10000, delim = "\t", col_select = NULL) {
  chunks <- list()
  callback <- function(df, pos) {
    if (!is.null(col_select)) {
      df <- df %>% dplyr::select(all_of(col_select))
    }
    chunks[[length(chunks) + 1]] <<- df
  }
  readr::read_delim_chunked(
    file = path,
    callback = readr::DataFrameCallback$new(callback),
    delim = delim,
    chunk_size = chunk_size,
    progress = TRUE
  )
  return(chunks)
}

col_select = c("f.eid",        # id
               "f.21003.0.0",  # Age at assessment center
               "f.31.0.0",     # Sex
               "f.21000.0.0",  # Ethnic background
               "f.53.0.0",     # Date of first attending assessment centre
               "f.53.1.0",     # Date of 2. attending assessment centre
               "f.53.2.0",     # Date of 3. attending assessment centre
               #CURRENTLY MISSING::::"f.53.3.0",     # Date of 3. attending assessment centre
               "f.54.0.0",     # UK Biobank assessment centre, 22 different codes
               "f.189.0.0",    # Townsend deprivation index (7.9.24: not on website)
               "f.738.0.0",    # Average total household income before tax
               
               # Seven distinct domains have been identified in the
               # English Indices of Deprivation;
               # 1) Income Deprivation, 
               # 2) Employment Deprivation,
               # 3) Health Deprivation and Disability,
               # 4) Education Skills and Training Deprivation,
               # 5) Barriers to Housing and Services,
               # 6) Living Environment Deprivation, and 
               # 7) Crime
               
               # Current employment status
               "f.6142.0.0",   # Employment status
               
               # 1) Income scores
               "f.26411.0.0",  # Income score (England)
               "f.26418.0.0",  # Income score (Wales)
               "f.26428.0.0",  # Income score (Scotland)
               
               # 2) Employment score
               "f.26412.0.0",  # Employment score (England)
               "f.26419.0.0",  # Employment score (Wales)
               "f.26429.0.0",  # Employment score (Scotland)
               
               # 3) Health score
               "f.26413.0.0",  # Health score (England)
               "f.26420.0.0",  # Health score (Wales)
               "f.26430.0.0",  # Health score (Scotland)
               
               # 4) Education score
               "f.6138.0.0",   # Qualifications
               "f.26414.0.0",  # Education score (England)
               "f.26421.0.0",  # Education score (Wales)
               "f.26431.0.0",  # Education score (Scotland)
               
               # 5) Housing
               "f.26415.0.0",  # Housing score (England)
               "f.26423.0.0",  # Housing score (Wales)
               "f.26432.0.0",  # Housing score (Scotland)
               
               # 6) Living Environment score
               "f.26417.0.0",  # Living Environment score (England)
               "f.26424.0.0",  # Physical Environment score (Wales)
               
               # 7) Crime score
               "f.26416.0.0",  # Crime score (England)
               "f.26434.0.0",  # Crime score (Scotland)
               "f.26425.0.0",  # Community safety score (Wales)
               
               # Current SHIFT WORK # ----
               "f.3426.0.0",   # Night shift work "Does your work involve night shifts?" (Assessment Centre)
               "f.826.0.0",    # Job involves shift work
               "f.816.0.0",    # Job involves heavy manual or physical work
               # ..... more, see showcase ...
               
               # SHIFT WORK History # ----
               # Several years after joining UK Biobank participants were asked to
               # record a longer job history via a web questionnaire, the results of which are presented in Category 130.
               
               "f.22602.0.0",  # Job 0 started
               "f.22603.0.0",  # Job 0 ended
               "f.22620.0.0",  # Job 0 involved shift work (Online Follow-Up)
               "f.22630.0.0",  # Day shifts worked
               "f.22640.0.0",  # Mixture of day and night shifts worked
               "f.22641.0.0",  # Period spent working mix of day and night shifts
               "f.22642.0.0",  # Usual length of each night shift during mixed shift periods 
               "f.22643.0.0",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.0",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.0",  # Rest days during mixed shift periods
               "f.22650.0.0",  # Night shifts worked Job 0; "Did you work night shifts for the whole of this job?"
               "f.22651.0.0",  # Period spent working night shifts
               "f.22652.0.0",  # Usual length of each night shift during night shift periods
               "f.22653.0.0",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.0",  # Consecutive night shifts during night shift periods
               "f.22655.0.0",  # Rest days during night shift periods
               
               "f.22602.0.1",  # Job 1 started
               "f.22603.0.1",  # Job 1 ended
               "f.22620.0.1",  # Job 1 involved shift work (Online Follow-Up)
               "f.22630.0.1",  # Day shifts worked
               "f.22640.0.1",  # Mixture of day and night shifts worked
               "f.22641.0.1",  # Period spent working mix of day and night shifts
               "f.22642.0.1",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.1",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.1",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.1",  # Rest days during mixed shift periods
               "f.22650.0.1",  # Night shifts worked Job 1
               "f.22651.0.1",  # Period spent working night shifts
               "f.22652.0.1",  # Usual length of each night shift during night shift periods
               "f.22653.0.1",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.1",  # Consecutive night shifts during night shift periods
               "f.22655.0.1",  # Rest days during night shift periods
               
               "f.22602.0.2",  # Job 2 started
               "f.22603.0.2",  # Job 2 ended
               "f.22620.0.2",  # Job 2 involved shift work (Online Follow-Up)
               "f.22630.0.2",  # Day shifts worked
               "f.22640.0.2",  # Mixture of day and night shifts worked
               "f.22641.0.2",  # Period spent working mix of day and night shifts
               "f.22642.0.2",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.2",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.2",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.2",  # Rest days during mixed shift periods
               "f.22650.0.2",  # Night shifts worked Job 2
               "f.22651.0.2",  # Period spent working night shifts
               "f.22652.0.2",  # Usual length of each night shift during night shift periods
               "f.22653.0.2",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.2",  # Consecutive night shifts during night shift periods
               "f.22655.0.2",  # Rest days during night shift periods
               
               "f.22602.0.3",  # Job 3 started
               "f.22603.0.3",  # Job 3 ended
               "f.22620.0.3",  # Job 3 involved shift work (Online Follow-Up)
               "f.22630.0.3",  # Day shifts worked
               "f.22640.0.3",  # Mixture of day and night shifts worked
               "f.22641.0.3",  # Period spent working mix of day and night shifts
               "f.22642.0.3",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.3",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.3",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.3",  # Rest days during mixed shift periods
               "f.22650.0.3",  # Night shifts worked Job 3
               "f.22651.0.3",  # Period spent working night shifts
               "f.22652.0.3",  # Usual length of each night shift during night shift periods
               "f.22653.0.3",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.3",  # Consecutive night shifts during night shift periods
               "f.22655.0.3",  # Rest days during night shift periods
               
               "f.22602.0.4",  # Job 4 started
               "f.22603.0.4",  # Job 4 ended
               "f.22620.0.4",  # Job 4 involved shift work (Online Follow-Up)
               "f.22630.0.4",  # Day shifts worked
               "f.22640.0.4",  # Mixture of day and night shifts worked
               "f.22641.0.4",  # Period spent working mix of day and night shifts
               "f.22642.0.4",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.4",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.4",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.4",  # Rest days during mixed shift periods
               "f.22650.0.4",  # Night shifts worked Job 4
               "f.22651.0.4",  # Period spent working night shifts
               "f.22652.0.4",  # Usual length of each night shift during night shift periods
               "f.22653.0.4",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.4",  # Consecutive night shifts during night shift periods
               "f.22655.0.4",  # Rest days during night shift periods
               
               "f.22602.0.5",  # Job 5 started
               "f.22603.0.5",  # Job 5 ended
               "f.22620.0.5",  # Job 5 involved shift work (Online Follow-Up)
               "f.22630.0.5",  # Day shifts worked
               "f.22640.0.5",  # Mixture of day and night shifts worked
               "f.22641.0.5",  # Period spent working mix of day and night shifts
               "f.22642.0.5",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.5",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.5",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.5",  # Rest days during mixed shift periods
               "f.22650.0.5",  # Night shifts worked Job 5
               "f.22651.0.5",  # Period spent working night shifts
               "f.22652.0.5",  # Usual length of each night shift during night shift periods
               "f.22653.0.5",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.5",  # Consecutive night shifts during night shift periods
               "f.22655.0.5",  # Rest days during night shift periods
               
               "f.22602.0.6",  # Job 6 started
               "f.22603.0.6",  # Job 6 ended
               "f.22620.0.6",  # Job 6 involved shift work (Online Follow-Up)
               "f.22630.0.6",  # Day shifts worked
               "f.22640.0.6",  # Mixture of day and night shifts worked
               "f.22641.0.6",  # Period spent working mix of day and night shifts
               "f.22642.0.6",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.6",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.6",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.6",  # Rest days during mixed shift periods
               "f.22650.0.6",  # Night shifts worked Job 6
               "f.22651.0.6",  # Period spent working night shifts
               "f.22652.0.6",  # Usual length of each night shift during night shift periods
               "f.22653.0.6",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.6",  # Consecutive night shifts during night shift periods
               "f.22655.0.6",  # Rest days during night shift periods
               
               "f.22602.0.7",  # Job 7 started
               "f.22603.0.7",  # Job 7 ended
               "f.22620.0.7",  # Job 7 involved shift work (Online Follow-Up)
               "f.22630.0.7",  # Day shifts worked
               "f.22640.0.7",  # Mixture of day and night shifts worked
               "f.22641.0.7",  # Period spent working mix of day and night shifts
               "f.22642.0.7",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.7",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.7",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.7",  # Rest days during mixed shift periods
               "f.22650.0.7",  # Night shifts worked Job 7
               "f.22651.0.7",  # Period spent working night shifts
               "f.22652.0.7",  # Usual length of each night shift during night shift periods
               "f.22653.0.7",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.7",  # Consecutive night shifts during night shift periods
               "f.22655.0.7",  # Rest days during night shift periods
               
               "f.22602.0.8",  # Job 8 started
               "f.22603.0.8",  # Job 8 ended
               "f.22620.0.8",  # Job 8 involved shift work (Online Follow-Up)
               "f.22630.0.8",  # Day shifts worked
               "f.22640.0.8",  # Mixture of day and night shifts worked
               "f.22641.0.8",  # Period spent working mix of day and night shifts
               "f.22642.0.8",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.8",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.8",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.8",  # Rest days during mixed shift periods
               "f.22650.0.8",  # Night shifts worked Job 8
               "f.22651.0.8",  # Period spent working night shifts
               "f.22652.0.8",  # Usual length of each night shift during night shift periods
               "f.22653.0.8",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.8",  # Consecutive night shifts during night shift periods
               "f.22655.0.8",  # Rest days during night shift periods
               
               "f.22602.0.9",  # Job 9 started
               "f.22603.0.9",  # Job 9 ended
               "f.22620.0.9",  # Job 9 involved shift work (Online Follow-Up)
               "f.22630.0.9",  # Day shifts worked
               "f.22640.0.9",  # Mixture of day and night shifts worked
               "f.22641.0.9",  # Period spent working mix of day and night shifts
               "f.22642.0.9",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.9",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.9",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.9",  # Rest days during mixed shift periods
               "f.22650.0.9",  # Night shifts worked Job 9
               "f.22651.0.9",  # Period spent working night shifts
               "f.22652.0.9",  # Usual length of each night shift during night shift periods
               "f.22653.0.9",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.9",  # Consecutive night shifts during night shift periods
               "f.22655.0.9",  # Rest days during night shift periods
               
               "f.22602.0.10",  # Job 10 started
               "f.22603.0.10",  # Job 10 ended
               "f.22620.0.10",  # Job 10 involved shift work (Online Follow-Up)
               "f.22630.0.10",  # Day shifts worked
               "f.22640.0.10",  # Mixture of day and night shifts worked
               "f.22641.0.10",  # Period spent working mix of day and night shifts
               "f.22642.0.10",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.10",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.10",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.10",  # Rest days during mixed shift periods
               "f.22650.0.10",  # Night shifts worked Job 10
               "f.22651.0.10",  # Period spent working night shifts
               "f.22652.0.10",  # Usual length of each night shift during night shift periods
               "f.22653.0.10",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.10",  # Consecutive night shifts during night shift periods
               "f.22655.0.10",  # Rest days during night shift periods
               
               "f.22602.0.11",  # Job 11 started
               "f.22603.0.11",  # Job 11 ended
               "f.22620.0.11",  # Job 11 involved shift work (Online Follow-Up)
               "f.22630.0.11",  # Day shifts worked
               "f.22640.0.11",  # Mixture of day and night shifts worked
               "f.22641.0.11",  # Period spent working mix of day and night shifts
               "f.22642.0.11",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.11",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.11",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.11",  # Rest days during mixed shift periods
               "f.22650.0.11",  # Night shifts worked Job 11
               "f.22651.0.11",  # Period spent working night shifts
               "f.22652.0.11",  # Usual length of each night shift during night shift periods
               "f.22653.0.11",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.11",  # Consecutive night shifts during night shift periods
               "f.22655.0.11",  # Rest days during night shift periods
               
               "f.22602.0.12",  # Job 12 started
               "f.22603.0.12",  # Job 12 ended
               "f.22620.0.12",  # Job 12 involved shift work (Online Follow-Up)
               "f.22630.0.12",  # Day shifts worked
               "f.22640.0.12",  # Mixture of day and night shifts worked
               "f.22641.0.12",  # Period spent working mix of day and night shifts
               "f.22642.0.12",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.12",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.12",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.12",  # Rest days during mixed shift periods
               "f.22650.0.12",  # Night shifts worked Job 12
               "f.22651.0.12",  # Period spent working night shifts
               "f.22652.0.12",  # Usual length of each night shift during night shift periods
               "f.22653.0.12",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.12",  # Consecutive night shifts during night shift periods
               "f.22655.0.12",  # Rest days during night shift periods
               
               "f.22602.0.13",  # Job 13 started
               "f.22603.0.13",  # Job 13 ended
               "f.22620.0.13",  # Job 13 involved shift work (Online Follow-Up)
               "f.22630.0.13",  # Day shifts worked
               "f.22640.0.13",  # Mixture of day and night shifts worked
               "f.22641.0.13",  # Period spent working mix of day and night shifts
               "f.22642.0.13",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.13",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.13",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.13",  # Rest days during mixed shift periods
               "f.22650.0.13",  # Night shifts worked Job 13
               "f.22651.0.13",  # Period spent working night shifts
               "f.22652.0.13",  # Usual length of each night shift during night shift periods
               "f.22653.0.13",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.13",  # Consecutive night shifts during night shift periods
               "f.22655.0.13",  # Rest days during night shift periods
               
               "f.22602.0.14",  # Job 14 started
               "f.22603.0.14",  # Job 14 ended
               "f.22620.0.14",  # Job 14 involved shift work (Online Follow-Up)
               "f.22630.0.14",  # Day shifts worked
               "f.22640.0.14",  # Mixture of day and night shifts worked
               "f.22641.0.14",  # Period spent working mix of day and night shifts
               "f.22642.0.14",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.14",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.14",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.14",  # Rest days during mixed shift periods
               "f.22650.0.14",  # Night shifts worked Job 14
               "f.22651.0.14",  # Period spent working night shifts
               "f.22652.0.14",  # Usual length of each night shift during night shift periods
               "f.22653.0.14",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.14",  # Consecutive night shifts during night shift periods
               "f.22655.0.14",  # Rest days during night shift periods
               
               "f.22602.0.15",  # Job 15 started
               "f.22603.0.15",  # Job 15 ended
               "f.22620.0.15",  # Job 15 involved shift work (Online Follow-Up)
               "f.22630.0.15",  # Day shifts worked
               "f.22640.0.15",  # Mixture of day and night shifts worked
               "f.22641.0.15",  # Period spent working mix of day and night shifts
               "f.22642.0.15",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.15",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.15",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.15",  # Rest days during mixed shift periods
               "f.22650.0.15",  # Night shifts worked Job 15
               "f.22651.0.15",  # Period spent working night shifts
               "f.22652.0.15",  # Usual length of each night shift during night shift periods
               "f.22653.0.15",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.15",  # Consecutive night shifts during night shift periods
               "f.22655.0.15",  # Rest days during night shift periods
               
               "f.22602.0.16",  # Job 16 started
               "f.22603.0.16",  # Job 16 ended
               "f.22620.0.16",  # Job 16 involved shift work (Online Follow-Up)
               "f.22630.0.16",  # Day shifts worked
               "f.22640.0.16",  # Mixture of day and night shifts worked
               "f.22641.0.16",  # Period spent working mix of day and night shifts
               "f.22642.0.16",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.16",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.16",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.16",  # Rest days during mixed shift periods
               "f.22650.0.16",  # Night shifts worked Job 16
               "f.22651.0.16",  # Period spent working night shifts
               "f.22652.0.16",  # Usual length of each night shift during night shift periods
               "f.22653.0.16",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.16",  # Consecutive night shifts during night shift periods
               "f.22655.0.16",  # Rest days during night shift periods
               
               "f.22602.0.17",  # Job 17 started
               "f.22603.0.17",  # Job 17 ended
               "f.22620.0.17",  # Job 17 involved shift work (Online Follow-Up)
               "f.22630.0.17",  # Day shifts worked
               "f.22640.0.17",  # Mixture of day and night shifts worked
               "f.22641.0.17",  # Period spent working mix of day and night shifts
               "f.22642.0.17",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.17",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.17",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.17",  # Rest days during mixed shift periods
               "f.22650.0.17",  # Night shifts worked Job 17
               "f.22651.0.17",  # Period spent working night shifts
               "f.22652.0.17",  # Usual length of each night shift during night shift periods
               "f.22653.0.17",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.17",  # Consecutive night shifts during night shift periods
               "f.22655.0.17",  # Rest days during night shift periods
               
               "f.22602.0.18",  # Job 18 started
               "f.22603.0.18",  # Job 18 ended
               "f.22620.0.18",  # Job 18 involved shift work (Online Follow-Up)
               "f.22630.0.18",  # Day shifts worked
               "f.22640.0.18",  # Mixture of day and night shifts worked
               "f.22641.0.18",  # Period spent working mix of day and night shifts
               "f.22642.0.18",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.18",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.18",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.18",  # Rest days during mixed shift periods
               "f.22650.0.18",  # Night shifts worked Job 18
               "f.22651.0.18",  # Period spent working night shifts
               "f.22652.0.18",  # Usual length of each night shift during night shift periods
               "f.22653.0.18",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.18",  # Consecutive night shifts during night shift periods
               "f.22655.0.18",  # Rest days during night shift periods
               
               "f.22602.0.19",  # Job 19 started
               "f.22603.0.19",  # Job 19 ended
               "f.22620.0.19",  # Job 19 involved shift work (Online Follow-Up)
               "f.22630.0.19",  # Day shifts worked
               "f.22640.0.19",  # Mixture of day and night shifts worked
               "f.22641.0.19",  # Period spent working mix of day and night shifts
               "f.22642.0.19",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.19",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.19",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.19",  # Rest days during mixed shift periods
               "f.22650.0.19",  # Night shifts worked Job 19
               "f.22651.0.19",  # Period spent working night shifts
               "f.22652.0.19",  # Usual length of each night shift during night shift periods
               "f.22653.0.19",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.19",  # Consecutive night shifts during night shift periods
               "f.22655.0.19",  # Rest days during night shift periods
               
               "f.22602.0.20",  # Job 20 started
               "f.22603.0.20",  # Job 20 ended
               "f.22620.0.20",  # Job 20 involved shift work (Online Follow-Up)
               "f.22630.0.20",  # Day shifts worked
               "f.22640.0.20",  # Mixture of day and night shifts worked
               "f.22641.0.20",  # Period spent working mix of day and night shifts
               "f.22642.0.20",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.20",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.20",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.20",  # Rest days during mixed shift periods
               "f.22650.0.20",  # Night shifts worked Job 20
               "f.22651.0.20",  # Period spent working night shifts
               "f.22652.0.20",  # Usual length of each night shift during night shift periods
               "f.22653.0.20",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.20",  # Consecutive night shifts during night shift periods
               "f.22655.0.20",  # Rest days during night shift periods
               
               "f.22602.0.21",  # Job 21 started
               "f.22603.0.21",  # Job 21 ended
               "f.22620.0.21",  # Job 21 involved shift work (Online Follow-Up)
               "f.22630.0.21",  # Day shifts worked
               "f.22640.0.21",  # Mixture of day and night shifts worked
               "f.22641.0.21",  # Period spent working mix of day and night shifts
               "f.22642.0.21",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.21",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.21",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.21",  # Rest days during mixed shift periods
               "f.22650.0.21",  # Night shifts worked Job 21
               "f.22651.0.21",  # Period spent working night shifts
               "f.22652.0.21",  # Usual length of each night shift during night shift periods
               "f.22653.0.21",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.21",  # Consecutive night shifts during night shift periods
               "f.22655.0.21",  # Rest days during night shift periods
               
               "f.22602.0.22",  # Job 22 started
               "f.22603.0.22",  # Job 22 ended
               "f.22620.0.22",  # Job 22 involved shift work (Online Follow-Up)
               "f.22630.0.22",  # Day shifts worked
               "f.22641.0.22",  # Period spent working mix of day and night shifts
               "f.22640.0.22",  # Mixture of day and night shifts worked
               "f.22642.0.22",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.22",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.22",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.22",  # Rest days during mixed shift periods
               "f.22650.0.22",  # Night shifts worked Job 22
               "f.22651.0.22",  # Period spent working night shifts
               "f.22652.0.22",  # Usual length of each night shift during night shift periods
               "f.22653.0.22",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.22",  # Consecutive night shifts during night shift periods
               "f.22655.0.22",  # Rest days during night shift periods
               
               "f.22602.0.23",  # Job 23 started
               "f.22603.0.23",  # Job 23 ended
               "f.22620.0.23",  # Job 23 involved shift work (Online Follow-Up)
               "f.22630.0.23",  # Day shifts worked
               "f.22640.0.23",  # Mixture of day and night shifts worked
               "f.22641.0.23",  # Period spent working mix of day and night shifts
               "f.22642.0.23",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.23",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.23",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.23",  # Rest days during mixed shift periods
               "f.22650.0.23",  # Night shifts worked Job 23
               "f.22651.0.23",  # Period spent working night shifts
               "f.22652.0.23",  # Usual length of each night shift during night shift periods
               "f.22653.0.23",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.23",  # Consecutive night shifts during night shift periods
               "f.22655.0.23",  # Rest days during night shift periods
               
               "f.22602.0.24",  # Job 24 started
               "f.22603.0.24",  # Job 24 ended
               "f.22620.0.24",  # Job 24 involved shift work (Online Follow-Up)
               "f.22630.0.24",  # Day shifts worked
               "f.22640.0.24",  # Mixture of day and night shifts worked
               "f.22641.0.24",  # Period spent working mix of day and night shifts
               "f.22642.0.24",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.24",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.24",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.24",  # Rest days during mixed shift periods
               "f.22650.0.24",  # Night shifts worked Job 24
               "f.22651.0.24",  # Period spent working night shifts
               "f.22652.0.24",  # Usual length of each night shift during night shift periods
               "f.22653.0.24",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.24",  # Consecutive night shifts during night shift periods
               "f.22655.0.24",  # Rest days during night shift periods
               
               "f.22602.0.25",  # Job 25 started
               "f.22603.0.25",  # Job 25 ended
               "f.22620.0.25",  # Job 25 involved shift work (Online Follow-Up)
               "f.22630.0.25",  # Day shifts worked
               "f.22640.0.25",  # Mixture of day and night shifts worked
               "f.22641.0.25",  # Period spent working mix of day and night shifts
               "f.22642.0.25",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.25",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.25",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.25",  # Rest days during mixed shift periods
               "f.22650.0.25",  # Night shifts worked Job 25
               "f.22651.0.25",  # Period spent working night shifts
               "f.22652.0.25",  # Usual length of each night shift during night shift periods
               "f.22653.0.25",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.25",  # Consecutive night shifts during night shift periods
               "f.22655.0.25",  # Rest days during night shift periods
               
               "f.22602.0.26",  # Job 26 started
               "f.22603.0.26",  # Job 26 ended
               "f.22620.0.26",  # Job 26 involved shift work (Online Follow-Up)
               "f.22630.0.26",  # Day shifts worked
               "f.22640.0.26",  # Mixture of day and night shifts worked
               "f.22641.0.26",  # Period spent working mix of day and night shifts
               "f.22642.0.26",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.26",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.26",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.26",  # Rest days during mixed shift periods
               "f.22650.0.26",  # Night shifts worked Job 26
               "f.22651.0.26",  # Period spent working night shifts
               "f.22652.0.26",  # Usual length of each night shift during night shift periods
               "f.22653.0.26",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.26",  # Consecutive night shifts during night shift periods
               "f.22655.0.26",  # Rest days during night shift periods
               
               "f.22602.0.27",  # Job 27 started
               "f.22603.0.27",  # Job 27 ended
               "f.22620.0.27",  # Job 27 involved shift work (Online Follow-Up)
               "f.22630.0.27",  # Day shifts worked
               "f.22640.0.27",  # Mixture of day and night shifts worked
               "f.22641.0.27",  # Period spent working mix of day and night shifts
               "f.22642.0.27",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.27",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.27",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.27",  # Rest days during mixed shift periods
               "f.22650.0.27",  # Night shifts worked Job 27
               "f.22651.0.27",  # Period spent working night shifts
               "f.22652.0.27",  # Usual length of each night shift during night shift periods
               "f.22653.0.27",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.27",  # Consecutive night shifts during night shift periods
               "f.22655.0.27",  # Rest days during night shift periods
               
               "f.22602.0.28",  # Job 28 started
               "f.22603.0.28",  # Job 28 ended
               "f.22620.0.28",  # Job 28 involved shift work (Online Follow-Up)
               "f.22630.0.28",  # Day shifts worked
               "f.22640.0.28",  # Mixture of day and night shifts worked
               "f.22641.0.28",  # Period spent working mix of day and night shifts
               "f.22642.0.28",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.28",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.28",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.28",  # Rest days during mixed shift periods
               "f.22650.0.28",  # Night shifts worked Job 28
               "f.22651.0.28",  # Period spent working night shifts
               "f.22652.0.28",  # Usual length of each night shift during night shift periods
               "f.22653.0.28",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.28",  # Consecutive night shifts during night shift periods
               "f.22655.0.28",  # Rest days during night shift periods
               
               "f.22602.0.29",  # Job 29 started
               "f.22603.0.29",  # Job 29 ended
               "f.22620.0.29",  # Job 29 involved shift work (Online Follow-Up)
               "f.22630.0.29",  # Day shifts worked
               "f.22640.0.29",  # Mixture of day and night shifts worked
               "f.22641.0.29",  # Period spent working mix of day and night shifts
               "f.22642.0.29",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.29",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.29",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.29",  # Rest days during mixed shift periods
               "f.22650.0.29",  # Night shifts worked Job 29
               "f.22651.0.29",  # Period spent working night shifts
               "f.22652.0.29",  # Usual length of each night shift during night shift periods
               "f.22653.0.29",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.29",  # Consecutive night shifts during night shift periods
               "f.22655.0.29",  # Rest days during night shift periods
               
               "f.22602.0.30",  # Job 30 started
               "f.22603.0.30",  # Job 30 ended
               "f.22620.0.30",  # Job 30 involved shift work (Online Follow-Up)
               "f.22630.0.30",  # Day shifts worked
               "f.22640.0.30",  # Mixture of day and night shifts worked
               "f.22641.0.30",  # Period spent working mix of day and night shifts
               "f.22642.0.30",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.30",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.30",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.30",  # Rest days during mixed shift periods
               "f.22650.0.30",  # Night shifts worked Job 30
               "f.22651.0.30",  # Period spent working night shifts
               "f.22652.0.30",  # Usual length of each night shift during night shift periods
               "f.22653.0.30",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.30",  # Consecutive night shifts during night shift periods
               "f.22655.0.30",  # Rest days during night shift periods
               
               "f.22602.0.31",  # Job 31 started
               "f.22603.0.31",  # Job 31 ended
               "f.22620.0.31",  # Job 31 involved shift work (Online Follow-Up)
               "f.22630.0.31",  # Day shifts worked
               "f.22640.0.31",  # Mixture of day and night shifts worked
               "f.22641.0.31",  # Period spent working mix of day and night shifts
               "f.22642.0.31",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.31",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.31",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.31",  # Rest days during mixed shift periods
               "f.22650.0.31",  # Night shifts worked Job 31
               "f.22651.0.31",  # Period spent working night shifts
               "f.22652.0.31",  # Usual length of each night shift during night shift periods
               "f.22653.0.31",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.31",  # Consecutive night shifts during night shift periods
               "f.22655.0.31",  # Rest days during night shift periods
               
               "f.22602.0.32",  # Job 32 started
               "f.22603.0.32",  # Job 32 ended
               "f.22620.0.32",  # Job 32 involved shift work (Online Follow-Up)
               "f.22630.0.32",  # Day shifts worked
               "f.22640.0.32",  # Mixture of day and night shifts worked
               "f.22641.0.32",  # Period spent working mix of day and night shifts
               "f.22642.0.32",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.32",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.32",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.32",  # Rest days during mixed shift periods
               "f.22650.0.32",  # Night shifts worked Job 32
               "f.22651.0.32",  # Period spent working night shifts
               "f.22652.0.32",  # Usual length of each night shift during night shift periods
               "f.22653.0.32",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.32",  # Consecutive night shifts during night shift periods
               "f.22655.0.32",  # Rest days during night shift periods
               
               "f.22602.0.33",  # Job 33 started
               "f.22603.0.33",  # Job 33 ended
               "f.22620.0.33",  # Job 33 involved shift work (Online Follow-Up)
               "f.22630.0.33",  # Day shifts worked
               "f.22640.0.33",  # Mixture of day and night shifts worked
               "f.22641.0.33",  # Period spent working mix of day and night shifts
               "f.22642.0.33",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.33",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.33",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.33",  # Rest days during mixed shift periods
               "f.22650.0.33",  # Night shifts worked Job 33
               "f.22651.0.33",  # Period spent working night shifts
               "f.22652.0.33",  # Usual length of each night shift during night shift periods
               "f.22653.0.33",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.33",  # Consecutive night shifts during night shift periods
               "f.22655.0.33",  # Rest days during night shift periods
               
               "f.22602.0.34",  # Job 34 started
               "f.22603.0.34",  # Job 34 ended
               "f.22620.0.34",  # Job 34 involved shift work (Online Follow-Up)
               "f.22630.0.34",  # Day shifts worked
               "f.22640.0.34",  # Mixture of day and night shifts worked
               "f.22641.0.34",  # Period spent working mix of day and night shifts
               "f.22642.0.34",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.34",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.34",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.34",  # Rest days during mixed shift periods
               "f.22650.0.34",  # Night shifts worked Job 34
               "f.22651.0.34",  # Period spent working night shifts
               "f.22652.0.34",  # Usual length of each night shift during night shift periods
               "f.22653.0.34",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.34",  # Consecutive night shifts during night shift periods
               "f.22655.0.34",  # Rest days during night shift periods
               
               "f.22602.0.35",  # Job 35 started
               "f.22603.0.35",  # Job 35 ended
               "f.22620.0.35",  # Job 35 involved shift work (Online Follow-Up)
               "f.22630.0.35",  # Day shifts worked
               "f.22640.0.35",  # Mixture of day and night shifts worked
               "f.22641.0.35",  # Period spent working mix of day and night shifts
               "f.22642.0.35",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.35",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.35",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.35",  # Rest days during mixed shift periods
               "f.22650.0.35",  # Night shifts worked Job 35
               "f.22651.0.35",  # Period spent working night shifts
               "f.22652.0.35",  # Usual length of each night shift during night shift periods
               "f.22653.0.35",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.35",  # Consecutive night shifts during night shift periods
               "f.22655.0.35",  # Rest days during night shift periods
               
               "f.22602.0.36",  # Job 36 started
               "f.22603.0.36",  # Job 36 ended
               "f.22620.0.36",  # Job 36 involved shift work (Online Follow-Up)
               "f.22630.0.36",  # Day shifts worked
               "f.22640.0.36",  # Mixture of day and night shifts worked
               "f.22641.0.36",  # Period spent working mix of day and night shifts
               "f.22642.0.36",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.36",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.36",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.36",  # Rest days during mixed shift periods
               "f.22650.0.36",  # Night shifts worked Job 36
               "f.22651.0.36",  # Period spent working night shifts
               "f.22652.0.36",  # Usual length of each night shift during night shift periods
               "f.22653.0.36",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.36",  # Consecutive night shifts during night shift periods
               "f.22655.0.36",  # Rest days during night shift periods
               
               "f.22602.0.37",  # Job 37 started
               "f.22603.0.37",  # Job 37 ended
               "f.22620.0.37",  # Job 37 involved shift work (Online Follow-Up)
               "f.22630.0.37",  # Day shifts worked
               "f.22640.0.37",  # Mixture of day and night shifts worked
               "f.22641.0.37",  # Period spent working mix of day and night shifts
               "f.22642.0.37",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.37",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.37",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.37",  # Rest days during mixed shift periods
               "f.22650.0.37",  # Night shifts worked Job 37
               "f.22651.0.37",  # Period spent working night shifts
               "f.22652.0.37",  # Usual length of each night shift during night shift periods
               "f.22653.0.37",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.37",  # Consecutive night shifts during night shift periods
               "f.22655.0.37",  # Rest days during night shift periods
               
               "f.22602.0.38",  # Job 38 started
               "f.22603.0.38",  # Job 38 ended
               "f.22620.0.38",  # Job 38 involved shift work (Online Follow-Up)
               "f.22630.0.38",  # Day shifts worked
               "f.22640.0.38",  # Mixture of day and night shifts worked
               "f.22641.0.38",  # Period spent working mix of day and night shifts
               "f.22642.0.38",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.38",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.38",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.38",  # Rest days during mixed shift periods
               "f.22650.0.38",  # Night shifts worked Job 38
               "f.22651.0.38",  # Period spent working night shifts
               "f.22652.0.38",  # Usual length of each night shift during night shift periods
               "f.22653.0.38",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.38",  # Consecutive night shifts during night shift periods
               "f.22655.0.38",  # Rest days during night shift periods
               
               "f.22602.0.39",  # Job 39 started
               "f.22603.0.39",  # Job 39 ended
               "f.22620.0.39",  # Job 39 involved shift work (Online Follow-Up)
               "f.22630.0.39",  # Day shifts worked
               "f.22640.0.39",  # Mixture of day and night shifts worked
               "f.22641.0.39",  # Period spent working mix of day and night shifts
               "f.22642.0.39",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.39",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.39",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.39",  # Rest days during mixed shift periods
               "f.22650.0.39",  # Night shifts worked Job 39
               "f.22651.0.39",  # Period spent working night shifts
               "f.22652.0.39",  # Usual length of each night shift during night shift periods
               "f.22653.0.39",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.39",  # Consecutive night shifts during night shift periods
               "f.22655.0.39",  # Rest days during night shift periods
               
               "f.22602.0.40",  # Job 40 started
               "f.22603.0.40",  # Job 40 ended
               "f.22620.0.40",  # Job 40 involved shift work (Online Follow-Up)
               "f.22630.0.40",  # Day shifts worked
               "f.22640.0.40",  # Mixture of day and night shifts worked
               "f.22641.0.40",  # Period spent working mix of day and night shifts
               "f.22642.0.40",  # Usual length of each night shift during mixed shift periods
               "f.22643.0.40",  # Number of night shifts worked monthly during mixed shift periods
               "f.22644.0.40",  # Consecutive night shifts during mixed shift periods
               "f.22645.0.40",  # Rest days during mixed shift periods
               "f.22650.0.40",  # Night shifts worked Job 40
               "f.22651.0.40",  # Period spent working night shifts
               "f.22652.0.40",  # Usual length of each night shift during night shift periods
               "f.22653.0.40",  # Number of night shifts worked monthly during night shift periods
               "f.22654.0.40",  # Consecutive night shifts during night shift periods
               "f.22655.0.40",  # Rest days during night shift periods
               # up to 40 jobs were held (by one person) according to showcase
               # some are not in the data set: 33,34,35,36,37,38,39,40
               
               "f.757.0.0",    # Time employed in main current job (Assessment Centre), "How many years have you worked in your current job? (If you have more than one job please answer this, and the following questions on work, for your MAIN job only)"
               "f.767.0.0",    # Length of working week for main job
               "f.22599.0.0",  # Number of jobs held; https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=22599
               
               # SLEEP #----
               "f.1160.0.0",   # "About how many hours sleep do you get in every 24 hours? (please include naps)"
               "f.1200.0.0",   # Insomnia symptoms ("Do you have trouble falling asleep at night or do you wake up in the middle of the night?")
               "f.1220.0.0",   # "How likely are you to doze off or fall asleep during the daytime when you don't mean to? (e.g. when working, reading or driving)"
               "f.1180.0.0",   # Morning/evening person (chronotype)
               "f.20517.0.0",  # Trouble falling or staying asleep, or sleeping too much
               "f.20534.0.0",  # Sleeping too much
               "f.1190.0.0",   # nap during the day
               "f.1210.0.0",   # snoring
               
               # Lifestyle # ----
               "f.904.0.0",    # Number of days/week of vigorous physical activity 10+ minutes
               "f.884.0.0",    # Number of days/week of moderate physical activity 10+ minutes
               "f.22038.0.0",  # MET minutes per week for moderate activity
               "f.22039.0.0",  # MET minutes per week for vigorous activity
               "f.22040.0.0",  # Summed MET minutes per week for all activity
               "f.20116.0.0",  # Smoking status
               "f.3476.0.0",   # Difficulty not smoking for 1 day
               
               # DIET # ----
               "f.1558.0.0",   # Alcohol intake frequency
               "f.1498.0.0",   # "How many cups of coffee do you drink each DAY? (Include decaffeinated coffee)"
               "f.1488.0.0",   # Tea intake
               
               # DEPRESSION/Mental Health # ----
               "f.4526.0.0",   # Happiness (Extremely happy .... Very unhappy)
               "f.4537.0.0",   # Work/job satisfaction (Extremely happy .... Very unhappy)
               "f.4548.0.0",   # Health satisfaction (Extremely happy .... Very unhappy)
               "f.4559.0.0",   # Family relationship satisfaction (Extremely happy .... Very unhappy)
               "f.4570.0.0",   # Friendships satisfaction (Extremely happy .... Very unhappy)
               "f.4581.0.0",   # Financial situation satisfaction (Extremely happy .... Very unhappy)
               "f.20127.0.0",  # Neuroticism score
               "f.20126.0.0",  # Bipolar and major depression status
               "f.20124.0.0",  # Probable recurrent major depression (moderate)
               "f.20125.0.0",  # Probable recurrent major depression (severe)
               "f.2100.0.0",   # Seen a psychiatrist for nerves, anxiety, tension or depression
               "f.2090.0.0",   # Seen doctor (GP) for nerves, anxiety, tension or depression
               "f.20123.0.0",  # Single episode of probable major depression
               "f.20433.0.0",  # Age at first episode of depression
               "f.20434.0.0",  # Age at last episode of depression
               "f.20435.0.0",  # Difficulty concentrating during worst depression
               "f.20438.0.0",  # Duration of worst depression
               "f.20466.0.0",  # Ever had prolonged feelings of sadness or depression
               "f.20449.0.0",  # Feelings of tiredness during worst episode of depression
               "f.20450.0.0",  # Feelings of worthlessness during worst period of depression
               "f.20436.0.0",  # Fraction of day affected during worst episode of depression
               "f.20439.0.0",  # Frequency of depressed days during worst episode of depression
               "f.20440.0.0",  # Impact on normal roles during worst period of depression
               "f.4609.0.0",   # Longest period of depression
               "f.4620.0.0",   # Number of depression episodes
               "f.20448.0.0",  # Professional informed about depression
               "f.20510.0.0",  # Recent feelings of depression
               "f.20437.0.0",  # Thoughts of death during worst depression
               "f.20536.0.0",  # Weight change during worst episode of depression
               
               # Big Five Proxy-questions # ----
               # (see https://www.nature.com/articles/s41598-022-10573-6/tables/1)
               # SOCIABILITY
               "f.1031.0.0",   # Frequency of friend/family visit (>once a month); "How often do you visit friends or family or have them visit you?"
               "f.2030.0.0",   # Guilty feelings (no); "Are you often troubled by feelings of guilt?"
               "f.2080.0.0",   # Frequency of tiredness/lethargy in last 2 weeks (not at all); "Over the past two weeks, how often have you felt tired or had little energy?"
               "f.6160.0.0",   # "Which of the following do you attend once a week or more often? (You can select more than one)"
               # WARMTH
               "f.2110.0.0",   # Able to confide to (>once a month); "How often are you able to confide in someone close to you?"
               "f.1940.0.0",   # Irritability (no); "Are you an irritable person?"
               "f.1920.0.0",   # Mood swings (no); "Does your mood often go up and down?"
               "f.1990.0.0",   # Tense / 'highly strung' (no); "Would you call yourself tense or 'highly strung'?"
               "f.1970.0.0",   # Nervous feelings (no); "Would you call yourself a nervous person?"
               # DILIGENCE
               "f.2060.0.0",   # Frequency of enthusiasm/disinterest in last 2 weeks (not at all); "Over the past two weeks, how often have you had little interest or pleasure in doing things?"
               "f.1960.0.0",   # Fed-up feelings (no); "Do you often feel 'fed-up'?"
               "f.2040.0.0",   # Risk taking (no); "Would you describe yourself as someone who takes risks?"
               "f.2000.0.0",   # Worry too long after embarrassment (yes); "Do you worry too long after an embarrassing experience?"
               # CURIOSITY
               "f.2020.0.0",   # Loneliness, isolation (no); "Do you often feel lonely?"
               "f.2010.0.0",   # Suffer from 'nerves' (no); "Do you suffer from 'nerves'?"
               "f.2070.0.0",   # Frequency of tenseness/restlessness in last 2 weeks (>several days); "Over the past two weeks, how often have you felt tense, fidgety or restless?"
               # Risk taking (yes)
               # NERVOUSNESS (some of the fields were already included above)
               # Tense / 'highly strung' (yes)
               # Irritability (yes)
               # Frequency of enthusiasm/disinterest in last 2 weeks (>several days)
               # Mood swings (yes)
               "f.1950.0.0",   # Sensitivity / hurt feelings; "Are your feelings easily hurt?"
               
               # OTHER # ----
               "f.2724.0.0",   # Had menopause
               "f.21001.0.0"   # BMI
               
               # Testosterone----
               #"f.30850.0.0", # Testosterone
               #"f.30855.0.0", # Testosterone missing reason
               
               # Oestradiol----
               #"f.30800.0.0", # Oestradiol
               #"f.30805.0.0"  # Oestradiol missing reason
)

#stop("Really load fresh?")
load_data_fresh <- FALSE

# read fresh----
first_chunk <- fread(path, nrows = 5) # Read only first few rows
(available_columns <- colnames(first_chunk))
dim(first_chunk)
missing_columns <- setdiff(col_select, available_columns)

if(load_data_fresh){
  tic()
  df_list <- read_file_in_chunks(path, 
                                 col_select = intersect(col_select, available_columns))
  ukb <- bind_rows(df_list)
  ukb <- as_tibble(ukb)
  toc() # 237.865 sec elapsed
saveRDS(ukb, "./UKB_data/ukb.RDS")
}

dim(ukb) # 502525     545 
format(object.size(ukb), units = "GB") # 1.5 GB


#ukb$f.6138.0.0 # check, education

if(load_data_fresh == FALSE){
  ukb <- readRDS("./UKB_data/ukb.RDS")  
}
# -----END READ-----

dim(ukb) # 502525 # 545/566
names(ukb)

# Checking after read-in without any further steps:
sum(table(ukb$f.3426.0.0)) # 50277 check
table(ukb$f.3426.0.0) # night shift work question
# 3942 Usually, 7147 always
sum(ukb$f.3426.0.0 %in% c(3,4)) # 11089

# [Review round 1]: "1)	General analysis: Is there information on whether-------
# participants are working multiple jobs? If so, could you evaluate whether
# that may be a contributing factor for SWSD?"
# f.22599.0.0 # Number of jobs held; https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=22599
ukb$f.22599.0.0
barplot(table(ukb$f.22599.0.0))
ukb %>% ggplot(aes(x = f.22599.0.0)) + 
  geom_bar() + 
  labs(title = "Number of jobs held",
       x = "Number of jobs held",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

# We could try to work with the individual work history to find out more about
# jobs held at baseline and if participants had multiple jobs at the same time.
# "f.53.0.0",     # Date of first attending assessment centre

# We use:
# "f.22602.0.1",  # Job 1 started
# "f.22603.0.1",  # Job 1 ended
# "f.22620.0.1",  # Job 1 involved shift work (Online Follow-Up)
# "f.22630.0.1",  # Day shifts worked
# "f.22640.0.1",  # Mixture of day and night shifts worked
# "f.22641.0.1",  # Period spent working mix of day and night shifts
# "f.22642.0.1",  # Usual length of each night shift during mixed shift periods
# "f.22643.0.1",  # Number of night shifts worked monthly during mixed shift periods
# "f.22644.0.1",  # Consecutive night shifts during mixed shift periods
# "f.22645.0.1",  # Rest days during mixed shift periods
# "f.22650.0.1",  # Night shifts worked Job 1
# "f.22651.0.1",  # Period spent working night shifts
# "f.22652.0.1",  # Usual length of each night shift during night shift periods
# "f.22653.0.1",  # Number of night shifts worked monthly during night shift periods
# "f.22654.0.1",  # Consecutive night shifts during night shift periods
# "f.22655.0.1",  # Rest days during night shift periods

ukb$f.22650.0.1
table(ukb$f.22650.0.1)

# "f.53.0.0",     # Date of first attending assessment centre
# "f.53.1.0",     # Date of 2. attending assessment centre
# "f.53.2.0",     # Date of 3. attending assessment centre

# _When was the person at the assessment centre?----
ukb$f.53.0.0
ukb$date_first_attend_assessment_centre <- as.POSIXct(strptime(ukb$f.53.0.0, format = "%Y-%m-%d"))
ukb$date_second_attend_assessment_centre <- as.POSIXct(strptime(ukb$f.53.1.0, format = "%Y-%m-%d"))
ukb$date_third_attend_assessment_centre <- as.POSIXct(strptime(ukb$f.53.2.0, format = "%Y-%m-%d"))

# _Start and end of job 1----
ukb$f.22602.0.1 # start
ukb$f.22603.0.1 # end -> -313 ongoing! "-313 represents "Ongoing when data entered"
ukb$f.22602.0.2 # start
#... up to 40 jobs were held (by one person) according to showcase

# Interesting detail: 
unique(ukb$f.22603.0.1) # contains -131 which is not in the Data-Coding: https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=22603 https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100698

# We want find out if the person had multiple jobs at the same time at the time
# of attending the assessment center

# First we check, if the assessment center date is within the job period:
# Define the function to check job during assessment center visit
convert_year_to_date <- function(year) {
  if (!is.na(year) && year >= 1900) {  # Assuming valid years start from 1900
    return(as.POSIXct(paste0(year, "-01-01"), format = "%Y-%m-%d", tz = "UTC"))
  } else {
    return(NA)
  }
}

check_job_during_assessment <- function(job_start, job_end, assessment_date) {
  
  # Initial prints to check the values before any processing
  # print("Before conversion:")
  # print(paste("job_start:", job_start))
  # print(paste("job_end:", job_end))
  # print(paste("assessment_date (before conversion):", assessment_date))
  
  # Check for NA values in job_start or job_end
  if(is.na(job_start) | is.na(job_end)){
    #print("Either job_start or job_end is NA. Returning NA.")
    return(NA) # -> undecidable, if assessment center date is within a not-defined interval
  }
  
  # Convert job_start to date and check the conversion
  #job_start <- convert_year_to_date(job_start)
  #print("After conversion:")
  #print(paste("job_start (converted):", job_start))
  
  # Convert assessment_date to Date using the correct origin
  if (is.numeric(assessment_date)) {
    # If it's numeric, assume it's days since 1970-01-01
    assessment_date <- as.Date(assessment_date, origin = "1970-01-01")
  } else if (is.character(assessment_date)) {
    # If it's character, convert using as.Date or as.POSIXct
    assessment_date <- as.POSIXct(assessment_date, format = "%Y-%m-%d", tz = "UTC")
  }
  #print(paste("assessment_date (converted):", assessment_date))
  
  # Handle ongoing job case
  if (job_end == -313) {
    if (!is.na(job_start) & year(assessment_date) >= job_start) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    # Convert job_end to a valid range and check
    if (!is.na(job_start) & !is.na(job_end)) {
      result <- year(assessment_date) >= job_start & year(assessment_date) <= job_end
      return(result)
    }
  }
  
  # If none of the above conditions are met, return NA
  #print("Returning NA due to unmatched conditions.")
  return(NA)
}

# Example usage with a specific index to verify the format
ind <- 1
not_na_ind <- !is.na(ukb$f.22602.0.1) & !is.na(ukb$f.22603.0.1) & !is.na(ukb$date_first_attend_assessment_centre)
check_job_during_assessment(ukb$f.22602.0.1[not_na_ind][ind], 
                            ukb$f.22603.0.1[not_na_ind][ind], 
                            ukb$date_first_attend_assessment_centre[not_na_ind][ind])

# _Visualize non-missing job ends----
job <- "s" # 
non_missing_counts <- c()
for (i in 1:40) {
  if(job == "start"){
    column_name <- paste0("f.22602.0.", i) # starts of jobs
  } else {
    column_name <- paste0("f.22603.0.", i) # ends of jobs
  }
  if (column_name %in% colnames(ukb)) {
    non_missing_counts <- c(non_missing_counts, sum(!is.na(ukb[[column_name]])))
  } else {
    non_missing_counts <- c(non_missing_counts, NA)
  }
}
barplot(non_missing_counts, 
        main = "Number of Non-Missing Values per Column",
        xlab = "Column Index",
        ylab = "Number of Non-Missing Values",
        names.arg = 1:40,
        las = 2,
        col = "lightblue")
grid(nx = NA, ny = NULL)
which(non_missing_counts==0)-1 # = 14 jobs starts/ends maximally

ukb$date_first_attend_assessment_centre <- ukb$f.53.0.0

# _Job 0 dates----
tic()
ukb$job0_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.0,
                                      job_end = ukb$f.22603.0.0,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job0_during_assessment1 <- lapply(ukb$job0_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job0_during_assessment1 <- unlist(ukb$job0_during_assessment1)
length(ukb$job0_during_assessment1)


# _Job 1 dates----
tic()
ukb$job1_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.1,
                                      job_end = ukb$f.22603.0.1,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc() 
ukb$job1_during_assessment1 <- lapply(ukb$job1_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job1_during_assessment1 <- unlist(ukb$job1_during_assessment1)
length(ukb$job1_during_assessment1)


# Job 2 dates
tic()
ukb$job2_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.2,
                                      job_end = ukb$f.22603.0.2,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job2_during_assessment1 <- lapply(ukb$job2_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job2_during_assessment1 <- unlist(ukb$job2_during_assessment1)


# Job 3 dates
tic()
ukb$job3_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.3,
                                      job_end = ukb$f.22603.0.3,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job3_during_assessment1 <- lapply(ukb$job3_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job3_during_assessment1 <- unlist(ukb$job3_during_assessment1)



# Job 4 dates
tic()
ukb$job4_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.4,
                                      job_end = ukb$f.22603.0.4,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job4_during_assessment1 <- lapply(ukb$job4_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job4_during_assessment1 <- unlist(ukb$job4_during_assessment1)

# Job 5 dates
tic()
ukb$job5_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.5,
                                      job_end = ukb$f.22603.0.5,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job5_during_assessment1 <- lapply(ukb$job5_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job5_during_assessment1 <- unlist(ukb$job5_during_assessment1)


# Job 6 dates
tic()
ukb$job6_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.6,
                                      job_end = ukb$f.22603.0.6,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job6_during_assessment1 <- lapply(ukb$job6_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job6_during_assessment1 <- unlist(ukb$job6_during_assessment1)


# Job 7 dates
tic()
ukb$job7_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.7,
                                      job_end = ukb$f.22603.0.7,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job7_during_assessment1 <- lapply(ukb$job7_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job7_during_assessment1 <- unlist(ukb$job7_during_assessment1)


# Job 8 dates
tic()
ukb$job8_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.8,
                                      job_end = ukb$f.22603.0.8,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job8_during_assessment1 <- lapply(ukb$job8_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job8_during_assessment1 <- unlist(ukb$job8_during_assessment1)


# Job 9 dates
tic()
ukb$job9_during_assessment1 <- mapply(check_job_during_assessment,
                                      job_start = ukb$f.22602.0.9,
                                      job_end = ukb$f.22603.0.9,
                                      assessment_date = ukb$date_first_attend_assessment_centre,
                                      SIMPLIFY = FALSE)
toc()
ukb$job9_during_assessment1 <- lapply(ukb$job9_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job9_during_assessment1 <- unlist(ukb$job9_during_assessment1)


# Job 10 dates
tic()
ukb$job10_during_assessment1 <- mapply(check_job_during_assessment,
                                       job_start = ukb$f.22602.0.10,
                                       job_end = ukb$f.22603.0.10,
                                       assessment_date = ukb$date_first_attend_assessment_centre,
                                       SIMPLIFY = FALSE)
toc()
ukb$job10_during_assessment1 <- lapply(ukb$job10_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job10_during_assessment1 <- unlist(ukb$job10_during_assessment1)


# Job 11 dates
tic()
ukb$job11_during_assessment1 <- mapply(check_job_during_assessment,
                                       job_start = ukb$f.22602.0.11,
                                       job_end = ukb$f.22603.0.11,
                                       assessment_date = ukb$date_first_attend_assessment_centre,
                                       SIMPLIFY = FALSE)
toc()
ukb$job11_during_assessment1 <- lapply(ukb$job11_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job11_during_assessment1 <- unlist(ukb$job11_during_assessment1)

# Job 12 dates
tic()
ukb$job12_during_assessment1 <- mapply(check_job_during_assessment,
                                       job_start = ukb$f.22602.0.12,
                                       job_end = ukb$f.22603.0.12,
                                       assessment_date = ukb$date_first_attend_assessment_centre,
                                       SIMPLIFY = FALSE)
toc()
ukb$job12_during_assessment1 <- lapply(ukb$job12_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job12_during_assessment1 <- unlist(ukb$job12_during_assessment1)

# Job 13 dates
tic()
ukb$job13_during_assessment1 <- mapply(check_job_during_assessment,
                                       job_start = ukb$f.22602.0.13,
                                       job_end = ukb$f.22603.0.13,
                                       assessment_date = ukb$date_first_attend_assessment_centre,
                                       SIMPLIFY = FALSE)
toc()
ukb$job13_during_assessment1 <- lapply(ukb$job13_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job13_during_assessment1 <- unlist(ukb$job13_during_assessment1)

# _Job 14 dates----
tic()
ukb$job14_during_assessment1 <- mapply(check_job_during_assessment,
                                       job_start = ukb$f.22602.0.14,
                                       job_end = ukb$f.22603.0.14,
                                       assessment_date = ukb$date_first_attend_assessment_centre,
                                       SIMPLIFY = FALSE)
toc()
ukb$job14_during_assessment1 <- lapply(ukb$job14_during_assessment1, function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
})
ukb$job14_during_assessment1 <- unlist(ukb$job14_during_assessment1)




# _Who has how many jobs at assessment center visit 1?----
ukb$num_jobs_during_assessment <- rowSums(cbind( # This sums ub TRUE/FALSE entries from above
  ukb$job0_during_assessment1,
  ukb$job1_during_assessment1,
  ukb$job2_during_assessment1,
  ukb$job3_during_assessment1,
  ukb$job4_during_assessment1,
  ukb$job5_during_assessment1,
  ukb$job6_during_assessment1,
  ukb$job7_during_assessment1,
  ukb$job8_during_assessment1,
  ukb$job9_during_assessment1,
  ukb$job10_during_assessment1,
  ukb$job11_during_assessment1,
  ukb$job12_during_assessment1,
  ukb$job13_during_assessment1,
  ukb$job14_during_assessment1
), na.rm = TRUE)

length(ukb$num_jobs_during_assessment) # 502525
dim(ukb)

# Check the distribution of the number of simultaneous jobs
table(ukb$num_jobs_during_assessment)
#      0      1      2      3      4      5      6      7 
# 425241  74472   2720     79      6      3      3      1
sum(table(ukb$num_jobs_during_assessment)) # 502525

# Compare:
ukb$f.6142.0.0 <- factor(ukb$f.6142.0.0, levels = c(1, 2, 3, 4, 5, 6, 7, -7, -3), 
                         labels = c("In paid employment or self-employed", "Retired", 
                                    "Looking after home and/or family", "Unable to work because of sickness or disability", 
                                    "Unemployed", "Doing unpaid or voluntary work", 
                                    "Full or part-time student", "None of the above", 
                                    "Prefer not to answer"))
table(ukb$f.6142.0.0)
# Probably not working are: 
# Sum of categories that likely indicate 0 jobs
no_job_sum <- sum(table(ukb$f.6142.0.0)[c("Retired", 
                                          "Looking after home and/or family", 
                                          "Unable to work because of sickness or disability", 
                                          "Unemployed", 
                                          "None of the above", 
                                          "Prefer not to answer")])

cat("Number of individuals likely having 0 jobs:", no_job_sum, "\n")
sum(table(ukb$f.6142.0.0)) # 501654
sum(is.na(ukb$f.6142.0.0)) # 871)
sum(table(ukb$f.6142.0.0)) + sum(is.na(ukb$f.6142.0.0)) # 502525 -> check

# There is a discrepancy....
# Reason for the discrepancy is that only 120k people have answered the online-follow up:
sum(!is.na(ukb$f.22602.0.0)) # 120324

# Check individual cases which state zero jobs at assessment center but have jobs
# ukb$f.6142.0.0) == "In paid employment or self-employed" & ukb$num_jobs_during_assessment == 0
ukb %>% filter(f.6142.0.0 == "In paid employment or self-employed" & num_jobs_during_assessment == 0) %>% 
  dplyr::select(f.eid, 
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


# Night shift workers who participated in the online follow-up
ukb %>% 
  filter(!is.na(f.22602.0.0) & f.3426.0.0 %in% c(3,4)) %>% 
  pull(num_jobs_during_assessment) %>% 
  table()



# 1.1) Withdrawals-----

# _Read withdrawals----
(list_csv_files <- list.files(path = "./UKB_data/Withdrawals/", pattern = "\\.csv$"))
df <- readr::read_csv(paste0(getwd(),"/UKB_data/Withdrawals/",list_csv_files), col_names = FALSE)
df <- unique(df$X1) # now vector
df
length(df) # 654 are withdrawn from whole data set
saveRDS(df, "withdrawal_exclusion_IDs.RDS")

# _Exclude withdrawals--------
dim(ukb %>% filter(f.eid %in% setdiff(ukb$f.eid, df)))[1] - dim(ukb)[1] # -367 fall out overall

# [execute as check after night_shift_work is defined]
# How many will fall out within 50277 who answered the night shift work question?
#dim(ukb %>% filter(!is.na(night_shift_work))) # 50277
# vs.
#dim( ukb %>% filter(!is.na(night_shift_work)) %>%
#       filter(f.eid %in% setdiff(ukb$f.eid, df)) ) # 50229
#50277-50229 # 48

ukb <- ukb %>% filter(f.eid %in% setdiff(ukb$f.eid, df))
dim(ukb) # 502158

# Checking after excluding withdrawals:
sum(table(ukb$f.3426.0.0)) # 50229 check
table(ukb$f.3426.0.0) # night shift work question
# 3941 Usually, 7143 always
sum(ukb$f.3426.0.0 %in% c(3,4)) # 11084



# 2) PREPARE -------------------------------------------------------------------
ukb$date_attending_ass_centre <- as.POSIXct(strptime(ukb$f.53.0.0, format = "%Y-%m-%d"))

ukb$age <- ukb$f.21003.0.0 # age at assessment center
label(ukb$age) <- "Age at assessment center"

ukb <- ukb %>% mutate(sex = case_when(
  f.31.0.0 == 0 ~ "Female",
  f.31.0.0 == 1 ~ "Male"
)) # Note 0/1 were the only levels
ukb$sex <- as.factor(ukb$sex)
label(ukb$sex) <- "Sex"

# Ethnicity (old definition, before 25.2.2024)----
ukb <- ukb %>% mutate(ethnicity = case_when(
  f.21000.0.0 == 1001 | f.21000.0.0 == 1002 |  f.21000.0.0 == 1003 ~ "White",
  f.21000.0.0 == 2001 | f.21000.0.0 == 2002 |  f.21000.0.0 == 2003 | f.21000.0.0 == 2004 ~ "Mixed",
  f.21000.0.0 == 3001 | f.21000.0.0 == 3002 |  f.21000.0.0 == 3003 | f.21000.0.0 == 3004 ~ "Asian or Asian British",
  f.21000.0.0 == 4001 | f.21000.0.0 == 4002 |  f.21000.0.0 == 4003 | f.21000.0.0 == 4004 ~ "Black or Black British"
))
ukb$ethnicity <- factor(ukb$ethnicity, 
                        levels = c("White", "Mixed", "Asian or Asian British", "Black or Black British"))
label(ukb$ethnicity) <- "Ethnicity"

# Define ethnicity_new:  now also includes levels 5,6 (Chinese/other ethnic group)----
ukb <- ukb %>% mutate(ethnicity_new = case_when(
  f.21000.0.0 == 1 | f.21000.0.0 == 1001 | f.21000.0.0 == 1002 |  f.21000.0.0 == 1003 ~ "White",
  f.21000.0.0 == 2 | f.21000.0.0 == 2001 | f.21000.0.0 == 2002 |  f.21000.0.0 == 2003 | f.21000.0.0 == 2004 ~ "Mixed",
  f.21000.0.0 == 3 | f.21000.0.0 == 3001 | f.21000.0.0 == 3002 |  f.21000.0.0 == 3003 | f.21000.0.0 == 3004 ~ "Asian or Asian British",
  f.21000.0.0 == 4 | f.21000.0.0 == 4001 | f.21000.0.0 == 4002 |  f.21000.0.0 == 4003 | f.21000.0.0 == 4004 ~ "Black or Black British",
  f.21000.0.0 == 5 ~ "Chinese",
  f.21000.0.0 == 6 ~ "Other ethnic group"
))
ukb$ethnicity_new <- factor(ukb$ethnicity_new, 
                        levels = c("White", "Mixed", 
                                   "Asian or Asian British", 
                                   "Black or Black British",
                                   "Chinese",
                                   "Other ethnic group"))
label(ukb$ethnicity_new) <- "Ethnicity"

# __SHIFT WORK -----

# f.3426.0.0,   # Night shift work; Job involves night shift work, Assessment Center
ukb <- ukb %>% mutate(night_shift_work = case_when(
  f.3426.0.0 == 1 ~ "Never/rarely",
  f.3426.0.0 == 2 ~ "Sometimes",
  f.3426.0.0 == 3 ~ "Usually",
  f.3426.0.0 == 4 ~ "Always",
  f.3426.0.0 == -3 ~ "Prefer not to answer",
  f.3426.0.0 == -1 ~ "Dont know"
)
)
ukb$night_shift_work <- factor(ukb$night_shift_work, 
                               levels = c("Prefer not to answer", "Dont know",
                                          "Never/rarely","Sometimes", 
                                          "Usually","Always"))
sum(!is.na(ukb$night_shift_work)) # 50277
ukb$night_shift_work <- relevel( ukb$night_shift_work, ref = "Never/rarely")
label(ukb$night_shift_work) <- "Job involves night shift work"

table(ukb$night_shift_work)
sum(table(ukb$night_shift_work)) # 50277

table(ukb$f.3426.0.0) # nothing lost
sum(table(ukb$f.3426.0.0))
unique(ukb$f.3426.0.0)


# __SHIFT WORK history -----
# Field 22620:  Job involved shift work

# Time employed in main current job 757
# -10 (Less than a year)
# -3 (Prefer not to answer)
# -1 (Do not know)
ukb$time_employed_in_current_job <- ifelse(ukb$f.757.0.0 == -10, 0, ukb$f.757.0.0) # Assessment Center 2010; https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=757





# SLEEP # 

ukb <- ukb %>% mutate(chronotype = case_when(
  f.1180.0.0 == -3 ~ "Prefer not to answer",
  f.1180.0.0 == -1 ~ "Do not know",
  f.1180.0.0 == 1 ~ "Definitely a morning person",
  f.1180.0.0 == 2 ~ "More a morning than an evening person",
  f.1180.0.0 == 3 ~ "More an evening person than a morning person",
  f.1180.0.0 == 4 ~ "Definitely an evening person",
))
ukb$chronotype <- factor(ukb$chronotype,
                         levels = c("Prefer not to answer", "Do not know", 
                                    "Definitely a morning person", "More a morning than an evening person", 
                                    "More an evening person than a morning person", "Definitely an evening person"))
label(ukb$chronotype) <- "Chronotype"
ukb$chronotype <- relevel(ukb$chronotype, ref = "Definitely a morning person")

# Sleep duration
ukb$sleep_duration <- ukb$f.1160.0.0
sort(unique(ukb$f.1160.0.0)) # -3,-1

# dozing off
ukb <- ukb %>% mutate(dozing_off = case_when(
  f.1220.0.0 == 0 ~ "Never/rarely",
  f.1220.0.0 == 1 ~ "Sometimes",
  f.1220.0.0 == 2 ~ "Often",
  f.1220.0.0 == -1 ~ "Do not know",
  f.1220.0.0 == -3 ~ "Prefer not to answer",
  f.1220.0.0 == 3 ~ "All of the time",
))


# __Insomnia symptoms-------------------------------------------------------------
# Data-Field 1200: "Do you have trouble falling asleep at night or do you wake up in the middle of the night?"
ukb <- ukb %>% mutate(Sleeplessness_Insomnia = case_when(
  f.1200.0.0 == -3 ~ "Prefer not to answer",
  f.1200.0.0 == 1 ~ "Never/rarely",
  f.1200.0.0 == 2 ~ "Sometimes",
  f.1200.0.0 == 3 ~ "Usually"
))
ukb$Sleeplessness_Insomnia <- factor(ukb$Sleeplessness_Insomnia,
                                     levels = c("Prefer not to answer", "Never/rarely", "Sometimes", "Usually"))
label(ukb$Sleeplessness_Insomnia) <- "Do you have trouble falling asleep at night or do you wake up in the middle of the night?"


# ___FREQUENT insomnia symptoms ("never/rarely" versus "usually") ------
ukb <- ukb %>% mutate(freqIns = case_when(
  Sleeplessness_Insomnia == "Usually" ~ "Yes",
  Sleeplessness_Insomnia == "Never/rarely" ~ "No",
  Sleeplessness_Insomnia == "Prefer not to answer" ~ "Prefer not to answer"
))
ukb$freqIns <- factor(ukb$freqIns, 
                      levels = c("Prefer not to answer", "No", "Yes"))
label(ukb$freqIns) <- "Frequent insomnia symptoms"

# ___ANY insomnia symptoms ("never/rarely" versus "sometimes"/"usually") -----
ukb <- ukb %>% mutate(anyIns = case_when(
  Sleeplessness_Insomnia == "Usually" |  Sleeplessness_Insomnia == "Sometimes"~ "Yes",
  Sleeplessness_Insomnia == "Never/rarely" ~ "No",
  Sleeplessness_Insomnia == "Prefer not to answer" ~ "Prefer not to answer"
))
ukb$anyIns <- factor(ukb$anyIns,
                     levels = c("Prefer not to answer", "No","Yes"))
label(ukb$anyIns) <- "Any insomnia symptoms"

# ___HAMMERSCHLAG def. for InsomniaDisorder-------------------------------------
# (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5600256/)
# "Individuals that answered ‘usually’ were scored as cases and individuals 
# reporting ‘never/rarely’ or ‘sometimes’ were scored as controls."
ukb <- ukb %>% mutate(InsomniaDisorder = case_when(
  Sleeplessness_Insomnia == "Usually" ~ "Yes",
  Sleeplessness_Insomnia == "Never/rarely" | Sleeplessness_Insomnia == "Sometimes" ~ "No",
  Sleeplessness_Insomnia == "Prefer not to answer" ~ "Prefer not to answer"
))
ukb$InsomniaDisorder <- factor(ukb$InsomniaDisorder,
                     levels = c("Prefer not to answer", "No","Yes"))
label(ukb$InsomniaDisorder) <- "Insomnia Disorder (H)"

# __Personality Big-five proxies: ----

if(v == 1){
  # ___Variant 1 for Definition----
  # Scores with at least one question "Do not know"/"Prefer not to answer"
  # (i.e. <0) are not defined (NA)
  # For all variables, the responses "do not know" and "prefer not to answer" 
  # were coded as missing values.
  
  # "Sociability"
  ukb <- ukb %>% mutate(sociability_scale = 
                          ifelse(f.1031.0.0 <= 4,1,ifelse(f.1031.0.0 < 0,NA,0)) + # Frequency of friend/family visit (>=once a month)
                          ifelse(f.2030.0.0 == 0,1,ifelse(f.2030.0.0 < 0,NA,0)) + # Guilty feelings (no)
                          ifelse(f.2080.0.0 == 1,1,ifelse(f.2080.0.0 < 0,NA,0)) + # Frequency of tiredness/lethargy in last 2 weeks (not at all)
                          ifelse(f.6160.0.0 >= 1,1,ifelse(f.6160.0.0 == -7,NA,0))   # Leisure/social activities (-7 = None of the above)
  )
  
  # "Warmth"
  ukb <- ukb %>% mutate(warmth_scale = 
                          ifelse(f.2110.0.0 >= 2,1,ifelse(f.2110.0.0 < 0,NA,0)) + # Able to confide to (>=once a month)
                          ifelse(f.1940.0.0 == 0,1,ifelse(f.1940.0.0 < 0,NA,0)) + # Irritability (no)
                          ifelse(f.1920.0.0 == 0,1,ifelse(f.1920.0.0 < 0,NA,0)) + # Mood swings (no)
                          ifelse(f.1990.0.0 == 0,1,ifelse(f.1990.0.0 < 0,NA,0)) + # Tense / 'highly strung' (no)
                          ifelse(f.1970.0.0 == 0,1,ifelse(f.1970.0.0 < 0,NA,0))   # Nervous feelings (no)
  )
  
  # "Diligence"
  ukb <- ukb %>% mutate(diligence_scale = 
                          ifelse(f.2060.0.0 == 1,1,ifelse(f.2060.0.0 < 0,NA,0)) + # Frequency of enthusiasm/disinterest in last 2 weeks (not at all)
                          ifelse(f.1960.0.0 == 0,1,ifelse(f.1960.0.0 < 0,NA,0)) + # Fed-up feelings (no)
                          ifelse(f.2040.0.0 == 0,1,ifelse(f.2040.0.0 < 0,NA,0)) + # Risk taking (no)
                          ifelse(f.2000.0.0 == 1,1,ifelse(f.2000.0.0 < 0,NA,0))   # Worry too long after embarrassment (yes)
  )
  
  # "Curiosity"
  ukb <- ukb %>% mutate(curiosity_scale = 
                          ifelse(f.2020.0.0 == 0,1,ifelse(f.2020.0.0 < 0,NA,0)) + # Loneliness, isolation (no)
                          ifelse(f.2010.0.0 == 0,1,ifelse(f.2010.0.0 < 0,NA,0)) + # Suffer from 'nerves' (no)
                          ifelse(f.2070.0.0 %in% c(2,3,4),1,ifelse(f.2070.0.0 < 0,NA,0)) + # Frequency of tenseness/restlessness in last 2 weeks (<= nearly every day)
                          ifelse(f.2040.0.0 == 1,1,ifelse(f.2040.0.0 < 0,NA,0))   # Risk taking (yes)
  )
  
  # "Nervousness"
  ukb <- ukb %>% mutate(nervousness_scale = 
                          ifelse(f.1990.0.0 == 1,1,ifelse(f.1990.0.0 < 0,NA,0)) +  # Tense / 'highly strung' (yes)
                          ifelse(f.1940.0.0 == 1,1,ifelse(f.1940.0.0 < 0,NA,0)) +  # Irritability (yes)
                          ifelse(f.2060.0.0 %in% c(2,3,4),1,ifelse(f.2060.0.0 < 0,NA,0)) + # Frequency of enthusiasm/disinterest in last 2 weeks (<= nearly every day)
                          ifelse(f.1920.0.0 == 1,1,ifelse(f.1920.0.0 < 0,NA,0)) +  # Mood swings (yes)
                          ifelse(f.1950.0.0 == 1,1,ifelse(f.1950.0.0 < 0,NA,0))    # Sensitivity/hurt feelings (yes)
  )
}

if(v == 2){
  # ___Variant 2 for Definition----
  # If condition for adding a point is not fulfilled, a 0 is added instead of NA (see Variant 1)
  
  # "Sociability"
  ukb <- ukb %>% mutate(sociability_scale = 
                          ifelse(f.1031.0.0 <= 4,1,0) + # Frequency of friend/family visit (>once a month)
                          ifelse(f.2030.0.0 == 0,1,0) + # Guilty feelings (no)
                          ifelse(f.2080.0.0 == 1,1,0) + # Frequency of tiredness/lethargy in last 2 weeks (not at all)
                          ifelse(f.6160.0.0 >= 1,1,0)   # Leisure/social activities (-7 = None of the above)
  )
  
  # "Warmth"
  ukb <- ukb %>% mutate(warmth_scale = 
                          ifelse(f.2110.0.0 >= 2,1,0) + # Able to confide to (>once a month)
                          ifelse(f.1940.0.0 == 0,1,0) + # Irritability (no)
                          ifelse(f.1920.0.0 == 0,1,0) + # Mood swings (no)
                          ifelse(f.1990.0.0 == 0,1,0) + # Tense / 'highly strung' (no)
                          ifelse(f.1970.0.0 == 0,1,0)   # Nervous feelings (no)
  )
  
  # "Diligence"
  ukb <- ukb %>% mutate(diligence_scale = 
                          ifelse(f.2060.0.0 == 1,1,0) + # Frequency of enthusiasm/disinterest in last 2 weeks (not at all)
                          ifelse(f.1960.0.0 == 0,1,0) + # Fed-up feelings (no)
                          ifelse(f.2040.0.0 == 0,1,0) + # Risk taking (no)
                          ifelse(f.2000.0.0 == 1,1,0)   # Worry too long after embarrassment (yes)
  )
  
  # "Curiosity"
  ukb <- ukb %>% mutate(curiosity_scale = 
                          ifelse(f.2020.0.0 == 0,1,0) + # Loneliness, isolation (no)
                          ifelse(f.2010.0.0 == 0,1,0) + # Suffer from 'nerves' (no)
                          ifelse(f.2070.0.0 %in% c(2,3,4),1,0) + # Frequency of tenseness/restlessness in last 2 weeks (<= nearly every day)
                          ifelse(f.2040.0.0 == 1,1,0)   # Risk taking (yes)
  )
  
  # "Nervousness"
  ukb <- ukb %>% mutate(nervousness_scale = 
                          ifelse(f.1990.0.0 == 1,1,0) +  # Tense / 'highly strung' (yes)
                          ifelse(f.1940.0.0 == 1,1,0) +  # Irritability (yes)
                          ifelse(f.2060.0.0 %in% c(2,3,4),1,0) + # Frequency of enthusiasm/disinterest in last 2 weeks (<= nearly every day)
                          ifelse(f.1920.0.0 == 1,1,0) +  # Mood swings (yes)
                          ifelse(f.1950.0.0 == 1,1,0)    # Sensitivity/hurt feelings (yes)
  )
}


# __DEPRESSION/Mental health # ----

ukb$neuroticism_score <- ukb$f.20127.0.0
label(ukb$neuroticism_score) <- "Neuroticism score"

ukb <- ukb %>% mutate(happiness = case_when(
  f.4526.0.0 == -3 ~ "Prefer not to answer",
  f.4526.0.0 == -1 ~ "Do not know",
  f.4526.0.0 == 1 ~ "Extremely happy",
  f.4526.0.0 == 2 ~ "Very happy",
  f.4526.0.0 == 3 ~ "Moderately happy",
  f.4526.0.0 == 4 ~ "Moderately unhappy",
  f.4526.0.0 == 5 ~ "Very unhappy",
  f.4526.0.0 == 6 ~ "Extremely unhappy"
))
ukb$happiness <- factor(ukb$happiness,
                        levels = c("Prefer not to answer", "Do not know", "Extremely happy", 
                                   "Very happy", "Moderately happy", "Moderately unhappy", 
                                   "Very unhappy", "Extremely unhappy"))
label(ukb$happiness) <- "Happiness"

ukb <- ukb %>% mutate(work_job_satisfaction = case_when(
  f.4537.0.0 == -3 ~ "Prefer not to answer",
  f.4537.0.0 == -1 ~ "Do not know",
  f.4537.0.0 == 1 ~ "Extremely happy",
  f.4537.0.0 == 2 ~ "Very happy",
  f.4537.0.0 == 3 ~ "Moderately happy",
  f.4537.0.0 == 4 ~ "Moderately unhappy",
  f.4537.0.0 == 5 ~ "Very unhappy",
  f.4537.0.0 == 6 ~ "Extremely unhappy"
))
ukb$work_job_satisfaction <- factor(ukb$work_job_satisfaction,
                                    levels = c("Prefer_not_to_answer", "Do_not_know", "Extremely happy", 
                                               "Very happy", "Moderately happy", "Moderately unhappy", 
                                               "Very unhappy", "Extremely unhappy"))
label(ukb$work_job_satisfaction) <- "Work/job satisfaction"

ukb <- ukb %>% mutate(health_satisfaction = case_when(
  f.4548.0.0 == -3 ~ "Prefer not to answer",
  f.4548.0.0 == -1 ~ "Do not know",
  f.4548.0.0 == 1 ~ "Extremely happy",
  f.4548.0.0 == 2 ~ "Very happy",
  f.4548.0.0 == 3 ~ "Moderately happy",
  f.4548.0.0 == 4 ~ "Moderately unhappy",
  f.4548.0.0 == 5 ~ "Very unhappy",
  f.4548.0.0 == 6 ~ "Extremely unhappy"
))
ukb$health_satisfaction <- factor(ukb$health_satisfaction, 
                                  levels = c("Prefer_not_to_answer", "Do_not_know", "Extremely happy", 
                                             "Very happy", "Moderately happy", "Moderately unhappy", 
                                             "Very unhappy", "Extremely unhappy"))
label(ukb$health_satisfaction) <- "Health satisfaction"

ukb <- ukb %>% mutate(family_satisfaction = case_when(
  f.4559.0.0 == -3 ~ "Prefer not to answer",
  f.4559.0.0 == -1 ~ "Do not know",
  f.4559.0.0 == 1 ~ "Extremely happy",
  f.4559.0.0 == 2 ~ "Very happy",
  f.4559.0.0 == 3 ~ "Moderately happy",
  f.4559.0.0 == 4 ~ "Moderately unhappy",
  f.4559.0.0 == 5 ~ "Very unhappy",
  f.4559.0.0 == 6 ~ "Extremely unhappy"
))
ukb$family_satisfaction <- factor(ukb$family_satisfaction,
                                  levels = c("Prefer_not_to_answer", "Do_not_know", "Extremely happy", 
                                             "Very happy", "Moderately happy", "Moderately unhappy", 
                                             "Very unhappy", "Extremely unhappy"))
label(ukb$family_satisfaction) <- "Family satisfaction"

ukb <- ukb %>% mutate(friendships_satisfaction = case_when(
  f.4570.0.0 == -3 ~ "Prefer not to answer",
  f.4570.0.0 == -1 ~ "Do not know",
  f.4570.0.0 == 1 ~ "Extremely happy",
  f.4570.0.0 == 2 ~ "Very happy",
  f.4570.0.0 == 3 ~ "Moderately happy",
  f.4570.0.0 == 4 ~ "Moderately unhappy",
  f.4570.0.0 == 5 ~ "Very unhappy",
  f.4570.0.0 == 6 ~ "Extremely unhappy"
))
ukb$friendships_satisfaction <- factor(ukb$friendships_satisfaction,
                                       levels = c("Prefer_not_to_answer", "Do_not_know", "Extremely happy", 
                                                  "Very happy", "Moderately happy", "Moderately unhappy", 
                                                  "Very unhappy", "Extremely unhappy"))
label(ukb$friendships_satisfaction) <- "Friendships satisfaction"

ukb <- ukb %>% mutate(financial_situation_satisfaction = case_when(
  f.4581.0.0 == -3 ~ "Prefer not to answer",
  f.4581.0.0 == -1 ~ "Do not know",
  f.4581.0.0 == 1 ~ "Extremely happy",
  f.4581.0.0 == 2 ~ "Very happy",
  f.4581.0.0 == 3 ~ "Moderately happy",
  f.4581.0.0 == 4 ~ "Moderately unhappy",
  f.4581.0.0 == 5 ~ "Very unhappy",
  f.4581.0.0 == 6 ~ "Extremely unhappy"
))
ukb$financial_situation_satisfaction <- factor(ukb$financial_situation_satisfaction,
                                               levels = c("Prefer not to answer", "Do not know", "Extremely happy", 
                                                          "Very happy", "Moderately happy", "Moderately unhappy", 
                                                          "Very unhappy", "Extremely unhappy"))
label(ukb$financial_situation_satisfaction) <- "Financial situation satisfaction"

# Depression
ukb$longest_period_of_depression <- ukb$f.4609.0.0
ukb$number_of_depression_episodes <- ukb$f.4620.0.0
ukb$doctor_nerves_anxiety_tension_depression <- ukb$f.2090.0.0 # Seen doctor (GP) for nerves, anxiety, tension or depression

# __Lifestyle ----
ukb <- ukb %>% mutate(smoking_status = case_when(
  f.20116.0.0 == -3 ~ "Prefer not to answer",
  f.20116.0.0 == 0 ~ "Never",
  f.20116.0.0 == 1 ~ "Previous",
  f.20116.0.0 == 2 ~ "Current"
)) # 1 case of -121 -> NOT defined in the UKB: https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=90 
ukb$smoking_status <- factor(ukb$smoking_status,
                             levels = c("Prefer not to anwer", "Never", "Previous", "Current"))
label(ukb$smoking_status) <- "Smoking status"

ukb <- ukb %>% mutate(difficulty_not_smoking_for_a_day = case_when(
  f.3476.0.0 == -3 ~ "Prefer not to answer",
  f.3476.0.0 == 1 ~ "Very easy",
  f.3476.0.0 == 2 ~ "Fairly easy",
  f.3476.0.0 == 3 ~ "Fairly difficult",
  f.3476.0.0 == 4 ~ "Very difficult"
))
ukb$difficulty_not_smoking_for_a_day <- factor(ukb$difficulty_not_smoking_for_a_day,
                                               levels = c("Prefer not to answer", "Very easy",
                                                          "Fairly easy", "Fairly difficult",
                                                          "Very difficult"))
label(ukb$difficulty_not_smoking_for_a_day) <- "Difficulty not smoking for 1 day"

# __Summed MET minutes per week for all activity----
#hist(ukb$f.22040.0.0)
#sum(ukb$f.22040.0.0 < 0, na.rm = TRUE) # 0
ukb$MET_minutes_week <- ukb$f.22040.0.0

# __Diet ----

ukb$coffee_intake <- ukb$f.1498.0.0
label(ukb$coffee_intake) <- "Coffee intake"
table(ukb$coffee_intake) # what about -3/-1? -> not in model anyways.

ukb <- ukb %>% mutate(alcohol_intake_frequency = case_when(
  f.1558.0.0 == -3 ~ "Prefer not to answer",
  f.1558.0.0 == 1 ~ "Daily or almost daily",
  f.1558.0.0 == 2 ~ "Three or four times a week",
  f.1558.0.0 == 3 ~ "Once or twice a week",
  f.1558.0.0 == 4 ~ "One to three times a week",
  f.1558.0.0 == 5 ~ "Special occasions only",
  f.1558.0.0 == 6 ~ "Never"
))
ukb$alcohol_intake_frequency <- factor(ukb$alcohol_intake_frequency,
                                       levels = c("Prefer not to answer","Never","Special occasions only",
                                                  "Once or twice a week","One to three times a week", 
                                                  "Three or four times a week",  "Daily or almost daily")
)
ukb$alcohol_intake_frequency <- relevel( ukb$alcohol_intake_frequency, ref = "Never")
label(ukb$alcohol_intake_frequency) <- "Alcohol intake frequency"

# Other # 
unique(ukb$f.21001.0.0)
ukb$BMI <- ukb$f.21001.0.0
label(ukb$BMI) <- "BMI"


# __DEFINITIONS of night shift work disorder (NSWD) ----

# ___SWD_1 Sometimes (or more) night shift work and not any insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_1 = case_when(
  (night_shift_work == "Sometimes" |  night_shift_work == "Usually" | night_shift_work == "Always") & anyIns == "No" ~ "No",
  (night_shift_work == "Sometimes" |  night_shift_work == "Usually" | night_shift_work == "Always") & anyIns == "Yes" ~ "Yes"
)) 
ukb$SWD_1 <- as.factor(ukb$SWD_1)
ukb$SWD_1 <- relevel( ukb$SWD_1, "No" )
label(ukb$SWD_1) <- "SWD_1: Sometimes (or more) night shift work and not any insomnia symptoms"
table(ukb$SWD_1)
table(ukb$SWD_1)[2]/sum(table(ukb$SWD_1))

# ___SWD_2 Usually (or more) night shift work and not any insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_2 = case_when(
  (night_shift_work == "Usually" | night_shift_work == "Always") & anyIns == "No" ~ "No",
  (night_shift_work == "Usually" | night_shift_work == "Always") & anyIns == "Yes" ~ "Yes"
)) 
ukb$SWD_2 <- as.factor(ukb$SWD_2)
ukb$SWD_2 <- relevel( ukb$SWD_2, "No" )
label(ukb$SWD_2) <- "SWD_2: Usually (or more) night shift work and not any insomnia symptoms" 
table(ukb$SWD_2)
table(ukb$SWD_2)[2]/sum(table(ukb$SWD_2))

# ___SWD_3 Always night shift work and not any insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_3 = case_when(
  night_shift_work == "Always" & anyIns == "No" ~ "No",
  night_shift_work == "Always" & anyIns == "Yes" ~ "Yes"
)) 
ukb$SWD_3 <- as.factor(ukb$SWD_3)
ukb$SWD_3 <- relevel( ukb$SWD_3, "No" )
label(ukb$SWD_3) <- "SWD_3: Always night shift work and not any insomnia symptoms"
table(ukb$SWD_3)
table(ukb$SWD_3)[2]/sum(table(ukb$SWD_3))

# ___SWD_4 Sometimes (or more) night shift work and no frequent insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_4 = case_when(
  (night_shift_work == "Sometimes" |  night_shift_work == "Usually" | night_shift_work == "Always") & freqIns == "No" ~ "No",
  (night_shift_work == "Sometimes" |  night_shift_work == "Usually" | night_shift_work == "Always") & freqIns == "Yes" ~ "Yes"
)) 
ukb$SWD_4 <- as.factor(ukb$SWD_4)
ukb$SWD_4 <- relevel( ukb$SWD_4, "No" )
label(ukb$SWD_4) <- "SWD_4: Sometimes (or more) night shift work and no frequent insomnia symptoms"
table(ukb$SWD_4)
table(ukb$SWD_4)[2]/sum(table(ukb$SWD_4))

# ___SWD_5 Usually (or more) night shift work and no frequent insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_5 = case_when(
  (night_shift_work == "Usually" | night_shift_work == "Always") & freqIns == "No" ~ "No",
  (night_shift_work == "Usually" | night_shift_work == "Always") & freqIns == "Yes" ~ "Yes"
)) 
ukb$SWD_5 <- as.factor(ukb$SWD_5)
ukb$SWD_5 <- relevel( ukb$SWD_5, "No" )
label(ukb$SWD_5) <- "SWD_5: Usually (or more) night shift work and no frequent insomnia symptoms"
table(ukb$SWD_5)
table(ukb$SWD_5)[2]/sum(table(ukb$SWD_5))

# ___SWD_6 Always night shift work and no frequent insomnia symptoms ----
ukb <- ukb %>% mutate(SWD_6 = case_when(
  night_shift_work == "Always" & freqIns == "No" ~ "No",
  night_shift_work == "Always" & freqIns == "Yes" ~ "Yes"
)) 
ukb$SWD_6 <- as.factor(ukb$SWD_6)
ukb$SWD_6 <- relevel( ukb$SWD_6, "No" )
label(ukb$SWD_6) <- "SWD_6: Always night shift work and no frequent insomnia symptoms"
table(ukb$SWD_6)
table(ukb$SWD_6)[2]/sum(table(ukb$SWD_6))

# Ordinal versions of SWD:

# a) Always night shift work
ukb <- ukb %>% mutate(SWD_ord1 = case_when(
  night_shift_work == "Always" & freqIns == "No" & anyIns == "No" ~ "Tolerant",
  night_shift_work == "Always" & is.na(freqIns) & anyIns == "Yes" ~ "Moderately tolerant",
  night_shift_work == "Always" & freqIns == "Yes" & anyIns == "Yes" ~ "Intolerant"
)) 
ukb$SWD_ord1 <- factor(ukb$SWD_ord1, levels=c("Intolerant", "Moderately tolerant", "Tolerant"))
label(ukb$SWD_ord1) <- "SWD_ord1: disorder levels for Always night shift"
table(ukb$SWD_ord1)
table(ukb$SWD_ord1)[2]/sum(table(ukb$SWD_ord1))

# b) At least Usually night shift work
ukb <- ukb %>% mutate(SWD_ord2 = case_when(
  (night_shift_work == "Always" | night_shift_work == "Usually") & freqIns == "No" & anyIns == "No" ~ "Tolerant",
  (night_shift_work == "Always" | night_shift_work == "Usually") & is.na(freqIns) & anyIns == "Yes" ~ "Moderately tolerant",
  (night_shift_work == "Always" | night_shift_work == "Usually") & freqIns == "Yes" & anyIns == "Yes" ~ "Intolerant"
)) 
ukb$SWD_ord2 <- factor(ukb$SWD_ord2, levels=c("Intolerant", "Moderately tolerant", "Tolerant"))
label(ukb$SWD_ord2) <- "SWD_ord2: disorder levels for at least sometimes night shift"
table(ukb$SWD_ord2)
table(ukb$SWD_ord2)[2]/sum(table(ukb$SWD_ord2))

# b) At least Sometimes night shift work
ukb <- ukb %>% mutate(SWD_ord3 = case_when(
  (night_shift_work == "Always" | night_shift_work == "Usually" | night_shift_work == "Sometimes") & freqIns == "No" & anyIns == "No" ~ "Tolerant",
  (night_shift_work == "Always" | night_shift_work == "Usually" | night_shift_work == "Sometimes") & is.na(freqIns) & anyIns == "Yes" ~ "Moderately tolerant",
  (night_shift_work == "Always" | night_shift_work == "Usually" | night_shift_work == "Sometimes") & freqIns == "Yes" & anyIns == "Yes" ~ "Intolerant"
)) 
ukb$SWD_ord3 <- factor(ukb$SWD_ord3, levels=c("Intolerant", "Moderately tolerant", "Tolerant"))
label(ukb$SWD_ord3) <- "SWD_ord3: disorder levels for at least sometimes night shift"
table(ukb$SWD_ord3)
table(ukb$SWD_ord3)[2]/sum(table(ukb$SWD_ord3))


# no "any insomnia" symptoms show 28% of night shift workers (irrespective of NSW frequency)
# no "frequent insomnia symptoms show 53% of night shift workers (irrespective of NSW frequency)



# 3) CLEAN ---------------------------------------------------------------------

# coffee
# -10 represents "Less than one"
# -1 represents "Do not know"
# -3 represents "Prefer not to answer"
ukb <- as.data.table(ukb)
ukb[coffee_intake == -10,]$coffee_intake <- 0
ukb[coffee_intake %in% c(-3,-1),]$coffee_intake <- as.numeric(NA)



# Drop variables with very low count:
#ukb <- ukb %>% dplyr::select(-f.20123.0.0, # Single episode of probable major depression
#                             -f.20125.0.0, # Probable recurrent major depression (severe)
#                             -f.20124.0.0) # Probable recurrent major depression (moderate)




# 4) SAVE ----------------------------------------------------------------------
setwd("./UKB_data/")
filename <- paste0("ukb_cleaned_v",v,"b5_", date(),".RDS")
filename <- gsub(" ", "_", filename)
filename <- gsub(":", ".", filename)
saveRDS(ukb, file=filename) 

# Infos about R and Packages----
#sessionInfo()
# Code runs without ANY errors on
today() # "2024-11-17"
filename
writeLines(capture.output(sessionInfo()), "session_info_17.11.24.txt")

