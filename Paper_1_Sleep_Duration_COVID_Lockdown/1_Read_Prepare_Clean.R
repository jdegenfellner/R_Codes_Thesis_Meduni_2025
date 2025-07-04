# 1_Read_Prepare_Clean.R # 
# data sets: sleep surveys 2017/2020 conducted by the Medical University of Vienna

pacman::p_load(
  haven, data.table, foreign, Hmisc, plyr, 
  tidyverse, ggpubr, fancycut, lubridate, flowchart, visdat,
  forcats, patchwork, gridExtra
)
conflicts_prefer(Hmisc::`label<-`)
conflicts_prefer(dplyr::combine)
library(gridExtra)

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Comments:
# - sleepWorkDays_bw is used for the sleep duration on work days in 2017
# - sleepWorkDays is used for the sleep duration on workdays on 2020
# - both reflect the difference between bed and wake time.
# - There are two analytic data set versions to work with:
#   - (1) sleep17_an/sleep20_an: throw out rows according to description 
#         in text and use winsorization for covariables
#   - (2) sleep17/sleep20: data set has NAs, does not throw out rows, but 
#         uses winsorization for covariables

# WHICH data set to CREATE?---------
analytical_data_set_version <- "(1)"
#analytical_data_set_version <- "(2)"

# Functions ----
# Calculate sleep duration via bed-time and wake-up time in the format HH:MM
sleepTime <- function(bed, wake){ 
  wake <- paste(Sys.Date(), wake)
  tmpbed <- paste(Sys.Date(), bed)
  d <- apply(data.frame(tmpbed, wake), 1, function(x) difftime(x[2], x[1], units = "hours"))
  adjust <- -(d < 0)
  tmpbed <- paste(Sys.Date() + adjust, bed)
  apply(data.frame(tmpbed, wake), 1, function(x) difftime(x[2], x[1], units = "hours"))
}

# Social Jet Lag (SJL)
midpoint <- function(bed, wake){
  h <- sleepTime(bed, wake)/2
  add_h <- floor(h)
  add_m <- round((h-floor(h))*60)
  as.POSIXct(strptime(bed,format = "%H:%M")) + hours(add_h) + minutes(add_m)
}
#difftime(midpoint("21:00","06:30"), midpoint("22:00","07:30"))

# READ data---------------------------------------------------------------------
# _2017---------
sleep17 <- as.data.table(read.spss("./RAW_DATA_2017_2020/2017/UniWien_SPSS_Rohdaten.sav", 
                                   to.data.frame = TRUE))
var_info_17 <- fread("./RAW_DATA_2017_2020/2017/uni_wien_1_3_doc.csv", 
                     header = TRUE, sep = ",")
dim(sleep17) # 1004  609


# _2020--------
sleep20  <- as.data.table(read_sas("./RAW_DATA_2017_2020/2020/wien_schlaf.sas7bdat", NULL))
formats20 <- as.data.table(read_sas("./RAW_DATA_2017_2020/2020/formats.sas7bdat", NULL))
dim(sleep20) # 1010  298

# Metainfos---------------------------------------------------------------------
# 2017 - mean/median duration of questionnaire:
boxplot(sleep17$QDURATION) # two large numbers: >5000 seconds
mean(sleep17$QDURATION)/60
# 22.25493 min
median(sleep17$QDURATION)/60
# 20.45

# 2020 - mean/median duration of questionnaire:
# ????????????
# 2020 questionnaire conducted in Mai 2020






# 2017---Prepare----------------------------------------------------------------
# ______Waketime ----
# waketime hours - work days
sleep17$Q2103_rev <- revalue(sleep17$Q2103, c("00" = "0", 
                                              "01" = "1", 
                                              "02" = "2", 
                                              "03" = "3", 
                                              "04" = "4", 
                                              "05" = "5",
                                              "06" = "6",
                                              "07" = "7",
                                              "08" = "8",
                                              "09" = "9",
                                              "Please Choose" = NA
))
sleep17$Q2103_num <- as.numeric(levels(sleep17$Q2103_rev))[sleep17$Q2103_rev]

# waketime minutes - work days
sleep17$Q2104_rev <- revalue(sleep17$Q2104, c("00"="0","Please Choose" = NA))
sleep17$Q2104_num <- as.numeric(levels(sleep17$Q2104_rev))[sleep17$Q2104_rev]
 
# waketime_hours
sleep17$wakeTimeWorkDay_in_hours <- sleep17$Q2103_num + sleep17$Q2104_num /60

# ______Bedtime ----
# BEDTIME -> careful, 2400/00:00, 1:00 is later than 23:00 but has a smaller number! -> how to integrate in regression model?
# bedtime hours
sleep17$Q2101_rev <- revalue(sleep17$Q2101, c("00" = "0", 
                                              "01" = "1", 
                                              "02" = "2", 
                                              "03" = "3", 
                                              "04" = "4", 
                                              "05" = "5",
                                              "06" = "6",
                                              "07" = "7",
                                              "08" = "8",
                                              "09" = "9",
                                              "Please Choose" = NA))
sleep17$Q2101_num <- as.numeric(levels(sleep17$Q2101_rev))[sleep17$Q2101_rev]

# bedtime minutes
sleep17$Q2102_rev <- revalue(sleep17$Q2102, c("00"="0","Please Choose" = NA))
sleep17$Q2102_num <- as.numeric(levels(sleep17$Q2102_rev))[sleep17$Q2102_rev]

# bedtime in hours:
sleep17$bedTimeWorkDay_in_hours <- sleep17$Q2101_num + sleep17$Q2102_num /60


sleep17[, insomniaChron:=ifelse( (  (Q30=="Yes" & (Q3001=="3-4 times per week"|Q3001=="more than 5 times per week")) |
                                      (Q31=="Yes" & (Q3101=="3-4 times per week"|Q3101=="more than 5 times per week")) |
                                      (Q32=="Yes" & (Q3201=="3-4 times per week"|Q3201=="more than 5 times per week")) & (Q33=="Yes" & (Q3301=="3-4 times per week"|Q3301=="more than 5 times per week"))  )
                                 & 
                                   Q34==">3 months" 
                                 & 
                                   (Q35=="much" | Q35=="very much")
                                 , "Chronic insomnia", "No chronic insomnia")]
label(sleep17$insomniaChron) <- "Chronic insomnia"
sleep17$insomniaChron <- factor(sleep17$insomniaChron, levels = c("No chronic insomnia", "Chronic insomnia"))
sleep17$insomniaChron <- relevel(sleep17$insomniaChron, ref = "No chronic insomnia")
is.factor(sleep17$insomniaChron) # TRUE

# Method 1 for sleepWorkDays and sleepFreeDays
sleep17[, sleepWorkDays:=as.numeric(levels(sleep17$Q2001)[sleep17$Q2001]) + as.numeric(levels(sleep17$Q2002)[sleep17$Q2002])/60] # hours and minutes where seperately given in two questions
sleep17[, sleepFreeDays:=as.numeric(levels(sleep17$Q2201)[sleep17$Q2201]) + as.numeric(levels(sleep17$Q2202)[sleep17$Q2202])/60]

# Method 2 for sleepWorkDays and sleepFreeDays using bed and waketimes
# Q2101[h]:Q2102[min] bed time working day
# Q2103[h]:Q2104[min] wake time working day
# Q2301[h]:Q2302[min] bed time free day
# Q2303[h]:Q2304[min] bed time free day
sleep17[, sleepWorkDays_bw:=sleepTime(paste0(Q2101,":",Q2102),paste0(Q2103,":",Q2104))]
sleep17[, sleepFreeDays_bw:=sleepTime(paste0(Q2301,":",Q2302),paste0(Q2303,":",Q2304))]
label(sleep17$sleepWorkDays_bw) <- "Hours of sleep per night on a week or work day (via bed- and wake-times)"
label(sleep17$sleepFreeDays_bw) <- "Hours of sleep per night on a weekend or day off (via bed- and wake-times)"
sleep17[, bedTimeWorkDay_bw := paste0(Q2101,":",Q2102)]
sleep17[, wakeTimeWorkDay_bw := paste0(Q2103,":",Q2104)]
sleep17[, bedTimeFreeDay_bw := paste0(Q2301,":",Q2302)]
sleep17[, wakeTimeFreeDay_bw := paste0(Q2303,":",Q2304)]

sleep17$bedTimeWorkDay_bw_POSIX <- as.POSIXct(strptime(sleep17$bedTimeWorkDay_bw, "%H:%M"))
sleep17$wakeTimeWorkDay_bw_POSIX <- as.POSIXct(strptime(sleep17$wakeTimeWorkDay_bw, "%H:%M"))
sleep17$bedTimeFreeDay_bw_POSIX <- as.POSIXct(strptime(sleep17$bedTimeFreeDay_bw, "%H:%M"))
sleep17$wakeTimeFreeDay_bw_POSIX <- as.POSIXct(strptime(sleep17$wakeTimeFreeDay_bw, "%H:%M"))

#optimalSleepTime
sleep17[, optimalSleepTime_bw := sleepTime(paste0(Q2701,":",Q2701),paste0(Q2601,":",Q2602))]

# ______Social Jet Lag (SJL) ----
sleep17 <- sleep17 %>% mutate( SJL = difftime(midpoint(paste0(Q2301,":",Q2302),paste0(Q2303,":",Q2304)),  # free days
                                              midpoint(paste0(Q2101,":",Q2102),paste0(Q2103,":",Q2104)), units = "hours") ) # work days

sleep17 <- as.data.table(sleep17)
sleep17[, SleepProblems:=ifelse( (  (Q30=="Yes" & (Q3001=="3-4 times per week"|Q3001=="more than 5 times per week")) |
                                      (Q31=="Yes" & (Q3101=="3-4 times per week"|Q3101=="more than 5 times per week")) |
                                      (Q32=="Yes" & (Q3201=="3-4 times per week"|Q3201=="more than 5 times per week")) & (Q33=="Yes" & (Q3301=="3-4 times per week"|Q3301=="more than 5 times per week"))  )
                                 & 
                                   Q34==">3 months" 
                                 & 
                                   (Q35=="somewhat") # now without:  | Q35=="very much"
                                 ,"Sleep problems", "Not sleep problems")]

#sleep17[, CatVar := case_when(
#  insomniaChron == "Chronic insomnia" ~ "Chronic insomnia",
#  SleepProblems == "Sleep problems" & insomniaChron == "No chronic insomnia" ~ "Sleep problems not fulfilling ICSD-3 criteria"
#)]

#sleep17$insomniaChron <- as.factor(sleep17$insomniaChron)

label(sleep17$sleepWorkDays) <- "Hours of sleep per night on a week or work day"
label(sleep17$sleepFreeDays) <- "Hours of sleep per night on a weekend or day off"

# ______Minimum amount of sleep needed ----
sleep17$Q2401 <- as.numeric(sleep17$Q2401)
sleep17[Q2401==24,]$Q2401 <- 23
sleep17$Q2402 <- as.numeric(sleep17$Q2402)
sleep17[Q2402==1,]$Q2402 <- 0
sleep17[Q2402==2,]$Q2402 <- 15
sleep17[Q2402==3,]$Q2402 <- 30
sleep17[Q2402==4,]$Q2402 <- 45
sleep17 <- sleep17 %>% mutate(min_sleep_required = Q2401 + Q2402/60) # hours + min/60
label(sleep17$min_sleep_required) <- "What is the minimum amount of sleep (in hours) required to function best during a 24-hour period?"

# ______Napping ----
label(sleep17$Q25) <- "Nap during the day"
sleep17$napping <- sleep17$Q25
sleep17$napping <- relevel(sleep17$napping, ref = "No")
is.factor(sleep17$Q25) # TRUE
table(sleep17$Q25)/sum(table(sleep17$Q25)) # Yes: 0.377551 
label(sleep17$Q2502) <- "Frequency of naps"
label(sleep17$Q2503) <- "Average length of naps (min)"

# ______Chronotype ----
# need: Chronotype 2020 (see below...):
sleep20$Q28 <- revalue(as.factor(sleep20$Q28), c("1"="definitely a 'morning' type", 
                                                 "2"="rather more a 'morning' than an 'evening' type", 
                                                 "3"="rather more an 'evening' than a 'morning' type", 
                                                 "4"="definitely an 'evening' type"))
label(sleep20$Q28) <- "Chronotype"
sleep20$chronotype <- sleep20$Q28
sleep20$chronotype <- relevel(sleep20$chronotype, ref = "definitely a 'morning' type")
# end need
levels(sleep17$Q28) <- levels(sleep20$Q28) # use same levels!
label(sleep17$Q28) <- "Chronotype"
sleep17$chronotype <- sleep17$Q28
sleep17$chronotype <- relevel(sleep17$chronotype, ref="definitely a 'morning' type")

# ______Sleep problems ----
label(sleep17$Q30) <- "Trouble falling asleep in the past 4 weeks"
label(sleep17$Q3001) <- "How often trouble falling asleep in the past 4 weeks"
levels(sleep17$Q3001) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))
label(sleep17$Q3002_1) <- "Restlessness"
label(sleep17$Q3002_2) <- "Pain"
label(sleep17$Q3002_3) <- "Stress/Anxiety"
label(sleep17$Q3002_4) <- "Bed partner (snoring, restlessness)"
#define new other reasons variable
sleep17[, Q3002_other := ifelse(Q3002_5=="marked" | Q3002_6=="marked" | Q3002_7=="marked" | Q3002_8=="marked" | Q3002_9=="marked" | Q3002_10=="marked"| Q3002_99=="marked", "marked", NA)]
label(sleep17$Q3002_other) <- "Other"
#label(sleep17$Q3002_5) <- "Light in the bedroom"
#label(sleep17$Q3002_6) <- "Noise"
#label(sleep17$Q3002_7) <- "Need to urinate"
#label(sleep17$Q3002_8) <- "Hungry at night"
#label(sleep17$Q3002_9) <- "Babies, young children"
#label(sleep17$Q3002_10) <- "Pets"
#label(sleep17$Q3002_99) <- "Other"

label(sleep17$Q31) <- "Wake up several times at night in the past 4 weeks"
label(sleep17$Q3101) <- "How often did you wake up at night during the past 4 weeks"
levels(sleep17$Q3101) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))

label(sleep17$Q3102_1) <- "Restlessness"
label(sleep17$Q3102_2) <- "Pain"
label(sleep17$Q3102_3) <- "Stress/Anxiety"
label(sleep17$Q3102_4) <- "Bed partner (snoring, restlessness)"
#define new other reasons variable
sleep17[, Q3102_other := ifelse(Q3102_5=="marked" | Q3102_6=="marked" | Q3102_7=="marked" | Q3102_8=="marked" | Q3102_9=="marked" | Q3102_10=="marked"| Q3102_99=="marked", "marked", NA)]
label(sleep17$Q3102_other) <- "Other"
#label(sleep17$Q3102_5) <- "Light in the bedroom"
#label(sleep17$Q3102_6) <- "Noise"
#label(sleep17$Q3102_7) <- "Need to urinate"
#label(sleep17$Q3102_8) <- "Hungry at night"
#label(sleep17$Q3102_9) <- "Babies, young children"
#label(sleep17$Q3102_10) <- "Pets"
#label(sleep17$Q3102_99) <- "Other"

label(sleep17$Q32) <-  "Waking up earlier than planned in the past 4 weeks" 
label(sleep17$Q3201) <- "How often did you wake up earlier than you planned to?"
levels(sleep17$Q3201) <- list("less or once per week" = "less or once per week", 
                              "1-2 times per week" = "1-2 times per week", 
                              ">=3 times per week" = c("3-4 times per week", "more than 5 times per week"))
label(sleep17$Q3202_1) <- "Restlessness"
label(sleep17$Q3202_2) <- "Pain"
label(sleep17$Q3202_3) <- "Stress/Anxiety"
label(sleep17$Q3202_4) <- "Bed partner (snoring, restlessness)"
#define new other reasons variable
sleep17[, Q3202_other := ifelse(Q3202_5=="marked" | Q3202_6=="marked" | Q3202_7=="marked" | 
                                  Q3202_8=="marked" | Q3202_9=="marked" | Q3202_10=="marked"| 
                                  Q3202_99=="marked", "marked", NA)]
label(sleep17$Q3202_other) <- "Other"
#label(sleep17$Q3202_5) <- "Light in the bedroom"
#label(sleep17$Q3202_6) <- "Noise"
#label(sleep17$Q3202_7) <- "Need to urinate"
#label(sleep17$Q3202_8) <- "Hungry at night"
#label(sleep17$Q3202_9) <- "Babies, young children"
#label(sleep17$Q3202_10) <- "Pets"
#label(sleep17$Q3202_99) <- "Other"
label(sleep17$Q33) <-   "Trouble getting back to sleep after woke up too early in the past 4 weeks" 
label(sleep17$Q3301) <- "How often did you have trouble getting back to sleep after you woke up too early?" 
levels(sleep17$Q3301) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))
label(sleep17$Q3302_1) <- "Restlessness"
label(sleep17$Q3302_2) <- "Pain"
label(sleep17$Q3302_3) <- "Stress/Anxiety"
label(sleep17$Q3302_4) <- "Bed partner (snoring, restlessness)"
#define new other reasons variable
sleep17[, Q3302_other := ifelse(Q3302_5=="marked" | Q3302_6=="marked" | Q3302_7=="marked" | Q3302_8=="marked" | Q3302_9=="marked" | Q3302_10=="marked"| Q3302_99=="marked", "marked", NA)]
label(sleep17$Q3302_other) <- "Other"
label(sleep17$Q3302_5) <- "Light in the bedroom"
label(sleep17$Q3302_6) <- "Noise"
label(sleep17$Q3302_7) <- "Need to urinate"
label(sleep17$Q3302_8) <- "Hungry at night"
label(sleep17$Q3302_9) <- "Babies, young children"
label(sleep17$Q3302_10) <- "Pets"
label(sleep17$Q3302_99) <- "Other"
label(sleep17$Q34) <- "Sleep problem duration"  
label(sleep17$Q35) <- "Extent to what the daily functioning is affected by not being well rested" 
label(sleep17$Q36) <- "Has your family doctor ever asked you about your sleep?"  

label(sleep17$Q37) <-  "Ever asking for medical help or being treated for sleep problem" 
label(sleep17$Q3701_1) <- "Medical help for sleep problem: family doctor or GP" 
label(sleep17$Q3701_2) <- "Medical help for sleep problem: psychologist or psychotherapist"
label(sleep17$Q3701_3) <- "Other clinitian"
#merging 1 and 3
sleep17[, Q3701_13 := ifelse(Q3701_1 == "marked" | Q3701_3 =="marked", "marked", NA)]
label(sleep17$Q3701_13) <- "Physician"
label(sleep17$Q3701_4) <- "Medical help for sleep problem: pharmacist"
label(sleep17$Q3701_5) <- "Medical help for sleep problem: other"

label(sleep17$Q3702_2) <- "Ever taking sleep medication: sleeping pills"
label(sleep17$Q3702_3) <- "Ever taking sleep medication: natural remedies"
label(sleep17$Q3702_4) <- "Ever taking sleep medication: melatonin"
label(sleep17$Q3702_5) <- "Ever taking sleep medication: other"

table(sleep17$Q3702_2) # 59
sum(is.na(sleep17$Q3702_2)) # 671
table(sleep17$Q3702_3) # 42
sum(is.na(sleep17$Q3702_3)) # 671
table(sleep17$Q3702_4) # 11
sum(is.na(sleep17$Q3702_4)) # 671
table(sleep17$Q3702_5) # 12

sleep17$sleep_medication <- case_when(
  sleep17$Q3702_2 == "marked" | 
    sleep17$Q3702_3 == "marked" | 
     sleep17$Q3702_4 == "marked" | 
    sleep17$Q3702_5 == "marked" ~ "yes",
  is.na(sleep17$Q3702_2) & 
    is.na(sleep17$Q3702_3) & 
    is.na(sleep17$Q3702_4) & 
    is.na(sleep17$Q3702_5) ~ NA_character_, # sum = 671 (aslo: there is no case where not all 4 at the same time are NA)
  TRUE ~ "no"
)
table(sleep17$sleep_medication) # 94
boxplot(sleepWorkDays_bw ~ sleep_medication, data = sleep17)
t.test(sleep17$sleepWorkDays_bw ~ sleep17$sleep_medication) # ns
table(is.na(sleep17$sleep_medication)) # 845

sleep17$diagn_sleep_disorder <- sleep17$Q38
sleep17$diagn_sleep_disorder <- relevel(sleep17$diagn_sleep_disorder, ref = "No")

# ______Alertness ----
label(sleep17$Q39) <- "How alert / sleepy did you feel during a working day over the last 4 weeks"
sleep17$Q39_numeric <- as.numeric(sleep17$Q39)

sleep17[, employed_full_part := ifelse(Q16_1 == "marked" | Q16_2 == "marked","marked","not marked")]
sleep17$employed_full_part <- as.factor(sleep17$employed_full_part)
label(sleep17$employed_full_part) <- "Employed (full-/part time)"
label(sleep17$Q16_3) <- "Student, further training, unpaid work experience"
label(sleep17$Q16_4) <- "Unemployed"

# ______Shift work ----
sleep17 <- sleep17 %>% mutate(work = case_when(
  Q16_1 == "marked" & (Q18 == "Rotating shifts including nights" | Q18 == "Regular night shifts") ~ "full_time_inkl_NS",
  Q16_1 == "marked" & (Q18 == "Regular day shifts" | Q18 == "Rotating shifts without nights" | Q18 == "Regular evening shifts") ~ "full_time_exkl_NS",
  Q16_2 == "marked" & (Q18 == "Regular day shifts" | Q18 == "Rotating shifts without nights" | Q18 == "Regular evening shifts") ~ "part_time_exkl_NS",
  Q16_2 == "marked" & (Q18 == "Rotating shifts including nights" | Q18 == "Regular night shifts") ~ "part_time_inkl_NS",
  Q16_3 == "marked" | Q16_4 == "marked" | Q16_5 == "marked" | Q16_6 == "marked" | Q16_7 == "marked" | Q16_8 == "marked" ~ "not_currently_working"
)) %>% mutate(across(work, as_factor))
sleep17$work <- factor(sleep17$work, levels = c("full_time_exkl_NS", "part_time_exkl_NS", "full_time_inkl_NS", "part_time_inkl_NS", "not_currently_working"))
sleep17$work <- relevel(sleep17$work, ref = "not_currently_working")

sleep17 <- sleep17 %>% mutate(regular_night_shifts = ifelse(Q18=="Regular night shifts",1,0)) # only 8!
sleep17 <- sleep17 %>% mutate(any_night_shifts = ifelse(Q18=="Regular night shifts" | Q18 =="Rotating shifts including nights",1,0)) # only 8!

label(sleep17$Q19) <- "Have you ever worked night shifts?"
is.factor(sleep17$Q19) # TRUE

label(sleep17$Q2) <- "Gender"
is.factor(sleep17$Q2) # TRUE

# ______Age ----
# Age for using in regression (parallel to 2020)!
sleep17$Q1_help <- sleep17$Q1
sleep17 <- sleep17 %>% mutate(Q1 = case_when(
                       Q1_help<20 ~ 1,
                       Q1_help>=20 & Q1_help<=24 ~ 2,
                       Q1_help>=25 & Q1_help<=29 ~ 3,
                       Q1_help>=30 & Q1_help<=34 ~ 4,
                       Q1_help>=35 & Q1_help<=39 ~ 5,
                       Q1_help>=40 & Q1_help<=44 ~ 6,
                       Q1_help>=45 & Q1_help<=49 ~ 7,
                       Q1_help>=50 & Q1_help<=54 ~ 8,
                       Q1_help>=55 & Q1_help<=59 ~ 9,
                       Q1_help>=60 & Q1_help<=64 ~ 10,
                       Q1_help>=65 & Q1_help<=69 ~ 11,
                       Q1_help>=70 ~ 12
                       ))
label(sleep17$Q1) <- "Age"
sleep17$age <- sleep17$Q1

# Age categorical
sleep17$Q1_cat <- wafflecut(sleep17$Q1_help, c('[0,20)','[20,24]','[25,29]', '[30,34]', 
                                               '[35,39]','[40,44]','[45,49]','[50,54]',
                                               '[55,59]','[60,64]','[65,69]','[70,100]' ),
                            c('<20 years','20-24 years','25-29 years', '30-34 years', 
                              '35-39 years','40-44 years','45-49 years','50-54 years',
                              '55-59 years','60-64 years','65-69 years','70 or older'))
label(sleep17$Q1_cat) <- "Age"

label(sleep17$Q6) <- "Area of residency"
label(sleep17$Q7) <- "Education"
label(sleep17$Q8) <- "Marital status"

sleep17$number_children <- sleep17$Q9
sleep17$number_children <- relevel(sleep17$number_children, ref = "0")

label(sleep17$Q39) <- "How alert / sleepy did you feel during a working day over the last 4 weeks"
label(sleep17$Q39_numeric) <- "KSS score"

# ______Demographics ----
label(sleep17$Q10) <- "Size of household"
sleep17$Q10 <- mapvalues(as.factor(sleep17$Q10), 
                         from = c("1","2","3","4","5","6 or more"), 
                         to = c("One person", "Two persons", "Three persons", "Four persons", 
                                "Five or more persons", "Five or more persons"))
label(sleep17$Q5) <- "Austrian states"
label(sleep17$Q6) <- "Residence"

label(sleep17$Q7) <- "Education (highest degree)"
table(sleep17$Q7)
boxplot(sleepWorkDays_bw ~ Q7, data = sleep17)
sleep17$education <- sleep17$Q7
ggplot(sleep17, aes(x = education)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Education (2017)", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # center title
  theme(plot.title = element_text(hjust = 0.5))
# Merge levels elementary school and high school:
levels(sleep17$education) # "elementary School (Volksschule)" "high School (Hauptschule)"       "Matura (Gymnasium)"              "university degree and above"
sleep17$education <- fct_collapse(
  sleep17$education,
  "Elementary or High School" = c("elementary School (Volksschule)", "high School (Hauptschule)")
)

label(sleep17$Q2) <- "Gender"
sleep17$Q2 <- mapvalues(sleep17$Q2, 
                        from = c("Female", "Male"), 
                        to = c("Women", "Men"))
sleep17$gender <- sleep17$Q2

sleep17$ethnicity <- sleep17$Q3
# levels(sleep17$Q3)
# table(sleep17$Q3)/sum(table(sleep17$Q3)) # 92% White/Caucasian; 1.8% Asian; <1% Black or Black or African American.
# sum(is.na(sleep17$Q3)) # 0
ggplot(sleep17, aes(x = ethnicity)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Ethnien (2017)", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # center title
  theme(plot.title = element_text(hjust = 0.5))
levels(sleep17$ethnicity)
# create new levels White and Non-White:
sleep17$ethnicity <- fct_collapse(
  sleep17$ethnicity,
  "White" = c("White/Caucasian"),
  "Non-White" = c("Asian",
                  "Black or African American", 
                  "Other"))

# ______BMI -----
sleep17$height_m <- sleep17$Q12/100
sleep17$mass_kg <- sleep17$Q13
sleep17$BMI <- round(sleep17$mass_kg/sleep17$height_m^2, digits = 1)
sleep17$BMI_category <- as.factor(wafflecut(sleep17$BMI, c('[30,100]','[18.5,24.9]','[25,29.9]', '[0,18.5)'), 
                                            c('obesity','normal weight','overweight','underweight')))
label(sleep17$Q60_16) <- "No health issues during the past 12 months"
label(sleep17$Q6101) <- "Taking medications regularly in the past 12 months"

#sleep17[, CatVar_Table3 := case_when(
#  SleepProblems == "Sleep problems not fulfilling ICSD-3 criteria" & Q37=="Yes" ~ "Sleep problems health care use",
#  SleepProblems == "Sleep problems not fulfilling ICSD-3 criteria" & Q37=="No" ~ "Sleep problems no health care use",
#  insomniaChron == "Chronic insomnia" &  Q37=="Yes" ~ "Chronic insomnia healthcare use",
#  insomniaChron == "Chronic insomnia" &  Q37=="No" ~ "Chronic insomnia no healthcare use"
#)]
#sleep17$CatVar_Table3 <- factor(sleep17$CatVar_Table3, levels=c("Sleep problems health care use", "Sleep problems no health care use", "Chronic insomnia healthcare use", "Chronic insomnia no healthcare use"))


#sleep17[, employed_full_part := ifelse(Q16_1 == "marked" | Q16_2 == "marked","marked","not marked")]
#sleep17$employed_full_part <- as.factor(sleep17$employed_full_part)
#label(sleep17$employed_full_part) <- "Employed (full-/part time)"


label(sleep17$Q16_3) <- "Student, further training, unpaid work experience"
label(sleep17$Q16_4) <- "Unemployed"

# ______Marrital status ----
label(sleep17$Q8) <- "Marital status"
sleep17 <- sleep17 %>% 
  mutate(marrital_status = case_when(
    Q8 == "married or in a partnership" ~ "married_or_partnership",
    Q8 != "married or in a partnership" ~ "single_divorced_widowed"
  )) %>% mutate(across(marrital_status, as_factor))
sleep17$marrital_status <- factor(sleep17$marrital_status, levels = c("married_or_partnership", "single_divorced_widowed"))
sleep17$marrital_status <- relevel(sleep17$marrital_status, ref = "married_or_partnership")

label(sleep17$Q10) <- "Size of household"
sleep17$Q10 <- mapvalues(as.factor(sleep17$Q10), 
                         from = c("1","2","3","4","5","6 or more"), 
                         to = c("One person", "Two persons", "Three persons", 
                                "Four persons", "Five or more persons", "Five or more persons"))
label(sleep17$Q5) <- "Austrian states"
label(sleep17$Q6) <- "Residence"
label(sleep17$Q7) <- "Education (highest degree)"
#sleep variables
label(sleep17$Q39) <- "How alert / sleepy did you feel during a working day over the last 4 weeks"
label(sleep17$Q39_numeric) <- "KSS score"

sleep17$year <- factor("2017")

# ______Optimism score ----
# optimism score LOT-R -> https://www.cmu.edu/dietrich/psychology/pdf/scales/LOTR_Scale.pdf
#1) =ITEM 1 -> In uncertain times, I usually expect the best.
sleep17$Q43_1 <- revalue(as.factor(sleep17$Q43_1), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))
#2) =ITEM 3 ->If something can go wrong for me, it will.
sleep17$Q43_2 <- revalue(as.factor(sleep17$Q43_2), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))

#3) =ITEM 4 -> I am always optimistic about my future.
sleep17$Q43_3 <- revalue(as.factor(sleep17$Q43_3), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))


#4) =ITEM 7 -> I hardly ever expect things to go my way.
sleep17$Q43_4 <- revalue(as.factor(sleep17$Q43_4), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))

#5) =ITEM 9 -> I rarely count on good things happening to me.
sleep17$Q43_5 <- revalue(as.factor(sleep17$Q43_5), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))

#6) =ITEM 10 -> Overall, I expect more good things to happen to me than bad.
sleep17$Q43_6 <- revalue(as.factor(sleep17$Q43_6), c("disagree strongly"="0", 
                                                     "disagree a little"="1", 
                                                     "neither agree nor disagree"="2", 
                                                     "agree a little"="3",
                                                     "agree strongly"="4"))
sleep17 <- sleep17 %>% 
  mutate(Q43_2_rev = 4-(as.numeric(Q43_2)-1), 
         Q43_4_rev = 4-(as.numeric(Q43_4)-1), 
         Q43_5_rev=4-(as.numeric(Q43_5)-1)) %>%
  mutate(optimScore = 
           as.numeric(Q43_1)-1 + # In uncertain times, I usually expect the best.
           Q43_2_rev + # If something can go wrong for me, it will.
           as.numeric(Q43_3)-1 + # I am always optimistic about my future.
           Q43_4_rev + # I hardly ever expect things to go my way.
           Q43_5_rev + # I rarely count on good things happening to me.
           as.numeric(Q43_6)-1) #  Overall, I expect more good things to happen to me than bad.
label(sleep17$optimScore) <- "Optimism Score (LOT-R)"

sleep17$sleepWorkDays_bw
plot(sleep17$sleepWorkDays_bw, sleep17$optimScore, data = sleep17)
cor.test(sleep17$sleepWorkDays_bw, sleep17$optimScore) # ns

# ______10 min intense sports ----
sleep17$Q48_numeric <- as.numeric(revalue(sleep17$Q48, c("None"=0, "1 day"=1, 
                                                         "2 days"=2, "3 days"=3, 
                                                         "4 days"=4, "5 days"=5, 
                                                         "6 days"=6, "7 days"=7)))-1
sleep17 <- sleep17 %>% mutate(sport_yes_no = case_when(
  Q48 == "None" ~ "No",
  Q48 %in% c("1 day","2 days", "3 days", "4 days", "5 days", "6 days", "7 days") ~ "Yes"
)) %>% mutate(across(sport_yes_no, as_factor))
sleep17$sport_yes_no <- relevel(sleep17$sport_yes_no, ref = "No")

# ______Alcohol and Coffee ----
# define NAs
sleep17[Q4601==-9999,]$Q4601 <- as.numeric(NA)
sleep17[Q4602==-9999,]$Q4602 <- as.numeric(NA)
sleep17[Q4603==-9999,]$Q4603 <- as.numeric(NA)
sleep17[Q4604==-9999,]$Q4604 <- as.numeric(NA)
sleep17[Q4605==-9999,]$Q4605 <- as.numeric(NA)
sleep17[Q4606==-9999,]$Q4606 <- as.numeric(NA)

sleep17 <- sleep17 %>% 
  mutate(alcohol_intake = ifelse(!is.na(Q4601),Q4601,0) + ifelse(!is.na(Q4602),Q4602,0) + ifelse(!is.na(Q4603),Q4603,0),
         caffeine_intake = ifelse(!is.na(Q4604),Q4604,0) + ifelse(!is.na(Q4605),Q4605,0) + ifelse(!is.na(Q4606),Q4606,0)
)

# ______Dinner on workdays ----
sleep17 <- sleep17 %>% 
  mutate(dinner_WorkDays = case_when(
    Q4503 %in% c("before 17h", "17h to 18h") ~ "<=1800",
    Q4503 == "18h to 19h" ~ "18h to 19h",
    Q4503 == "19h to 20h" ~ "19h to 20h",
    Q4503 == "20h to 21h" ~ "20h to 21h",
    Q4503 == "I usually do not have dinner" ~ "I usually do not have dinner",
    Q4503 %in% c("21h to 22h", "22h to 23h", "later than 23h") ~ ">=2100"
  )) %>% mutate(across(dinner_WorkDays, as_factor))
sleep17$dinner_WorkDays <- factor(sleep17$dinner_WorkDays, levels = c("<=1800", "18h to 19h", "19h to 20h", "20h to 21h", "I usually do not have dinner"))
sleep17$dinner_WorkDays <- relevel(sleep17$dinner_WorkDays, ref = "<=1800")

# ______Smoking ----
sleep17$smoking <- sleep17$Q54
sleep17$smoking <- relevel(sleep17$smoking, ref = "No, never")

# ______General health ----
sleep17$general_health <- sleep17$Q59
sleep17$general_health <- relevel(sleep17$general_health, ref = "very good")

dim(sleep17) # 1004  671

# ...up to here data set versions to not differ--------

# 2017---Clean------------------------------------------------------------------

#______sleep duration-----

# ______1) sleepWorkDays_bw 2017:----

# ________Inclusion Flowchart 2017--------------
fc17 <- sleep17 %>% 
  as_fc(label = "Participants 2017") %>%
  fc_filter(!grepl("Please Choose", bedTimeWorkDay_bw, 
                  fixed=TRUE) | !grepl("Please Choose", wakeTimeWorkDay_bw, fixed=TRUE), 
            show_exc = TRUE,
            label = "Bed and wake times available",
            label_exc = "Bed or wake times missing") %>%
  fc_filter(sleepWorkDays_bw > 0,
            label = "Sleep duration > 0",
            label_exc = "Sleep duration ≤ 0",
            show_exc = TRUE) %>%
  fc_filter(abs(sleepWorkDays - sleepWorkDays_bw) <= 3.5,
            label = "Discrepancy ≤ 3.5h",
            label_exc = "Discrepancy > 3.5h",
            show_exc = TRUE) %>%
  fc_filter(sleepWorkDays_bw > 3.5,
            label = "Sleep duration > 3.5h",
            label_exc = "Sleep duration ≤ 3.5h",
            show_exc = TRUE) %>%
  fc_filter(!is.na(dinner_WorkDays),
            label = "Dinner time work days available",
            label_exc = "Dinner time work days missing",
            show_exc = TRUE) %>%
  fc_filter(!is.na(work),
            label = "Work information available",
            label_exc = "Work information missing",
            show_exc = TRUE) %>%
  fc_draw()

# ________2017 analytic set after filter:-----------
if(analytical_data_set_version == "(1)"){ # throw out rows
  
  sleep17 <- sleep17 %>%
    dplyr::filter(!grepl("Please Choose", bedTimeWorkDay_bw, 
                         fixed = TRUE) | !grepl("Please Choose", wakeTimeWorkDay_bw, 
                                                fixed=TRUE)) %>%
    dplyr::filter(sleepWorkDays_bw > 0) %>%
    dplyr::filter(abs(sleepWorkDays - sleepWorkDays_bw) <= 3.5) %>%
    dplyr::filter(sleepWorkDays_bw > 3.5) %>%
    dplyr::filter(!is.na(dinner_WorkDays)) %>%
    dplyr::filter(!is.na(work))
  
  dim(sleep17) # 784 671 (check: run from top of script)
  
} else if (analytical_data_set_version == "(2)"){ # keep rows and set values to NA
  
  # ________SET values to NA (TRUE/FALSE):------------
  # CAUTION: if this step is executed, 2017 analytic data set above is not necessary
  #          and one should impute the missing values, at least in the covariates.
  # _______Step 1 (31 cases): Eliminate if "Please Choose" is in the bedTimeWorkDay_bw or wakeTimeWorkDay_bw:------
  sleep17[grepl("Please Choose", bedTimeWorkDay_bw, 
                fixed=TRUE) | grepl("Please Choose", wakeTimeWorkDay_bw, fixed = TRUE),]$sleepWorkDays_bw <- as.numeric(NA)
  
  # _______Step 2 (6+2 cases): sleepWorkdays_bw == 0 -> set sleepWorkdays_bw to NA---------
  sleep17[sleepWorkDays == 0,]$sleepWorkDays <- as.numeric(NA) # (6)
  sleep17[sleepWorkDays_bw == 0,]$sleepWorkDays_bw <- as.numeric(NA) # (2)
  
  # _______Step 3 (80 cases, 0 cases lost): Transform sleepWorkDays_bw > 15: subtract 12h.-------
  #sleep17 %>% 
  #  dplyr::select(sleepWorkDays, sleepWorkDays_bw, bedTimeWorkDay_bw, wakeTimeWorkDay_bw) %>%
  #  filter(sleepWorkDays_bw > 15) %>%
  #  mutate(sleepWorkDays_bw_minus_12 = sleepWorkDays_bw - 12) %>%
  #  mutate(diff = sleepWorkDays_bw_minus_12 - sleepWorkDays) %>%
  #  ggplot(aes(x = sleepWorkDays, y = sleepWorkDays_bw_minus_12)) +
  #  geom_point()
  sleep17[sleepWorkDays_bw > 15,]$sleepWorkDays_bw <- sleep17[sleepWorkDays_bw > 15,]$sleepWorkDays_bw - 12 # (80)
  
  # _______Step 4 (13 cases): discrepancy--------
  sleep17[abs(sleepWorkDays - sleepWorkDays_bw) > 3.5,]$sleepWorkDays_bw <- as.numeric(NA)
  
  # _______Step 5 (7 cases): exclude sleepWorkDays_bw < 3.5:--------
  sleep17[sleepWorkDays_bw < 3.5,]$sleepWorkDays_bw <- as.numeric(NA)
  
  
  sum(is.na(sleep17$sleepWorkDays_bw)) # 53 NA
  
}
  

# ______2) sleepFreeDays_bw 2017:----

#OLD___
# CHECK small and large sleep durations: FreeDays
#sleep17[sleepFreeDays_bw > 15 | sleepFreeDays_bw < 5, c("QID","bedTimeFreeDay_bw", "wakeTimeFreeDay_bw","sleepFreeDays_bw", "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
# 132

# CHECK: Stefan input: chronic insomniacs often sleep 3-4 hours subjectively:
#sleep17[(sleepFreeDays_bw < 3 & insomniaChron == "Chronic insomnia"), c("QID","bedTimeFreeDay_bw", "wakeTimeFreeDay_bw","sleepFreeDays_bw", 
#                                                       "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
#7
# FILTER chronic insomniacs below 3 hours of sleep:
#sleep17[sleepFreeDays_bw < 3 & insomniaChron == "Chronic insomnia", ]$sleepFreeDays_bw <- as.numeric(NA)

# FILTER sleep durations of more than 15 hours and non-insomniacs sleeping less than 5 hours
#sleep17[sleepFreeDays_bw > 15 | (insomniaChron != "Chronic insomnia" & sleepFreeDays_bw < 5),]$sleepFreeDays_bw <- as.numeric(NA) # this rather crude filter could be improved
# (by imputing manually) if sample size should be too small later


# CHECK: randomly normal sleep times
#sleep17[sample(1:dim(sleep17)[1], 100, replace = FALSE), c("bedTimeWorkDay_bw", "wakeTimeWorkDay_bw","sleepWorkDays_bw", 
#                                                           "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]

# ______3) sleepWorkDays 2017:-----

#__OLD
# CHECK small and large sleep durations: WorkDays
#sleep17[sleepWorkDays > 15 | sleepWorkDays < 5, c("QID","bedTimeWorkDay_bw", "wakeTimeWorkDay_bw","sleepWorkDays_bw", "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
# 44
# CHECK: Stefan input: chronic insomniacs often sleep 3-4 hours subjectively:
#sleep17[(sleepWorkDays < 5 & insomniaChron == "Chronic insomnia"), c("QID","bedTimeWorkDay_bw", "wakeTimeWorkDay_bw","sleepWorkDays_bw", 
#                                                    "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
#6

# OLD___
# FILTER chronic insomniacs below 3 hours of sleep:
#sleep17[sleepWorkDays < 3 & insomniaChron == "Chronic insomnia", ]$sleepWorkDays <- as.numeric(NA)

# FILTER sleep durations of more than 15 hours and non-insomniacs sleeping less than 5 hours
#sleep17[sleepWorkDays > 15 | (insomniaChron != "Chronic insomnia" & sleepWorkDays < 5),]$sleepWorkDays <- as.numeric(NA) # this rather crude filter could be improved
# (by imputing manually) if sample size should be too small later


# ______4) sleepFreeDays 2017:----

# CHECK small and large sleep durations: FreeDays
#sleep17[sleepFreeDays > 15 | sleepFreeDays < 5, c("QID","bedTimeFreeDay_bw", "wakeTimeFreeDay_bw","sleepFreeDays_bw", "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
# 40

# CHECK: Stefan input: chronic insomniacs often sleep 3-4 hours subjectively:
#sleep17[(sleepFreeDays < 5 & insomniaChron == "Chronic insomnia"), c("QID","bedTimeFreeDay_bw", "wakeTimeFreeDay_bw","sleepFreeDays_bw", 
#                                                    "bedTimeFreeDay_bw","wakeTimeFreeDay_bw","sleepFreeDays_bw")]
# FILTER chronic insomniacs below 3 hours of sleep:
#sleep17[sleepFreeDays < 3 & insomniaChron == "Chronic insomnia", ]$sleepFreeDays <- as.numeric(NA)

# FILTER sleep durations of more than 15 hours and non-insomniacs sleeping less than 5 hours
#sleep17[sleepFreeDays > 15 | (insomniaChron != "Chronic insomnia" & sleepFreeDays < 5),]$sleepFreeDays <- as.numeric(NA) # this rather crude filter could be improved
# (by imputing manually) if sample size should be too small later

# CHECK: randomly normal sleep times
# sleep17[sample(1:dim(sleep17)[1], 100, replace = FALSE), c("bedTimeWorkDay", "wakeTimeWorkDay","sleepWorkDays_bw", 
#                                                           "bedTimeFreeDay","wakeTimeFreeDay","sleepFreeDays")]

# ______5) min sleep required---- now WINSORIZED ....
# FILTER for Q2401/2: "What is the minimum number of hours you need to sleep to function at your best during a 24 hour period? - Hours"
#sleep17[min_sleep_required < 3 | min_sleep_required > 20,]$min_sleep_required <- as.numeric(NA)

# ______6) optimal sleep time-----
# not in analysis
sleep17[optimalSleepTime_bw < 5 | optimalSleepTime_bw > 15,]$optimalSleepTime_bw <- as.numeric(NA)

# ______7) Social Jet Lag (SJL):-----
# not in analysis
sleep17[is.na(sleepWorkDays_bw) | is.na(sleepFreeDays_bw) | SJL < -2 | SJL > 8,]$SJL <- NA

# ______8) bed and waketimes: ----


# Work days 2017
ggplot(sleep17, aes(x = sleepWorkDays, y = sleepWorkDays_bw)) + 
  geom_point() + 
  xlim(0,15) + 
  ylim(0,15) + 
  stat_function(fun = function(x) x) + 
  ggtitle("2017 comparison of sleep durations on work days")+
  xlab("Sleep work days (self reported)") + 
  ylab("Via bed- and wake times (self reported)") + 
  stat_cor()+ 
  theme(plot.title = element_text(hjust = 0.5))

# Free days 2017
ggplot(sleep17, aes(x = sleepFreeDays, y = sleepFreeDays_bw)) + 
  geom_point() + 
  xlim(0,15) + 
  ylim(0,15) + 
  stat_function(fun = function(x) x) + 
  ggtitle("2017 comparison of sleep durations on free days")+
  xlab("Sleep free days (self reported)") + 
  ylab("Via bed- and wake times (self reported)") + 
  stat_cor()+ 
  theme(plot.title = element_text(hjust = 0.5))
# zeros?

# compare optimal vs real sleep time on working days:
ggplot(sleep17, aes(x = optimalSleepTime_bw, y = sleepWorkDays_bw)) + 
  geom_point() + 
  xlim(0,15) + 
  ylim(0,15) + 
  stat_function(fun = function(x) x) + 
  ggtitle("2017: optimal vs real sleep time on working days")+
  xlab("Optimal sleep time (self reported)") + 
  ylab("Real sleep time on work days via bed and wake times (self reported)") + 
  stat_cor() + 
  theme(plot.title = element_text(hjust = 0.5))
# ->biased
sleep17 %>% mutate(Vienna = ifelse(Q5 == "Vienna", "Vienna","Not Vienna")) %>%
  ggplot(aes(x = optimalSleepTime_bw, y = sleepWorkDays_bw,color=Vienna)) + 
  geom_point() + 
  xlim(0, 15) + 
  ylim(0, 15) + 
  stat_function(fun = function(x) x) + 
  ggtitle("2017: optimal vs real sleep time on working days")+
  xlab("Optimal sleep time (self reported)") + 
  ylab("Real sleep time on work days via bed and wake times (self reported)") + 
  stat_cor() + 
  theme(plot.title = element_text(hjust = 0.5))

# ______9) BMI, winsorize, set obs above 99% to the 99%th percentile and same with 1%. ----
perc99 <- quantile(sleep17$BMI, probs=0.99)
perc1 <- quantile(sleep17$BMI, probs=0.01)
sleep17$BMI <- ifelse(sleep17$BMI < perc1, perc1,sleep17$BMI)
sleep17$BMI <- ifelse(sleep17$BMI > perc99, perc99,sleep17$BMI)

# ______10) min_sleep_required windsorize ----
perc99 <- quantile(sleep17$min_sleep_required, na.rm = TRUE, probs=0.99)
perc1 <- quantile(sleep17$min_sleep_required, na.rm = TRUE, probs=0.01)
sleep17$min_sleep_required <- ifelse(sleep17$min_sleep_required < perc1, perc1,sleep17$min_sleep_required)
sleep17$min_sleep_required <- ifelse(sleep17$min_sleep_required > perc99, perc99,sleep17$min_sleep_required)

# ______11) alcohol_intake, windsorize top 99% ----
perc99 <- quantile(sleep17$alcohol_intake, na.rm = TRUE, probs=0.99)
sleep17$alcohol_intake <- ifelse(sleep17$alcohol_intake > perc99, perc99,sleep17$alcohol_intake)

# ______12) caffeine_intake, windsorize top 99% ----
perc99 <- quantile(sleep17$caffeine_intake, na.rm = TRUE, probs=0.99)
sleep17$caffeine_intake <- ifelse(sleep17$caffeine_intake > perc99, perc99,sleep17$caffeine_intake)


# Compare sleep durations 2017 ####
# WorkDays
mean(sleep17$sleepWorkDays, na.rm = TRUE)
mean(sleep17$sleepWorkDays_bw, na.rm = TRUE)
wilcox.test(sleep17$sleepWorkDays, sleep17$sleepWorkDays_bw, alternative = "less")

# # !!NEED!!: sleepWorkDays caclulation from below....
# mean(sleep20$sleepWorkDays, na.rm = TRUE)
# wilcox.test(sleep17$sleepWorkDays_bw,sleep20$sleepWorkDays, alternative = "less")
# # FreeDays
# mean(sleep17$sleepFreeDays, na.rm = TRUE)
# mean(sleep17$sleepFreeDays_bw, na.rm = TRUE)
# wilcox.test(sleep17$sleepFreeDays, sleep17$sleepFreeDays_bw, alternative = "less")
# mean(sleep20$sleepFreeDays, na.rm = TRUE)
# wilcox.test(sleep17$sleepFreeDays_bw, sleep20$sleepFreeDays, alternative = "two.sided") # Sleep on Free Days has not changed (p-value = 0.2241)


# fun: weighted sleep duration for week and free days...
#sleep17_fun <- sleep17 %>% mutate(weighted_sleep_duration = (sleepWorkDays_bw*5 + sleepFreeDays_bw*2)/7)
#sleep20_fun <- sleep20 %>% mutate(weighted_sleep_duration = (sleepWorkDays*5 + sleepFreeDays*2)/7)
#mean(sleep17_fun$weighted_sleep_duration, na.rm=TRUE)-mean(sleep20_fun$weighted_sleep_duration, na.rm=TRUE)
#mean(sleep17$sleepWorkDays_bw, na.rm=TRUE) - mean(sleep20$sleepWorkDays, na.rm=TRUE)

dim(sleep17) # 784 671
table(sleep17$ethnicity)
# White/Caucasian          Asian Black or African American   Other 
# 720                        12           1                    51 
boxplot(sleepWorkDays_bw ~ ethnicity, data = sleep17)

# 2020---Prepare------------------------------------------------------------------
# ______Sleep duration ----
sleep20$Q2001 <- as.factor(sleep20$Q2001)
label(sleep20$Q2001) <- "An Wochentagen (Mo - Fr) schlief ich normalerweise ein um (h):"
sleep20$Q2001 <- revalue(sleep20$Q2001, c("1" = "00", 
                                          "2" = "00", 
                                          "3" = "01", 
                                          "4" = "02", 
                                          "5" = "03",
                                          "6" = "04",
                                          "7" = "05",
                                          "8" = "06",
                                          "9" = "07",
                                          "10" = "08",
                                          "11" = "09",
                                          "12" = "10",
                                          "13" = "11",
                                          "14" = "12",
                                          "15" = "13",
                                          "16" = "14",
                                          "17" = "15",
                                          "18" = "16",
                                          "19" = "17",
                                          "20" = "18",
                                          "21" = "19",
                                          "22" = "20",
                                          "23" = "21",
                                          "24" = "22",
                                          "25" = "23",
                                          "26" = "24",
                                          "99" = as.numeric(NA)))

sleep20$Q2002 <- as.factor(sleep20$Q2002)
label(sleep20$Q2002) <- "An Wochentagen (Mo - Fr) schlief ich normalerweise ein um (min):"
sleep20$Q2002 <- revalue(sleep20$Q2002, c("1" = "00", 
                                          "99" = as.numeric(NA)))

sleep20$Q2003 <- as.factor(sleep20$Q2003)
label(sleep20$Q2003) <- "An Wochentagen (Mo - Fr) wachte ich normalerweise auf um (h):"
sleep20$Q2003 <- revalue(sleep20$Q2003, c("1" = "00", 
                                          "2" = "00", 
                                          "3" = "01", 
                                          "4" = "02", 
                                          "5" = "03",
                                          "6" = "04",
                                          "7" = "05",
                                          "8" = "06",
                                          "9" = "07",
                                          "10" = "08",
                                          "11" = "09",
                                          "12" = "10",
                                          "13" = "11",
                                          "14" = "12",
                                          "15" = "13",
                                          "16" = "14",
                                          "17" = "15",
                                          "18" = "16",
                                          "19" = "17",
                                          "20" = "18",
                                          "21" = "19",
                                          "22" = "20",
                                          "23" = "21",
                                          "24" = "22",
                                          "25" = "23",
                                          "26" = "24",
                                          "99" = as.numeric(NA)))

sleep20$Q2004 <- as.factor(sleep20$Q2004)
label(sleep20$Q2004) <- "An Wochentagen (Mo - Fr) wachte ich normalerweise auf um (min):"
sleep20$Q2004 <- revalue(sleep20$Q2004, c("1" = "00",
                                          "99" = as.numeric(NA)))

sleep20$Q2005 <- as.factor(sleep20$Q2005)
label(sleep20$Q2005) <- "An Wochenenden (Sa/So) schlief ich normalerweise ein um (h):"
sleep20$Q2005 <- revalue(sleep20$Q2005, c("1" = "00", 
                                          "2" = "00", 
                                          "3" = "01", 
                                          "4" = "02", 
                                          "5" = "03",
                                          "6" = "04",
                                          "7" = "05",
                                          "8" = "06",
                                          "9" = "07",
                                          "10" = "08",
                                          "11" = "09",
                                          "12" = "10",
                                          "13" = "11",
                                          "14" = "12",
                                          "15" = "13",
                                          "16" = "14",
                                          "17" = "15",
                                          "18" = "16",
                                          "19" = "17",
                                          "20" = "18",
                                          "21" = "19",
                                          "22" = "20",
                                          "23" = "21",
                                          "24" = "22",
                                          "25" = "23",
                                          "26" = "24",
                                          "99" = as.numeric(NA)))

sleep20$Q2006 <- as.factor(sleep20$Q2006)
label(sleep20$Q2006) <- "An Wochenenden (Sa/So) schlief ich normalerweise ein um (min):"
sleep20$Q2006 <- revalue(sleep20$Q2006, c("1" = "00",
                                          "99" = as.numeric(NA)))

sleep20$Q2007 <- as.factor(sleep20$Q2007)
label(sleep20$Q2007) <- "An Wochenenden (Sa/So) wachte ich normalerweise auf um (h):"
sleep20$Q2007 <- revalue(sleep20$Q2007, c("1" = "00", 
                                          "2" = "00", 
                                          "3" = "01", 
                                          "4" = "02", 
                                          "5" = "03",
                                          "6" = "04",
                                          "7" = "05",
                                          "8" = "06",
                                          "9" = "07",
                                          "10" = "08",
                                          "11" = "09",
                                          "12" = "10",
                                          "13" = "11",
                                          "14" = "12",
                                          "15" = "13",
                                          "16" = "14",
                                          "17" = "15",
                                          "18" = "16",
                                          "19" = "17",
                                          "20" = "18",
                                          "21" = "19",
                                          "22" = "20",
                                          "23" = "21",
                                          "24" = "22",
                                          "25" = "23",
                                          "26" = "24",
                                          "99" = as.numeric(NA)))

sleep20$Q2008 <- as.factor(sleep20$Q2008)
label(sleep20$Q2008) <- "An Wochenenden (Sa/So) wachte ich normalerweise auf um (min):"
sleep20$Q2008 <- revalue(sleep20$Q2008, c("1" = "00",
                                          "99" = as.numeric(NA)))

sleep20[, sleepWorkDays:=sleepTime(paste0(Q2001,":",Q2002),paste0(Q2003,":",Q2004))] # hours and minutes where seperately given in two questions
sleep20[, sleepFreeDays:=sleepTime(paste0(Q2005,":",Q2006),paste0(Q2007,":",Q2008))]
label(sleep20$sleepWorkDays) <- "Hours of sleep per night on a week or work day"
label(sleep20$sleepFreeDays) <- "Hours of sleep per night on a weekend or day off"
sleep20[, bedTimeWorkDay := paste0(Q2001,":",Q2002)]
sleep20[, wakeTimeWorkDay := paste0(Q2003,":",Q2004)]
sleep20[, bedTimeFreeDay := paste0(Q2005,":",Q2006)]
sleep20[, wakeTimeFreeDay := paste0(Q2007,":",Q2008)]

sleep20[, min_sleep_required := Q2401 + Q2402/60] # hours + min/60
label(sleep20$min_sleep_required) <- "What is the minimum amount of sleep (in hours) required to function best during a 24-hour period?"

# optimalSleepTime (not available in 2017)
#sleep20[, optimalSleepTime_bw:=sleepTime(paste0(Q2701,":",Q2701),paste0(Q2601,":",Q2602))]

sleep20 <- sleep20 %>% mutate( SJL = difftime(midpoint(paste0(Q2005,":",Q2006),paste0(Q2007,":",Q2008)),  # free days
                                              midpoint(paste0(Q2001,":",Q2002),paste0(Q2003,":",Q2004)), units = "hours") ) # work days

# checking
#sleep20[!is.na(sleepWorkDays) & !is.na(sleepFreeDays) & SJL>=-2 & SJL<=8,c("bedTimeWorkDay", "wakeTimeWorkDay","bedTimeFreeDay","wakeTimeFreeDay","SJL")]


# ______Chronotype ----
sleep20$Q28 <- revalue(as.factor(sleep20$Q28), c("1"="definitely a 'morning' type", 
                                                 "2"="rather more a 'morning' than an 'evening' type", 
                                                 "3"="rather more an 'evening' than a 'morning' type", 
                                                 "4"="definitely an 'evening' type"))
label(sleep20$Q28) <- "Chronotype"
sleep20$chronotype <- sleep20$Q28
sleep20$chronotype <- relevel(sleep20$chronotype, ref="definitely a 'morning' type")

# revalue for insomniaChron definition:
sleep20$Q30 <- as.factor(sleep20$Q30)
sleep20$Q30 <- revalue(sleep20$Q30, c("1"="No", "2"="Yes"))
sleep20$Q3001 <- as.factor(sleep20$Q3001)
sleep20$Q3001 <- revalue(sleep20$Q3001, c("1"="less or once per week", 
                                          "2"="1-2 times per week", 
                                          "3"="3-4 times per week", 
                                          "4"="more than 5 times per week"))
sleep20$Q31 <- as.factor(sleep20$Q31)
sleep20$Q31 <- revalue(sleep20$Q31, c("1"="No", "2"="Yes"))

sleep20$Q3101 <- as.factor(sleep20$Q3101)
sleep20$Q3101 <- revalue(sleep20$Q3101, c("1"="less or once per week", 
                                          "2"="1-2 times per week", 
                                          "3"="3-4 times per week", 
                                          "4"="more than 5 times per week"))
levels(sleep20$Q3101) <- list("less or once per week" = "less or once per week", 
                              "1-2 times per week"="1-2 times per week", 
                              ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))

sleep20$Q32 <- as.factor(sleep20$Q32)
sleep20$Q32 <- revalue(sleep20$Q32, c("1"="No", "2"="Yes"))

sleep20$Q3201 <- as.factor(sleep20$Q3201)
sleep20$Q3201 <- revalue(sleep20$Q3201, c("1"="less or once per week", "2"="1-2 times per week", "3"="3-4 times per week", "4"="more than 5 times per week"))
levels(sleep20$Q3201) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))

sleep20$Q33 <- as.factor(sleep20$Q33)
sleep20$Q33 <- revalue(sleep20$Q33, c("1"="No", "2"="Yes"))

sleep20$Q3301 <- as.factor(sleep20$Q3301)
sleep20$Q3301 <- revalue(sleep20$Q3301, c("1"="less or once per week", "2"="1-2 times per week", "3"="3-4 times per week", "4"="more than 5 times per week"))
levels(sleep20$Q3301) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))

sleep20$Q34 <- as.factor(sleep20$Q34)
sleep20$Q34 <- revalue(sleep20$Q34, c("1"="<3 months", "2"=">3 months"))
sleep20$Q35 <- as.factor(sleep20$Q35)
sleep20$Q35 <- revalue(sleep20$Q35, c("1"="not at all", "2"="a little", "3"="somewhat", "4"="much", "5"="very much"))

sleep20[, insomniaChron := ifelse( (  (Q30=="Yes" & (Q3001=="3-4 times per week"|Q3001=="more than 5 times per week")) |
                                      (Q31=="Yes" & (Q3101=="3-4 times per week"|Q3101=="more than 5 times per week")) |
                                      (Q32=="Yes" & (Q3201=="3-4 times per week"|Q3201=="more than 5 times per week")) & (Q33=="Yes" & (Q3301=="3-4 times per week"|Q3301=="more than 5 times per week"))  )
                                 & 
                                   Q34==">3 months" 
                                 & 
                                   (Q35=="much" | Q35=="very much")
                                 , "Chronic insomnia", "No chronic insomnia")]
sleep20$insomniaChron <- factor(sleep20$insomniaChron, 
                                levels = c("No chronic insomnia", "Chronic insomnia"))
sleep20$insomniaChron <- relevel(sleep20$insomniaChron, ref = "No chronic insomnia")
label(sleep20$insomniaChron) <- "Chronic insomnia"

label(sleep20$Q23) <- "Has your sleeping behavior changed since the beginning of the corona measures?"
label(sleep20$Q73_9) <- "What has helped you the most do deal with corona restrictions? Sufficient sleep."

sleep20$Q25 <- revalue(as.factor(sleep20$Q25), c("1"="No", "2"="Yes"))
label(sleep20$Q25) <- "Nap during the day"
sleep20$napping <- sleep20$Q25
sleep20$napping <- relevel(sleep20$napping, ref="No")

# ______Caffeine, Alcohol ----
#define NAs
sleep20[Q4601==-9999,]$Q4601 <- NA
sleep20[Q4602==-9999,]$Q4602 <- NA
sleep20[Q4603==-9999,]$Q4603 <- NA
sleep20[Q4604==-9999,]$Q4604 <- NA
sleep20[Q4605==-9999,]$Q4605 <- NA
sleep20[Q4606==-9999,]$Q4606 <- NA
sleep20 <- sleep20 %>% 
  mutate(alcohol_intake = ifelse(!is.na(Q4601),Q4601,0) + 
           ifelse(!is.na(Q4602),Q4602,0) + 
           ifelse(!is.na(Q4603),Q4603,0),
         caffeine_intake = ifelse(!is.na(Q4604),Q4604,0) + 
           ifelse(!is.na(Q4605),Q4605,0) + 
           ifelse(!is.na(Q4606),Q4606,0)
  )

# ______Smoking-----
sleep20$Q54 <- revalue(as.factor(sleep20$Q54), c("1" = "No, never", 
                                                 "2" = "No, not anymore", 
                                                 "3" = "Yes, I currently smoke"))
sleep20$smoking <- sleep20$Q54
sleep20$smoking <- relevel(sleep20$smoking, ref = "No, never")

# ______Dinner time on workday ----
sleep20 <- sleep20 %>% 
  mutate(dinner_WorkDays = case_when(
    Q4503 %in% c(1, 2) ~ "<=1800",
    Q4503 == 3 ~ "18h to 19h",
    Q4503 == 4 ~ "19h to 20h",
    Q4503 == 5 ~ "20h to 21h",
    Q4503 == 9 ~ "I usually do not have dinner",
    Q4503 %in% c(6, 7, 8) ~ ">=2100"
  )) %>% mutate(across(dinner_WorkDays, as_factor))
sleep20$dinner_WorkDays <- factor(sleep20$dinner_WorkDays, 
                                  levels = c("<=1800", "18h to 19h", "19h to 20h", 
                                             "20h to 21h", "I usually do not have dinner"))
sleep20$dinner_WorkDays <- relevel(sleep20$dinner_WorkDays, ref = "<=1800")

# ______General health status-----
sleep20$Q59 <- revalue(as.factor(sleep20$Q59), c("1" = "very good", 
                                                 "2" = "good", 
                                                 "3" = "fair", 
                                                 "4" = "bad", 
                                                 "5" = "very bad"))
sleep20$general_health <- sleep20$Q59
sleep20$general_health <- relevel(sleep20$general_health, ref = "very good")


# ______BMI ----
sleep20$height_m <- sleep20$Q12/100
sleep20$mass_kg <- sleep20$Q13
sleep20$BMI <- round(sleep20$mass_kg/sleep20$height_m^2, digits = 1)
sleep20$BMI_category <- as.factor(wafflecut(sleep20$BMI, c('[30,100]','[18.5,24.9]','[25,29.9]', '[0,18.5)'), 
                                            c('obesity','normal weight','overweight','underweight')))
label(sleep20$BMI) <- "Body mass index (BMI)"
sleep20$Q5 <- as.factor(sleep20$Q5)
label(sleep20$Q5) <- "Austrian states"
sleep20$Q5 <- revalue(sleep20$Q5, c("1"="Burgenland", "2"="Carinthia","3"="Lower Austria","4"="Salzburg","5"="Styria",
                                    "6"="Tyrol","7"="Upper Austria", "8"="Vienna","9"="Vorarlberg"))

# ______Demographics ----
sleep20$Q6 <- as.factor(sleep20$Q6)
label(sleep20$Q5) <- "Residence"
sleep20$Q6 <- revalue(sleep20$Q6, c("1"="urban area", "2"="rural area ( <50.000 inhabitants)","3"="rural area ( >50.000 inhabitants)"))

sleep20$Q7 <- as.factor(sleep20$Q7)
label(sleep20$Q7) <- "Education (highest degree)"
sleep20$Q7 <- revalue(sleep20$Q7, c("1"="elementary School (Volksschule)", 
                                    "2"="high School (Hauptschule)", 
                                    "3"="Matura (Gymnasium)", 
                                    "4"="university degree and above"))
sleep20$education <- sleep20$Q7
# barplot education using ggplot:
ggplot(sleep20, aes(x = education)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Education (2020)", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # center title
  theme(plot.title = element_text(hjust = 0.5))
levels(sleep20$education) # "elementary School (Volksschule)" "high School (Hauptschule)"       "Matura (Gymnasium)"              "university degree and above"    
sleep20$education <- fct_collapse(
  sleep20$education,
  "Elementary or High School" = c("elementary School (Volksschule)", "high School (Hauptschule)")
)

boxplot(sleepWorkDays ~ Q7, data = sleep20)
sleep20$ethnicity <- as.factor(sleep20$Q76)
sleep20$ethnicity <- revalue(sleep20$ethnicity, c("1"="White/Caucasian", 
                                                  "2"="Asian",
                                                  "3"="Black or African American",
                                                  "4"="Other"))
table(sleep20$ethnicity)
ggplot(sleep20, aes(x = ethnicity)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Ethnien (2020)", x = "", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # center title
  theme(plot.title = element_text(hjust = 0.5))
sleep20$ethnicity <- fct_collapse(
  sleep20$ethnicity,
  "White" = c("White/Caucasian"),
  "Non-White" = c("Asian",
                  "Black or African American", 
                  "Other"))

# ______Marrital status ----
sleep20$Q8 <- as.factor(sleep20$Q8)
label(sleep20$Q8) <- "Marital status"
sleep20$Q8 <- revalue(sleep20$Q8, c("1"="single", "2"="married or in a partnership","3"="divorced","4"="widowed"))
sleep20 <- sleep20 %>% 
  mutate(marrital_status = case_when(
    Q8 == "married or in a partnership" ~ "married_or_partnership",
    Q8 != "married or in a partnership" ~ "single_divorced_widowed"
  )) %>% mutate(across(marrital_status, as_factor))
sleep20$marrital_status <- factor(sleep20$marrital_status, levels = c("married_or_partnership", "single_divorced_widowed"))
sleep20$marrital_status <- relevel(sleep20$marrital_status, ref = "married_or_partnership")

sleep20$Q9 <- revalue(as.factor(sleep20$Q9), c("1"="0", "2"="1","3"="2","4"="3 or more"))
sleep20$number_children <- sleep20$Q9
sleep20$number_children <- relevel(sleep20$number_children, ref = "0")

sleep20$Q16_5 <- as.factor(sleep20$Q16_5)
label(sleep20$Q16_5) <- "Student, further training, unpaid work experience"
sleep20$Q16_5 <- revalue(sleep20$Q16_5, c("1"="marked", "0"="not marked"))

sleep20$Q16_4 <- as.factor(sleep20$Q16_4)
label(sleep20$Q16_4) <- "Unemployed"
sleep20$Q16_4 <- revalue(sleep20$Q16_4, c("1"="marked", "0"="not marked"))

sleep20[, employed_full_part := ifelse(Q16_1 == 1 | Q16_2 == 1,"marked","not marked")]
sleep20$employed_full_part <- as.factor(sleep20$employed_full_part)
label(sleep20$employed_full_part) <- "Employed (full-/part time)"
label(sleep20$Q16_5) <- "Student, further training, unpaid work experience"
label(sleep20$Q16_4) <- "Unemployed"

# ______Shift work-----
sleep20$Q19 <- revalue(as.factor(sleep20$Q19), c("1"="No",
                                                 "2"="Yes, in the past",
                                                 "3"="Yes, currently"))
label(sleep20$Q19) <- "Have you ever worked night shifts?"
#sleep20 <- sleep20 %>% mutate(any_night_shifts = ifelse(Q18=="Regular night shifts" | Q18=="Rotating shifts including nights",1,0))


# Q18
#1 Regular day shifts
#2 Regular evening shifts
#3 Regular night shifts
#4 Rotating shifts including nights
#5 Rotating shifts without nights
#6 Other

sleep20$Q18 <- revalue(as.factor(sleep20$Q18), c("1"="Regular day shifts",
                                                 "2"="Regular evening shifts",
                                                 "3"="Regular night shifts",
                                                 "4"="Rotating shifts including nights",
                                                 "5"="Rotating shifts without nights",
                                                 "6"="Other"))

sleep20 <- sleep20 %>% mutate(work = case_when(
  (Q16_1 == 1 | Q16_1 == "marked") & (Q18 == "Rotating shifts including nights" | Q18 == 4 | Q18 == "Regular night shifts" | Q18 == 3) ~ "full_time_inkl_NS",
  (Q16_1 == 1 | Q16_1 == "marked") & (Q18 == "Regular day shifts" | Q18 == 1 | Q18 == "Rotating shifts without nights" | Q18 == 5 | Q18 == "Regular evening shifts" | Q18 == 2) ~ "full_time_exkl_NS",
  (Q16_2 == 1 | Q16_2 == "marked") & (Q18 == "Regular day shifts" | Q18 == 1 | Q18 == "Rotating shifts without nights" | Q18 == 5 | Q18 == "Regular evening shifts") ~ "part_time_exkl_NS",
  (Q16_2 == 1 | Q16_2 == "marked") & (Q18 == "Rotating shifts including nights" | Q18 == 4 | Q18 == "Regular night shifts" | Q18 == 3) ~ "part_time_inkl_NS",
  Q16_3 == 1 | Q16_3 == "marked" | Q16_4 == 1 | Q16_4 == "marked" | Q16_5 == 1 | Q16_5 == "marked" | Q16_6 == 1 | Q16_6 == "marked" | Q16_7 == 1| Q16_7 == "marked" | Q16_8 == 1 | Q16_8 == "marked" ~ "not_currently_working"
)) %>% mutate(across(work, as_factor))
sleep20$work <- factor(sleep20$work, levels = c("full_time_exkl_NS", "part_time_exkl_NS", "full_time_inkl_NS", "part_time_inkl_NS", "not_currently_working"))
sleep20$work <- relevel(sleep20$work, ref = "not_currently_working")

sleep20$Q6101 <- revalue(as.factor(sleep20$Q6101), c("1" = "No", "2" = "Yes"))
label(sleep20$Q6101) <- "Taking medications regularly in the past 12 months"

# ______Change in employment status -----
label(sleep20$Q68_1) <- "My employment status did not change compared to before the crisis and I continued working (e.g. in home office)."
label(sleep20$Q68_2) <- "My hours at work were reduced."
label(sleep20$Q68_3) <- "I lost my job."
label(sleep20$Q68_4) <- "I switched jobs."
label(sleep20$Q68_5) <- "Ich war auch caritativ taetig (z.B. Zivildiener, Nachbarschaftsbetreuung)"

label(sleep20$Q2) <- "Gender"
sleep20$Q2 <- as.factor(sleep20$Q2)
sleep20[Q2 == "3" | Q2 == "4",]$Q2 <- NA
sleep20$Q2 <- droplevels(sleep20$Q2)

label(sleep20$Q1) <- "Age"
sleep20$age <- sleep20$Q1
label(sleep20$Q8) <- "Marital status"
#age
sleep20$Q1_cat <- as.factor(sleep20$Q1)
label(sleep20$Q1_cat) <- "Age"
sleep20$Q1_cat <- revalue(sleep20$Q1_cat, c("1" = "<20 years",
                                            "2" = "20-24 years",
                                            "3" = "25-29 years",
                                            "4" = "30-34 years",
                                            "5" = "35-39 years",
                                            "6" = "40-44 years",
                                            "7" = "45-49 years",
                                            "8" = "50-54 years",
                                            "9" = "55-59 years",
                                            "10" = "60-64 years",
                                            "11" = "65-69 years",
                                            "12" = "70 or older"))


label(sleep20$Q10) <- "Size of household"
sleep20$Q10 <- mapvalues(as.factor(sleep20$Q10), from=c("1","2","3","4","5","6"), to=c("One person", "Two persons", "Three persons", "Four persons", "Five or more persons", "Five or more persons"))
label(sleep20$Q5) <- "Austrian states"
label(sleep20$Q6) <- "Residence"
label(sleep20$Q7) <- "Education (highest degree)"
sleep20$Q2 <- mapvalues(sleep20$Q2, from=c(1, 2), to=c("Women", "Men"))
sleep20$gender <- sleep20$Q2

# Sleep variables
label(sleep20$Q3001) <- "How often did you have trouble falling asleep?"
levels(sleep20$Q3001) <- list("less or once per week" = "less or once per week", "1-2 times per week"="1-2 times per week", ">=3 times per week"=c("3-4 times per week", "more than 5 times per week"))

sleep20$Q38 <- revalue(as.factor(sleep20$Q38), c("1"="No","2"="Yes"))
sleep20$diagn_sleep_disorder <- sleep20$Q38
sleep20$diagn_sleep_disorder <- relevel(sleep20$diagn_sleep_disorder, ref="No")

label(sleep20$Q39) <- "How alert / sleepy did you feel during a working day over the last 4 weeks"
sleep20$Q39_numeric <- sleep20$Q39
label(sleep20$Q39_numeric) <- "KSS score"
sleep20$Q39 <- revalue(as.factor(sleep20$Q39), c("1"="extremely alert","2"="very alert","3"="alert","4"="rather alert","5"="neither alert nor sleepy", "6"="some signs of sleepiness","7"="sleepy, but no difficulty remaining awake","8"="sleepy, some effort to keep alert","9"="extremely sleepy - fighting sleep"))

sleep20$Q36 <- as.factor(sleep20$Q36)
label(sleep20$Q36) <- "Has your family doctor ever asked you about your sleep?"
sleep20$Q36 <- revalue(sleep20$Q36, c("2"="Yes","1"="No"))

sleep20$Q37 <- as.factor(sleep20$Q37)
label(sleep20$Q37) <- "Ever asking for medical help or being treated for sleep problem"
sleep20$Q37 <- revalue(sleep20$Q37, c("2"="Yes","1"="No"))

label(sleep20$Q3701_1) <- "Medical help for sleep problem: family doctor or GP"
sleep20$Q3701_1 <- revalue(as.factor(sleep20$Q3701_1), c("1"="marked","0"="not marked"))

label(sleep20$Q3701_2) <- "Medical help for sleep problem: psychologist or psychotherapist"
sleep20$Q3701_2 <- revalue(as.factor(sleep20$Q3701_2), c("1"="marked","0"="not marked"))

label(sleep20$Q3701_3) <- "Other clinitian"
sleep20$Q3701_3 <- revalue(as.factor(sleep20$Q3701_3), c("1"="marked","0"="not marked"))

label(sleep20$Q3701_4) <- "Medical help for sleep problem: pharmacist"
sleep20$Q3701_4 <- revalue(as.factor(sleep20$Q3701_4), c("1"="marked","0"="not marked"))

label(sleep20$Q3701_5) <- "Medical help for sleep problem: other"
sleep20$Q3701_5 <- revalue(as.factor(sleep20$Q3701_5), c("1"="marked","0"="not marked"))

label(sleep20$Q3702_2) <- "Ever taking sleep medication: sleeping pills"
sleep20$Q3702_2 <- revalue(as.factor(sleep20$Q3702_2), c("1"="marked","0"="not marked"))

label(sleep20$Q3702_3) <- "Ever taking sleep medication: natural remedies"
sleep20$Q3702_3 <- revalue(as.factor(sleep20$Q3702_3), c("1"="marked","0"="not marked"))

label(sleep20$Q3702_4) <- "Ever taking sleep medication: melatonin"
sleep20$Q3702_4 <- revalue(as.factor(sleep20$Q3702_4), c("1"="marked","0"="not marked"))

label(sleep20$Q3702_5) <- "Ever taking sleep medication: other"
sleep20$Q3702_5 <- revalue(as.factor(sleep20$Q3702_5), c("1"="marked","0"="not marked"))

sleep20$sleep_medication <- case_when(
  sleep20$Q3702_2 == "marked" | sleep20$Q3702_3 == "marked" | 
    sleep20$Q3702_4 == "marked" | sleep20$Q3702_5 == "marked" ~ "Yes",
  sleep20$Q3702_2 == "not marked" & sleep20$Q3702_3 == "not marked" & 
    sleep20$Q3702_4 == "not marked" & sleep20$Q3702_5 == "not marked" ~ "No",
  TRUE ~ NA_character_
)

table(sleep20$sleep_medication) # 121
boxplot(sleepWorkDays ~ sleep_medication, data = sleep20, 
        main = "Sleep duration by sleep medication", 
        xlab = "Sleep medication", 
        ylab = "Sleep duration (hours)")
t.test(sleep20$sleepWorkDays ~ sleep_medication, data = sleep20) # sign

sleep20$year <- factor("2020")

# ______Optimism score ----
# optimism score LOT-R -> https://www.cmu.edu/dietrich/psychology/pdf/scales/LOTR_Scale.pdf
#1) =ITEM 1 -> In uncertain times, I usually expect the best.
sleep20$Q43_1 <- sleep20$Q43_1-1

#2) =ITEM 3 -> If something can go wrong with me it will.
sleep20$Q43_2 <- sleep20$Q43_2-1

#3) =ITEM 4 -> I am always optimistic about the future.
sleep20$Q43_3 <- sleep20$Q43_3-1

#4) =ITEM 7 -> I hardly ever expect things to go my way.
sleep20$Q43_4 <- sleep20$Q43_4-1

#5) =ITEM 9 -> I rarely count on good things happening to me.
sleep20$Q43_5 <- sleep20$Q43_5-1

#6) =ITEM 10 -> Overall, I expect more good things to happen to me than bad.
sleep20$Q43_6 <- sleep20$Q43_6-1

sleep20 <- sleep20 %>% mutate(optimScore = Q43_1 + (4-Q43_2) + Q43_3 + (4-Q43_4) + (4-Q43_5) + Q43_6)
label(sleep20$optimScore) <- "Optimism Score (LOT-R)"

summary(sleep20$optimScore)
plot(sleep20$optimScore, sleep20$sleepWorkDays, 
     xlab = "Optimism Score", 
     ylab = "Sleep duration (hours)", 
     main = "Optimism Score and Sleep duration")
cor.test(sleep20$optimScore, sleep20$sleepWorkDays, method = "pearson") # sign

# 10 min intense sports... CORONA???
#sleep20$Q48
sleep20$Q48_numeric <- sleep20$Q48-1 # change range from 1-8 to 0-7 to harmonise with 2017

sleep20 <- sleep20 %>% mutate(sport_yes_no = case_when(
  Q48 == 1 ~ "No",
  Q48 >= 2 ~ "Yes"
)) %>% mutate_at(vars(sport_yes_no), funs(as_factor))
sleep20$sport_yes_no <- relevel(sleep20$sport_yes_no, ref = "No")

# ______Weight change ----
#Q161 Im Vergleich zu den Zeiten vor den Corona-Massnahmen hat sich mein Gewicht veraendert:
sleep20 <- sleep20 %>% mutate(weight_change = case_when(
  Q161 == 1 ~ "no_change",
  Q161 %in% c(2,3,4,5,6,7) ~ "gained_up_to_10_kg",
  Q161 == 8 ~ "gained_11_to_20_kg",
  Q161 == 9 ~ "gained_more_than_20_kg",
  Q161 %in% c(10,11,12,13,14,15) ~ "lost_up_to_10_kg",
  Q161 == 16 ~ "lost_11_to_20_kg",
  Q161 == 17 ~ "lost_more_than_20_kg",
)) %>% mutate(across(weight_change, as_factor))

# ______Home office ----
sleep20 <- sleep20 %>% mutate(home_office = case_when(
  is.na(Q69) ~ "dummy",
  Q69 == 0 ~ "No",
  Q69 == 1 ~ "Yes_all_the_time",
  Q69 == 2 ~ "Yes_partially",
)) %>% mutate(across(home_office, as_factor))
sleep20$home_office <- relevel(sleep20$home_office, ref = "No")

sleep20 <- sleep20 %>% mutate(home_office_yes_no = case_when(
  is.na(Q69) ~ "dummy",
  Q69 == 0 ~ "No",
  Q69 == 1 ~ "Yes",
  Q69 == 2 ~ "Yes",
)) %>% mutate(across(home_office_yes_no, as_factor))
sleep20$home_office_yes_no <- relevel(sleep20$home_office_yes_no, ref = "No")

sleep20 <- sleep20 %>% dplyr::mutate(ho_new = case_when(
  home_office_yes_no == "dummy" |  home_office_yes_no == "No" ~ 0,
  home_office_yes_no == "Yes" ~ 1
))

# ______Sleep quality compare to before corona measures----
# "Im Vergleich zu vor den Corona-Massnahmen hat sich meine Schlafqualitaet..."
sleep20 <- sleep20 %>% mutate(sleep_quality_change = case_when(
  Q21 == 1 ~ "much_worse",
  Q21 == 2 ~ "worse",
  Q21 == 3 ~ "no_change",
  Q21 == 4 ~ "improved",
  Q21 == 5 ~ "improved_a_lot",
)) %>% mutate(across(sleep_quality_change, as_factor))
sleep20$sleep_quality_change <- relevel(sleep20$sleep_quality_change, ref = "no_change")

# ______Alcohol consumption change? -----
# Haben sich seit Beginn der Corona-Ma?nahmen Ihre Alkoholkonsum-Gewohnheiten ver?ndert?
sleep20 <- sleep20 %>% mutate(alc_change = case_when(
  Q531 == 1 ~ "No",
  Q531 == 2 ~ "Yes"
)) %>% mutate(across(alc_change, as_factor))
sleep20$alc_change <- relevel(sleep20$alc_change, ref = "No")

# ______Coffee change?----
# Haben sich seit Beginn der Corona-Ma?nahmen Ihre Kaffeekonsum-Gewohnheiten ver?ndert?
sleep20 <- sleep20 %>% mutate(coff_change = case_when(
  Q533 == 1 ~ "No",
  Q533 == 2 ~ "Yes"
)) %>% mutate(across(coff_change, as_factor))
sleep20$coff_change <- relevel(sleep20$coff_change, ref = "No")

# ______Food habits change? ----
# Haben sich seit Beginn der Corona-Ma?nahmen Ihre Ern?hrungsgewohnheiten ver?ndert?
sleep20 <- sleep20 %>% mutate(diet_change = case_when(
  Q541 == 1 ~ "No",
  Q541 == 2 ~ "Yes"
)) %>% mutate(across(diet_change, as_factor))
sleep20$diet_change <- factor(sleep20$diet_change, levels = c("No", "Yes"))
sleep20$diet_change <- relevel(sleep20$diet_change, ref = "No")

# ______Food habits change how?----
# Wie haben sich seit Beginn der Corona-Ma?nahmen Ihre Ern?hrungsgewohnheiten ver?ndert?
sleep20 <- sleep20 %>% mutate(diet_change_how = case_when(
  Q542 == 1 ~ "diet_improved",
  Q542 == 2 ~ "diet_worsened"
)) %>% mutate(across(diet_change_how, as_factor))

# ______Sports change?----
# Im Vergleich zu vor der Corona-Ma?nahmen hat das Ausma? an Sport, den ich betreibe...
sleep20 <- sleep20 %>% mutate(sport_change = case_when(
  Q40 == 1 ~ "declined_strongly",
  Q40 == 2 ~ "declined",
  Q40 == 3 ~ "no_change",
  Q40 == 4 ~ "increased",
  Q40 == 5 ~ "increased_strongly"
)) %>% mutate(across(sport_change, as_factor))

# ______Going for walks----
# Bitte beantworten Sie die gesamte Frage  An wie vielen Tagen in der Zeit seit Einf?hrung der Corona-Ma?nahmen waren Sie f?r mindestens 10 Minuten Spazieren?
sleep20$days_walking <- sleep20$Q50 - 1
label(sleep20$days_walking) <- "Days walking for at least 10 minutes (per week)"

# ______Smoking since corona?----
# Rauchen Sie seit Beginn der Corona-Ma?nahmen:
sleep20 <- sleep20 %>% mutate(smoking_change = case_when(
  Q41 == 1 ~ "more_than_before",
  Q41 == 2 ~ "less_than_before",
  Q41 == 3 ~ "no_change"
)) %>% mutate(across(smoking_change, as_factor))

# ______Digital device use?----
# Hat sich im Vergleich zur Zeit vor den Corona-Ma?nahmen Ihre Nutzungsdauer digitaler Endger?te (TV, PC-Bildschirm, Handy, Pad oder Tablet, Smart Watch) ver?ndert?
sleep20 <- sleep20 %>% mutate(digital_device_use = case_when(
  Q42 == 1 ~ "no_change",
  Q42 == 2 ~ "increased",
  Q42 == 3 ~ "decreased"
)) %>% mutate(across(digital_device_use, as_factor))
sleep20$digital_device_use <- relevel(sleep20$digital_device_use, ref = "no_change")

# ______How much change?----
# Um wieviel Prozent hat sich Ihre Nutzungsdauer ver?ndert?
sleep20 <- sleep20 %>% mutate(digital_device_use_change_percent = case_when(
  Q47 == 1 ~ "<25%",
  Q47 == 2 ~ "25-50%",
  Q47 == 3 ~ ">75%"
)) %>% mutate(across(digital_device_use_change_percent, as_factor))

# ______Emotional burden? ----
# Wie emotional belastend/beeintr?chtigend empfinden Sie die Zeit seit Beginn der Corona-Ma?nahmen:
sleep20 <- sleep20 %>% mutate(emotional_burden_measures = case_when(
  Q65 == 1 ~ "very_challenging",
  Q65 == 2 ~ "somewhat_challenging",
  Q65 == 3 ~ "not_at_all_challenging"
)) %>% mutate(across(emotional_burden_measures, as_factor))
sleep20$emotional_burden_measures <- relevel(sleep20$emotional_burden_measures, ref = "not_at_all_challenging")

# ______Financial burden?-----
# Haben die Corona-Ma?nahmen Auswirkungen auf Ihre finanziellen Ressourcen?
sleep20 <- sleep20 %>% mutate(financial_burden_measures = case_when(
  Q66 == 1 ~ "None",
  Q66 == 2 ~ "small_burden",
  Q66 == 3 ~ "strong_burden",
  Q66 == 4 ~ "almost_no_fin_resources"
)) %>% mutate(across(financial_burden_measures, as_factor))
sleep20$financial_burden_measures <- relevel(sleep20$financial_burden_measures, ref = "None")

# ______Attitude change?----
# Haben Sie seit Beginn der Corona-Ma?nahmen Ver?nderungen in Ihrer allgemeinen Einstellung zum Leben/Zukunft bemerkt?
sleep20 <- sleep20 %>% mutate(attitude_towards_future = case_when(
  Q67 == 1 ~ "no_change",
  Q67 == 2 ~ "somewhat_more_thoughtful",
  Q67 == 3 ~ "very_thoughtful",
  Q67 == 4 ~ "very_thoughtful_and_pessimistic"
)) %>% mutate(across(attitude_towards_future, as_factor))
sleep20$attitude_towards_future <- relevel(sleep20$attitude_towards_future, ref = "no_change")


dim(sleep20) # 1010  348

# 2020---Clean--------------------------------------------------------------------

# ______1) sleepWorkDays: ----

# ________Inclusion flowchart 2020-----------
fc20 <- sleep20 %>% 
  as_fc(label="Participants 2020") %>%
  fc_filter(!grepl("Please Choose", bedTimeWorkDay, 
                   fixed=TRUE) | !grepl("Please Choose", wakeTimeWorkDay, fixed=TRUE), 
            show_exc = TRUE,
            label = "Bed and wake times available",
            label_exc = "Bed or wake times missing") %>%
  fc_filter(sleepWorkDays > 0,
            label = "Sleep duration > 0",
            label_exc = "Sleep duration ≤ 0",
            show_exc = TRUE) %>%
  fc_filter(abs(sleepWorkDays - sleepWorkDays) <= 3.5,
            label = "Discrepancy ≤ 3.5h",
            label_exc = "Discrepancy > 3.5h",
            show_exc = TRUE) %>%
  fc_filter(sleepWorkDays > 3.5,
            label = "Sleep duration > 3.5h",
            label_exc = "Sleep duration ≤ 3.5h",
            show_exc = TRUE) %>%
  fc_filter(!is.na(gender),
            label = "Sex available",
            label_exc = "Sex missing",
            show_exc = TRUE) %>%
  fc_filter(!is.na(dinner_WorkDays),
            label = "Dinner time work days available",
            label_exc = "Dinner time work days missing",
            show_exc = TRUE) %>%
  fc_filter(!is.na(work),
            label = "Work information available",
            label_exc = "Work information missing",
            show_exc = TRUE) %>%
  fc_draw()

# FIGURE 1----------
g1 <- grid.grabExpr(fc_draw(fc17))
g2 <- grid.grabExpr(fc_draw(fc20))

grid.arrange(g1, g2, ncol = 2)


#View(sleep20[sleepWorkDays < 4, c("QID","bedTimeWorkDay", "wakeTimeWorkDay","sleepWorkDays","bedTimeFreeDay","wakeTimeFreeDay","sleepFreeDays","Q19","Q1")])

# ________2020 analytic data set--------

if(analytical_data_set_version == "(1)"){ # throw out rows
  
  sleep20 <- sleep20 %>%
    # Step 1: eliminate if "Please Choose" is in the bedTimeWorkDay or wakeTimeWorkDay
    dplyr::filter(
      !grepl("Please Choose", bedTimeWorkDay, fixed = TRUE) | 
        !grepl("Please Choose", wakeTimeWorkDay, fixed = TRUE)
    ) %>%
    # Step 2: eliminate if sleepWorkDays == 0
    dplyr::filter(sleepWorkDays > 0) %>%
    # Step 3: Transform sleepWorkDays > 15: subtract 12h
    dplyr::mutate(
      sleepWorkDays = ifelse(sleepWorkDays > 15, sleepWorkDays - 12, sleepWorkDays)
    ) %>%
    # Step 4: discrepancy not possible in 2020
    # Step 5: exclude sleepWorkDays < 3.5
    dplyr::filter(sleepWorkDays > 3.5) %>%
    dplyr::filter(!is.na(gender)) %>%
    dplyr::filter(!is.na(dinner_WorkDays)) %>%
    dplyr::filter(!is.na(work))
  dim(sleep20) # Check dimensions
  
} else if(analytical_data_set_version == "(2)"){ # keep rows, set to NA
  
  # ________SET NAs-----------
  
  # _______Step 1 (0 cases): Eliminate if "Please Choose" is in the bedTimeWorkDay or wakeTimeWorkDay:----
  sleep20[grepl("Please Choose", bedTimeWorkDay, fixed=TRUE) | grepl("Please Choose", wakeTimeWorkDay, fixed=TRUE),]$sleepWorkDays <- as.numeric(NA)
  
  # _______Step 2 (65 cases): sleepWorkdays == 0 -> set sleepWorkdays to NA----
  sleep20[sleepWorkDays == 0,]$sleepWorkDays <- as.numeric(NA) # bw times are not necessarily useless
  
  # _______Step 3 (71 cases, 0 cases lost): Transform sleepWorkDays_bw > 15: subtract 12h.----
  sleep20[sleepWorkDays > 15, ]$sleepWorkDays <- sleep20[sleepWorkDays > 15,]$sleepWorkDays - 12
  
  # _______Step 4: discrepancy -> not possible------
  
  # _______Step 5 (38 cases): exclude sleepWorkDays < 3.5:-------
  sleep20[sleepWorkDays < 3.5,]$sleepWorkDays <- as.numeric(NA)
  
  
  sum(is.na(sleep20$sleepWorkDays)) # 101 missing
  
  
}

dim(sleep20) # 847 348 (1)
hist(sleep20$sleepWorkDays, breaks = 50, main = "Sleep duration work days", xlab = "hours")

table(sleep20$ethnicity)
# White/Caucasian Asian Black or African American       Other 
# 809               14         1                        23
boxplot(sleepWorkDays ~ ethnicity, data = sleep20)

# ______2) sleepFreeDays:----

# OLD___
# CHECK: small and large sleep durations: FreeDays
#sleep20[sleepFreeDays > 15 | sleepFreeDays < 5, c("QID","bedTimeWorkDay", "wakeTimeWorkDay","sleepWorkDays", 
#                                                  "bedTimeFreeDay","wakeTimeFreeDay","sleepFreeDays")]
#166
# CHECK: Stefan input: chronic insomniacs often sleep 3-4 hours subjectively:
#sleep20[(sleepFreeDays < 4 & insomniaChron == "Chronic insomnia"), c("QID","bedTimeWorkDay", "wakeTimeWorkDay","sleepWorkDays", 
#                                                  "bedTimeFreeDay","wakeTimeFreeDay","sleepFreeDays")]
#9
# FILTER chronic insomniacs below 3 hours of sleep:
#sleep20[sleepFreeDays < 4 & insomniaChron == "Chronic insomnia", ]$sleepFreeDays <- as.numeric(NA)

# FILTER: sleep durations of more than 15 hours and non-insomniacs sleeping less than 5 hours
#sleep20[sleepFreeDays > 15 | (insomniaChron != "Chronic insomnia" & sleepFreeDays < 5),]$sleepFreeDays <- as.numeric(NA) # this rather crude filter could be improved
# (by imputing manually) if sample size should be too small later

# CHECK: randomly normal sleep times
#sleep20[sample(1:dim(sleep20)[1], 100, replace = FALSE), c("bedTimeWorkDay", "wakeTimeWorkDay","sleepWorkDays", 
#                                                           "bedTimeFreeDay","wakeTimeFreeDay","sleepFreeDays")]
# --> calculation of sleep times seems fine!

# ______3) min sleep required:----

# FILTER for Q2401/2: "What is the minimum number of hours you need to sleep to function at your best during a 24 hour period? - Hours"
#sleep20[min_sleep_required < 3 | min_sleep_required > 20,]$min_sleep_required <- as.numeric(NA)


# ______4) Social Jet Lag (SJL)----
# not used in analysis
sleep20[is.na(sleepWorkDays) | is.na(sleepFreeDays) | SJL < -2 | SJL > 8,]$SJL <- NA

# ______5) BMI, winsorize, set obs above 99% to the 99%th percentile and same with 1%. ----
perc99 <- quantile(sleep20$BMI, probs=0.99)
perc1 <- quantile(sleep20$BMI, probs=0.01)
sleep20$BMI <- ifelse(sleep20$BMI < perc1, perc1, sleep20$BMI)
sleep20$BMI <- ifelse(sleep20$BMI > perc99, perc99, sleep20$BMI)

# ______6) min_sleep_required winsorize ----
perc99 <- quantile(sleep20$min_sleep_required, na.rm = TRUE, probs = 0.99)
perc1 <- quantile(sleep20$min_sleep_required, na.rm = TRUE, probs = 0.01)
sleep20$min_sleep_required <- ifelse(sleep20$min_sleep_required < perc1, perc1, sleep20$min_sleep_required)
sleep20$min_sleep_required <- ifelse(sleep20$min_sleep_required > perc99, perc99, sleep20$min_sleep_required)

# ______7) alcohol_intake, winsorize top 99% ----
perc99 <- quantile(sleep20$alcohol_intake, na.rm = TRUE, probs = 0.99)
sleep20$alcohol_intake <- ifelse(sleep20$alcohol_intake > perc99, perc99, sleep20$alcohol_intake)

# ______8) caffeine_intake, winsorize top 99% ----
perc99 <- quantile(sleep20$caffeine_intake, na.rm = TRUE, probs = 0.99)
sleep20$caffeine_intake <- ifelse(sleep20$caffeine_intake > perc99, perc99, sleep20$caffeine_intake)





# SAVE data sets----------------------------------------------------------------
if(analytical_data_set_version == "(1)"){ # throw out rows

  saveRDS(sleep17, "./DATA/sleep17_an.RDS")
  saveRDS(sleep20, "./DATA/sleep20_an.RDS")
  cat(analytical_data_set_version, ": throw out rows \n")
  print(dim(sleep17)) # 784 671
  print(dim(sleep20)) # 847 348
  
  
} else if(analytical_data_set_version == "(2)"){ # keep rows, set to NA
  
  saveRDS(sleep17, "./DATA/sleep17.RDS")
  saveRDS(sleep20, "./DATA/sleep20.RDS")
  cat(analytical_data_set_version,": keep rows, set NA \n")
  print(dim(sleep17)) #  1004  671
  print(dim(sleep20)) # 1010  348
  
}

# missings in new covariables:--------
# _2017---------
vis_miss(sleep17 %>% dplyr::select(ethnicity, sleep_medication, 
                                   optimScore, education)) + 
  ggtitle("Missings 2017") +
  theme(plot.title = element_text(hjust = 0.5))
unique(sleep17$education)
unique(sleep17$optimScore)
unique(sleep17$sleep_medication) # NA
unique(sleep17$ethnicity)

# 2020---------
vis_miss(sleep20 %>% dplyr::select(ethnicity, sleep_medication, 
                                   optimScore, education)) +
  ggtitle("Missings 2020") +
  theme(plot.title = element_text(hjust = 0.5))

# Check: Run through without stop: 25.6.25
date_string <- format(Sys.Date(), "%Y-%m-%d")
file_name <- paste0("./z_Session_Infos/Read_Prepare_Clean_", date_string, ".txt")
# save session info
sink(file_name)
sessionInfo()
sink()



















































