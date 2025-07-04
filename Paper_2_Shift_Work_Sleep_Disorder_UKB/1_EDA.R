# DATA Overview/exploration

# https://github.com/jdegenfellner/Paper2-dissertation/blob/main/1_EDA.R

# Load packages ----------------------------------------------------------------
library(pacman)
pacman::p_load(
  data.table, plyr, dplyr, tidyverse, ggmosaic, DataExplorer, crosstable,
  mgcv, fancycut, pscl, sure, blorr, glmtoolbox, caret, naniar, visdat,
  Hmisc, mice, miceFast, micemd, gtsummary, flextable, quarto, tictoc,
  MASS, officer, corrgram, rFSA, miceadds, YesSiR, janitor, PRISMAstatement,
  grid, gridExtra, htmlwidgets, webshot, magick, ggpubr
)

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ukb_reg <- ukb %>% dplyr::select(
  
  # Definition(s) Shift Work Disorder (SWD)
  #SWD_2, # Usually (or more) night shift work and not 'any insomnia symptoms' 
  SWD_5, # Usually (or more) night shift work and no 'frequent insomnia symptoms'
  InsomniaDisorder, # Validated phenotype (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5600256/)
  night_shift_work, # frequency of night shift work
  
  # Traits
  age, 
  sex,
  ethnicity,
  chronotype,
  #neuroticism_score, # Debut: Jan 2015, would explain missingness?
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
  f.1950.0.0,
  
  # Modifiable (risk) factors
  time_employed_in_current_job,
  BMI,
  smoking_status, 
  #MET_minutes_week, # Debut Jan 2018, see http://bmjopen.bmj.com/content/6/3/e010038 # maybe omit
  #difficulty_not_smoking_for_a_day, 
  coffee_intake, 
  alcohol_intake_frequency,
  # Other factors
  #f.189.0.0
) %>% 
  # ___b) Exclusions #-----------------
filter(age <= 65, 
       night_shift_work == "Always" |  night_shift_work == "Usually",
       time_employed_in_current_job >= 0) %>%
  filter(chronotype != "Prefer not to answer" | is.na(chronotype)) %>% # leave in NAs (imputed later)
  mutate(chronotype = droplevels(chronotype, "Prefer not to answer")) %>%
  filter(alcohol_intake_frequency != "Prefer not to answer" | is.na(alcohol_intake_frequency)) %>% # these levels distort the estimates
  mutate(alcohol_intake_frequency = droplevels(alcohol_intake_frequency, "Prefer not to answer")) %>% # just to be save that the logistic regression is not singular
  filter(InsomniaDisorder != "Prefer not to answer" | is.na(InsomniaDisorder)) %>% 
  mutate(InsomniaDisorder = droplevels(InsomniaDisorder, "Prefer not to answer"))
#filter(sociability_scale != 0) # possibly the reason for degenerate logistic regression in complete case analysis?
#mutate(smoking_status = droplevels(smoking_status, "Prefer not to anwer"))

# Violin-Plot Time employed in current job----
ukb_reg %>%
  ggplot(aes(x = '', y = time_employed_in_current_job)) +
  geom_violin(fill = 'lightgray') +
  geom_jitter(width = 0.1, size = 0.2, color = 'blue') +
  stat_summary(fun=median, geom="point", shape=23, size=3, fill="red") +
  labs(title = 'Time employed in current job',
       x = '',
       y = 'Years') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()





# Compare alcohol distributions complete big5 vs. at least one personality scale missing----

# _a) Missing at least one big-5 scales----
ukb_reg <- ukb_reg %>% 
  dplyr::mutate(data_type = ifelse(is.na(nervousness_scale) |
                                     is.na(sociability_scale) |
                                     is.na(warmth_scale) |
                                     is.na(diligence_scale) |
                                     is.na(curiosity_scale), "missing_at_least_one_big_5", 
                                   "Non-Missing"))

plot_data <- ukb_reg %>%
  group_by(data_type, alcohol_intake_frequency) %>%
  tally() %>%
  mutate(percentage = n / sum(n) * 100)

p1 <- ggplot(plot_data, aes(x = alcohol_intake_frequency, y = percentage, 
                      fill = data_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 1), vjust = -0.5) +
  ylab("Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + 
  #ggtitle("Alcohol frequency complete.case compared with full") + 
  ggtitle("Alcohol frequency in complete cases for personality traits 
          compared with at least one big 5 trait missing") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("Non-Missing" = "green", "missing_at_least_one_big_5" = "red"),
                    labels = c("Personality traits complete", "Missing in at least one personality trait")) +
  theme(legend.position = "bottom", legend.title = element_blank())
p1

# _b) Missing ANY variable in ukb_reg----
ukb_reg <- ukb_reg %>% 
  dplyr::mutate(data_type = ifelse(!complete.cases(.), "Missing", "Non_missing_Complete.case"))

plot_data <- ukb_reg %>%
  group_by(data_type, alcohol_intake_frequency) %>%
  tally() %>%
  mutate(percentage = n / sum(n) * 100)

p2 <- ggplot(plot_data, aes(x = alcohol_intake_frequency, y = percentage, 
                      fill = data_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 1), vjust = -0.5) +
  ylab("Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + 
  #ggtitle("Alcohol frequency complete.case compared with full") + 
  ggtitle("Alcohol frequency complete cases compared with any missing at") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("Non_missing_Complete.case" = "green", "Missing" = "red"),
                    labels = c("Complete case", "Missing any variable")) +
  theme(legend.position = "bottom", legend.title = element_blank())
p2

p1 <- p1 + ggtitle("Alcohol frequency in complete cases for\npersonality traits compared with at least one big 5 trait missing")
p2 <- p2 + ggtitle("Alcohol frequency complete cases compared\nwith any missing")

alc_plot <- ggarrange(p1, p2, ncol = 2)
ggsave(filename = "./Plots/Alcohol_plot.pdf", plot = alc_plot, width = 14, height = 6)


#split(plot_data, plot_data$data_type) %>%
#  lapply(function(df) sum(df$percentage))





# __Sleep duration of SWD_5==Yes/No:
ukb_reg <- ukb %>% dplyr::select(
  age, 
  sex,
  ethnicity,
  #MET_minutes_week, # Debut Jan 2018, only for 1267! see http://bmjopen.bmj.com/content/6/3/e010038
  BMI,
  SWD_5,
  sleep_duration, # sleep duration
  dozing_off,
  longest_period_of_depression, 
  number_of_depression_episodes, 
  night_shift_work,
  time_employed_in_current_job,
  chronotype,
  neuroticism_score,
  nervousness_scale, # (caution: proxy for neuroticism)
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
  f.1950.0.0,
  smoking_status, 
  #difficulty_not_smoking_for_a_day, 
  coffee_intake, 
  alcohol_intake_frequency) %>% 
  filter(age <= 65, 
         night_shift_work == "Always" |  night_shift_work == "Usually",
         time_employed_in_current_job >= 0, !is.na(SWD_5))

# Lower sleep duration if SWD == Yes?
p <- wilcox.test(ukb_reg[SWD_5 == "No",]$f.1160.0.0, ukb_reg[SWD_5 == "Yes",]$f.1160.0.0)$p.value
diff_means <- mean(ukb_reg[SWD_5 == "No",]$f.1160.0.0) - mean(ukb_reg[SWD_5 == "Yes",]$f.1160.0.0)
diff_medians <- median(ukb_reg[SWD_5 == "No",]$f.1160.0.0) - median(ukb_reg[SWD_5 == "Yes",]$f.1160.0.0)
ukb_reg %>% ggplot(aes(x = SWD_5, y = f.1160.0.0)) + 
  geom_boxplot() +
  ylab("Sleep duration in hours") + xlab("Shift work disorder (SWD_5)") + 
  ggtitle(paste0("p_wilcox = ",round(p,3))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = "No", y = 16, label = paste0("diff in means = ", round(diff_means,2)," h")) + 
  annotate("text", x = "No", y = 14, label = paste0("diff in medians = ", diff_medians," h"))

# __SWD_5 and deprivation index
ukb_reg <- ukb %>% dplyr::select(
  age, 
  sex,
  ethnicity,
  MET_minutes_week, # Debut Jan 2018, only for 1267! see http://bmjopen.bmj.com/content/6/3/e010038
  BMI,
  SWD_5,
  f.1160.0.0, 
  f.189.0.0,
  night_shift_work,
  time_employed_in_current_job,
  chronotype,
  neuroticism_score,
  nervousness_scale, # (caution: proxy for neuroticism)
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
  f.1950.0.0,
  smoking_status, 
  #difficulty_not_smoking_for_a_day, 
  coffee_intake, 
  alcohol_intake_frequency) %>% 
  filter(age <= 65, 
         night_shift_work == "Always" |  night_shift_work == "Usually",
         time_employed_in_current_job >= 0, !is.na(SWD_5))

# Higher deprivation index?
p <- wilcox.test(ukb_reg[SWD_5=="No",]$f.189.0.0, ukb_reg[SWD_5=="Yes",]$f.189.0.0)$p.value
ukb_reg %>% ggplot(aes(x=SWD_5, y=f.189.0.0)) + 
  geom_boxplot() +
  ylab("Townsend deprivation index at recruitment") + xlab("Shift work disorder (SWD_5)")+ 
  ggtitle(paste0("p_wilcox = ",p))


# Density plot: Sleep duration in hours
df1 <- data.frame(x = ukb_reg[SWD_5=="No",]$f.1160.0.0)
df2 <- data.frame(y = ukb_reg[SWD_5=="Yes",]$f.1160.0.0)
ggplot() + 
  geom_density(data = df1, aes(x = x), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = df2, aes(x = y),
               fill = "#56B4E9", color = "black", alpha = 0.7)

# When were night shift workers recruited?
# f.22620.0.0 in online follow up
#ukb[night_shift_work == "Usually" | night_shift_work == "Always",]$date_attending_ass_centre

# How many participants visited 3 times?
dim(ukb[!is.na(f.53.0.0) & !is.na(f.53.1.0) & !is.na(f.53.2.0),]) # 7380
# How many participants visited 2 times?
dim(ukb[(is.na(f.53.0.0) & !is.na(f.53.1.0) & !is.na(f.53.2.0)) |
          (!is.na(f.53.0.0) & is.na(f.53.1.0) & !is.na(f.53.2.0)) |
          (!is.na(f.53.0.0) & !is.na(f.53.1.0) & is.na(f.53.2.0)),]) # 41613



# __Compare 2 neuroticism-scores
ukb$neuroticism_score
ukb$nervousness_scale
cor(ukb$neuroticism_score, ukb$nervousness_scale, use = "pairwise.complete.obs")

ukb_ <- ukb %>% dplyr::select(SWD_1, SWD_2, SWD_3, SWD_4, SWD_5, SWD_6,
                              age, 
                              sex, 
                              ethnicity, 
                              chronotype, 
                              neuroticism_score, 
                              nervousness_scale,
                              sociability_scale, 
                              warmth_scale, 
                              diligence_scale, 
                              curiosity_scale, 
                              alcohol_intake_frequency,
                              coffee_intake,
                              smoking_status, 
                              BMI, 
                              #MET_minutes_week,
                              #f.54.0.0,
                              night_shift_work,
                              time_employed_in_current_job) # UKB Assessment Centre
#dim(na.omit(ukb_)) # complete cases, 90% loss, if complete case is used!
#dim(ukb_[!is.na(SWD_4),]) 

#create_report(ukb[night_shift_work == "Sometimes" | night_shift_work == "Usually" | night_shift_work == "Always",])
#create_report(ukb[night_shift_work == "Usually" | night_shift_work == "Always",])
#create_report(ukb[night_shift_work == "Always",])
#create_report(ukb_[!is.na(SWD_4),])

#__Structure of missingness ----

# Reasons for missingness:
# 1) Different observation times: Person did not enter an online follow-up after initial assessment center 2012.
# 2) Missingness by variable design (e.g., SWD)
# 3) "Prefer not to answer" was given as answer and then defined as missing - this is not MCAR but a nonignorable non-response

sum(is.na(ukb_[!is.na(SWD_4),]))
#??skim(ukb_[!is.na(SWD_4),])
#dplyr::glimpse(ukb_[!is.na(SWD_4),])
vis_dat(ukb_[!is.na(SWD_5),])
vis_miss(ukb_[!is.na(SWD_5),])
ggplot(ukb_[!is.na(SWD_4),], 
       aes(x = MET_minutes_week, 
           y = as.numeric(age))) + 
  geom_miss_point()

vis_miss(ukb[!is.na(SWD_4), c("f.1031.0.0","f.2030.0.0","f.2080.0.0","f.6160.0.0")])

vis_miss(ukb_ %>% filter(age <= 65, 
                         night_shift_work == "Always" | night_shift_work == "Usually",
                         time_employed_in_current_job >= 0), 
         warn_large_data = FALSE
)

# __Cluster analysis of missingness
naplot(naclus((ukb_ %>% filter(age <= 65, 
                               night_shift_work == "Always" | night_shift_work == "Usually",
                               time_employed_in_current_job >= 0))))
plot(naclus((ukb_ %>% filter(age <= 65, 
                             night_shift_work == "Always" | night_shift_work == "Usually",
                             time_employed_in_current_job >= 0))))


# Overview: Night shift work tolerance across different night shift work levels
ukb[!is.na(SWD_1) & night_shift_work!="Prefer_not_to_answer",] %>% 
  droplevels() %>% ggplot() +
  geom_mosaic(aes(x = product(SWD_1, night_shift_work), fill = SWD_1)) +   
  labs(y="", x="Frequency of night shift work", title="Night shift work tolerance (= absence of any insomnia symptoms)") + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  geom_mosaic_text(aes(x = product(SWD_1, night_shift_work), label = after_stat(.wt)), as.label=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))

ukb[!is.na(SWD_4) & night_shift_work!="Prefer_not_to_answer",] %>% 
  droplevels() %>% ggplot() +
  geom_mosaic(aes(x = product(SWD_4, night_shift_work), fill = SWD_4)) +   
  labs(y="", x="Frequency of night shift work", title="Night shift work tolerance (= absence of frequent insomnia symptoms)") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  geom_mosaic_text(aes(x = product(SWD_4, night_shift_work), label = after_stat(.wt)), as.label=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))
# The outcome seems to be rather balanced in both definitions of SWD.

ukb[!is.na(SWD_ord1) & night_shift_work!="Prefer_not_to_answer",] %>% 
  droplevels() %>% ggplot() +
  geom_mosaic(aes(x = product(SWD_ord1, night_shift_work), fill = SWD_ord1)) +   
  labs(y="", x="Frequency of night shift work", title="Night shift work tolerance (ordinal definition 1)") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  geom_mosaic_text(aes(x = product(SWD_ord1, night_shift_work), label = after_stat(.wt)), as.label=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))

ukb[!is.na(SWD_ord2) & night_shift_work!="Prefer_not_to_answer",] %>% 
  droplevels() %>% ggplot() +
  geom_mosaic(aes(x = product(SWD_ord2, night_shift_work), fill = SWD_ord2)) +   
  labs(y="", x="Frequency of night shift work", title="Night shift work tolerance (ordinal definition 2)") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  geom_mosaic_text(aes(x = product(SWD_ord2, night_shift_work), label = after_stat(.wt)), as.label=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))

ukb[!is.na(SWD_ord3) & night_shift_work!="Prefer_not_to_answer",] %>% 
  droplevels() %>% ggplot() +
  geom_mosaic(aes(x = product(SWD_ord3, night_shift_work), fill = SWD_ord3)) +   
  labs(y="", x="Frequency of night shift work", title="Night shift work tolerance (ordinal definition 3)") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  geom_mosaic_text(aes(x = product(SWD_ord3, night_shift_work), label = after_stat(.wt)), as.label=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))

ukb %>% filter(night_shift_work == "Sometimes" | night_shift_work == "Usually" | night_shift_work == "Always") %>% 
  droplevels() %>%
  crosstable(c(night_shift_work, SWD_ord3), by="night_shift_work",funs = mean, test=TRUE, showNA = "no") %>%
  as_flextable()

# Stacked + percent
ukb %>% filter(night_shift_work == "Usually" | night_shift_work == "Always") %>% 
  droplevels() %>%
  ggplot(aes(fill=SWD_ord2, x=night_shift_work)) + 
  geom_bar(position="fill", stat="count")

ukb %>% filter(night_shift_work == "Sometimes" | night_shift_work == "Usually" | night_shift_work == "Always") %>% 
  droplevels() %>%
  ggplot(aes(fill=SWD_ord3, x=night_shift_work)) + 
  geom_bar(position="fill", stat="count")

# BMI-categories in night shift workers vs non-night-shift-workers
ukb$BMI_cat <- wafflecut(ukb$BMI, c('[0,18.4)','[18.4,24.9)','[25,29.9)', '[30,100)'), c('Unterweight','Normal','Overweight','Obese'))
ukb %>%
  droplevels() %>%
  ggplot(aes(fill=BMI_cat, x=night_shift_work)) + 
  geom_bar(position="fill", stat="count")
ukb %>%
  droplevels() %>%
  ggplot(aes(fill=BMI_cat, x=SWD_4)) + 
  geom_bar(position="fill", stat="count")


#__Missingness of big 5 variables
# Do Select and Filter and Exclude first....
ukb_reg %>% dplyr::select(f.1031.0.0, # items for sociability scale (needed for passive imputation)
                          f.2030.0.0,
                          f.2080.0.0,
                          f.6160.0.0, 
                          sociability_scale) %>%
  vis_miss()
# -> The missingness is caused ONLY by the definition of the score itself!

ukb_reg %>% dplyr::select(f.2110.0.0, # items for warmth scale
                          f.1940.0.0,
                          f.1920.0.0,
                          f.1990.0.0,
                          f.1970.0.0, 
                          warmth_scale) %>%
  vis_miss()
# -> same again!

# ____Missingness of score-items
ukb_reg_big5 <- ukb_reg %>% dplyr::select(f.1031.0.0, # items for sociability scale (needed for passive imputation)
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
                                          f.1950.0.0) 
ukb_reg_big5 %>% vis_miss()


# __Models for missingness ---- 

# ___1) missingness of X (explanatory variables) ----
# SWD_5 was both included and excluded as a predictor

# _____Ethnicity ------
# depends on sociability_scale/Alcohol intake frequency
model <- glm(is.na(ethnicity) ~ SWD_5 + age + 
               sex + 
               #ethnicity + 
               chronotype + 
               neuroticism_score + 
               #nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# _____Chronotype (no convergence) ----
model <- glm(is.na(chronotype) ~ SWD_5 + 
               age + 
               sex + 
               ethnicity + 
               #chronotype + 
               neuroticism_score + 
               #nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# _____neuroticism_score (degenerated model) ----
model <- glm(is.na(neuroticism_score) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               #nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# _____nervousness_scale (nothing sign.) ----
model <- glm(is.na(nervousness_scale) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               #nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# _____sociability_scale ----
model <- glm(is.na(sociability_scale) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               #sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)


# _____warmth_scale (nothing sign.) ----
model <- glm(is.na(warmth_scale) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               #warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)


# _____diligence_scale (nothing sign.) ----
model <- glm(is.na(diligence_scale) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               #diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)


# _____coffee_intake (no convergence) ----
model <- glm(is.na(coffee_intake) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# _____coffee_intake (no convergence) ----
model <- glm(is.na(coffee_intake) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)

# smoking_status, BMI have low missingness...


# _____MET_minutes_week ----
# missingness depends on Male/Age/warmth_scale
model <- glm(is.na(MET_minutes_week) ~ SWD_5 + age + 
               sex + 
               ethnicity + 
               chronotype + 
               #neuroticism_score + 
               nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week + 
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = binomial) # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)



# Correlated Variables for odds for missingess of:
# neuroticism_score: genderMale
# sociability_scale: ethnicity, Alcohol intake frequency, Smoking status, as.factor(f.54.0.0), time_employed_in_current_job
# ... models do not fit well...warnings...


# ___2) missingness of Y (outcome(s)) ----
# CAUTION use different filter: remove !is.na(SWD_5)
# Maybe dependent on: MET_minutes_week
model <- glm(is.na(SWD_5) ~ age + 
               sex + 
               ethnicity + 
               chronotype + 
               neuroticism_score + 
               #nervousness_scale +  #(caution: proxy for neuroticism)
               sociability_scale + 
               warmth_scale + 
               diligence_scale + 
               curiosity_scale + 
               alcohol_intake_frequency + #ref-level: "Never"
               smoking_status + 
               BMI + 
               #MET_minutes_week +
               #as.factor(f.54.0.0) + # Assessment Centre, see List_Ass_Centres.xlsx for Coding
               time_employed_in_current_job, # Time employed in main current job
             # f.2724.0.0, ns, Had menopause
             # f.189.0.0, ns, Townsend deprivation index
             # f.22655.0.0, no convergence
             # f.22645.0.0, # Rest days during mixed shift periods
             # f.4581.0.0, ns, Financial situation satisfaction (Extremely happy .... Very unhappy)
             # f.4570.0.0, ns, Friendships satisfaction (Extremely happy .... Very unhappy)
             # f.4559.0.0, ns, Family relationship satisfaction (Extremely happy .... Very unhappy)
             # f.4537.0.0, # ns, Work/job satisfaction (Extremely happy .... Very unhappy)
             # f.4526.0.0, Happiness (Extremely happy .... Very unhappy)
             # f.20126.0.0, (depression status, severely limits sample size!)
             data = ukb_reg, family = "binomial") # if included: & f.22645.0.0 > 0 &  & f.4537.0.0 > 0 ...
summary(model)
#exp(model$coefficients)
tbl_regression(model, exponentiate = TRUE)


# Outcomes over time_employed_in_current_job----
ukb_plot_dat <- ukb_reg
ukb_plot_dat$interval <- cut(ukb_reg$time_employed_in_current_job, 
                             breaks = seq(0, max(ukb_reg$time_employed_in_current_job) + 5, 5), 
                             include.lowest = TRUE)

proportion_data <- ukb_plot_dat %>%
  group_by(interval) %>%
  dplyr::summarise(Count = n(),
                   YesCount = sum(outcome == "Yes"),
                   Proportion = YesCount / Count)

ggplot(proportion_data, aes(x = interval, y = Proportion)) +
  geom_line() +
  geom_point(size=5) +
  labs(x = "Time Employed in Current Job (years)", y = "Proportion of 'Yes'", 
       title = "Proportion of 'Yes' Over Time Employed") +
  theme_minimal()

# Cumulative proportions of outcome:
proportion_data <- ukb_plot_dat %>%
  group_by(interval) %>%
  dplyr::summarise(YesCount = sum(outcome == "Yes"),
                   Count = n()) %>%
  mutate(CumulativeYes = cumsum(YesCount),
         CumulativeCount = cumsum(Count),
         CumulativeProportion = CumulativeYes / CumulativeCount)

# Plot
ggplot(proportion_data, aes(x = interval, y = CumulativeProportion)) +
  geom_line() +
  geom_point() +
  labs(x = "Time Employed in Current Job (years)", y = "Cumulative Proportion of 'Yes'", 
       title = "Cumulative Proportion of 'Yes' Over Time Employed") +
  theme_minimal()

