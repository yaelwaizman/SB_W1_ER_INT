#########################################################################################################################################################
# Effects of Emotion Regulation Strategy Usage on Internalizing Symptoms Following Early Institutional Care
# Script written by Yael H. Waizman to process, score, and analyze data used in this manuscript
# Note: H4 and Q1 are not listed below as they were tested using PROCESS macro (version 3.5) in SPSS 
#########################################################################################################################################################

# remove everything from the environment
rm(list = ls())

# load packages
library(readr)
library(expss)
library(janitor)
library(dplyr)
library(psych)

# set project path
project_path <- file.path("~","Desktop", "ER_INT_project")
# read the all_ages+caregiver child clean questionnaire data file
SB_all_scored <- read_csv(file.path(project_path,"SB_AllAges+CaregiverChild_Questionnaires_CLEANMasterCopy.csv"))
SB_all_scored <- as.data.frame(SB_all_scored) #set as a dataframe
# read the CBCL Tscores
CBCL_t_score_table <- read_csv(file.path(project_path, "CBCL_Scoring.csv"))

### Import custom functions file 
# function that will score the ERQ
source(file.path(project_path, "ERQ_scoring.R"))
# function that will score the CBCL subscale t scores
source(file.path(project_path, "CBCL_ASR_subscale_tscores_funcs.R"))
# function that will actually score the CBCL questionnaire
source(file.path(project_path, "CBCL_scoring_SB.R"))

####################################################  
# SCORING DATA
####################################################  

## Emotion Regulation Questionnaire for Children and Adolescents Version administered in W1 (ERQ-CA)
# Uses function to score Emotion Regulation for Children and Adolescents questionnaire
SB_all_scored <- ERQ(SB_all_scored)

## Child Behavior Checklist for Ages 6-18 (CBCL/6-18) 
# Uses function to score CBCL questionnaire
SB_all_scored <- CBCL_scoring_SB(SB_all_scored,CBCL_t_score_table)

####################################################  
# TEST SUBSCALE RELIABILITY
#################################################### 
# Cognitive Reappraisal subscale from the ERQ-CA was reliable α = .82
alpha(SB_all_scored[,paste("ERQ",c(1,3,5,7,8,10),sep="")])
# Expressive Suppression subscalef rom the ERQ-CA was reliable α = .76
alpha(SB_all_scored[,paste("ERQ",c(2,4,6,9),sep="")])
# Anxious/Depressed subscale from the CBCL/6-18 was reliable α = .84
alpha(SB_all_scored[,paste('CBCL_',c(14,29,30,31,32,33,35,45,50,52,71,91,112),sep="")])
# Withdrawn/Depressed subscale from the CBCL/6-18 was reliable α = .75
alpha(SB_all_scored[,paste('CBCL_',c(5,42,65,69,75,102,103,111),sep="")])
# Somatic Complaints subscale from the CBCL/6-18 was reliable α = .76
alpha(SB_all_scored[,paste('CBCL_',c(47,49,51,54,'56_A','56_B','56_C','56_D','56_E','56_F','56_G'),sep="")])

####################################################  
# SET UP DATAFRAME FOR ANALYSES 
####################################################  

# select the data that will be used in analyses
data <- select(SB_all_scored, SUB_ID, Age, Group, Gender, Ethnicity, Inst_dur, Parental_education, ERQ_Supp_TOTAL, ERQ_Reapp_TOTAL, CBCL_INT_TSCORE)

# code Males as 0 and Females as 1
data$Gender[which(data$Gender == "M")] <- 0
data$Gender[which(data$Gender == "F")] <- 1

# create mean-centered Expressive Suppression scores across entire sample
data$Supp_mc <- data$ERQ_Supp_TOTAL - mean(data$ERQ_Supp_TOTAL)

# save the scored data
write.csv(data, file.path(project_path,"scored_ER_INT_data.csv"), row.names=FALSE) 

# create new dataframe with only the comparison youth's data 
comparisons <- filter(data, Group == 0) 

# create new dataframe with only the PI youth's data
PIs <- filter(data, Group == 1)

####################################################  
# ANALYSES
####################################################  

# H1: PI status will predict internalizing symptoms. 
H1 <- lm(CBCL_INT_TSCORE ~ Group + Gender, data)
summary(H1)

# H2: Expressive suppression usage will be positively associated with internalizing symptoms across both PI and comparison groups.
cor.test(PIs$ERQ_Supp_TOTAL, PIs$CBCL_INT_TSCORE) #PI group only
cor.test(comparisons$ERQ_Supp_TOTAL, comparisons$CBCL_INT_TSCORE) #Comparison group only
cor.test(data$ERQ_Supp_TOTAL, data$CBCL_INT_TSCORE) #across entire sample

# H3: Cognitive reappraisal usage will be negatively associated with internalizing symptoms across both the PI and comparison groups.
cor.test(PIs$ERQ_Reapp_TOTAL, PIs$CBCL_INT_TSCORE) #PI group only
cor.test(comparisons$ERQ_Reapp_TOTAL, comparisons$CBCL_INT_TSCORE) #Comparison group only
cor.test(data$ERQ_Reapp_TOTAL, data$CBCL_INT_TSCORE) #across entire sample

##### Exploratory Analyses

# Q2: Is age associated with the usage of an emotion regulation strategy? If so, does this relationship differ for PI versus comparison youth? 
cor.test(PIs$ERQ_Supp_TOTAL, PIs$Age) #PI group only
cor.test(comparisons$ERQ_Supp_TOTAL, comparisons$Age) #Comparison group only
cor.test(data$ERQ_Supp_TOTAL, data$Age) #across entire sample

# Q3: Is the duration of institutional care associated with the usage of an emotion regulation strategy or internalizing symptoms?
cor.test(PIs$ERQ_Supp_TOTAL, PIs$Inst_dur) #Expressive Suppression and Institutionalization Duration
cor.test(PIs$CBCL_INT_TSCORE, PIs$Inst_dur) #Internalizing Symptoms and Institutionalization Duration

####################################################  
# Supplemental Material Table S1
####################################################  

t.test(PIs$Age, comparisons$Age) # Group differences for Age
t.test(PIs$ERQ_Supp_TOTAL, comparisons$ERQ_Supp_TOTAL) # Group differences for Expressive Suppression
t.test(PIs$ERQ_Reapp_TOTAL, comparisons$ERQ_Reapp_TOTAL) # Group differences for Cognitive Reappraisal
t.test(PIs$CBCL_INT_TSCORE, comparisons$CBCL_INT_TSCORE) # Group differences for Internalizing Symptoms
describe(PIs) # Descriptive Statistics for PI youth
describe(comparisons) # Descriptive Statistics for Comparison youth

##### Gender count per group
PI_female_n <- length(which(PIs$Gender == 1)) # number of female participants in PI group 
PI_female_percent <- (PI_female_n/length(PIs$Gender))*100 # percent of female participants in PI group 
comparisons_female_n <- length(which(comparisons$Gender == 1)) # number of female participants in Comparison group 
comparisons_female_percent <- (comparisons_female_n/length(comparisons$Gender))*100 # percent of female participants in Comparison group

##### Ethnicity count per group

# African America/Black = 1
AAB_PIs_n <- length(which(PIs$Ethnicity == 1)) # number of African America/Black participants in PI group
AAB_PIs_percent <- (AAB_PIs_n/length(PIs$Ethnicity))*100 # percent of African America/Black participants in PI group
AAB_comparisons_n <- length(which(comparisons$Ethnicity == 1)) # number of African America/Black participants in Comparison group
AAB_comparisons_percent <- (AAB_comparisons_n/length(comparisons$Ethnicity))*100 # percent of African America/Black participants in Comparison group

# Asian/Asian-American = 3
AAA_PIs_n <- length(which(PIs$Ethnicity == 3)) # number of Asian/Asian-American participants in PI group
AAA_PIs_percent <- (AAA_PIs_n/length(PIs$Ethnicity))*100 # percent of Asian/Asian-American participants in PI group
AAA_comparisons_n <- length(which(comparisons$Ethnicity == 3)) # number of Asian/Asian-American participants in Comparison group
AAA_comparisons_percent <- (AAA_comparisons_n/length(comparisons$Ethnicity))*100 # percent of Asian/Asian-American participants in Comparison group

# European-American/Caucasian = 5 
EAC_PIs_n <- length(which(PIs$Ethnicity == 5)) # number of European-American/Caucasian participants in PI group
EAC_PIs_percent <- (EAC_PIs_n/length(PIs$Ethnicity))*100 # percent of European-American/Caucasian participants in PI group
EAC_comparisons_n <- length(which(comparisons$Ethnicity == 5)) # number of European-American/Caucasian participants in Comparison group
EAC_comparisons_percent <- (EAC_comparisons_n/length(comparisons$Ethnicity))*100 # percent of European-American/Caucasian participants in Comparison group

# Hispanic/Latino = 6
HL_PIs_n <- length(which(PIs$Ethnicity == 6)) # number of Hispanic/Latino participants in PI group
HL_PIs_percent <- (HL_PIs_n/length(PIs$Ethnicity))*100 # percent of Hispanic/Latino participants in PI group
HL_comparisons_n <- length(which(comparisons$Ethnicity == 6)) # number of Hispanic/Latino participants in Comparison group
HL_comparisons_percent <- (HL_comparisons_n/length(comparisons$Ethnicity))*100 # percent of Hispanic/Latino participants in Comparison group

##### Parental Education count per group

# High school degree = 2
HSD_PIs_n <- length(which(PIs$Parental_education == 2)) # number of PI participants whose parents parental education level is High school degree
HSD_PIs_percent <- (HSD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is High school degree
HSD_comparisons_n <- length(which(comparisons$Parental_education == 2)) # number of comparison participants whose parents parental education level is High school degree
HSD_comparisons_percent <- (HSD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is High school degree

# Some College = 3
SC_PIs_n <- length(which(PIs$Parental_education == 3)) # number of PI participants whose parents parental education level is Some College
SC_PIs_percent <- (SC_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Some College
SC_comparisons_n <- length(which(comparisons$Parental_education == 3)) # number of comparison participants whose parents parental education level is Some College
SC_comparisons_percent <- (SC_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Some College

# Community college/two-year degree = 4
CCTYD_PIs_n <- length(which(PIs$Parental_education == 4)) # number of PI participants whose parents parental education level is Community college/two-year degree
CCTYD_PIs_percent <- (CCTYD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Community college/two-year degree
CCTYD_comparisons_n <- length(which(comparisons$Parental_education == 4)) # number of comparison participants whose parents parental education level is Community college/two-year degree
CCTYD_comparisons_percent <- (CCTYD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Community college/two-year degree

# Four-year college degree = 5
FYCD_PIs_n <- length(which(PIs$Parental_education == 5)) # number of PI participants whose parents parental education level is Four-year college degree
FYCD_PIs_percent <- (FYCD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Four-year college degree
FYCD_comparisons_n <- length(which(comparisons$Parental_education == 5)) # number of comparison participants whose parents parental education level is Four-year college degree
FYCD_comparisons_percent <- (FYCD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Four-year college degree

# Some graduate school = 6
SGS_PIs_n <- length(which(PIs$Parental_education == 6)) # number of PI participants whose parents parental education level is Some graduate school
SGS_PIs_percent <- (SGS_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Some graduate school
SGS_comparisons_n <- length(which(comparisons$Parental_education == 6)) # number of comparison participants whose parents parental education level is Some graduate school
SGS_comparisons_percent <- (SGS_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Some graduate school

# Master's degree = 7 
MD_PIs_n <- length(which(PIs$Parental_education == 7)) # number of PI participants whose parents parental education level is Master's degree
MD_PIs_percent <- (MD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Master's degree
MD_comparisons_n <- length(which(comparisons$Parental_education == 7)) # number of comparison participants whose parents parental education level is Master's degree
MD_comparisons_percent <- (MD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Master's degree

# Doctoral degree = 8 
DD_PIs_n <- length(which(PIs$Parental_education == 8)) # number of PI participants whose parents parental education level is Doctoral degree
DD_PIs_percent <- (DD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Doctoral degree
DD_comparisons_n <- length(which(comparisons$Parental_education == 8)) # number of comparison participants whose parents parental education level is Doctoral degree
DD_comparisons_percent <- (DD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Doctoral degree

# Professional degree = 9
PD_PIs_n <- length(which(PIs$Parental_education == 9)) # number of PI participants whose parents parental education level is Professional degree
PD_PIs_percent <- (PD_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is Professional degree
PD_comparisons_n <- length(which(comparisons$Parental_education == 9)) # number of comparison participants whose parents parental education level is Professional degree
PD_comparisons_percent <- (PD_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is Professional degree

# No parental education listed = NA
NPEL_PIs_n <- length(which(is.na(PIs$Parental_education))) # number of PI participants whose parents parental education level is No parental education listed
NPEL_PIs_percent <- (NPEL_PIs_n/length(PIs$Parental_education))*100 # percent of PI participants whose parents parental education level is No parental education listed
NPEL_comparisons_n <- length(which(is.na(comparisons$Parental_education))) # number of comparison participants whose parents parental education level is No parental education listed
NPEL_comparisons_percent <- (NPEL_comparisons_n/length(comparisons$Parental_education))*100 # percent of comparison participants whose parents parental education level is No parental education listed

