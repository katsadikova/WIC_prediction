#~~ 2_predict.R
#------------------------------------------------

library(tidyverse)
library(kableExtra)
library(gtsummary)
library(expss)
library(haven)
library(sjlabelled)
library(readxl)
library(gtools)
library(tableone)
library(mice)
library(corrplot)
library(reshape2)
library(Hmisc)
library(xgboost)
library(mice)
library(finalfit)

#-----------------------------------------------
#-- Import the data

#-- Analysis data set
ad <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/6b. WIC2_ANAL_20210503_CSV.csv")

#-- Utilization 6mo post enrollment
ut <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/utilization_6mo.csv")

#----------------------------------------------
#-- Import the data dictionary for the raw data sets (bl & fu)

#-- Dictionary for Raw BL data
dict_bl <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/BL data dictionary.csv")

#-- Dictionary for Raw FU data
dict_fu <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/FU data dictionary.csv")

#-----------------------------------------------

summary(as.factor(ut$VISIT_TYPE))

ut_wide <- ut %>%
  mutate(
    VISIT_TYPE = case_when(VISIT_TYPE=="Emergency (ED)" ~ "ED",
                           VISIT_TYPE=="Inpatient" ~ "Inp",
                           VISIT_TYPE=="Observation" ~ "Obs")
  ) %>%
  group_by(ID,VISIT_TYPE) %>%
  mutate(
    visit_count = row_number()
  ) %>% 
  ungroup() %>%
  pivot_wider(names_from = c(VISIT_TYPE,visit_count), values_from = SERVICE_DT) %>%
  mutate(
    any_ED = case_when(!is.na(ED_1)~1,
                       T~0),
    any_inp = case_when(!is.na(Inp_1)~1,
                        T~0),
    any_obs = case_when(!is.na(Obs_1)~1,
                        T~0)
  ) %>%
  select(ID,ARM,INDEX_DT,WITHDREW,SIX_MONTHS,any_ED,any_inp,any_obs,ED_1,ED_2,ED_3,ED_4,ED_5,ED_6,ED_7,Obs_1,Obs_2,
         Inp_1,Inp_2,Inp_3,Inp_4)

names(ut_wide)[1] <- "wic_id" 

ad1 <- merge(ad,ut_wide,by="wic_id",all=TRUE) %>%
  mutate(
    any_ED = case_when(is.na(ED_1)~0,
                       T~any_ED),
    any_inp = case_when(is.na(Inp_1)~0,
                        T~any_inp),
    any_obs = case_when(is.na(Obs_1)~0,
                        T~any_obs),
    any_util = case_when(any_ED==1 | any_inp==1 | any_obs==1 ~ 1,
                         T ~ 0)
  ) 

#-- Continuous
cont <- Cs(A1C, PAM13_tot, PAM13_act, PAM13_cat, PHQ8,
PSS10, MOS_emot, MOS_tang, MOS_aff, MOS_pos, MOS_ss, PTD, PTD_caring,
PTD_character, PTD_competence, PTD_confidence, PTD_connection,
PTD_contribution, DDS_total, DDS_emotburd, DDS_physician, DDS_regimen, DDS_interpers,
PROMIS_pf, PROMIS_anx, PROMIS_dep, PROMIS_fat, PROMIS_slp, PROMIS_sat, PROMIS_pain,
BlockFat17, BlockFat16, BlockVeg10, BlockVeg9)

#-- Discrete / factor
cat <- Cs(A1C9, race8,insure11,educ13,work11,
finances5,income14, marital8,living8,numchild7,
headhouse3,hlitconf7,pcpnum6,specnum6,hospnum6,
ernum6,PHQ8_cat,sds_work,sds_social,sds_famlife,
sds_any,lacktime,socinf,lackenergy,lackwp,fearinj,lackskill,lackres)

#-- Summarize the predictors
#-- Convert categorical / dichotomous variables to factors
ad1[cat] <- lapply(ad[cat], factor)
vars <- c(cat, cont)
tableOne <- CreateTableOne(vars = vars,
                           strata = c("wic_randomization"), 
                           includeNA = TRUE, 
                           data = ad1)
tableOnem <- print(tableOne, printToggle = FALSE, noSpaces = TRUE)
write.csv(tableOnem, file = "/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/tab1_small.csv")
tableOnem <- read.csv(file = "/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/tab1_small.csv")
tableOnem$rowNum <- seq.int(nrow(tableOnem))
#-- Have to save as csv then read in the csv in order to preserve the rownames without the X... nonsense

#-- Column labels
names(tableOnem) <- c("Var", "Face_to_face", "Virtual", "p_value", "test", "rowNum")

#-- Row labels - merge Table 1 with dict_bl to get the variable names
tableOnem$Variable <- gsub("\\(.*", "", tableOnem$Var)
tableOnem$Variable <- gsub("\\ .*", "", tableOnem$Variable)
tableOnem_labeled <- merge(tableOnem, dict_bl, by.x="Variable", all.x=T) 
tableOnem_labeled <- tableOnem_labeled[order(tableOnem_labeled$rowNum),]

test <-
  tableOnem_labeled %>%
  mutate(
    Characteristic = case_when(!is.na(Label) ~ Label,
                               TRUE ~ Var)) %>%
  select(Characteristic, Face_to_face, Virtual, p_value, test, Variable)
test

kbl(test) %>% 
  kable_styling()

write.csv(test, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/Table1_small.csv")

#-- Look at pattern of missing data
ad2 <- data.frame(ad1[,c("any_util","any_ED","any_inp","any_obs",c(cont,cat))])
ad2_missing <- missing_pattern(ad2)

#-- Drop variables with the most missingness (BlockFat17,BlockVeg10, sds_work)
#-- Continuous
cont <- Cs(A1C, PAM13_tot, PAM13_act, PAM13_cat, PHQ8,
           PSS10, MOS_emot, MOS_tang, MOS_aff, MOS_pos, MOS_ss, PTD, PTD_caring,
           PTD_character, PTD_competence, PTD_confidence, PTD_connection,
           PTD_contribution, DDS_total, DDS_emotburd, DDS_physician, DDS_regimen, DDS_interpers,
           PROMIS_pf, PROMIS_anx, PROMIS_dep, PROMIS_fat, PROMIS_slp, PROMIS_sat, PROMIS_pain,
            BlockFat16, BlockVeg9)

#-- Discrete / factor
cat <- Cs(A1C9, race8,insure11,educ13,work11,
          finances5,income14, marital8,living8,numchild7,
          headhouse3,hlitconf7,pcpnum6,specnum6,hospnum6,
          ernum6,PHQ8_cat,sds_social,sds_famlife,
          sds_any,lacktime,socinf,lackenergy,lackwp,fearinj,lackskill,lackres)


#-- Look at pattern of missing data
ad2 <- data.frame(ad1[,c("wic_randomization","any_util","any_ED","any_inp","any_obs",c(cont,cat))])
ad2_missing <- missing_pattern(ad2)

#-- Look at variable importance
ad3 <- na.omit(ad2) %>% #268 when sds_work, veg, fat variables dropped (84 if these are included)
  filter(wic_randomization==1)
#-- Need to reconvert factor vars back to numeric format
ad3[cat] <- lapply(ad3[cat], as.numeric)
W1_lim<-data.frame(ad3[,c("wic_randomization",cont,cat)])
summary(W1_lim)
Wmat<-as.matrix(W1_lim)

xgb_util <- xgboost(data = Wmat, label = ad3$any_util, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_util <- xgb.importance(model = xgb_util)
importance_matrix_util
## Create table comparing distributions of top 10 important variables by any_util
vars <- as.vector(unlist(importance_matrix_util[1:10,1]))
table_util_imp <- CreateTableOne(vars = vars,
                                 strata = c("any_util"), 
                                 includeNA = F, 
                                 addOverall = F,
                                 data = ad3)
table_util_imp <- data.frame(print(table_util_imp, missing=F))
table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:4)]
table_util_imp
write.csv(table_util_imp, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/table_util_imp_SL.csv")


xgb_ED <- xgboost(data = Wmat, label = ad3$any_ED, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_ED <- xgb.importance(model = xgb_ED)
importance_matrix_ED
## Create table comparing distributions of top 10 important variables by any_ED
vars <- as.vector(unlist(importance_matrix_ED[1:10,1]))
table_ED_imp <- CreateTableOne(vars = vars,
                          strata = c("any_ED"), 
                          includeNA = F, 
                          addOverall = F,
                          data = ad3)
table_ED_imp <- data.frame(print(table_ED_imp, missing=F))
table_ED_imp <- tibble::rownames_to_column(table_ED_imp, "Characteristic")[,c(1:4)]
table_ED_imp
write.csv(table_ED_imp, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/table_ED_imp_SL.csv")

xgb_inp <- xgboost(data = Wmat, label = ad3$any_inp, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_inp <- xgb.importance(model = xgb_inp)
importance_matrix_inp
## Create table comparing distributions of top 10 important variables by any_inp
vars <- as.vector(unlist(importance_matrix_inp[1:10,1]))
table_inp_imp <- CreateTableOne(vars = vars,
                               strata = c("any_inp"), 
                               includeNA = F, 
                               addOverall = F,
                               data = ad3)
table_inp_imp <- data.frame(print(table_inp_imp, missing=F))
table_inp_imp <- tibble::rownames_to_column(table_inp_imp, "Characteristic")[,c(1:4)]
table_inp_imp
write.csv(table_inp_imp, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/table_inp_imp_SL.csv")

xgb_obs <- xgboost(data = Wmat, label = ad3$any_obs, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_obs <- xgb.importance(model = xgb_obs)
importance_matrix_obs
## Create table comparing distributions of top 10 important variables by any_inp
vars <- as.vector(unlist(importance_matrix_obs[1:10,1]))
table_obs_imp <- CreateTableOne(vars = vars,
                                strata = c("any_obs"), 
                                includeNA = F, 
                                addOverall = F,
                                data = ad3)
table_obs_imp <- data.frame(print(table_obs_imp, missing=F))
table_obs_imp <- tibble::rownames_to_column(table_obs_imp, "Characteristic")[,c(1:4)]
table_obs_imp
write.csv(table_obs_imp, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/table_obs_imp_SL.csv")


