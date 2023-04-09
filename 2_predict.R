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

#-----------------------------------------------
#-- Import the data

#-- Analysis data set
ad <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/6b. WIC2_ANAL_20210503_CSV.csv")

#-- Utilization 6mo post enrollment
ut <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/utilization_6mo.csv")

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
                        T~0),
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
                        T~any_obs)
  ) 

#-- Continuous
library(Hmisc)
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

ad2 <- ad1[complete.cases(ad1[,c("any_ED","any_inp","any_obs",c(cont,cat))]),]
W1_lim<-as.data.frame(ad2[,c(cont,cat)])
Wmat<- as.matrix(W1_lim)

xgb_ED <- xgboost(data = Wmat, label = ad2$any_ED, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_ED <- xgb.importance(model = xgb_ED)
importance_matrix_ED


xgb_inp <- xgboost(data = Wmat, label = ad2$any_inp, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_inp <- xgb.importance(model = xgb_inp)
importance_matrix_inp


xgb_obs <- xgboost(data = Wmat, label = ad2$any_obs, nrounds = 200, verbose=FALSE,objective = "binary:logistic")
importance_matrix_obs <- xgb.importance(model = xgb_obs)
importance_matrix_obs

