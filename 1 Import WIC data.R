#~~ 1 Import WIC data.R
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

#-----------------------------------------------
#-- Import the data

#-- Analysis data set
ad <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/6b. WIC2_ANAL_20210503_CSV.csv")

#-- Raw BL data
bl <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/WIC2_BL_CURRENT.csv")

#-- Raw FU data
fu <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/WIC2_FU_CURRENT.csv")

#-- Encounter data
en <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/WomenInControl20Grou_DATA_2022-05-16_1711.csv")

#-- Utilization 6mo post enrollment
ut <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/utilization_6mo.csv")

#----------------------------------------------
#-- Import the data dictionary for the raw data sets (bl & fu)

#-- Dictionary for Raw BL data
dict_bl <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/BL data dictionary.csv")

#-- Dictionary for Raw FU data
dict_fu <- read.csv("/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/WIC2Data/FU data dictionary.csv")


#----------------------------------------------
#-- Look at the distributions of baseline variables by study group
#----------------------------------------------

#-- Merge dict_bl with column names from ad

#-- create a data set with variables names from ad
ad_names <- data.frame(names(ad))
names(ad_names) <- "Variable"

common_vars <- merge(dict_bl, ad_names, by="Variable")
ad_vars_only <- subset(ad_names, !(Variable %in% dict_bl$Variable))

f <- common_vars %>%
  filter(Factor==1) %>%
  select(Variable)

#-- Convert categorical / dichotomous variables to factors
varsToFactor <- f$Variable
ad[varsToFactor] <- lapply(ad[varsToFactor], factor)


#-- Select continuous variables

c <- common_vars %>%
  filter(Factor==0) %>%
  select(Variable)

varsCont <- c$Variable

#-- Build table 1 (by wic_randomization)

vars <- c(varsToFactor, varsCont)
tableOne <- CreateTableOne(vars = vars,
                           strata = c("wic_randomization"), 
                           includeNA = TRUE, 
                           data = ad)
tableOnem <- print(tableOne, printToggle = FALSE, noSpaces = TRUE)
write.csv(tableOnem, file = "/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/tab1.csv")
tableOnem <- read.csv(file = "/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/tab1.csv")
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

write.csv(test, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/Prelim_Prediction_Analysis/Results/Table1.csv")


