##############################
###### ANALYTIC DATASET ######
########### EPI 204 ##########
##############################

# Date last edited: 4 May 2026
# Author: Parker Tope

## Dependencies
library(dplyr)

## Load raw data
nhanes <- read.csv("/Users/parkertope/Desktop/EPI 204/HW/FINAL PROJECT/NHANES2.csv")

## Data Structure
### confirming one row per person
length(unique(nhanes$SEQNO)) == nrow(nhanes)

# Selecting variables of interest:

nhanes <- nhanes |>
  select(
    # admin vars
    c(SEQNO, AGEYRS, AGEDIE, LAST_YR, BORN_YR, DEATH, DIE_YR,
    # exposure
      NIACIN,
    # chronic conditions/comorbidities
      ANGINA, ARTHRITI, ASPIRIN , EVPREG, HTN_REP, STROKE, 
    # sociodempgraphic variables
      MARRY, RACE, SEX, URBAN, SCHOOL,
    # lifestyle variables
      BOOZE, RECEX, AVGSMK, HEIGHT, WT)
  )

### Missingness

# Niacin Quantiles

niacin_qval <- nhanes %>%
  filter(!is.na(NIACIN)) %>%
  summarise(p25 = quantile(NIACIN, probs = 0.25),
            p50 = quantile(NIACIN, probs = 0.50),
            p75 = quantile(NIACIN, probs = 0.75))

nhanes$niacin_q <- ifelse(nhanes$NIACIN < niacin_qval$p25, 1, 
                         ifelse(nhanes$NIACIN >= niacin_qval$p25 & nhanes$NIACIN < niacin_qval$p50, 2, 
                                ifelse(nhanes$NIACIN >= niacin_qval$p50 & nhanes$NIACIN < niacin_qval$p75, 3, 
                                       ifelse(nhanes$NIACIN > niacin_qval$p75, 4, NA))))
table(nhanes$niacin_q)

# Variable Operationalization
## BMI, person-time for Poisson analysis
nhanes <- nhanes |>
  mutate(
    # using age as the time scale
    # time zero is age at entry
    age_tstart = AGEYRS,
    # end of follow up is censoring or death
    age_tend = LAST_YR - BORN_YR,
    pt = age_tend - age_tstart,
    bmi = WT/(HEIGHT)^2
  )

# Checking something funky going on with time variables
table(!is.na(nhanes$BORN_YR))
table(!is.na(nhanes$LAST_YR))

nhanes_check <- nhanes |>
  filter(is.na(BORN_YR),
         is.na(LAST_YR))

nhanes_model <- nhanes |>
  filter(
    is.na(pt),
    pt > 0
  )

