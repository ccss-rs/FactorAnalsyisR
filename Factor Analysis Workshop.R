# ANS
# CCSS DSF Factor Analysis w/ R Workshop
#todo: select new efa df

# Data Prep ----

## Libraries  ----
# install.packages()
install.packages("lavaanPlot")
install.packages("lavaan")
install.packages("semPlot")
install.packages("semTools")
install.packages("psychTools")
install.packages("tidyverse")
install.packages("corrplot")

library(lavaan)
library(lavaanPlot)
library(semPlot)
library(sem)
library(semTools)
library(psychTools)
library(psych)

library(tidyverse)
library(ggplot2)
library(corrplot)

## Import Raw data ----

### CFA data ----
data(bfi)
describe(bfi)
dim(bfi)

### EFA data ----
efa_data = read.csv(file="https://quantdev.ssri.psu.edu/sites/qdev/files/dataBIG5.csv", header=TRUE)


## Example & Vocab w/ SES & Risk Model  ---- 
#vocab: latent variable, observed/manifest variable, 

#SEM Code format ----
#myModel <- ' # regressions
             #y1 + y2 ~ f1 + f2 + x1 + x2
             #     f1 ~ f2 + f3
             #     f2 ~ f3 + x1 + x2

             # latent variable definitions 
             #  f1 =~ y1 + y2 + y3 
             #  f2 =~ y4 + y5 + y6 
             #  f3 =~ y7 + y8 + y9 + y10

             # variances and covariances 
             #  y1 ~~ y1 
             #  y1 ~~ y2 
             #  f1 ~~ f2

             # intercepts 
             #  y1 ~ 1 
             #  f1 ~ 1

Descriptive_Library <- read_csv("final_scores_2021_3-22.csv")

Full_Set <- select(Descriptive_Library, "BART_adj_total_pumps", "BART_points", "BART_balloons_popped","BIS_score", 
                   "BAS_score", "reward_sensitivity", "punishment_sensitivity", "AK_insight_plan_control", 
                   "Radimer_home_food_insecure", "Radimer_cornell_food_insecure", "FIS_food_insecurity_score", 
                   "FIS_food_insecurity_status", "childhood_residential_moves", "Demographics_Age", "Demographics_Race", "Demographics_Year", 
                   "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", 
                   "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", 
                   "Demographics_Live") %>%
  na.omit  %>%
  dplyr::mutate(BART_adj_total_pumps  = scale(BART_adj_total_pumps))


risk_tolerance_CFA <- '
# latent variable definitions 
Risk_Taking =~ reward_sensitivity + BAS_score  + BART_adj_total_pumps
SES =~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + Demographics_Race 
FI =~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure 

# regressions
Risk_Taking ~ FI + SES
SES~FI

#residual correlations
'

risktolerance <- lavaan::sem(model= risk_tolerance_CFA, 
                     data = Full_Set)
summary(risktolerance, standardized=TRUE, fit.measures = TRUE)

semPaths(risktolerance, what="std", whatLabels="std", layout = "tree2",nCharNodes=5,  edge.width = 1,
         ThreshAtSide=TRUE, sizeMan = 6) 

# EFA ----

# removing first 7 columns bc not part of item pool                                            
efa_data <- efa_data[ ,8:57]
# replacing 0 with NA
efa_data[efa_data == 0] <- NA
#standardize data
efa_data_standard <- data.frame(scale(efa_data, center=TRUE, scale=TRUE))

## Step 1: Determine which vars are highly correlated and potential number of factors ----
# cor matrix
efa_data_standard %>%
  cor() %>%
corrplot(method = 'number')

## Step 2: Determine number of factors  ----
scree(efa_data_standard, pc=FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(efa_data_standard, fa="fa") # another way of viz above

## Step 3: Extract (and rotate) factors ----
# assumption of uncorrelated (independent) factors
# options: promax, oblimin
# assumption of correlated (non-independent) factors
# options: varimax(), quartimax, equamax

### Maximum Likelihood Factor Analysis with no rotation ----
fit_no_rotate <- factanal(~ .,data=efa_data_standard, factors = 10, rotation="none",na.action = na.exclude)
print(fit_no_rotate, digits=2, cutoff=0.3, sort=TRUE)

###  Maximum Likelihood Factor Analysis with varimax rotation ----               
# orthogonally rotates the factor axes with the goal of maximizing the variance of the squared loadings of a factor on all the variables
fit_varimax_rotate <- factanal(~ .,data=efa_data_standard, factors = 10, rotation="varimax",na.action = na.exclude)
print(fit_varimax_rotate, digits=2, cutoff=0.3, sort=TRUE)

###  Maximum Likelihood Factor Analysis with promax rotation ----               -> 
# oblique transformation, assumes factors are correlated
fit_promax_rotate <- factanal(~ .,data=efa_data_standard, factors = 10, rotation="promax",na.action = na.exclude)
print(fit_promax_rotate, digits=2, cutoff=0.3, sort=TRUE)

## Step 4: Visualize  ----
loads <- fit_promax_rotate$loadings
fa.diagram(loads)


# Big 5: CFA/SEM - Fit a CFA model using the lavaan package in R  ----
describe(bfi)
## Step 1: create the bfi.keys ----
keys <-
  list(
    agree=c("-A1","A2","A3","A4","A5"),
    conscientious=c("C1","C2","C3","-C4","-C5"),
    extraversion=c("-E1","-E2","E3","E4","E5"),
    neuroticism=c("N1","N2","N3","N4","N5"),
    openness = c("O1","-O2","O3","O4","-O5")
    ) 

scores <- psych::scoreItems(keys,bfi,min=1,max=6) #specify the minimum and maximum values
scores

## Setup Models ----
### one factor ----
one_factor_cfamodel <- 'agree =~ A1 + A2 + A3 + A4 + A5'
one_factor_cfamodel_fit <- lavaan::cfa(one_factor_cfamodel, data = bfi)
summary(one_factor_cfamodel_fit, fit.measures = TRUE)
semPaths(one_factor_cfamodel_fit)

### two factors  ----
two_factor_cfamodel <- '
agree  =~ A1 + A2 + A3 + A4 + A5
conscientious   =~ C1 + C2 + C3 + C4 + C5
'
two_factor_cfamodel_fit <- lavaan::cfa(two_factor_cfamodel, data = bfi)
summary(two_factor_cfamodel_fit, fit.measures = TRUE)
semPaths(two_factor_cfamodel_fit)

### all factors  ----
full_cfa_model <- '
 # measurement model
agree  =~ A1 + A2 + A3 + A4 + A5
conscientious   =~ C1 + C2 + C3 + C4 + C5
extraversion   =~ E1 + E2 + E3 + E4 + E5
neuroticism   =~ N1 + N2 + N3 + N4 + N5
openness      =~ O1 + O2 + O3 + O4 + O5
' 

full_cfa_model_fit <- lavaan::cfa(full_cfa_model, bfi)
summary(full_cfa_model_fit, fit.measures = TRUE)
semPaths(full_cfa_model_fit)

## Step 3: Visualize and Interpretation ----
summary(one_factor_cfamodel_fit, fit.measures = TRUE)
semPaths(one_factor_cfamodel_fit,title=FALSE, "std",edge.label.cex=0.5)

summary(two_factor_cfamodel_fit, fit.measures = TRUE)
semPaths(two_factor_cfamodel_fit,title=FALSE, "std",edge.label.cex=0.5)

summary(full_cfa_model_fit, fit.measures = TRUE)
semPaths(full_cfa_model_fit,title=FALSE, "std",edge.label.cex=0.5)
