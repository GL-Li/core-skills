# https://quantscience.rbind.io/2017/12/26/model-selection-for-multilevel-modeling/

library(tidyverse)
library(haven)
library(lme4)
hsb <- haven::read_sas('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsb.sas7bdat')

# models

# It's generally recommended to use ML instead of REML to get ICs
m0 <- lmer(MATHACH ~ (1 | ID), data = hsb, REML = FALSE)
# You can use the `update` function to add terms
m1 <- update(m0, . ~ . + SES)
# To add random slope, replace the original random intercept term
m2 <- update(m1, . ~ . - (1 | ID) + (SES | ID))
m3 <- update(m2, . ~ . + MEANSES)
m4 <- update(m3, . ~ . + SECTOR)
m5 <- update(m4, . ~ . + SES:SECTOR)
m6 <- update(m5, . ~ . + MEANSES:SECTOR)


# model selection

m_3wayinter <- lmer(MATHACH ~ MINORITY * FEMALE * SES + (1 | ID), 
                    data = hsb, 
                    REML = FALSE)
library(MuMIn)
options(na.action = "na.fail")  # set the missing data handling method
dd <- dredge(m_3wayinter)
model.sel(dd, rank = AIC)
