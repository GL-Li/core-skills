library(lme4)
library(lmerTest)
library(tidyverse)
library(data.table)


# example =====================================================================
# level 1 unit: candidate_id
# level 2 unit: center_id
# level 1 variables: gender, score_paper
# level 2 variables: none
# dependent variable: score_teacher

dat <- fread("mulitlevel-regression/SCI.DAT") %>%
    setnames(c("center_id", "candidate_id", "gender", 
               "score_paper", "score_teacher")) %>%
    # center contineous independent variables
    .[, score_paper := score_paper - mean(score_paper)]

# .. step 1: is multilevel model necessary? ----

# compare two models
m1 <- lm(score_teacher ~ 1, data = dat)
summary(m1)
    # This model simply takes the mean, which is significantly different from 0
    #
    # Coefficients:
    #             Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  79.0346     0.4063   194.5   <2e-16 ***

m2 <- lmer(score_teacher ~ (1 | center_id), data = dat)
summary(m2)
    # Random effects:
    #     Groups    Name        Variance Std.Dev.
    #      center_id (Intercept)  90.48    9.512  
    #      Residual              226.64   15.055  
    # Number of obs: 1905, groups:  center_id, 73
    # 
    # Fixed effects:
    #             Estimate Std. Error     df t value Pr(>|t|)    
    # (Intercept)   79.358      1.206 69.201   65.81   <2e-16 ***

# anova shows significant difference
anova(m2, m1)  # the complicated m2 as the first argument


# .. step 2: add all level 1 variables
m3 <- lmer(score_teacher ~ 1 + gender + score_paper + (1 | center_id),
           data = dat)
summary(m3)

# explained variance
data.frame(VarCorr(m3))


# compare from complicated to simplist
anova(m3, m2, m1)
