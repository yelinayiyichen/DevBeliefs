#authors: Yelina Yiyi Chen, Tianxiang Li, John Flournoy, Patrick Mair
if(!require("ggplot2")){install.packages("ggplot2");require("ggplot2")}
if(!require("mgcv")){install.packages("mgcv");require("mgcv")}
if(!require("dplyr")) {install.packages("dplyr");require("dplyr")} 
if(!require('parameters')) {install.packages('parameters');require('parameter')}
if(!require('tidyverse')) {install.packages('tidyverse');require('tidyverse')}
if(!require('sjPlot')) {install.packages('sjPlot');require('sjPlot')}
if(!require('effects')){install.packages("effects");require('effects')}
if(!require('visreg')){install.packages("visreg");require('visreg')}
if(!require('gratia')){install.packages('gratia');require('gratia')}
if(!require('itsadug')){install.packages('itsadug');require('itsadug')}
if(!require('ggeffects')){install.packages('ggeffects');require('ggeffects')}
if(!require('emmeans')){install.packages('emmeans');require('emmeans')}
if(!require('svglite')){install.packages('svglite');require('svglite')}
if(!require('fitdistrplus')){install.packages('fitdistrplus');require('fitdistrplus')}
if(!require('graphics')){install.packages('graphics');require('graphics')}

# using svglite as the plotting function (format is vectorized image)

PriskyCol <- "#345995"
PriskyCol2<- "#1C97E7"#6C456A"
totalEarnCol <- "#115B37"
totalEarnCol2<- "#63BBA6"#439a86"
AlphaCol <- "#a4303f" 
AlphaCol2<-"#F73F25"#F39C12"
WeightCol <- "mediumorchid4"
weightCol2 <- "darkorchid3"

beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("primary_analysis.rdata")


#==========================================================================================#
####                 I. Accuracy of Friend Predicted Risk Seeking                       ####
#==========================================================================================#

#####                     Alpha                        #####
d_Baseline_FP_Alpha %>% group_by(Condition) %>% summarise(Mean = mean(Alpha))

m_FP_baseline_FP_Alpha <- gam(Alpha ~
                                s(c_age, bs="tp", by = Condition) +
                                Condition +
                                s(SID, bs = "re"),
                              method = 'REML', 
                              data = d_Baseline_FP_Alpha)
summary(m_FP_baseline_FP_Alpha)

add_minAge_Baseline_FP_Alpha <- function(a) {
  a=a+min(d_Baseline_FP_Alpha$age)
}

set.seed(123)
plot_diff(m_FP_baseline_FP_Alpha, view = "c_age", sim.ci = TRUE, 
          comp = list(Condition = c("self_FP", "frd_baseline")),
          xlab = "Age", 
          ylab = "Difference in Risk Seeking (Alpha)", main = "",
          transform.view = add_minAge_Baseline_FP_Alpha, col = AlphaCol, 
          hide.label = T, par(bg = "transparent"))

#####                     Prisky                       #####
d_Baseline_FP_Prisky %>% group_by(Condition) %>% summarise(Mean = mean(Prisky))

m_FP_baseline_FP_Prisky_sqz <- gam(Prisky_sqz ~
                                     s(c_age, bs="tp", by = Condition) +
                                     Condition +
                                     s(SID, bs = "re"),
                                   method = 'REML', 
                                   family = betar,
                                   data = d_Baseline_FP_Prisky)
summary(m_FP_baseline_FP_Prisky_sqz)

add_minAge_Baseline_FP_Prisky <- function(a) {
  a=a+min(d_Baseline_FP_Prisky$age)
}

set.seed(123)
plot_diff(m_FP_baseline_FP_Prisky_sqz, view = "c_age", sim.ci = TRUE, 
          comp = list(Condition = c("self_FP", "frd_baseline")),
          xlab = "Age", 
          ylab = "Difference in Proportion of Risky Choices", main = "",
          transform.view = add_minAge_Baseline_FP_Prisky, col = PriskyCol, 
          hide.label = T, par(bg = "transparent"))


#==========================================================================================#
####            II. Perspective taking: Desired Opposite vs. Opposite                   ####
#==========================================================================================#

#####       intercept model for overall average        #####
emp_intcp<-gam(emp ~ 1, method = "REML",
                   family=binomial, data = d_test4)

# mean probability
plogis(summary(emp_intcp)$p.coeff)

# upper bound 95% CI
plogis(summary(emp_intcp)$p.coeff + 2 * summary(emp_intcp)$se)

# lower bound 95% CI
plogis(summary(emp_intcp)$p.coeff - 2 * summary(emp_intcp)$se)

#####                     Alpha                        #####
d_mfitbhv_DA_BA_alpha%>% group_by(cond)%>%summarise(mean(alpha, na.rm = T))

m_mfitbhv_BA_DA_gam <- gam(alpha ~ 
                             s(c_age, bs = "tp", by = cond) +
                             cond + 
                             s(SID, bs = "re"), 
                           method = "REML", 
                           data = d_mfitbhv_DA_BA_alpha)
summary(m_mfitbhv_BA_DA_gam)

#####                     Prisky                       #####
d_mfitbhv_DA_BA%>% group_by(cond)%>%summarise(mean(Prisky, na.rm = T))

m_mfitbhv_BA_DA_gam_Prisky_sqz <- gam(Prisky_sqz ~ 
                                        s(c_age, bs = "tp", by = cond) +
                                        cond + 
                                        s(SID, bs = "re"), 
                                      method = "REML", 
                                      family = betar,
                                      data = d_mfitbhv_DA_BA)
summary(m_mfitbhv_BA_DA_gam_Prisky_sqz)

#####                 weight_friend                    #####
d_mfitbhv_DA_BA_alpha%>% group_by(cond)%>%summarise(mean(weight_frd, na.rm = T))

m_BADA_weight_frd_excluA2 <- gam(weight_frd ~
                                   s(c_age, bs="tp", by = cond) +
                                   cond+s(SID, bs = "re"),
                                 method = 'REML',
                                 data = d_mfitbhv_DA_BA_alpha)
summary(m_BADA_weight_frd_excluA2)

add_minAge_mfitbhv_DA_BA_alpha <- function(a) {
  a=a+min(d_mfitbhv_DA_BA_alpha$age)
}

set.seed(123)
plot_diff(m_BADA_weight_frd_excluA2, view = "c_age", sim.ci = TRUE, 
          comp = list(cond = c("DA","BA")),
          xlab = "Age (years)", 
          ylab = expression("Difference in "*Weight["friend"]*": Desired Opposite - Opposite"), main = "",
          transform.view = add_minAge_mfitbhv_DA_BA_alpha, 
          col = WeightCol, hide.label = T, par(bg = "transparent"),
          font.main = 1, family = "sans", cex.lab = 1, cex.axis = 1, cex.main =1)

#####               Simulated Earnings                 #####

d_mfitbhv_DA_BA%>% group_by(cond)%>%summarise(mean(totalEarn_prop, na.rm = T))

m_BADA_totalEarnprop_beta_REML <- gam(totalEarn_prop_sqz ~
                                        s(c_age, bs="tp", by = cond) +
                                        cond +
                                        s(SID, bs = "re"),
                                      method = 'REML',
                                      family = betar,
                                      data = d_mfitbhv_DA_BA)
summary(m_BADA_totalEarnprop_beta_REML)

add_minAge_mfitbhv_DA_BA <- function(a) {
  a=a+min(d_mfitbhv_DA_BA$age)
}

set.seed(123)
plot_diff(m_BADA_totalEarnprop_beta_REML, view = "c_age", sim.ci = TRUE, 
          comp = list(cond = c("DA","BA")),
          xlab = "Age (years)", 
          ylab = expression("Difference in "*Weight["friend"]*": Desired Opposite - Opposite"), main = "",
          transform.view = add_minAge_mfitbhv_DA_BA,
          col = totalEarnCol, hide.label = T, par(bg = "transparent"),
          font.main = 1, family = "sans", cex.lab = 1, cex.axis = 1, cex.main =1)


#==========================================================================================#
#### III. Relinquishing preferred option when confronted with conflicting friend wishes ####
#==========================================================================================#

add_minAge <- function(a) {
  a=a+min(d_test6$age)
}

#####       intercept model for overall average        #####
m_original_wanted_intcp<-gam(relinquish ~  1, method = "REML",
                             family=binomial, data = d_test6)

# mean probability
plogis(summary(m_original_wanted_intcp)$p.coeff)

# upper bound 95% CI
plogis(summary(m_original_wanted_intcp)$p.coeff +
         2 * summary(m_original_wanted_intcp)$se)

# lower bound 95% CI
plogis(summary(m_original_wanted_intcp)$p.coeff - 
         2 * summary(m_original_wanted_intcp)$se)


#####                 age only                   #####
accm_age <- gam(relinquish ~ s(c_age, bs="tp") +
                  s(SID, bs = "re"), 
                method = 'REML', 
                family = binomial, 
                data = d_test6)
summary(accm_age)

#####                 safe vs. risky                   #####
d_test6 %>% group_by(response_self_BA) %>% 
  summarise(mean(as.numeric(as.character(relinquish)), na.rm=T))

m_original_wanted<-gam(relinquish ~
                         s(c_age, bs="tp", by = response_self_BA) +
                         response_self_BA +
                         s(SID, bs = "re"),
                       method = "REML",
                       family=binomial,
                       data = d_test6)
summary(m_original_wanted)

set.seed(123)
plot_diff(m_original_wanted, view = "c_age", sim.ci = TRUE, 
          comp = list(response_self_BA = c("certain", "chance")),
          xlab = "Age (years)", 
          ylab = "Odds of relinquishing preferred option: safe - risky", main="",
          transform.view = add_minAge, hide.label = T)

#####             EQEV: safe vs. risky                 #####
d_test6$EV_safe_risky <- relevel(d_test6$EV_safe_risky,ref = "risky-EQEV")

d_test6 %>% group_by(EV_safe_risky) %>% 
  summarise(mean(as.numeric(as.character(relinquish)), na.rm=T))

m_6EV_safe_risky<-gam(relinquish ~ s(c_age, bs="tp", by = EV_safe_risky) + 
                        EV_safe_risky + 
                        s(SID, bs = "re"), 
                      method = "REML",
                      family=binomial,
                      data = d_test6)
summary(m_6EV_safe_risky)

set.seed(123)
plot_diff(m_6EV_safe_risky, view = "c_age", sim.ci = TRUE, 
          comp = list(EV_safe_risky = c("safe-EQEV", "risky-EQEV")),
          xlab = "Age (years)", 
          ylab = "Odds of relinquishing preferred option: safe - risky", main="", 
          transform.view = add_minAge, hide.label = T)

