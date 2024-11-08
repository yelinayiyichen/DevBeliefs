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
load("supplementary_analysis.rdata")

#============================================================================================================#
####                     A.	Distributions of actual and simulated random earnings                         ####
#============================================================================================================#

# the following code generates simulated earnings through 10000 random draws. 
# 
# probs <- data.frame(probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
# amounts <- data.frame(amounts = c(5, 10, 20, 30, 40, 50, 100))
# random_sim <- cross_join(probs, amounts)
# random_sim$EV_safe <- 5
# random_sim <- random_sim %>% mutate(EV_risky = probs * amounts)
# 
# sim_data <- data.frame(matrix(ncol = 7, nrow = 0))
# colnames(sim_data) <- c("probs", "amounts", "EV_safe", "EV_risky", "id", "sim_chose_risky", "trial_earn")
# for (x in 1:10000){
#   random_choice <- data.frame(sim_chose_risky = rbern(length(random_sim$EV_risky), p = 0.5))
#   this_sim <- random_sim
#   this_sim$id <- x
#   this_sim <- cbind(this_sim, random_choice)
#   this_sim <- this_sim %>% mutate(trial_earn = ifelse(sim_chose_risky == 1, EV_risky, EV_safe))
#   sim_data <- rbind(sim_data, this_sim)
# }
# 
# earns_dstb <- sim_data %>% 
#   group_by(id) %>%
#   summarise(total_earning = sum(trial_earn))

# compare simulated earnings to actual data in baseline
mean(earns_dstb$total_earning)
sd(earns_dstb$total_earning)
mean(d_baseline_only$totalEarn)
sd(d_baseline_only$totalEarn)

ggplot(earns_dstb, aes(x = total_earning, fill = "t")) +
  geom_density(alpha = 0.4) +
  geom_density(data = d_baseline_only, aes(x = totalEarn,fill = "s"), alpha = 0.4) +
  labs(title = "Density Plot of Earnings", x = "Earning", y = "Density", fill = "Data Types")+
  theme_classic(base_size = 11,
                base_family = "sans") +
  theme(plot.title = element_text(size=11, hjust = 0.5, face = "plain"),
        text = element_text(family = "sans", size=11),
        axis.text = element_text(size = 11)) +
  scale_fill_manual(name = 'Data Types', 
                    values =c('t'='turquoise','s'='lightsalmon'), 
                    labels = c('actual', 'simulated'))


#============================================================================================================#
#### B.	Distribution of trials and participants across age for analyses on relinquishing preferred option ####
#============================================================================================================#

#####                             all trials                               #####
# number of trials
set.seed(123)
hist(d_test6$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Trials")

# number of participants
set.seed(123)
hist((d_test6 %>% dplyr::select(SID, age) %>% unique(.))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Participants", ylim = c(0, 20))

#####                            EQEV trials                               #####
# number of trials
set.seed(123)
hist((d_test6 %>% filter(EV_type == "EQEV"))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Trials", ylim = c(0, 50))

# number of participants
set.seed(123)
hist((d_test6 %>% filter(EV_type == "EQEV") %>% dplyr::select(SID, age) %>% unique(.))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Participants", ylim = c(0, 20))

#####                             RA trials                                #####
# number of trials
set.seed(123)
hist((d_test6 %>% filter(EV_type == "RA"))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Trials")

# number of participants
set.seed(123)
hist((d_test6 %>% filter(EV_type == "RA") %>% dplyr::select(SID, age) %>% unique(.))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Participants", ylim = c(0, 20))

#####                             RD trials                                #####
# number of trials
set.seed(123)
hist((d_test6 %>% filter(EV_type == "RD"))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Trials")

# number of participants
set.seed(123)
hist((d_test6 %>% filter(EV_type == "RD") %>% dplyr::select(SID, age) %>% unique(.))$age,
     main = "",xlab = "Age (Years)", ylab = "Number of Participants", ylim = c(0, 20))

#============================================================================================================#
####        C.	Relinquishing preferred options on trials with different EV for risky and safe options    ####
#============================================================================================================#

d_test6$EV_safe_risky <- relevel(d_test6$EV_safe_risky,ref = "risky-RA")

m_6EV_safe_risky<-gam(relinquish ~ s(c_age, bs="tp", by = EV_safe_risky) + 
                        EV_safe_risky + s(SID, bs = "re"), method = "REML",
                      family=binomial,data = d_test6)
summary(m_6EV_safe_risky)


set.seed(123)
plot_diff(m_6EV_safe_risky, view = "c_age", sim.ci = TRUE, comp = list(EV_safe_risky = c("safe-RA", "risky-RA")),
          xlab = "Age (years)", ylab = "Odds of relinquishing preferred option: Safe - Risky", main = "",
          transform.view = add_minAge, hide.label = T)

d_test6$EV_safe_risky <- relevel(d_test6$EV_safe_risky,ref = "risky-RD")

m_6EV_safe_risky<-gam(relinquish ~ s(c_age, bs="tp", by = EV_safe_risky) + 
                        EV_safe_risky + s(SID, bs = "re"), method = "REML",
                      family=binomial,data = d_test6)
summary(m_6EV_safe_risky)

#============================================================================================================#
####                        D.	 Effect of observation on risky choice and peer effects                   ####
#============================================================================================================#

m_zoomA_accm <- gam(relinquish ~ s(c_age, bs="tp", by = viewing_status) +
                      viewing_status +
                      Amount + 
                      Chance +
                      s(SID, bs = "re"), 
                    method = 'REML', 
                    family = binomial, 
                    data = d_test6)
summary(m_zoomA_accm)

m_accm_age <- gam(relinquish ~ s(c_age, bs="tp") + 
                    viewing_status + 
                    Amount + 
                    Chance + 
                    s(SID, bs = "re"), 
                  method = 'REML', 
                  family = binomial, 
                  data = d_test6)
summary(m_accm_age)
