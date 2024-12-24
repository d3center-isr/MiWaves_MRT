# Project: MiWaves
# Description: Screen covariates 
# Author: Lauren Zimmermann

need_package <- c("dplyr","tidyverse","lubridate")
have_package <- need_package %in% rownames(installed.packages())
if (any(have_package==FALSE)) {
  install.packages(need_package[!have_package])
}

# set directory
library(tidyverse)
library(dplyr)
library(lubridate)
outdir<-"~/output"
proc_dat <- "//maize.umhsnas.med.umich.edu/Psychiatry/Restricted/MiWaves/Pilot/Processed Data/"

# load R datafile
load(file = paste0(proc_dat,"miwaves_analysis_dataset.Rdata"))
require(lme4)

# initialize output
reg_output <- data.frame()
rhs.vars.linear.me <- c("cov_prop_awakeuse_72hrs", "cov_prop_awakeuse_48hrs", "cov_prop_awakeuse_24hrs", "week_day_binary", "week_day","cov_interv_engag_72hrs", "cov_interv_engag_48hrs", "cov_interv_engag_24hrs")

library(lmerTest) # for p-values
# perform mixed effects linear regressions
for(var_name in rhs.vars.linear.me) {

  print("Covariate:")
  print(var_name)
  
  #lmm_formula <- paste0("prop_awakeuse ~ ",var_name," + morning + (1 | id)") # random intercept and adjusting for day, day of week and missingness indicator    
  lmm_formula <- paste0("interv_engagement_l ~ ",var_name," + morning + (1 | id)") # random intercept and adjusting for day, day of week and missingness indicator    
  res_lmm <- lme4::lmer(formula = lmm_formula, data = prim_dat 
                  #,REML = F
                  #,na.action = na.omit
                  )
  sum_res <- summary(res_lmm)
  print(sum_res)
  
  # next print with p-values
  res_lmm <- lmer(formula = lmm_formula, data = prim_dat
                        #,REML = F
                        #,na.action = na.omit
  )
  print(summary(res_lmm))
  
  # #next, compute a model where the effect of status is not estimated
  # lmm_formula <- paste0("prop_awakeuse ~ morning + (1 | id)")
  # restricted_fit = lmer(
  #   formula = value ~ (1|experiment)
  #   , REML = F #because we want to compare models on likelihood
  # )
  # #compute the AIC-corrected log-base-2 likelihood ratio (a.k.a. "bits" of evidence)
  # (AIC(restricted_fit)-AIC(unrestricted_fit))*log2(exp(1))
  
  reg_temp <- as.data.frame(sum_res$coefficients)
  reg_temp$Covariate <- rownames(reg_temp)
  rownames(reg_temp) <- c()
  reg_temp$RHS_variable <- var_name

  reg_output <- rbind(reg_output,reg_temp)
}

reg_results <- reg_output%>%
  rename(SE=`Std. Error`) %>%
  dplyr::mutate(UI_lower = Estimate - (1.96 * SE),
                UI_upper = Estimate + (1.96 * SE)
                )%>%
  relocate(RHS_variable, Covariate, .before=Estimate)

#write.csv(reg_results,file=paste0(outdir,"/Table_CanUse_Cov_Associations.csv"), row.names = F)
write.csv(reg_results,file=paste0(outdir,"/Table_Engag_Cov_Associations.csv"), row.names = F)

