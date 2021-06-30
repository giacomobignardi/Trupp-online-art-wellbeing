# Author: Giacomo Bignardi partialy adapted from Specker et al. YEAR here https://osf.io/42vm7/
# Date: 19 06 2020
#
#
#Description: re-run MANOVAs with outliers in to compare result
#ToC:
# Outliers: Assess if there univariate and multivariate outliers before running the MANOVA
# Assumptions: Test assumption for MANOVA
# MANOVA fit: Manova on the main effect of the experience (pre and post), the condition (art and control), 
#            and their interaction on Lonliness, STAI, Wellbeing, SAT
# Post hoc: Roy Bargman stepdown analysis
#          prioritezied ANOVA: negMood
#          stepdown ANCOVAs: posMood, STAI. Lone, SAT, wellbeing
# Program: Multivariate Analysis of Variance (MANOVA)----------------------------------------------------------------------------------------
# Load Libraries 

library(tidyverse)
library(rstatix)
library(tidylog)

#clear wd
rm(list = ls())

#set Open Access working directories
wdOA = getwd()
wdOA_scripts = "02_scripts"
wdOA_output = "03_outputs/processedData"

#set not Open Access working directories
wdNOA = substr(
  getwd(),
  0,
  nchar(getwd())-nchar("04_analysis_OA")-1
)
wdNOA_Data = "03_rawData/private"
wdNOA_ImageOutput = "05_images/image/processedData"


#prepare df
VirtualArt_data = read_csv(sprintf("%s/%s/01_VirtualArt_data_.csv", wdOA,wdOA_output))

#remove participants with more less 10 sec viewing
VirtualArt_data = VirtualArt_data%>%filter(time_exc>10)

VirtualArt_data_MANOVA =  VirtualArt_data %>%
  select(SubID = ResponseId, 
         aex = sex, 
         artInterest=art_interest_1, 
         LockDown_days = lock_days_1,
         experience_visit_again = art_experience_1,
         experience_meaning = art_experience_2,
         experience_beautifulness = art_experience_3,
         experience_goodness = art_good_1,
         condition = experience, 
         age = age_year,
         see_art,
         starts_with('per'), 
         starts_with('final_pre'),
         starts_with('final_post'))%>%
  pivot_longer(cols=final_pre_posMood:final_post_wellbeing,names_to= c("Final", "block", "Scale"),names_sep = "_", values_to = "Rating")%>%
  pivot_wider(names_from = "Scale", values_from = "Rating")%>%
  select (-Final)

#final df for MANOVA analysis (select to tidy and easly compute metrics such as Malhanobis distance)
Data = select(VirtualArt_data_MANOVA, SubID, condition, block,lone,STAI,wellbeing, SAT,posMood,negMood)

#remove all participants with no post measures
Data = Data[!(Data$SubID %in% Data[
  (is.na(Data$lone) + 
     is.na(Data$STAI) + 
     is.na(Data$wellbeing) + 
     is.na(Data$SAT) +
     is.na(Data$posMood) +
     is.na(Data$negMood)
  ) > 0,]$SubID),] #6 participants removed


####RESULTS####
#Does online engagement impact mood, anxiety, loneliness, and well-being in the art vs. non-art condition? 
Data$SubID = as.factor(Data$SubID)
Data$condition = as.factor(Data$condition)
Data$block = as.factor(Data$block)

####_MANOVA Fit####
#fit the model
fit_Manova_interaction  <- manova(cbind(lone,
                                        STAI,
                                        negMood,
                                        SAT,
                                        wellbeing,
                                        posMood
) ~ +block*condition + Error(factor(SubID)),data=Data) 

#model
result_MANOVA = summary(fit_Manova_interaction)

#####_post hoc roy bargman step-down analysis anova and ancova#####
#Roy bargman (Tabachink & Fidel):
#first step: post hoc ANOVA with prioritized DV (negative mood)
#stepdown ANCOVAs: positve mood, STAI, Lone, SAT, Wellbeing (inclued each DVs as covariate in a stepwise fashion). Below both ANCOVA and ANOVA are reported

####__Negative mood####
#ANOVA 1
fit_Anova_negMod = aov(negMood ~ block + Error(SubID), Data)
result_fit_Anova_negMod =  summary(fit_Anova_negMod)

#compute differences
Data %>%
  group_by(block) %>%
  get_summary_stats(negMood)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(negMood)

####__Positive mood####
#anova 2
fit_Anova_posMod = aov(posMood ~ block + Error(SubID), Data) 
result_fit_Anova_posMod = summary(fit_Anova_posMod)
#ANCOVA 1
#check for homogeneoty of regression slopes
summary(aov(posMood ~ negMood*block + Error(SubID), Data)) #met
fit_Ancova_posMod = aov(posMood ~ negMood + block + Error(SubID), Data) 
result_fit_Ancova_posMood = summary(fit_Ancova_posMod)
#difference
Data %>%
  group_by(block) %>%
  get_summary_stats(posMood)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(posMood)

####__STAI####
#anova 3
fit_Anova_STAI = aov(STAI ~ block + Error(SubID), Data) 
result_fit_Anova_STAI = summary(fit_Anova_STAI)
#ANCOVA 2
#check for homogeneoty of regression slopes
summary(aov(STAI ~ posMood*negMood*block + Error(SubID), Data)) #met
fit_Ancova_STAI = aov(STAI ~  negMood + posMood + block + Error(SubID), Data) 
result_fit_Ancova_STAI = summary(fit_Ancova_STAI)
#difference
Data %>%
  group_by(block) %>%
  get_summary_stats(STAI)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(STAI)

####__lone####
#anova 4
fit_Anova_lone = aov(lone ~ block + Error(SubID), Data)
result_fit_Anova_lone = summary(fit_Anova_lone)
#ANCOVA 3
#check for homogeneoty of regression slopes
summary(aov(lone ~ negMood*posMood*STAI*block + Error(SubID), Data)) #met
fit_Ancova_lone = aov(lone ~ negMood +posMood + STAI + block + Error(SubID), Data) 
result_fit_Ancova_lone = summary(fit_Ancova_lone)
#difference
Data %>%
  group_by(block) %>%
  get_summary_stats(lone)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(lone)

####__SAT####
#Anova 5
fit_Anova_SAT = aov(SAT ~ block + Error(SubID), Data) 
result_fit_Anova_SAT = summary(fit_Anova_SAT)
#ANCOVA 4
#check for homogeneoty of regression slopes
summary(aov(SAT ~ negMood*posMood*STAI*lone*block + Error(SubID), Data)) #met
fit_Ancova_SAT = aov(SAT ~ negMood + posMood + STAI + lone + block+ Error(SubID), Data) 
result_fit_Ancova_SAT = summary(fit_Ancova_SAT)
#difference
Data %>%
  group_by(block) %>%
  get_summary_stats(SAT)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(SAT)

####__Wellbeing####
#Anova 6
fit_Anova_wellbeing = aov(wellbeing ~ block + Error(SubID), Data) 
result_fit_Anova_wellbeing = summary(fit_Anova_wellbeing)
#ANCOVA 5
#check for homogeneoty of regression slopes
summary(aov(wellbeing ~ negMood*posMood*STAI*lone*SAT*block + Error(SubID), Data)) #met
fit_Ancova_wellbeing = aov(wellbeing ~ negMood + posMood + STAI + lone +SAT +  block + Error(SubID), Data) 
result_fit_Ancova_wellbeing = summary(fit_Ancova_wellbeing)
#difference
Data %>%
  group_by(block) %>%
  get_summary_stats(wellbeing)
Data %>%
  group_by(block,condition) %>%
  get_summary_stats(wellbeing)

#Wrap up####
#very tedious coding to prodcuce compact df
#List of stepdown model
Model_result = list(
  Manova = result_MANOVA,
  Anova_negMood = result_fit_Anova_negMod,
  Anova_posMood = result_fit_Anova_posMod,
  Ancova_poosMood = result_fit_Ancova_posMood,
  Anova_STAI = result_fit_Anova_STAI,
  Ancova_STAI = result_fit_Ancova_STAI,
  Anova_lone = result_fit_Anova_lone,
  Ancova_lone = result_fit_Ancova_lone,
  Ancova_SAT = result_fit_Ancova_SAT,
  Anova_wellbeing =  result_fit_Anova_wellbeing,
  Anova_wellbeing =  result_fit_Ancova_wellbeing
)

#name etaSquared
eta_ANOVA_negMood = effectsize::eta_squared(fit_Anova_negMod)
eta_ANOVA_posMood = effectsize::eta_squared(fit_Anova_posMod)
eta_ANCOVA_posMood = effectsize::eta_squared(fit_Ancova_posMod)
eta_ANOVA_STAI = effectsize::eta_squared(fit_Anova_STAI)
eta_ANCOVA_STAI = effectsize::eta_squared(fit_Ancova_STAI)
eta_ANOVA_lone = effectsize::eta_squared(fit_Anova_lone)
eta_ANCOVA_lone = effectsize::eta_squared(fit_Ancova_lone)
eta_ANOVA_SAT = effectsize::eta_squared(fit_Anova_SAT)
eta_ANCOVA_SAT = effectsize::eta_squared(fit_Ancova_SAT)
eta_ANOVA_wellbeing = effectsize::eta_squared(fit_Anova_wellbeing)
eta_ANCOVA_wellbeing = effectsize::eta_squared(fit_Ancova_wellbeing)

#signficane (univariate stepdown)
Anova_univariate_significance = rbind(tibble(type= "Anova", step = as.factor(0), var = "negMood",  condition = "Combined",
                                             p=result_fit_Anova_negMod$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_negMood$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_negMood$CI_low,3),
                                             CI_high = round(eta_ANOVA_negMood$CI_high,3)), # (alpha/alpha_adj)
                                      tibble(type= "Anova", step = as.factor(1), var = "posMood", condition = "Combined",
                                             p=result_fit_Anova_posMod$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_posMood$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_posMood$CI_low,3),
                                             CI_high = round(eta_ANOVA_posMood$CI_high,3)),
                                      
                                      tibble(type= "Anova", step = as.factor(2), var = "STAI", condition = "Combined",
                                             p=result_fit_Anova_STAI$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_STAI$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_STAI$CI_low,3),
                                             CI_high = round(eta_ANOVA_STAI$CI_high,3)),
                                      tibble(type= "Anova", step = as.factor(3), var = "lone", condition = "Combined",
                                             p=result_fit_Anova_lone$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_lone$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_lone$CI_low,3),
                                             CI_high = round(eta_ANOVA_lone$CI_high,3)),
                                      tibble(type= "Anova", step = as.factor(4), var = "SAT",condition = "Combined",
                                             p=result_fit_Anova_SAT$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_SAT$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_SAT$CI_low,3),
                                             CI_high = round(eta_ANOVA_SAT$CI_high,3)),
                                      tibble(type= "Anova", step = as.factor(5), var = "wellbeing", condition = "Combined",
                                             p=result_fit_Anova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[1],
                                             eta_squared = round(eta_ANOVA_wellbeing$Eta2_partial,3),
                                             CI_low = round(eta_ANOVA_wellbeing$CI_low,3),
                                             CI_high = round(eta_ANOVA_wellbeing$CI_high,3))
)

#adjust main effect for multiple comparisons
Anova_univariate_significance$p_adj = round(p.adjust(Anova_univariate_significance$p, method = "bonferroni"),3)
#tidy order
Anova_univariate_significance_outliers = Anova_univariate_significance[,c(1,2,3,4,5,9,6,7,8)]

#signficane (multivariate stepdown), i.e. taking into account shared effects)
Ancova_RoyBergamn_significance = rbind( #negMood
  tibble(type= "Anova",step = as.factor(0), main = "negMood", covariate = NA, 
         p=result_fit_Anova_negMod$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANOVA_negMood$Eta2_partial,
         CI = sprintf("[%s,%s]",round(eta_ANOVA_negMood$CI_low,3),round(eta_ANOVA_negMood$CI_high,3))),
  #posMood
  tibble(type= "Ancova",step = as.factor(1), main = "posMood", covariate = "negMood",
         p=result_fit_Ancova_posMood$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANCOVA_posMood$Eta2_partial[3],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_posMood$CI_low[3],3),round(eta_ANCOVA_posMood$CI_high[3],3))),
  tibble(type= "Ancova",step = as.factor(1), main = "posMood", covariate = NA, 
         p=result_fit_Ancova_posMood$`Error: Within`[[1]]$`Pr(>F)`[2],
         eta_squared = eta_ANCOVA_posMood$Eta2_partial[2],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_posMood$CI_low[2],3),round(eta_ANCOVA_posMood$CI_high[2],3))),
  
  #STAI
  tibble(type= "Ancova",step = as.factor(2), main = "STAI", covariate = "negMood",
         p=result_fit_Ancova_STAI$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANCOVA_STAI$Eta2_partial[5],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_STAI$CI_low[5],3),round(eta_ANCOVA_STAI$CI_high[5],3))),
  tibble(type= "Ancova",step = as.factor(2), main = "STAI", covariate = "posMood",
         p=result_fit_Ancova_STAI$`Error: Within`[[1]]$`Pr(>F)`[2],
         eta_squared = eta_ANCOVA_STAI$Eta2_partial[4],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_STAI$CI_low[4],3),round(eta_ANCOVA_STAI$CI_high[4],3))),
  tibble(type= "Ancova",step = as.factor(2), main = "STAI", covariate = NA,
         p=result_fit_Ancova_STAI$`Error: Within`[[1]]$`Pr(>F)`[3],
         eta_squared = eta_ANCOVA_STAI$Eta2_partial[3],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_STAI$CI_low[3],3),round(eta_ANCOVA_STAI$CI_high[3],3))),
  #lone
  tibble(type= "Ancova",step = as.factor(3), main = "lone", covariate = "negMood",
         p=result_fit_Ancova_lone$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANCOVA_lone$Eta2_partial[6],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_lone$CI_low[6],3),round(eta_ANCOVA_lone$CI_high[6],3))),
  tibble(type= "Ancova",step = as.factor(3), main = "lone", covariate = "posMood",
         p=result_fit_Ancova_lone$`Error: Within`[[1]]$`Pr(>F)`[2],
         eta_squared = eta_ANCOVA_lone$Eta2_partial[4],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_lone$CI_low[4],3),round(eta_ANCOVA_lone$CI_high[4],3))),
  tibble(type= "Ancova",step = as.factor(3), main = "lone", covariate = "STAI",
         p=result_fit_Ancova_lone$`Error: Within`[[1]]$`Pr(>F)`[3],
         eta_squared = eta_ANCOVA_lone$Eta2_partial[7],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_lone$CI_low[7],3),round(eta_ANCOVA_lone$CI_high[7],3))),
  tibble(type= "Ancova",step = as.factor(3), main = "lone", covariate = NA,
         p=result_fit_Ancova_lone$`Error: Within`[[1]]$`Pr(>F)`[4],
         eta_squared = eta_ANCOVA_lone$Eta2_partial[5],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_lone$CI_low[5],3),round(eta_ANCOVA_lone$CI_high[5],3))),
  #SAT
  tibble(type= "Ancova",step = as.factor(4), main = "SAT", covariate = "negMood",
         p=result_fit_Ancova_SAT$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANCOVA_SAT$Eta2_partial[8],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_SAT$CI_low[8],3),round(eta_ANCOVA_SAT$CI_high[8],3))),
  tibble(type= "Ancova",step = as.factor(4), main = "SAT", covariate = "posMood",
         p=result_fit_Ancova_SAT$`Error: Within`[[1]]$`Pr(>F)`[2],
         eta_squared = eta_ANCOVA_SAT$Eta2_partial[9],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_SAT$CI_low[9],3),round(eta_ANCOVA_SAT$CI_high[9],3))),
  tibble(type= "Ancova",step = as.factor(4), main = "SAT", covariate = "STAI",
         p=result_fit_Ancova_SAT$`Error: Within`[[1]]$`Pr(>F)`[3],
         eta_squared = eta_ANCOVA_SAT$Eta2_partial[5],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_SAT$CI_low[5],3),round(eta_ANCOVA_SAT$CI_high[5],3))),
  tibble(type= "Ancova",step = as.factor(4), main = "SAT", covariate = "lone",
         p=result_fit_Ancova_SAT$`Error: Within`[[1]]$`Pr(>F)`[4],
         eta_squared = eta_ANCOVA_SAT$Eta2_partial[6],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_SAT$CI_low[6],3),round(eta_ANCOVA_SAT$CI_high[6],3))),
  tibble(type= "Ancova",step = as.factor(4), main = "SAT", covariate = NA,
         p=result_fit_Ancova_SAT$`Error: Within`[[1]]$`Pr(>F)`[5],
         eta_squared = eta_ANCOVA_SAT$Eta2_partial[7],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_SAT$CI_low[7],3),round(eta_ANCOVA_SAT$CI_high[7],3))),
  #wellbeing
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = "negMood",
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[1],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[11],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[11],3),round(eta_ANCOVA_wellbeing$CI_high[11],3))),
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = "posMood",
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[2],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[10],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[10],3),round(eta_ANCOVA_wellbeing$CI_high[10],3))),
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = "STAI",
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[3],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[6],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[6],3),round(eta_ANCOVA_wellbeing$CI_high[6],3))),
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = "lone",
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[4],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[9],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[9],3),round(eta_ANCOVA_wellbeing$CI_high[9],3))),
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = "SAT",
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[5],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[7],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[7],3),round(eta_ANCOVA_wellbeing$CI_high[7],3))),
  tibble(type= "Ancova",step = as.factor(5), main = "wellbeing", covariate = NA,
         p=result_fit_Ancova_wellbeing$`Error: Within`[[1]]$`Pr(>F)`[6],
         eta_squared = eta_ANCOVA_wellbeing$Eta2_partial[8],
         CI = sprintf("[%s,%s]",round(eta_ANCOVA_wellbeing$CI_low[8],3),round(eta_ANCOVA_wellbeing$CI_high[8],3)))
)

#adjust main effect for multiple comparisons but not covariate
Ancova_RoyBergamn_significance$p_adj = ifelse(is.na(Ancova_RoyBergamn_significance$covariate), 
                                              round(p.adjust(Ancova_RoyBergamn_significance[is.na(Ancova_RoyBergamn_significance$covariate),]$p, method = "bonferroni"),3),
                                              round(Ancova_RoyBergamn_significance$p,3))
#tidy order
Ancova_RoyBergamn_significance_outliers = Ancova_RoyBergamn_significance[c(1,2,3,4,5,8,6,7)]

save(Anova_univariate_significance_outliers,Ancova_RoyBergamn_significance_outliers,file = sprintf("%s/%s/04_MANOVA_outliers_results.Rdata", wdOA,wdOA_output))