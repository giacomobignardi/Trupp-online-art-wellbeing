# Author: Giacomo Bignardi
# Date: 12 04 2021
#
#
#Program: Exploratory analysis------------------------------------------------------------------------------------------------------------------------------------------------
# Load Libraries 
library(readr)
library(tidyverse)
library(psych)
library(rstatix)

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


####_I saw art: Effect on DVs####
Data = read_csv(sprintf("%s/%s/03_df_pre_post.csv", wdOA,wdOA_output))
#load exclusion criteria
multivariate_outliers = read_csv(sprintf("%s/%s/03_multivariate_outliers.csv", wdOA,wdOA_output))
VirtualArt_data = read_csv(sprintf("%s/%s/01_VirtualArt_data_.csv", wdOA,wdOA_output))

#remove all participants with no post measures
Data = Data[!(Data$SubID %in% Data[
  (is.na(Data$lone) + 
     is.na(Data$STAI) + 
     is.na(Data$wellbeing) + 
     is.na(Data$SAT) +
     is.na(Data$posMood) +
     is.na(Data$negMood)
  ) > 0,]$SubID),]

Data = filter(Data, !SubID %in% pull(multivariate_outliers))

#tidy df as in previous scripts
VirtualArt_data$see_art  = ifelse(VirtualArt_data$see_art == "Yes, I saw art", 1, 2)
VirtualArt_data = VirtualArt_data%>%filter(time_exc>10)
#exculde participants which were multivariate outliers
VirtualArt_data_multivariate = filter(VirtualArt_data, ResponseId %in% Data$SubID)

####EXPLORATORY ANALYSIS####
####_Time####
cor_mat(VirtualArt_data_multivariate%>%select(ends_with("_change"),time_exc), method = "spearman")
cor_pmat(VirtualArt_data_multivariate%>%select(ends_with("_change"),time_exc), method = "spearman")


####_relationship with aesthetic evaluation ratings####
Df_evaluation_DVs = rbind(
  
  Pos_mood_desire = VirtualArt_data_multivariate%>%cor_test(positive_mood_change,art_experience_1, method = "spearman"),
  Pos_mood_meaning = VirtualArt_data_multivariate%>%cor_test(positive_mood_change,art_experience_2, method = "spearman"),
  Pos_mood_beauty = VirtualArt_data_multivariate%>%cor_test(positive_mood_change,art_experience_3, method = "spearman"),
  Pos_mood_goodness = VirtualArt_data_multivariate%>%cor_test(positive_mood_change,art_good_1, method = "spearman"),
  
  Neg_mood_desire = VirtualArt_data_multivariate%>%cor_test(negative_mood_change,art_experience_1, method = "spearman"),
  Neg_mood_meaning = VirtualArt_data_multivariate%>%cor_test(negative_mood_change,art_experience_2, method = "spearman"),
  Neg_mood_beauty = VirtualArt_data_multivariate%>%cor_test(negative_mood_change,art_experience_3, method = "spearman"),
  Neg_mood_goodness = VirtualArt_data_multivariate%>%cor_test(negative_mood_change,art_good_1, method = "spearman"),
  
  Lone_desire = VirtualArt_data_multivariate%>%cor_test(Lone_change,art_experience_1, method = "spearman"),
  Lone_meaning =  VirtualArt_data_multivariate%>%cor_test(Lone_change,art_experience_2, method = "spearman"),
  Lone_beauty = VirtualArt_data_multivariate%>%cor_test(Lone_change,art_experience_3, method = "spearman"),
  Lone_goodness = VirtualArt_data_multivariate%>%cor_test(Lone_change,art_good_1, method = "spearman"),
  
  STAI_desire = VirtualArt_data_multivariate%>%cor_test(STAI_change,art_experience_1, method = "spearman"),
  STAI_meaning =  VirtualArt_data_multivariate%>%cor_test(STAI_change,art_experience_2, method = "spearman"),
  STAI_beauty =  VirtualArt_data_multivariate%>%cor_test(STAI_change,art_experience_3, method = "spearman"),
  STAI_goodness =  VirtualArt_data_multivariate%>%cor_test(STAI_change,art_good_1, method = "spearman"),

  Wellbeing_desire =  VirtualArt_data_multivariate%>%cor_test(Wellbeing_change,art_experience_1, method = "spearman"),
  Wellbeing_meaning = VirtualArt_data_multivariate%>%cor_test(Wellbeing_change,art_experience_2, method = "spearman"),
  Wellbeing_beauty = VirtualArt_data_multivariate%>%cor_test(Wellbeing_change,art_experience_3, method = "spearman"),
  Wellbeing_goodness = VirtualArt_data_multivariate%>%cor_test(Wellbeing_change,art_good_1, method = "spearman"),
  
  SAT_desire =  VirtualArt_data_multivariate%>%cor_test(SAT_change,art_experience_1, method = "spearman"),
  SAT_meaning = VirtualArt_data_multivariate%>%cor_test(SAT_change,art_experience_2, method = "spearman"),
  SAT_beauty = VirtualArt_data_multivariate%>%cor_test(SAT_change,art_experience_3, method = "spearman"),
  SAT_goodness = VirtualArt_data_multivariate%>%cor_test(SAT_change,art_good_1, method = "spearman")
)

#significant relationships
Df_evaluation_DVs%>%filter(p <.05)
Df_evaluation_DVs%>%filter(p >=.05 & p <.1)
Df_evaluation_DVs$p_adj = p.adjust(Df_evaluation_DVs$p, method = "bonferroni")
Df_evaluation_DVs%>%filter(p_adj <.1)
nrow(Df_evaluation_DVs)

####_difference in aesthetic evaluation between saw art groups####
VirtualArt_data_multivariate$see_art = factor(VirtualArt_data_multivariate$see_art, levels = c("1", "2"),
                                  labels = c("saw_Art", "saw_no_Art"))

table(VirtualArt_data_multivariate$see_art)
VirtualArt_data_multivariate%>%group_by(see_art)%>%get_summary_stats(art_experience_1,art_experience_2,art_experience_3,art_good_1)

#descriptives DVs_saw art
VirtualArt_data_multivariate%>%group_by(see_art)%>%get_summary_stats(negative_mood_change, positive_mood_change, STAI_change, Lone_change, SAT_change, Wellbeing_change)

#Are aesthetic evaluations similar between subjective reported condition (i.e meaning responses were similar between reported exp.)
W_art_desire = VirtualArt_data_multivariate%>%wilcox_test(art_experience_1~see_art)
W_art_meaning = VirtualArt_data_multivariate%>%wilcox_test(art_experience_2~see_art)
W_art_beauty = VirtualArt_data_multivariate%>%wilcox_test(art_experience_3~see_art)
W_art_good = VirtualArt_data_multivariate%>%wilcox_test(art_good_1~see_art)

#Z for Wilcox test
Z_art_desire = qnorm(W_art_desire$p/2)
Z_art_meaning = qnorm(W_art_meaning$p/2)
Z_art_beauty = qnorm(W_art_beauty$p/2)
Z_art_good = qnorm(W_art_good$p/2)


####_pre post T test grouped by see art####
#difference between the two condition
t.test(negative_mood_change~see_art,VirtualArt_data_multivariate)
t.test(positive_mood_change~see_art,VirtualArt_data_multivariate)
t.test(STAI_change~see_art,VirtualArt_data_multivariate)
t.test(Lone_change~see_art,VirtualArt_data_multivariate)
t.test(SAT_change~see_art,VirtualArt_data_multivariate)
t.test(Wellbeing_change~see_art,VirtualArt_data_multivariate)

####SUPPLEMENTARY####
#correlations between the DV changes and the Evaluations of the experience grouped by the conditions bento and waterlilies
Df_evaluation_DVs_con = rbind(
  
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(negative_mood_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(negative_mood_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(negative_mood_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(negative_mood_change,art_good_1, method = "spearman"),
  
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(positive_mood_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(positive_mood_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(positive_mood_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(positive_mood_change,art_good_1, method = "spearman"),

  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(STAI_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(STAI_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(STAI_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(STAI_change,art_good_1, method = "spearman"),
  
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Lone_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Lone_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Lone_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Lone_change,art_good_1, method = "spearman"),
  
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Wellbeing_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Wellbeing_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Wellbeing_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(Wellbeing_change,art_good_1, method = "spearman"),

  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(SAT_change,art_experience_1, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(SAT_change,art_experience_2, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(SAT_change,art_experience_3, method = "spearman"),
  VirtualArt_data_multivariate%>%group_by(Condition)%>%cor_test(SAT_change,art_good_1, method = "spearman")

)