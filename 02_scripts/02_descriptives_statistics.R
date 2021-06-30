#Author: Giacomo Bignardi
#Date: 29/12/2020
#
#
#
#Calculate descriptives
#Program: Descriptives_statistics------------------------------------------------------------------------------------------------------------------------------
#load packages
library(readr)
library(tidyverse)
library(rstatix)
library(tidylog)
require(psych)

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



#load df
VirtualArt_data = read_csv(sprintf("%s/%s/01_VirtualArt_data_.csv", wdOA,wdOA_output))

####METHOD####
###_Participants####
#n of participants
nrow(VirtualArt_data)

####__Remove####
#criteria: participants who spent less than 10 sec on the online experience were removed.
#remove participants with more less 10 sec viewing
VirtualArt_data = VirtualArt_data%>%filter(time_exc>10)
#n of participants after exclusion
nrow(VirtualArt_data)

####__age####
VirtualArt_data%>%get_summary_stats(age_year)
#Age by type of cultural experience
VirtualArt_data%>%group_by(experience)%>%get_summary_stats(age_year)
sd(VirtualArt_data$age_year)
####__sex####
table(VirtualArt_data$sex)
sum(is.na(VirtualArt_data$sex))
#Sex by type of culturale experience
table(VirtualArt_data$sex, VirtualArt_data$experience)
VirtualArt_data[which(is.na(VirtualArt_data$sex)),]$experience
####__nationality###
Nationality = as.data.frame(table(VirtualArt_data$nationality))
#manually add information about continents (matching their nationality) from which the participants were originally from
Continent = c(
  "Europe",
  "America",
  "America",
  "Asia",
  "Africa",
  "Europe",
  "Europe",
  "Asia",
  "Europe",
  "Asia",
  "Europe",
  "Europe",
  "Asia",
  "Europe",
  "Europe",
  "Asia",
  "Europe",
  "Asia",
  "Europe",
  "America"
)
Nationality_continent = cbind(Nationality,Continent)

N_Europe_nat = sum(Nationality_continent[Nationality_continent$Continent == "Europe","Freq"])
P_Europe_nat = N_Europe_nat/sum(Nationality_continent$Freq) *100
N_Asia_nat = sum(Nationality_continent[Nationality_continent$Continent == "Asia","Freq"])
P_Asia_nat = N_Asia_nat/sum(Nationality_continent$Freq) *100
N_America_nat = sum(Nationality_continent[Nationality_continent$Continent == "America","Freq"])
P_America_nat = N_America_nat/sum(Nationality_continent$Freq) *100
N_Africa_nat = sum(Nationality_continent[Nationality_continent$Continent == "Africa","Freq"])
P_Africa_nat = N_Africa_nat/sum(Nationality_continent$Freq) *100

nationality_freq = c(N_Europe_nat = N_Europe_nat,
                     P_Europe_nat = P_Europe_nat,
                     N_Asia_nat = N_Asia_nat,
                     P_Asia_nat = P_Asia_nat,
                     N_America_nat = N_America_nat,
                     P_America_nat = P_America_nat,
                     N_Africa_nat = N_Africa_nat,
                     P_Africa_nat = P_Africa_nat)

####RESULTS####
####_Final Sample Characteristics####
#Proportion of participant who attended to art events before
prop.table(table(VirtualArt_data$pre_gall))
####_General viewing time and art/not art classification####
####__time####
ggplot(VirtualArt_data, aes(time_exc)) +
geom_histogram() +
facet_grid(~Condition)
VirtualArt_data%>%group_by(Condition)%>%get_summary_stats(time_exc)
#check if the difference is significant
W_time = VirtualArt_data%>%wilcox_test(time_exc~Condition)
#Z-score
Z_time = qnorm(W_time$p/2)
####__saw art####
#test for differences
Freq_cond_see_art = table(VirtualArt_data$Condition,VirtualArt_data$see_art)
Prop_cond_art = prop.table(table(VirtualArt_data$Condition,VirtualArt_data$see_art)[1,]) #prop condition art
Prop_cond_non_art = prop.table(table(VirtualArt_data$Condition,VirtualArt_data$see_art)[2,]) #prop condition non_art
VirtualArt_data$see_art  = ifelse(VirtualArt_data$see_art == "Yes, I saw art", 1, 2)
# Test the assumption that the two individuals saw art in one but not in the other ( Chi Square )
SawArtTable = as.data.frame(Freq_cond_see_art)
chisq.test(rbind(SawArtTable[SawArtTable$Var1 == 1,"Freq"],SawArtTable[SawArtTable$Var1 == 2,"Freq"])) #assumption not meet


####_Descriptive Results of Online Cultural Engagement####
####__aesthetic evaluation####
#art_experience_1 = desire to visit again
#art_experience_2 =  meaningfull
#art_experience_3 =  beautifull
#art_good_1 = good

#means and sd
VirtualArt_data%>%
  group_by(Condition)%>%
  get_summary_stats(art_experience_1,art_experience_2,art_experience_3,art_good_1)

#HO: the sample distib. per condition are similar (i.e meaning responses were similar between condition)
W_art_desire = VirtualArt_data%>%wilcox_test(art_experience_1~Condition)
W_art_meaning = VirtualArt_data%>%wilcox_test(art_experience_2~Condition)
W_art_beauty = VirtualArt_data%>%wilcox_test(art_experience_3~Condition)
W_art_good = VirtualArt_data%>%wilcox_test(art_good_1~Condition)
#Z for Wilcox test
Z_art_desire = qnorm(W_art_desire$p/2)
Z_art_meaning = qnorm(W_art_meaning$p/2)
Z_art_beauty = qnorm(W_art_beauty$p/2)
Z_art_good = qnorm(W_art_good$p/2)
#significance
aesthetic_adjusted_pvalue = p.adjust(c(W_art_desire$p,
                                       W_art_meaning$p,
                                       W_art_beauty$p,
                                       W_art_good$p), "bonferroni", n = 4)




####SUPPLEMENTARY####
####_Supplementary Table 1####
####__art interest####
VirtualArt_data$art_visit = as.factor(VirtualArt_data$art_visit)
VirtualArt_data$art_read = as.factor(VirtualArt_data$art_read)                           
VirtualArt_data$art_pic  = as.factor(VirtualArt_data$art_pic)                               
VirtualArt_data$art_event = as.factor(VirtualArt_data$art_event)

#Compute mean for the two art vs non art conditions (Art Interest)
Art_Interest = VirtualArt_data %>%get_summary_stats(art_interest_1)
#Compute median for the two art vs non art conditions (Art Visit)
Art_Visit = VirtualArt_data %>%summarise(median(as.numeric(art_visit)), min(as.numeric(art_visit)), max(as.numeric(art_visit)))
#Compute median for the two art vs non art conditions (Art reading)
Art_reading = VirtualArt_data%>%summarise(median(as.numeric(art_read)), min(as.numeric(art_read)), max(as.numeric(art_read)))
#Compute median for the two art vs non art conditions (Art looking)
Art_looking = VirtualArt_data%>%summarise(median(as.numeric(art_pic)), min(as.numeric(art_pic)), max(as.numeric(art_pic)))
#Compute median for the two art vs non art conditions  (Art event)
Art_event = VirtualArt_data %>%summarise(median(as.numeric(art_event)), min(as.numeric(art_event)), max(as.numeric(art_event)))

####__test for random allocation####
#Pre DV: Compare the mean of the DVs by the two sample T test 
VirtualArt_data %>%group_by(Condition)%>%get_summary_stats(final_pre_posMood)
T_pre_posMood = VirtualArt_data%>%t_test(final_pre_posMood~Condition)
T_pre_negMood = VirtualArt_data%>%t_test(final_pre_negMood~Condition)
T_pre_STAI = VirtualArt_data%>%t_test(final_pre_STAI~Condition)
T_pre_lone = VirtualArt_data%>%t_test(final_pre_lone~Condition)
T_pre_SAT = VirtualArt_data%>%t_test(final_pre_SAT~Condition)
T_pre_wellbeing = VirtualArt_data%>%t_test(final_pre_wellbeing~Condition)

#ART Interest: Compare the mean of the two sample T test
VirtualArt_data %>%group_by(Condition)%>%get_summary_stats(art_interest_1)
T_art_interest = VirtualArt_data%>%t_test(art_interest_1~Condition)
#ART Visit: Compare the median of the two sample Wilcoxn rank sign test
VirtualArt_data %>%summarise(median(as.numeric(art_visit)))
VirtualArt_data %>%group_by(Condition) %>%summarise(median(as.numeric(art_visit)))
W_art_visit = wilcox.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$art_visit),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$art_visit))
Z_art_visit = qnorm(W_art_visit$p.value/2)#Z for Wilcox test
#ART reading: Compare the median of the two sample Wilcoxn rank sign test
VirtualArt_data %>%summarise(median(as.numeric(art_read)))
VirtualArt_data %>%group_by(Condition) %>%summarise(median(as.numeric(art_read)))
W_art_read = wilcox.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$art_read),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$art_read))
Z_art_read = qnorm(W_art_read$p.value/2) #Z for Wilcox test
#ART looking: Compare the median of the two sample Wilcoxn rank sign test
VirtualArt_data %>%summarise(median(as.numeric(as.factor(art_pic))))
VirtualArt_data %>%group_by(Condition) %>%summarise(median(as.numeric(as.factor(art_pic))))
W_art_pic  = wilcox.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$art_pic),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$art_pic))
Z_art_pic = qnorm(W_art_pic$p.value/2)#Z for Wilcox test
#Art event: ompare the median of the two sample Wilcoxn rank sign test
VirtualArt_data %>%summarise(median(as.numeric(as.factor(art_event))))
VirtualArt_data %>%group_by(Condition) %>%summarise(median(as.numeric(as.factor(art_event))))
W_art_event  = wilcox.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$art_event),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$art_event))
Z_art_event = qnorm(W_art_event$p.value/2)#Z for Wilcox test
#Create output for Wilcox test (Art visit, pic, read, event)
Art_statistics = cbind(
  rbind("interest","visit", "pic", "read", "event"),
  rbind("T-test","Wicoxon", "Wicoxon", "Wicoxon", "Wicoxon"),
  rbind(T_art_interest$statistic,Z_art_visit,Z_art_pic,Z_art_read,Z_art_event),
  rbind(T_art_interest$p.value,W_art_visit$p.value,W_art_pic$p.value,W_art_read$p.value,W_art_event$p.value)
)
colnames(Art_statistics) = c("Variable","Test", "statistic", "p.value")


####__lockdown####
VirtualArt_data$lock_people_1 = as.factor(VirtualArt_data$lock_people_1)
#times spent in lockdown
Lock_days = VirtualArt_data%>%get_summary_stats(lock_days_1)
VirtualArt_data%>%group_by(Condition)%>%get_summary_stats(lock_days_1)
#Compute median for the number of people that particpants spent the lockdown with
VirtualArt_data %>%group_by(Condition) %>%drop_na(lock_people_1)%>%summarise(median(as.numeric(lock_people_1)))

####__lockdown nationality####
LockDown_loc = as.data.frame(table(VirtualArt_data$location))
Continent_loc = c("Europe",
                  "America",
                  "Africa",
                  "Europe",
                  "Europe",
                  "Europe",
                  "Europe",
                  "Europe",
                  "Europe",
                  "America")

LockDown_loc_continent= cbind(LockDown_loc,Continent = Continent_loc)

#frequency of locations of lockdown in the study
N_Europe_nat_loc = sum(LockDown_loc_continent[LockDown_loc_continent$Continent == "Europe","Freq"])
P_Europe_nat_loc = N_Europe_nat_loc/sum(LockDown_loc_continent$Freq)*100
N_Asia_nat_loc = sum(LockDown_loc_continent[LockDown_loc_continent$Continent == "Asia","Freq"])
P_Asia_nat_loc = N_Asia_nat_loc/sum(LockDown_loc_continent$Freq) *100
N_America_nat_loc = sum(LockDown_loc_continent[LockDown_loc_continent$Continent == "America","Freq"])
P_America_nat_loc = N_America_nat_loc/sum(LockDown_loc_continent$Freq) *100
N_Africa_nat_loc = sum(LockDown_loc_continent[LockDown_loc_continent$Continent == "Africa","Freq"])
P_Africa_nat_loc  = N_Africa_nat_loc/sum(LockDown_loc_continent$Freq) *100

nationality_freq_loc = c(N_Europe_nat_loc = N_Europe_nat_loc,
                         P_Europe_nat_loc = P_Europe_nat_loc,
                         N_Asia_nat_loc = N_Asia_nat_loc,
                         P_Asia_nat_loc = P_Asia_nat_loc,
                         N_America_nat_loc = N_America_nat_loc,
                         P_America_nat_loc = P_America_nat_loc,
                         N_Africa_nat_loc = N_Africa_nat_loc,
                         P_Africa_nat_loc = P_Africa_nat_loc)

#Test the assumption that the two random sample (allocated to conditions) are drawn from the same population 
LockDownTable_1= as.data.frame(table(VirtualArt_data$lock_type))
LockDownTable_1%>%select(Freq)/sum(LockDownTable_1%>%select(Freq))*100
LockDownTable = as.data.frame(table(VirtualArt_data$lock_type, VirtualArt_data$Condition))
LockDownTable%>%filter(Var2==1)%>%select(Freq)/sum(LockDownTable%>%filter(Var2==1)%>%select(Freq))*100
LockDownTable%>%filter(Var2==2)%>%select(Freq)/sum(LockDownTable%>%filter(Var2==2)%>%select(Freq))*100
Chi_Lockdown = chisq.test(rbind(LockDownTable[LockDownTable$Var2 == 1,3],LockDownTable[LockDownTable$Var2 == 2,3]))
# Warning message:
# In chisq.test(rbind(LockDownTable[LockDownTable$Var2 == 1, 3], LockDownTable[LockDownTable$Var2 ==  :Chi-squared approximation may be incorrect

#Test the assumption that the average days that participant spent on lockdown between participants (allocated to conditions) was the same
VirtualArt_data %>%group_by(Condition) %>%get_summary_stats(lock_days_1)
T_lock_days = t.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$lock_days_1),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$lock_days_1))
#Compare the median for the number of people that particpants spent the lockdown with per group
VirtualArt_data %>%summarise(median(as.numeric((lock_people_1)), na.rm = T))
VirtualArt_data %>%group_by(Condition) %>%summarise(median(as.numeric((lock_people_1)), na.rm = T))
W_lock_people  = wilcox.test(as.numeric(VirtualArt_data[VirtualArt_data$Condition == 1, ]$lock_people_1),as.numeric(VirtualArt_data[VirtualArt_data$Condition == 2, ]$lock_people_1))
Z_lock_people = qnorm(W_lock_people$p.value/2)#Z for Wilcox test

Lockdown = cbind(
  rbind("type","days", "people"),
  rbind("Chi-square","T-test", "Wicoxon"),
  rbind(sprintf("%s (%s)",Chi_Lockdown$statistic,Chi_Lockdown$parameter),T_lock_days$statistic,Z_lock_people),
  rbind(Chi_Lockdown$p.value,T_lock_days$p.value,W_lock_people$p.value) #note that 3 cells are less then 5 and Chi-squared approximation may be incorrect
)
colnames(Lockdown) = c("Variable","Test", "statistic", "p.value")

####__personality####
Personality_descriptives =
  VirtualArt_data %>%
  group_by(Condition) %>%
  summarise(mean(per_Ext), sd(per_Ext),
            mean(per_A), sd(per_A),
            mean(per_Ce), sd(per_Ce),
            mean(per_ES), sd(per_ES),
            mean(per_OE), sd(per_OE)
  )

Personality_descriptives =
  VirtualArt_data %>%
  summarise(mean(per_Ext), sd(per_Ext),
            mean(per_A), sd(per_A),
            mean(per_Ce), sd(per_Ce),
            mean(per_ES), sd(per_ES),
            mean(per_OE), sd(per_OE)
  )


#Compare the mean of the two sample T test

T_per_Ext = VirtualArt_data%>%t_test(per_Ext~Condition)
T_per_A = VirtualArt_data%>%t_test(per_A~Condition)
T_per_Ce = VirtualArt_data%>%t_test(per_Ce~Condition)
T_per_ES = VirtualArt_data%>%t_test(per_ES~Condition)
T_per_OE = VirtualArt_data%>%t_test(per_OE~Condition)

####__Age####
#Compare the mean of the two sample T test
VirtualArt_data  %>% get_summary_stats(age_year)
VirtualArt_data %>%group_by(Condition) %>% get_summary_stats(age_year)
T_age = VirtualArt_data%>%t_test(age_year~Condition)

####_Supplementary Table 2####
####__aesthetic evaluation####
VirtualArt_data%>%
  group_by(Condition)%>%
  get_summary_stats(art_experience_1,art_experience_2,art_experience_3,art_good_1)

####__cognitive emotional terms####
VirtualArt_data_g_emotion = VirtualArt_data%>%
  select(ResponseId,Condition,starts_with("art_14"),starts_with("art_87"))%>%
  group_by(Condition)%>%
  get_summary_stats(starts_with("art_14"),starts_with("art_87"))%>%
  select(variable,n,Condition,mean,ci)%>%
  mutate(variable = as.factor(variable))%>%
  arrange(reorder(variable,-mean))

VirtualArt_data_g_emotion$Condition =  factor(VirtualArt_data_g_emotion$Condition, levels = c("1", "2"),
                                              labels = c("Water-lillies", "Bento"))
write_csv(VirtualArt_data_g_emotion, sprintf("%s/%s/02_emotionCognitive_csv.csv",wdOA, wdOA_output))

#---------------------------------------------------------------------------
#save for vizualization
write_csv(VirtualArt_data,sprintf("%s/05_images/processedData/01_VirtualArt_data.csv",wdNOA))
write_csv(SawArtTable,sprintf("%s/05_images/processedData/01_SawArtTable.csv",wdNOA))
#---------------------------------------------------------------------------
