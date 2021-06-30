# #Author: Giacomo Bignardi
# #Date: 29/12/2020
# #
# #
# #
# #Description: 
# #tidy the Online Art Data words xlx dataset for further analysis 
# #create summary scores for DVs and other variables
# #cope with technical errors
# #Program: Tidy_xlx------------------------------------------------------------------------------------------------------------------------------
# 
# #load packages
# library(readxl)
# library(tidyverse)
# library(tidylog)
# library(rstatix)
# library(psych)
# 
# #clean working enviroment 
# rm(list = ls())
# 
# #set Open Access working directories
# wdOA = getwd()
# wdOA_scripts = "02_scripts"
# wdOA_output = "03_outputs/processedData"
# 
# #set not Open Access working directories
# wdNOA = substr(
#   getwd(),
#   0,
#   nchar(getwd())-nchar("04_analysis_OA")-1
# )
# wdNOA_Data = "03_rawData/private"
# wdNOA_ImageOutput = "05_images/image/processedData"
# 
# #load data_frame
# VirtualArt_data  = read_excel(sprintf("%s/%s/Online Art Data words (with coding).xlsx", wdNOA,wdNOA_Data))
#
# #####TIDY####
# #remove
# VirtualArt_data = VirtualArt_data[3:105,] #first two row contain irrelevant info and last last row scontains only NA
# 
# 
# #rename/tidy
# VirtualArt_data = VirtualArt_data%>%
#   mutate(age_year = 2020 - as.numeric(age))%>%
#   rename("Duration" = `Duration (in seconds)`)%>%
#   rename(Time_control = `control_online_time_Page Submit...167`)%>%
#   rename(Time_art = `time_art_online_Page Submit`)
# #Grouping cultural experiences (1:art 2:non_art)
# VirtualArt_data$experience = as.numeric(ifelse(!is.na(VirtualArt_data[,"time_art_online_First Click"]), 1,2))
# VirtualArt_data$experience = factor(VirtualArt_data$experience, labels = c("art",'non_art'))
# #create a colum with the total time spent in each of the cultural experiences
# VirtualArt_data$time_exc = 
#   ifelse(is.na(as.numeric(VirtualArt_data$Time_art)), 0, as.numeric(VirtualArt_data$Time_art)) + 
#   ifelse(is.na(as.numeric(VirtualArt_data$Time_control)), 0, as.numeric(VirtualArt_data$Time_control))
# 
# ####_Experimental Error: start####--------------------------------------------------------------------------------------------------
# #Lonliness: deal wiht NA  (particpants were mistakenly allowed to leave the loneliness marker for the pre- conditon on the starting position without clicking it)
# #assumption: marker left to the starting postiton is equal to the starting value
# #convert to numeric 
# VirtualArt_data$pre_lone_1_1 = as.numeric(VirtualArt_data$pre_lone_1_1)
# VirtualArt_data$pre_lone_2_1 = as.numeric(VirtualArt_data$pre_lone_2_1)
# VirtualArt_data$pre_lone_3_1 = as.numeric(VirtualArt_data$pre_lone_3_1)
# VirtualArt_data$pre_lone_4_1 = as.numeric(VirtualArt_data$pre_lone_4_1)
# VirtualArt_data$pre_lone_5_1 = as.numeric(VirtualArt_data$pre_lone_5_1)
# VirtualArt_data$pre_lone_6_1 = as.numeric(VirtualArt_data$pre_lone_6_1)
# 
# which(is.na(VirtualArt_data$pre_lone_1_1 + VirtualArt_data$pre_lone_2_1 + 
#   VirtualArt_data$pre_lone_3_1 + VirtualArt_data$pre_lone_4_1 +
#   VirtualArt_data$pre_lone_5_1 + VirtualArt_data$pre_lone_6_1))
# #n of no input from complete  pre_lone
# length(c(which(is.na(VirtualArt_data$pre_lone_1_1)),which(is.na(VirtualArt_data$pre_lone_2_1)),
#          which(is.na(VirtualArt_data$pre_lone_3_1)),which(is.na(VirtualArt_data$pre_lone_4_1)),
#          which(is.na(VirtualArt_data$pre_lone_5_1)),which(is.na(VirtualArt_data$pre_lone_6_1))))
# #7 inputs were left to the starting position
# #Manipulate NA (no forced input. assumption indiviudal with only one NA they did not moved the slider, that is NA = 4)
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_1_1))),"pre_lone_1_1"] = 4
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_2_1))),"pre_lone_2_1"] = 4
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_3_1))),"pre_lone_3_1"] = 4
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_4_1))),"pre_lone_4_1"] = 4
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_5_1))),"pre_lone_5_1"] = 4
# VirtualArt_data[c(which(is.na(VirtualArt_data$pre_lone_6_1))),"pre_lone_6_1"] = 4
# ##_Experimental Error:end--------------------------------------------------------------------------------------------------
# 
# ####SUMMARY Score####
# #Personality
# #rename TIPI (qualtrics error)
# VirtualArt_data = VirtualArt_data %>% 
#   rename(TIPI_2 = "TIPI_4",
#          TIPI_3 = "TIPI_5",                         
#          TIPI_4 = "TIPI_6",                        
#          TIPI_5 = "TIPI_7",                         
#          TIPI_6 = "TIPI_8",                         
#          TIPI_7 = "TIPI_9",                        
#          TIPI_8 = "TIPI_10",                         
#          TIPI_9 = "TIPI_11",                         
#          TIPI_10 = "TIPI_12")
# 
# #sum score TIPI (Gosling et al., 2003)
# VirtualArt_data$per_Ext = (as.numeric(VirtualArt_data$TIPI_1) + (8-as.numeric(VirtualArt_data$TIPI_6)))/2
# VirtualArt_data$per_A = (as.numeric(VirtualArt_data$TIPI_7) + (8-as.numeric(VirtualArt_data$TIPI_2)))/2
# VirtualArt_data$per_Ce = (as.numeric(VirtualArt_data$TIPI_3) + (8-as.numeric(VirtualArt_data$TIPI_8)))/2
# VirtualArt_data$per_ES = (as.numeric(VirtualArt_data$TIPI_9) + (8-as.numeric(VirtualArt_data$TIPI_4)))/2
# VirtualArt_data$per_OE = (as.numeric(VirtualArt_data$TIPI_5) + (8-as.numeric(VirtualArt_data$TIPI_10)))/2
# 
# #Calculate final scores
# #Loneliness (De Jong Gierveld & Van Tilburg, 2010)
# VirtualArt_data$final_pre_lone = (as.numeric(VirtualArt_data$pre_lone_1_1) + 
#                                     as.numeric(VirtualArt_data$pre_lone_2_1) + 
#                                     as.numeric(VirtualArt_data$pre_lone_3_1) + 
#                                     (8-as.numeric(VirtualArt_data$pre_lone_4_1)) + 
#                                     (8-as.numeric(VirtualArt_data$pre_lone_5_1)) + 
#                                     (8-as.numeric(VirtualArt_data$pre_lone_6_1)))/6
# 
# VirtualArt_data$final_post_lone = (as.numeric(VirtualArt_data$post_lone_1_1...148) + 
#                                      as.numeric(VirtualArt_data$post_lone_1_1...149) + 
#                                      as.numeric(VirtualArt_data$post_lone_3_1) + 
#                                      (8-as.numeric(VirtualArt_data$post_lone_4_1)) + 
#                                      (8-as.numeric(VirtualArt_data$`post_lone-5_1`)) + 
#                                      (8-as.numeric(VirtualArt_data$post_lone_6_1)))/6
# #State-Trait Anxiety Inventory (STAI; Marteau & Bekker, 1992)
# VirtualArt_data$final_pre_STAI = ((8-as.numeric(VirtualArt_data$Pre_STAI_1)) + 
#                                     as.numeric(VirtualArt_data$Pre_STAI_2) + 
#                                     as.numeric(VirtualArt_data$Pre_STAI_3) + 
#                                     (8-as.numeric(VirtualArt_data$Pre_STAI_4)) + 
#                                     (8-as.numeric(VirtualArt_data$Pre_STAI_5)) + 
#                                     as.numeric(VirtualArt_data$Pre_STAI_6))/6
# 
# VirtualArt_data$final_post_STAI = ((8-as.numeric(VirtualArt_data$post_STAI_1)) + 
#                                      as.numeric(VirtualArt_data$post_STAI_2) + 
#                                      as.numeric(VirtualArt_data$post_STAI_3) + 
#                                      (8-as.numeric(VirtualArt_data$post_STAI_4)) + 
#                                      (8-as.numeric(VirtualArt_data$post_STAI_5)) + 
#                                      as.numeric(VirtualArt_data$post_STAI_6))/6
# 
# 
# #SAT(Diener et al., 1985)
# VirtualArt_data$final_pre_SAT = (as.numeric(VirtualArt_data$Pre_sat_1) + 
#                                    as.numeric(VirtualArt_data$Pre_sat_2) + 
#                                    as.numeric(VirtualArt_data$Pre_sat_3) + 
#                                    as.numeric(VirtualArt_data$Pre_sat_4) + 
#                                    as.numeric(VirtualArt_data$Pre_sat_5))/5
# VirtualArt_data$final_post_SAT = (as.numeric(VirtualArt_data$post_sat_1) + 
#                                     as.numeric(VirtualArt_data$post_sat_2) + 
#                                     as.numeric(VirtualArt_data$post_sat_3) + 
#                                     as.numeric(VirtualArt_data$post_sat_4) + 
#                                     as.numeric(VirtualArt_data$post_sat_5))/5
# 
# #Subjective_WellBeing (Tinkler & Hicks, 2011)
# VirtualArt_data$final_pre_wellbeing = (as.numeric(VirtualArt_data$pre_wellbeing_1) + 
#                                          as.numeric(VirtualArt_data$pre_wellbeing_2) + 
#                                          (8-as.numeric(VirtualArt_data$pre_wellbeing_3)) + 
#                                          as.numeric(VirtualArt_data$pre_wellbeing_4))/4 
# VirtualArt_data$final_post_wellbeing = (as.numeric(VirtualArt_data$post_wellbeing_1) + 
#                                           as.numeric(VirtualArt_data$post_wellbeing_2) + 
#                                           (8-as.numeric(VirtualArt_data$post_wellbeing_3)) + 
#                                           as.numeric(VirtualArt_data$post_wellbeing_4))/4 
# 
# #Rename Mood
# VirtualArt_data = VirtualArt_data%>%
#   rename(final_pre_posMood = "pre_mood_1",
#          final_pre_negMood = "pre_mood_2",
#          final_post_posMood = "post_mood_1",
#          final_post_negMood = "post_mood_2")
# 
# #Convert post self assesment
# VirtualArt_data$self_assess_1 = as.numeric(VirtualArt_data$self_assess_1)
# VirtualArt_data$self_assess_2 = as.numeric(VirtualArt_data$self_assess_2)
# VirtualArt_data$self_assess_3 = as.numeric(VirtualArt_data$self_assess_3)
# VirtualArt_data$self_assess_4 = as.numeric(VirtualArt_data$self_assess_4)
# 
# #compute prepost changes
# VirtualArt_data$SAT_change = as.numeric(VirtualArt_data$final_post_SAT) - as.numeric(VirtualArt_data$final_pre_SAT)
# VirtualArt_data$Wellbeing_change = as.numeric(VirtualArt_data$final_post_wellbeing) - as.numeric(VirtualArt_data$final_pre_wellbeing)
# VirtualArt_data$positive_mood_change = as.numeric(VirtualArt_data$final_post_posMood) - as.numeric(VirtualArt_data$final_pre_posMood)
# VirtualArt_data$Lone_change = as.numeric(VirtualArt_data$final_post_lone) - as.numeric(VirtualArt_data$final_pre_lone)
# VirtualArt_data$STAI_change = as.numeric(VirtualArt_data$final_post_STAI) - as.numeric(VirtualArt_data$final_pre_STAI)
# VirtualArt_data$negative_mood_change = as.numeric(VirtualArt_data$final_post_negMood) - as.numeric(VirtualArt_data$final_pre_negMood)
# 
# VirtualArt_data = VirtualArt_data%>%
#   select(-c(
#     "IPAddress",
#     "DistributionChannel",
#     "RecipientLastName",                   
#     "RecipientFirstName",                  
#     "RecipientEmail",                     
#     "ExternalReference",
#     "LocationLatitude",                     
#     "LocationLongitude")
#     )
# 
# 
# #save the tidy df as csv
# write_csv(VirtualArt_data,sprintf("%s/%s/01_VirtualArt_data_.csv",wdOA,wdOA_output))