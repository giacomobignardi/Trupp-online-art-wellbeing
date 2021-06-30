# Author: Giacomo Bignardi
# Date: 12 04 2021
#
# Script to check differences in MANOVAs results with or without outliers
# Program: Compare Manovas----------------------------------------------------------------------------------------
# Load Libraries 
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


load(sprintf("%s/%s/03_MANOVA_results.Rdata", wdOA,wdOA_output))
load(sprintf("%s/%s/04_MANOVA_outliers_results.Rdata", wdOA,wdOA_output))


#visually inspect differences
#add the p valuev for the stepdown analysis with the the outlier-in version (p_adj_out) in the main MANOVA
Ancova_RoyBergamn_significance$p_adj_out = Ancova_RoyBergamn_significance_outliers$p_adj
Anova_univariate_significance$p_adj_out = Anova_univariate_significance_outliers$p_adj

# excluding outliers did not impacted the results. 
# The only difference was obsereved in the effect of negative mood as a covariate on loneliness, 
# which droped from p = .032 to p = .067 