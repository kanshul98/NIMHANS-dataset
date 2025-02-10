library(psych)
library(tidyverse)
library(writexl)
library(readxl)
library(ppcor)
getwd()

# we are going to analyze a dataset of n=325. Which include various neuropsychological tests.
# We are going to see how they relate to increasing obesity(BMI) in Indian Adults

# import the dataset nimdata_325

view(nimdata_325)
str(nimdata_325) # explore the structure of the dataset in brief
describe(nimdata_325) #descriptive stats or the dataset in brief

# first we check for normality assumptions
shapiro.test(nimdata_325$moca) # using shapiro tests
shapiro.test(nimdata_325$BMI)
shapiro.test(nimdata_325$cf_tc)
shapiro.test(nimdata_325$cf_ir)
shapiro.test(nimdata_325$cf_dr)
shapiro.test(nimdata_325$animal_names)

# and then a histogram
ggplot(nimdata_325, aes(x = moca)) + # FOR MOCA scores
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, fill = "steelblue", color = "white") +
  geom_density(color = "black") +
  theme_minimal() +
  labs(x = "MOCA Scores", y = "Density", title = "MOCA Histogram and Density Plot")

ggplot(nimdata_325, aes(x = BMI)) + # FOR BMI values
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, fill = "steelblue", color = "white") +
  geom_density(color = "black") +
  theme_minimal() +
  labs(x = "BMI Values", y = "Density", title = "BMI Histogram and Density Plot")


# first we apply a simple correlation between BMI and moca scores
bmi_moca_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, moca, method = "kendall"),
    p_value = cor.test(BMI, moca, method = "kendall")$p.value
  ) # we are using kendall tau here because spearman does not work best when their are multiple tied values
print(bmi_moca_cor)

# Complex figure Test Copy and BMI correlation
bmi_cftc_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_tc, method = "kendall"),
    p_value = cor.test(BMI, cf_tc, method = "kendall")$p.value
  )
print(bmi_cftc_cor)

# Complex figure Immediate Recall and BMI correlation
bmi_cfir_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_ir, method = "kendall"),
    p_value = cor.test(BMI, cf_ir, method = "kendall")$p.value
  )
print(bmi_cfir_cor)

# Complex figure Delayed Recall and BMI correlation
bmi_cfdr_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_dr, method = "kendall"),
    p_value = cor.test(BMI, cf_dr, method = "kendall")$p.value
  )
print(bmi_cfdr_cor)

# Animal Names Test and BMI correlation
bmi_animal_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, animal_names, method = "kendall"),
    p_value = cor.test(BMI, animal_names, method = "kendall")$p.value
  )
print(bmi_animal_cor)



# WE HAVE TO CONSIDER 2 WAYS OF ASSESSING STROOP TASK: 1) COMPOSITE MULTIPLIED Z SCORE, 2) CALCULATE INVERSE EFFICIENCY SCORE (IES)

# 1st method: To assess the stroop test scores, which has two components: Stroop_time and Stroop_errors, we first standardize this score and combine it
nimdata_325 <- nimdata_325 %>% 
  mutate( # scale() is used to calculate Z scores
    stroop_composite = scale(stroop_tt) * scale(stroop_err) # we specifically multiply the data since these two scores are generally uncorrelated, and tie together to increase or decrease accuracy
  )
stroop_bmi_z_corr <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, stroop_composite, method = "spearman"),
    p_value = cor.test(BMI, stroop_composite, method = "spearman")$p.value
  )
print(stroop_bmi_z_corr)

# 2nd method: using Inverse Efficiency Score (IES). Very useful for speed-accuracy trade-off psychological scores
nimdata_325 <- nimdata_325 %>% 
  mutate(stroop_ies = stroop_tt / (1 + stroop_err)) # stroop_ies = stroop inverse efficiency score

ies_stroop_result <- nimdata_325 %>% # now we store the correlation results here
  summarise(
    N = n(),
    correlation = cor(BMI, stroop_ies, method = "spearman"),
    p_value = cor.test(BMI, stroop_ies, method = "spearman")$p.value
  )
print(ies_stroop_result) #print the results



# now we move to nBack tests (1 and 2)
bmi_nback1_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, nback_1, method = "spearman"),
    p_value = cor.test(BMI, nback_1, method = "spearman")$p.value
  )
print(bmi_nback1_cor)

bmi_nback2_cor <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, nback_2, method = "spearman"),
    p_value = cor.test(BMI, nback_2, method = "spearman")$p.value
  )
print(bmi_nback2_cor)



# now we do for Digit Vigilance Test
# We apply the same procedure that we did with Stroop test scores, first we calculcate composite_score via multiplication, second method involves calculating Inverse Efficiency Score (IES)

nimdata_325 <- nimdata_325 %>% 
  mutate( # scale() is used to calculate Z scores
    dvt_composite = scale(dvt_tt) * scale(dvt_err) # we specifically multiply the data since these two scores are generally uncorrelated, and tie together to increase or decrease accuracy
  )
bmi_dvt_z_corr <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, dvt_composite, method = "spearman"),
    p_value = cor.test(BMI, dvt_composite, method = "spearman")$p.value
  )
print(bmi_dvt_z_corr)

# Now we use Inverse Efficiency Score calculation for DVT scores
nimdata_325 <- nimdata_325 %>% 
  mutate(dvt_ies = dvt_tt/(dvt_err + 1))

dvt_ies_result <- nimdata_325 %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, dvt_ies, method = "spearman"),
    p_value = cor.test(BMI, dvt_ies, method = "spearman")$p.value
  )
print(dvt_ies_result)



# So till now we have calculate correlations for all neuropsychological tests with BMI's
# We take the next step and assess **SUBGROUP ANALYSIS BY EDUCATION**

nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, moca, method = "kendall"),
    p_value = cor.test(BMI, moca, method = "kendall")$p.value
  )
# we replace the 'moca' value with different psychological tests to calculate accordingly

# Subgroup Analysis by Education for CF_TC
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_tc, method = "kendall"),
    p_value = cor.test(BMI, cf_tc, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for CF_IR
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_ir, method = "kendall"),
    p_value = cor.test(BMI, cf_ir, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for CF_DR
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_dr, method = "kendall"),
    p_value = cor.test(BMI, cf_dr, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for Animal Names Test
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, animal_names, method = "kendall"),
    p_value = cor.test(BMI, animal_names, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for Stroop Composite Score
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, stroop_composite, method = "kendall"),
    p_value = cor.test(BMI, stroop_composite, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for DVT Composite Score
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, dvt_composite, method = "kendall"),
    p_value = cor.test(BMI, dvt_composite, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for Stroop IES
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, stroop_ies, method = "kendall"),
    p_value = cor.test(BMI, stroop_ies, method = "kendall")$p.value
  )

# Subgroup Analysis by Education for DVT IES
nimdata_325 %>% 
  group_by(edu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, dvt_ies, method = "kendall"),
    p_value = cor.test(BMI, dvt_ies, method = "kendall")$p.value
  )



# We take the next step and assess **SUBGROUP ANALYSIS BY OCCUPATION**
nimdata_325 %>% 
  group_by(occu) %>% 
  summarise(
    N = n(),
    correlation = cor(BMI, cf_tc, method = "kendall"),
    p_value = cor.test(BMI, cf_tc, method = "kendall")$p.value
  )
# we replace the 'moca' value with different psychological tests to calculate accordingly



# WE DO SOME ***PLOTTING AND GRAPHING*** IN THE END
# Since results for normal correlation are insignificant, we do plotting for subgroups of EDUCATION


# first we do plotting between BMI and MOCA for subgroups of Education
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = moca)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "MOCA scores", title = "BMI vs MOCA correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting

# then for cf_tt
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = cf_tc)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "CF_TC scores", title = "BMI vs CF_TC correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting

# then for cf_ir
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = cf_ir)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "CF_IR scores", title = "BMI vs CF_IR correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting


# then for cf_dr
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = cf_dr)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "CF_DR scores", title = "BMI vs CF_DR correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting

# now for animal names test
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = animal_names)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "Animal names scores", title = "BMI vs Animal Names Test correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting

# now for nback_1
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = nback_1)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "Nback_1 scores", title = "BMI vs Nback_1 correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting


# now for nback_2
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = nback_2)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "Nback_2 scores", title = "BMI vs Nback_2 correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting


# now for Stroop IES scores
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = stroop_ies)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "Stroop IES scores", title = "BMI vs Stroop_IES correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting


# now for DVT IES scores
nimdata_325 %>% 
  ggplot(aes(x = BMI, y = dvt_ies)) +
  geom_point() + #for scatterplots
  geom_smooth(color = "blue", method = lm, se = FALSE) + # for density line
  theme_minimal() +
  labs(x = "BMI", y = "DVT IES scores", title = "BMI vs DVT_IES correlation for subgroups of Education") +
  facet_wrap(~ edu, scale = "free") # facet_wrap is like subgroup analysis but for plotting





















