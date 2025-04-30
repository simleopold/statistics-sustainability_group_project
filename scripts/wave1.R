##############################################
##############################################
#
#this script is dedicated to the statistics and sustainability group project and wave 1 analysis
#
#############################################
#############################################

library(haven)
library(mice)
library(lavaan)
library(ggplot2)
library(dplyr)
library(car)


#loading data
wave1 <- read_dta('/Users/icesim/Desktop/tcd/cours/ss project/data/Child Cohort Wave 1/0020-01 GUI Child Cohort Wave 1_Data/9 Year Cohort Data/Stata/GUI Data_9YearCohort.dta')
wave2 <- read_dta('/Users/icesim/Desktop/tcd/cours/ss project/data/Child Cohort Wave 2/0020-02 GUI Child Cohort Wave 2_Data/13 year cohort data/Stata/GUI Data_ChildCohortWave2.dta')
wave3 <- read_dta('/Users/icesim/Desktop/tcd/cours/ss project/data/Child Cohort Wave 3 revised/0020-03 GUI Child Cohort Wave 3_Data revised/Stata/0020-03_GUI_Data_ChildCohortWave3_V1.3.dta')
wave4 <- read_dta('/Users/icesim/Desktop/tcd/cours/ss project/data/Child Cohort Wave 4/0020-04 GUI Child Cohort Wave 4_Data/STATA/0020-04_GUI_Data_ChildCohortWave4.dta')

#select covariates 
wave1_cleanv2 <- wave1[,c("p14a","p14b","p14c","mma5pesp1","MMA4","MMJ12",
                          "EIncDec","Pianta_conflict_PCG", "MMJ18","MMJ8",
                          "MMJ10","MMJ11","MMD12","p48a","p48b","TS19a",
                          "TS19b", "TS19c","TS19e","mathsls","readingls","p37a","p37b","p37f")]

ggplot(wave1_cleanv2) +  
  geom_histogram(aes(x = mathsls, fill = "Maths"), bins = 20, alpha = 0.8, color = "black") +  
  geom_histogram(aes(x = readingls, fill = "Reading"), bins = 20, alpha = 0.6, color = "black") +  
  scale_fill_manual(values = c("Maths" = "blue", "Reading" = "pink")) + 
  labs(x = "Score",fill = "Subject") + 
  theme_minimal()

#data cleaning
wave1_cleanv2$p14a[wave1_cleanv2$p14a %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p14a), decreasing = TRUE)[1]))
wave1_cleanv2$p14a[is.na(wave1_cleanv2$p14a)] <- mode_value

wave1_cleanv2$p14b[wave1_cleanv2$p14b %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p14b), decreasing = TRUE)[1]))
wave1_cleanv2$p14b[is.na(wave1_cleanv2$p14b)] <- mode_value

wave1_cleanv2$p14c[wave1_cleanv2$p14c %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p14c), decreasing = TRUE)[1]))
wave1_cleanv2$p14c[is.na(wave1_cleanv2$p14c)] <- mode_value

wave1_cleanv2$MMJ12[wave1_cleanv2$MMJ12 %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$MMJ12), decreasing = TRUE)[1]))
wave1_cleanv2$MMJ12[is.na(wave1_cleanv2$MMJ12)] <- mode_value

mode_value <- as.numeric(names(sort(table(wave1_cleanv2$EIncDec), decreasing = TRUE)[1]))
wave1_cleanv2$EIncDec[is.na(wave1_cleanv2$EIncDec)] <- mode_value

mode_value <- as.numeric(names(sort(table(wave1_cleanv2$Pianta_conflict_PCG), decreasing = TRUE)[1]))
wave1_cleanv2$Pianta_conflict_PCG[is.na(wave1_cleanv2$Pianta_conflict_PCG)] <- mode_value

wave1_cleanv2$MMJ11[wave1_cleanv2$MMJ11 %in% c(98, 99)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$MMJ11), decreasing = TRUE)[1]))
wave1_cleanv2$MMJ11[is.na(wave1_cleanv2$MMJ11)] <- mode_value

wave1_cleanv2$p48a[wave1_cleanv2$p48a %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p48a), decreasing = TRUE)[1]))
wave1_cleanv2$p48a[is.na(wave1_cleanv2$p48a)] <- mode_value

wave1_cleanv2$p48b[wave1_cleanv2$p48b %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p48b), decreasing = TRUE)[1]))
wave1_cleanv2$p48b[is.na(wave1_cleanv2$p48b)] <- mode_value

wave1_cleanv2$TS19a[wave1_cleanv2$TS19a %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$TS19a), decreasing = TRUE)[1]))
wave1_cleanv2$TS19a[is.na(wave1_cleanv2$TS19a)] <- mode_value

wave1_cleanv2$TS19b[wave1_cleanv2$TS19b %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$TS19b), decreasing = TRUE)[1]))
wave1_cleanv2$TS19b[is.na(wave1_cleanv2$TS19b)] <- mode_value

wave1_cleanv2$TS19c[wave1_cleanv2$TS19c %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$TS19c), decreasing = TRUE)[1]))
wave1_cleanv2$TS19c[is.na(wave1_cleanv2$TS19c)] <- mode_value

wave1_cleanv2$TS19e[wave1_cleanv2$TS19e %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$TS19e), decreasing = TRUE)[1]))
wave1_cleanv2$TS19e[is.na(wave1_cleanv2$TS19e)] <- mode_value

wave1_cleanv2$p37a[wave1_cleanv2$p37a %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p37a), decreasing = TRUE)[1]))
wave1_cleanv2$p37a[is.na(wave1_cleanv2$p37a)] <- mode_value

wave1_cleanv2$p37b[wave1_cleanv2$p37b %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p37b), decreasing = TRUE)[1]))
wave1_cleanv2$p37b[is.na(wave1_cleanv2$p37b)] <- mode_value

wave1_cleanv2$p37f[wave1_cleanv2$p37f %in% c(8, 9)] <- NA
mode_value <- as.numeric(names(sort(table(wave1_cleanv2$p37f), decreasing = TRUE)[1]))
wave1_cleanv2$p37f[is.na(wave1_cleanv2$p37f)] <- mode_value

mode_value <- as.numeric(names(sort(table(wave1_cleanv2$mathsls), decreasing = TRUE)[1]))
wave1_cleanv2$mathsls[is.na(wave1_cleanv2$mathsls)] <- mode_value

mode_value <- as.numeric(names(sort(table(wave1_cleanv2$readingls), decreasing = TRUE)[1]))
wave1_cleanv2$readingls[is.na(wave1_cleanv2$readingls)] <- mode_value

###################################################################
#visualisation and possible interactions
#################################################################

#income vs mathsls 
ggplot(data = wave1_cleanv2, aes(x = EIncDec, y = mathsls, fill = EIncDec))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Household income", y = "Drumcondra Maths Test Score")

ggplot(data = wave1_cleanv2, aes(x = MMJ8, y = mathsls, fill =MMJ8))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Absences", y = "Drumcondra Maths Test Score")

#interaction of income and school quality
ggplot(data = wave1_cleanv2, aes(x = wave1_cleanv2$EIncDec, y = wave1_cleanv2$mathsls, fill = wave1_cleanv2$p14c))+
  geom_boxplot() +
  labs(x = "Household Income", y = "Drumcondra Maths Test Score",
       title = "Household Income & School Quality Impact on Maths Scores",
       fill = "School Quality") 

#absences and bullying victims
ggplot(data = wave1_cleanv2, aes(x = as.factor(MMJ8), y = mathsls, fill = as.factor(MMJ18))) +
  geom_boxplot() +
  labs(title = "Absenteeism & Bullying Impact on Maths Scores",
       x = "Number of Absences",
       y = "Maths Score",
       fill = "Bullying Victim") +
  theme_minimal()

#interaction of distance and absenteeism
ggplot(wave1_cleanv2, aes(x = as.factor(MMJ8), y = mathsls, fill = as.factor(MMD12))) +
  geom_boxplot() +
  labs(title = "Maths Scores by Absenteeism and Distance from School",
       x = "Absenteeism Level",
       y = "Maths Score",
       fill = "Distance from School (km)") +
  theme_minimal()

##################################################################
#regression
##################################################################

mod1 <- lm(wave1$mathsls ~ ., data = wave1_cleanv2)
summary(mod1)
test <- step(mod1, direction = "backward")
summary(test)

mod2 <- lm(wave1$readingls ~ ., data = wave1_cleanv2)
summary(mod2)

#check for corelation between variables
vif(test)
vif(mod1)
vif(mod2)

#no VIF > 5 --> no need to remove variables 

##################################################################
#SEM on wave 1 
##################################################################

sem_model <- '
  # Regression equations
  mathsls ~ p14a + MMJ12 + p14b+p14c+MMA4+EIncDec+Pianta_conflict_PCG+
  MMJ18+MMJ8+MMJ10+MMJ11+MMD12+p48a+p48b+TS19a+TS19b+TS19c+TS19e+p37a+p37b+p37f+mma5pesp1
  
  readingls ~ p14a + MMJ12 + p14b+p14c+MMA4+EIncDec+Pianta_conflict_PCG+
  MMJ18+MMJ8+MMJ10+MMJ11+MMD12+p48a+p48b+TS19a+TS19b+TS19c+TS19e+p37a+p37b+p37f+mma5pesp1
  
  # Allow outcomes to correlate
  mathsls ~~ readingls
'
fit <- sem(sem_model, data = wave1)
summary(fit, fit.measures = TRUE, standardized = TRUE)




##########################################################################################
#Used factors instead of numerical values (to see if the results are changing based on how the variables are defined)
##########################################################################################

wave1_cleanv2$p14a <- factor(wave1_cleanv2$p14a, 
                             levels = c(1, 2, 3, 4),  # The numeric values for the levels
                             labels = c("Poor", "Fair", "Good", "Excellent"))  # Make it an ordered factor

# Convert p14b
wave1_cleanv2$p14b <- factor(wave1_cleanv2$p14b, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Poor", "Fair", "Good", "Excellent"))

# Convert p14c
wave1_cleanv2$p14c <- factor(wave1_cleanv2$p14c, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Poor", "Poor/Fair", "Good", "Excellent"))

# Convert mma5pesp1
wave1_cleanv2$mma5pesp1 <- factor(wave1_cleanv2$mma5pesp1, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                  labels = c("Pre-school", "School/Education", 
                                             "At work/training", "Unemployed", 
                                             "Other", "Homemaker", "Retired", 
                                             "Studying", "Other"))

# Convert MMA4
wave1_cleanv2$MMA4 <- factor(wave1_cleanv2$MMA4, 
                             levels = c(1, 2, 3, 4, 5, 6, 7), 
                             labels = c("one", "two", "three", "four", 
                                        "five", "six", "seven"))

# Convert MMJ12
wave1_cleanv2$MMJ12 <- factor(wave1_cleanv2$MMJ12, 
                              levels = c(1, 2, 3, 4, 5, 6, 8, 9), 
                              labels = c("Always/Nearly Always", "Regularly", 
                                         "Now and Again", "Rarely", "Never", 
                                         "Sometimes", "Refusal", "Dontknow"))

# Convert EIncDec
wave1_cleanv2$EIncDec <- factor(wave1_cleanv2$EIncDec, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                labels = c("Lowest", "2nd", "3rd", "4th", "5th", 
                                           "6th", "7th", "8th", "9th", "Highest"))

# Convert Pianta_conflict_PCG
wave1_cleanv2$Pianta_conflict_PCG <- factor(wave1_cleanv2$Pianta_conflict_PCG, 
                                            levels = c(1, 2, 3, 4, 5, 6, 7), 
                                            labels = c("None", "Low", "Moderate", 
                                                       "High", "Severe", "Extremely High", 
                                                       "Other"))

# Convert MMJ18
wave1_cleanv2$MMJ18 <- factor(wave1_cleanv2$MMJ18, 
                              levels = c(1, 2), 
                              labels = c("Yes", "No"))

# Convert MMJ8
wave1_cleanv2$MMJ8 <- factor(wave1_cleanv2$MMJ8, 
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                             labels = c("0 days", "1 - 3 days", "4 to 6 days", 
                                        "7 to 10 days", "11 to 15 days", 
                                        "16 to 20 days", "21 to 30 days", 
                                        "31 to 40 days", "More than 40 days"))

# Convert MMJ10
wave1_cleanv2$MMJ10 <- factor(wave1_cleanv2$MMJ10, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                              labels = c("Never", "Less than once a month", 
                                         "Once a month", "A few times a month or less", 
                                         "Once a week", "A few times a week", 
                                         "Every day", "Several times a day", "Other"))

# Convert MMJ11
wave1_cleanv2$MMJ11 <- factor(wave1_cleanv2$MMJ11, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                              labels = c("0 to 15 minutes", "16 to 30 minutes", 
                                         "31 minutes to less than one hour", 
                                         "1 to less than 1.5 hours", 
                                         "1.5 to 2 hours", "2 to 2.5 hours", 
                                         "2.5 to 3 hours", "3 to 3.5 hours", 
                                         "3.5 to 4 hours", "More than 4 hours"))

# Convert MMD12
wave1_cleanv2$MMD12 <- factor(wave1_cleanv2$MMD12, 
                              levels = c(1, 2, 3, 4, 5, 8, 9), 
                              labels = c("Less than ½mile (1km)", "½ to 1 mile (1-2km)", 
                                         "1-5 miles (2-8km)", "More than 5 miles away (8km)", 
                                         "Unknown", "Refusal", "Dontknow"))

# Convert p48a
wave1_cleanv2$p48a <- factor(wave1_cleanv2$p48a, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Very", "Fairly", "Not Very", "Not at all"))

# Convert p48b
wave1_cleanv2$p48b <- factor(wave1_cleanv2$p48b, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Very", "Fairly", "Not Very", "Not at all"))

# Convert TS19a
wave1_cleanv2$TS19a <- factor(wave1_cleanv2$TS19a, 
                              levels = c(1, 2, 3, 4), 
                              labels = c("Nearly all", "More than half", 
                                         "Less than half", "Only a few"))

# Convert TS19b
wave1_cleanv2$TS19b <- factor(wave1_cleanv2$TS19b, 
                              levels = c(1, 2, 3, 4), 
                              labels = c("Nearly all", "More than half", 
                                         "Less than half", "Only a few")
                              )

# Convert TS19c
wave1_cleanv2$TS19c <- factor(wave1_cleanv2$TS19c, 
                              levels = c(1, 2, 3, 4), 
                              labels = c("Nearly all", "More than half", 
                                         "Less than half", "Only a few") 
                             )

# Convert TS19e
wave1_cleanv2$TS19e <- factor(wave1_cleanv2$TS19e, 
                              levels = c(1, 2, 3, 4), 
                              labels = c("Nearly all", "More than half", 
                                         "Less than half", "Only a few") 
                              )

wave1_cleanv2 <- wave1_cleanv2 %>% select(-Pianta_conflict_PCG)
wave1_cleanv2 <- wave1_cleanv2 %>% select(-readingls)

lm_model <- lm(mathsls ~ ., data = wave1_cleanv2)
summary(lm_model)
AIC(lm_model)
new_mod <- step(lm_model,direction = "backward")
summary(new_mod) #results are consistent with the previous ones (numerical values)
AIC(new_mod)

#############################################################################
#interaction effect 
#############################################################################

#interaction between bullying and absences 
lm_model2 <- lm(mathsls ~ . + MMJ8*MMJ18, data = wave1_cleanv2)
summary(lm_model2)
AIC(lm_model2) #not improving the model 

#distance from school and absences
lm_model3 <- lm(mathsls ~ . + MMJ8*MMD12, data = wave1_cleanv2)
AIC(lm_model3) #not improving

#bullying and ejoying beeing at school
lm_model4 <- lm(mathsls ~ . + TS19a*MMJ18, data = wave1_cleanv2)
AIC(lm_model4)

#enjoy being at school and parental conflict
lm_model5 <- lm(mathsls ~ . + TS19a*Pianta_conflict_PCG, data = wave1_cleanv2)
AIC(lm_model5)

#well behaved in class and parental conflict 
lm_model6 <- lm(mathsls ~ . + TS19b*Pianta_conflict_PCG, data = wave1_cleanv2)
AIC(lm_model6)

AIC(lm_model, lm_model2, lm_model3, lm_model4, lm_model5, lm_model6)
BIC(lm_model, lm_model2, lm_model3, lm_model4, lm_model5, lm_model6)


