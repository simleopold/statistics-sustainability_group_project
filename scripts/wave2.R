# Install necessary packages if not already installed
install.packages("haven")     # For reading SPSS files
install.packages("tidyverse") # For data wrangling
install.packages("dplyr")     # For selecting variables
install.packages("ggplot2")   # For visualizations

# Load libraries
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(corrplot)
library(lavaan)

# Load GUI Data
df_gui <- read.csv("/Users/fuxinyu/Desktop/wave2data/GUI_Data_ChildCohortWave2.csv")

# Load XGUI Data
df_xgui <- read.csv("/Users/fuxinyu/Desktop/wave2data/XGUI_Data_ChildCohortWave2.csv")

# Select relevant variables
df_gui_selected <- df_gui %>%
  select(
    vrlsse,       #reading performance
    napct, nals,   # Math Performance (Target Variable)
    
    #Child’s Behavior & Well-being
    p2q32b, p2q32c, p2q32d, pc2b1, pc2b2, pc2b6, 
    
    #Parental Influence
    p2q12a, p2q12b, p2q12c, p2q12d, p2q12e, p2q12f, p2q12g, p2q12h, p2q12i,
    p2q28f, w2partner, p1empw2, pc2b23, 
    
    #Educational & School Environment
    p2q26a, p2q26b, p2q26d, p2q26e, p2q26f, p2q26g, p2q26h, p2q26i, p2q26j,
    p2q6, p2q31a, p2q31b, p2q31c, p2q31d,  
    
    #Social & Family Factors
    pc2e5a, pc2e5b, pc2e5c, pc2e8a, w2hsdclass, cq2q2t, pc2g34, pc2g36, w2equivinc, w2eincquin, w2eincdec
  )

# Remove variables with >50% missing
df_gui_selected <- df_gui_selected %>% select(where(~ mean(is.na(.)) < 0.5))

# Impute missing values
df_gui_selected <- df_gui_selected %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(!is.na(nals), !is.na(vrlsse))  # ensure outcome variables complete

# Multiple regression model
model_math <- lm(nals ~ ., data = df_gui_selected)
summary(model_math)

# Figure 1.1: Coefficients for Math Performance (nals)
model_math <- lm(nals ~ ., data = df_gui_selected %>% select(-vrlsse))
tidy_math <- tidy(model_math, conf.int = TRUE)

ggplot(tidy_math[-1, ], aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  labs(
    title = "Figure 1.1: Coefficient Estimates for Math Scores (nals)",
    x = "Estimate", y = "Variable"
  ) +
  theme_minimal(base_size = 14)

# Figure 1.2: Coefficients for Reading Performance (vrlsse)
model_reading <- lm(vrlsse ~ ., data = df_gui_selected %>% select(-nals))
tidy_reading <- tidy(model_reading, conf.int = TRUE)

ggplot(tidy_reading[-1, ], aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkred", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  labs(
    title = "Figure 1.2: Coefficient Estimates for Reading Scores (vrlsse)",
    x = "Estimate", y = "Variable"
  ) +
  theme_minimal(base_size = 14)

# Figure 2: Residuals vs Fitted (Math Model)
model_math <- lm(nals ~ ., data = df_gui_selected)
df_gui_selected$resid <- residuals(model_math)
df_gui_selected$fitted <- fitted(model_math)

ggplot(df_gui_selected, aes(x = fitted, y = resid)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Figure 2: Residuals vs Fitted Values (Math Model)",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Figure 3: Distribution of Math Scores
ggplot(df_gui_selected, aes(x = nals)) +
  geom_histogram(fill = "darkgreen", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Figure 3: Distribution of Math Scores (nals)",
       x = "Math Score (%)", y = "Number of Students") +
  theme_minimal()

# SEM: No latent variables

model_sem2 <- '
  behavior =~ p2q32b + p2q32c + p2q32d
  parental_support =~ pc2e5a + pc2e5b + pc2e5c
  school_quality =~ p2q12a + p2q12b + p2q12c
  teacher_env =~ p2q31a + p2q31b + p2q31c

  nals ~ behavior + parental_support + school_quality + teacher_env
  vrlsse ~ behavior + parental_support + school_quality + teacher_env
'

fit_sem2 <- sem(model_sem2, data = df_gui_selected, 
                fixed.x = FALSE, 
                do.fit = TRUE, 
                warn = TRUE)

summary(fit_sem2, fit.measures = TRUE, standardized = TRUE)


# Install and load semPlot
if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
library(semPlot)

# Generate SEM path diagram
semPaths(
  fit_sem,
  whatLabels = "std",   # Show standardized coefficients
  layout = "tree",      # Layout style
  edge.label.cex = 0.8, # Label size
  fade = FALSE,
  residuals = FALSE,
  exoCov = FALSE,
  intercepts = FALSE,
  title = FALSE
)
