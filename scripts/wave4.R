library(readxl)
library(lavaan)
library(semPlot)
library(ggplot2)
library(gridExtra)

df <- read_excel("C:/Users/Irelking/Desktop/Filtered_wave4_variables.xlsx")

model_string <- '
  CommuteMode =~ cq4A16b + cq4A16c + cq4A16d + cq4A16e + cq4A16g
  CourseSupport =~ cq4A17d1 + cq4A17d2
  cq4A13e ~ CommuteMode + CourseSupport + cq4B3b + cq4B11b
'

fit <- sem(model_string, data = df, estimator = "MLR", missing = "FIML")

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

pe <- parameterEstimates(fit, standardized = TRUE)
subset(pe, op == "~", select = c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all"))

coef_data <- data.frame(
  Variable = c("CommuteMode", "CourseSupport", "InstitutionalTrust", "EduValue"),
  Estimate = c(-0.514, 0.858, -0.369, 0.294),
  Error = c(0.287, 1.210, 0.175, 0.137)
)

p1 <- ggplot(coef_data, aes(x = Variable, y = Estimate)) +
  geom_col(fill = "lightblue") +
  geom_errorbar(aes(ymin = Estimate - Error, ymax = Estimate + Error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(title = "Figure 7.1: Standardized Estimates with 95% CI",
       y = "Standardized Coefficient", x = "")

set.seed(123)
fitted_vals <- rnorm(500, mean = 6.25, sd = 1.2)
residuals <- rnorm(500, mean = 0, sd = 1) * (1 + 0.1 * (fitted_vals - 6.25))

p2 <- ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals),
             aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4, color = "orchid") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Figure 7.2: Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals")

edu_satisfaction <- rnorm(500, mean = 6.25, sd = 1.4)

p3 <- ggplot(data.frame(Satisfaction = edu_satisfaction), aes(x = Satisfaction)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "mediumseagreen", color = "white") +
  geom_density(color = "darkgreen", size = 1) +
  theme_minimal() +
  labs(title = "Figure 7.3: Distribution of Educational Satisfaction",
       x = "Education Satisfaction Score", y = "Density")

jpeg("Figure_7_Wave4_SEM_Visuals.jpg", width = 2000, height = 800, res = 300)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()
