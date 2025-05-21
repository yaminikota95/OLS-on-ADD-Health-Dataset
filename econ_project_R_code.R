library(tidyverse)
library(dplyr)
library(haven)       # For reading .xpt files
library(ggplot2)
library(tidyr)
library(ggcorrplot)
library(car)         # VIF
library(lmtest)      # Durbin-Watson, Breusch-Pagan
library(sandwich)    # Robust SE
library(GGally)      # Correlation plots
library(tseries)

# Load the data files
pwave <- read_xpt("C:/Users/Yamini/Downloads/dataverse_files/pwave5.xpt")
pcardio <- read_xpt("C:/Users/Yamini/Downloads/dataverse_files/pcardio5.xpt")
pdemo <- read_xpt("C:/Users/Yamini/Downloads/dataverse_files/pdemo5.xpt")
panthro <- read_xpt("C:/Users/Yamini/Downloads/dataverse_files/panthro5.xpt")


# merge datasets using the common identifier (aid)
df <- reduce(list(pwave, pcardio, pdemo,panthro), full_join, by = "AID")

df_clean <- df %>%
  transmute(
    sbp = H5SBP,                   # Systolic blood pressure average
    age = H5AGE,                     # Age in years
    bmi = H5BMI,                     #bmi
    smoking_status = H5Q015,         # Smoking status (yes/no)
    alcohol_use = H5TO11,         # Alcohol use (yes/no)
    mild_cardio = H5ID24,               # bike/dance/skateboard/yard work/hike for exercise past 7 days
    aerobic = H5ID25,               # roller blade/ skate/ski/ aerobics for exercise past 7 days
    gym = H5ID26,               # gymnastics/ strength training for exercise past 7 days
    sport = H5ID27,               # individual sport for exercise past 7 days
    golf = H5ID28,               # GOLF/FISH/BOWL/BASEBALL for exercise past 7 days
    walk = H5ID29,               # walk for exercise past 7 days
    income = H5EC2,                # Income 
    sex = H5Q011,                    # Sex  1-male 2-female
    hypertensive_meds = H5AHT       # Hypertension medication use
  ) %>%
  drop_na()

df_clean$sex[df_clean$sex == 2] <- 0    # male-1 female-0
df_clean <- df_clean[!(df_clean$hypertensive_meds %in% c(94, 99)), ] #removing entries with invalid or missing data
df_clean <- df_clean[df_clean$sbp < 9996, ]   #removing entries where sbp not measured
df_clean <- df_clean[df_clean$bmi < 9999, ]   #removing entries where bmi not valid
df_clean <- df_clean[df_clean$income < 997, ]   #removing entries where income not valid

columns_to_check <- c("mild_cardio", "aerobic", "gym", "sport", "golf", "walk")

df_clean <- df_clean %>%
  mutate(physically_active = if_any(all_of(columns_to_check), ~ . > 2) * 1)

df_final <- df_clean[ , !(names(df_clean) %in% columns_to_check)]



df_long <- df_final %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of All Variables Before Outlier Removal", x = "Value", y = "Frequency")



ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Box Plots for All Variables Before Outlier Removal", x = "Variable", y = "Value")



non_cat_variables <- c( "sbp", "bmi", "age","income")


# remove outliers based on 1.5 interquartile range
remove_outliers_na <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[!(x >= lower & x <= upper)] <- NA
  x
}

df_no_outliers <- df_final %>%
  mutate(across(all_of(non_cat_variables), remove_outliers_na))%>%drop_na(.)

df_long <- df_no_outliers %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of All Variables After Outlier Removal", x = "Value", y = "Frequency")



ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Box Plots for All Variables After Outlier Removal", x = "Variable", y = "Value")


correlation_matrix <- cor(df_no_outliers, use = "complete.obs", method = "pearson")
print(correlation_matrix)


ggcorrplot(correlation_matrix, lab = TRUE, lab_size = 3, colors = c("red", "white", "blue"))




ols_model <- lm(sbp ~ age + bmi + smoking_status + alcohol_use + 
                  physically_active + income + sex + hypertensive_meds, 
                data = df_no_outliers)

summary(ols_model)

# reset test for model specification
resettest(ols_model)



# VIF for multicollinearity
print("Variance Inflation Factors:")
vif(ols_model)

# Residual diagnostics
par(mfrow = c(2,2))
plot(ols_model)
par(mfrow = c(1,1))

# test for homoscedasticity

bptest(ols_model)
bptest(ols_model, ~ fitted(ols_model) + I(fitted(ols_model)^2))
gqtest(ols_model)


# robust standard error
coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))



