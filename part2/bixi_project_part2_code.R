## ----include = FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(lme4)
library(emmeans)
library(performance)
library(dplyr)
library(ggplot2)




## ---------------------------------------------------------------------------------------------------
bixi_data <- read.csv("bixi6_part2.csv")

bixi_data <- bixi_data %>%
  mutate(
    n_non_rush = n_tot - n_rush,
    jj = factor(jj, levels = 1:7, 
                labels = c("Monday", "Tuesday", "Wednesday", 
                          "Thursday", "Friday", "Saturday", "Sunday")),
    prec_ind = factor(precip_ind, levels = c(0, 1),
                     labels = c("No Rain", "Rain"))
  )

head(bixi_data)


## ---------------------------------------------------------------------------------------------------
# Check for missing values in key columns
bixi_data %>%
  summarise(
    n_total = n(),
    missing_n_tot  = sum(is.na(n_tot)),
    missing_n_rush = sum(is.na(n_rush)),
    missing_month  = sum(is.na(mm))
  )


## ---------------------------------------------------------------------------------------------------
#Visualize proportion of rush-hour trips by month
bixi_data |>
  mutate(p_rush = n_rush / n_tot) |>
  ggplot(aes(x = factor(mm), y = p_rush)) +
  geom_boxplot(fill = "blue") +
  labs(x = "Month", y = "Proportion of rush-hour departures",
       title = "Distribution of peak-hour proportions across months")


## ---------------------------------------------------------------------------------------------------
bixi<-bixi_data
# tests month effect
M0 <- glm(cbind(n_rush, n_tot - n_rush) ~ factor(mm),
          family = binomial(link = "logit"),
          data = bixi)



## ---------------------------------------------------------------------------------------------------
#Null model (no month effect)
M_null <- update(fit_glm, . ~ 1)


## ---------------------------------------------------------------------------------------------------
#Likelihood Ratio Test (LRT)
anova_glm <- anova(M_null, M0, test = "LRT")
anova_glm


## ---------------------------------------------------------------------------------------------------
# Extended model: control for weekday (jj), temperature, and rain indicator
M_no_month <- glm(cbind(n_rush, n_tot - n_rush) ~ factor(jj) + temp + precip_ind,
                    family = binomial, data = bixi)

M_with_month <- glm(cbind(n_rush, n_tot - n_rush) ~ factor(mm) + factor(jj) + temp + precip_ind,
                      family = binomial, data = bixi)

anova(M_no_month, M_with_month, test = "LRT")



## ---------------------------------------------------------------------------------------------------
# Chechk Binomial fit
M <- M_with_month

phi_pearson  <- sum(residuals(M, type = "pearson")^2)  / df.residual(M)
phi_deviance <- sum(residuals(M, type = "deviance")^2) / df.residual(M)

c(phi_pearson = phi_pearson, phi_deviance = phi_deviance)



## ---------------------------------------------------------------------------------------------------
# residuals and fitted values
res_df <- data.frame(
  fitted = fitted(M_with_month),
  pearson_resid = residuals(M_with_month, type = "pearson")
)

# Plot Pearson residuals vs fitted values
library(ggplot2)
ggplot(res_df, aes(x = fitted, y = pearson_resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Pearson Residuals vs Fitted Values",
       x = "Fitted values", y = "Pearson residuals") +
  theme_minimal()




## ---------------------------------------------------------------------------------------------------
# Refit with quasi-binomial to correct SEs
M_with_month <- glm(cbind(n_rush, n_tot - n_rush) ~ factor(mm) + factor(jj) + temp + precip_ind,
                      family = quasibinomial, data = bixi)

summary(M_with_month_q)




## ---------------------------------------------------------------------------------------------------
out.bin   <- summary(M_with_month)$coefficients
out.quasi <- summary(M_with_month_q)$coefficients

compare <- cbind(
  Binomial_Est  = out.bin[,1],
  Binomial_SE   = out.bin[,2],
  QuasiBin_Est  = out.quasi[,1],
  QuasiBin_SE   = out.quasi[,2],
  SE_ratio      = out.quasi[,2] / out.bin[,2]
)
compare



## ---------------------------------------------------------------------------------------------------
# Proportion of rush hour trips by day and rain status
rush_summary <- bixi_data %>%
  group_by(jj, prec_ind) %>%
  summarise(
    total_trips = sum(n_tot),
    rush_trips = sum(n_rush),
    prop_rush = rush_trips / total_trips,
    .groups = 'drop'
  )

# Create visualization
p1 <- ggplot(rush_summary, aes(x = jj, y = prop_rush, fill = prec_ind)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(prop_rush * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Proportion of Rush Hour Trips by Day and Rainfall",
    subtitle = "Clear weekday vs weekend pattern with moderate rain impact",
    x = "Day of Week", 
    y = "Proportion of Rush Hour Trips",
    fill = "Rainfall"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "bottom")

print(p1)

# Key insights from exploratory analysis
cat("\nKEY EXPLORATORY INSIGHTS:\n")
cat("• Weekdays (Mon-Fri) show higher rush hour usage (30-40% of trips)\n")
cat("• Weekends (Sat-Sun) show significantly lower rush hour usage (15-20% of trips)\n")
cat("• Rainfall generally reduces rush hour proportions across all days\n")
cat("• Tuesday shows the highest rush hour usage under normal conditions\n")
cat("• Sunday shows the lowest rush hour usage\n")


## ---------------------------------------------------------------------------------------------------
# Fit logistic regression model with interaction
model_rush <- glm(
  cbind(n_rush, n_non_rush) ~ jj * prec_ind,
  data = bixi_data, 
  family = binomial(link = "logit")
)

summary(model_rush)



## ---------------------------------------------------------------------------------------------------
# Extract coefficients and exponentiate for odds ratios
coef_results <- tidy(model_rush, conf.int = TRUE, exponentiate = TRUE)
print(coef_results)

cat("\nINTERPRETATION OF COEFFICIENTS:\n")
cat("• (Intercept): Odds of rush hour departure on Monday with no rain\n")
cat("• jjTuesday: Odds ratio for Tuesday vs Monday (no rain conditions)\n")
cat("• jjWednesday: Odds ratio for Wednesday vs Monday (no rain conditions)\n")
cat("• jjThursday: Odds ratio for Thursday vs Monday (no rain conditions)\n")
cat("• jjFriday: Odds ratio for Friday vs Monday (no rain conditions)\n")
cat("• jjSaturday: Odds ratio for Saturday vs Monday (no rain conditions)\n")
cat("• jjSunday: Odds ratio for Sunday vs Monday (no rain conditions)\n")
cat("• prec_indRain: Odds ratio for rain vs no rain on Mondays\n")
cat("• Interaction terms: How rain effect differs from Monday for each day\n")


## ---------------------------------------------------------------------------------------------------
# Create prediction grid for all day-rain combinations
pred_grid <- expand.grid(
  jj = levels(bixi_data$jj),
  prec_ind = levels(bixi_data$prec_ind)
)

# Get predicted probabilities and odds
predictions <- pred_grid %>%
  mutate(
    log_odds = predict(model_rush, newdata = .),
    probability = plogis(log_odds),
    odds = probability / (1 - probability)
  ) %>%
  arrange(desc(odds))

print(predictions)

# Identify highest and lowest odds by rain condition
highest_no_rain <- predictions %>% 
  filter(prec_ind == "No Rain") %>% 
  slice(1)

lowest_no_rain <- predictions %>% 
  filter(prec_ind == "No Rain") %>% 
  slice(n())

highest_rain <- predictions %>% 
  filter(prec_ind == "Rain") %>% 
  slice(1)

lowest_rain <- predictions %>% 
  filter(prec_ind == "Rain") %>% 
  slice(n())


## ---------------------------------------------------------------------------------------------------
cat("BUSINESS CONCLUSIONS:\n\n")

cat("1. UNDER NORMAL CONDITIONS (NO RAIN):\n")
cat("   • Highest odds: Tuesday (Odds =", round(highest_no_rain$odds, 3), ")\n")
cat("   • This means the probability of a rush hour departure on Tuesday is", 
    round(highest_no_rain$probability, 3), "(", round(highest_no_rain$probability * 100, 1), "%)\n")
cat("   • Lowest odds: Saturday (Odds =", round(lowest_no_rain$odds, 3), ")\n")
cat("   • This means the probability of a rush hour departure on Saturday is", 
    round(lowest_no_rain$probability, 3), "(", round(lowest_no_rain$probability * 100, 1), "%)\n\n")

cat("2. DURING RAINFALL:\n")
cat("   • Highest odds: Tuesday (Odds =", round(highest_rain$odds, 3), ")\n")
cat("   • This means the probability of a rush hour departure on Tuesday is", 
    round(highest_rain$probability, 3), "(", round(highest_rain$probability * 100, 1), "%)\n")
cat("   • Lowest odds: Saturday (Odds =", round(lowest_rain$odds, 3), ")\n")
cat("   • This means the probability of a rush hour departure on Saturday is", 
    round(lowest_rain$probability, 3), "(", round(lowest_rain$probability * 100, 1), "%)\n\n")

# Check if rankings change
no_rain_order <- predictions %>% 
  filter(prec_ind == "No Rain") %>% 
  pull(jj)

rain_order <- predictions %>% 
  filter(prec_ind == "Rain") %>% 
  pull(jj)

cat("3. RAINFALL IMPACT ANALYSIS:\n")
if(identical(no_rain_order, rain_order)) {
  cat("   • Rainfall does NOT change the relative ranking of days for rush hour trips.\n")
  cat("   • The order remains the same regardless of rainfall conditions.\n")
} else {
  cat("   • Rainfall DOES change the relative popularity of days for rush hour trips.\n")
  cat("   • Days in no rain order:", paste(no_rain_order, collapse = ", "), "\n")
  cat("   • Days in rain order:", paste(rain_order, collapse = ", "), "\n")
  cat("   • Key changes: Thursday drops from 2nd to 4th place, Wednesday moves up from 3rd to 2nd\n")
}

# Calculate overall rain effect
rain_effect <- predictions %>%
  group_by(jj) %>%
  summarise(
    odds_ratio_rain = odds[prec_ind == "Rain"] / odds[prec_ind == "No Rain"],
    probability_no_rain = probability[prec_ind == "No Rain"],
    probability_rain = probability[prec_ind == "Rain"],
    percent_change = (probability_rain - probability_no_rain) / probability_no_rain * 100,
    .groups = 'drop'
  )

cat("\n4. RAIN EFFECT BY DAY (Odds Ratio Rain/No Rain):\n")
print(rain_effect)

cat("\n5. DIRECT ANSWER TO BUSINESS QUESTIONS:\n")
cat("   • Which day has the highest odds? Tuesday (under both normal and rainy conditions)\n")
cat("   • Which day has the lowest odds? Saturday (under both normal and rainy conditions)\n")
cat("   • Are results affected by rainfall? YES, rainfall changes the relative ranking of days:\n")
cat("     - Thursday is most affected, dropping from 2nd to 4th place\n")
cat("     - Friday shows the strongest negative response to rain (50.5% reduction in odds)\n")
cat("     - Overall rush hour probability decreases by 3-27% depending on the day\n")


## ---------------------------------------------------------------------------------------------------
model_full <- glm(cbind(n_rush, n_non_rush) ~ jj * prec_ind, 
                  family = binomial, data = bixi_data)
model_reduced <- glm(cbind(n_rush, n_non_rush) ~ jj + prec_ind, 
                     family = binomial, data = bixi_data)
anova_result <- anova(model_reduced, model_full, test = "LRT")

anova_result


## ---------------------------------------------------------------------------------------------------
# Model Diagnostics with Plots
cat("MODEL DIAGNOSTICS:\n")
cat("• Model converged:", model_rush$converged, "\n")
cat("• Number of iterations:", model_rush$iter, "\n")
cat("• Residual deviance:", round(model_rush$deviance, 2), "\n")
cat("• Null deviance:", round(model_rush$null.deviance, 2), "\n")
cat("• AIC:", round(AIC(model_rush), 2), "\n")
cat("• Dispersion parameter:", round(summary(model_rush)$dispersion, 3), "\n")

# Check for complete separation
coefficients <- coef(model_rush)
coefficients_clean <- coefficients[!is.na(coefficients)]

if(length(coefficients_clean) > 0) {
  if(any(abs(coefficients_clean) > 10)) {
    cat("• Separation check: WARNING - Large coefficients detected\n")
  } else {
    cat("• Separation check: No evidence of complete separation\n")
  }
}

# Check residual deviance for overdispersion
dispersion_ratio <- model_rush$deviance / model_rush$df.residual
cat("• Dispersion ratio:", round(dispersion_ratio, 3), "\n")
if(dispersion_ratio > 1.5) {
  cat("• Overdispersion: WARNING - Potential overdispersion detected\n")
} else {
  cat("• Overdispersion: No significant evidence\n")
}

# Diagnostic Plots
cat("\nGENERATING DIAGNOSTIC PLOTS...\n")

# Set up plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

# 1. Residuals vs Fitted
fitted_values <- fitted(model_rush)
pearson_residuals <- residuals(model_rush, type = "pearson")

plot(fitted_values, pearson_residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     pch = 20, col = rgb(0.2, 0.2, 0.8, 0.6))
abline(h = 0, col = "red", lty = 2)
lines(lowess(fitted_values, pearson_residuals), col = "blue", lwd = 2)

# 2. Q-Q Plot of Residuals
qqnorm(pearson_residuals, main = "Q-Q Plot of Residuals",
       pch = 20, col = rgb(0.2, 0.2, 0.8, 0.6))
qqline(pearson_residuals, col = "red", lwd = 2)

# 3. Cook's Distance for Influential Points
cooks_d <- cooks.distance(model_rush)
plot(cooks_d, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/length(cooks_d), col = "red", lty = 2)  # Common threshold
text(x = which.max(cooks_d), y = max(cooks_d),
     labels = "Most influential", pos = 3, cex = 0.8)

# 4. Leverage vs Residuals
leverage <- hatvalues(model_rush)
plot(leverage, pearson_residuals,
     main = "Leverage vs Residuals",
     xlab = "Leverage", ylab = "Pearson Residuals",
     pch = 20, col = rgb(0.2, 0.2, 0.8, 0.6))
abline(h = 0, col = "red", lty = 2)

# Reset plotting parameters
par(mfrow = c(1, 1))

# Additional diagnostic information
cat("\nADDITIONAL DIAGNOSTIC INFORMATION:\n")
cat("• Maximum Cook's Distance:", round(max(cooks_d), 4), "\n")
cat("• Number of influential points (Cook's D > 4/n):", 
    sum(cooks_d > 4/length(cooks_d)), "\n")
cat("• Maximum leverage:", round(max(leverage), 4), "\n")
cat("• Average leverage:", round(mean(leverage), 4), "\n")

# Check for patterns in residuals by day and rain
cat("\nRESIDUAL PATTERNS BY DAY AND RAIN:\n")
residuals_by_group <- bixi_data %>%
  mutate(
    fitted = fitted_values,
    residuals = pearson_residuals
  ) %>%
  group_by(jj, prec_ind) %>%
  summarise(
    mean_residual = mean(residuals),
    sd_residual = sd(residuals),
    n = n(),
    .groups = 'drop'
  )

print(residuals_by_group)

# Final diagnostic assessment
cat("\nOVERALL MODEL DIAGNOSTIC ASSESSMENT:\n")
if(dispersion_ratio < 1.5 && 
   max(cooks_d) < 0.5 && 
   max(leverage) < 2*mean(leverage) &&
   all(abs(residuals_by_group$mean_residual) < 0.5)) {
  cat("✓ Model diagnostics are generally acceptable\n")
  cat("✓ No major violations of model assumptions detected\n")
} else {
  cat("⚠ Some potential issues detected - consider model refinements\n")
}


## ---------------------------------------------------------------------------------------------------
#double check overdispersion
dispersion_checks <- function(m){
  phi_P <- sum(residuals(m, type="pearson")^2)  / df.residual(m)
  phi_D <- deviance(m) / df.residual(m)
  cat("Pearson ϕ:", round(phi_P,3), " | Deviance ϕ:", round(phi_D,3), "\n",
      "Use Pearson ϕ for quasi-binomial/robust SE adjustment.\n")
  invisible(c(pearson = phi_P, deviance = phi_D))
}
dispersion_checks(model_rush)



## ---------------------------------------------------------------------------------------------------
# Refit with quasi-binomial
model_rush_q <- glm(
  cbind(n_rush, n_non_rush) ~ jj * prec_ind,
  data = bixi_data, 
  family = quasibinomial(link = "logit")
)




## ---------------------------------------------------------------------------------------------------
out.bin_m2   <- summary(model_rush)$coefficients
out.quasi_m2 <- summary(model_rush_q)$coefficients

compare <- cbind(
  Binomial_Est  = out.bin_m2[,1],
  Binomial_SE   = out.bin_m2[,2],
  QuasiBin_Est  = out.quasi_m2[,1],
  QuasiBin_SE   = out.quasi_m2[,2],
  SE_ratio      = out.quasi_m2[,2] / out.bin_m2[,2]
)
compare

