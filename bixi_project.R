## ----include = false----------------------------------------------------------
#library(tidyverse)
library(broom)
#library(car)
library(ggplot2)
library(cowplot)
library(dplyr)
library(gridExtra)


## -----------------------------------------------------------------------------
bixi <- read.csv("bixi6_part1.csv")


## ----fig.width=10, fig.height=4, message=FALSE, warning=FALSE-----------------

# Ensure variables exist
stopifnot("dur" %in% names(bixi))
bixi$logdur <- log(bixi$dur)

# Reusable minimalist theme
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size = 0.2),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey35"),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", size = 9, margin = margin(t = 6))
  )

# Helper to add median/mean lines with labels
add_center_lines <- function(p, xvar, data) {
  med <- median(data[[xvar]], na.rm = TRUE)
  mn  <- mean(data[[xvar]], na.rm = TRUE)
  p +
    geom_vline(xintercept = med, linetype = 2, linewidth = 0.6) +
    geom_vline(xintercept = mn,  linetype = 1, linewidth = 0.6, alpha = 0.8) +
    annotate("text", x = med, y = Inf, vjust = 1.5, label = "Median", size = 3.2) +
    annotate("text", x = mn,  y = Inf, vjust = 3.0, label = "Mean",   size = 3.2)
}

p_dur <- ggplot(bixi, aes(x = dur)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.9) +
  geom_density(linewidth = 0.9) +
  geom_rug(alpha = 0.15, length = unit(2.5, "pt")) +
  labs(
    title = "Trip duration (minutes)",
    subtitle = "Histogram with density overlay;",
    x = "Duration (minutes)", y = "Density",
    caption = "Visual check for right skew and potential variance stabilization via log transform."
  ) +
  theme_clean
p_dur <- add_center_lines(p_dur, "dur", bixi)

p_logdur <- ggplot(bixi[is.finite(bixi$logdur), ], aes(x = logdur)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.9) +
  geom_density(linewidth = 0.9) +
  geom_rug(alpha = 0.15, length = unit(2.5, "pt")) +
  labs(
    title = "Log trip duration",
    subtitle = "Histogram with density overlay; ",
    x = "log(Duration)", y = "Density",
    caption = "Log transform typically reduces right tail and improves normality for linear modeling."
  ) +
  theme_clean
p_logdur <- add_center_lines(p_logdur, "logdur", bixi)

plot_grid(p_dur, p_logdur, nrow = 1, labels = c("A", "B"))


## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=4-----------------
# Ensure variables and build rainy-day intensity
stopifnot("precip" %in% names(bixi))
bixi$Wet      <- as.integer(bixi$prec > 0)
bixi$rain_int <- ifelse(bixi$prec > 0, log1p(bixi$prec), NA_real_)
bixi_wet      <- subset(bixi, Wet == 1 & is.finite(rain_int))

# Reuse theme and helper from above (theme_clean, add_center_lines)

p_prec <- ggplot(bixi_wet, aes(x = precip)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.9) +
  geom_density(linewidth = 0.9) +
  geom_rug(alpha = 0.15, length = unit(2.5, "pt")) +
  labs(
    title = "Precipitation on rainy days",
    subtitle = "Histogram with density overlay; ",
    x = "Precipitation (mm)", y = "Density",
    caption = "Raw precipitation often shows a heavy right tail on rainy days."
  ) +
  theme_clean
p_prec <- add_center_lines(p_prec, "precip", bixi_wet)

p_logprec <- ggplot(bixi_wet, aes(x = rain_int)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.9) +
  geom_density(linewidth = 0.9) +
  geom_rug(alpha = 0.15, length = unit(2.5, "pt")) +
  labs(
    title = "log1p(precipitation) on rainy days",
    subtitle = "Histogram with density overlay;",
    x = "log(1 + precipitation)", y = "Density",
    caption = "The log1p transform compresses the heavy tail,."
  ) +
  theme_clean
p_logprec <- add_center_lines(p_logprec, "rain_int", bixi_wet)

plot_grid(p_prec, p_logprec, nrow = 1, labels = c("C", "D"))



## ----message=FALSE, warning=FALSE---------------------------------------------

#install.packages("corrplot")
library(corrplot)

# Keep only numeric columns with non-zero variance
bixi_num <- bixi[sapply(bixi, is.numeric)]
bixi_num <- bixi_num[, sapply(bixi_num, function(x) sd(x, na.rm = TRUE) > 0)]

# Correlation matrix
corr_matrix <- cor(bixi_num, use = "pairwise.complete.obs")

# Plot
corrplot(
  corr_matrix,
  method = "color",                         # colored heatmap
  type = "full",                            # show full matrix
  addCoef.col = "black",                    # add correlation values
  number.cex = 0.6,                         # size of numbers
  tl.col = "black",                         # text color
  tl.srt = 45,                              # rotate x labels
  col = colorRampPalette(c("#f7fbff", "#6baed6", "#08306b"))(200)
)

bixi$Weekend <- as.integer(bixi$jj %in% c(6,7))
# Boxplot of Trip Duration by Weekend vs Weekday
ggplot(bixi, aes(x = as.factor(Weekend), y = dur)) +
  geom_boxplot() +
  labs(
    title = "Trip Duration by Weekend vs Weekday",
    x = "Weekend (1 = Weekend, 0 = Weekday)",
    y = "Trip Duration (minutes)"
  ) + 
  theme_clean


## ----message=FALSE, warning=FALSE---------------------------------------------
# Load data
bixi <- read.csv("bixi6_part1.csv")

# Remove trips shorter than 1.5 minutes
#bixi <- subset(bixi, dur >= 1.5)

# Construct variables (base R)
bixi$Weekend <- as.integer(bixi$jj %in% c(6,7))
bixi$mm <- factor(bixi$mm)
bixi$arrondissement <- factor(bixi$arrondissement)
bixi$Wet <- as.integer(bixi$prec > 0)
bixi$temp_c <- bixi$temp - 20
bixi$logdur <- log(bixi$dur)
bixi <- bixi[is.finite(bixi$logdur), ]

# Models
m_q1_crude <- lm(logdur ~ Weekend, data = bixi)


## ----message=FALSE, warning=FALSE---------------------------------------------
m_q1_adj <- lm(
  logdur ~ Weekend +
    temp_c + I(temp_c^2) +
    Wet + log(1 + precip):Wet +
    mm + arrondissement,
  data = bixi
)



## -----------------------------------------------------------------------------
summary(m_q1_crude)


## -----------------------------------------------------------------------------
summary(m_q1_adj)


## ----message=FALSE, warning=FALSE---------------------------------------------
# Residuals vs Fitted plot to check for homoscedasticity
plot(m_q1_adj$fitted.values, m_q1_adj$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)


## ----message=FALSE, warning=FALSE---------------------------------------------
# Q-Q plot to check normality of residuals
qqnorm(m_q1_adj$residuals)
qqline(m_q1_adj$residuals, col = "red", lwd = 2)


## ----message=FALSE, warning=FALSE---------------------------------------------
# Histogram of residuals to check normality
hist(m_q1_adj$residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightblue", border = "black")


## ----message=FALSE, warning=FALSE---------------------------------------------
# Plot Fitted values vs Actual values
plot(m_q1_adj$fitted.values, bixi$logdur, 
     xlab = "Fitted Values", ylab = "Actual Values", 
     main = "Fitted vs Actual Plot")
abline(0, 1, col = "red", lwd = 2)


## -----------------------------------------------------------------------------
## Full model
m_full <- lm(
  logdur ~ Weekend +
    temp_c + I(temp_c^2) +
    Wet + log(1 + precip):Wet +
    mm + arrondissement,
  data = bixi
)
summary(m_full)


## -----------------------------------------------------------------------------
## 1) No climate
m_no_climate <- lm(
  logdur ~ Weekend + mm + arrondissement,
  data = bixi
)

anova(m_no_climate, m_full)


## -----------------------------------------------------------------------------
## 2) No temperature
m_no_temp <- lm(
  logdur ~ Weekend +
    Wet + log(1 + precip):Wet +
    mm + arrondissement,
  data = bixi
)
anova(m_no_temp, m_full)



## -----------------------------------------------------------------------------
## 3) No rain
m_no_rain <- lm(
  logdur ~ Weekend +
    temp_c + I(temp_c^2) +
    mm + arrondissement,
  data = bixi
)

anova(m_no_rain, m_full)


## -----------------------------------------------------------------------------
#full original model
m_no_int <- lm(logdur ~ Weekend + temp_c + I(temp_c^2) +
                   Wet + log(1+precip):Wet +
                   factor(mm) + arrondissement, data=bixi)
summary(m_no_int)


## -----------------------------------------------------------------------------
# with Weekend×Temp
m_temp_int <- lm(logdur ~ Weekend*(temp_c + I(temp_c^2)) +
                   Wet + log(1+precip):Wet +
                   factor(mm) + arrondissement, data=bixi)


## -----------------------------------------------------------------------------

# block F-test: H0: gamma1=gamma2=0
anova(m_no_int, m_temp_int)  



## -----------------------------------------------------------------------------
## With Weekend × rain interactions
m_rain_int <- lm(
  logdur ~ Weekend +
           temp_c + I(temp_c^2) +
           Weekend*(Wet + log(1+precip):Wet) +   # <-- Weekend×Rain interactions
           factor(mm) + arrondissement,
  data = bixi
)
summary(m_rain_int)


## -----------------------------------------------------------------------------
## Global interaction block (γ3=γ4=0?) — 2 df}
anova(m_no_int, m_rain_int)   # decision at alpha = 0.01


## -----------------------------------------------------------------------------
# Predictions across temperature by weekend status
newdat <- expand.grid(
  Weekend = c(0,1),
  temp_c  = seq(min(bixi$temp_c), max(bixi$temp_c), length.out = 100),
  Wet = 0, precip = 0,  # dry day for clarity
  mm = factor(5, levels = sort(unique(bixi$mm))),            # baseline month
  arrondissement = levels(bixi$arrondissement)[1]            # baseline borough
)
newdat$pred <- predict(m_no_int, newdata = newdat, se.fit = TRUE)
newdat$fit  <- newdat$pred$fit
newdat$se   <- newdat$pred$se.fit
newdat$lwr  <- newdat$fit - 1.96*newdat$se
newdat$upr  <- newdat$fit + 1.96*newdat$se

p_temp <-
ggplot(newdat, aes(temp_c, fit, color = factor(Weekend), fill = factor(Weekend))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15, color = NA) +
  labs(x = "Centered temperature (°C)", y = "Predicted log duration",
       color = "Weekend", fill = "Weekend",
       title = "Temperature effect on trip duration: Weekend vs. Weekday") +
  theme_minimal()



## -----------------------------------------------------------------------------
wetdat <- expand.grid(
  Weekend = c(0,1),
  Wet = 1,
  precip = seq(0, quantile(bixi$precip[bixi$precip>0], .95, na.rm=TRUE), length.out=80),
  temp_c = 0, mm = factor(5, levels = sort(unique(bixi$mm))),
  arrondissement = levels(bixi$arrondissement)[1]
)
wetdat$log1p_prec <- log1p(wetdat$precip)
wetdat$pred <- predict(m_no_int, newdata = transform(wetdat, `log(1+precip)`=log1p_prec), se.fit=TRUE)
wetdat$fit  <- wetdat$pred$fit
wetdat$se   <- wetdat$pred$se.fit
wetdat$lwr  <- wetdat$fit - 1.96*wetdat$se
wetdat$upr  <- wetdat$fit + 1.96*wetdat$se

p_rain <-ggplot(wetdat, aes(precip, fit, color=factor(Weekend), fill=factor(Weekend))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15, color=NA) +
  labs(x="Rainfall (mm) on wet days", y="Predicted log duration",
       color="Weekend", fill="Weekend",
       title="Rainfall effect on trip duration (wet days): Weekend vs. Weekday") +
  theme_minimal()



## -----------------------------------------------------------------------------
grid.arrange(
  p_temp + theme(legend.position = "bottom"),
  p_rain + theme(legend.position = "bottom"),
  ncol = 1,nrow=2
)


## -----------------------------------------------------------------------------
## Full model used in Q2 (includes month dummies)
m_full <- lm(
  logdur ~ Weekend +
    temp_c + I(temp_c^2) +
    Wet + log(1 + precip):Wet +
    mm + arrondissement,
  data = bixi
)

## Reduced model without month dummies
m_no_month <- lm(
  logdur ~ Weekend +
    temp_c + I(temp_c^2) +
    Wet + log(1 + precip):Wet +
    arrondissement,
  data = bixi
)

## Block F-test: are months jointly significant?
anova(m_no_month, m_full)

