library(ggplot2)

# create merged_filtered based on p_error outliers

# Calculate the IQR
Q1 <- quantile(merged_data$p_error, 0.25)
Q3 <- quantile(merged_data$p_error, 0.75)
IQR <- Q3 - Q1

# Determine the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter the data to remove outliers
merged_filtered <- merged_data[merged_data$p_error >= lower_bound &
                                 merged_data$p_error <= upper_bound, ]
nrow(merged_filtered)
# is justified since 3 * k doesn't include the 0 p_errors

hist(merged_filtered$p_error)
boxplot(merged_filtered$p_error)

# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(merged_filtered$p_error)
print(shapiro_test)

# calc skewness
# Install the e1071 package if it's not already installed
if (!require(e1071))
  install.packages("e1071")
library(e1071)
# Assuming merged_filtered is your data frame and boxcox_p_error is your variable

skewness_value <- skewness(merged_filtered$p_error)
skewness_value2 <- skewness(merged_filtered$boxcox_reflected_p_error)
print(skewness_value)
print(skewness_value2)


kurtosis_value <- kurtosis(merged_filtered$p_error)
kurtosis_value2 <- kurtosis(merged_filtered$boxcox_reflected_p_error)
print(kurtosis_value)
print(kurtosis_value2)

## Example Text in Results Section:
# "In order to assess the distribution of the perseverative error data, skewness values were 
# calculated for both the original and Box-Cox transformed variables. The skewness of the 
# original p_error variable was found to be -0.54, indicating a slight left skew (i.e., the 
# data distribution is slightly skewed to the left). After applying the Box-Cox 
# transformation, the skewness of the boxcox_p_error variable was 0.04, suggesting that the 
# transformation successfully normalized the data, as the skewness value is close to zero."

mean(merged_filtered$p_error)
sd(merged_filtered$p_error)


merged_filtered$p_correct <- (merged_filtered$total_correct /64) * 100

mean(merged_filtered$p_correct)
sd(merged_filtered$p_correct)


# Reflect the variable

max_value <- max(merged_filtered$p_error)
reflected_p_error <- max_value + 1 - merged_filtered$p_error

# Install and load the MASS package if not already installed
if (!require(MASS))
  install.packages("MASS")
library(MASS)

# Apply Box-Cox transformation
boxcox_result_reflected <- boxcox(reflected_p_error ~ 1)
lambda_reflected <- boxcox_result_reflected$x[which.max(boxcox_result_reflected$y)]
boxcox_reflected_p_error <- (reflected_p_error ^ lambda_reflected - 1) / lambda_reflected

# Perform Shapiro-Wilk test on Box-Cox-transformed reflected data
shapiro_test_boxcox_reflected <- shapiro.test(boxcox_reflected_p_error)
print(shapiro_test_boxcox_reflected)


merged_filtered$boxcox_reflected_p_error <- boxcox_reflected_p_error

# Analysis

hist(merged_filtered$boxcox_reflected_p_error)
boxplot(merged_filtered$boxcox_reflected_p_error)

library(ggplot2)
ggplot(merged_filtered,
       aes(x = MMM_S_Index, y = boxcox_reflected_p_error)) +
  geom_point()
model <- lm(boxcox_reflected_p_error ~ MMM_S_Index, data = merged_filtered)
model2  <- lm(boxcox_reflected_p_error ~ poly(MMM_S_Index, 2), data = merged_filtered)

summary(model)
summary(model2)

par(mfrow = c(2, 2))
plot(model)
plot(model2)

# how many participants are in each group?

mean_val <- mean(merged_filtered$MMM_S_Index, na.rm = TRUE)
sd_val <- sd(merged_filtered$MMM_S_Index, na.rm = TRUE)
  
group_sd <- cut(merged_filtered$MMM_S_Index,
                                breaks = c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf),
                                labels = c("LMMs", "IMMs", "HMMs"))




quantiles <- quantile(merged_filtered$MMM_S_Index, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

  

group_quantile <- cut(merged_filtered$MMM_S_Index,
                                      breaks = quantiles,
                                      labels = c("LMMs", "IMMs", "HMMs"),
                                      include.lowest = TRUE)
table(group_sd)
table(group_quantile)

nrow(merged_filtered)
nrow(merged_data)

summary(merged_filtered$Age)
summary(merged_filtered$Gender)
table(merged_filtered$Gender)

nfemale <- nrow(merged_filtered[merged_filtered$Gender == "female", ])
m_age <- round(mean(merged_filtered$Age), 2)
sd_age <- round(sd(merged_filtered$Age), 2)

info <- paste0("(M = ", m_age, ", SD = ", sd_age, ", ", nfemale, " females)")
nrow(merged_filtered)
