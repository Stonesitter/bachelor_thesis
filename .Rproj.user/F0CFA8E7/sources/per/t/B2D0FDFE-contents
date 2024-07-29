hist(x = merged_data$total_correct)
hist(x = merged_data$total_correct_log)


library(MASS)



# Fit a polynomial model with two terms
model <- lm(total_correct ~ poly(MMM_S_Index, 2), data = merged_data)

# Find the optimal Box-Cox transformation parameter
boxcox_result <- boxcox(model, plotit = FALSE)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
lambda

# Apply the Box-Cox transformation with the optimal lambda
if (lambda == 0) {
  merged_data$total_correct_boxcox <- log(merged_data$total_correct + 1)  # Add 1 to avoid log(0)
} else {
  merged_data$total_correct_boxcox <- (merged_data$total_correct^lambda - 1) / lambda
}

# Plot the transformed variable
par(mfrow = c(1, 2))
hist(merged_data$total_correct, main = "Histogram of Original Total Correct", xlab = "Total Correct")
hist(merged_data$total_correct_boxcox, main = "Histogram of Box-Cox Transformed Total Correct", xlab = "Box-Cox Transformed Total Correct")



# Fit a polynomial model on the Box-Cox transformed variable
model_transformed <- lm(total_correct_boxcox ~ poly(MMM_S_Index, 2), data = merged_data)

# Calculate Cook's distance
cooksd <- cooks.distance(model_transformed)

# Calculate studentized residuals
studres <- rstudent(model_transformed)

# Determine thresholds for Cook's distance and studentized residuals
threshold_cooks <- 4 / (nrow(merged_data) - length(model_transformed$coefficients) - 1)
threshold_studres <- 3

# Identify outliers based on thresholds
outliers <- which(cooksd > threshold_cooks | abs(studres) > threshold_studres)

# Remove outliers from the dataset
data_filtered_no_outliers <- merged_data[-outliers, ]


# Fit polynomial regression on the cleaned dataset
model_cleaned <- lm(total_correct_boxcox ~ poly(MMM_S_Index, 2), data = data_filtered_no_outliers)
summary(model_cleaned)

# Perform Spearman rank correlation on the cleaned dataset
result_spearman_cleaned <- cor.test(data_filtered_no_outliers$MMM_S_Index, data_filtered_no_outliers$total_correct, method = "spearman")
print(result_spearman_cleaned)

# Perform Kendall's Tau correlation test on the cleaned dataset
result_kendall_cleaned <- cor.test(data_filtered_no_outliers$MMM_S_Index, data_filtered_no_outliers$total_correct, method = "kendall")
print(result_kendall_cleaned)

par(mfrow = c(2, 2))
plot(model_cleaned)


# Perform Spearman rank correlation on the Box-Cox transformed data
result_spearman <- cor.test(data_filtered_no_outliers$MMM_S_Index, data_filtered_no_outliers$total_correct_boxcox, method = "spearman")
print(result_spearman)

# Perform Kendall's Tau correlation test on the Box-Cox transformed data
result_kendall <- cor.test(data_filtered_no_outliers$MMM_S_Index, data_filtered_no_outliers$total_correct_boxcox, method = "kendall")
print(result_kendall)



