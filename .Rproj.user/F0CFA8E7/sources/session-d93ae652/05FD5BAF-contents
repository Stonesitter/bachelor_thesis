plot(merged_data$MMM_S_Index, merged_data$total_correct, main = "My Scatterplot",
     ylab="correct", xlab = "MM", las =1)

abline(v = thresholds_MMM_S_Index, col = "blue")


# Get x-limits from CURRENT plot
x_lim <- par("usr")[1:2]

# Adding horizontal segments for mean values of each category
segments(x0 = x_lim[1], x1 = thresholds_MMM_S_Index[1], y0 = mean_low, y1 = mean_low, col = "red")
segments(x0 = thresholds_MMM_S_Index[1], x1 = thresholds_MMM_S_Index[2], y0 = mean_medium, y1 = mean_medium, col = "green")
segments(x0 = thresholds_MMM_S_Index[2], x1 = x_lim[2], y0 = mean_high, y1 = mean_high, col = "purple")
plot(log(merged_data$MMM_S_Index), merged_data$total_correct, main = "My Scatterplot",
     ylab="correct", xlab = "MM", las =1)

mean_low <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "Low"])
mean_medium <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "Medium"])
mean_high <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "High"])







#log x variable
model <- lm(total_correct ~ log(MMM_S_Index), data = merged_data)
summary(model)

# log y variable
plot(merged_data$MMM_S_Index, log(merged_data$total_correct), main = "My Scatterplot",
     ylab="correct", xlab = "MM", las =1)
model <- lm(log(total_correct) ~ poly(MMM_S_Index, 2), data = merged_data)
summary(model)

summary(model)


model <- lm(merged_data$total_correct ~ merged_data$MMM_S_Index + I(merged_data$MMM_S_Index^2))



summary(model)


plot(model)



# Center the MMM_S_Index variable
merged_data$MMM_S_Index_centered <- merged_data$MMM_S_Index - mean(merged_data$MMM_S_Index)

# Create the squared term of the centered MMM_S_Index
merged_data$MMM_S_Index_centered_squared <- merged_data$MMM_S_Index_centered^2

# Run the linear model
model <- lm(total_correct ~ MMM_S_Index_centered + MMM_S_Index_centered_squared, data = merged_data)

# Summary of the model to view results
summary(model)

plot(merged_data$MMM_S_Index_centered, merged_data$total_correct, main = "My Scatterplot",
     ylab="correct", xlab = "MM", las =1)




# Calculate mean and standard deviation
mean_MMM_S_Index <- mean(merged_data$MMM_S_Index)
sd_MMM_S_Index <- sd(merged_data$MMM_S_Index)

# Create a categorical variable based on SD
merged_data$catMMM_S_Index <- cut(merged_data$MMM_S_Index,
                     breaks = c(-Inf, mean_MMM_S_Index - sd_MMM_S_Index, mean_MMM_S_Index + sd_MMM_S_Index, Inf),
                     labels = c("Low", "Medium", "High"),
                     right = FALSE)

# View the distribution of categories
table(merged_data$catMMM_S_Index)

# Fit a linear model with the categorical variable
model <- lm(total_correct ~ catMMM_S_Index, data = merged_data)

# Summary of the model
summary(model)


merged_data$MMM_S_Index[1:10]
merged_data$catMMM_S_Index[1:10]

# Assuming 'data' is your DataFrame and it includes the column 'MMM_S_Index'
thresholds_MMM_S_Index <- c(mean_MMM_S_Index - sd_MMM_S_Index, 
                            mean_MMM_S_Index + sd_MMM_S_Index)

print(thresholds_MMM_S_Index)

# Print the interquartile range
print(IQR_MMM_S_Index)

# Basic boxplot using base R
boxplot(merged_data$MMM_S_Index, main="Boxplot of MMM_S_Index", ylab="MMM_S_Index", col="blue")

