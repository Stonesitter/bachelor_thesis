# data_prep.R

# Load necessary packages
library(dplyr)

# how many participants are in each group?

mean_val <- mean(merged_filtered$MMM_S_Index, na.rm = TRUE)
sd_val <- sd(merged_filtered$MMM_S_Index, na.rm = TRUE)

group_sd <- cut(merged_filtered$MMM_S_Index,
                breaks = c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf),
                labels = c("LMMs", "IMMs", "HMMs"))




# Calculate quartiles for grouping
quartiles <- quantile(merged_filtered$MMM_S_Index, probs = c(0.25, 0.75), na.rm = TRUE)

# Create the group_quantile variable based on quartiles
group_quantile <- cut(merged_filtered$MMM_S_Index,
                                      breaks = c(-Inf, quartiles[1], quartiles[2], Inf),
                                      labels = c("LMMs", "IMMs", "HMMs"),
                                      include.lowest = TRUE)

# Create groups based on standard deviation
group_sd <- cut(merged_filtered$MMM_S_Index,
                                breaks = c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf),
                                labels = c("LMMs", "IMMs", "HMMs"))

# Create tables
table_sd <- table(group_sd)
table_quantile <- table(group_quantile)




# Combine into a list
analysis_data <- list(
  mean_val = mean_val,
  sd_val = sd_val,
  quantiles = quantiles,
  table_sd = table_sd,
  table_quantile = table_quantile
)


nfemale <- nrow(merged_filtered[merged_filtered$Gender == "female", ])
m_age <- round(mean(merged_filtered$Age), 2)
sd_age <- round(sd(merged_filtered$Age), 2)
min_age <- min(merged_filtered$Age)
max_age <- max(merged_filtered$Age)

info_participants <- paste0("(M = ", m_age, ", SD = ", sd_age, ", ", nfemale, " females)")

# Add the new elements to the existing list
analysis_data$nfemale <- nfemale
analysis_data$m_age <- m_age
analysis_data$sd_age <- sd_age
analysis_data$info_participants <- info_participants
analysis_data$max_age <- max_age
analysis_data$min_age <- min_age
analysis_data$group_sd <- group_sd
analysis_data$group_quantile <- group_quantile


# Save the updated list
getwd()
save(analysis_data, file = "analysis_data.RData")
analysis_data
     