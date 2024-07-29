library(ggplot2)

# Base plot with background coloring
p <- ggplot(merged_data, aes(x = MMM_S_Index, y = total_correct)) +
  # Add colored background rectangles for each segment
  geom_rect(aes(xmin = -Inf, xmax = thresholds_MMM_S_Index[1], ymin = -Inf, ymax = Inf), fill = "#219DD1", alpha = 0.01) +
  geom_rect(aes(xmin = thresholds_MMM_S_Index[1], xmax = thresholds_MMM_S_Index[2], ymin = -Inf, ymax = Inf), fill = "#048CC6", alpha = 0.01) +
  geom_rect(aes(xmin = thresholds_MMM_S_Index[2], xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#219DD1", alpha = 0.01) +
  # Add scatter points
  geom_point() +
  # Set labels and theme
  labs(x = "MMM_S Index", y = "Total Correct") +

theme(
  panel.grid.major = element_line(color = "black", size = 0.2, linetype = "dotdash"),
  panel.grid.minor = element_line(color = "black", size = 0.2, linetype = "dotdash")
)


# Adding horizontal segments for mean values using annotate

mean_low <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "Low"])
mean_medium <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "Medium"])
mean_high <- mean(merged_data$total_correct[merged_data$catMMM_S_Index == "High"])


p <- p + annotate("segment", x = -Inf, xend = thresholds_MMM_S_Index[1], y = mean_low, yend = mean_low, color = "yellow") +
  annotate("segment", x = thresholds_MMM_S_Index[1], xend = thresholds_MMM_S_Index[2], y = mean_medium, yend = mean_medium, color = "yellow") +
  annotate("segment", x = thresholds_MMM_S_Index[2], xend = Inf, y = mean_high, yend = mean_high, color = "yellow")

# Calculate positions based on thresholds
x_positions <- c((min(merged_data$MMM_S_Index) + thresholds_MMM_S_Index[1]) / 2, 
                 (thresholds_MMM_S_Index[1] + thresholds_MMM_S_Index[2]) / 2,
                 (thresholds_MMM_S_Index[2] + max(merged_data$MMM_S_Index)) / 2)

# adding lables
category = levels(merged_data$catMMM_S_Index)
counts = as.numeric(table(merged_data$catMMM_S_Index))
labels <- paste0(category, ": ", counts)

# Add annotations below the graph
for (i in seq_along(labels)) {
  p <- p + annotate("text", x = x_positions[i], y = -10, label = labels[i], size = 5, vjust = -0.5, color = "black")
}


# Print the plot
print(p)

table(merged_data$catMMM_S_Index)
