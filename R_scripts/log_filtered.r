# Log-transform the variable
merged_data$total_correct_log <- log(merged_data$total_correct)

# Perform polynomial regression
# Assume you are regressing total_correct_log on another variable, say 'MMM_S_Index'
# with polynomial terms up to degree 2
log_model <- lm(total_correct_log ~ poly(MMM_S_Index, 2), data = merged_data)

# Summary of the model to see the results
summary(log_model)


### analyse mit filtered_data (total_correct >= 15)
# Bereinigung der Daten
filtered_data <- merged_data[merged_data$total_correct >= 1, ]

# Modell mit den bereinigten Daten anpassen
model_filtered <- lm(total_correct ~ poly(MMM_S_Index, 2), data = filtered_data)

# Zusammenfassung des Modells anzeigen
summary(model_filtered)



# Adding the log variable

filtered_data$total_correct_log <- log(filtered_data$total_correct)

# Perform polynomial regression
# Assume you are regressing total_correct_log on another variable, say 'MMM_S_Index'
# with polynomial terms up to degree 2
filtered_log_model <- lm(total_correct_log ~ poly(MMM_S_Index, 2), data = filtered_data)

summary(filtered_log_model)

### special filtering

# Identifying outliers
outliers <- merged_data[merged_data$total_correct == 1 |
                          (merged_data$MMM_S_Index == 4 | merged_data$MMM_S_Index == 1), ]

# Histogram
hist(filtered_data$total_correct_log)
hist(filtered_data$total_correct)

# checking the assumptions with the log-transformed data

par(mar = c(2, 2, 2, 2)) # Setzt die inneren Margen (unten, links, oben, rechts)
par(oma = c(0, 1, 0, 1)) # Setzt die äußeren Margen (unten, links, oben, rechts)

par(mfrow = c(2, 2))
plot(log_model_reduced)

plot(log_model_reduced)

summary(log_model_reduced)

ggplot(data_filtered, aes(x = MMM_S_Index, y = total_correct_log)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE, color = "blue")


ggplot(merged_data, aes(x = MMM_S_Index, y = total_correct_log)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue")

data_filtered$predicted <- predict(log_model_reduced, newdata = data_filtered)
