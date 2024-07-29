install.packages("caret")
library(caret)

# Kreuzvalidierung des bereinigten Modells
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(total_correct ~ poly(MMM_S_Index, 2), data = final_cleaned_data, method = "lm", trControl = train_control)
print(model_cv)





library(MASS)
model_robust <- rlm(total_correct ~ poly(MMM_S_Index, 2), data = merged_data)
summary(model_robust)






# Visualisierung des robusten Modells
library(ggplot2)

# Vorhersagen für das robuste Modell
merged_data$predicted_robust <- predict(model_robust, newdata = merged_data)

# Plot des robusten Modells
ggplot(merged_data, aes(x = MMM_S_Index, y = total_correct)) +
  geom_point() +
  geom_line(aes(y = predicted_robust), color = "blue") +
  ggtitle("Robustes Modell")

# Kreuzvalidierung des robusten Modells (falls gewünscht)
model_cv_robust <- train(total_correct ~ poly(MMM_S_Index, 2), data = merged_data, method = "rlm", trControl = train_control)
print(model_cv_robust)
