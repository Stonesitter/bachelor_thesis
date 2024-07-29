library(mgcv)
model_gam <- gam(total_correct ~ s(MMM_S_Index), data = merged_data)
summary(model_gam)
plot(model_gam)


library(MASS)
library(caret)

# Robuste Regression
model_robust <- rlm(total_correct ~ poly(MMM_S_Index, 2), data = merged_data)
summary(model_robust)

# Kreuzvalidierung des robusten Modells
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(total_correct ~ poly(MMM_S_Index, 2), data = merged_data, method = "lm", trControl = train_control)
print(model_cv)


model_gam_sw_clean <- gam(total_correct ~ s(MMM_S_Index), data = sw_clean_data)
summary(model_gam_sw_clean)
plot(model_gam)


model_gam_stud_filtered <- gam(total_correct ~ s(MMM_S_Index), data = data_filtered)
summary(model_gam_stud_filtered)
plot(model_gam_stud_filtered)


gam(total_correct ~ s(MMM_S_Index), data = merged_data)
