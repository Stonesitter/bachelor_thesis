### UEBERPRUEFUNG VON MODELLANNAHMEN



# Residuenplot (Residuals vs Fitted)
plot(model_poly, which = 1)


# Normal-QQ-Plot
plot(model_poly, which = 2)


# Einfluss auf die Modellanpassung:

plot(log_model, which = 4)  # Cook's Distance Plot
cooks.distance(log_model)

# Bestimmung einflussreicher Punkte

n <- nrow(merged_data)  # Anzahl der Beobachtungen
k <- length(coef(log_model))  # Anzahl der Prädiktoren im Modell (einschließlich des Intercepts)
cook_threshold <- 4 / (n - k - 1)
cook_threshold

cooks_d <- cooks.distance(log_model)
influential_points <- which(cooks_d > cook_threshold)
merged_data[influential_points, ]


### Studentisierte Residuen: sind die Punkte auch ausreisser?

stud_res <- rstudent(log_model)
plot(stud_res, type = "h")
abline(h = c(-2, 2), col = "red")

outlier_points <- which(abs(stud_res) > 2)


# Schritt 4: Kombination der einflussreichen Punkte und Ausreißer
all_outliers <- union(influential_points, outlier_points)

## Erstelle ein Modell ohne diese outliers

data_filtered <- merged_data[-all_outliers, ]
log_model_reduced <- lm(total_correct_log ~ poly(MMM_S_Index, 2), data = data_filtered)

## Vergleich die modelle 
summary(model_poly)
summary(log_model_reduced)



### Visualisierungen

# Vorhersage für das Originalmodell
merged_data$predicted <- predict(model_poly, newdata = merged_data)

# Vorhersage für das bereinigte Modell
data_filtered$predicted <- predict(model_poly_reduced, newdata = data_filtered)

# Plot mit ggplot2 für das Originalmodell
ggplot(merged_data, aes(x = MMM_S_Index, y = total_correct)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  ggtitle("Originalmodell")

# Plot mit ggplot2 für das bereinigte Modell
ggplot(data_filtered, aes(x = MMM_S_Index, y = total_correct)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red") +
  ggtitle("Bereinigtes Modell")




#### 2. Multikollinearität prüfen
  


library(car)
vif(model_poly)


#### 3. Modellgüte bewerten
  

# R-squared und Adjusted R-squared sind in der summary-Funktion enthalten
summary(model_poly)

# AIC und BIC
AIC(model_poly)
BIC(model_poly)

AIC(model_poly_reduced)
BIC(model_poly_reduced)






# Veränderung der Modellergebnisse anhand des Q-Q Plots

# Modell ohne die fraglichen Punkte
model_poly_reduced <- lm(total_correct ~ poly(MMM_S_Index, 2), data = merged_data[-c(9, 10, 54), ])
summary(model_poly)
summary(model_poly_reduced)


## Robustere Ausreißer entnahme


install.packages("MASS")
library(MASS)
model_robust <- rlm(total_correct ~ poly(MMM_S_Index, 2), data = data)
summary(model_robust)



# Überprüfung auf Heteroskedastizität: Verwende den Breusch-Pagan-Test

library(lmtest)
bptest(model_poly)


# Skalierte Residuen vs Leverage
plot(model_poly, which = 5)

# Residuenhistogramm
hist(residuals(model_poly), breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")












