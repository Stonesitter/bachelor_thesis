# Bereinigung der Daten

## Ausreisser total_correct
# Tukey's Fence Methode.

# Berechnung der Quartile und des IQR
Q1 <- quantile(merged_data$total_correct, 0.25)
Q3 <- quantile(merged_data$total_correct, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

## Erstellen sauberer Dataframe


clean_data <- merged_data[merged_data$total_correct >= lower_bound & merged_data$total_correct <= upper_bound, ]


# Anzahl der ursprünglichen und bereinigten Datenzeilen
nrow(merged_data)
nrow(clean_data)

boxplot(clean_data$total_correct, main = "Boxplot of Clean Total Correct", ylab = "Total Correct")



## write csv
# write.csv(clean_data, file = paste0(path, "/clean_data_1906.csv"), row.names = FALSE)



### DataFrame mit den Ausreißern
outliers_data <- merged_data[merged_data$total_correct < lower_bound | merged_data$total_correct > upper_bound, ]


### visueller vergleich

# Scatterplot der bereinigten Daten
plot(merged_data$MMM_S_Index, merged_data$total_correct, main="Scatterplot von bereinigten Daten und Ausreißern", 
     xlab="MMM_S_Index", ylab="Total Correct", col="blue", pch=19)

# Hinzufügen der Ausreißer zum Scatterplot
points(outliers_data$MMM_S_Index, outliers_data$total_correct, col="red", pch=19)

# Legende hinzufügen
legend("topright", legend=c("Bereinigte Daten", "Ausreißer"), col=c("blue", "red"), pch=19)




### Analyse mit clean_data

# Neues Modell mit bereinigten Daten anpassen
model_clean <- lm(total_correct ~ poly(MMM_S_Index, 2), data = clean_data)


# Zusammenfassung des neuen Modells anzeigen
summary(model_clean)







### analyse mit filtered_data (total_correct >= 10)

# Bereinigung der Daten
filtered_data <- merged_data[merged_data$total_correct >= 15, ]

# Modell mit den bereinigten Daten anpassen
model_filtered <- lm(total_correct ~ poly(MMM_S_Index, 2), data = filtered_data)

# Zusammenfassung des Modells anzeigen
summary(model_filtered)









## Duration Ausreisser


# Definition des strengeren Schwellenwerts für Ausreißer
threshold_duration <- 15 * 60  # 15 Minuten in Sekunden

# Berechnung des Durchschnittswerts der nicht-ausreißerischen Dauer
mean_duration <- mean(duration_seconds[duration_seconds <= threshold_duration])

# Kopie erstellen
clean_duration_seconds <- duration_seconds

# Ersetzen der Ausreißer durch den Durchschnittswert
clean_duration_seconds[clean_duration_seconds > threshold_duration] <- mean_duration

# Boxplot der bereinigten Daten erstellen
boxplot(clean_duration_seconds, main="Boxplot of Duration with Stricter Threshold", ylab="Seconds")

# Optional: Winsorizing anwenden
library(DescTools)

# Winsorizing der Daten
winsorized_duration_seconds <- Winsorize(clean_duration_seconds, probs=c(0, 0.95))

# Boxplot der winsorisierten Daten erstellen
boxplot(winsorized_duration_seconds, main="Boxplot of Winsorized Duration", ylab="Seconds")




