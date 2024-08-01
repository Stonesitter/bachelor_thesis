# Notwendige Pakete laden
library(car)
library(stats)


# Eine Kopie des Modells und der Daten erstellen
sw_clean_poly <- model_poly
sw_clean_data <- merged_data

# Funktion zum Berechnen der Student-Residuen
calc_student_resid <- function(model) {
  return(rstudent(model))
}

# Funktion zum Berechnen des Shapiro-Wilk-Tests
perform_shapiro_test <- function(residuals) {
  return(shapiro.test(residuals)$p.value)
}

# Schleife zum Entfernen der Datenpunkte mit den höchsten Student-Residuen
repeat {
  # Berechne die Student-Residuen für das sw_clean_poly Modell
  student_resid <- calc_student_resid(sw_clean_poly)
  
  # Führe den Shapiro-Wilk-Test durch
  p_value <- perform_shapiro_test(student_resid)
  
  # Überprüfe, ob der Shapiro-Wilk-Test nicht signifikant ist (p > 0.05)
  if (p_value > 0.05) {
    break
  }
  
  # Finde den Index des Datenpunkts mit dem höchsten absoluten Student-Residuum
  max_resid_index <- which.max(abs(student_resid))
  
  # Entferne diesen Datenpunkt aus den Daten
  sw_clean_data <- sw_clean_data[-max_resid_index, ]
  
  # Passe das sw_clean_poly Modell mit den verbleibenden Daten erneut an
  sw_clean_poly <- lm(total_correct ~ poly(MMM_S_Index, 2), data = sw_clean_data)
}

# Das endgültige Modell und die verbleibenden Daten
sw_clean_poly
sw_clean_data


summary(sw_clean_poly)
