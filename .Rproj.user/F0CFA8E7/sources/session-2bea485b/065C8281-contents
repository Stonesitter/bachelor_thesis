
# Initiales R-squared
initial_r_squared <- summary(model_poly)$r.squared
current_r_squared <- initial_r_squared

# Initialisierung der Zähler für die Iterationen
iteration <- 1
current_data <- merged_data

# Funktion zur Identifizierung des Ausreißers mit dem höchsten studentisierten Residuum
identify_outlier <- function(model) {
  stud_res <- rstudent(model)
  return(which.max(abs(stud_res)))
}

# Iteratives Entfernen von Ausreißern
improvement_threshold <- 0.0001  # Mindestverbesserung von R-squared
max_iterations <- 100  # Maximale Anzahl an Iterationen
continue_removal <- TRUE

while (continue_removal & iteration <= max_iterations) {
  # Kopiere die aktuellen Daten
  assign(paste0("merged_data_outlier", iteration), current_data)
  
  # Identifiziere den Ausreißer mit dem höchsten absoluten studentisierten Residuum
  max_resid_index <- identify_outlier(model_poly)
  
  # Entferne diesen Ausreißer aus den Daten
  current_data <- current_data[-max_resid_index, ]
  
  # Aktualisiere das Modell mit den bereinigten Daten
  model_poly_reduced <- lm(total_correct ~ poly(MMM_S_Index, 2), data = current_data)
  new_r_squared <- summary(model_poly_reduced)$r.squared
  
  # Überprüfe die Verbesserung von R-squared
  if ((new_r_squared - current_r_squared) > improvement_threshold) {
    current_r_squared <- new_r_squared
    iteration <- iteration + 1
  } else {
    continue_removal <- FALSE
  }
}

# Endgültiges Modell nach der Entfernung der Ausreißer
summary(model_poly_reduced)
r2_cleaned <- current_data


nrow(merged_data)
nrow(r2_cleaned)

model_r2 <- lm(total_correct ~ poly(MMM_S_Index, 2), data = r2_cleaned)
summary(model_r2)
