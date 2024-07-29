# Löschen aller Objekte im Workspace
# rm(list = ls())


### change dir

path <- "/Users/maalt/OneDrive - Universität Wien/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/Experimente/Results/Stichtag_15_06"
setwd(path)

data1 <- read.csv("jatos_results_data_1506_converted.csv")
data2 <- read.csv("jatos_results_data_1506_en_converted.csv")



unique_ids1 <- unique(data1$jatosStudyResultId)
unique_ids2 <- unique(data2$jatosStudyResultId)


# Initialize an empty data frame to store the results
exp_result1 <- data.frame(jatosStudyResultId = character(), total_correct = numeric())

# Loop through each unique id
for (id in unique_ids1) {
  # Subset data for the current id
  participant_data <- subset(data1, jatosStudyResultId == id)
  
  # Assuming each participant has 64 trials and we need the total_correct after the last trial
  last_total_correct <- tail(participant_data$total_correct, n = 1)
  
  # Create a new data frame row
  new_row <- data.frame(jatosStudyResultId = id, total_correct = last_total_correct)
  
  # Append the new row to the result data frame
  exp_result1 <- rbind(exp_result1, new_row)
}

# Initialize an empty data frame to store the results
exp_result2 <- data.frame(jatosStudyResultId = character(), total_correct = numeric())

# Loop through each unique id
for (id in unique_ids2) {
  # Subset data for the current id
  participant_data <- subset(data2, jatosStudyResultId == id)
  
  # Assuming each participant has 64 trials and we need the total_correct after the last trial
  last_total_correct <- tail(participant_data$total_correct, n = 1)
  
  # Create a new data frame row
  new_row <- data.frame(jatosStudyResultId = id, total_correct = last_total_correct)
  
  # Append the new row to the result data frame
  exp_result2 <- rbind(exp_result2, new_row)
}


# Zusammenfügen der DataFrames
exp_result <- rbind(exp_result1, exp_result2)


# Display the result
print(exp_result)
