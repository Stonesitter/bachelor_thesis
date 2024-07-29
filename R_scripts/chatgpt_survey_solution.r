# Löschen aller Objekte im Workspace
# rm(list = ls())


### change dir

path <- "/Users/maalt/OneDrive - Universität Wien/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/Experimente/Results/Stichtag_09_07"
setwd(path)

### read.csv table

survey_data_0907 <- read.table("survey_data_0907.csv", header = TRUE, sep = ",")
survey_data_0907_en <- read.table("survey_data_0907_en.csv", header = TRUE, sep = ",")
survey_data_1905 <- read.table("survey_data_1905.csv", header = TRUE, sep = ",")



# Zusammenfügen der DataFrames
survey_data <- rbind(survey_data_0907, survey_data_0907_en, survey_data_1905)






# Function to extract numeric value from the response

extract_numeric <- function(response) {
  as.numeric(sub("AO0", "", response))
}

# Convert character strings to numeric values
subscale_items <- survey_data[, grep("G01Q01", names(survey_data))]
subscale_items[] <- lapply(subscale_items, extract_numeric)

# Calculate the MMM-S index for each participant
MMM_S_Index <- rowMeans(subscale_items, na.rm = TRUE)

# Add the MMM-S index to the original data frame
survey_data$MMM_S_Index <- MMM_S_Index







# Create a new data frame with the results
survey_result <- survey_data[, c("jatosStudyResultId", "MMM_S_Index","G02Q05", "G02Q06")]


# Ändere die Spaltennamen G02Q05 und G02Q06
names(survey_result)[names(survey_result) == "G02Q05"] <- "Age"
names(survey_result)[names(survey_result) == "G02Q06"] <- "Gender"

survey_result$Gender <- factor(survey_result$Gender,
                               levels = c("AO01", "AO02", "AO03"),
                               labels = c("male", "female", "diverse"))






# Ersetzen leerer Zeichenketten durch NA-Werte
survey_result[survey_result == ""] <- NA
# Finden der Zeilen, die mehr als einen NA-Wert enthalten
rows_to_keep <- apply(survey_result, 1, function(row) sum(is.na(row)) <= 1)

# Erstellen eines neuen DataFrames ohne diese Zeilen
survey_result <- survey_result[rows_to_keep, ]



# reset rownames
rownames(survey_result) <- NULL

# Print the result
print(survey_result)




