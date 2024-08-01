# Load the CSV file

path <- "C:/Users/maalt/ONEDRI~1/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/bachelor_thesis/Experimente/Results/Stichtag_09_07"
setwd(path)

data1 <- read.csv("jatos_results_data_0907_converted.csv")
data2 <- read.csv("jatos_results_data_0907_en_converted.csv")

common_columns <- intersect(names(data1), names(data2))

data1 <- data1[, common_columns]
data2 <- data2[, common_columns]


data <- rbind(data1, data2)

# Select the required columns
selected_data <- data[, c("jatosStudyResultId", "total_correct", "total_responses", "cum_perseverative_error")]

# Filter to keep only the rows where total_responses is 64
last_trial_data <- selected_data[selected_data$total_responses == 64, ]

# Sort the data by jatosStudyResultId
sorted_data <- last_trial_data[order(last_trial_data$jatosStudyResultId), ]

rownames(sorted_data) <- NULL






# Load Metadata

metadata1 <- read.csv("jatos_results_metadata_0907.csv")
metadata2 <- read.csv("jatos_results_metadata_0907_en.csv")

metadata <- rbind(metadata1, metadata2)
# Ensure colnames are the same

colnames(metadata)[colnames(metadata) == "Result.ID"] <- "jatosStudyResultId"

# Merge the two data.frames
exp_result <- merge(sorted_data, metadata[, c("jatosStudyResultId", "Start.time", "Duration")], 
                     by = "jatosStudyResultId", all.x = TRUE)

# delete total responses 

exp_result$total_responses <- NULL


# head(exp_result, n = 120)

nrow(exp_result)











