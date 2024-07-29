


### Change Dir

path <- "/Users/maalt/OneDrive - Universität Wien/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/Experimente/Results"
setwd(path)


## Install packages


library(rjson)
library(jsonlite)
library(readtext)
library(stringr)

## read Jatos data

e <- jsonlite::fromJSON(file = "test_results_jatos.txt", method = "c", 
	unexpected.escape = "error", simplify = TRUE )
e <- jsonlite::fromJSON(file = "test_results_jatos.txt")

is.data.frame(e)
length(e)
e
e$data[[64]]$total_correct
e$context$jatosStudyResultId




