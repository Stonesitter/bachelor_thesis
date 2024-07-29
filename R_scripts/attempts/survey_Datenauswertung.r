### change dir

path <- "/Users/maalt/OneDrive - Universität Wien/Dokumente/Uni/Psychologie/8. Semester/Bachelorarbeit/Experimente/Results"
setwd(path)

### read.csv table

test <- read.table("test_file.csv", header = TRUE, sep = ",")


### Wie komme ich an die Id's

test
test$jatosStudyResultId
str(test)


### wie komme ich an die richtigen Spalten?


### Wie komme ich an einzelne rows?

test[1, ]
is.matrix(test)
is.vector(test)
is.list(test)
mode(test)
#spielereien

### kreiere eine copy

work.test <- test
work.test

names(work.test)
work.test[["G01Q01.SQ001."]]
work.test$G01Q01.SQ001


####kreiere Formel



