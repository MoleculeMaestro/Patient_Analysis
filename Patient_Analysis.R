# Load patient_info.csv

# set working directory automatically to project folder
getwd()   # just to check where you are

# read data
patient <- read.csv("data/patient_info.csv")
head(patient)

# check first rows
head(patient)

# check structure
str(patient)
str(patient)

str(patient)
summary(patient)
table(patient$diagnosis)
hist(patient$age, main="Age Distribution", xlab="Age")
table(patient$diagnosis)
table(patient$gender)
aggregate(bmi ~ diagnosis, data = patient, mean)
patient$gender <- as.factor(patient$gender)
patient$diagnosis <- as.factor(patient$diagnosis)
patient$smoker <- as.factor(patient$smoker)

str(patient)

summary_table <- data.frame(
  Variable = c("Gender", "Diagnosis", "Age (years)", "BMI", "Smoker"),
  Category = c("Female / Male", "Cancer / Normal", "-", "-", "Yes / No"),
  Count_or_Mean_SD = c(
    paste(table(patient$gender), collapse = " / "),
    paste(table(patient$diagnosis), collapse = " / "),
    paste(round(mean(patient$age),1), "±", round(sd(patient$age),1)),
    paste(round(mean(patient$bmi),1), "±", round(sd(patient$bmi),1)),
    paste(table(patient$smoker), collapse = " / ")
  )
)
if(!dir.exists("results")) {
  dir.create("results")
}
write.csv(summary_table, file = "results/patient_summary.csv", row.names = FALSE)
table(patient$gender, patient$diagnosis)

gender_diagnosis <- as.data.frame(table(patient$gender, patient$diagnosis))
View(gender_diagnosis)

gender_diagnosis <- table(patient$gender, patient$diagnosis)

barplot(gender_diagnosis, beside = TRUE,
        col = c("skyblue", "pink"),
        legend = rownames(gender_diagnosis),
        main = "Gender vs Diagnosis",
        xlab = "Diagnosis", ylab = "Count")

colnames(patient)

gender_table <- table(patient$Gender, patient$Diagnosis)

mean_by_diagnosis <- aggregate(cbind(`Age (years)`, BMI) ~ Diagnosis,
                               data = patient, mean)

colnames(patient)
mean_by_diagnosis <- aggregate(cbind(age, bmi) ~ diagnosis, data = patient, mean)

mean_by_diagnosis <- aggregate(cbind(age, bmi) ~ diagnosis, data = patient, mean)

table_by_diagnosis <- table(patient$diagnosis)

library(dplyr)

patient %>%
  group_by(diagnosis) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE),
    count = n()
  )
install.packages("dplyr")
library(dplyr)
summary_by_diagnosis <- patient %>%
  group_by(diagnosis) %>%
  summarise(
    n         = n(),
    mean_age  = mean(age, na.rm = TRUE),
    mean_bmi  = mean(bmi, na.rm = TRUE),
    smoker_pct = mean(smoker == "Yes", na.rm = TRUE) * 100
  )
gender_vs_diagnosis <- as.data.frame(table(patient$gender, patient$diagnosis))

names(gender_vs_diagnosis) <- c("gender", "diagnosis", "count")
View(gender_vs_diagnosis)


hist(patient$age,
     main = "Age Distribution",
     xlab = "Age (years)",
     col = "skyblue",
     border = "white")

hist(patient$bmi,
     main = "BMI Distribution",
     xlab = "BMI",
     col = "lightgreen",
     border = "white")

barplot(table(patient$diagnosis),
        main = "Diagnosis Counts",
        col = "orange",
        ylab = "Count")
tab <- table(patient$gender, patient$diagnosis)

barplot(tab,
        beside = TRUE,
        col = c("skyblue", "pink"),
        legend = rownames(tab),
        main = "Gender vs Diagnosis")
colnames(patient)
tab <- table(patient$gender, patient$diagnosis)

barplot(tab,
        beside = TRUE,
        col = c("skyblue", "pink"),
        legend = rownames(tab),
        main = "Gender vs Diagnosis")

barplot(mean_by_diagnosis$age,
        names.arg = mean_by_diagnosis$diagnosis,
        col = "lightblue",
        main = "Mean Age by Diagnosis",
        ylab = "Mean Age")

barplot(mean_by_diagnosis$bmi,
        names.arg = mean_by_diagnosis$diagnosis,
        col = "lightgreen",
        main = "Mean BMI by Diagnosis",
        ylab = "Mean BMI")

barplot(table(patient$smoker),
        main = "Smoker Distribution",
        col = c("gray", "red"),
        ylab = "Count")
write.csv(tab, "gender_diagnosis_table.csv")
write.csv(tab, "mean_by_diagnosis_table.csv")
write.csv(tab, "summary_by_diagnosis_table.csv")
