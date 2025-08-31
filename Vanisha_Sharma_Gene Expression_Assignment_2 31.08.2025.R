getwd()
list.files()
data1 <- read.csv("DEGs_Data_1.csv")
data2 <- read.csv("DEGs_Data_2.csv")

head(data1)
head(data2)


classify_gene <- function(log2fc, pval) {
  if (pval < 0.05 & log2fc >= 1) {
    return("Upregulated")
  } else if (pval < 0.05 & log2fc <= -1) {
    return("Downregulated")
  } else {
    return("Not significant")
  }
}
classify_gene <- function(logFC, padj) {
  if (padj < 0.05 & logFC > 1) {
    return("Upregulated")
  } else if (padj < 0.05 & logFC < -1) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}
status <- c()

for (i in 1:nrow(data1)) {
  status[i] <- classify_gene(data1$logFC[i], data1$padj[i])
}
data1$status <- status
head(data1)

status2 <- c()  

for (i in 1:nrow(data2)) {
  status2[i] <- classify_gene(data2$logFC[i], data2$padj[i])
}
data2$status <- status2
head(data2)

table(data1$status)


classify_gene <- function(logFC, padj) {
  if (is.na(padj)) {
    return("NA")
  } else if (padj < 0.05 & logFC > 1) {
    return("Upregulated")
  } else if (padj < 0.05 & logFC < -1) {
    return("Downregulated")
  } else {
    return("Not Significant")
  }
}
status1 <- c()  

for (i in 1:nrow(data1)) {
  status1[i] <- classify_gene(data1$logFC[i], data1$padj[i])
}

data1$status <- status1
head(data1)

if (!dir.exists("Results")) {
  dir.create("Results")
}
files <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

classify_gene <- function(logFC, padj) {
  if (is.na(padj)) {
    return("Not_Significant")   # treat NA as not significant
  } else if (padj < 0.05 & logFC > 1) {
    return("Upregulated")
  } else if (padj < 0.05 & logFC < -1) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}
for (file in files)

  data <- read.csv(file, header = TRUE)  
data$padj[is.na(data$padj)] <- 1
status_vec <- c()

for (i in 1:nrow(data)) {
  status_vec[i] <- classify_gene(data$logFC[i], data$padj[i])
}

data$status <- status_vec

write.csv(data, file.path("Results", paste0("Processed_", file)), row.names = FALSE)

cat("\nSummary for", file, ":\n")
print(table(data$status)[table(data$status) > 0])


if (!dir.exists("Results")) {
  dir.create("Results")
}

files <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")
classify_gene <- function(logFC, padj) {
  if (is.na(padj)) {
    return("Not_Significant")   # treat NA as not significant
  } else if (padj < 0.05 & logFC > 1) {
    return("Upregulated")
  } else if (padj < 0.05 & logFC < -1) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}
all_summaries <- list()

for (file in files)
  
  data <- read.csv(file, header = TRUE)
data$padj[is.na(data$padj)] <- 1

status_vec <- c()

for (i in 1:nrow(data)) {
  status_vec[i] <- classify_gene(data$logFC[i], data$padj[i])
}

data$status <- status_vec

write.csv(data, file.path("Results", paste0("Processed_", file)), row.names = FALSE)


summary_counts <- table(data$status)[table(data$status) > 0]


cat("\nSummary for", file, ":\n")
print(summary_counts)

all_summaries[[file]] <- summary_counts

combined_summary <- do.call(cbind, all_summaries)
cat("\nCombined summary for both datasets:\n")
print(combined_summary)

write.csv(as.data.frame(combined_summary), 
          file.path("Results", "Combined_Summary.csv"), 
          row.names = TRUE)
