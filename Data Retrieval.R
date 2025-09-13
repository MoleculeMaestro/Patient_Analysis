
setwd("D:/My Files/AI and Bioinfo/R_Bioinformatics_Class/Data Retrieval Assignment")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
library(GEOquery)

gse <- getGEO("GSE6364", GSEMatrix = TRUE)

expr_matrix <- exprs(gse[[1]])

metadata <- pData(phenoData(gse[[1]]))

write.csv(expr_matrix, "GSE6364_matrix.csv")
write.csv(metadata, "GSE6364_metadata.csv")
